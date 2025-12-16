-- =====================================================
-- PostGIS Topology Simplification with Generalization
-- =====================================================
-- Description: Simplifies polygon boundaries while preserving topology
--              Uses different tolerances for coastlines vs inland boundaries
-- Author: [Konan Pruiksma]
-- Date: 2025-12-11
-- Version: 1.0
-- =====================================================

CREATE OR REPLACE FUNCTION generalise_boundaries(
    input_schema TEXT,
    input_table TEXT,
    geom_column TEXT DEFAULT 'geom',
    output_table TEXT DEFAULT NULL,
    srid INTEGER DEFAULT 3035,
    coastline_tolerance NUMERIC DEFAULT 10000,
    inland_tolerance NUMERIC DEFAULT 100,
    topology_tolerance NUMERIC DEFAULT 0
)
RETURNS TEXT AS $$
DECLARE
    topo_name TEXT;
    topo_generalised TEXT;
    output_table_full TEXT;
    result_msg TEXT;
    non_geom_columns TEXT;
BEGIN
    -- Ensure output schema exists
    EXECUTE 'CREATE SCHEMA IF NOT EXISTS generalised';

    -- Generate topology names
    topo_name := input_table || '_topology';
    topo_generalised := input_table || '_topology_generalised';

    -- Determine output table name
    IF output_table IS NULL THEN
        output_table_full := format('generalised.%I_generalised', input_table);
    ELSE
        output_table_full := output_table;
    END IF;

    -- Step 0: Determine non-geometry columns dynamically
    SELECT string_agg(format('%I', column_name), ', ')
    INTO non_geom_columns
    FROM information_schema.columns
    WHERE table_schema = input_schema
      AND table_name = input_table
      AND column_name <> geom_column;

    -- Step 1: Drop existing topologies if present
    BEGIN
        PERFORM topology.DropTopology(topo_name);
    EXCEPTION WHEN OTHERS THEN
        RAISE NOTICE 'Topology % does not exist, continuing...', topo_name;
    END;

    BEGIN
        PERFORM topology.DropTopology(topo_generalised);
    EXCEPTION WHEN OTHERS THEN
        RAISE NOTICE 'Topology % does not exist, continuing...', topo_generalised;
    END;

    -- Step 2: Create initial topology
    PERFORM CreateTopology(topo_name, srid, topology_tolerance);

    -- Step 3: Populate initial topology with original geometries
    EXECUTE format(
        'SELECT ST_CreateTopoGeo(%L, ST_Collect(%I)) FROM %I.%I',
        topo_name, geom_column, input_schema, input_table
    );

    -- Step 4: Create generalised topology
    PERFORM CreateTopology(topo_generalised, srid, topology_tolerance);

    -- Step 5: Populate generalised topology with generalised edges
    EXECUTE format(
        'SELECT ST_CreateTopoGeo(%L, geom)
         FROM (
             SELECT ST_Collect(
                 CASE
                     WHEN left_face = 0 OR right_face = 0 THEN st_simplifyvw(geom, %s)
                     ELSE ST_SimplifyPreserveTopology(geom, %s)
                 END
             ) AS geom
             FROM %I.edge_data
         ) AS foo',
        topo_generalised, coastline_tolerance, inland_tolerance, topo_name
    );

    -- Step 6: Create output table with generalised geometries and original attributes
    EXECUTE format('DROP TABLE IF EXISTS %s', output_table_full);

    EXECUTE format(
        'CREATE TABLE %s AS
         WITH generalised AS (
             SELECT face_id, st_getFaceGeometry(%L, face_id) AS geom
             FROM %I.face
             WHERE face_id > 0
         ),
         original_points AS (
             SELECT ST_PointOnSurface(%I) AS pt_geom, %I AS orig_geom, %s
             FROM %I.%I
         )
         SELECT g.geom, o.*
         FROM generalised g
         LEFT JOIN original_points o
         ON ST_Contains(g.geom, o.pt_geom)',
        output_table_full, topo_generalised, topo_generalised,
        geom_column, geom_column, non_geom_columns,
        input_schema, input_table
    );

    -- Step 7: Drop topologies to clean database
    BEGIN
        PERFORM topology.DropTopology(topo_name);
    EXCEPTION WHEN OTHERS THEN
        RAISE NOTICE 'Failed to drop topology %', topo_name;
    END;

    BEGIN
        PERFORM topology.DropTopology(topo_generalised);
    EXCEPTION WHEN OTHERS THEN
        RAISE NOTICE 'Failed to drop topology %', topo_generalised;
    END;

    result_msg := format(
        'Success! Generalised geometries with original attributes created in table: %s',
        output_table_full
    );

    RAISE NOTICE '%', result_msg;
    RETURN result_msg;

END;
$$ LANGUAGE plpgsql;