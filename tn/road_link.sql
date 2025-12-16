
CREATE OR REPLACE PROCEDURE tn_50.initial_selection_road_link()
LANGUAGE PLPGSQL
AS $$
BEGIN

    RAISE NOTICE 'Running tn_50.initial_selection_road_link()...';

    DROP TABLE IF EXISTS tn_50.road_link;
    CREATE TABLE tn_50.road_link ( LIKE tn.road_link INCLUDING INDEXES);

    INSERT INTO tn_50.road_link
        SELECT *
        FROM tn.road_link
        WHERE begin_lifespan_version < CURRENT_DATE AND end_lifespan_version is NULL
        AND form_of_way in ('motorway', 'dual_carriage_way', 'single_carriage_way', 'roundabout')
        AND functional_road_class in ('main_road', 'first_class', 'second_class', 'third_class', 'void_unk')
        AND country = 'nl' -- TEMPORARY FILTER
    ;

    RAISE NOTICE 'Finished tn_50.initial_selection_road_link()...';
END; $$;



CREATE OR REPLACE PROCEDURE tn_50.generalize_road_link()
LANGUAGE PLPGSQL
AS $$
DECLARE
  
    partition_cursor CURSOR FOR
    SELECT objectid, country, national_code, label, national_level_code, geom
    FROM au.administrative_unit_area_3;
    partition_record RECORD;
    p_national_level_code text;
    p_national_code text;
    p_label text;
    p_geom geometry;
  
BEGIN
    RAISE NOTICE 'Running tn_50.generalize_road_link()...';

    OPEN partition_cursor;

    LOOP
        FETCH NEXT FROM partition_cursor INTO partition_record;
        EXIT WHEN NOT FOUND;

        p_national_level_code := partition_record.national_level_code;
        p_national_code := partition_record.national_code;
        p_geom := partition_record.geom;
        p_label := partition_record.label;

        -- TEMPORARY FILTER
        IF p_national_code in ('0344', '1904', '0632' ) AND p_national_level_code = '2103' THEN -- TEMPORARY FILTER, tested on Utrecht, Stichtse Vecht and Woerden

        RAISE NOTICE 'Partition found for %', p_label;
        
        CALL tn_50.collapse_roundabouts(p_geom);
        
        --CALL tn_50.remove_road_dangles_iterative(p_geom, p_minimum_length => 100, p_max_iterations => 5);
        CALL tn_50.thin_roads_by_topology(p_geom);
        END IF;
    END LOOP;
    CLOSE partition_cursor;
  
    RAISE NOTICE 'Finished tn_50.generalize_road_link()...';
END; $$;

--
-- These functions have been replaced by thin_roads_by_topology()
--
-- CREATE OR REPLACE PROCEDURE tn_50.remove_road_dangles_iterative(p_geom geometry, p_minimum_length integer, p_max_iterations integer)
-- LANGUAGE PLPGSQL
-- AS $$
-- DECLARE
--   count_cursor CURSOR FOR
--     SELECT COUNT(*)
--     FROM tn_50.road_link
--     WHERE ST_Intersects(road_link.geom, p_geom);

--   p_count_before integer;
--   p_count_after integer;
--   p_removed_dangles integer;
--   p_iteration integer := 1;

-- BEGIN
--   RAISE NOTICE 'Running tn_50.remove_road_dangles_iterative()...';
--   LOOP
--     RAISE NOTICE 'Current iteration: %', p_iteration;
    
--     -- Count number of road links before removing dangles
--     OPEN count_cursor;
--     FETCH FIRST FROM count_cursor INTO p_count_before;
--     RAISE NOTICE 'Count before: %', p_count_before;
--     CLOSE count_cursor;

--     -- Remove road link dangles
--     CALL tn_50.remove_road_dangles(p_geom, p_minimum_length);

--     -- Count number of road links after removing dangles
--     OPEN count_cursor;
--     FETCH FIRST FROM count_cursor INTO p_count_after;
--     RAISE NOTICE 'Count after: %', p_count_after;
--     CLOSE count_cursor;

--     p_removed_dangles := p_count_before - p_count_after;
--     RAISE NOTICE 'Number of removed dangles in iteration %: %', p_iteration, p_removed_dangles;
    
--     -- Keep going until no more road_link dangles are removed or number of max iterations is reached
--     IF p_count_before = p_count_after OR p_iteration >= p_max_iterations THEN
--       EXIT;
--     END IF;

--     p_iteration := p_iteration + 1;
--   END LOOP;

-- END; $$;


-- CREATE OR REPLACE PROCEDURE tn_50.remove_road_dangles(p_geom geometry, p_minimum_length integer)
-- LANGUAGE PLPGSQL
-- AS $$
-- BEGIN
--     RAISE NOTICE 'Running tn_50.remove_road_dangles()...';
  
--   -- Select all road_links for this partition
--   DROP TABLE IF EXISTS tn_50._temp_road_link_partition;
--   CREATE TABLE tn_50._temp_road_link_partition AS (
--     SELECT *
--     FROM tn_50.road_link
--     WHERE ST_Intersects(road_link.geom, p_geom)
      
--   );
  
--   -- Find all dangle points for this partition
--   DROP TABLE IF EXISTS tn_50._temp_road_link_partition_endpoints;
--   CREATE TABLE tn_50._temp_road_link_partition_endpoints AS (
--     WITH all_endpoints AS (
--       SELECT ST_StartPoint(geom) geom
--       FROM tn_50._temp_road_link_partition
--       UNION ALL
--       SELECT ST_EndPoint(geom) geom
--       FROM tn_50._temp_road_link_partition 
--     )
--     SELECT count(*), geom
--     FROM all_endpoints
--     WHERE ST_Within(geom, p_geom) -- Don't process endpoints outside this partition
--     GROUP BY geom
--     HAVING count(*) = 1
--   );

--   -- Select all dangling road_links which we want to delete
--   DROP TABLE IF EXISTS tn_50._temp_road_link_partition_remove_this_dangle;
--   CREATE TABLE tn_50._temp_road_link_partition_remove_this_dangle AS (
--     SELECT rl.objectid, rl.geom
--     FROM tn_50._temp_road_link_partition rl, tn_50._temp_road_link_partition_endpoints ep
--     WHERE ST_Touches(rl.geom, ep.geom)
--     AND ST_Length(rl.geom) < p_minimum_length
--   );

--   -- Perform deletion
--   DELETE
--   FROM tn_50.road_link rl
--   WHERE objectid IN (
--       SELECT objectid FROM tn_50._temp_road_link_partition_remove_this_dangle
--   );

-- END; $$;


CREATE OR REPLACE PROCEDURE tn_50.collapse_roundabouts(p_geom geometry)
LANGUAGE PLPGSQL
AS $$
DECLARE

BEGIN
    RAISE NOTICE 'Running tn_50.collapse_roundabouts()...';

    DROP TABLE IF EXISTS roundabout_road_links;
    CREATE TEMP TABLE roundabout_road_links AS (
        SELECT road_link.objectid, road_link.form_of_way, road_link.functional_road_class, road_link.geom, road_link.vertical_position, road_link.vertical_level
        FROM tn_50.road_link
        WHERE form_of_way = 'roundabout'
        AND ST_Intersects(road_link.geom, p_geom)
    );

    DROP TABLE IF EXISTS roundabout_areas;
    CREATE TEMP TABLE roundabout_areas AS (
        WITH roundabout_area AS (
            SELECT (ST_Dump(ST_Union(ST_Buffer(rrl.geom, 1.0)))).geom AS geom
            FROM roundabout_road_links rrl
        )
        SELECT
            geom,
            mbr.center,
            mbr.radius,
            ST_X(mbr.center) AS center_x,
            ST_Y(mbr.center) AS center_y,
            -- ST_MakePolygon(ST_ExteriorRing(geom)) exterior_geom,
            ST_Area(ST_MakePolygon(ST_ExteriorRing(geom))) exterior_area,
            --ST_Buffer(mbr.center, mbr.radius) mbr_geom,
	          ST_Area(ST_Buffer(mbr.center, mbr.radius)) mbr_area,
            ST_Area(ST_MakePolygon(ST_ExteriorRing(geom))) / ST_Area(ST_Buffer(mbr.center, mbr.radius)) circle_area_perc
        FROM roundabout_area, ST_MinimumBoundingRadius(geom) mbr
    );

    -- Select circular roundabouts which have a diameter smaller than 50m
    DROP TABLE IF EXISTS roundabout_areas_filtered;
    CREATE TEMP TABLE roundabout_areas_filtered AS (
        SELECT *
	    FROM roundabout_areas
	    WHERE circle_area_perc > 0.9 AND (radius*2) < 50
    );

    -- Select road links which are part of the filtered roundabouts
    DROP TABLE IF EXISTS roundabouts_road_links;
    CREATE TEMP TABLE roundabouts_road_links AS (
        SELECT
            rl.objectid,
            rl.geom,
            rl.vertical_position,
            rl.vertical_level,
            center_x,
            center_y
        FROM tn_50.road_link rl, roundabout_areas_filtered
        WHERE form_of_way = 'roundabout' AND ST_Contains(roundabout_areas_filtered.geom, rl.geom)
    );

    -- Select road links which connect to the filtered roundabouts
    DROP TABLE IF EXISTS roundabouts_connecting_road_links;
    CREATE TEMP TABLE roundabouts_connecting_road_links AS (
        SELECT DISTINCT rl.objectid, rl.geom, rrl.center_x, rrl.center_y
        FROM tn_50.road_link rl, roundabouts_road_links rrl
        WHERE form_of_way <> 'roundabout' AND ST_Touches(rrl.geom, rl.geom)
    );

    DROP TABLE IF EXISTS roundabouts_connecting_points;
    CREATE TEMP TABLE roundabouts_connecting_points AS (
        WITH collected_roundabout_road_links AS (
          SELECT ST_Collect(geom) AS geom
          FROM roundabouts_road_links
      )
      SELECT crl.objectid,
          ST_Intersection(crl.geom, rrl.geom),
          ST_Equals(ST_Intersection(crl.geom, rrl.geom), ST_StartPoint(crl.geom)) as is_startpoint,
          ST_Equals(ST_Intersection(crl.geom, rrl.geom), ST_EndPoint(crl.geom)) as is_endpoint
      FROM roundabouts_connecting_road_links crl, collected_roundabout_road_links rrl
      WHERE ST_Intersects(crl.geom, rrl.geom)
    );


    DROP TABLE IF EXISTS roundabouts_extended_lines;
    CREATE TEMP TABLE roundabouts_extended_lines AS (
        SELECT
            crl.objectid AS objectid,
            CASE
                WHEN cp.is_startpoint THEN ST_AddPoint(crl.geom, ST_MakePoint(crl.center_x::float, crl.center_y::float),0)
                WHEN cp.is_endpoint THEN ST_AddPoint(crl.geom, ST_MakePoint(crl.center_x::float, crl.center_y::float),-1)
                -- This is not expected, connecting road_link doesnt touch but intersects with the roundabout road_link
                ELSE ST_AddPoint(crl.geom, ST_MakePoint(crl.center_x::float, crl.center_y::float),-1)
            END AS GEOM
        FROM roundabouts_connecting_road_links crl, roundabouts_connecting_points cp
        WHERE crl.objectid = cp.objectid
    );

    -- Update geometries for the extended lines
    UPDATE tn_50.road_link
    SET geom = road_links_extended.geom
    FROM roundabouts_extended_lines road_links_extended
    WHERE tn_50.road_link.objectid = road_links_extended.objectid;

    -- Remove the collapsed roundabout lines
    DELETE FROM tn_50.road_link
    WHERE objectid IN (
        SELECT objectid FROM roundabouts_road_links
    );

    RAISE NOTICE 'Finished tn_50.collapse_roundabouts()...';

END; $$;


CREATE OR REPLACE PROCEDURE tn_50.setup_topology(p_geom geometry)
LANGUAGE PLPGSQL
AS $$
DECLARE
    topo_id INTEGER;
    topo_layer_id INTEGER;
BEGIN
    RAISE NOTICE 'Running tn_50.setup_topology()...';

    DROP TABLE IF EXISTS tn_50.road_link_topo_partition;
    CREATE TABLE tn_50.road_link_topo_partition AS (
    SELECT 
        "objectid",
        "country",
        "begin_lifespan_version",
        "end_lifespan_version",
        ST_Force2d(geom) as geom,
        "form_of_way",
        "functional_road_class",
        "number_of_lanes",
        "vertical_position",
        "vertical_level",
        "tent_network",
        "street_name",
        "street_label",
        "road_surface_category",
        "traffic_flow_direction",
        "access_restriction",
        "restriction_for_vehicles",
        "speed_limit",
        "condition_of_facility",
        "link_to_road",
        "road_national_road_code",
        "road_european_route_number",
        "road_name",
        "road_label",
        "w_national_identifier",
        "w_step",
        "xy_source",
        "z_source",
        "w_scale",
        "w_release"
    FROM tn_50.road_link
    WHERE ST_Intersects(road_link.geom, p_geom)
    );

    DROP INDEX IF EXISTS road_link_topo_partition_geom_ix;
    CREATE INDEX IF NOT EXISTS road_link_topo_partition_geom_ix
        ON tn_50.road_link_topo_partition USING gist
        (geom)
        TABLESPACE pg_default;
    
	-- Create a (new) topology
	BEGIN
	    PERFORM topology.DropTopology('tn_50_road_link_topology');
	EXCEPTION WHEN OTHERS THEN
	END;
	
	SELECT topology.CreateTopology('tn_50_road_link_topology', 3035) INTO topo_id;
    
	-- Add a (new) topo geometry column
	BEGIN
	    PERFORM topology.DropTopoGeometryColumn('tn_50', 'road_link_topo_partition', 'topo_geom');
	EXCEPTION WHEN OTHERS THEN
	END;
	
    SELECT topology.AddTopoGeometryColumn('tn_50_road_link_topology', 'tn_50', 'road_link_topo_partition', 'topo_geom', 'MULTILINE')
	INTO topo_layer_id;
	
	-- Fill the topology
    UPDATE tn_50.road_link_topo_partition
    SET topo_geom = topology.toTopoGeom(geom, 'tn_50_road_link_topology', topo_layer_id, 0.0);
	
	-- Store the original attributes for each edge
    DROP TABLE IF EXISTS tn_50_road_link_topology.edge_attributes_lookup;
    CREATE TABLE tn_50_road_link_topology.edge_attributes_lookup AS (
        SELECT
            r.objectid,
            r.form_of_way,
            r.functional_road_class,
            r.vertical_position,
            r.vertical_level,
            edge_data.edge_id,
            ST_Intersects(ST_Boundary(p_geom), edge_data.geom) on_partition_border
        FROM tn_50_road_link_topology.edge_data
        INNER JOIN tn_50_road_link_topology.relation rel
        ON edge_data.edge_id = rel.element_id
        INNER JOIN tn_50.road_link_topo_partition r
        ON rel.topogeo_id = (r.topo_geom).id
    );

    -- Create view for edges including the original attributes
    DROP VIEW IF EXISTS tn_50_road_link_topology.edge_incl_attributes;
    CREATE OR REPLACE VIEW tn_50_road_link_topology.edge_incl_attributes
    AS
    SELECT
        lookup.objectid,
        lookup.form_of_way,
        lookup.functional_road_class,
        lookup.vertical_position,
        lookup.vertical_level,
        lookup.on_partition_border,
        edge.edge_id,
        edge.start_node,
        edge.end_node,
        edge.next_left_edge,
        edge.next_right_edge,
        edge.left_face,
        edge.right_face,
        edge.geom
    FROM tn_50_road_link_topology.edge, tn_50_road_link_topology.edge_attributes_lookup lookup
    WHERE edge.edge_id = lookup.edge_id;
    
    -- We need to clear the topo geom relationship before we can modify the topology
    PERFORM clearTopoGeom(topo_geom) from tn_50.road_link_topo_partition;

    RAISE NOTICE 'Finished tn_50.setup_topology()...';

END;
$$;


CREATE OR REPLACE PROCEDURE tn_50.thin_roads_by_topology(p_geom geometry)
LANGUAGE PLPGSQL
AS $$
DECLARE

BEGIN
    RAISE NOTICE 'Running tn_50.thin_roads_by_topology()...';

    CALL tn_50.setup_topology(p_geom);

    CALL tn_50.merge_continuous_lines();
    CALL tn_50.remove_short_dangling_lines(200);
    CALL tn_50.merge_continuous_lines();
    CALL tn_50.remove_single_edge_faces(3000);
    CALL tn_50.remove_short_dangling_lines(200);
    CALL tn_50.merge_continuous_lines();
    CALL tn_50.simplify_two_edge_faces(3000);
    CALL tn_50.remove_isolated_nodes();
    CALL tn_50.merge_continuous_lines();
    CALL tn_50.thin_tjunctions(300);
    CALL tn_50.merge_continuous_lines();

    CALL tn_50.apply_topology_thinning();

    RAISE NOTICE 'Finished tn_50.thin_roads_by_topology()...';

END; $$;



CREATE OR REPLACE PROCEDURE tn_50.merge_continuous_lines()
LANGUAGE PLPGSQL
AS $$
DECLARE
    node_id_row RECORD;
    edge_ids INTEGER[];
    is_on_partition_border BOOLEAN;
    counter INTEGER := 1;

BEGIN
    RAISE NOTICE 'Running tn_50.merge_continuous_lines()...';

    FOR node_id_row IN
        WITH superfluous_node_ids AS (
            SELECT node_id
            FROM tn_50_road_link_topology.node n, tn_50_road_link_topology.edge e
            WHERE (e.start_node = n.node_id OR e.end_node = n.node_id)
			    AND NOT (e.start_node = n.node_id AND e.end_node = n.node_id)
            GROUP BY node_id
            HAVING count(*) = 2
        )
        SELECT n.node_id, n.geom
        FROM tn_50_road_link_topology.node n, superfluous_node_ids d
        WHERE n.node_id = d.node_id

        LOOP
			edge_ids := ARRAY(SELECT edge_id
							  FROM tn_50_road_link_topology.edge
							  WHERE start_node = node_id_row.node_id OR end_node = node_id_row.node_id
							 );
			
            -- Make sure that we have 2 edges
			IF ARRAY_LENGTH(edge_ids, 1) <> 2 OR edge_ids[1] IS NULL OR edge_ids[2] IS NULL THEN
				CONTINUE;
			END IF;
            
            -- Check if any of these edges are on the partition borders
            SELECT EXISTS(
                SELECT edge_id FROM tn_50_road_link_topology.edge_incl_attributes WHERE edge_id IN (edge_ids[1], edge_ids[2]) AND on_partition_border = true 
            ) INTO is_on_partition_border;
			
            IF is_on_partition_border THEN
                -- RAISE NOTICE 'Skipped the merging of edge % and edge % because of the partition border.', edge_ids[1], edge_ids[2];
                CONTINUE;
            END IF;

			BEGIN
			    PERFORM ST_ModEdgeHeal('tn_50_road_link_topology', edge_ids[1], edge_ids[2]);
                counter := counter + 1;
			EXCEPTION WHEN OTHERS THEN
			    -- For debugging purposes
			END;
        END LOOP;

    RAISE NOTICE 'Number of merges: %.', counter;
    RAISE NOTICE 'Finished tn_50.merge_continuous_lines()...';

END;
$$;


CREATE OR REPLACE PROCEDURE tn_50.remove_short_dangling_lines(p_max_length integer)
LANGUAGE PLPGSQL
AS $$
DECLARE
    edge_id_row RECORD;
    edge_id_attributes_row RECORD;
    counter INTEGER := 1;
BEGIN
    RAISE NOTICE 'Running tn_50.remove_short_dangling_lines()...';

    DROP TABLE IF EXISTS dangling_node_ids;
    CREATE TEMP TABLE dangling_node_ids AS (
        SELECT node_id
        FROM tn_50_road_link_topology.node n, tn_50_road_link_topology.edge e
        WHERE e.start_node = n.node_id OR e.end_node = n.node_id
        GROUP BY node_id
        HAVING count(*) = 1
    );

    FOR edge_id_row IN
        SELECT e.edge_id, e.geom, dn.node_id, ST_Length(e.geom) length
        FROM tn_50_road_link_topology.edge e, dangling_node_ids dn
        WHERE e.start_node = dn.node_id OR e.end_node = dn.node_id


    LOOP
        IF edge_id_row.length < p_max_length THEN

			SELECT form_of_way, on_partition_border INTO edge_id_attributes_row
            FROM tn_50_road_link_topology.edge_incl_attributes WHERE edge_id = edge_id_row.edge_id;

			-- Only remove if the road_link has form_of_way = 'single_carriage_way' and is not on partition border.
			-- We could make other choices based on form_of_way, functional_road_class, vertical_position and/or vertical_level.
			IF edge_id_attributes_row.form_of_way = 'single_carriage_way' AND edge_id_attributes_row.on_partition_border = false THEN
			    -- RAISE NOTICE 'Removing edge_id: %', edge_id_row.edge_id;
				BEGIN
					PERFORM topology.ST_RemEdgeModFace('tn_50_road_link_topology', edge_id_row.edge_id);
                    counter := counter + 1;
				EXCEPTION WHEN OTHERS THEN
				-- Isolated edges (having two dangling nodes) are processed twice and may have been removed already.
				END;
				PERFORM topology.ST_RemoveIsoNode('tn_50_road_link_topology', edge_id_row.node_id );		
			END IF;

        END IF;
    END LOOP;
	RAISE NOTICE 'Number of dangling lines removed: %.', counter;
    RAISE NOTICE 'Finished tn_50.remove_short_dangling_lines()...';
END;
$$;


CREATE OR REPLACE PROCEDURE tn_50.remove_single_edge_faces(p_max_area integer)
LANGUAGE PLPGSQL
AS $$
DECLARE
    single_edge_face_row RECORD;
    face_area DOUBLE PRECISION;
    edge_id_attributes_row RECORD;
    counter INTEGER := 1;
BEGIN
    RAISE NOTICE 'Running tn_50.remove_single_edge_faces()...';

        FOR single_edge_face_row IN
            WITH single_edge_face_ids AS (
		        SELECT f.face_id
		        FROM tn_50_road_link_topology.edge e, tn_50_road_link_topology.face f
		        WHERE f.face_id <> 0 -- Skip universal face
                AND (f.face_id = e.left_face OR f.face_id = e.right_face)
		        GROUP BY face_id
		        HAVING count(*) = 1
	        )
            SELECT e.edge_id, sf.face_id
		    FROM tn_50_road_link_topology.edge e, single_edge_face_ids sf
		    WHERE (sf.face_id = e.left_face OR sf.face_id = e.right_face)
        LOOP
		    SELECT ST_Area( ST_GetFaceGeometry('tn_50_road_link_topology', single_edge_face_row.face_id)) INTO face_area;
			IF face_area < p_max_area THEN

				SELECT form_of_way, on_partition_border INTO edge_id_attributes_row
                FROM tn_50_road_link_topology.edge_incl_attributes WHERE edge_id = single_edge_face_row.edge_id;

				-- Only remove if the road_link has form_of_way = 'single_carriage_way' and is not on partition border.
				-- We could make other choices based on form_of_way, functional_road_class, vertical_position and/or vertical_level.
				IF edge_id_attributes_row.form_of_way = 'single_carriage_way' AND edge_id_attributes_row.on_partition_border = false THEN
					--RAISE NOTICE 'Removing edge_id: %', single_edge_face_row.edge_id;
					PERFORM topology.ST_RemEdgeModFace('tn_50_road_link_topology', single_edge_face_row.edge_id);
                    counter := counter + 1;
				END IF;
			END IF;
        END LOOP;
    RAISE NOTICE 'Number of faces removed: %.', counter;
    RAISE NOTICE 'Finished tn_50.remove_single_edge_faces()...';
END;
$$;


CREATE OR REPLACE PROCEDURE tn_50.simplify_two_edge_faces(p_max_length INTEGER)
LANGUAGE PLPGSQL
AS $$
DECLARE
    edge_id_length_row RECORD;
    edge_id_attributes_row RECORD;
    counter INTEGER := 1;
BEGIN
    RAISE NOTICE 'Running tn_50.simplify_two_edge_faces()...';

    FOR edge_id_length_row IN
        WITH two_edge_face_ids AS (
		    SELECT f.face_id, count(*)
		    FROM tn_50_road_link_topology.edge e, tn_50_road_link_topology.face f
		    WHERE f.face_id = e.left_face OR f.face_id = e.right_face
		    GROUP BY face_id
		    HAVING count(*) = 2
	    ), two_edge_faces AS (
            SELECT e.edge_id, tf.face_id, ST_Length(e.geom) length, e.geom
		    FROM tn_50_road_link_topology.edge e, two_edge_face_ids tf
		    WHERE tf.face_id = e.left_face OR tf.face_id = e.right_face
        ), 	max_length_per_face AS (
		    SELECT face_id, MAX(length) length
		    FROM two_edge_faces
		    GROUP BY face_id
	    )
	    SELECT tf.edge_id, mf.length
	    FROM two_edge_faces tf, max_length_per_face mf
	    WHERE tf.length = mf.length
    LOOP
        IF edge_id_length_row.length < p_max_length THEN
			
            SELECT form_of_way, on_partition_border INTO edge_id_attributes_row
            FROM tn_50_road_link_topology.edge_incl_attributes WHERE edge_id = edge_id_length_row.edge_id;

			-- Only remove if the road_link has form_of_way = 'single_carriage_way' and is not on partition border.
			-- We could make other choices based on form_of_way, functional_road_class, vertical_position and/or vertical_level.
            IF edge_id_attributes_row.form_of_way = 'single_carriage_way' AND edge_id_attributes_row.on_partition_border = false THEN
			    --RAISE NOTICE 'Removing edge_id: %', edge_id_length_row.edge_id;
                PERFORM ST_RemEdgeModFace('tn_50_road_link_topology', edge_id_length_row.edge_id);
                counter := counter + 1;
			END IF;
        END IF;
    END LOOP;
    RAISE NOTICE 'Number of faces removed: %.', counter;
    RAISE NOTICE 'Finished tn_50.simplify_two_edge_faces()...';
END;
$$;


CREATE OR REPLACE FUNCTION normalize_angle(p_angle DOUBLE PRECISION)
RETURNS DOUBLE PRECISION LANGUAGE plpgsql AS $$
BEGIN
    IF p_angle >= 270 THEN
	    RETURN 360 - p_angle;
	END IF;
	IF p_angle >= 180 THEN
	    RETURN p_angle - 180;
	END IF;
	IF p_angle >= 90 THEN
	    RETURN 180 - p_angle;
	END IF;
	  
	RETURN p_angle;
END; $$;


DROP TYPE IF EXISTS direction_info;
CREATE TYPE direction_info AS (
	intersection_node_id INT,
	intersection_geom GEOMETRY,
	edge_id INT,
	geom GEOMETRY
);

CREATE OR REPLACE PROCEDURE tn_50.thin_tjunctions(p_max_length INTEGER, p_max_continuous_angle DOUBLE PRECISION default 5.0, p_min_perpendicular_angle DOUBLE PRECISION default 75.0)
LANGUAGE PLPGSQL
AS $$
DECLARE
    edge_id_row RECORD;
    tjunction_candidate_row RECORD;
    direction_node_id_row RECORD;
    direction_info_list direction_info[];
    direction_1 direction_info;
    direction_2 direction_info;
    direction_3 direction_info;
    nr_of_continuous_angles INTEGER;
    nr_of_perpendicular_angles INTEGER;
    tjunction_perpendicular_edge_id INTEGER;
    edge_id_attributes_row RECORD;
    counter INTEGER := 1;
BEGIN
    RAISE NOTICE 'Running tn_50.thin_tjunctions()...';

        -- All intersections with 3 connecting edges could be a T-junction
        DROP TABLE IF EXISTS tjunction_candidates;
	    CREATE TEMP TABLE tjunction_candidates AS
	        SELECT n.node_id, n.geom 
	        FROM tn_50_road_link_topology.node n, tn_50_road_link_topology.edge e
	        WHERE (n.node_id = e.start_node OR n.node_id = e.end_node) AND NOT (n.node_id = e.start_node AND n.node_id = e.end_node)
	        GROUP BY node_id
	        HAVING COUNT(*) = 3;
			
		-- Create direction points on all 3 connecting edges, these will be used to calculate angles
        DROP TABLE IF EXISTS tjunction_candidate_directions;
	    CREATE TEMP TABLE tjunction_candidate_directions AS 
		    SELECT i.node_id, e.edge_id, ST_Intersection(ST_ExteriorRing(ST_Buffer(i.geom,0.1)), e.geom) geom
	        FROM tjunction_candidates i, tn_50_road_link_topology.edge e
	        WHERE ST_Intersects(ST_ExteriorRing(ST_Buffer(i.geom,0.1)), e.geom);
		
		-- Table used for storing the node_id for T-junctions, along with the edge_id of the perpendicular edge
		DROP TABLE IF EXISTS tjunctions;
		CREATE TEMP TABLE tjunctions (
		    node_id INT,
            geom GEOMETRY,
            edge_id INT			
		);

        FOR tjunction_candidate_row IN
	        SELECT i.node_id, i.geom FROM tjunction_candidates i
		LOOP
			
            -- Store direction info for this junction
			SELECT ARRAY_AGG(row(
					tjunction_candidate_row.node_id,
					tjunction_candidate_row.geom,
					d.edge_id,
					d.geom)::direction_info)
				INTO direction_info_list
				FROM tjunction_candidate_directions d
				WHERE d.node_id = tjunction_candidate_row.node_id;
			
			-- Make sure that we have 3 directions, this should always be the case
			IF ARRAY_LENGTH(direction_info_list, 1) <> 3 THEN
			    -- RAISE NOTICE 'Number of directions is not equal to 3 for intersection_id: %', tjunction_candidate_row.node_id;
				CONTINUE;
			END IF;
			
			-- Make three pairs of directions and calculate the corresponding angles
			DROP TABLE IF EXISTS tjunction_candidate_angles;
			CREATE TEMP TABLE tjunction_candidate_angles (
			    angle double precision,
			    first_edge_id int,
			    second_edge_id int		
			);
			
			direction_1 := direction_info_list[1];
			direction_2 := direction_info_list[2];
			direction_3 := direction_info_list[3];
			
			INSERT INTO tjunction_candidate_angles VALUES (normalize_angle(degrees(ST_Angle(direction_1.geom, tjunction_candidate_row.geom, direction_2.geom))), direction_1.edge_id, direction_2.edge_id);
			INSERT INTO tjunction_candidate_angles VALUES (normalize_angle(degrees(ST_Angle(direction_1.geom, tjunction_candidate_row.geom, direction_3.geom))), direction_1.edge_id, direction_3.edge_id);
			INSERT INTO tjunction_candidate_angles VALUES (normalize_angle(degrees(ST_Angle(direction_2.geom, tjunction_candidate_row.geom, direction_3.geom))), direction_2.edge_id, direction_3.edge_id);
			
			-- Determine number of continuous and perpendicular angles
			SELECT COUNT(*) INTO nr_of_continuous_angles FROM tjunction_candidate_angles WHERE angle < p_max_continuous_angle;
			SELECT COUNT(*) INTO nr_of_perpendicular_angles FROM tjunction_candidate_angles WHERE angle > p_min_perpendicular_angle;
			
			-- Find T-juctions: one combination of directions should be close to continuous, the other two combinations should be somewhat to perpendicular
			IF nr_of_continuous_angles = 1 AND nr_of_perpendicular_angles = 2 THEN
			    --RAISE NOTICE 'T-junction found for node_id: %', tjunction_candidate_row.node_id;
				
				-- Find the non-continuous edge for this T-junction. Both angles for this direction are somewhat perpendicular
				WITH all_tjunction_edge_ids AS (
				    SELECT first_edge_id AS edge_id FROM tjunction_candidate_angles WHERE angle > p_min_perpendicular_angle
				UNION ALL
				    SELECT second_edge_id AS edge_id FROM tjunction_candidate_angles WHERE angle > p_min_perpendicular_angle
                )
                SELECT edge_id INTO tjunction_perpendicular_edge_id FROM all_tjunction_edge_ids GROUP BY edge_id HAVING COUNT(*) = 2;
				
				INSERT INTO tjunctions VALUES (tjunction_candidate_row.node_id, tjunction_candidate_row.geom, tjunction_perpendicular_edge_id);
			END IF;
		END LOOP;
		
		-- Remove edges which have two perpendicular connections to T-junctions
		FOR edge_id_row IN
		    WITH edge_ids AS (
                SELECT edge_id FROM tjunctions GROUP BY edge_id HAVING COUNT(*) = 2
            )
            SELECT edge_ids.edge_id, ST_Length(geom) length
            FROM edge_ids, tn_50_road_link_topology.edge e
            WHERE edge_ids.edge_id = e.edge_id
			AND e.left_face <> e.right_face -- Don't remove the edge in case of dead ends
        LOOP
            IF edge_id_row.length < p_max_length THEN

				SELECT form_of_way, on_partition_border INTO edge_id_attributes_row
                FROM tn_50_road_link_topology.edge_incl_attributes WHERE edge_id = edge_id_row.edge_id;

				-- Only remove if the road_link has form_of_way = 'single_carriage_way' and is not on partition border.
				-- We could make other choices based on form_of_way, functional_road_class, vertical_position and/or vertical_level.
				IF edge_id_attributes_row.form_of_way = 'single_carriage_way' AND edge_id_attributes_row.on_partition_border = false THEN
					-- RAISE NOTICE 'Removing edge_id: %', edge_id_row.edge_id;
					PERFORM ST_RemEdgeModFace('tn_50_road_link_topology', edge_id_row.edge_id);
                    counter := counter + 1;
				END IF;
            END IF;
    END LOOP;
    
    RAISE NOTICE 'Number of edges removed: %.', counter;
    RAISE NOTICE 'Finished tn_50.thin_tjunctions()...';
END;
$$;


CREATE OR REPLACE PROCEDURE tn_50.apply_topology_thinning()
LANGUAGE PLPGSQL
AS $$
DECLARE

BEGIN
    RAISE NOTICE 'Running tn_50.apply_topology_thinning()...';

	-- Delete objects that were removed during the thinning
	DELETE FROM tn_50.road_link WHERE objectid IN (
        SELECT objectid FROM tn_50.road_link_topo_partition WHERE objectid NOT IN (
		    SELECT DISTINCT objectid FROM tn_50_road_link_topology.edge_incl_attributes
        )
	);

    -- Update geometry for the remaining objects
	WITH merged_geom AS (
		SELECT objectid, ST_Force3D(ST_LineMerge(St_Collect(e.geom))) geom
		FROM tn_50_road_link_topology.edge_incl_attributes e
		GROUP BY objectid
	) 
	UPDATE tn_50.road_link rl
	SET geom = mg.geom
	FROM merged_geom mg
	WHERE rl.objectid = mg.objectid;

	DROP VIEW IF EXISTS tn_50_road_link_topology.edge_incl_attributes;
	DROP TABLE IF EXISTS tn_50_road_link_topology.edge_attributes_lookup;
    DROP TABLE IF EXISTS tn_50.road_link_topo_partition;

END;
$$;
