CREATE OR REPLACE PROCEDURE tn_50.initial_selection_port()
LANGUAGE PLPGSQL
AS $$
BEGIN

  RAISE NOTICE 'Running tn_50.initial_selection_port()...';

  DROP TABLE IF EXISTS tn_50.port_area;
  CREATE TABLE tn_50.port_area ( LIKE tn.port_area INCLUDING INDEXES);

  WITH parameters(minimum_area) AS (
      values (100) 
  )
  INSERT INTO tn_50.port_area
      SELECT tn.port_area.*
      FROM tn.port_area, parameters
      WHERE begin_lifespan_version < CURRENT_DATE AND end_lifespan_version is NULL
      AND ST_Area(geom) >= minimum_area
  ;

  DROP TABLE IF EXISTS tn_50.port_point;
  CREATE TABLE tn_50.port_point ( LIKE tn.port_point INCLUDING INDEXES);

  INSERT INTO tn_50.port_point
    SELECT tn.port_point.*
    FROM tn.port_point
    WHERE begin_lifespan_version < CURRENT_DATE AND end_lifespan_version is NULL
    AND objectid NOT IN (
      -- These ports are already present as an area
      SELECT pp.objectid
      FROM tn.port_point AS pp
      JOIN tn_50.port_area AS pa
      ON ST_Contains(pa.geom, pp.geom)
      WHERE pa.label = pp.label
    )
  ;

END; $$;
