CREATE OR REPLACE PROCEDURE tn_50.initial_selection_aerodrome()
LANGUAGE PLPGSQL
AS $$
BEGIN

  RAISE NOTICE 'Running tn_50.initial_selection_aerodrome...';

  DROP TABLE IF EXISTS tn_50.aerodrome_area;
  CREATE TABLE tn_50.aerodrome_area ( LIKE tn.aerodrome_area INCLUDING INDEXES);

  INSERT INTO tn_50.aerodrome_area
    WITH parameters (minimum_area) AS (
        values (100) -- set minimum_area here
    )
      SELECT tn.aerodrome_area.*
      FROM tn.aerodrome_area, parameters
      WHERE begin_lifespan_version < CURRENT_DATE AND end_lifespan_version is NULL
      AND ST_Area(geom) >= minimum_area
  ;

  DROP TABLE IF EXISTS tn_50.aerodrome_point;
  CREATE TABLE tn_50.aerodrome_point ( LIKE tn.aerodrome_point INCLUDING INDEXES);

  INSERT INTO tn_50.aerodrome_point
    SELECT tn.aerodrome_point.*
    FROM tn.aerodrome_point
    WHERE begin_lifespan_version < CURRENT_DATE AND end_lifespan_version is NULL
    AND objectid NOT IN (
      -- These aerodromes are already present as an area
      SELECT ap.objectid
      FROM tn.aerodrome_point AS ap
      JOIN tn_50.aerodrome_area AS aa
      ON ST_Contains(aa.geom, ap.geom)
      WHERE aa.aerodrome_type = ap.aerodrome_type
    )
  ;

END; $$;
