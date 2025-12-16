CREATE OR REPLACE PROCEDURE tn_50.initial_selection_runway()
LANGUAGE PLPGSQL
AS $$
BEGIN

  RAISE NOTICE 'Running tn_50.initial_selection_runway()...';

  DROP TABLE IF EXISTS tn_50.runway_area;
  CREATE TABLE tn_50.runway_area ( LIKE tn.runway_area INCLUDING INDEXES);
  
  INSERT INTO tn_50.runway_area
      SELECT *
      FROM tn.runway_area
      WHERE begin_lifespan_version < CURRENT_DATE AND end_lifespan_version is NULL
  ;

END; $$;



CREATE OR REPLACE PROCEDURE tn_50.dissolve_runway_areas()
LANGUAGE PLPGSQL
AS $$
BEGIN

  RAISE NOTICE 'Running tn_50.dissolve_runway_areas()...';
  WITH parameters (buffer_distance) AS (
        values (0.1) -- set buffer_distance here
    ),
  dissolve_areas AS (
    SELECT
      row_number() OVER () AS dissolve_id,
      geom
    FROM (
      SELECT (ST_Dump(ST_Buffer((ST_Dump(ST_Union(ST_Buffer(ra.geom, buffer_distance)))).geom, -AVG(buffer_distance), 'join=mitre'))).geom -- Buffer to prevent slivers in dissolved geometry
      AS geom FROM tn_50.runway_area ra, parameters
    ) diss
  ),
  join_table AS (
    SELECT ra.objectid,
    ST_Area(ra.geom) area,
    da.dissolve_id,
    ROW_NUMBER() OVER(PARTITION BY da.dissolve_id ORDER BY St_Area(ra.geom) DESC) AS rank -- Order objects by area per dissolve group
    FROM tn_50.runway_area ra
    JOIN dissolve_areas da
    ON ST_Covers(ST_Buffer(da.geom,0.1), rsa.geom)
  ),
  update_geom AS (
    UPDATE tn_50.runway_area ra
    SET geom = ST_Force3D(ST_Multi(da.geom))
    FROM join_table jt
    JOIN dissolve_areas da
    ON da.dissolve_id = jt.dissolve_id
    WHERE ra.objectid = jt.objectid AND jt.rank = 1 -- Set the dissolved geometry on the object with the largest area in this dissolve group
  )
  DELETE FROM tn_50.runway_area ra WHERE objectid NOT IN (
    SELECT objectid FROM join_table WHERE rank = 1  -- Remove all other objects
  );

END; $$;
