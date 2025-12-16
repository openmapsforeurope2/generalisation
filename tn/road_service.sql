CREATE OR REPLACE PROCEDURE tn_50.initial_selection_road_service()
LANGUAGE PLPGSQL
AS $$
BEGIN

  RAISE NOTICE 'Running tn_50.initial_selection_road_service()...';

  DROP TABLE IF EXISTS tn_50.road_service_area;
  CREATE TABLE tn_50.road_service_area ( LIKE tn.road_service_area INCLUDING INDEXES);
  
  INSERT INTO tn_50.road_service_area
      SELECT *
      FROM tn.road_service_area
      WHERE begin_lifespan_version < CURRENT_DATE AND end_lifespan_version is NULL;

END; $$;


CREATE OR REPLACE PROCEDURE tn_50.dissolve_road_service_areas()
LANGUAGE PLPGSQL
AS $$
BEGIN

  RAISE NOTICE 'Running tn_50.dissolve_road_service_areas()...';
  WITH parameters (buffer_distance) AS (
        values (1.5) -- set buffer_distance here
    ),
  dissolve_areas AS (
    SELECT
      row_number() OVER () AS dissolve_id,
      geom
    FROM (
      SELECT (ST_Dump(ST_Buffer((ST_Dump(ST_Union(ST_Buffer(rsa.geom, buffer_distance)))).geom, -AVG(buffer_distance), 'join=mitre'))).geom -- Buffer to prevent slivers in dissolved geometry and merge nearby areas
      AS geom FROM tn_50.road_service_area rsa, parameters
      GROUP BY rsa.type -- Can we dissolve all areas regardless of type and skip this groupby?
    ) diss
  ),
  join_table AS (
    SELECT rsa.objectid,
    ST_Area(rsa.geom) area,
    da.dissolve_id,
    ROW_NUMBER() OVER(PARTITION BY da.dissolve_id ORDER BY St_Area(rsa.geom) DESC) AS rank -- Order objects by area per dissolve group
    FROM tn_50.road_service_area rsa
    JOIN dissolve_areas da
    ON ST_Covers(ST_Buffer(da.geom, 1.5), rsa.geom)
  ),
  update_geom AS (
    UPDATE tn_50.road_service_area rsa
    SET geom = ST_Force3D(ST_Multi(da.geom))
    FROM join_table jt
    JOIN dissolve_areas da
    ON da.dissolve_id = jt.dissolve_id
    WHERE rsa.objectid = jt.objectid AND jt.rank = 1 -- Set the dissolved geometry on the object with the largest area in this dissolve group
  )
  DELETE FROM tn_50.road_service_area rsa WHERE objectid NOT IN (
    SELECT objectid FROM join_table WHERE rank = 1  -- Remove all other objects
  );

END; $$;



CREATE OR REPLACE PROCEDURE tn_50.remove_road_service_areas()
LANGUAGE PLPGSQL
AS $$
BEGIN

  RAISE NOTICE 'Running tn_50.remove_road_service_areas()...';
  WITH parameters (minimum_exterior_area, minimum_mic_radius) AS (
    values (1000, 10)
  ),
  size_attributes AS (
    SELECT
        rsa.objectid,
        ST_Area(ST_BuildArea(ST_ExteriorRing((ST_Dump(rsa.geom)).geom))) as exterior_area,
        mic.radius as mic_radius
    FROM tn_50.road_service_area rsa, parameters,
    LATERAL ST_MaximumInscribedCircle(rsa.geom) AS mic
  )
  DELETE FROM tn_50.road_service_area
  WHERE objectid IN (
      SELECT sa.objectid
      FROM size_attributes sa, parameters
      WHERE exterior_area < minimum_exterior_area
      OR sa.mic_radius < minimum_mic_radius
  );

END; $$;


CREATE OR REPLACE PROCEDURE tn_50.simplify_road_service_areas()
LANGUAGE PLPGSQL
AS $$
BEGIN

  RAISE NOTICE 'Running tn_50.simplify_road_service_areas()...';
  WITH parameters (buffer_distance, simplify_tolerance) AS (
        values (10, 4)
  )
  UPDATE tn_50.road_service_area
  SET geom = ST_Force3D(ST_Multi(ST_Simplify(ST_Buffer(ST_Buffer(geom, buffer_distance,'join=miter'), -buffer_distance, 'join=miter mitre_limit=1.0'), simplify_tolerance)))
  FROM parameters;
END; $$;
