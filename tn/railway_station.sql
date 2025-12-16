CREATE OR REPLACE PROCEDURE tn_50.initial_selection_railway_station()
LANGUAGE PLPGSQL
AS $$
DECLARE
  
  partition_cursor CURSOR FOR
    SELECT objectid, country, national_code, label, national_level_code, geom
    FROM au.administrative_unit_area_3 WHERE country in ('nl')
    UNION
    SELECT objectid, country, national_code, label, national_level_code, geom
    FROM au.administrative_unit_area_4 WHERE country in ('fr', 'be');
  partition_record RECORD;
  
  p_national_level_code text;
  p_national_code text;
  p_label text;
  p_country text;
  p_geom geometry;

BEGIN

  RAISE NOTICE 'Running tn_50.initial_selection_railway_station()...';

  DROP TABLE IF EXISTS tn_50.railway_station_point ;
  CREATE TABLE tn_50.railway_station_point ( LIKE tn.railway_station_point INCLUDING INDEXES);

  OPEN partition_cursor;

  LOOP
    FETCH NEXT FROM partition_cursor INTO partition_record;
    EXIT WHEN NOT FOUND;

    p_national_level_code := partition_record.national_level_code;
    p_national_code := partition_record.national_code;
    p_geom := partition_record.geom;
    p_label := partition_record.label;
    p_country := partition_record.country;

    -- TEMPORARY FILTER
    --IF p_national_code = '75' AND p_national_level_code = '1003' THEN
    IF 1 = 1 THEN
      RAISE NOTICE 'Partition found for %', p_label;

      WITH parameters (buffer_distance) AS (
        values (5) -- set buffer_distance here
      ),  
      buffer AS (
        SELECT (ST_Dump( ST_Union( ST_Intersection(ST_Buffer(rl.geom, buffer_distance), p_geom )) )).geom
        FROM tn_50.railway_link rl, parameters
        WHERE rl.country = p_country
        AND ST_Intersects(rl.geom, p_geom)
	  )
      INSERT INTO tn_50.railway_station_point
        SELECT tn.railway_station_point.*
        FROM tn.railway_station_point
        WHERE objectid IN (
          SELECT objectid
          FROM tn.railway_station_point as rsp
          JOIN buffer
          ON ST_Within(rsp.geom, buffer.geom)
      );

    END IF;
  END LOOP;

END; $$;
