CREATE OR REPLACE PROCEDURE tn_50.initial_selection_railway_link()
LANGUAGE PLPGSQL
AS $$
BEGIN

  RAISE NOTICE 'Running tn_50.initial_selection_railway_link()...';

  DROP TABLE IF EXISTS tn_50.railway_link;
  CREATE TABLE tn_50.railway_link ( LIKE tn.railway_link INCLUDING INDEXES);

  INSERT INTO tn_50.railway_link
      SELECT *
      FROM tn.railway_link
      WHERE begin_lifespan_version < CURRENT_DATE AND end_lifespan_version is NULL
      AND type in ('train')
      AND country = 'nl' -- TEMPORARY FILTER
  ;

END; $$;
