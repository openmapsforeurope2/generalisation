CREATE OR REPLACE PROCEDURE tn_50.initial_selection_ferry_crossing()
LANGUAGE PLPGSQL
AS $$
BEGIN

  RAISE NOTICE 'Running tn_50.initial_selection_ferry_crossing()...';

  DROP TABLE IF EXISTS tn_50.ferry_crossing;
  CREATE TABLE tn_50.ferry_crossing ( LIKE tn.ferry_crossing INCLUDING INDEXES);
  
  INSERT INTO tn_50.ferry_crossing
      SELECT *
      FROM tn.ferry_crossing
      WHERE begin_lifespan_version < CURRENT_DATE AND end_lifespan_version is NULL;

END; $$;
