
-- Drop generalization schemas
\i _database/drop_schemas.sql

-- Drop procedure in the tn schema
DROP PROCEDURE IF EXISTS tn.generalize();
