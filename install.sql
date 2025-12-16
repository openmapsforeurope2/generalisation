-- =====================================================
-- Installation Script
-- =====================================================
-- Run this to install all functions in your database

\echo 'Installing PostGIS Topology Simplification Functions...'

-- Check for PostGIS and topology extension
DO $$
BEGIN
    IF NOT EXISTS (SELECT 1 FROM pg_extension WHERE extname = 'postgis') THEN
        RAISE EXCEPTION 'PostGIS extension not found. Please install PostGIS first.';
    END IF;
    
    IF NOT EXISTS (SELECT 1 FROM pg_extension WHERE extname = 'postgis_topology') THEN
        RAISE EXCEPTION 'PostGIS Topology extension not found. Please install: CREATE EXTENSION postgis_topology;';
    END IF;
END $$;

\echo 'Loading functions...'
\i sql/functions/generalise_boundaries.sql

\echo 'Installation complete!'
\echo 'Run sql/examples/usage_examples.sql to see usage examples.'