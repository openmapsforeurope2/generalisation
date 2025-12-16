-- =====================================================
-- Usage Examples for Topology Simplification
-- =====================================================

-- Example 1: Basic usage with defaults ('schema_name', 'table_name')
SELECT generalise_boundaries('au', 'administrative_unit_area_1');

-- Example 2: Custom output table
SELECT generalise_boundaries(
    input_schema := 'temp',
    input_table := 'au_selection',
    output_table := 'temp.my_generalised_output'
);

-- Example 3: Aggressive simplification
SELECT generalise_boundaries(au
    input_schema := 'temp',
    input_table := 'au_selection',
    coastline_tolerance := 5000,
    inland_tolerance := 100
);

-- Example 4: Process multiple tables
DO $$
DECLARE
    tbl TEXT;
    result TEXT;
BEGIN
    FOR tbl IN SELECT unnest(ARRAY['table1', 'table2', 'table3'])
    LOOP
        result := generalise_boundaries('temp', tbl);
        RAISE NOTICE '%', result;
    END LOOP;
END $$;

-- Example 5: Check results
SELECT 
    COUNT(*) as feature_count,
    ST_GeometryType(geom) as geom_type,
    AVG(ST_NPoints(geom)) as avg_vertices
FROM temp.au_selection_simplified
GROUP BY ST_GeometryType(geom);