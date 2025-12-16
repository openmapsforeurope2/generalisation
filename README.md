# PostGIS Topology Simplification

A PostgreSQL/PostGIS function for polygon simplification that preserves topological relationships while applying different generalization algorithms to coastlines and inland boundaries.

## Features

- ✅ Topology-aware simplification (no gaps or overlaps)
- ✅ Different algorithms for coastlines vs inland boundaries
- ✅ Configurable tolerance parameters
- ✅ Batch processing support
- ✅ Automatic cleanup of temporary topologies

## Requirements

- PostgreSQL 12+
- PostGIS 3.0+
- PostGIS Topology extension

## Installation

### 1. Enable required extensions
```sql
CREATE EXTENSION IF NOT EXISTS postgis;
CREATE EXTENSION IF NOT EXISTS postgis_topology;
```

### 2. Install the function
```bash
psql -d your_database -f sql/install.sql
```

Or manually:
```sql
\i sql/functions/generalise_boundaries.sql
```

## Usage

### Basic Usage
```sql
SELECT generalise_boundaries('schema_name', 'table_name');
```

Output will be created in: `generalised.your_table_generalised`

### Custom Parameters
```sql
SELECT generalise_boundaries(
    input_schema := 'au',
    input_table := 'administrative_unit_area_1',
    geom_column := 'geom',
    output_table := 'temp.simplified_result',
    srid := 3035,
    coastline_tolerance := 2500,  -- square meters for coastlines
    inland_tolerance := 50,        -- meters for inland boundaries
    topology_tolerance := 0
);
```

## Parameters

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `input_schema` | TEXT | - | Schema containing input table (required) |
| `input_table` | TEXT | - | Input table name (required) |
| `geom_column` | TEXT | `'geom'` | Name of geometry column |
| `output_table` | TEXT | `NULL` | Full output path (e.g., `'schema.table'`) |
| `srid` | INTEGER | `3035` | Spatial Reference System ID |
| `coastline_tolerance` | NUMERIC | `2500` | Simplification tolerance for coastlines (square meters) |
| `inland_tolerance` | NUMERIC | `50` | Simplification tolerance for inland boundaries (meters) |
| `topology_tolerance` | NUMERIC | `0` | Topology creation tolerance |

## How It Works

1. **Creates topology** from input geometries
2. **Identifies edge types**: Coastlines (touching outer boundary) vs inland boundaries
3. **Applies different algorithms**:
   - Coastlines: `ST_SimplifyVW` (Visvalingam-Whyatt) - preserves shape better
   - Inland: `ST_SimplifyPreserveTopology` (Douglas-Peucker) - faster
4. **Rebuilds topology** with simplified edges
5. **Extracts polygons** from simplified topology

## Examples

See `sql/usage_examples.sql` for more examples.

### Process Multiple Tables
```sql
DO $$
DECLARE
    result TEXT;
BEGIN
    result := generalise_boundaries('schema1', 'table1');
    RAISE NOTICE '%', result;
    
    result := generalise_boundaries('schema2', 'table2');
    RAISE NOTICE '%', result;
END $$;
```

## Performance Tips

- Use appropriate SRID for your region
- Larger tolerance = faster processing but less detail
- Consider spatial indexes on input data
- For very large datasets, consider partitioning

## Troubleshoot

**Error: "Topology extension not found"**
```sql
CREATE EXTENSION postgis_topology;
```

**Error: "Topology already exists"**
- The function automatically drops existing topologies
- Check for manual cleanup: `SELECT topology.DropTopology('topology_name');`

**Poor results:**
- Adjust `coastline_tolerance` and `inland_tolerance` parameters
- Check input geometry validity: `SELECT ST_IsValid(geom) FROM your_table;`

## Contributing

Contributions welcome! Please:
1. Fork the repository
2. Create a feature branch
3. Test your changes
4. Submit a pull request

## License

MIT License - see LICENSE file

## Authors

- [Konan Pruiksma] - Initial work

## Acknowledgments

- Based on PostGIS Topology documentation
- Inspired by cartographic generalization techniques
```

---

### **.gitignore**
```
# OS files
.DS_Store
Thumbs.db

# Editor files
*.swp
*.swo
*~
.vscode/
.idea/

# Database dumps (don't commit data!)
*.sql.gz
*.dump
*.backup

# Logs
*.log

# Credentials (IMPORTANT!)
*.env
credentials.sql
secrets/