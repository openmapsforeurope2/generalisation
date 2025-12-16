CREATE OR REPLACE PROCEDURE tn_50.generalize_marker_post()
LANGUAGE PLPGSQL
AS $$
DECLARE
  
BEGIN
  RAISE NOTICE 'Running tn_50.generalize_marker_post()...';

  DROP TABLE IF EXISTS tn_50.marker_post;
  CREATE TABLE tn_50.marker_post ( LIKE tn.marker_post INCLUDING INDEXES);

  INSERT INTO tn_50.marker_post
    WITH parameters (cluster_distance) AS (
        values (100) -- set cluster_distance here
    ),
    clustered_marker_posts AS (
        SELECT
            objectid,
            geom,
            ST_ClusterDBSCAN(geom, eps => cluster_distance, minpoints => 1) OVER() AS cluster_id
        FROM tn.marker_post, parameters
        WHERE begin_lifespan_version < CURRENT_DATE AND end_lifespan_version is NULL
        ORDER BY cluster_id DESC
    ),
    cluster_centroids AS
    (
        SELECT 
            cluster_id,
            ST_Centroid(ST_Collect(geom)) AS geom
        FROM clustered_marker_posts
        GROUP BY cluster_id
    ),
    marker_post_closest_to_cluster_centroid AS
    (
        SELECT DISTINCT ON (cmp.cluster_id)
            cmp.objectid,
            cmp.cluster_id,
            ST_Distance(cmp.geom, cc.geom) distance
        FROM clustered_marker_posts cmp, cluster_centroids cc
        WHERE cmp.cluster_id = cc.cluster_id
        ORDER BY cmp.cluster_id, distance
    )
    SELECT *
    FROM tn.marker_post
    WHERE objectid IN
        (
            SELECT objectid FROM marker_post_closest_to_cluster_centroid
        )
  ;

END; $$;
