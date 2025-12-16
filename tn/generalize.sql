CREATE OR REPLACE PROCEDURE tn_50.generalize()
LANGUAGE PLPGSQL
AS $$
BEGIN
  RAISE NOTICE 'Running tn_50.generalize()...';
  
  CALL tn_50.initial_selection_road_link();
  CALL tn_50.generalize_road_link();

  CALL tn_50.generalize_marker_post();

  CALL tn_50.initial_selection_railway_link();

  CALL tn_50.initial_selection_port();

  CALL tn_50.initial_selection_runway_area();
  CALL tn_50.dissolve_runway_areas();

  CALL tn_50.initial_selection_road_service();
  CALL tn_50.dissolve_road_service_areas();
  CALL tn_50.remove_road_service_areas();
  CALL tn_50.simplify_road_service_areas();

  CALL tn_50.initial_selection_railway_station();

  CALL tn_50.initial_selection_aerodrome();
  CALL tn_50.initial_selection_ferry_crossing();

END; $$;
