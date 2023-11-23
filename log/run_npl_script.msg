Error in st_transform.sfc(st_geometry(x), crs, ...) : cannot transform sfc object with missing crs 
Traceback: 
 [1] "8: stop(\"cannot transform sfc object with missing crs\")"                                                                                  
 [2] "7: st_transform.sfc(st_geometry(x), crs, ...)"                                                                                              
 [3] "6: st_transform(st_geometry(x), crs, ...)"                                                                                                  
 [4] "5: st_transform.sf(receptor_sf, crs = projection_crs)"                                                                                      
 [5] "4: sf::st_transform(receptor_sf, crs = projection_crs) at check_point_receptor_near_border.R#76"                                            
 [6] "3: check_point_receptor_near_border(receptor_sf, border_sf = us_borders_sf, "                                                               
 [7] "       buffer_distance_km = buffer_distance_km, projection_crs = projection_crs, "                                                          
 [8] "       print_log_to_console = print_log_to_console, write_log_to_file = write_log_to_file) at npl_proximity_points_transform_calculate.R#89"
 [9] "2: check_us_border(check_near_us_border = TRUE, us_borders_filepath, "                                                                      
[10] "       buffer_distance_km, write_log_to_file, print_log_to_console, "                                                                       
[11] "       projection_crs, receptor_sf = receptor_sf) at script_npl_proximity_for_points_V2.R#134"                                              
[12] "1: get_npl_facility_proximity_for_points_V2(receptor_filepath = \"./input/input_receptor.csv\", "                                           
[13] "       source_npl_facilities_filepath = \"./input/epa-national-priorities-list-ciesin-mod-v2-2014.xls\", "                                  
[14] "       us_borders_filepath = \"./input/input_us_borders.rds\", buffer_distance_km = 10, "                                                   
[15] "       p_NPL_Status = \"Deleted from the Final NPL\", assessment_year = \"2017-10-01\", "                                                   
[16] "       time_option = \"single_year_only\")"                                                                                                 

