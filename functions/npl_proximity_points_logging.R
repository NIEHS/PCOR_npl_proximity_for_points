###########################################
# function : npl_proximity_points_logging
# Logging all script information
#
###########################################
npl_proximity_points_logging <- 
  function(write_log_to_file, 
           show_notes,
           print_log_to_console,
           receptor_filepath,
           source_npl_facilities_filepath,
           us_borders_filepath,
           buffer_distance_km,
           proximity_metrics,
           receptor_crs,
           projection_crs,
           check_near_us_border,
           add_all_to_output,
           npl_year){
    if(write_log_to_file == TRUE) {
      
      logr::log_open(show_notes = FALSE)
      
      logr::sep("Set arguments for function 'get_npl_proximity_for_points()'.", 
                console = print_log_to_console)
      logr::put(stringr::str_c("receptor_filepath:", receptor_filepath, sep = " "), 
                console = print_log_to_console)
      logr::put(stringr::str_c("source_npl_facilities_filepath:", source_npl_facilities_filepath, sep = " "), 
                console = print_log_to_console)
      logr::put(stringr::str_c("us_borders_filepath:", us_borders_filepath, sep = " "), 
                console = print_log_to_console)
      logr::put(stringr::str_c("npl_year:", npl_year, sep = " "), 
                console = print_log_to_console)
      logr::put(stringr::str_c("buffer_distance_km:", buffer_distance_km, "km", sep = " "), 
                console = print_log_to_console)
      logr::put(stringr::str_c("proximity_metrics:", paste(proximity_metrics, sep = ", ", collapse = ", "), sep = " "), 
                console = print_log_to_console)
      logr::put(stringr::str_c("receptor_crs:", receptor_crs$wkt, sep = " "), 
                console = print_log_to_console)
      logr::put(stringr::str_c("projection_crs:", projection_crs$wkt, sep = " "), 
                console = print_log_to_console)
      logr::put(stringr::str_c("check_near_us_border:", check_near_us_border, sep = " "), 
                console = print_log_to_console)
      logr::put(stringr::str_c("add_all_to_output:", add_all_to_output, sep = " "), 
                console = print_log_to_console)
      logr::put(stringr::str_c("print_log_to_console:", print_log_to_console, sep = " "), 
                console = print_log_to_console)
      logr::put(stringr::str_c("write_log_to_file:", write_log_to_file, sep = " "), 
                console = print_log_to_console)
    }

  }