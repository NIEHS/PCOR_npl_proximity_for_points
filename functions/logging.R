###########################################
# function : npl_proximity_points_logging
# Logging all script information
#
###########################################
xyz_logging <- 
  function(
    receptor_xyz_filepath,
    source_xyz_facilities_filepath,
    us_borders_filepath,
    buffer_distance_km,
    assessment_year,
    
    proximity_metrics,
    receptor_crs,
    projection_crs,
    check_near_us_border,
    add_all_to_output,
    
    write_log_to_file, 
    show_notes,
    print_log_to_console){
    
    if(write_log_to_file == TRUE) {
      
      if(logr::log_status() != "open") {
        logr::log_open(show_notes = FALSE)
      }
      logr::sep("Write log to file.",
                console = print_log_to_console)
      
      logr::sep("Set arguments for function 'get_npl_proximity_for_points()'.", 
                console = print_log_to_console)
      logr::put(stringr::str_c("receptor_xyz_filepath:", receptor_xyz_filepath, sep = " "), 
                console = print_log_to_console)
      logr::put(stringr::str_c("source_xyz_facilities_filepath:", source_xyz_facilities_filepath, sep = " "), 
                console = print_log_to_console)
      logr::put(stringr::str_c("us_borders_filepath:", us_borders_filepath, sep = " "), 
                console = print_log_to_console)
      logr::put(stringr::str_c("assessment_year:", assessment_year, sep = " "), 
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
      logr::put(stringr::str_c("show_notes:", show_notes, sep = " "), 
                console = print_log_to_console)
    }
    
  }