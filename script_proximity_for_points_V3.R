################################################################################
# Function: get_npl_facility_proximity_for_points
################################################################################

get_proximity_for_points_V3 <- 
  function(receptor_xyz_filepath,
           source_xyz_facilities_filepath = source_xyz_facilities_filepath,
           us_borders_filepath = us_borders_filepath,
           buffer_distance_km,
           assessment_year = assessment_year,
           #start_year = start_year,
           #end_year = end_year,
           time_option = time_option,
           check_near_us_border = TRUE,
           add_all_to_output = TRUE,
           print_log_to_console = TRUE,
           write_log_to_file = TRUE
           ) {
    
    #Static variables 
    receptor_crs_local = sf::st_crs("EPSG:4269")
    projection_crs = sf::st_crs("ESRI:102008")
    proximity_metrics = c("distance_to_nearest", "count_in_buffer", "distance_in_buffer")
    show_notes = FALSE
    add_nearest_source = TRUE
    print("**************************")
    print("Set loging and arguments:")
    print("**************************")
    # Open log and print arguments------------------
    xyz_proximity_points_logging(
                                 receptor_xyz_filepath = receptor_xyz_filepath,
                                 source_xyz_facilities_filepath = source_xyz_facilities_filepath,
                                 us_borders_filepath = us_borders_filepath,
                                 buffer_distance_km = buffer_distance_km,
                                 assessment_year = assessment_year,

                                 proximity_metrics = proximity_metrics,
                                 receptor_crs = receptor_crs_local,
                                 projection_crs = projection_crs,
                                 check_near_us_border = check_near_us_border,
                                 add_all_to_output = add_all_to_output,

                                 write_log_to_file = write_log_to_file,
                                 show_notes = show_notes,
                                 print_log_to_console = print_log_to_console
                                 )

    print("**************************")
    print("Success loging")
    print("**************************")
    
    print("**************************")
    print("Validate all input variables")
    print("**************************")
    # Check argument format ------------------------------------------------------
    
    #sue = sf::st_crs("EPSG:4269")
    #projection_crs = sf::st_crs("ESRI:102008")
    #proximity_metrics = c("distance_to_nearest", "count_in_buffer", "distance_in_buffer")
    if(class(receptor_crs) != "crs") {
      stop("******Optional argument 'receptor_crs' must be of class 'crs'.")
    }
    
    assm_year <- xyz_proximity_points_input_validation(receptor_xyz_filepath = receptor_xyz_filepath,
                                          source_xyz_facilities_filepath = source_xyz_facilities_filepath,
                                          us_borders_filepath = us_borders_filepath,
                                          buffer_distance_km,
                                          assessment_year = assessment_year,
                                          check_near_us_border = check_near_us_border,
                                          time_option,
                                          #receptor_crs = sf::st_crs("EPSG:4269"),
                                          receptor_crs = receptor_crs_local,
                                          projection_crs = projection_crs,
                                          proximity_metrics = proximity_metrics,
                                          print_log_to_console,
                                          write_log_to_file,
                                          add_all_to_output
                                          )
    print("**************************")
    #assm_year <- '2018'
    print(assm_year)
    print("Success validate all input variables.")
    print("**************************")
    
    output_df_list <- xyz_proximity_points_calculate(
                                                      receptor_xyz_filepath,
                                                      source_xyz_facilities_filepath,
                                                      us_borders_filepath,
                                                      buffer_distance_km,
                                                      assessment_year = assm_year,
                                                      check_near_us_border = check_near_us_border,
                                                      time_option,
                                                      receptor_crs = receptor_crs_local,
                                                      projection_crs= projection_crs,
                                                      proximity_metrics= proximity_metrics,
                                                      add_nearest_source = TRUE,
                                                      add_all_to_output = TRUE,
                                                      write_log_to_file,
                                                      print_log_to_console,
                                                      show_notes)
    # Write exposures to CSV
    readr::write_csv(output_df_list, 
                     file ="./output/output_xyz_proximity_metrics_Vxx.csv")
    
    output_df_list
    
  }
#xyz_proximity_metrics_v3