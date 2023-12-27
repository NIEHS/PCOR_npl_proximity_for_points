################################################################################
#
#Function Definition:
#  Function Name: xyz_facility_proximity_for_points
#
#Parameters:
#  File paths for receptors, source facilities, and US borders.
#  buffer_distance_km: Distance parameter for proximity calculation.
#  assessment_year: Year for the assessment.
#  time_option: Options for time frame consideration.
#  Flags for various options like checking proximity to US border, adding all data to output, 
#. logging to console, and writing logs to a file.
################################################################################
xyz_facility_proximity_for_points <- 
  function(receptor_xyz_filepath,
           source_xyz_facilities_filepath = source_xyz_facilities_filepath,
           us_borders_filepath = us_borders_filepath,
           buffer_distance_km,
           assessment_year = assessment_year,
           time_option = time_option,
           check_near_us_border = TRUE,
           add_all_to_output = TRUE,
           print_log_to_console = TRUE,
           write_log_to_file = TRUE
  ) {
    # Setting Static Variables:
      # Defines coordinate reference systems (CRS) for receptors and projection.
      # Specifies a list of proximity metrics (distance_to_nearest, count_in_buffer, distance_in_buffer).
  
    local_receptor_crs = sf::st_crs("EPSG:4269")
    projection_crs = sf::st_crs("ESRI:102008")
    proximity_metrics = c("distance_to_nearest", "count_in_buffer", "distance_in_buffer")
    show_notes1 <- FALSE
    add_nearest_source = TRUE
    print("**************************")
    print("Set loging and arguments:")
    print("**************************")
    # Logging Setup:
    
      #Initializes logging with parameters like file paths, CRS, and other settings.
      #Prints messages indicating the start of logging and successful initialization.
    
    xyz_logging(
      receptor_xyz_filepath = receptor_xyz_filepath,
      source_xyz_facilities_filepath = source_xyz_facilities_filepath,
      us_borders_filepath = us_borders_filepath,
      buffer_distance_km = buffer_distance_km,
      assessment_year = assessment_year,

      proximity_metrics = proximity_metrics,
      receptor_crs = local_receptor_crs,
      projection_crs = projection_crs,
      check_near_us_border = check_near_us_border,
      add_all_to_output = add_all_to_output,

      write_log_to_file = write_log_to_file,
      show_notes = show_notes1,
      print_log_to_console = print_log_to_console
    )

    print("**************************")
    print("Success loging")
    print("**************************")
    
    print("**************************")
    print("Validate all input variables")
    print("**************************")
    
    # Input Validation:
      
      #Validates all input variables and files, ensuring they meet the expected formats and criteria. 
      #This includes checking the receptor file path, source facility file path, buffer distance, 
      #assessment year, and other parameters.
    
    assm_year <- validation_v4(receptor_xyz_filepath = receptor_xyz_filepath,
                                          source_xyz_facilities_filepath = source_xyz_facilities_filepath,
                                          us_borders_filepath = us_borders_filepath,
                                          buffer_distance_km,
                                          assessment_year = assessment_year,
                                          check_near_us_border = check_near_us_border,
                                          time_option,
                                          local_receptor_crs,
                                          projection_crs = projection_crs,
                                          proximity_metrics = proximity_metrics,
                                          print_log_to_console,
                                          write_log_to_file,
                                          add_all_to_output
                                          )
    # print("**************************")
    
    print("Success validate all input variables.")
    print("**************************")
    
    file_validate (
      receptor_xyz_filepath,
      source_xyz_facilities_filepath,
      time_option,
      year = assm_year,
      write_log_to_file = TRUE,
      print_log_to_console = TRUE
    ) 
    
    print("Success validate all input files.")
    print("++++++++++++++++++++++++++++++++++")
    # Proximity Calculation:
    
      # Calls another function xyz_proximity_points_calculate with all relevant parameters.
      # This function presumably performs the actual proximity calculations between 
      # receptors and facilities based on the provided criteria and settings.
    
    output_df_list <- xyz_proximity_points_calculate(
      receptor_xyz_filepath,
      source_xyz_facilities_filepath,
      us_borders_filepath,
      buffer_distance_km,
      assessment_year = assm_year,
      check_near_us_border = check_near_us_border,
      time_option,
      local_receptor_crs,
      projection_crs= projection_crs,
      proximity_metrics= proximity_metrics,
      add_nearest_source = TRUE,
      add_all_to_output = TRUE,
      write_log_to_file,
      print_log_to_console,
      show_notes)
    # Output Handling (Commented Out):
    
    # The script includes commented-out lines for writing the output to a CSV file 
    # and returning the output data frame list.
    #readr::write_csv(output_df_list,
     #              file ="./output/output_xyz_proximity_metrics_V44.csv")
    #output_df_list
    
  }

xyz_facility_proximity_for_points