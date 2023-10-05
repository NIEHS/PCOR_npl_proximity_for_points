################################################################################
# Title: Functions to calculate proximity-based exposures for point receptors to 
# point source npl facilities Version 2.0
# Date updated: 2023-09-07
# get_npl_facility_proximity_for_points function - A work flow main function 
# invoke methods in a order to calculate proximity-based exposure metrics to 
# National Priorities List (NPL) Superfund sites in the United States, Puerto
# Rico, and other territories facilities in the United States (US) using data from 
# https://sedac.ciesin.columbia.edu/data/set/superfund-epa-national-priorities-list-ciesin-mod-v2.
# Script description: Contains the following functions to calculate proximity-
#   based exposures for point receptors to EPA NPL facilities:
#     - npl_proximity_points_input_validation
#.       - npl_proximity_points_input_validation
#     - npl_proximity_points_logging
#     - check_pointer_receipters
#        - check_point_receptor_near_border
#.    - trainsform_receptor_sf
#.    - check_us_border
#.    - transform_filter_npl_source_points
#.    - calculate_distance_npl_receptor
#     - npl_proximity_points_summary_metrics
#        - summary_of_point_sources_in_buffer_of_point_receptor

#     - nearest_distance_to_point_source_from_point_receptor
#     - summary_of_point_sources_in_buffer_of_point_receptor
# Packages required: logr, sf, tidyverse
# 
################################################################################

################################################################################
# Function: get_npl_facility_proximity_for_points
################################################################################

get_npl_facility_proximity_for_points_V2 <- 
  function(receptor_filepath = receptor_filepath,
           source_npl_facilities_filepath = source_npl_facilities_filepath,
           us_borders_filepath = us_borders_filepath,
           p_NPL_Status = p_NPL_Status,
           npl_year = npl_year,
           buffer_distance_km = 10,
           start_year = start_year,
           end_year = end_year,
           time_option = time_option
           ) {
    #Static variables 
    receptor_crs = sf::st_crs("EPSG:4269")
    projection_crs = sf::st_crs("ESRI:102008")
    proximity_metrics = c("distance_to_nearest", "count_in_buffer", "distance_in_buffer")
    check_near_us_border = TRUE
    add_all_to_output = TRUE
    print_log_to_console = TRUE
    write_log_to_file = TRUE
    print(source_npl_facilities_filepath)
    print("**************************")
    print("Validate all input variables")
    print("**************************")
    # Check argument format ------------------------------------------------------

    npl_proximity_points_input_validation(receptor_filepath = receptor_filepath,
                                          source_npl_facilities_filepath = source_npl_facilities_filepath,
                                          us_borders_filepath = us_borders_filepath,
                                          npl_year = npl_year,
                                          buffer_distance_km = 10,
                                          start_year = start_year,
                                          end_year = end_year,
                                          check_near_us_border = check_near_us_border)
    print("**************************")
    print("Success validate all input variables.")
    print("**************************")
    # Open log and print arguments------------------------------------------------
    print("**************************")
    print("Set loging and arguments:")
    print("**************************")
     
    npl_proximity_points_logging(write_log_to_file = write_log_to_file,
                                 show_notes = show_notes,
                                 print_log_to_console = print_log_to_console,
                                 receptor_filepath = receptor_filepath,
                                 source_npl_facilities_filepath = source_npl_facilities_filepath,
                                 us_borders_filepath = us_borders_filepath,
                                 buffer_distance_km = buffer_distance_km,
                                 proximity_metrics = proximity_metrics,
                                 receptor_crs = receptor_crs,
                                 projection_crs = projection_crs,
                                 check_near_us_border = check_near_us_border,
                                 add_all_to_output = add_all_to_output,
                                 npl_year = npl_year)
    
    print("**************************")
    print("Success loging")
    print("**************************")
    
    # Read and check receptor points file ----------------------------------------
    print("**************************")
    print("Read and check receptor points file. npl_proximity_points_input_validation.R")
    print("**************************")
      
    check_pointer_receipters(receptor_filepath = receptor_filepath, 
                             show_col_types = FALSE,
                             write_log_to_file,
                             print_log_to_console,
                             receptor_crs,
                             projection_crs,
                             check_near_us_border,
                             us_borders_filepath,
                             buffer_distance_km,
                             npl_year,
                             time_option
                             )
    print("**************************")
    print("Success read and check receptor points.")
    print("**************************")
    
    # Transform and filter source points -----------------------------------------
    print("**************************")
    print("Transform and filter receptor points. npl_proximity_points_transform_calculate.R")
    print("**************************")
    receptor_sf <- trainsform_receptor_sf ( receptor_filepath,
                                     write_log_to_file,
                                     print_log_to_console,
                                     npl_year,
                                     start_year = start_year,
                                     end_year = end_year,
                                     receptor_crs,
                                     check_near_us_border)
    
   
    print("Success transform and filter receptor points.")
    print("**************************")
    
    print("**************************")
    print("Check if point receptors are within buffer of border. npl_proximity_points_transform_calculate.R")
    print("**************************")
    output_df_list <- check_us_border (check_near_us_border,
                     us_borders_filepath,
                     buffer_distance_km,
                     write_log_to_file,
                     print_log_to_console,
                     projection_crs,
                     receptor_sf = receptor_sf)
    print("**************************")
    print("Success check if point receptors are within buffer of border.")
    print("**************************")
    print("**************************")
    print("Transform and filter source points npl_proximity_points_transform_calculate.R")
    print("**************************")
    source_npl_sf <- transform_filter_npl_source_points(source_npl_facilities_filepath,
                                   p_NPL_Status,
                                   print_log_to_console,
                                   projection_crs,
                                   receptor_sf)
    
    print("Success transform and filter source points.")
    print("**************************")
    print("**************************")
    print("Calculate distance to nearest NPL from point receptor npl_proximity_points_transform_calculate.R")
    print("**************************")
   
    output_df_list <- calculate_distance_npl_receptor(proximity_metrics,
                                    receptor_sf,
                                    source_npl_sf,
                                    projection_crs,
                                    check_near_us_border,
                                    output_df_list
                                    )
    print("**************************")
    print("Success Calculate distance to nearest NPL facility from point receptor.")
    print("**************************")
    
    print("**************************")
    print("Calculate NPL facility summary metrics within buffer distance of point receptor npl_proximity_points_summary_metrics.R")
    print("**************************")
    
    output_df_list <- npl_proximity_points_summary_metrics(
                                         proximity_metrics,
                                         receptor_sf = receptor_sf,
                                         source_npl_sf,
                                         summary_metrics = summary_metrics,
                                         buffer_distance_km = buffer_distance_km,
                                         projection_crs = projection_crs,
                                         print_log_to_console = print_log_to_console,
                                         write_log_to_file = write_log_to_file,
                                         add_all_to_output = add_all_to_output,
                                         #receptor_df,
                                         check_near_us_border,
                                         output_df_list = output_df_list
                                         )

    print("**************************")
    print("Success Calculate NPL facility summary metrics within buffer distance of point receptor npl_proximity_points_summary_metrics.R")
    print("**************************")
    
    # Return data frame with exposures by receptor id---------------------------
    receptor_df <- readr::read_csv(receptor_filepath, show_col_types = FALSE) 
    #  if add_all_to_output is TRUE.
    #  It adds the receptor_df datafram to the beginning of the output_df_list
    #. list. This is achieved by appending receptor_df to the list at the position specified by after = 1.
    
    if(add_all_to_output == TRUE) {
      output_df_list <- append(list(receptor_df), output_df_list, after = 1)
    }
    # The purrr::reduce function is used to iteratively apply a function to 
    # elements of a list.In this context, applying the dplyr::left_join function
    # to dataframes within output_df_list, joining them by the "id" column.
    # and then sorts the resulting dataframe (output_df) by the "id" column.
    
    output_df <-  purrr::reduce(output_df_list, dplyr::left_join, by = "id") %>%
      dplyr::arrange(id)

    # Print status update
    if(write_log_to_file == TRUE) {
      logr::sep("Completed NPL facility proximity metric calculations.",
                console = print_log_to_console)
    } else if(print_log_to_console == TRUE) {
      message("Completed NPL facility proximity metric calculations.")
    }
    
    # Close log
    if(write_log_to_file == TRUE) {
      logr::log_close()
    }
    
    #print('dim(Final output_df):',dim(output_df))
    # Return data frame with aircraft facility proximity-based exposures for receptors
    return (output_df)
  }
