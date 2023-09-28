################################################################################
# Title: Function to calculate proximity-based exposure metrics for point 
#   receptors to point source aircraft landing facilities based on FAA data
# Date modified: 2023-08-15
# Script description: Contains function to calculate proximity-based exposure 
#   metrics for point receptors to aircraft landing facilities for years 1981-
#   2020. Required function arguments are the file paths to receptor and source 
#   data and year for exposure assessment. Function returns a data frame with 
#   proximity-based metrics for each receptor.
# Packages required: logr, sf, tidyverse 
# Other functions required: check_point_receptor_format, 
#   check_point_receptor_near_border, 
#   nearest_distance_to_point_source_from_point_receptor,
#   summary_of_point_sources_in_buffer_of_point_receptor
################################################################################

################################################################################
# Function: get_aircraft_facility_proximity_for_points
################################################################################
get_aircraft_facility_proximity_for_points <- 
  function(receptor_filepath = NULL,
           source_aircraft_facilities_filepath = NULL,
           us_borders_filepath = NULL,
           aircraft_year = NULL,
           buffer_distance_km = 10,
           receptor_crs = sf::st_crs("EPSG:4269"),
           projection_crs = sf::st_crs("ESRI:102008"),
           aircraft_facility_type = c("airport", "heliport", "seaplane base",
                                      "gliderport", "ultralight", "balloonport"),
           proximity_metrics = c("distance_to_nearest", "count_in_buffer",
                                 "distance_in_buffer"),
           check_near_us_border = TRUE,
           add_all_to_output = TRUE,
           print_log_to_console = TRUE,
           write_log_to_file = TRUE) {
    
    # Check argument format ------------------------------------------------------
    
    if(is.null(receptor_filepath)) {
      stop("Required argument 'receptor_filepath' is missing.")
    } else if(!file.exists(receptor_filepath)) {
      stop("'receptor_filepath' must be a valid file path.")
    } else if((stringr::str_sub(receptor_filepath, -3, -1) != "csv") & 
              (stringr::str_sub(receptor_filepath, -3, -1) != "CSV")) {
      stop("'receptor_filepath' must be a valid file path for a csv file.")
    }
    
    if(is.null(source_aircraft_facilities_filepath)) {
      stop("Required argument 'source_aircraft_facilities_filepath' is missing.")
    } else if(!file.exists(source_aircraft_facilities_filepath)) {
      stop("'source_aircraft_facilities_filepath' must be a valid file path.")
    } else if((stringr::str_sub(source_aircraft_facilities_filepath, -3, -1) != "rds") & 
              (stringr::str_sub(source_aircraft_facilities_filepath, -3, -1) != "RDS")) {
      stop("'source_aircraft_facilities_filepath' must be a valid file path for a rds file.")
    }
    
    if(check_near_us_border == TRUE) {
      if(is.null(us_borders_filepath)) {
        stop("Required argument 'us_borders_filepath' is missing.")
      } else if(!file.exists(us_borders_filepath)) {
        stop("'us_borders_filepath' must be a valid file path.")
      } else if((stringr::str_sub(us_borders_filepath, -3, -1) != "rds") & 
                (stringr::str_sub(us_borders_filepath, -3, -1) != "RDS")) {
        stop("'us_borders_filepath' must be a valid file path for a rds file.")
      }
    }
    
    if(is.null(aircraft_year)) {
      stop("Required argument 'aircraft_year' is missing.")
    } else if(!is.numeric(aircraft_year)) {
      stop("Required argument 'aircraft_year' must have numeric format.")
    } else if((aircraft_year < 1981) | (aircraft_year > 2020)) {
      stop("Required argument 'aircraft_year' must have 4-digit 'YYYY' format and be within range 1981 to 2020.")
    }
    
    if(class(buffer_distance_km) != "numeric") {
      stop("Optional argument 'buffer_distance_km' must have numeric format.")
    } else if((buffer_distance_km > 1000) | (buffer_distance_km < 0.001)) {
      stop("Optional argument 'buffer_distance_km' must be in units of kilometers and must be within range 0.001 km to 1000 km.")
    }
    
    if(class(receptor_crs) != "crs") {
      stop("Optional argument 'receptor_crs' must be of class 'crs'.")
    }
    
    if(class(projection_crs) != "crs") {
      stop("Optional argument 'projection_crs' must be of class 'crs'.")
    } else if(sf::st_is_longlat(projection_crs) == TRUE) {
      stop("Optional argument 'projection_crs' must be a projected coordinate reference system.")
    }
    
    for(i in 1:length(aircraft_facility_type)) {
      if((aircraft_facility_type[i] %in% c("airport", "heliport", "seaplane base",
                                           "gliderport", "ultralight", "balloonport")) == FALSE) {
        stop("Optional argument 'aircraft_facility_type' must only include the following: airport, heliport, seaplane base, gliderport, ultralight, balloonport)." )
      }
    } 
    
    for(i in 1:length(proximity_metrics)) {
      if((proximity_metrics[i] %in% c("distance_to_nearest", "count_in_buffer", "distance_in_buffer")) == FALSE) {
        stop("Optional argument 'proximity_metrics' must only include the following: distance_to_nearest, count_in_buffer, distance_in_buffer." )
      }
    } 
    
    if(!is.logical(check_near_us_border)) {
      stop("Optional argument 'check_near_us_border' must be logical (i.e., TRUE or FALSE).")
    }
    
    if(!is.logical(add_all_to_output)) {
      stop("Optional argument 'add_all_to_output' must be logical (i.e., TRUE or FALSE).")
    }
    
    if(!is.logical(print_log_to_console)) {
      stop("Optional argument 'print_log_to_console' must be logical (i.e., TRUE or FALSE).")
    }
    
    if(!is.logical(write_log_to_file)) {
      stop("Optional argument 'write_log_to_file' must be logical (i.e., TRUE or FALSE).")
    }
    
    # Open log and print arguments------------------------------------------------
    
    if(write_log_to_file == TRUE) {
      
      logr::log_open(show_notes = FALSE)
      
      logr::sep("Set arguments for function 'get_aircraft_facility_proximity_for_points()'.", 
                console = print_log_to_console)
      logr::put(stringr::str_c("receptor_filepath:", receptor_filepath, sep = " "), 
                console = print_log_to_console)
      logr::put(stringr::str_c("source_aircraft_facilities_filepath:", source_aircraft_facilities_filepath, sep = " "), 
                console = print_log_to_console)
      logr::put(stringr::str_c("us_borders_filepath:", us_borders_filepath, sep = " "), 
                console = print_log_to_console)
      logr::put(stringr::str_c("aircraft_year:", aircraft_year, sep = " "), 
                console = print_log_to_console)
      logr::put(stringr::str_c("buffer_distance_km:", buffer_distance_km, "km", sep = " "), 
                console = print_log_to_console)
      logr::put(stringr::str_c("aircraft_facility_type:", paste(aircraft_facility_type, sep = ", ", collapse = ", "), sep = " "), 
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
    
    # Read and check receptor points file ----------------------------------------
    
    receptor_df <- readr::read_csv(receptor_filepath, show_col_types = FALSE) 
    
    check_point_receptor_format(receptor = receptor_df, 
                                year = aircraft_year,
                                time_option = "single_year_only",
                                print_log_to_console = print_log_to_console, 
                                write_log_to_file = write_log_to_file)
    
    receptor_sf <- sf::st_as_sf(receptor_df, coords = c('longitude','latitude'), 
                                crs = receptor_crs)
    
    # Check if point receptors are within buffer of border -----------------------
    
    if(check_near_us_border == TRUE) {
      
      us_borders_sf <- readr::read_rds(us_borders_filepath) 
      
      if(("sf" %in% class(us_borders_sf)) == FALSE) {
        stop("Required argument 'us_borders_filepath' must lead to a simple features object.")
      }
      if((nrow(us_borders_sf) != 30) | (ncol(us_borders_sf) != 2)) {
        stop("'Required argument 'us_borders_filepath' must lead to a simple features object with 30 rows and 2 columns.")
      }
      
      receptor_border_check_df <- 
        check_point_receptor_near_border(receptor_sf = receptor_sf,
                                         border_sf = us_borders_sf,
                                         buffer_distance_km = buffer_distance_km,
                                         projection_crs = projection_crs,
                                         print_log_to_console = print_log_to_console, 
                                         write_log_to_file = write_log_to_file) 
      
      output_receptor_border_check_df <- receptor_border_check_df %>% 
        dplyr::select(id, within_border_buffer)
      
      output_df_list <- list(output_receptor_border_check_df) 
    }
    
    # Transform and filter source points -----------------------------------------
    
    source_aircraft_sf <- readr::read_rds(source_aircraft_facilities_filepath) 
    
    if(("sf" %in% class(source_aircraft_sf)) == FALSE) {
      stop("'Required argument 'source_aircraft_facilities_filepath' must lead to a simple features object.")
    }
    if((nrow(source_aircraft_sf) != 19628) | (ncol(source_aircraft_sf) != 5)) {
      stop("'Required argument 'source_aircraft_facilities_filepath' must lead to a simple features object with 19628 rows and 6 columns.")
    }
    
    if(sf::st_crs(source_aircraft_sf) != projection_crs) {
      source_aircraft_sf <- sf::st_transform(source_aircraft_sf, crs = projection_crs)
    }
    if(sf::st_crs(receptor_sf) != projection_crs) {
      receptor_sf <- sf::st_transform(receptor_sf, crs = projection_crs)
    }
    
    source_aircraft_sf <- source_aircraft_sf %>% 
      dplyr::filter(fac_type %in% aircraft_facility_type,
                    year_activation <= aircraft_year)
    
    # Calculate distance to nearest aircraft facility from point receptor ------------------
    
    if(("distance_to_nearest" %in% proximity_metrics) == TRUE) {
      receptor_nearest_distance_df <- 
        nearest_distance_to_point_source_from_point_receptor(receptor_sf = receptor_sf, 
                                                             source_sf = source_aircraft_sf,
                                                             projection_crs = projection_crs,
                                                             add_nearest_source = TRUE,
                                                             print_log_to_console = TRUE,
                                                             write_log_to_file = TRUE)
      
      output_receptor_nearest_distance_df <- receptor_nearest_distance_df %>% 
        dplyr::select(id, starts_with("nearest_")) %>%
        dplyr::rename_with(function(x){paste0("aircraft_", x)}, .cols = !id) %>%
        dplyr::mutate(aircraft_nearest_distance = round(aircraft_nearest_distance, digits = 3))
      
      if(check_near_us_border == FALSE) {
        output_df_list <- list(output_receptor_nearest_distance_df) 
      } else {
        output_df_list <- append(output_df_list, list(output_receptor_nearest_distance_df))
      }
    }
    
    # Calculate aircraft facility summary metrics within buffer distance of point receptor -----------
    
    if((("count_in_buffer" %in% proximity_metrics) |
        ("distance_in_buffer" %in% proximity_metrics)) == TRUE) {
      
      summary_metrics <- proximity_metrics[which((proximity_metrics == "distance_in_buffer") | 
                                                   (proximity_metrics == "count_in_buffer"))]
      
      receptor_summary_distance_metrics_df <- 
        summary_of_point_sources_in_buffer_of_point_receptor(receptor_sf = receptor_sf,
                                                             source_sf = source_aircraft_sf,
                                                             summary_metrics = summary_metrics,
                                                             buffer_distance_km = buffer_distance_km,
                                                             projection_crs = projection_crs,
                                                             print_log_to_console = print_log_to_console,
                                                             write_log_to_file = write_log_to_file)
      
      summary_distance_metrics_names <- c("count_in_buffer", "mean_distance_in_buffer",
                                          "log_mean_distance_in_buffer", "p25_distance_in_buffer",
                                          "p50_distance_in_buffer", "p75_distance_in_buffer")
      
      output_receptor_summary_distance_metrics_df <- receptor_summary_distance_metrics_df %>% 
        dplyr::select(id, tidyselect::any_of(summary_distance_metrics_names)) %>%
        dplyr::mutate(dplyr::across(tidyselect::any_of(summary_distance_metrics_names), ~ round(.x, 3))) %>%
        dplyr::rename_with(function(x){paste0("aircraft_", x)}, .cols = !id) 
           
      if((check_near_us_border == FALSE) &
         (("distance_to_nearest" %in% proximity_metrics) == FALSE)) {
        output_df_list <- list(output_receptor_summary_distance_metrics_df) 
      } else {
        output_df_list <- append(output_df_list, list(output_receptor_summary_distance_metrics_df))
      }
      
    }
    
    # Return data frame with exposures by receptor id-----------------------------
    
    if(add_all_to_output == TRUE) {
      output_df_list <- append(list(receptor_df), output_df_list, after = 1)
    }
    
    output_df <-  purrr::reduce(output_df_list, dplyr::left_join, by = "id") %>%
      dplyr::arrange(id)
    
    # Print status update
    if(write_log_to_file == TRUE) {
      logr::sep("Completed aircraft facility proximity metric calculations.",
                      console = print_log_to_console)
    } else if(print_log_to_console == TRUE) {
      message("Completed aircraft facility proximity metric calculations.")
    }
    
    # Close log
    if(write_log_to_file == TRUE) {
      logr::log_close()
    }
    
    # Return data frame with aircraft facility proximity-based exposures for receptors
    output_df
  }
