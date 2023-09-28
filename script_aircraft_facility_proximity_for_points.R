################################################################################
# Title: Functions to calculate proximity-based exposures for point receptors to 
# point source aircraft facilities
# Date modified: 2023-08-16
# Script description: Contains the following functions to calculate proximity-
#   based exposures for point receptors to FAA aircraft facilities:
#     - get_airport_facility_proximity_for_points
#     - check_point_receptor_format
#     - check_point_receptor_near_border
#     - nearest_distance_to_point_source_from_point_receptor
#     - summary_of_point_sources_in_buffer_of_point_receptor
# Packages required: logr, sf, tidyverse
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
      stop("Optional argument 'buffer_distance _km' must be in units of kilometers and must be within range 0.001 km to 1000 km.")
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


################################################################################
# Function: check_point_receptor_format
################################################################################
check_point_receptor_format <- function(receptor = NULL, 
                                        year = NULL,
                                        time_option = NULL,
                                        print_log_to_console = TRUE,
                                        write_log_to_file = TRUE) {
  
  # Open log -------------------------------------------------------------------
  
  if(write_log_to_file == TRUE) {
    if(logr::log_status() != "open") {
      logr::log_open(show_notes = FALSE)
    }
    logr::sep("Check format of receptor points data frame.",
              console = print_log_to_console)
  }
  
  # Check input arguments format ----------------------------------------------
  
  if(is.null(receptor)) {
    stop("Required argument 'receptor' is missing.")
  }
  if(!is.data.frame(receptor)) {
    stop("Required argument 'receptor' must be a data frame.")
  }
  
  if(is.null(time_option)) {
    stop("Required argument 'time_option' is missing.")
  }
  if(!is.character(time_option)) {
    stop("Required argument 'time_option' must be in character format.")
  }
  if((length(time_option) != 1) | 
     (time_option %in% c("single_year_only", "variable_year_only", "either_single_or_variable_year", "none") == FALSE)) {
    stop("Required argument 'time_option' must include only one of the following selections: 'single_year_only', 'variable_year_only', 'either_single_or_variable_year', 'none'." )
  }
  
  if(!is.logical(print_log_to_console)) {
    stop("Optional argument 'print_log_to_console' must be logical (i.e., TRUE or FALSE).")
  }
  
  if(!is.logical(write_log_to_file)) {
    stop("Optional argument 'write_log_to_file' must be logical (i.e., TRUE or FALSE).")
  }
  
  # Check point receptor data frame columns format -----------------------------
  
  if(("id" %in% colnames(receptor) == FALSE) | 
     ("latitude" %in% colnames(receptor) == FALSE) |
     ("longitude" %in% colnames(receptor) == FALSE)) {
    stop("Point receptor data frame must have columns: id, latitude, longitude.") 
  } else if((length(receptor$id) == length(unique(receptor$id))) == FALSE) {
    stop("Point receptor data frame must have unique values for column 'id'.")
  } else if((!is.numeric(receptor$latitude)) | (!is.numeric(receptor$longitude))) {
    stop("Point receptor data frame must have columns 'latitude' and 'longitude' in numeric format.")
  } else if((min(receptor$latitude) < -90) | (max(receptor$latitude) > 90) |
            (min(receptor$longitude) < -180) | (max(receptor$longitude) > 180)) {
    stop("Point receptor data frame must have column 'latitude' in decimal degrees format (range: -90 to 90) and column 'longitude' in decimal degrees format (range: -180 to 180).")
  }
  
  # Check receptor time information --------------------------------------------
  
  # Check time information when time_option = "single_year_only" 
  if(time_option == "single_year_only") {
    if(is.null(year)) {
      stop("Required argument for year for exposure assessment is missing.")
    }
    else if(!is.numeric(year)) {
      stop("Required argument for year for exposure assessment must be in numeric format.")
    } 
    else if((year < 1800) | (year > 2100)) {
      stop("Required argument for year for exposure assessment must have 4-digit 'YYYY' format.")
    }
    if(write_log_to_file == TRUE) {
      logr::log_print(stringr::str_c('Year for exposure assessment set to ', 
                                     year, '.', sep = ""),
                      console = print_log_to_console)
    } else if(print_log_to_console == TRUE) {
      message(stringr::str_c('Year for exposure assessment set to ', year, '.', sep = ""))
    }
  }
  
  # Check time information when time_option = "variable_year_only" 
  else if(time_option == "variable_year_only") {
    if(("time_start" %in% colnames(receptor) == FALSE) | 
       ("time_end" %in% colnames(receptor) == FALSE)) {
      stop("Point receptor data frame required columns missing: 'time_start' and/or 'time_end'.")
    } else if((!is.numeric(receptor$time_start)) | (!is.numeric(receptor$time_end))) {
      stop("Point receptor data frame required columns 'time_start' and 'time_end' must be in numeric format.")
    } else if((min(receptor$time_start) < 1800) | (max(receptor$time_start) > 2100) |
              (min(receptor$time_end) < 1800) | (max(receptor$time_end) > 2100)) {
      stop("Point receptor data frame required columns 'time_start' and 'time_end' must have 4-digit 'YYYY' format.")
    } else {
      test_time_range <- receptor %>%
        dplyr::mutate(end_before_start = dplyr::if_else(time_start > time_end, 1, 0))
      if(sum(test_time_range$end_before_start) > 0) {
        stop("Year in 'time_start' must be less than or equal to year in 'time_end' for all point receptors.")
      }
    } 
    
    if(write_log_to_file == TRUE) {
      logr::log_print("Year(s) for exposure assessment set by 'time_start' and 'time_end'.",
                      console = print_log_to_console)
    } else if(print_log_to_console == TRUE) {
      message("Year(s) for exposure assessment set by 'time_start' and 'time_end'.")
    }
  }
  
  # Check time information when time_option = "either_single_or_variable_year" 
  else if(time_option == "either_single_or_variable_year") {
    if(!is.null(year)) {
      if(!is.numeric(year)) {
        stop("Optional argument for year for exposure assessment must be in numeric format.")
      } else if((year < 1800) | (year > 2100)) {
        stop("Optional argument for year for exposure assessment must have 4-digit 'YYYY' format.")
      }
      if(write_log_to_file == TRUE) {
        logr::log_print(stringr::str_c('Year for exposure assessment set to ', 
                                       year, '.', sep = ""),
                        console = print_log_to_console)
      } else if(print_log_to_console == TRUE) {
        message(stringr::str_c('Year for exposure assessment set to ', year, '.', sep = ""))
      }
    } else {
      if(("time_start" %in% colnames(receptor) == FALSE) | 
         ("time_end" %in% colnames(receptor) == FALSE)) {
        stop("Point receptor data frame expected columns missing: 'time_start' and/or 'time_end'.")
      } else if((!is.numeric(receptor$time_start)) | (!is.numeric(receptor$time_end))) {
        stop("Point receptor data frame expected columns 'time_start' and 'time_end' must be in numeric format.")
      } else if((min(receptor$time_start) < 1800) | (max(receptor$time_start) > 2100) |
                (min(receptor$time_end) < 1800) | (max(receptor$time_end) > 2100)) {
        stop("Point receptor data frame expected columns 'time_start' and 'time_end' must have 4-digit 'YYYY' format.")
      } else {
        test_time_range <- receptor %>%
          dplyr::mutate(end_before_start = dplyr::if_else(time_start > time_end, 1, 0))
        if(sum(test_time_range$end_before_start) > 0) {
          stop("Year in 'time_start' must be less than or equal to year in 'time_end' for all point receptors.")
        }
      } 
      
      if(write_log_to_file == TRUE) {
        logr::log_print("Year(s) for exposure assessment set by 'time_start' and 'time_end'.",
                        console = print_log_to_console)
      } else if(print_log_to_console == TRUE) {
        message("Year(s) for exposure assessment set by 'time_start' and 'time_end'.")
      }
    }
  }
  
  # Print update to log and console --------------------------------------------
  
  if(write_log_to_file == TRUE) {
    logr::log_print(stringr::str_c('Point receptor data frame has', ncol(receptor), 
                                   'columns and', nrow(receptor), 'rows.', sep = " "),
                    console = print_log_to_console)
  } else if(print_log_to_console == TRUE) {
    message(stringr::str_c('Point receptor data frame has', ncol(receptor), 
                           'columns and', nrow(receptor), 'rows.', sep = " "))
  }
}

################################################################################
# Function: check_point_receptor_near_border
################################################################################
check_point_receptor_near_border <- function(receptor_sf = NULL, 
                                             border_sf = NULL,
                                             buffer_distance_km = 10,
                                             projection_crs = NULL,
                                             print_log_to_console = TRUE,
                                             write_log_to_file = TRUE) {
  # Open log -------------------------------------------------------------------
  
  if(write_log_to_file == TRUE) {
    if(logr::log_status() != "open") {
      logr::log_open(show_notes = FALSE)
    }
    logr::sep("Check whether point receptors are within buffer distance of border.",
              console = print_log_to_console)
  }
  
  # Check arguments ------------------------------------------------------------
  
  if(is.null(receptor_sf)) {
    stop("Required argument 'receptor_sf' is missing.")
  }
  if(("sf" %in% class(receptor_sf)) == FALSE) {
    stop("Required argument 'receptor_sf' must be a simple features object.")
  }
  
  if(is.null(border_sf)) {
    stop("Required argument 'border_sf' is missing.")
  }
  if(("sf" %in% class(border_sf)) == FALSE) {
    stop("Required argument 'border_sf' must be a simple features object.")
  }
  
  if(is.null(projection_crs)) {
    projection_crs <- sf::st_crs(border_sf)
    if(sf::st_is_longlat(projection_crs) == TRUE) {
      stop("Argument 'border_sf' must have a projected coordinate reference system, or a projected coordinate reference system must be supplied to argument 'projected_crs'.")
    }
  } else if(class(projection_crs) != "crs") {
    stop("Optional argument 'projection_crs' must be of class 'crs'.")
  } else if(sf::st_is_longlat(projection_crs) == TRUE) {
    stop("Optional argument 'projection_crs' must be a projected coordinate reference system.")
  }
  
  if(class(buffer_distance_km) != "numeric") {
    stop("Optional argument 'buffer_distance_km' must have numeric format.")
  } else if((buffer_distance_km > 1000) | (buffer_distance_km < 0.1)) {
    stop("Optional argument 'buffer_distance_km' must be in units of kilometers and must be within range 0.1 km to 1000 km.")
  }
  
  if(!is.logical(print_log_to_console)) {
    stop("Optional argument 'print_log_to_console' must be logical (i.e., TRUE or FALSE).")
  }
  
  if(!is.logical(write_log_to_file)) {
    stop("Optional argument 'write_log_to_file' must be logical (i.e., TRUE or FALSE).")
  }
  
  # Transform projections ------------------------------------------------------
  
  # Transform projections for receptor points and borders 
  receptor_sf <- sf::st_transform(receptor_sf, crs = projection_crs)
  border_sf <- sf::st_transform(border_sf, crs = projection_crs)
  
  if(sf::st_crs(receptor_sf) != sf::st_crs(border_sf)) {
    stop("Unable to transform point receptor coordinate reference system to match border.")
  }
  
  # Convert buffer length units to match projection  
  if((projection_crs$units == "m")) {
    buffer_distance <- buffer_distance_km * 1000 
  } else if((projection_crs$units == "us-ft") | (projection_crs$units == "ft")) {
    buffer_distance <- buffer_distance_km * 1000 * 3.28084
  } else {
    stop("'projection_crs' has unknown length units.")
  }
  
  # Check whether receptor points are within buffer distance of border ---------
  
  # Print status update
  if(write_log_to_file == TRUE) {
    logr::log_print("Status update: Buffering borders...",
                    console = print_log_to_console)
  } else if(print_log_to_console == TRUE) {
    message("Status update: Buffering borders...")
  }
  
  # Buffer border by buffer_distance
  border_buffer_sf <- sf::st_buffer(border_sf, 
                                    dist = buffer_distance,
                                    endCapStyle = "ROUND")
  
  # Add binary variable indicating whether receptor point is within buffer
  receptor_sf <- receptor_sf %>%
    dplyr::mutate(within_border_buffer = lengths(sf::st_within(receptor_sf, border_buffer_sf)))
  
  receptors_within_border_buffer <- sum(receptor_sf$within_border_buffer)
  
  # Print status update
  if(receptors_within_border_buffer > 0) {
    if(write_log_to_file == TRUE)  {
      logr::log_print(stringr::str_c('Warning:', receptors_within_border_buffer, 
                                     "receptor points are within", buffer_distance_km, 
                                     "km of border.", sep = " "), 
                      console = print_log_to_console)
    } else if(print_log_to_console == TRUE) {
      message(stringr::str_c('Warning:', receptors_within_border_buffer, 
                             "receptor points are within", buffer_distance_km, 
                             "km of border.", sep = " "))
    }
  } else {
    if(write_log_to_file == TRUE)  {
      logr::log_print(stringr::str_c("No receptor points are within", buffer_distance_km, 
                                     "km of border.", sep = " "), 
                      console = print_log_to_console)
    } else if(print_log_to_console == TRUE) {
      message(stringr::str_c("No receptor points are within", buffer_distance_km, 
                             "km of border.", sep = " "))
    }
  }
  
  # Return receptor points as data frame
  output_df <- sf::st_drop_geometry(receptor_sf)
  output_df
}

################################################################################
# Function: nearest_distance_to_point_source_from_point_receptor
################################################################################
nearest_distance_to_point_source_from_point_receptor <- 
  function(receptor_sf = NULL, 
           source_sf = NULL,
           projection_crs = NULL,
           add_nearest_source = TRUE,
           print_log_to_console = TRUE,
           write_log_to_file = TRUE) {
    
    # Open log -------------------------------------------------------------------
    
    if(write_log_to_file == TRUE) {
      if(logr::log_status() != "open") {
        logr::log_open(show_notes = FALSE)
      }
      logr::sep("Calculate nearest distance to point source from point receptor.",
                console = print_log_to_console)
    }
    
    # Check arguments ------------------------------------------------------------
    
    if(is.null(receptor_sf)) {
      stop("Required argument 'receptor_sf' is missing.")
    }
    if(("sf" %in% class(receptor_sf)) == FALSE) {
      stop("Required argument 'receptor_sf' must be a simple features object.")
    }
    
    if(is.null(source_sf)) {
      stop("Required argument 'source_sf' is missing.")
    }
    if(("sf" %in% class(source_sf)) == FALSE) {
      stop("Required argument 'source_sf' must be a simple features object.")
    }
    
    if(is.null(projection_crs)) {
      projection_crs <- sf::st_crs(source_sf)
      if(sf::st_is_longlat(projection_crs) == TRUE) {
        stop("Argument 'source_sf' must have a projected coordinate reference system, or a projected coordinate reference system must be supplied to argument 'projected_crs'.")
      }
    } else if(class(projection_crs) != "crs") {
      stop("Optional argument 'projection_crs' must be of class 'crs'.")
    } else if(sf::st_is_longlat(projection_crs) == TRUE) {
      stop("Optional argument 'projection_crs' must be a projected coordinate reference system.")
    }
    
    if(!is.logical(add_nearest_source)) {
      stop("Optional argument 'add_nearest_source' must be logical (i.e., TRUE or FALSE).")
    }
    
    if(!is.logical(print_log_to_console)) {
      stop("Optional argument 'print_log_to_console' must be logical (i.e., TRUE or FALSE).")
    }
    
    if(!is.logical(write_log_to_file)) {
      stop("Optional argument 'write_log_to_file' must be logical (i.e., TRUE or FALSE).")
    }
    
    # Transform projections ------------------------------------------------------
    
    # Transform projections for receptor points and borders 
    if(sf::st_crs(receptor_sf) != projection_crs) {
      receptor_sf <- sf::st_transform(receptor_sf, crs = projection_crs)
    }
    if(sf::st_crs(source_sf) != projection_crs) {
      source_sf <- sf::st_transform(source_sf, crs = projection_crs)
    }
    if(sf::st_crs(receptor_sf) != sf::st_crs(source_sf)) {
      stop("Unable to transform point receptor coordinate reference system to match point source.")
    }
    
    # Calculate nearest distance to point source from point receptor -------------
    
    # Print status update
    if(write_log_to_file == TRUE) {
      logr::log_print("Status update: Calculating distances...",
                      console = print_log_to_console)
    } else if(print_log_to_console == TRUE) {
      message("Status update: Calculating distances...")
    }
    
    # Get vector of indices of nearest source for each receptor
    nearest_source_index <- sf::st_nearest_feature(receptor_sf, source_sf)
    
    # Get simple features and data frame of nearest source for each receptor
    nearest_source_details_sf <- source_sf[nearest_source_index,]
    nearest_source_details_df <- sf::st_drop_geometry(nearest_source_details_sf) %>%
      dplyr::rename_with(~ paste0("nearest_", .x)) %>%
      tibble::remove_rownames()
    
    # Get distance of each receptor point to nearest source
    nearest_distance_to_source <- sf::st_distance(receptor_sf, 
                                                  nearest_source_details_sf, 
                                                  by_element = TRUE)
    
    # Convert distances from projection units to km
    nearest_distance_to_source_km <- units::set_units(nearest_distance_to_source, km)
    
    # Get data frame of point receptors
    receptor_df <- sf::st_drop_geometry(receptor_sf)
    
    # Build a data frame with point receptors with nearest source details and distance
    if(add_nearest_source == TRUE) {
      output_df <- dplyr::bind_cols(receptor_df, 
                                    nearest_distance = units::drop_units(nearest_distance_to_source_km),
                                    nearest_source_details_df) 
    } else {
      output_df <- dplyr::bind_cols(receptor_df, 
                                    nearest_distance = units::drop_units(nearest_distance_to_source_km)) 
    }   
    
    # Print status update
    if(write_log_to_file == TRUE) {
      logr::log_print("Calculated distances to nearest point sources.",
                      console = print_log_to_console)
    } else if(print_log_to_console == TRUE) {
      message("Calculated distances to nearest point sources.")
    }
    
    # Return nearest distances data frame ----------------------------------------
    output_df
    
  }

################################################################################
# Function: summary_of_point_sources_in_buffer_of_point_receptor
################################################################################
summary_of_point_sources_in_buffer_of_point_receptor <- 
  function(receptor_sf = NULL, 
           source_sf = NULL,
           summary_metrics = c("count_in_buffer",
                               "distance_in_buffer"),
           buffer_distance_km = 10,
           projection_crs = NULL,
           print_log_to_console = TRUE,
           write_log_to_file = TRUE) {
    
    # Open log -------------------------------------------------------------------
    
    if(write_log_to_file == TRUE) {
      if(logr::log_status() != "open") {
        logr::log_open(show_notes = FALSE)
      }
      logr::sep("Summarize point sources within buffer distance of point receptors.",
                console = print_log_to_console)
    }
    
    # Check arguments ------------------------------------------------------------
    
    if(is.null(receptor_sf)) {
      stop("Required argument 'receptor_sf' is missing.")
    }
    if(("sf" %in% class(receptor_sf)) == FALSE) {
      stop("Required argument 'receptor_sf' must be a simple features object.")
    }
    
    if(is.null(source_sf)) {
      stop("Required argument 'source_sf' is missing.")
    }
    if(("sf" %in% class(source_sf)) == FALSE) {
      stop("Required argument 'source_sf' must be a simple features object.")
    }
    
    for(i in 1:length(summary_metrics)) {
      if((summary_metrics[i] %in% c("count_in_buffer", "distance_in_buffer")) == FALSE) {
        stop("Optional argument 'summary_metrics' must only include the following: count_in_buffer, distance_in_buffer." )
      }
    }  
    
    if(is.null(projection_crs)) {
      projection_crs <- sf::st_crs(source_sf)
      if(sf::st_is_longlat(projection_crs) == TRUE) {
        stop("Argument 'source_sf' must have a projected coordinate reference system, or a projected coordinate reference system must be supplied to argument 'projected_crs'.")
      }
    } else if(class(projection_crs) != "crs") {
      stop("Optional argument 'projection_crs' must be of class 'crs'.")
    } else if(sf::st_is_longlat(projection_crs) == TRUE) {
      stop("Optional argument 'projection_crs' must be a projected coordinate reference system.")
    }
    
    if(class(buffer_distance_km) != "numeric") {
      stop("Optional argument 'buffer_distance_km' must have numeric format.")
    } else if((buffer_distance_km > 1000) | (buffer_distance_km < 0.01)) {
      stop("Optional argument 'buffer_distance_km' must be in units of kilometers and must be within range 0.01 km to 1000 km.")
    }
    
    if(!is.logical(print_log_to_console)) {
      stop("Optional argument 'print_log_to_console' must be logical (i.e., TRUE or FALSE).")
    }
    
    if(!is.logical(write_log_to_file)) {
      stop("Optional argument 'write_log_to_file' must be logical (i.e., TRUE or FALSE).")
    }
    
    # Transform projections ------------------------------------------------------
    
    # Transform projections for receptor points and borders 
    if(sf::st_crs(receptor_sf) != projection_crs) {
      receptor_sf <- sf::st_transform(receptor_sf, crs = projection_crs)
    }
    if(sf::st_crs(source_sf) != projection_crs) {
      source_sf <- sf::st_transform(source_sf, crs = projection_crs)
    }
    
    if(sf::st_crs(receptor_sf) != sf::st_crs(source_sf)) {
      stop("Unable to transform point receptor coordinate reference system to match point source.")
    }
    
    # Convert buffer length units to match projection  
    if((projection_crs$units == "m")) {
      buffer_distance <- buffer_distance_km * 1000 
    } else if((projection_crs$units == "us-ft") | (projection_crs$units == "ft")) {
      buffer_distance <- buffer_distance_km * 1000 * 3.28084
    } else {
      stop("'projection_crs' has unknown length units.")
    }
    
    # Identify point sources in buffer distance of point receptor ----------------
    
    # Print status update
    if(write_log_to_file == TRUE) {
      logr::log_print("Status update: Identifying point sources within buffer distance of point receptors...",
                      console = print_log_to_console)
    } else if(print_log_to_console == TRUE) {
      message("Status update: Identifying point sources within buffer distance of point receptors...")
    }
    
    # Buffer point receptors by buffer_distance
    receptor_buffer_sf <- sf::st_buffer(receptor_sf, 
                                        dist = buffer_distance)
    
    # Get lists of source points in each receptor point buffer
    sources_in_receptor_buffer_ls = sf::st_intersects(receptor_buffer_sf, source_sf)
    
    # Print status update
    if(write_log_to_file == TRUE) {
      logr::log_print("Identified point sources within buffer distance of point receptors.",
                      console = print_log_to_console)
    } else if(print_log_to_console == TRUE) {
      message("Identified point sources within buffer distance of point receptors.")
    }
    
    # Count point sources in buffer distance of point receptor ---------------- 
    
    if("count_in_buffer" %in% summary_metrics) {
      
      # Print status update
      if(write_log_to_file == TRUE) {
        logr::log_print("Status update: Counting point sources within buffer distance of point receptors...",
                        console = print_log_to_console)
      } else if(print_log_to_console == TRUE) {
        message("Status update: Counting point sources within buffer distance of point receptors...")
      }
      
      # Add variable counting source points in buffer distance of receptor
      receptor_sf <- receptor_sf %>%
        dplyr::mutate(count_in_buffer = lengths(sources_in_receptor_buffer_ls))
      
      # Print status update
      if(write_log_to_file == TRUE) {
        logr::log_print("Counted point sources within buffer distance of point receptors.",
                        console = print_log_to_console)
      } else if(print_log_to_console == TRUE) {
        message("Counted point sources within buffer distance of point receptors.")
      }
    }
    
    # Calculate summary distance metrics to point receptors in buffer distance -----------
    
    if("distance_in_buffer" %in% summary_metrics) {
      
      # Print status update
      if(write_log_to_file == TRUE) {
        logr::log_print("Status update: Calculating summary distances to point sources within buffer distance of point receptors...",
                        console = print_log_to_console)
      } else if(print_log_to_console == TRUE) {
        message("Status update: Calculating summary distances to point sources within buffer distance of point receptors...")
      }
      
      # Initialize a matrix for collecting mean distance and mean of log distance
      summary_distances <- matrix(data = NA, 
                                  nrow = length(sources_in_receptor_buffer_ls),
                                  ncol = 5)
      colnames(summary_distances) <- c("mean_distance_in_buffer", 
                                       "log_mean_distance_in_buffer",
                                       "p25_distance_in_buffer",
                                       "p50_distance_in_buffer",
                                       "p75_distance_in_buffer")
      
      # Loop through the list of identified point sources in each receptor buffer  
      # and calculate the mean distance, mean of log distance, and percentile 
      # (25th, 50th, and 75th percentiles) distance for each receptor
      for(i in 1:length(sources_in_receptor_buffer_ls)) {
        
        sources_in_receptor_buffer_ls_i <- source_sf[sources_in_receptor_buffer_ls[[i]],]
        
        if(nrow(sources_in_receptor_buffer_ls_i) != 0) {
          
          distances_i <- sf::st_distance(receptor_sf[i,], 
                                         sources_in_receptor_buffer_ls_i,
                                         by_element = FALSE) 
          
          distances_i_km <- units::set_units(distances_i, km) %>%
            units::drop_units()
          
          mean_distance_i_km = mean(distances_i_km)
          log_mean_distance_i_km = mean(log10(distances_i_km))
          
          summary_distances[i, 1] <- mean_distance_i_km
          summary_distances[i, 2] <- log_mean_distance_i_km
          
          if(nrow(sources_in_receptor_buffer_ls_i) >= 10) {
            
            p_distance_in_buffer_i_km = unname(stats::quantile(distances_i_km, 
                                                               probs = c(0.25, 0.5, 0.75),
                                                               type = 1))
            
            summary_distances[i, 3:5] <- p_distance_in_buffer_i_km
          }
        }
      }
      
      receptor_sf <- dplyr::bind_cols(receptor_sf, 
                                      summary_distances) 
      
      # Print status update
      if(write_log_to_file == TRUE) {
        logr::log_print("Calculated summary distances to point sources within buffer distance of point receptors.",
                        console = print_log_to_console)
      } else if(print_log_to_console == TRUE) {
        message("Calculated summary distances to point sources within buffer distance of point receptors.")
      }
    }
    
    # Return receptor points as data frame ---------------------------------------
    output_df <- sf::st_drop_geometry(receptor_sf)
    output_df
    
  }