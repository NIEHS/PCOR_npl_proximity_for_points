xyz_proximity_points_calculate <- function
  ( receptor_xyz_filepath,
    source_xyz_facilities_filepath,
    us_borders_filepath,
    buffer_distance_km,
    assessment_year = assessment_year,
    check_near_us_border = check_near_us_border,
    time_option,
    receptor_crs,
    projection_crs,
    proximity_metrics,
    add_nearest_source = TRUE,
    add_all_to_output = TRUE,
    write_log_to_file,
    print_log_to_console,
    show_notes ){

    ##############################################################################
    #
    # Open log -------------------------------------------------------------------
    #
    ##############################################################################
  
    
    if(write_log_to_file == TRUE) {
      if(logr::log_status() != "open") {
        logr::log_open(show_notes = FALSE)
      }
      logr::sep("Check whether point receptors are within buffer distance of border.",
                console = print_log_to_console)
    }
    logr::sep("Calculate results...",  console = print_log_to_console)
    ##############################################################################
    #
    # Read in files and check file format
    # 1/3. Receptor file :
    #
    ##############################################################################
    receptor <- readr::read_csv(receptor_xyz_filepath, show_col_types = FALSE)
    if(is.null(receptor)) {
      stop("Required argument 'receptor' is missing.")
    }
    if(!is.data.frame(receptor)) {
      stop("Required argument 'receptor' must be a data frame.")
    }
    ##############################################################################
    #
    # 1.1 Check point receptor data frame columns format -----------------------------
    #
    ##############################################################################
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
    ##############################################################################
    #
    # 1.2 Receptor transform projections  -----------------------------
    #
    ##############################################################################
    receptor_sf <- sf::st_as_sf(receptor, coords = c('longitude','latitude'), 
                                crs = receptor_crs)
    
    if(("sf" %in% class(receptor_sf)) == FALSE) {
      stop("Required argument 'receptor_sf' must be a simple features object.")
    }
    ##############################################################################
    #
    # Transform projections for receptor points and borders 
    #
    ##############################################################################
    if(sf::st_crs(receptor_sf) != projection_crs) {
      receptor_sf <- sf::st_transform(receptor_sf, crs = projection_crs)
    }
    ##############################################################################
    #
    # Print update to log and console --------------------------------------------
    #
    ##############################################################################
    
    if(write_log_to_file == TRUE) {
      logr::log_print(stringr::str_c('Point receptor data frame has', ncol(receptor), 
                                     'columns and', nrow(receptor), 'rows.', sep = " "),
                      console = print_log_to_console)
    } else if(print_log_to_console == TRUE) {
      message(stringr::str_c('Point receptor data frame has', ncol(receptor), 
                             'columns and', nrow(receptor), 'rows.', sep = " "))
    }
    ##############################################################################
    #
    # Read in files and check file format
    # 2/3. source_xyz_facilities_file :
    #
    ##############################################################################
    #source_xyz_df <- read_excel(source_xyz_facilities_filepath, sheet = "EPA_NPL_Sites_asof_27Feb2014")
    #readr::write_csv(source_xyz_df, file ="./input/EPA_NPL_Site_2014.csv")
    source_xyz_df <- readr::read_csv(source_xyz_facilities_filepath, show_col_types = FALSE)  
    
    if(write_log_to_file == TRUE) {
      logr::log_print(stringr::str_c('Point source_xyz_df data frame has', ncol(source_xyz_df), 
                                     'columns and', nrow(source_xyz_df), 'rows.', sep = " "),
                      console = print_log_to_console)
    } else if(print_log_to_console == TRUE) {
      message(stringr::str_c('Point source_xyz_df data frame has', ncol(source_xyz_df), 
                             'columns and', nrow(source_xyz_df), 'rows.', sep = " "))
    }
    
    ##############################################################################
    #
    # 2.1/3 Convert to sf object with POINT geometry
    #
    ##############################################################################
    if(sf::st_crs(source_xyz_df) != projection_crs) {
      source_sf <- st_as_sf(source_xyz_df, coords = c("LONGITUDE","LATITUDE"), crs = 4326)
    }
    
    if(("sf" %in% class(source_sf)) == FALSE) {
      stop("Required argument 'source_sf' must be a simple features object.")
    }
    
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
    ##############################################################################
    #
    # 3.1/3 US border
    #
    ##############################################################################
    if(write_log_to_file == TRUE) {
      if(logr::log_status() != "open") {
        logr::log_open(show_notes = FALSE)
      }
      logr::sep("Check whether point receptors are within buffer distance of border.",
                console = print_log_to_console)
    }
    
    if(check_near_us_border == TRUE) {
      
      us_borders_sf <- readr::read_rds(us_borders_filepath) 
      if(("sf" %in% class(us_borders_sf)) == FALSE) {
        stop("Required argument 'us_borders_filepath' must lead to a simple features object.")
      }
      if((nrow(us_borders_sf) != 30) | (ncol(us_borders_sf) != 2)) {
        stop("'Required argument 'us_borders_filepath' must lead to a simple features object with 30 rows and 2 columns.")
      }
    } 
    if(is.null(projection_crs)) {
      projection_crs <- sf::st_crs(us_borders_sf)
      if(sf::st_is_longlat(projection_crs) == TRUE) {
        stop("Argument 'border_sf' must have a projected coordinate reference system, or a projected coordinate reference system must be supplied to argument 'projected_crs'.")
      }
    } else if(class(projection_crs) != "crs") {
      stop("Optional argument 'projection_crs' must be of class 'crs'.")
    } else if(sf::st_is_longlat(projection_crs) == TRUE) {
      stop("Optional argument 'projection_crs' must be a projected coordinate reference system.")
    }
    
    border_sf <- sf::st_transform(us_borders_sf, crs = projection_crs)
    
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
    # Get lists of source points in each receptor point buffer
    #sources_in_receptor_buffer_ls = sf::st_intersects(receptor_buffer_sf, source_sf)
    
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
    
    # st_drop_geometry function from the sf package to remove the geometry 
    # column(s) from a simple features object (sf object).
    receptor_border_check_df <- sf::st_drop_geometry(receptor_sf)
  
    output_receptor_border_check_df <- receptor_border_check_df %>% 
      dplyr::select(id, within_border_buffer)
    output_df_list <- list(output_receptor_border_check_df) 
    
    ######End of check_point_receptor_near_border
    
    ##############################################################################
    #
    # Calculate nearest distance to point source from point receptor -------------
    #
    ##############################################################################
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
    receptor_nearest_distance_df<-output_df
    #output_receptor_nearest_distance_df
    # Print status update
    if(write_log_to_file == TRUE) {
      logr::log_print("Calculated distances to nearest point sources.",
                      console = print_log_to_console)
    } else if(print_log_to_console == TRUE) {
      message("Calculated distances to nearest point sources.")
    }

    if(write_log_to_file == TRUE) {
      if(logr::log_status() != "open") {
        logr::log_open(show_notes = FALSE)
      }
      logr::sep("Calculate nearest distance to point source from point receptor.",
                console = print_log_to_console)
    }
    #The %>% is a piping operator which means "then". It's used to pass an
    # object to the following function. In this case, it's passing receptor_nearest_distance_df to the next function.
    # Selecting columns from the dataframe. It selects the 
    # id column and any other columns that start with the string "nearest_".
    # Any column name (except for the column named id) will be prefixed with
    # "XYZ_".
    
    output_receptor_nearest_distance_df <- receptor_nearest_distance_df %>% 
      dplyr::select(id, starts_with("nearest_")) %>%
      dplyr::rename_with(function(x){paste0("XYZ_", x)}, .cols = !id) %>%
      dplyr::mutate(XYZ_nearest_distance = round(XYZ_nearest_distance, digits = 3))
    ##############################################################################
    #
    # If check_near_us_border is FALSE, then a new list output_df_list is 
    # created with just one element: the data frame 
    # output_receptor_nearest_distance_df.
    # If check_near_us_border is TRUE, the dataframe 
    # output_receptor_nearest_distance_df is added as an additional element
    # to the existing output_df_list list. Note that append is used to add 
    # to the list, and the dataframe is wrapped within list() to ensure 
    # that it is added as a new list element.
    #
    ##############################################################################
    
    if(check_near_us_border == FALSE) {
      output_df_list <- list(output_receptor_nearest_distance_df)
    } else {
      output_df_list <- append(output_df_list, list(output_receptor_nearest_distance_df))
    }
    ###############################################
    print("**************************")
    print("Success Calculate distance to nearest XYZ facility from point receptor.")
    print("**************************")
    ##############################################################################
    #
    #   Summarize point sources located within 
    #   specified buffer distance of point receptor. Required function arguments are 
    #   simple features objects of point sources and point receptors and buffer distance. 
    #   Function returns a data frame of receptor points with added columns containing:
    #   count of point sources within buffer distance, summary metrics of distances 
    #   to all point sources within buffer distance of point receptor (mean distance,
    #   mean of log distance, and 25th, 50th and 75th percentile distances).
    #
    ##############################################################################
    # Open log -------------------------------------------------------------------
    if(write_log_to_file == TRUE) {
      if(logr::log_status() != "open") {
        logr::log_open(show_notes = FALSE)
      }
      logr::sep("Summarize point sources within buffer distance of point receptors.",
                console = print_log_to_console)
    }
    # Calculate XYZ facility summary metrics within buffer distance of point receptor -----------
    
    if((("count_in_buffer" %in% proximity_metrics) |
        ("distance_in_buffer" %in% proximity_metrics)) == TRUE) {
      
      summary_metrics <- proximity_metrics[which((proximity_metrics == "distance_in_buffer") | 
                                                   (proximity_metrics == "count_in_buffer"))]
      
      for(i in 1:length(summary_metrics)) {
        if((summary_metrics[i] %in% c("count_in_buffer", "distance_in_buffer")) == FALSE) {
          stop("Optional argument 'summary_metrics' must only include the following: count_in_buffer, distance_in_buffer." )
        }
      } 
      #summary_of_point_sources_in_buffer_of_point_receptor
      # calculates the distances between the ith receptor and the sources within its buffer.
      # Calculate Summary Metrics:
      #   The mean distance and the log-transformed mean distance (base 10) are 
      #   computed for the sources within the buffer of the ith receptor.
      #   These values are then stored in the appropriate columns of the 
      #   summary_distances matrix.
      # Calculate Percentiles:
      #   If there are 10 or more sources within the buffer of the ith receptor 
      #.  (nrow(sources_in_receptor_buffer_ls_i) >= 10), it calculates the 25th, 
      #.  50th (median), and 75th percentiles of the distances. These values are 
      #.  then stored in the last three columns of the summary_distances matrix.
      
      if(sf::st_crs(receptor_sf) != sf::st_crs(source_sf)) {
        stop("Unable to transform point receptor coordinate reference system to match point source.")
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
      
      receptor_sf <- dplyr::bind_cols(receptor_sf, summary_distances) 
      print('within_border_buffer is good ')
      
      # Print status update
      if(write_log_to_file == TRUE) {
        logr::log_print("Calculated summary distances to point sources within buffer distance of point receptors.",
                        console = print_log_to_console)
      } else if(print_log_to_console == TRUE) {
        message("Calculated summary distances to point sources within buffer distance of point receptors.")
      }
      }
      #receptor_sf has no geometry after drop it receptor_df has 
      output_df <- sf::st_drop_geometry(receptor_sf)
      
      receptor_summary_distance_metrics_df <- output_df
        summary_distance_metrics_names <- c("count_in_buffer", "mean_distance_in_buffer",
                                          "log_mean_distance_in_buffer", "p25_distance_in_buffer",
                                          "p50_distance_in_buffer", "p75_distance_in_buffer")
      
        output_receptor_summary_distance_metrics_df <- receptor_summary_distance_metrics_df %>% 
        dplyr::select(id, tidyselect::any_of(summary_distance_metrics_names)) %>%
        dplyr::mutate(dplyr::across(tidyselect::any_of(summary_distance_metrics_names), ~ round(.x, 3))) %>%
        dplyr::rename_with(function(x){paste0("XYZ_", x)}, .cols = !id) 
      
      if((check_near_us_border == FALSE) &
         (("distance_to_nearest" %in% proximity_metrics) == FALSE)) {
        output_df_list <- list(output_receptor_summary_distance_metrics_df) 
      } else {
        output_df_list <- append(output_df_list, list(output_receptor_summary_distance_metrics_df))
      }
    }
    
    ####Sue Drop extra column within_border_buffer
    receptor_df$within_border_buffer <- NULL
    receptor_sf$within_border_buffer <- NULL
    
    # Return data frame with exposures by receptor id-----------------------------
    
    if(add_all_to_output == TRUE) {
      output_df_list <- append(list(receptor_sf), output_df_list, after = 1)
    }
    
    output_df <-  purrr::reduce(output_df_list, dplyr::left_join, by = "id") %>%
      dplyr::arrange(id)
    
    # Print status update
    if(write_log_to_file == TRUE) {
      logr::sep("Completed XYZ facility proximity metric calculations.",
                console = print_log_to_console)
    } else if(print_log_to_console == TRUE) {
      message("Completed XYZ facility proximity metric calculations.")
    }
    
    # Close log
    if(write_log_to_file == TRUE) {
      logr::log_close()
    }
    
    #print(colnames(output_df))
    # Return data frame with aircraft facility proximity-based exposures for receptors
    output_df
  }