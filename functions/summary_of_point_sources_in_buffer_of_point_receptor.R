################################################################################
# Title: Function to count point sources and within buffer of receptor points
# Date modified: 2023-08-15
# Script description: Contains function to summarize point sources located within 
#   specified buffer distance of point receptor. Required function arguments are 
#   simple features objects of point sources and point receptors and buffer distance. 
#   Function returns a data frame of receptor points with added columns containing:
#   count of point sources within buffer distance, summary metrics of distances 
#   to all point sources within buffer distance of point receptor (mean distance,
#   mean of log distance, and 25th, 50th and 75th percentile distances).
# Packages required: logr, sf, tidyverse
################################################################################

################################################################################
# Function: summary_of_point_sources_in_buffer_of_point_receptor
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
    n_col <- ncol(source_sf)
    n_row <- nrow(source_sf)
    print ("********Summary *****")
    print(n_col)
    print(n_row)
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
  #readr::write_csv(receptor_buffer_sf, file ="./output/zzreceptor_buffer_sf.csv")
  # Get lists of source points in each receptor point buffer
  sources_in_receptor_buffer_ls = sf::st_intersects(receptor_buffer_sf, source_sf)
  #readr::write_csv(sources_in_receptor_buffer_ls,file ="./output/zzsources_in_receptor_buffer_ls.csv")
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