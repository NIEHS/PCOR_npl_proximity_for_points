################################################################################
# Title: Function to get distance to nearest point source from point receptor
# Date modified: 2023-08-15
# Script description: Contains function to calculate distance to nearest point 
#   source from point receptors. Required function arguments are simple features 
#   objects of point sources and point receptors and a projected coordinate
#   reference system for use in distance calculations. Function 
#   returns a data frame of receptor points with added column 'nearest_distance'
#   and, optionally, added columns corresponding to columns from the nearest 
#   point source in the point source simple features object (for use identifying
#   the nearest point source).
# Packages required: logr, sf, tidyverse
################################################################################

################################################################################
# Function: nearest_distance_to_point_source_from_point_receptor
################################################################################
nearest_distance_to_point_source_from_point_receptor <- 
  function(receptor_sf = NULL, 
           receptor_filepath,
           receptor_crs,
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
  
    receptor_df <- readr::read_csv(receptor_filepath, show_col_types = FALSE)
    # check_point_receptor_format(receptor = receptor_df,
    #                             year = year(assessment_year),
    #                             time_option = time_option,
    #                             print_log_to_console = print_log_to_console,
    #                             write_log_to_file = write_log_to_file)
    # 
    receptor_sf <- sf::st_as_sf(receptor_df, coords = c('longitude','latitude'), 
                                crs = receptor_crs)
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
  print("****Transform projections for receptor points and borders***")
    if(sf::st_crs(source_sf) != projection_crs) {
      # Convert to sf object with POINT geometry
      source_sf <- st_as_sf(source_sf, coords = c("LONGITUDE","LATITUDE"), crs = 4326)
      print("Transform source_sf success.")
    }
    
  # Transform projections for receptor points and borders 
  if(sf::st_crs(receptor_sf) != projection_crs) {
    print("Transform receptor begining.")
    receptor_sf <- sf::st_transform(receptor_sf, crs = projection_crs)
    print("Transform receptor done.")
  }
  if(sf::st_crs(source_sf) != projection_crs) {
    source_sf <- sf::st_transform(source_sf, crs = projection_crs)
    print("Transform source done.")
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