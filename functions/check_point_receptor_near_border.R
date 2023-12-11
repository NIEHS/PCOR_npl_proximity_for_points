################################################################################
# Title: Function to check whether point receptors are within buffer of border
# Date modified: 2022-10-13
# Script description: Contains function to check whether point receptors are 
#   within a set buffer distance of a border. Required function arguments are 
#   simple features object of borders, simple features object of receptor points, and
#   buffer distance. Function returns a data frame of receptor points
#   with added column 'within_border_buffer' identifying receptor points within 
#   buffer distance of border (1 = within buffer; 0 = not within buffer).
# Packages required: logr, sf, tidyverse 
################################################################################

################################################################################
# Function: check_point_receptor_near_border
################################################################################


check_point_receptor_near_border <- function(receptor_sf = receptor_sf, 
                                             border_sf = us_borders_sf,
                                             buffer_distance_km ,
                                             projection_crs ,
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

  border_sf <- sf::st_transform(border_sf, crs = projection_crs)
  receptor_sf <- sf::st_transform(receptor_sf, crs = projection_crs)

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
  # st_drop_geometry function from the sf package to remove the geometry 
  # column(s) from a simple features object (sf object).
  # Return receptor points as data frame
  
  output_df <- sf::st_drop_geometry(receptor_sf)
  print(colnames(output_df))
  output_df
}