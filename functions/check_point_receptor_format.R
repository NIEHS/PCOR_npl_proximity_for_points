################################################################################
# Title: Function to check format of point receptor data frame
# Date modified: 2022-10-21
# Script description: Contains function to check format of point receptor data
#   frame for compatibility with other NIEHS geospatial toolbox functions. 
#   Required function argument is a data frame containing point receptors. 
#   Function prints an error message if format errors are found.
# Packages required: logr, tidyverse 
################################################################################

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
      stop("Required argument for year for exposure assessment must in the range 1800 to 2100. (need update)")
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