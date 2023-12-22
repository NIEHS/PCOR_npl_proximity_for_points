# Check point receptor data frame columns format -----------------------------


file_validate <- function(
    receptor_xyz_filepath,
    source_xyz_facilities_filepath,
    time_option,
    year,
    write_log_to_file = TRUE,
    print_log_to_console = TRUE  
  ){

# Open log -------------------------------------------------------------------
if(write_log_to_file == TRUE) {
  if(logr::log_status() != "open") {
    logr::log_open(show_notes = FALSE)
  }
  logr::sep("Check format of receptor points data frame.",
            console = print_log_to_console)
}

receptor <- readr::read_csv(receptor_xyz_filepath, show_col_types = FALSE) 
# Check input arguments format ----------------------------------------------

if(is.null(receptor)) {
  stop("Required argument 'receptor' is missing.")
}else {
  logr::sep("The receptor file is not null. Check format of receptor file.",
            console = print_log_to_console)
}
if(!is.data.frame(receptor)) {
  stop("Required argument 'receptor' must be a data frame.")
}

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

logr::sep("The receptor file has unique id , latitude and longitude.",
          console = print_log_to_console)
# Check receptor time information --------------------------------------------
if(!is.numeric(year))
{
  year <- as.numeric(year)
}
# Check time information when time_option = "single_year_only" 
if(time_option == "single_year_only") {
  if(is.null(year)) {
    stop("Required argument for year for exposure assessment is missing.")
  }
  else if(!is.numeric(year)) {
    stop("Required argument for year for exposure assessment must be in numeric format.")
  }
}else if (time_option == "variable_year_only"){
# Check time information when time_option = \"variable_year_only\" 
 #else if(time_option == "variable_year_only") {
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
}
