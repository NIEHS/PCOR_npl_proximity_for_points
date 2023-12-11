########################################
# function xyz_proximity_points_input_validation
# Check input file exist 
# Check all required columns for the calculation
#
#function check_pointer_receipters
# Check if receptor file exist
# Check us_borders_sf is 30 rows and 2 columns
# Check if point receptors are within buffer of border
# Invoke check_point_receptor_near_border function form check_point_receptor_near_border.R 
#  time_option = "single_year_only" for now
#
#########################################
xyz_proximity_points_input_validation <- 
  function( receptor_xyz_filepath = NULL,
            source_xyz_facilities_filepath = NULL,
            us_borders_filepath = NULL,
            buffer_distance_km,
            assessment_year = NULL,
            check_near_us_border = NULL,
            time_option = NULL,
            receptor_crs,
            projection_crs,
            proximity_metrics,
            print_log_to_console,
            write_log_to_file
            
  ){
    
    if(is.null(receptor_xyz_filepath)) {
      stop("Required argument 'receptor_filepath' is missing.")
    } else if(!file.exists(receptor_xyz_filepath)) {
      stop("'receptor_filepath' must be a valid file path:",receptor_cyz_filepath)
    } else if((stringr::str_sub(receptor_xyz_filepath, -3, -1) != "csv") & 
              (stringr::str_sub(receptor_xyz_filepath, -3, -1) != "CSV")) {
      stop("'receptor_xyz_filepath' must be a valid file path for a csv file.")
    }
    
    if(is.null(source_xyz_facilities_filepath)) {
      stop("Required argument 'source_xyz_facilities_filepath' is missing.")
    } else if(!file.exists(source_xyz_facilities_filepath)) {
      stop("'source_xyz_facilities_filepath' must be a valid file path.")
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
    if(is.null(assessment_year)) {
      stop("Required argument 'assessment_year' is missing.")
    } else if (is.na(as.Date(assessment_year, format = "%Y-%m-%d"))){
      print(assessment_year)
      stop("Required argument 'assessment_year' must be %Y-%m-%d  format.")
    } 
    #else if((year(assessment_year) < start_year) || (year(assessment_year) > end_year)) {
    #stop("Required argument 'assessment_year'", (year(assessment_year)), " must be within range start_year : ", start_year, " to end_year: ", end_year)
    #}
    
    if(class(buffer_distance_km) != "numeric") {
      stop("Optional argument 'buffer_distance_km' must have numeric format.")
    } else if((buffer_distance_km > 1000) | (buffer_distance_km < 0.001)) {
      stop("Optional argument 'buffer_distance _km' must be in units of kilometers and must be within range 0.001 km to 1000 km.")
    }
    
     if(is.null(time_option)){
       stop("Required argument 'time_option' is missing.")
     }
    
    # Check time information when time_option = "single_year_only"
    assessment_year = year(assessment_year)
    if(time_option == "single_year_only") {
      logr::log_print(stringr::str_c('time_option set to ', 
                                     time_option, '.', sep = ""),
                      console = print_log_to_console)
      if(is.null(assessment_year)) {
        stop("Required argument for assessment_year for exposure assessment is missing.")
      }
      else if(!is.numeric(assessment_year)) {
        stop("Required argument for year for exposure assessment must be in numeric format.")
      } 
      else if((assessment_year < 1800) | (assessment_year > 2100)) {
        stop("Required argument for year for exposure assessment must in the range 1800 to 2100. ")
      }
      if(write_log_to_file == TRUE) {
        logr::log_print(stringr::str_c('Year for exposure assessment set to ', 
                                       assessment_year, '.', sep = ""),
                        console = print_log_to_console)
      } else if(print_log_to_console == TRUE) {
        message(stringr::str_c('Year for exposure assessment set to ', assessment_year, '.', sep = ""))
      }
    } # Check time information when time_option = "variable_year_only" 
    else if(time_option == "variable_year_only") {
      if(write_log_to_file == TRUE) {
        stop ("Option variable_year_only is under development.",
              console = print_log_to_console)
      } 
    }
    
     if(class(receptor_crs) != "crs") {
       stop("Optional argument 'receptor_crs' must be of class 'crs'.")
     }
     
     if(class(projection_crs) != "crs") {
       stop("Optional argument 'projection_crs' must be of class 'crs'.")
     } else if(sf::st_is_longlat(projection_crs) == TRUE) {
       stop("Optional argument 'projection_crs' must be a projected coordinate reference system.")
     }
    
     for(i in 1:length(proximity_metrics)) {
       if((proximity_metrics[i] %in% c("distance_to_nearest", "count_in_buffer", "distance_in_buffer")) == FALSE) {
         stop("Optional argument 'proximity_metrics' must only include the following: distance_to_nearest, count_in_buffer, distance_in_buffer." )
       }
     } 
    
    if(!is.logical(check_near_us_border)) {
      stop("Optional argument 'check_near_us_border' must be logical (i.e., TRUE or FALSE).")
    }
    
    logr::sep("All input arguments are valid.",
              console = TRUE)
    
  }

########################################
# Function: Read and check receptor points file
########################################
check_pointer_receipters <- 
  function (receptor_filepath, 
            show_col_types = FALSE,
            write_log_to_file,
            print_log_to_console,
            receptor_crs,
            projection_crs,
            check_near_us_border,
            us_borders_filepath,
            buffer_distance_km,
            assessment_year,
            time_option){
    
    receptor_df <- readr::read_csv(receptor_filepath, show_col_types = FALSE)
    check_point_receptor_format(receptor = receptor_df,
                                year = year(assessment_year),
                                time_option = time_option,
                                print_log_to_console = print_log_to_console,
                                write_log_to_file = write_log_to_file)
    
    receptor_sf <- sf::st_as_sf(receptor_df, coords = c('longitude','latitude'), 
                                crs = receptor_crs)
    # Check if point receptors are within buffer of border
    us_borders_sf <- readr::read_rds(us_borders_filepath)
    if(check_near_us_border == TRUE) {
      if(("sf" %in% class(us_borders_sf)) == FALSE) {
        stop("Required argument 'us_borders_filepath' must lead to a simple features object.")
      }
      if((nrow(us_borders_sf) != 30) | (ncol(us_borders_sf) != 2)) {
        stop("'Required argument 'us_borders_filepath' must lead to a simple features object with 30 rows and 2 columns.")
      }
      ################################################################################
      # check_point_receptor_near_border: check_point_receptor_near_border.R
      # Function to check whether point receptors are within buffer of border
      # Script description: Contains function to check whether point receptors are 
      #   within a set buffer distance of a border. Required function arguments are 
      #   simple features object of borders, simple features object of receptor points, and
      #   buffer distance. Function returns a data frame of receptor points
      #   with added column 'within_border_buffer' identifying receptor points within 
      #   buffer distance of border (1 = within buffer; 0 = not within buffer).
      ################################################################################
      #receptor_sf <- sf::st_transform(receptor_sf, crs = projection_crs)
      #border_sf <- sf::st_transform(us_borders_sf, crs = projection_crs)
      #receptor_sf = receptor_sf,
      #us_borders_sf,
      
      #  receptor_border_check_df <-
      #   check_point_receptor_near_border( receptor_sf,
      #                                     us_borders_sf,
      #                                    buffer_distance_km = buffer_distance_km,
      #                                    projection_crs = projection_crs,
      #                                    print_log_to_console = print_log_to_console,
      #                                    write_log_to_file = write_log_to_file)
      # 
      # output_receptor_border_check_df <- receptor_border_check_df %>%
      #   dplyr::select(id, within_border_buffer)
      # 
      # output_df_list <- list(output_receptor_border_check_df)
      
    }
    
  }
