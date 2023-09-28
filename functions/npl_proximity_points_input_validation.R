########################################
# function npl_proximity_points_input_validation
# Check input file exist 
# Check all required columns for the calculation
#
#function check_pointer_receipters
# Check if receptor file exist
# Check us_borders_sf is 30 rows and 2 columns
# Check if point receptors are within buffer of border
# Invoke check_point_receptor_near_border form file in the same name
#  time_option = "single_year_only" for now
#
#########################################
npl_proximity_points_input_validation <- 
  function( receptor_filepath = NULL,
            source_npl_facilities_filepath = NULL,
            us_borders_filepath = NULL,
            npl_year = NULL,
            buffer_distance_km = 10,
            start_year = NULL,
            end_year = NULL,
            check_near_us_border = NULL
    
    ){
    print(receptor_filepath)
    print(source_npl_facilities_filepath)
    if(is.null(receptor_filepath)) {
      stop("Required argument 'receptor_filepath' is missing.")
    } else if(!file.exists(receptor_filepath)) {
      stop("'receptor_filepath' must be a valid file path:",receptor_filepath)
    } else if((stringr::str_sub(receptor_filepath, -3, -1) != "csv") & 
              (stringr::str_sub(receptor_filepath, -3, -1) != "CSV")) {
      stop("'receptor_filepath' must be a valid file path for a csv file.")
    }
     
    #print("source_npl_facilities_filepath: ", source_npl_facilities_filepath)
    print("llllllllll")
    print(source_npl_facilities_filepath)
    print("kkkkkkkk")
    if(is.null(source_npl_facilities_filepath)) {
      stop("Required argument 'source_npl_facilities_filepath' is missing.")
    } else if(!file.exists(source_npl_facilities_filepath)) {
      stop("'source_npl_facilities_filepath' must be a valid file path.")
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
    if(is.null(npl_year)) {
      stop("Required argument 'npl_year' is missing.")
    } else if (is.na(as.Date(npl_year, format = "%Y-%m-%d"))){
      print(npl_year)
      stop("Required argument 'npl_year' must be %Y-%m-%d  format.")
    } else if((year(npl_year) < start_year) || (year(npl_year) > end_year)) {
      stop("Required argument 'npl_year'", (year(npl_year)), " must be within range start_year : ", start_year, " to end_year: ", end_year)
    }
    
    if(class(buffer_distance_km) != "numeric") {
      stop("Optional argument 'buffer_distance_km' must have numeric format.")
    } else if((buffer_distance_km > 1000) | (buffer_distance_km < 0.001)) {
      stop("Optional argument 'buffer_distance _km' must be in units of kilometers and must be within range 0.001 km to 1000 km.")
    }
    
    # if(class(receptor_crs) != "crs") {
    #   stop("Optional argument 'receptor_crs' must be of class 'crs'.")
    # }
    # 
    # if(class(projection_crs) != "crs") {
    #   stop("Optional argument 'projection_crs' must be of class 'crs'.")
    # } else if(sf::st_is_longlat(projection_crs) == TRUE) {
    #   stop("Optional argument 'projection_crs' must be a projected coordinate reference system.")
    # }
    
    # for(i in 1:length(proximity_metrics)) {
    #   if((proximity_metrics[i] %in% c("distance_to_nearest", "count_in_buffer", "distance_in_buffer")) == FALSE) {
    #     stop("Optional argument 'proximity_metrics' must only include the following: distance_to_nearest, count_in_buffer, distance_in_buffer." )
    #   }
    # } 
    
    if(!is.logical(check_near_us_border)) {
      stop("Optional argument 'check_near_us_border' must be logical (i.e., TRUE or FALSE).")
    }

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
            npl_year){
    print("&&&&&&7777777777")
    #print("receptor_filepath:",receptor_filepath)
    print(receptor_filepath)
    print("888888888")
    receptor_df <- readr::read_csv(receptor_filepath, show_col_types = FALSE)
    
    check_point_receptor_format(receptor = receptor_df,
                                year = year(npl_year),
                                time_option = "single_year_only",
                                print_log_to_console = print_log_to_console,
                                write_log_to_file = write_log_to_file)
    
    receptor_sf <- sf::st_as_sf(receptor_df, coords = c('longitude','latitude'),
                                crs = receptor_crs)
    
    # Check if point receptors are within buffer of border
    if(check_near_us_border == TRUE) {
      
      us_borders_sf <- readr::read_rds(us_borders_filepath)
      
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
      receptor_border_check_df <-
        check_point_receptor_near_border(receptor_sf = receptor_sf,
                                         border_sf = us_borders_sf,
                                         buffer_distance_km = buffer_distance_km,
                                         projection_crs = projection_crs,
                                         print_log_to_console = print_log_to_console,
                                         write_log_to_file = write_log_to_file)
      
      output_receptor_border_check_df <- receptor_border_check_df %>%
        dplyr::select(id, within_border_buffer)
      print("9999**********************************")
     # print("output_receptor_border_check_df dim: ",dim(output_receptor_border_check_df))
      print("9990**********************************")
      output_df_list <- list(output_receptor_border_check_df)
    }
    
  }
