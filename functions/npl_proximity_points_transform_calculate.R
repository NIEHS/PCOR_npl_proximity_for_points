##############################################
# function : trainsform_receptor_sf
#   Useing sf package st_crs to retrieves the CRS of the receptor cvs file 
#   A CRS defines how the two-dimensional (or potentially three-dimensional) 
#   data of the spatial object relates to real places on the Earth
#   return a receptor sf object
# function : check_us_border
#   Check if point receptors are within buffer of border 
#   Validate US borders' file and data frame us_borders_sf 
#   Log the information
# function : trainform_filter_source_points
#   Load dataset from excel file 
#   filter the data as needed
#   Convert to sf object with POINT geometry and return source_npl_sf
# function calculate_distance_npl_receptor
#   Calculate distance to nearest npl facility from point receptor
#   Using functions from the dplyr package, which is part of the tidyverse. 
#   The dplyr package provides a suite of functions designed to facilitate 
#   data manipulation tasks. The code block is performing a sequence of 
#   operations on the receptor_nearest_distance_df data frame 
#   using the "pipe" operator (%>%).
#   selecting, renaming, and modifying certain columns of the 
#   receptor_nearest_distance_df dataframe and then storing the result in 
#   output_receptor_nearest_distance_df.
##############################################

# Load the readxl package
library(readxl)

trainsform_receptor_sf <- 
  function (receptor_filepath,
            write_log_to_file,
            print_log_to_console,
            npl_year,
            start_year = NULL,
            end_year = NULL,
            receptor_crs,
            check_near_us_border
            ){
    print("llllllll1")               
    receptor_df <- readr::read_csv(receptor_filepath, show_col_types = FALSE) 
    print("kkkkkkkkkk2")
    ##########################################################
    #
    #  npl_check_point_receptor_format.R
    #  Function validate input file receptor and log the information
    #  Detail see file npl_check_point_receptor_format.R
    ##########################################################
    npl_check_point_receptor_format(receptor = receptor_df, 
                                year = year(npl_year),
                                start_year = NULL,
                                end_year = NULL,
                                time_option = "single_year_only",
                                print_log_to_console = print_log_to_console, 
                                write_log_to_file = write_log_to_file)
    
    receptor_sf <- sf::st_as_sf(receptor_df, coords = c('longitude','latitude'), 
                                crs = receptor_crs)
    return(receptor_sf)
  }
############################################
# function check_us_border
# Check if point receptors are within buffer of border 
# Validate US borders' file and data frame us_borders_sf 
# US border file require to be or can be convert to a simple features object 
# with 30 rows and 2 columns
# Log the information
#
############################################
check_us_border <- 
  function(check_near_us_border,
           us_borders_filepath,
           buffer_distance_km,
           write_log_to_file,
           print_log_to_console,
           projection_crs,
           receptor_sf){
    
    if(check_near_us_border == TRUE) {
      
      us_borders_sf <- readr::read_rds(us_borders_filepath) 
      if(("sf" %in% class(us_borders_sf)) == FALSE) {
        stop("Required argument 'us_borders_filepath' must lead to a simple features object.")
      }
      if((nrow(us_borders_sf) != 30) | (ncol(us_borders_sf) != 2)) {
        stop("'Required argument 'us_borders_filepath' must lead to a simple features object with 30 rows and 2 columns.")
      }
      print("Invocate receptor_border_check_df ()")
      receptor_border_check_df <- 
        check_point_receptor_near_border(receptor_sf,
                                         border_sf = us_borders_sf,
                                         buffer_distance_km = buffer_distance_km,
                                         projection_crs = projection_crs,
                                         print_log_to_console = print_log_to_console, 
                                         write_log_to_file = write_log_to_file) 
      
      output_receptor_border_check_df <- receptor_border_check_df %>% 
        dplyr::select(id, within_border_buffer)
     
      output_df_list <- list(output_receptor_border_check_df) 
      print(length(output_df_list))
      
      return (output_df_list)
    }
  }
##############################################
#
# function : transform_filter_npl_source_points
# Load dataset from excel file 
# filter the data as needed
# Convert to sf object with POINT geometry and return source_npl_sf
##############################################
transform_filter_npl_source_points <- 
  function (source_npl_facilities_filepath,
            p_NPL_STATUS,
            print_log_to_console,
            projection_crs,
            receptor_sf
            ) {
    source_npl_sf <- read_excel(source_npl_facilities_filepath, sheet = "EPA_NPL_Sites_asof_27Feb2014")
    
    logr::put(stringr::str_c("nrow(source_npl_sf):", nrow(source_npl_sf), sep = " "), 
              console = print_log_to_console)
    logr::put(stringr::str_c("ncol=" , ncol(source_npl_sf), sep= ":"),
              console = print_log_to_console)
    
    # if ((nrow(source_aircraft_sf) != 1747) & (ncol(source_aircraft_sf != 16)) ){
    #   stop("Input EPA dataset with 1747 rows and 16 columns.")
    # }
    #filter out NPL_STATUS
    if(is.null(p_NPL_STATUS)){
      logr::put(stringr::str_c("No NPL_STATUS to filter out the dataset."),
                console = print_log_to_console)
    }else {
      #dataset filter by NPL_STATUS
      source_npl_sf_status <- source_npl_sf[source_npl_sf$NPL_STATUS != p_NPL_STATUS, ]
      #activate_npl_points_list <- npl_points_list[npl_points_list$NPL_STATUS != "Deleted from the Final NPL", ]
      
      logr::put(stringr::str_c("NPL_dataset filter by NPL_STATUS :", p_NPL_STATUS))
      #resign the value to source_npl_status
      source_npl_sf <- source_npl_sf_status
    }
    
    if(sf::st_crs(source_npl_sf) != projection_crs) {
      # Convert to sf object with POINT geometry
      source_npl_sf <- st_as_sf(source_npl_sf, coords = c("LONGITUDE","LATITUDE"), crs = 4326)
      
    }
    
    return (source_npl_sf)
  }
#######################################
# function calculate_distance_npl_receptor
# Calculate distance to nearest npl facility from point receptor
# Using functions from the dplyr package, which is part of the tidyverse. 
# The dplyr package provides a suite of functions designed to facilitate 
# data manipulation tasks. The code block is performing a sequence of 
# operations on the receptor_nearest_distance_df data frame 
# using the "pipe" operator (%>%).
# selecting, renaming, and modifying certain columns of the 
# receptor_nearest_distance_df dataframe and then storing the result in 
# output_receptor_nearest_distance_df.
#
#######################################
calculate_distance_npl_receptor <-
  function(proximity_metrics,
           receptor_sf,
           source_npl_sf,
           projection_crs,
           check_near_us_border,
           output_df_list
           ){
    if(("distance_to_nearest" %in% proximity_metrics) == TRUE) {
        receptor_nearest_distance_df <- 
          nearest_distance_to_point_source_from_point_receptor(
                receptor_sf = receptor_sf,
                source_sf = source_npl_sf,
                projection_crs = projection_crs,
                add_nearest_source = TRUE,
                print_log_to_console = TRUE,
                write_log_to_file = TRUE)
        #The %>% is a piping operator which means "then". It's used to pass an
        # object to the following function. In this case, it's passing receptor_nearest_distance_df to the next function.
        # Selecting columns from the dataframe. It selects the 
        # id column and any other columns that start with the string "nearest_".
        # Any column name (except for the column named id) will be prefixed with
        # "NPL_".
         output_receptor_nearest_distance_df <- receptor_nearest_distance_df %>% 
           dplyr::select(id, starts_with("nearest_")) %>%
           dplyr::rename_with(function(x){paste0("NPL_", x)}, .cols = !id) %>%
           dplyr::mutate(NPL_nearest_distance = round(NPL_nearest_distance, digits = 3))
        # If check_near_us_border is FALSE, then a new list output_df_list is 
        # created with just one element: the data frame 
        # output_receptor_nearest_distance_df.
        # If check_near_us_border is TRUE, the dataframe 
        # output_receptor_nearest_distance_df is added as an additional element
        # to the existing output_df_list list. Note that append is used to add 
        # to the list, and the dataframe is wrapped within list() to ensure 
        # that it is added as a new list element.
        if(check_near_us_border == FALSE) {
          output_df_list <- list(output_receptor_nearest_distance_df)
        } else {
          output_df_list <- append(output_df_list, list(output_receptor_nearest_distance_df))
        }
        return (output_df_list)
       }
  }
