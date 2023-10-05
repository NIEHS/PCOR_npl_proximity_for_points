################################################################################
# function : npl_proximity_points_summary_metrics
#
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
# Calculate NPL facility summary metrics within buffer distance of point receptor 

################################
npl_proximity_points_summary_metrics <-
  function(proximity_metrics,
           receptor_sf = receptor_sf,
           source_npl_sf,
           summary_metrics = summary_metrics,
           buffer_distance_km = buffer_distance_km,
           projection_crs = projection_crs,
           print_log_to_console = print_log_to_console,
           write_log_to_file = write_log_to_file,
           add_all_to_output,
           check_near_us_border,
           output_df_list
           ){
    #n_col <- ncol(source_npl_sf)
    #n_row <- nrow(source_npl_sf)
    #print(n_col)
    #print(n_row)
    if((("count_in_buffer" %in% proximity_metrics) |
        ("distance_in_buffer" %in% proximity_metrics)) == TRUE) {
      
      summary_metrics <- proximity_metrics[which((proximity_metrics == "distance_in_buffer") | 
                                                   (proximity_metrics == "count_in_buffer"))]
      ############
      #  function  add column for lists of source points in each receptor point buffer
      #            add column for count of source points in each receptor point buffer
      #            Initialize a matrix for collecting mean distance and mean of log distance
      #            
      #############
      receptor_summary_distance_metrics_df <- 
        summary_of_point_sources_in_buffer_of_point_receptor(receptor_sf = receptor_sf,
                                                             source_npl_sf,
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
        dplyr::rename_with(function(x){paste0("NPL_", x)}, .cols = !id) 
      # It takes the receptor_summary_distance_metrics_df dataframe, selects and rounds 
      # specific columns, then renames those columns by adding a prefix, and finally
      # stores the output in a new dataframe.
      if((check_near_us_border == FALSE) &
         (("distance_to_nearest" %in% proximity_metrics) == FALSE)) {
        output_df_list <- list(output_receptor_summary_distance_metrics_df) 
      } else {
        output_df_list <- append(output_df_list, list(output_receptor_summary_distance_metrics_df))
      }
    }
    
   return (output_df_list) 
  }
