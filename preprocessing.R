# This script contains functions for primary preprocessing of extracted data with extract_mydata_Roman.m

Preprocessing = function(df, perch_df){
  # Deletes some randomly appeared column X, converts date from serial number to proper date, converts cluster_ids to factor variables
  #
  # Args:
  #   Dataframe, extracted from flatclust via script "extract_mydata_Roman.m"
  #
  # Returns: 
  #   Processed dataframa which was fed
  df$X = NULL
  df$cluster_id = as.factor(df$cluster_id)
  df$day_serial = as.Date(df$day_serial- 719529, origin = "1970-01-01")
  colnames(df)[which(names(df) == "day_serial")] = "day"
  df$day = as.character(df$day)
  df = MapElementsInsidePerches(df, perch=perch_df)
  df = df[order(df$day, df$time_h), ]  # first order by day, then by timestamp
  rownames(df) = seq(length=nrow(df))  # reset indices 
  return(df)
}

PreprocessingPerches = function(df){
  # Almost the same as Preprocessing, this only doesn't have cluster_id preprocessing because there is no such column in raw dataframe
  # Deletes some randomly appeared column X, converts date from serial number to proper date, converts cluster_ids to factor variables
  #
  # Args:
  #   Dataframe, extracted from flatclust via script "extract_perches_Roman.m"
  #
  # Returns: 
  #   Processed dataframa which was fed
  df$X = NULL
  df$day_serial = as.Date(df$day_serial- 719529, origin = "1970-01-01")
  colnames(df)[which(names(df) == "day_serial")] = "day"
  df$day = as.character(df$day)
  return(df)
}

MapElementsInsidePerches= function(bird_df, perch) {
  # Creates a new dataframe with additional columns: "window", "t1", "t2"
  # "window": tells whether the given element is inside or outside any of the perch window
  # "t1" - timestamp of the beginning of the communication window where this element was pronounced
  # "t2" - timestamp of the end of the communication window where this element was pronounced
  # NOTE: if we have element outside window, t1 and t2 of this element will indicate the begin and end of its interval between perch windows
  # Args:
  #  bird_df: dataframe with given bird
  #  perch: dataframe containing timestamps of perch opening and closing timestamps
  #
  # Returns:
  #  input dataframe bird_df, but with additional added columns
  #
  # NOTE: t1 and t2 - are timestamps of begin and end of communication window for give element IN HOURS!
  bird_df_mapped = bird_df
  perch = perch[with(perch, order(day, time_h)), ]
  rownames(perch) = seq(length=nrow(perch))  # reset indices 
  
  CheckIfInside = function(bird_day, bird_time_h) {  # function to assign window, t1 and t2 to element
    temp_index = which(perch[, "day"] == bird_day &
                         perch[, "time_h"] < bird_time_h &  # find index of window which contains given element (if contains)
                         perch[, "time_h"] + perch[, "duration"] / 3600 > bird_time_h)  # there should be only one element by definition, so we don't need [1], I did it just in case
    
    if (length(temp_index) > 0) {
      inside_t1 = perch[temp_index, "time_h"]
      inside_t2 = perch[temp_index, "time_h"] + perch[temp_index, "duration"] / 3600
      results = c(1, inside_t1, inside_t2)
      return(results)
    } else {
      outside_index = tail(which(perch[, "day"] == bird_day & perch[, "time_h"] + perch[, "duration"] / 3600 < bird_time_h), n=1)  # take last index of the window before the outsider - need it for assigning between-windows timestamps
      if (length(outside_index) == 0) {  # check if this element appeared before any perch window in this day
        outside_t1 = 8.0
        outside_t2 = perch[which(perch[, "day"] == bird_day)[1], "time_h"]  # assign timestamp of the first window in this day
      } else {
        outside_t1 = perch[outside_index, "time_h"] + perch[outside_index, "duration"] / 3600 
        if (outside_index < nrow(perch)) {
          if (perch[outside_index, "day"] == perch[outside_index + 1, "day"]) {  # Check if the given timestamp was after any time window, assign artificial time 10.5 (otherwise it will automativally assign timestamp of the next day - don't want it)
            outside_t2 = perch[outside_index + 1, "time_h"]  # assign timestamp of the next window 
          } else {
            outside_t2 = 10.5
          }
        } else {
          outside_t2 = 10.5
        }
      } 
      
      results = c(0, outside_t1, outside_t2)
      return(results)
    }
  }
  results_matrix = mapply(CheckIfInside, bird_df_mapped$day, bird_df_mapped$time)
  bird_df_mapped$window  = ifelse(results_matrix[1, ] == 1, "inside", "outside")  # need this trick to be able to work with mapply - if we want to return several results,
  bird_df_mapped$t1 = results_matrix[2, ]                                         # it can do it only with results of the same format, but we have char, double, double
  bird_df_mapped$t2 = results_matrix[3, ]
  return(bird_df_mapped)
}