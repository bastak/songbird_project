

CountElements = function(df1, days, df_full = data.frame(), normalize = TRUE){
  # This function calculates the number (or fraction) of the elements in df1 across days 
  # If normalize = TRUE (default), then for each day it divides the number of elements in df1 to the number of elements in df_full (?)
  # The idea of this function is to quantify how much elements we lose (day-wise) if we subsetting df_full -> df1
  # 
  # Returns: df with 2 columns: "day" - corresponding day, "x" - number of elements within this day (if normalize = TRUE, then returns fraction)
  df1_count = aggregate(df1$element_id, by=list(day = df1$day), FUN = length)
  if (nrow(df_full) > 0){
    df_full_count = aggregate(df_full$element_id, by=list(day = df_full$day), FUN = length)
    df_full_count = df_full_count[order(df_full_count$day), ]
    missing_days_df1 = setdiff(days, df1$day)  # stupid hack to make it work in case if the number of days in df2 is less than in df_full
    missing_days_df_full = setdiff(days, df_full$day)
    if (length(missing_days_df1) > 0){
      zero_df = data.frame(day = missing_days_df1, x = 0)         #  and we want to have line x = 0 for missing day, aggregate, mat' ego, can't do it
      df1_count = rbind(df1_count, zero_df)
      df1_count = df1_count[order(df1_count$day), ]  # to sort by day because after attaching zero elements days mixed up
    }  # i hate it
    if (length(missing_days_df_full) > 0){
      zero_df = data.frame(day = missing_days_df_full, x = 0)         #  and we want to have line x = 0 for missing day, aggregate, mat' ego, can't do it
      df_full_count = rbind(df_full_count, zero_df)
      df_full_count = df_full_count[order(df_full_count$day), ]  # to sort by day because after attaching zero elements days mixed up
    }  # i hate it
    if (normalize == TRUE){
      df1_count$x = df1_count$x / df_full_count$x
    }
  }
  return(df1_count)
}


PlotElementsNumber = function(df1, df_full, type, df2 = data.frame(), color_adult = "indianred2", normalize = FALSE, chosen_clusters = c(1:38)){
  # Plots statistics across days: how many elements were pronounced inside and outside of communication window
  # Can calculate either: 1) statistics for juvenile (calculates how many elements were pronounced during perch A,
  #                          during perch C, and outside any of the perch - helpful to compare how much 
  #                          juvenile communicated with bird A and bird C)
  #                       2) statistics for adult: it will be just number of elements inside and outside the perch
  #                          (when juvenile decided to talk with this adult)
  # 
  # Args:
  #  df1: df for elements which were inside the window. For example: if plot juvenile: df1 = s7s8_BA, if adult: s7s8_A  
  #  df_full: df for all elements, inside and outside perch. For example: if plot juvenile: df_full = s7s8_B_full, 
  #                                                                               if adult: df_full = s7s8_A_full
  #  type: "juvenile" or "adult", this is needed to decide whether function should plot 3 areas (perch A, perch C, outside)
  #        or 2 areas (just inside and outside for adult)
  #  df2: needed only in case of juvenile (that's why default is NA) - df of elements inside another perch
  #       for example: df2 = s7s8_BC  
  #  color adult: "indianred2" or "mediumseagreen", depending which adult you plot, A (red) or C (green)
  #  normalize: FALSE or TRUE, to normalize the number of elements to the total number of elements within this day or not
  #  chosen_clusters: vector of clusters to keep (for example c(1,2,20)). By default: c(1:38) - to filter out garbage elements
  #
  # Returns: area plot with distribution of the number of elements across days
  chosen_days = unique(df_full$day)  # put it on top in case 
  df1_filtered = filter(df1, cluster_id %in% chosen_clusters)
  df_full_filtered = filter(df_full, cluster_id %in% chosen_clusters)
  if (nrow(df2) > 0){
    df2_filtered = filter(df2, cluster_id %in% chosen_clusters)
  }
  
  if (normalize == TRUE){
    y_name = "Fraction"
  } else {
    y_name = "Number"
  }
  
  if (type == "juvenile"){  
    df1_count = CountElements(df1 = df1_filtered, days = chosen_days, df_full = df_full_filtered, normalize = normalize)
    df2_count = CountElements(df1 = df2_filtered, days = chosen_days, df_full = df_full_filtered, normalize = normalize)
    df_count_outside = df1_count
    if (normalize == TRUE){
      df_count_outside$x = 1 - df1_count$x - df2_count$x
    } else {
      df_full_count = CountElements(df1 = df_full_filtered, days = chosen_days, df_full = df_full_filtered, normalize = normalize)
      df_count_outside$x = df_full_count$x - df1_count$x - df2_count$x
    }
    df_count_joined = rbind(cbind(df_count_outside, channel="outside"), cbind(df1_count, channel="bird A"), cbind(df2_count, channel="bird C"))
    # for (i in 1:length(chosen_days)){  # trick to be able to plot area graph (it doesn't allow if you have strings or factors on x axis)
    #   df_count_joined[which(df_count_joined == chosen_days[i]), "day_num"] = i
    # }
    ggplot()+geom_col(data=df_count_joined, aes(x=day, y=x, fill=channel), position = "dodge")+
      scale_fill_manual(values=c("grey", "indianred2", "mediumseagreen"))+
      theme(axis.text.x = element_text(angle = 45))+xlab("Day")+ylab(paste(y_name, "of vocalizations"))
  } else {
    df1_count = CountElements(df1 = df1_filtered, days = chosen_days, df_full = df_full_filtered, normalize = normalize)
    df_count_outside = df1_count
    if (normalize == TRUE){
      df_count_outside$x = 1 - df1_count$x
    } else {
      df_full_count = CountElements(df1 = df_full_filtered, days = chosen_days, df_full = df_full_filtered, normalize = normalize)
      df_count_outside$x = df_full_count$x - df1_count$x
    }
    df_count_joined = rbind(cbind(df_count_outside, channel="outside"), cbind(df1_count, channel="inside"))
    # for (i in 1:length(chosen_days)){  # trick to be able to plot area graph (it doesn't allow if you have strings or factors on x axis)
    #   df_count_joined[which(df_count_joined == chosen_days[i]), "day_num"] = i
    # }
    ggplot()+geom_col(data=df_count_joined, aes(x=day, y=x, fill=channel), position = "dodge")+
      scale_fill_manual(values=c("grey", color_adult))+
      theme(axis.text.x = element_text(angle = 45))+xlab("Day")+ylab(paste(y_name, "of vocalizations"))
  }
}


PlotPerchTimes = function(perch_A, perch_C, type="area", normalize=TRUE){
  # plots total time the bird sat on the perch across days (it is just general statistics)
  # Note: now you calculate time outside as the time outside any perch if we start counting from the moment
  # the bird first sat on the perch and until it used the perch the last time during the day.
  # Maybe it is better to count from the first vocalization of juvenile bird??
  #  type: "area" or "line"
  #
  #
  #
  #
  perch = rbind(perch_A, perch_C)
  days = unique(perch$day)
  
  perch_A_total_time = aggregate(perch_A$duration / 3600, by=list(day=perch_A$day), FUN=sum) 
  perch_C_total_time = aggregate(perch_C$duration / 3600, by=list(day=perch_C$day), FUN=sum)  
  missing_days_A = setdiff(days, perch_A_total_time$day)  # stupid hack to make it work in case if the number of days in df2 is less than in df_full
  missing_days_C = setdiff(days, perch_C_total_time$day)
  if (length(missing_days_A) > 0){
    zero_df = data.frame(day = missing_days_A, x = 0)         #  and we want to have line x = 0 for missing day, aggregate, mat' ego, can't do it
    perch_A_total_time = rbind(perch_A_total_time, zero_df)
    perch_A_total_time = perch_A_total_time[order(perch_A_total_time$day), ]  # to sort by day because after attaching zero elements days mixed up
  }  # i hate it
  if (length(missing_days_C) > 0){
    zero_df = data.frame(day = missing_days_C, x = 0)         #  and we want to have line x = 0 for missing day, aggregate, mat' ego, can't do it
    perch_C_total_time = rbind(perch_C_total_time, zero_df)
    perch_C_total_time = perch_C_total_time[order(perch_C_total_time$day), ]  # to sort by day because after attaching zero elements days mixed up
  }  # i hate it
  
  time_min = aggregate(perch$time_h, by=list(Category=perch$day), FUN=min)
  time_max = aggregate(perch$time_h + perch$duration / 3600, by=list(day=perch$day), FUN=max)
  time_outside = time_max  # just trick to copy df structure
  time_outside$x = time_max$x - time_min$x - perch_A_total_time$x - perch_C_total_time$x  # time which bird spent outside any perch
  norm = time_max
  norm$x = time_max$x - time_min$x
  
  if (normalize == TRUE){
    time_outside$x = time_outside$x / norm$x
    perch_A_total_time$x = perch_A_total_time$x / norm$x
    perch_C_total_time$x = perch_C_total_time$x / norm$x
    time_joined = rbind(cbind(time_outside, channel="outside"), cbind(perch_A_total_time, channel="perch A"), cbind(perch_C_total_time, channel="perch C"))
  } else {
    time_joined = rbind(cbind(time_outside, channel="outside"), cbind(perch_A_total_time, channel="perch A"), cbind(perch_C_total_time, channel="perch C"))
  }
  
  for (i in 1:length(days)){  # change all strings of days to the number because otherwise ggplot refuses to plot the graph:(
    time_joined[which(time_joined == days[i]), "day_num"] = i
  }
  
  if (type == "area"){ 
    ggplot()+geom_col(data=time_joined, aes(x=day, y=x, fill=channel), position = "dodge")+
      scale_fill_manual(values=c("grey", "indianred2", "mediumseagreen"))+theme(axis.text.x = element_text(angle = 45))+
      ylab("Time (Hours)")+xlab("Day")
  } else {
    ggplot()+geom_line(data=time_joined, aes(x=day_num, y=x, color=channel), size=0.8)+
      scale_color_manual(values=c("grey", "indianred2", "mediumseagreen"))+ylab("Time (Hours)")+xlab("Day")
  }
}


     