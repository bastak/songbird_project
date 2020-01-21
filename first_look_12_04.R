# Combination of preliminary visualization of data of vocalizations between juvenile bird (B) with adult birds A and C

#setwd('C:/cmatlab-Roman/DivPrograms/Individual/RomanDoronin/Scripts/Analysis')  
library(ggplot2)
#library(spatstat)
library(dplyr)
#library(plotly)
#library(egg)
#library(cowplot)
#source("analyze_perches.R")
theme_set(theme_grey())  # to return cool grey background which disappears after calling cowplot

# # This is basic function
# PlotTimestamps = function(juvenile_df, adult_df){
#   # Plots timestamps of all vocalizations from birds A (blue) and bird B (red) across all days.
#   # Note that function throws away cluster 39 (in this data it is garbage, but in other data garbage cluster can have another number!)
#   #
#   # Args:
#   #   adult_df: processed (via Preprocessing) data from one of the adult bird 
#   #   juvenile_df: processed (via Preprocessing) data from juvenile bird
#   #
#   # Returns:
#   #   Plot of the timestamps of vocalizations of juvenile and adult birds across all days (39 cluster not included)
#   choose_cluster = c(2:38)
#   adult_filtered = filter(adult_df, cluster_id %in% choose_cluster)          
#   juvenile_filtered = filter(juvenile_df, cluster_id %in% choose_cluster)  
#   p = ggplot()+geom_point(data=adult_filtered, aes(time_h, 1), size=1, color='blue')+
#                   geom_point(data=juvenile_filtered, aes(time_h, 2), size=1, color='red')+ylim(0,4)+
#                   facet_wrap(~day)+scale_colour_brewer(palette = "Set1")
#   return(p) 
# }
# PlotTimestamps(s7s8_BA, s7s8_A)
# PlotTimestamps(s7s8_B_none, s7s8_A_none)
# PlotTimestamps(s7s8_B_full, s7s8_A_full)



#---- functions for the stack plot BEGIN ----

SubsetInWindow = function(df, time_1, time_2, t_normalize, df_name = "time_h", y_axis = "index"){
  # Subsets elements from df where df_name is between t1 and t2 and normalize by the value t_normalize
  # Will subset only within given day - you will need to specify day outside this function
  stack_df = cbind(df[1, ], counter = 0)
  for (index in 1:length(time_1)) {
    df_in_window = filter(df, df[, df_name] < time_2[index]  & # choose all adults within [time_1, time_2] window of juvenile element i
                            df[, df_name] > time_1[index])
    
    if (nrow(df_in_window) > 0){  # if condition - in case if we don't find any adults element within window
      if (y_axis == "index") { # specifying Y-coordinate for lines
        df_in_window$counter = index
      } else {
        df_in_window$counter = time_1[index]
      }
      df_in_window[, df_name] = (df_in_window[, df_name] - t_normalize[index]) * 3600  # align adult elements to the beginning of juvenile element
      stack_df = rbind(stack_df, df_in_window)  # append adults within window
    }
  }
  stack_df = stack_df[-1, ]  # get rid of seed element
  rownames(stack_df) = seq(length=nrow(stack_df))  # reset indices 
  return(stack_df)
}


DFForStackPlot = function(juvenile_df, adult_df, chosen_days="all", window_length=5,
                                   juvenile_chosen_clusters=c(1:38), adult_chosen_clusters=c(1:38),
                                   y_axis="index") {
  # This creates stack dataframe (stack_df) needed for function "CreateStackPlot" for plotting stack of lines (line=vocalization)
  # as in paper Okobi2019 ("Motor cortical control of vocal interaction in neotropical singing mice"):
  # Within 5 sec window we plot vocalizations of adult around each vocalization of juvenile bird 
  # (we set zero time at the end of each juvenile vocalization)
  # Different color - different cluster. 
  # Note: by far we can't compare colors of adult with colors of juvenile 
  # (i.e. blue line in adult and blue line in juvenile - not necessarily same type of vocalization! Need to create a mapping between adult and juvenile cluster!)
  #
  # Args:
  #   juvenile_df: processed (via Preprocessing) data from juvenile bird
  #   adult_df: processed (via Preprocessing) data from one of the adult bird 
  #   chosen_days: vector of dates for which we want to plot stack plot. By default it plots all days, BUT:
  #                if you want to specify custom days, you need to provide a vector of days, EVEN if it one day. e.g. chosen_days=c("2018-07-28") 
  #   window_length: numeric, length of time window in sec, by default window_length = 5 sec,  i.e. it filters data in [-5, 5] sec window
  #   juvenile_chosen_clusters: vector of ids of clusters of juvenile you want to plot, by default plots all clusters except garbage 39. 
  #                             Note 1: for s7s8 data, 2nd version of experiment our garbage cluster is 39, for other data it can be different! Change default if needed
  #                             Note 2: provide vector, if you need only one cluster, you still need specify it as vector e.g. c(2)
  #   adult_chosen_clusters: vector of ids of clusters of adult you want to plot. Same notes as for juvenile clusters
  #   y_axis: "index" or "timestamp". If "index" - plots indices of juvenile elements along Y. (elements don't overlap)
  #                                   If "timestamp" - plots timestamps of juvenile elements along Y (can see temporal structure of elements)
  #                                   Default: "index"
  
  # We need to create a dataframe (stack_df) of all juveniles elements [within some subset of days (specified in call) 
  # + in some subset of clusters(speciefed in call)] AND all adult elements within 5-sec time window around each juvenile element
  # For convinience we need to add 3 additional columns to our stack_df (to be able to conviniently plot graphs):
  # 1) bird: takes values 'A' or 'B', because as in our stack_df we will have both elements from juvenile and adult, we need to somehow discern them
  # 2) counter: integer number numerating any juvenile element and corresponding adult elements with the same number 
  # (basically it just specifies y-coordinate on stack plot for elements, the smaller the coordinate - the earlier this element was pronounced)
  # Note: we can change counter between just index and the timestamps of elements on y-axis. If use index, then elements don't overlap,
  # but not possible to understand temporal relationships between elements, with timestamps - inversely
  
  if (chosen_days=="all") {
    chosen_days = unique(juvenile_df$day)
  }
  # Choose only the elements of interest: within specified subset of days and within specified subset of clusters, for juvenile and adult - SEPARATE specifications for clusters of interest to allow more flexibility
  filtered_juvenile = filter(juvenile_df, day %in% chosen_days, cluster_id %in% juvenile_chosen_clusters) # CHANGE TO %IN% extracts only juvenile only within given subset of days and only from given subset of clusters
  filtered_juvenile = filtered_juvenile[order(filtered_juvenile$day, filtered_juvenile$time_h), ] # orders the juvenile elements: first go from earlier days, earlier times
  rownames(filtered_juvenile) = seq(length=nrow(filtered_juvenile))  # reset indices
  filtered_adult = filter(adult_df, day %in% chosen_days, cluster_id %in% adult_chosen_clusters) 
  stack_df = cbind(filtered_juvenile[1, ], counter=0, bird = "juvenile") # creates a seed for rbinding other pieces of dataframe (we need something to attach dataframes to)
  # Finds all adult elements within given 5-sec window of each juvenile element and appends it to stack_df (we append all juvenile elements too)
  # First, iterate across all days:
  for (day_index in 1:length(chosen_days)) {
    #print(filter(filtered_juvenile, day == chosen_days[day_index]))
    # if else condition is needed if we have so strict filter that there are no juvenile elements, but we still want to plot empty window for consistency 
    if (nrow(filter(filtered_juvenile, day == chosen_days[day_index])) > 0){
      filtered_juvenile_within_day = filter(filtered_juvenile, day == chosen_days[day_index])
    } else {
      filtered_juvenile_within_day = juvenile_df[1, ]#  data.frame(element_id=as.integer(-1), "2018-07-28", time_h=8.0, duration=0.001, cluster_id=as.factor(39), bird="B", juv_element_id=-1, counter=1)
      filtered_juvenile_within_day$day = chosen_days[day_index]
      filtered_juvenile_within_day$time_h = 8.0
      filtered_juvenile_within_day$cluster_id = 39  # PAY ATTENTION TO THE INDEX OF THIS PSEUDOCLUSTER SO IT WON'T BE FILTERED OUT FURTHER ON THE ROAD! 
      filtered_juvenile_within_day$duration = 0.01  # (for example later you filter out 39 cluster and this code for adding pseudoelement will be useless)
    }
    if (y_axis == "index"){
      filtered_juvenile_within_day$counter = 1:nrow(filtered_juvenile_within_day)
      filtered_juvenile_within_day$counter = as.numeric(filtered_juvenile_within_day$counter)
    } else{
      filtered_juvenile_within_day$counter = filtered_juvenile_within_day$time_h
    }
    
    filtered_adult_within_day = filter(filtered_adult, day == chosen_days[day_index])
    juvenile_t1 = filtered_juvenile_within_day$time_h + filtered_juvenile_within_day$duration / 3600 - window_length / 3600  # assign left timestamp of the window, do not need it anywhere else, just created new variable for readability
    juvenile_t2 = filtered_juvenile_within_day$time_h + filtered_juvenile_within_day$duration / 3600 + window_length / 3600  # assign right timestamp of the window
    juvenile_offset = filtered_juvenile_within_day$time_h + filtered_juvenile_within_day$duration / 3600
    temp_stack_df = SubsetInWindow(filtered_adult_within_day, juvenile_t1, juvenile_t2, juvenile_offset)
    #print(temp_stack_df)
    #print(filtered_juvenile_within_day)
    temp_juvenile = filtered_juvenile_within_day[, -which(colnames(filtered_juvenile_within_day) == "counter")]
    #print(temp_juvenile)
    juvenile_temp_stack_df = SubsetInWindow(temp_juvenile, juvenile_t1, juvenile_t2, juvenile_offset)

    filtered_juvenile_within_day$time_h = -filtered_juvenile_within_day$duration  # normalize by juvenile offset
    #stack_df = rbind(stack_df, cbind(filtered_juvenile_within_day, bird = "juvenile"))
    stack_df = rbind(stack_df, cbind(juvenile_temp_stack_df, bird = "juvenile"))
    if (nrow(temp_stack_df) > 0){
      stack_df = rbind(stack_df, cbind(temp_stack_df, bird = "adult"))  # hack to avoid error of attaching column "adult" to a possibly empty dataframe
    }
  }
  stack_df = stack_df[-1, ]  # get rid of seed element
  rownames(stack_df) = seq(length=nrow(stack_df))  # reset indices 
  return(stack_df)
}


CreateStackPlot = function(juvenile_df, adult_df, chosen_days="all", window_length=2,
                           juvenile_chosen_clusters=c(1:38), adult_chosen_clusters=c(1:38),
                           y_axis="index", display="both", coloring="clusters", dataframe="none"){
  # This creates stack of lines (line=vocalization) as in paper Okobi2019 ("Motor cortical control of vocal interaction in neotropical singing mice"):
  # Within 5 sec window we plot vocalizations of adult around each vocalization of juvenile bird 
  # (we set zero time at the end of each juvenile vocalization)
  # Different color - different cluster. 
  # Note: by far we can't compare colors of adult with colors of juvenile 
  # (i.e. blue line in adult and blue line in juvenile - not necessarily same type of vocalization! Need to create a mapping between adult and juvenile cluster!)
  #
  # Args:
  #   juvenile_df: processed (via Preprocessing) data from juvenile bird
  #   adult_df: processed (via Preprocessing) data from one of the adult bird 
  #   chosen_days: vector of dates for which we want to plot stack plot. By default it plots all days, BUT:
  #                if you want to specify custom days, you need to provide a vector of days, EVEN if it one day. e.g. chosen_days=c("2018-07-28") 
  #   window_length: numeric, length of time window in sec, by default window_length = 5 sec,  i.e. it filters data in [-5, 5] sec window
  #   juvenile_chosen_clusters: vector of ids of clusters of juvenile you want to plot, by default plots all clusters except garbage 39. 
  #                             Note 1: for s7s8 data, 2nd version of experiment our garbage cluster is 39, for other data it can be different! Change default if needed
  #                             Note 2: provide vector, if you need only one cluster, you still need specify it as vector e.g. c(2)
  #   adult_chosen_clusters: vector of ids of clusters of adult you want to plot. Same notes as for juvenile clusters
  #   y_axis: "index" or "timestamp". If "index" - plots indices of juvenile elements along Y. (elements don't overlap)
  #                                   If "timestamp" - plots timestamps of juvenile elements along Y (can see temporal structure of elements)
  #                                   Default: "index"
  #   display: "both", "juvenile" or "adult". If "both" - plots both stack of juvenile elements and stack of adult elements
  #                                           If "juvenile" - plots only juvenile elements
  #                                           If "adult" - only adult elements
  #                                           Note: This can plot only coloring by clusters, if you specify coloring = "bird X", function will not care about option "display"
  #                                           Default: "both"
  #   coloring: "clusters" or "birds". If "clusters" - colors each element (line) in color of corresponding cluster
  #                                    if "bird A" - colors each element in color of bird A (indianred2)
  #                                    if "bird C" - colors each element in color of bird A (mediumseagreen)
  #                                    Default: "clusters"
  #   dataframe: "none" or processed dataframe (obtained via "DFForCreateStackPlot"). If "none" - calls DFForCreateStackPlot
  #                                                                                   If you already got df, then specify it as argument to be able to speed up playing with plotting
  #   NOTE: by far arguments "display" and "coloring" are XOR! You specify either display or coloring (because otherwise the code wil break and you don't need to select only one bird which will be painted with one color)

  if (chosen_days=="all") {
    chosen_days = unique(juvenile_df$day)
  }
  
  if (dataframe == "none") {
    stack_df = DFForStackPlot(juvenile_df, adult_df, chosen_days=chosen_days, window_length=window_length,
                                    juvenile_chosen_clusters=juvenile_chosen_clusters, adult_chosen_clusters=adult_chosen_clusters,
                                    y_axis=y_axis) 
  } else {
    stack_df = dataframe
  }
  stack_df = filter(stack_df, day %in% chosen_days, cluster_id %in% union(juvenile_chosen_clusters, adult_chosen_clusters))
  
  
  
  # plot everything:
  # p = ggplot(data=stack_df)+geom_segment(aes(x=time_h, y=counter, xend=time_h+duration, yend=counter, color=cluster_id), size=1.6)+
  #   xlab('Time, sec')+ylab('Index/Time (h)')+facet_wrap(~day)

  if (display == "both") {
    p = ggplot(data=stack_df)+geom_segment(aes(x=time_h, y=counter, xend=time_h+duration, yend=counter, color=cluster_id), size=1.6)+
    xlab('Time, sec')+ylab('Index/Time (h)')+facet_wrap(~day, scales = "free")+xlim(-window_length, window_length)
  }
  if (display == "juvenile") {
    p = ggplot(data=filter(stack_df, bird == "juvenile"))+geom_segment(aes(x=time_h, y=counter, xend=time_h+duration, yend=counter, color=cluster_id), size=1.6)+
      xlab('Time, sec')+ylab('Index/Time (h)')+facet_wrap(~day, scales = "free")+xlim(-window_length, window_length)
  }
  if (display == "adult") {
    p = ggplot(data=filter(stack_df, bird == "adult"))+geom_segment(aes(x=time_h, y=counter, xend=time_h+duration, yend=counter, color=cluster_id), size=1.6)+
      xlab('Time, sec')+ylab('Index/Time (h)')+facet_wrap(~day, scales = "free")+xlim(-window_length, window_length)
  }
  
  if (coloring == "bird A") {
    p = ggplot(data=stack_df)+geom_segment(aes(x=time_h, y=counter, xend=time_h+duration, yend=counter, color=bird), size=1.6)+
      xlab('Time, sec')+ylab('Index/Time (h)')+facet_wrap(~day, scales = "free")+xlim(-window_length, window_length)+scale_color_manual(values=c("dodgerblue2", "indianred2"))
  }
  
  if (coloring == "bird C") {
    p = ggplot(data=stack_df)+geom_segment(aes(x=time_h, y=counter, xend=time_h+duration, yend=counter, color=bird), size=1.6)+
      xlab('Time, sec')+ylab('Index/Time (h)')+facet_wrap(~day, scales = "free")+xlim(-window_length, window_length)+scale_color_manual(values=c("dodgerblue2", "mediumseagreen"))
  }
  return(p)  # warning message "removed X rows" appears only if I explicitly specify xlim. Is it because some elements end after window_length?
}

#---- functions for the stack plot END ----



#---- functions for the cumulative plot a.k.a. Peristimulus Time Histogram BEGIN ----

CalculatePeristimulusHistogram = function(df, dt, window_length, z){
# Input should be during ONE day!
  time = seq(-window_length, window_length, dt)
  y = sapply(time, function(x) nrow(filter(df, time_h > x & time_h < x + dt))) / z  # simple peristimulus time histogram, WITHOUT taking into account durations
  return(y)
}


DFForCumulativePlot = function(stack_df, window_length, dt){
  # Creates dataframe of peristimulus time histogram out of stack df.
  # Important concept: all the specification (clusters, days) is made beforehand, during calling the function which makes stack_df
  #
  # Args:
  #  stack_df: df from the DFForStackPlot - specify all your arguments (clusters and days) there!
  #  window_length: time in sec. shows what is the neighbourhood in which we look within juvenile element. i.e. if window_length = 5, then for each element we look in [-5, 5] sec interval of each element
  #  dt: time in sec. Time of discretization: what is the size of the step with which we divide our time axis of PSTH
  #
  # Returns:
  #  dataframe, with columns: t: timestamps, our discretized timeline
  #                           y: value of PSTH (so what we plot is simply t-y plot)
  #                           day: corresponding day 
  #                           bird: for which bird this point stands. "juvenile" or "adult". "juvenile" is the bird whose elements are aligned to zero
  #
  chosen_days = unique(stack_df$day)  # extract what days we specifief beforehand (because this function initially doesn't know it! We didn't pass chosen days as an argument!)
  time = seq(-window_length, window_length, dt)
  cumulative_df = data.frame(t=0.0, y=0.0, day="1996-05-28", bird="FINCHY") # analog of stack_df. Create seed
  for (day_index in 1:length(chosen_days)) {
    filtered_juvenile = filter(stack_df, bird == "juvenile" & day == chosen_days[day_index])
    filtered_adult = filter(stack_df, bird == "adult" & day == chosen_days[day_index])
    y_adult = CalculatePeristimulusHistogram(filtered_adult, dt=dt, window_length=window_length, z=nrow(filtered_juvenile))  # this is proper peristimulus time histogram without any durations
    y_juvenile = CalculatePeristimulusHistogram(filtered_juvenile, dt=dt, window_length=window_length, z=nrow(filtered_juvenile))
    cumulative_df = rbind(cumulative_df, cbind(t=as.numeric(time), y=as.numeric(y_adult), day=chosen_days[day_index], bird="adult"))  
    cumulative_df = rbind(cumulative_df, cbind(t=as.numeric(time), y=as.numeric(y_juvenile), day=chosen_days[day_index], bird="juvenile"))
    
   # cumulative_df = rbind(cumulative_df, cbind(t=as.numeric(time + dt), y=as.numeric(y_adult), day=chosen_days[day_index], bird="adult"))  
   # cumulative_df = rbind(cumulative_df, cbind(t=as.numeric(time + dt), y=as.numeric(y_juvenile), day=chosen_days[day_index], bird="juvenile"))
  }
  cumulative_df = cumulative_df[-1, ]  # get rid of seed
  cumulative_df$t = as.numeric(cumulative_df$t) # no idea why t and y is transformed to character, i just added duck tape as.numeric
  cumulative_df$y = as.numeric(cumulative_df$y)
  return(cumulative_df)
}


CreateCumulativePlot = function(juvenile_df, adult_df, chosen_days="all", window_length=2,
                           juvenile_chosen_clusters=c(1:38), adult_chosen_clusters=c(1:38), dataframe="none", dt=0.1, coloring = "bird A"){
  # Almost identical copy of CreateStackPlot for calculating cumulating density of vocalizations inside time window interval (as in Okobi2019)
  # Copy was made just for having two separate functions for more flexibility 
  #
  # Args:
  #   juvenile_df: processed (via Preprocessing) data from juvenile bird
  #   adult_df: processed (via Preprocessing) data from one of the adult bird 
  #   chosen_days: vector of dates for which we want to plot stack plot. By default it plots all days, BUT:
  #                if you want to specify custom days, you need to provide a vector of days, EVEN if it one day. e.g. chosen_days=c("2018-07-28") 
  #   window_length: numeric, length of time window in sec, by default window_length = 5 sec,  i.e. it filters data in [-5, 5] sec window
  #   juvenile_chosen_clusters: vector of ids of clusters of juvenile you want to plot, by default plots all clusters except garbage 39. 
  #                             Note 1: for s7s8 data, 2nd version of experiment our garbage cluster is 39, for other data it can be different! Change default if needed
  #                             Note 2: provide vector, if you need only one cluster, you still need specify it as vector e.g. c(2)
  #   adult_chosen_clusters: vector of ids of clusters of adult you want to plot. Same notes as for juvenile clusters
  #   dataframe: 
  #   dt:
  #   coloring: "bird A" or "bird C" - just chooses the color with which we plot psth for the adult bird
  #
  #
  if (chosen_days=="all") {
    chosen_days = unique(juvenile_df$day)
  }
  
  if (dataframe == "none") {
    stack_df = DFForStackPlot(juvenile_df, adult_df, chosen_days=chosen_days, window_length=window_length,
                              juvenile_chosen_clusters=juvenile_chosen_clusters, adult_chosen_clusters=adult_chosen_clusters,
                              y_axis="index") 
  } else {
    stack_df = dataframe
  }
  
  cumulative_df = DFForCumulativePlot(stack_df=stack_df, window_length=window_length, dt=dt)
  
  if (coloring == "bird A") {
    color_adult = "indianred2"
  } else {
    color_adult = "mediumseagreen"
  }
  
  p = ggplot(data=cumulative_df)+geom_step(aes(t, y, color=bird), size=0.5)+
    geom_step(aes(t, y, color=bird), size=0.5)+facet_wrap(~day)+
    xlab('Time, sec')+ylab('n(t) / n')+xlim(-window_length, window_length)+scale_color_manual(values=c(color_adult, "dodgerblue2"))
  return(p)
}

#---- functions for the cumulative plot a.k.a. Peristimulus Time Histogram END ----






AdvancedPlotTimestamps = function(juvenile_B_none, juvenile_B, juvenile_B_full, 
                                  adult_A_none, adult_A, adult_A_full,
                                  adult_C_none, adult_C, adult_C_full, perch_A, perch_C, 
                                  chosen_days="all", choose_cluster_A=c(1:38), choose_cluster_B=c(1:38), choose_cluster_C=c(1:38)){
  # This is advanced timestamps plot: plots timestamps of elements of different birds across specified days
  # 1) can jump to any day by changing chosen_day
  # 2) plots full data, data within communication window and data outside any communication window. 
  # 
  # Args:
  #  [juvenile, adult]_[A,B,C]_[none, _, full] - dataframe with corresponding data
  #                                              none: outside any communication window
  #                                              -   : within communictaion window between ONLY ONE BIRD (e.g. A-B)
  #                                              full: data without any selection for any communication window
  #  chosen_days: "all" or specified days in vector format, even if you want one day, e.g. c("2018-07-28")
  #  choose_cluster_[A, B, C]: clusters you want to choose to display. Note: clusters are specified for each bird separately for additional flexibility
  #
  # Returns:
  #  plot object (?)
  
  # This is fucking mess. These 9 ifelse conditions - duck tape in case if any of the filters will be too strict and resulting
  # dataframe for rbinding will be empty and for some weirdo reason rbind CAN'T bind empty arrays! 
  # So here if we have empty array, we instead create some artificial meaningless point (in else condition)
  if (chosen_days=="all") {
    chosen_days = unique(adult_A_full$day)
  }
  
  # adult_A_full = adult_A[1, ]  # to temporarily get rid of the full set of datapoints
  # juvenile_B_full = juvenile_B[1, ]
  
  SubsetDF = function(df, df_label, df_height, df_choose_cluster) {
    if (nrow(filter(df, cluster_id %in% df_choose_cluster, day %in% chosen_days)) > 0) {
      df = cbind(filter(df, cluster_id %in% df_choose_cluster, day %in% chosen_days), label=df_label, height=df_height)
    } else {
      df = cbind(df[1,], label=df_label, height=df_height)
      df$day = chosen_days[1]
      df$time_h = 8.0
      df$cluster_id = 39
    }
    return(df)
  }
  
  adult_C_none = SubsetDF(adult_C_none, "adult C none", 3.3, choose_cluster_C)
  adult_C = SubsetDF(adult_C, "adult C", 3.15, choose_cluster_C)
  adult_C_full = SubsetDF(adult_C_full, "adult C full", 3.0, choose_cluster_C)
  adult_A_none = SubsetDF(adult_A_none, "adult A none", 2.3, choose_cluster_A)
  adult_A = SubsetDF(adult_A, "adult A", 2.15, choose_cluster_A)
  adult_A_full = SubsetDF(adult_A_full, "adult A full", 2.0, choose_cluster_A)
  juvenile_B_none = SubsetDF(juvenile_B_none, "juvenile B none", 1.3, choose_cluster_B)
  juvenile_B = SubsetDF(juvenile_B, "juvenile B", 1.15, choose_cluster_B)
  juvenile_B_full = SubsetDF(juvenile_B_full, "juvenile B full", 1.0, choose_cluster_B)

  df_for_plot = rbind(adult_C_none, adult_C, adult_C_full, adult_A_none, adult_A, adult_A_full,  # bind all the selected elements altogether
                      juvenile_B_none, juvenile_B, juvenile_B_full)

  # Add perch windows to the graph:
  perch = rbind(cbind(perch_A, bird = "A"), cbind(perch_C, bird = "C"))
  p = ggplot(data=df_for_plot)+geom_point(aes(time_h, height, color=label), size=1)+ylim(0,4)+
  scale_color_manual(values=c('palegreen2', 'mediumseagreen', 'darkgreen', 'pink', 'indianred2', 'darkred', 'lightblue', 'dodgerblue2', 'dodgerblue4' ))+
  geom_rect(data=filter(perch, day %in% chosen_days), aes(xmin=time_h, xmax=time_h+duration / 3600, ymin=0.5, ymax=3.8, fill=bird), alpha=0.2)+ylim(0.5, 3.8)+
    scale_fill_manual(values=c("indianred2", "mediumseagreen"))+facet_wrap(~day)+theme(legend.position="bottom")+xlab("Time (Hours)")+
    ylab("Index")
  return(p)
}


# AdvancedPlotTimestamps(s7s8_B_none, s7s8_BA, s7s8_B_full,
#                        s7s8_A_none, s7s8_A, s7s8_A_full,
#                        s7s8_C_none, s7s8_C, s7s8_C_full, perch_A, perch_C, chosen_days="all")


PlotExpandedTimestamps = function(juvenile_df, adult_df, perch_A, perch_C, chosen_day, juvenile_chosen_clusters=c(1:38), adult_chosen_clusters=c(1:38)){
  # Allows to build counter vs. time plot, BUT only for one day, don't think that it will be helpful to plot across all days, histogram is more helpful
  
  adult_filtered = cbind(filter(adult_df, cluster_id %in% adult_chosen_clusters, day %in% chosen_day), 'label'='adult A')
  juvenile_filtered = cbind(filter(juvenile_df, cluster_id %in% juvenile_chosen_clusters, day %in% chosen_day), 'label'='juvenile B')
  df_for_plot =   rbind(cbind(adult_filtered, 'counter'=1:nrow(adult_filtered)),
                        cbind(juvenile_filtered, 'counter'=1:nrow(juvenile_filtered)))
  # perches:
  perch = rbind(cbind(perch_A, bird = "A"), cbind(perch_C, bird = "C"))
  
  p = ggplot(data=df_for_plot)+geom_point(aes(time_h, counter, color=label))+
    geom_rect(data=filter(perch, day %in% chosen_day), aes(xmin=time_h, xmax=time_h+duration / 3600, ymin=0.0, ymax=nrow(juvenile_filtered), fill=bird), alpha=0.2)+
    scale_fill_manual(values=c("indianred2", "mediumseagreen"))+scale_color_manual(values=c("indianred2", "dodgerblue2"))+facet_wrap(~day)+ylim(0,nrow(juvenile_filtered))
  return(p)
}





PlotHistogramElements = function(bird_none, bird, chosen_days, choose_cluster=c(1:38), which_bird = "juvenile") {
  if (which_bird == "juvenile"){
    color_map = c("lightblue", "dodgerblue2")
  }
  if (which_bird == "adult A"){
    color_map = c('pink', 'indianred2')
  }
  if (which_bird == "adult C"){
    color_map = c('palegreen2', 'mediumseagreen')
  }
  p = ggplot(data=rbind(cbind(filter(bird_none, cluster_id %in% choose_cluster, day %in% chosen_days), type="Alone"), 
                        cbind(filter(bird, cluster_id %in% choose_cluster, day %in% chosen_days), type="In window")))+
    geom_histogram(aes(time_h, fill=type))+geom_histogram(aes(time_h, fill=type), alpha=0.3)+facet_wrap(~day, scales = "free")+
    scale_fill_manual(values=color_map)+xlab("Time (Hours)")+ylab("Number of vocalizations")#+ggtitle("Distribution of vocalizations during the day of juvenile bird for full data and when alone")
  return(p)
}







SummaryPlot = function(juvenile_B_none, juvenile_BA, juvenile_BC, juvenile_B_full, 
                         adult_A_none, adult_A, adult_A_full,
                         adult_C_none, adult_C, adult_C_full, perch_A, perch_C,
                         selected_day, juvenile_clusters=(1:38), adult_clusters=(1:38), 
                         time_limits=c(8, 10.2), window=5, y_axis_stack="index", stack_coloring="clusters",
                         which_adult="A") {
  # Combines all the plots in one summary plot. Plots the elements WITHIN one day (only one day)
  # However we can choose different clusters:)
  #
  # Args:
  #  [juvenile, adult]_[A,B,C]_[none, _, full]: dataframe with corresponding data
  #                                              none: outside any communication window
  #                                              -   : within communictaion window between ONLY ONE BIRD (e.g. A-B)
  #                                              full: data without any selection for any communication window
  #  selected_day: day in format "2018-07-28"
  #  clusters: vector of clusters of interest. defualt: from 1 to 38
  #  time_limits: time limits (in hours) for timestamps. default: from 8 to 10.2 hours
  #  window: length of time window (in sec) within which we plot stack_plot. Default: [-5, 5] sec
  #  y_axis_stack: "index" or "timestamp". Specifies what type of y-axis we should use for stack plot
  #  stack_coloring: "clusters" or "birds". Specifies according to which parameter we should color elements (lines) on stack plot
  #  which adult: "A" or "C". Specifies for which adult we plot all the graphs (except timestamp_plot)
  #
  # Returns:
  #  summary plot for one day (but somehow plot_grid makes shitty background, but gridarrange doesn't allow to combine gridarrange objects)
  
  if (which_adult == "A") {
    adult = adult_A
    juvenile = juvenile_BA
  } else {
    adult = adult_C
    juvenile = juvenile_BC
  }
  selected_day = c(selected_day)  # just to match data type when calling other functions
  stack_plot = CreateStackPlot(juvenile, adult, chosen_days=selected_day, juvenile_chosen_clusters=juvenile_clusters,
                               adult_chosen_clusters=adult_clusters, window_length=window, y_axis=y_axis_stack, coloring=stack_coloring)+
                               theme(legend.key.size=unit(2, "mm"), axis.title.x=element_blank())
  cumulative_plot = CreateCumulativePlot(juvenile, adult, chosen_days=selected_day, juvenile_chosen_clusters=juvenile_clusters,
                                         adult_chosen_clusters=adult_clusters, window_length=window)+
                                         theme(legend.position="bottom", legend.key.size=unit(2, "mm"))
  expanded_plot = PlotExpandedTimestamps(juvenile, adult, perch_A, perch_C, chosen_day=selected_day, 
                                         juvenile_chosen_clusters=juvenile_clusters, adult_chosen_clusters=adult_clusters)+
                                         theme(legend.position = "left", legend.key.size=unit(2, "mm"), 
                                         legend.text=element_text(size=7), axis.title.x=element_blank())+xlim(time_limits)
  timestamp_plot = AdvancedPlotTimestamps(juvenile_B_none, juvenile, juvenile_B_full,  # Note! on timestamp plot only B-A or B-C is plotted!
                                          adult_A_none, adult_A, adult_A_full,
                                          adult_C_none, adult_C, adult_C_full, perch_A, perch_C, 
                                          chosen_days=selected_day, choose_cluster_A=adult_clusters,
                                          choose_cluster_B=juvenile_clusters, choose_cluster_C=adult_clusters)+  # Note: in arguments of SummaryPlot I don't specify clusters for bird C, 
                                          theme(legend.key.size=unit(2, "mm"), legend.position="bottom",        # I use the same clusters as for another adult bird in AdvancedPlotTimestamps
                                          legend.text=element_text(size=7))+xlim(time_limits)
  hist_plot = PlotHistogramElements(juvenile_B_none, juvenile, juvenile_B_full,
                                    chosen_days=selected_day, choose_cluster=juvenile_clusters)+
                                    theme(legend.position = "left", legend.key.size=unit(2, "mm"), 
                                    legend.text=element_text(size=7), axis.title.x=element_blank())+xlim(time_limits)
  
  nested_left = ggarrange(expanded_plot, hist_plot, timestamp_plot, heights=c(5,1,1))
  nested_right = ggarrange(stack_plot, cumulative_plot, heights = c(2,1))
  p = plot_grid(nested_left, nested_right)
  return(p)
}





