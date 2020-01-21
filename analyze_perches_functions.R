library(ggplot2)

'%!in%' <- function(x,y)!('%in%'(x,y))


PlotTimestampsAndWindows = function(bird_df_mapped, perch_A, perch_C, chosen_days="all") {
  # Simple auxillary ode for plotting timestamps of one bird inside the window and outside the window wuth different colors (dark green, light green)
  # and also plots perch windows (red window is for A, green window is for C)
  # 
  # Args:
  #  bird_df_mapped: dataframe of one bird with added column "window" whether it is inside communication window or outside.
  #  Can make this dataframe with MapElementsInsidePerches by feeding usual dataframe (i.e. s7s8_A) and perch data
  #  perch_A: dataframe with perch window for communication with A
  #  perch_C: dataframe with perch window for communication with C
  #  chosen_days: "all" or vector of days in format c("2018-07-28", "2018-08-03") etc. Here we specify the set of days for 
  #                which we want to plot our data. If "all": plots for all days.
  #                By default: "all"
  #
  # Returns: 
  #  Plot of the timestamps and windows of communication

  if (chosen_days == "all") {
    chosen_days = unique(bird_df_mapped$day)
  }

  perch = rbind(cbind(perch_A, bird = "A"), cbind(perch_C, bird = "C"))
  points_color_map = c("darkolivegreen", "olivedrab2")
  names(points_color_map) = c("outside", "inside")
  ggplot()+geom_rect(data=filter(perch, day %in% chosen_days), aes(xmin=time_h, xmax=time_h+duration / 3600, ymin=1, ymax=2, fill=bird), alpha=0.5)+
    scale_fill_manual(values=c("indianred2", "mediumseagreen"))+
    geom_point(data=filter(bird_df_mapped, day %in% chosen_days), aes(time_h, 1.6, color=window), size=1)+
    scale_color_manual(values=points_color_map)+facet_wrap(~day)
    # geom_point(data=filter(s7s8_A_mapped, day %in% chosen_days), aes(time_h, 1.4, color=window), size=1.5)+
    # scale_color_manual(values=c("darkblue", "lightblue"))
} 

#PlotTimestampsAndWindows(r1r2_B_full_A, perch_A, perch_C, chosen_days=c("2018-10-31"))#+scale_x_continuous(breaks = seq(9.77, 9.83, by = 0.01), labels = seq(9.77, 9.83, by = 0.01) * 3600, limits = c(9.77, 9.84))


# Code for plotting perch stack plot BEGIN ----

DFForPerchStackPlot = function(bird_df, perch_df, perch_time_limit, chosen_days = "all", bird_chosen_clusters=c(1:38)){
  # aligns bird_df to the beginnings of the perch windows of dataframe perch_df (and adds counter for later plotting)
  # Logic of the algorithm: (write!) but basically the idea is that column counter creates enumeration from 1 to last element
  # of the perch within given day. All the rest algorithm is paying attention to this enumeration!
  if (chosen_days == "all") {
    chosen_days = unique(bird_df$day)
  }
  
  filtered_bird_df = filter(bird_df, day %in% chosen_days, cluster_id %in% bird_chosen_clusters)
  filtered_perch_df = filter(perch_df, day %in% chosen_days)
  filtered_perch_df[filtered_perch_df$duration > perch_time_limit, "duration"] = perch_time_limit  # to cut off too long perch windows 
  stack_df = cbind(bird_df[1, ], counter = 0)  # seed
  
  for (day_index in 1:length(chosen_days)){
    filtered_bird_within_day = filter(filtered_bird_df, day == chosen_days[day_index])
    filtered_perch_within_day = filter(filtered_perch_df, day == chosen_days[day_index])
    perch_t1 = filtered_perch_within_day$time_h
    perch_t2 = filtered_perch_within_day$time_h + filtered_perch_within_day$duration / 3600
    temp_stack_df = SubsetInWindow(filtered_bird_within_day, perch_t1, perch_t2, perch_t1)
    stack_df = rbind(stack_df, temp_stack_df)
  }
  stack_df = stack_df[-1, ]  # get rid of the seed
  return(stack_df)
}



CreatePerchStackPlot = function(juvenile_df, adult_df, perch_df, perch_window = 10, chosen_days="all", adult_chosen_clusters=c(1:38),
                                juvenile_chosen_clusters=c(1:38), coloring = "clusters", keep="either", color_perch="indianred2"){
  
  # keep: "either" or "both". If "either" - plot keeps only those perch windows which contain at least one juvenile or adult element (or both)
  #                           If "both" - plot keeps windows where there is at least one juvenile AND one adult element!
  # Juvenile is on the UPPER line! Adult is on the bottom
  #
  #
  #
  #
  #
  #
  #
  
  if (chosen_days=="all") {
    chosen_days = unique(juvenile_df$day)
  }
  
  stack_df_adult = DFForPerchStackPlot(adult_df, perch_df, perch_window, chosen_days = chosen_days, bird_chosen_clusters = adult_chosen_clusters)
  stack_df_juvenile = DFForPerchStackPlot(juvenile_df, perch_df, perch_window, chosen_days = chosen_days, bird_chosen_clusters = juvenile_chosen_clusters)
  filtered_perch_df = filter(perch_df, day %in% chosen_days)
  filtered_perch_df[filtered_perch_df$duration > perch_window, "duration"] = perch_window  # cut off too long perches 
  filtered_perch_df$counter <- ave(filtered_perch_df$time_h, filtered_perch_df$day, FUN = seq_along)  # create enumeration (hack to be able to get rid of the empty perches)
  
  for (day_index in 1:length(chosen_days)){  # 
    filtered_perch_within_day = filter(filtered_perch_df, day == chosen_days[day_index])  # just to insert blank element later (if there are no elements inside given day, but still want to plot this day)
    if (keep == "either"){
      nonempty_indices = union(filter(stack_df_adult, day == chosen_days[day_index])$counter,
                               filter(stack_df_juvenile, day == chosen_days[day_index])$counter)   # vector of indices of the perch windows (within day) which contain at least one element
    } 
    if (keep == "juvenile"){
      nonempty_indices = unique(filter(stack_df_juvenile, day == chosen_days[day_index])$counter)   # vector of indices of the perch windows (within day) which contain at least one element
    } 
    if (keep == "adult"){
      nonempty_indices = unique(filter(stack_df_adult, day == chosen_days[day_index])$counter)   # vector of indices of the perch windows (within day) which contain at least one element
    } 
    if (keep == "both"){
      nonempty_indices = intersect(filter(stack_df_adult, day == chosen_days[day_index])$counter,
                                   filter(stack_df_juvenile, day == chosen_days[day_index])$counter)   # vector of indices of the perch windows (within day) which contain at least one element
    }
    
    # nonempty indices say the indices of counter which we want to keep plotting : if "either" - keeps only the counters where at least one element is present
    # we need to delete within given day the rest of elements which do not lie in the set "nonempty_indices":
    filtered_perch_df = filter(filtered_perch_df, !(day == chosen_days[day_index] & !(counter %in% nonempty_indices)))
    stack_df_adult = filter(stack_df_adult, !(day == chosen_days[day_index] & !(counter %in% nonempty_indices)))
    stack_df_juvenile = filter(stack_df_juvenile, !(day == chosen_days[day_index] & !(counter %in% nonempty_indices)))
    
    if (length(nonempty_indices) == 0){  # just if there are none perch windows to plot (i.e. too strict selection, but you still want to plot the subwindow of that day), introduce artifical one window
      filtered_perch_df = rbind(filtered_perch_df, filtered_perch_within_day[1, ])
    }
    
    if (length(nonempty_indices) > 0){   # reset the values for "counter" if there are elements at all
      nonempty_indices = sort(nonempty_indices)
      mapdf = data.frame(old=nonempty_indices, new=1:length(nonempty_indices))  # create the mapping df for mapping old values to the resetted values
      stack_df_adult[stack_df_adult$day == chosen_days[day_index], "counter"] = mapdf$new[match(stack_df_adult[stack_df_adult$day == chosen_days[day_index], "counter"], mapdf$old)]  # stole this hack from stackoverflow
      stack_df_juvenile[stack_df_juvenile$day == chosen_days[day_index], "counter"] = mapdf$new[match(stack_df_juvenile[stack_df_juvenile$day == chosen_days[day_index], "counter"], mapdf$old)]
      filtered_perch_df[filtered_perch_df$day == chosen_days[day_index], "counter"] = mapdf$new[match(filtered_perch_df[filtered_perch_df$day == chosen_days[day_index], "counter"], mapdf$old)]
    }
  }
  
  stack_df_adult$counter = 3 * stack_df_adult$counter - 2  # DFForPerchStackPlot initally gives simple enumeration, but we want also put another bird, so we make spaces 
  stack_df_juvenile$counter = 3 * stack_df_juvenile$counter - 1
  
  if (nrow(stack_df_adult) > 0){  # these 2 conditions are needed if you want to insert column "bird", but df may be empty (due to strict selection) and won't allow cbinding
    stack_df_adult = cbind(stack_df_adult, bird = "adult")
  }
  if (nrow(stack_df_juvenile) > 0){
    stack_df_juvenile = cbind(stack_df_juvenile, bird = "juvenile")
  }
  
  joined_stack_df = rbind(stack_df_adult, stack_df_juvenile)
  filtered_perch_df$time_h = 0  # align onto onset of the perch
  temp1 = filtered_perch_df
  temp2 = filtered_perch_df
  temp1$counter = 3 * temp1$counter - 2
  temp2$counter = 3 * temp2$counter - 1
  joined_perch_df = rbind(temp1, temp2)
  print(filter(joined_stack_df, bird == "juvenile", counter <150))  # DELETE
  if (coloring == "bird"){
    color_map = c(color_perch, "dodgerblue2")
    names(color_map) = c("adult", "juvenile")  # direct mapping is needed in case if you have only juveniles to plot, without direct assignment juveniles elements will be colored in color_perch color
    p = ggplot()+geom_segment(data=joined_stack_df, aes(x=time_h, y=counter, xend=time_h+duration, yend=counter, color=bird), size=1.2)+scale_color_manual(values = color_map)+
      geom_segment(data=joined_perch_df, aes(x=time_h, y=counter, xend=time_h+duration, yend=counter), color=color_perch, size=1.2, alpha=0.2)+
      xlab('Time, sec')+ylab('Index')+facet_wrap(~day, scales = "free")
  } else{
    p = ggplot()+geom_segment(data=joined_perch_df, aes(x=time_h, y=counter, xend=time_h+duration, yend=counter), color = color_perch, size=1.2, alpha=0.2)+
      geom_segment(data=joined_stack_df, aes(x=time_h, y=counter, xend=time_h+duration, yend=counter, color=cluster_id), size=1.2)+
      xlab('Time, sec')+ylab('Index')+facet_wrap(~day, scales = "free")
  }
  return(p)
}

# Code for plotting perch stack plot END ----


 # CreatePerchStackPlot(r1r2_BA, r1r2_A, r1r2_perch_A, chosen_days=c("2018-10-18", "2018-10-19"), coloring="clusters",
 #                      adult_chosen_clusters = c(7), juvenile_chosen_clusters = c(11), keep="both", color_perch = "indianred2", perch_window=20)
 # 

