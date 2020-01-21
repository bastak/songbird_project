# Here I calculate statistics of answers 
# That idea that I had about looking at the statistics: I count N(current_adult | previous_adult, previous_juvenile)
# for each combination of vocalization types:

self_df = s7s8_A_no_jitters
neighbor_df = s7s8_BA_no_jitters
#self_df = filter(s7s8_A_no_jitters, day == "2018-08-05")
#neighbor_df = filter(s7s8_BA_no_jitters, day == "2018-08-05")

self_clusters = sort(c(0, as.numeric(as.character(unique(self_df$cluster_id)))))  # 0 - for silent state
neighbor_clusters = sort(c(0, as.numeric(as.character(unique(neighbor_df$cluster_id)))))

stat_df = data.frame(previous_neighbor = factor(), previous_self = factor(), current_self = factor(), counter = integer())


# Create the statistics table which will be filled later
for (i in self_clusters){
  for (j in self_clusters){
    for (k in neighbor_clusters){
      stat_df = rbind(stat_df, data.frame(previous_neighbor = k,
                                          previous_self = j, current_self = i, counter = 0))
    } 
  }
}


self_memory_window = 0.4
neighbor_memory_window = 0.4

chosen_days = unique(self_df$day)
previous_self_cluster = 0
previous_neighbor_cluster = 0
current_self_cluster = 0
for (chosen_day in chosen_days){
  self_df_within_day = filter(self_df, day == chosen_day)
  neighbor_df_within_day = filter(neighbor_df, day == chosen_day)
  for (index in 1:nrow(self_df_within_day)){  # nrow(self_df_within_day)
    current_self_cluster = self_df_within_day[index, "cluster_id"]
    previous_self_df = filter(self_df_within_day, time_h + duration / 3600 < self_df_within_day[index, "time_h"] & 
                              time_h + duration / 3600 > self_df_within_day[index, "time_h"] - self_memory_window / 3600)
    if (nrow(previous_self_df) > 0){
      previous_self_cluster = previous_self_df[which.max(previous_self_df$time_h), "cluster_id"]  # find cluster of the latest self element within memory window
    } else {
      previous_self_cluster = 0  # if there is no self element within memory window, then previous state was a silent state
    }
   
    previous_neighbor_df = filter(neighbor_df_within_day, time_h + duration / 3600 < self_df_within_day[index, "time_h"] & 
                                time_h + duration / 3600 > self_df_within_day[index, "time_h"] - neighbor_memory_window / 3600)
    if (nrow(previous_neighbor_df) > 0){
      previous_neighbor_cluster = previous_neighbor_df[which.max(previous_neighbor_df$time_h), "cluster_id"]  # find cluster of the latest self element within memory window
    } else {
      previous_neighbor_cluster = 0  # if there is no self element within memory window, then previous state was a silent state
    }
    #print(as.numeric(previous_neighbor_cluster))
    #find the position is stat_df corresponding given clusters and increase counter by 1
    #previous_neighbor_cluster = as.integer(as.character(previous_neighbor_cluster))  # it is the only way to conver factor to integer!! wtf
    #previous_self_cluster = as.integer(as.character(previous_self_cluster))
    #current_self_cluster = as.integer(as.character(current_self_cluster))
    #print(previous_neighbor_cluster)
    #print(previous_self_cluster)
    #print(current_self_cluster)
    stat_index = which(stat_df$previous_neighbor == previous_neighbor_cluster &
                       stat_df$previous_self == previous_self_cluster &
                       stat_df$current_self == current_self_cluster) # index of stat_df row where we have given neighbor and self clusters
    
    #print(stat_index)
    stat_df[stat_index, "counter"] = stat_df[stat_index, "counter"] + 1
    
  } 
  
}

n_c = 34
ggplot(filter(stat_df, previous_neighbor == n_c))+geom_point(aes(x=current_self, y=counter))+geom_segment( aes(x=current_self, xend=current_self, y=0, yend=counter))+
                                                facet_wrap(~previous_self, scales = "free")+scale_x_continuous(breaks = 1:14)+ggtitle(paste("Previous neighbor cluster - ", as.character(n_c)))

ggplot()+geom_histogram(data=s7s8_BA_no_jitters, aes(cluster_id), fill = "dodgerblue2", stat="count")  


CreatePerchStackPlot(s7s8_BA_no_jitters, s7s8_A_no_jitters, s7s8_perch_A, juvenile_chosen_clusters = c(3), adult_chosen_clusters = c(1:38), keep = "both", coloring = "clusters")
