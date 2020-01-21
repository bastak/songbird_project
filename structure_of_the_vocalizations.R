library(ggplot2)
library(dplyr)
source("upload_s7s8.R")



bird_df = s7s8_A

bird_df = bird_df[order(bird_df$day, bird_df$time_h), ]
rownames(bird_df) = seq(length=nrow(bird_df))  # reset indices 

my_stack = bird_df

DfSubsequentElements = function(bird_df, delta_t){
  # Subsets the elements which only have the following element closer than delta_t away and appends characteristics of the following 
  # element to additional columns
  #
  # Args:
  #  bird_df: dataframe of the bird
  #  delta_t: allowed interelement time in seconds
  #  
  # Returns:
  #  Dataframe with additional columns: 
  #   cluster_next: cluster of the subsequent element
  #   time_h_next: timestamp of the subsequent element
  #   duration_next: duration of the subsequent element
  #   interval: interelement interval 
  
  delta_t = delta_t / 3600
  indices_first = which(bird_df[2:nrow(bird_df), "time_h"] - bird_df[1:nrow(bird_df)-1, "time_h"]
                  - bird_df[1:nrow(bird_df)-1, "duration"] / 3600 < delta_t & 
                    bird_df[2:nrow(bird_df), "time_h"] - bird_df[1:nrow(bird_df)-1, "time_h"] 
                  - bird_df[1:nrow(bird_df)-1, "duration"] / 3600 > 0 )
  
  indices_second = indices_first + 1
  
  bird_df_sequences = bird_df[indices_first, ]
  bird_df_sequences$cluster_next = bird_df[indices_second, "cluster_id"]
  bird_df_sequences$time_h_next = bird_df[indices_second, "time_h"]
  bird_df_sequences$duration_next = bird_df[indices_second, "duration"]
  bird_df_sequences$interval = bird_df_sequences$time_h_next * 3600  - bird_df_sequences$time_h * 3600 - bird_df_sequences$duration
  return(bird_df_sequences)
}

r1r2_A_subsequent = DfSubsequentElements(r1r2_C, 0.06)



ggplot()+geom_histogram(data=r1r2_A_subsequent[!r1r2_A_subsequent$cluster_id %in% c(0), ], aes(cluster_next), stat = "count", fill="indianred2")+
  facet_wrap(~cluster_id, scales = "free")+ggtitle("Which clusters follow which less after 0.05 s for s7s8 A")


ggplot()+geom_histogram(data=r1r2_A_subsequent[!s7s8_A_subsequent$cluster_id %in% c(0), ], aes(interval, fill=cluster_next))+
  facet_wrap(~cluster_id, scales = "free")+ggtitle("Distribution of intervocalization times with respect to clusters of first and subsequent sounds")

































