library(ggplot2)
library(dplyr)

# NOTE: (core idea): Which sequence of elements will be transformed to a feature vector is determined only by list seq_indices - 
# it encodes indices of elements belonging to one vector.


# CODE FOR ENCODING DATAFRAME AS A DATAFRAME OF INTERACTIONS (FEATURE DATAFRAME) BEGIN -------------


PipelineCreateBirdFeatureDF = function(juvenile_df, adult_df, delay, n_instances, path_to_save, filename, rank = "no"){
  # DON'T FORGET TO FILTER OUT GARBAGE CLUSTERS FROM adult_df, juvenile_df BEFORE FEEDING INTO FUNCTION !!!!!!!!!!
  joined_df = MakeJoinedDF(juvenile_df, adult_df)
  seq_indices = GetSubsequentIndices(joined_df, delay)
  
  adult_feature_df = CreateEmptyFeatureDF(adult_df, "adult", seq_indices, n_instances)
  juvenile_feature_df = CreateEmptyFeatureDF(juvenile_df, "juvenile", seq_indices, n_instances)
  
  adult_map = CreateClusterMap(adult_df, n_instances)
  juvenile_map = CreateClusterMap(juvenile_df, n_instances)
  
  feature_df = CreateBirdFeatureDF(joined_df, seq_indices, adult_feature_df, juvenile_feature_df,
                                   adult_map, juvenile_map, n_instances, rank = rank)
  write.csv(feature_df, file = paste(path_to_save, filename), row.names = FALSE)
}


CreateBirdFeatureDF = function(joined_df, seq_indices, empty_adult_feature_df,
                               empty_juvenile_feature_df, adult_map, juvenile_map, n_instances, rank = "no"){
  # Creates feature dataframe from the joined_df - joined dataframe of adult and juvenile interactions
  #
  # 
  # 
  adult_feature_df = empty_adult_feature_df
  juvenile_feature_df = empty_juvenile_feature_df
  
  adult_duration_feature_df = empty_adult_feature_df  # we can delete it if don't want to have to take into account durations
  juvenile_duration_feature_df = empty_juvenile_feature_df
  
  feature_days = c()
  feature_time_h = c()
  feature_duration = c()
  for (i in 1:length(seq_indices)){  
    joined_sample_df = joined_df[seq_indices[[i]], ]
    feature_time_h = c(feature_time_h, joined_sample_df$time_h[1])  # for conversation unit append timestamp of its beginning
    duration = (joined_sample_df$time_h[length(joined_sample_df$time_h)] - joined_sample_df$time_h[1]) * 3600 + joined_sample_df$duration[length(joined_sample_df$duration)]
    feature_duration = c(feature_duration, duration)
    feature_days = c(feature_days, joined_sample_df[1, "day"])
    joined_sample_df$time_h = (joined_sample_df$time_h - joined_sample_df$time_h[1])*3600 + 0.5  # align timings of all elements in a sequence to a onset of a first element                                                                                             # also add some small shift of 0.5 sec to not set to zero first element in a sequence
    
    if (rank == "yes"){  # we can use ranks of timestamps as a features, not the actual timestamps!
      joined_sample_df$time_h = rank(joined_sample_df$time_h)
    }
    
    adult_sample_df = filter(joined_sample_df, bird == "adult")
    juvenile_sample_df = filter(joined_sample_df, bird == "juvenile")
    
    adult_feature_df[i, ] = MakeFeatureVector(adult_sample_df, adult_map, instances = n_instances, add = "time_h")
    juvenile_feature_df[i, ] = MakeFeatureVector(juvenile_sample_df, juvenile_map, instances = n_instances, add = "time_h")
    
    adult_duration_feature_df[i, ] = MakeFeatureVector(adult_sample_df, adult_map, instances = n_instances, add = "duration")  # variable part with durations, can delete it
    juvenile_duration_feature_df[i, ] = MakeFeatureVector(juvenile_sample_df, juvenile_map, instances = n_instances, add = "duration")
  }
  # joined_feature_df = cbind(adult_feature_df, juvenile_feature_df)  
  joined_feature_df = cbind(adult_feature_df, juvenile_feature_df, adult_duration_feature_df, juvenile_duration_feature_df)  
  joined_clean_feature_df = joined_feature_df[, -which(colSums(joined_feature_df[, 1:ncol(joined_feature_df)]) < 0.1)]  # < 0.1 and not 0.0 just in case if there will be some strange things with rounding 
  joined_clean_feature_df = cbind(joined_clean_feature_df, counter = 1:nrow(joined_clean_feature_df),
                                  day = feature_days, time_h = feature_time_h, duration = feature_duration)
  return(joined_clean_feature_df)
}


CreateEmptyFeatureDF = function(bird_df, name_of_column, seq_indices, instances){
  # Creates zero dataframe with feature columns
  # 
  # Args:
  #  bird_clusters: vector of bird clusters. Actually needed only to calculate number of clusters
  #  name_of_column: which name to put in each column
  #  seq_indices: list of vectors of subsequent elements. Needed only to calculate length of list (it will be number of rows because every sequence of elements is one conversation unit)
  #  instances: maximal number of elements of one cluster which we allow to be within sequence (conversation unit)
  # 
  # Returns:
  #  Zero dataframe of features with proper column names which we need to fill out 
  bird_clusters = as.integer(as.character(sort(unique(bird_df$cluster_id)))) 
  print(bird_clusters)
  bird_feature_df = data.frame(matrix(0, ncol = instances*length(bird_clusters), nrow = length(seq_indices)))
  colnames(bird_feature_df) = CreateColumnFeatureNames(bird_clusters, who = name_of_column, number_instances = instances)
  return(bird_feature_df)
}


MakeFeatureVector = function(df, bird_map, instances, add = "time_h"){
  # Returns vector of features constructed from time_h of df
  # 
  # Args:
  #  df: dataframe whose time_h will be mapped to feature vector
  #  REQUIREMENTS for df: should be df from one bird of one "unit" of interaction (or conversation, whatever) (what we consider to be unit which we represent with high-dimensional vector)
  #  bird_map: map from number of cluster for given bird to the position of first occurence of feature column representing this cluster minus one: column_index(([bird]_[cluster number]_1)) - 1
  #  instances: maximal number of occurences of elements of one cluster (of one bird) within unit of interaction
  #  add: "time_h" or "duration" - defines which characteristics of the df will be mapped to the feature vector
  # Returns:
  #  vector of features from df 
  vector = rep(0, instances * nrow(bird_map))  # create zero vector which we will fill with values time_h from df
  if (nrow(df) > 0){
    first_n_df = KeepFirstN(df, n = instances)  # keeps only first n instances of each class
    feature_column_indices = AssignColumnIndices(first_n_df, cluster_map = bird_map)
    vector[feature_column_indices] = first_n_df[, add]
    return(vector)
  } else {
    return(vector)  # return zero vector because df had no data 
  }
}


AssignColumnIndices = function(df, cluster_map){
  # For elements in df returns vector of indices of columns (in feature dataframe) where you should put timestamps of corresponding elements
  #
  # Args:
  #  df : dataframe of elements. Requirements: it should be df of elements of one bird from one perch line, which is SORTED by cluster_id first and by time_h in second)
  #  cluster_map: map dataframe: column x is all cluster_ids of the bird, column y - to which indices it maps to - should be divisible by 10. 
  #  How cluster_map should map: c(4, 7, 13, 45) -> c(0, 10, 20, 30) IF you set the maximum number of instances in the line to be 10. 
  #  So the general rule is that first cluster should map to 0 and to all subsequent cluster you add # of maximum elements in a perch line
  # 
  # Returns:
  #  vector of indices of columns to which elements in df are mapped
  instances_indices = ave(df$time_h, df$cluster_id, FUN = seq_along)
  elements_indices = cluster_map$y[match(df$cluster_id, cluster_map$x)] + instances_indices
  return(elements_indices)
}


# Functions CUtIndex and KeepfirstN: needed for cutting first n elements of each cluster within perch line:
KeepFirstN = function(df, n){
  # Keeps first 10 elements of each cluster in df (this function is needed for transforming perch data for tSNE and we need to take first n elements of each cluster in a perch line)
  # 
  # Args: 
  #  df: dataframe, representing elements within one perch line of one bird
  #  n: number of first elements from each cluster to keep
  #  
  # Returns:
  #  same dataframe, but with first n elements of each cluster
  v = df$cluster_id
  lst_ind = lapply(unique(v), FUN = CutIndex, full_v = v, n = n)
  df_first_n = df[sort(Reduce(c, lst_ind)), ]  # Reduce is for concatenating values from list to one vector of indices to keep
  return(df_first_n)
}


CutIndex = function(element, full_v, n){
  # Returns vector of indices of first n values equal to element in full_v 
  # Example: if full_v = c(1,2,7,1,1,8,1), element = 1, n = 3, it returns c(1, 4, 5)
  ind = which(full_v == element)
  cut_ind = ind[which(c(1:length(ind)) <= n)]
  return(cut_ind)
}


GetSubsequentIndices = function(df, delay){
  # Generates list of indices of df where each element of the list is a vector of indices of subsequent elements from df whose onsets are at most "delay" sec apart
  #
  # Args:
  #  df: dataframe of elements. This function actually uses only time_h column of df
  #  Note: your time_h should be arranged before!!! I don't put sorting inside function because if there are several days or another ordering,
  #  sorting without respect to day or other variable will mix up elements from different days!
  #  delay: maximal time between two subsequent elements which are still considered within sequence
  #
  # Returns:
  #  List of vectors of indices of subsequent elements
  intertime = diff(df$time_h) * 3600  # note that here I calculate time between subsequent ONSETS!! Not between offset and next onset!
  indices = which(intertime < delay & intertime > 0)    # here I keep only indices of intertime intervals which are less than delay (so basically these are the indices of elements in sequence except index of the last element in the sequence)
  breaks = c(0, which(diff(indices) != 1), length(indices))   # stole this code in the internet of how to split vector to vectors of sequenced integers
  seq_indices = sapply(seq(length(breaks) - 1), function(i) indices[(breaks[i] + 1):breaks[i+1]]) 
  seq_indices = sapply(seq_indices, function(x) c(x, x[length(x)]+1))  # to each vector of indices of sequenced elements add the index of last element in the sequence because initially it is lost
  return(seq_indices)
}


CreateColumnFeatureNames = function(clusters, who, number_instances){
  feature_names = c()
  for (cluster in clusters){
    for (i in 1:number_instances){
      name = paste(who, "_", as.character(cluster), "_", as.character(i), sep = "")
      feature_names = c(feature_names, name)
    }
  }
  return(feature_names)
}


CreateClusterMap = function(df, number_instances){
  # Creates map (dataframe) from the names (numbers) of clusters of elements of bird dataframe to the coordinates (minus one)
  # in the feature dataframe corresponding to the first occurence of element of given cluster 
  # Example: if we only have clusters 3, 5, 8 and we allow only 7 occurences of each cluster (within conversation), 
  # then map will be 3 -> 0, 5 -> 7, 8 -> 14
  # NOTE: your df should be ALREADY cleaned from the garbage clusters!!!
  bird_clusters = as.integer(as.character(sort(unique(df$cluster_id))))  # make sorted list of clusters of this dataframe
  bird_map = data.frame(x = bird_clusters, y = as.integer(factor(bird_clusters)) * number_instances - number_instances)
  return(bird_map)
}


MakeJoinedDF = function (juvenile_df, adult_df){
  # Just makes joined dataframe from adult_df and juvenile_df and adds corresponding label
  adult_df = filter(adult_df, cluster_id %in% c(1:38)) # just in case if you anyway forget to filter out,
  juvenile_df = filter(juvenile_df, cluster_id %in% c(1:38))
  joined_df = rbind(cbind(adult_df, bird = "adult"), cbind(juvenile_df, bird = "juvenile"))
  joined_df = arrange(joined_df, day, time_h)  # note that you need to sort time_h WITH RESPECT TO DAy!  Otherwise you mix up together elements from different days
  return(joined_df)
}


# CODE FOR ENCODING DATAFRAME AS A DATAFRAME OF INTERACTIONS (FEATURE DATAFRAME) END -------------

#--------------------------------------------------------------------

# CODE FOR RETRIEVING DATAFRAME OF INTERACTIONS (FEATURE DATAFRAME) BEGIN -------------

# make dataframes of interactions: firstly make joined
CreateInteractionDF = function(joined_df, seq_indices){
  # Creates interaction_df: subsets elements from seq_indices (which participate in interaction)
  # and assigns "counter" values to the elements. Elements belonging to the same counter value participate in the same interaction
  #
  #
  interaction_df = data.frame()
  for (i in 1:length(seq_indices)){
    joined_sample_df = joined_df[seq_indices[[i]], ]
    joined_sample_df$time_h = (joined_sample_df$time_h - joined_sample_df$time_h[1])*3600 + 0.5  # align timings of all elements in a sequence to a onset of a first element
    interaction_df = rbind(interaction_df, cbind(joined_sample_df, counter = i))
  }
  return(interaction_df)
}


AddInteractionTypes = function(df, path, name){
  # Adds label of interaction types for each element (to be able to subset elements based on chosen interaction type)
  #
  #
  interaction_df = df
  cluster_label_map = read.csv(paste(path, name, sep = ""))
  interaction_type_labels = cluster_label_map$interaction_type[match(interaction_df$counter, cluster_label_map$counter)]
  interaction_df$interaction_type = interaction_type_labels
  interaction_df$interaction_timestamp = cluster_label_map$time_h[match(interaction_df$counter, cluster_label_map$counter)]  # add timestamp when this interaction started
  return(interaction_df)
}


CreateInteractionPerchStackPlot = function(interaction_df, chosen_interaction_types = "all",
                                           chosen_days = "all", chosen_interaction_units = "all"){
  # split joined df to adult and juvenile:
  # HERE  I should insert some sorting, because if there are solitary elements as well, df will not be ordered by timestamps. 
  # I need to create new counter based on the order of the timestamps, but not to lose label of the interaction elements, to be able to find them later
  
  if (chosen_interaction_units == "all"){
    chosen_interaction_units = unique(interaction_df$counter)
  }
  
  if (chosen_interaction_types == "all"){
    chosen_interaction_types = unique(interaction_df$interaction_type)
  }
    
  if (chosen_days == "all"){
    chosen_days = unique(interaction_df$day)
  }  
  print(interaction_df)
  selected_interaction_df = filter(interaction_df, day %in% chosen_days, interaction_type %in% chosen_interaction_types,
                                   counter %in% chosen_interaction_units)
  
  # reset counter after selection:
  counter_map = data.frame(x = unique(selected_interaction_df$counter), y = 1:length(unique(selected_interaction_df$counter)))
  reset_counter = counter_map$y[match(selected_interaction_df$counter, counter_map$x)]
  
  selected_interaction_df$counter = reset_counter
  
  adult_interaction_df = filter(selected_interaction_df, bird == "adult")
  juvenile_interaction_df = filter(selected_interaction_df, bird == "juvenile")
  
  adult_interaction_df$counter = 3 * adult_interaction_df$counter - 2  # DFForPerchStackPlot initally gives simple enumeration, but we want also put another bird, so we make spaces 
  juvenile_interaction_df$counter = 3 * juvenile_interaction_df$counter - 1
  #adult_line_df = 3 * adult_interaction_df$counter - 2.6
  #juvenile_line_df = 3  * juvenile_interaction_df$counter - 1.6
  
  joined_interaction_df = rbind(adult_interaction_df, juvenile_interaction_df)
  joined_interaction_df$line_counter = joined_interaction_df$counter - 0.5
  #joined_line_df = rbind(cbind(adult_line_df, bird = "adult"), cbind(juvenile_line_df, bird = "juvenile"))
  
  ggplot()+geom_segment(data=joined_interaction_df, aes(x=time_h, y=counter, xend=time_h+duration, yend=counter, color=cluster_id), size=1.2)+
           geom_segment(data=joined_interaction_df, aes(x = 0.5, y=line_counter, xend = max(joined_interaction_df$time_h + joined_interaction_df$duration), yend=line_counter, linetype=bird))+
    xlab('Time, sec')+ylab('Index')+theme_classic(base_size=15)#+facet_wrap(~day, scales = "free")
}

# CODE FOR RETRIEVING DATAFRAME OF INTERACTIONS (FEATURE DATAFRAME) END -------------



# Function for vizualizing temporal structure of different types of vocalizations:

CreateInteractionTypePlot = function(interaction_df, chosen_days = "all"){
  # this is some code to visualize which type of sequence follows which:
  # the idea is that for each interaction we assign the counter associated with the corresponding type: 
  # counting is done within the type of interaction
  # Then we plot timestamp versus counter of each interaction and the line connects consecutive interactions.
  # The line is needed to not to get lost among a mess of points closely located to each other
  #
  # Args:
  #  interaction_df: df of interaction with assigned labels of interaction (= after clustering in python) via the following pipeline:
  #  CreateInteractionDF -> AddInteractionTypes
  #  chosen_days: vector of days to show
  if (chosen_days == "all"){
    chosen_days = unique(interaction_df$day)
  }
  
  cluster_seq = interaction_df
  cluster_seq$cluster_counter = ave(cluster_seq$interaction_type, list(cluster_seq$day, cluster_seq$interaction_type), FUN = seq_along)
  cluster_seq$interaction_type = as.factor(cluster_seq$interaction_type)
  
  
  ggplot(data = filter(cluster_seq, day %in% chosen_days))+geom_point(aes(x = interaction_timestamp, y = cluster_counter, color = interaction_type))+
    geom_line(aes(x = interaction_timestamp, y = cluster_counter), alpha = 0.3)+
    facet_wrap(~day, scale = "free")+ggtitle("Temporal sequence of conversation classes, clustered in original high-dimensional space")
}

# ---------------------------------------





# 
# delay = 0.4  # maximal delay within which we allow the response
# 
# adult_df = filter(b2p7_A, cluster_id %in% c(1:38)) # DON'T FORGET TO FILTER OUT GARBAGE CLUSTERS !!!!!!!!!!
# juvenile_df = filter(b2p7_BA, cluster_id %in% c(1:38))
# joined_df = rbind(cbind(adult_df, bird = "adult"), cbind(juvenile_df, bird = "juvenile"))
# joined_df = arrange(joined_df, day, time_h)  # note that you need to sort time_h WITH RESPECT TO DAy!  Otherwise you mix up together elements from different days
# 
# seq_indices = GetSubsequentIndices(joined_df, 0.4)
# adult_feature_df = CreateEmptyFeatureDF(adult_df, "adult", seq_indices, 10)
# juvenile_feature_df = CreateEmptyFeatureDF(juvenile_df, "juvenile", seq_indices, 10)
# adult_map = CreateClusterMap(adult_df, 10)
# juvenile_map = CreateClusterMap(juvenile_df, 10)
# 
# new_df = CreateBirdFeatureDF(joined_df, seq_indices, adult_feature_df, juvenile_feature_df, adult_map, juvenile_map, 10)
# 
# 




    






# juvenile_counters = unique(interaction_df$counter[which(interaction_df$cluster_id == 13 & interaction_df$bird == "juvenile")])









# # now try to transform dataframe of interactions to the feature dataframe (META STRUCTURE - TRYING TO FIND PATTERNS OF TYPES OF INTERACTIONS)

# cluster_label_map = read.csv(paste(interaction_path, interaction_file_name, sep = ""))
# colnames(cluster_label_map)[5] = "cluster_id"
# cluster_label_map
# 
# 
# path_to_save = "C:/cmatlab-Roman/DivPrograms/Individual/RomanDoronin/Data/s7s8/feature_dataframes/"
# filename_to_save = "s7s8_A_feature_interaction_df.csv"
# 
# adult_df = cluster_label_map
# juvenile_df = s7s8_BA = cluster_label_map[1, ]
# 
# # Need to create joined_df and seq_indices only to be able to retrieve conversation type labels from python.
# # To encode juvenile_df and adult_df it is enough to run PipelinCreateBirdFeatureDF
# joined_df = MakeJoinedDF(juvenile_df, adult_df)
# seq_indices =GetSubsequentIndices(joined_df, 2)
# PipelineCreateBirdFeatureDF(juvenile_df, adult_df, 2, 20, path_to_save, filename_to_save)
# 
# 
# interaction_file_name = "cluster_labels_meta_interaction.csv"
# interaction_path = "C:/cmatlab-Roman/DivPrograms/Individual/RomanDoronin/Data/s7s8/"
# 
# joined_df = joined_df[, -1]
# 
# interaction_df = CreateInteractionDF(joined_df, seq_indices)
# interaction_df = AddInteractionTypes(interaction_df, interaction_path, interaction_file_name)  # assigns label of type of interaction to each element in df
# interaction_df$cluster_id = as.factor(interaction_df$cluster_id)
# CreateInteractionPerchStackPlot(interaction_df, chosen_interaction_types = c(5))
# 







