# load extracted data
setwd("~/Documents/ETH/INI_Hahnloser_final/RomanDoronin/Scripts/Analysis/")
source("preprocessing.R")
source("first_look_12_04.R")
source("upload_s7s8.R")

#path_to_pic = 'C:/cmatlab-Roman/DivPrograms/Individual/RomanDoronin/Images/s7s8/'
path_to_pic = '~/Documents/ETH/INI_Hahnloser_final/RomanDoronin/updated_figures/'
source("general_statistics_functions.R")

theme_set(theme_grey(base_size = 15))

# Plot timestamps for sample day 
AdvancedPlotTimestamps(s7s8_B_none, rbind(s7s8_BA, s7s8_BC), s7s8_B_full, s7s8_A_none, s7s8_A, s7s8_A_full, 
                       s7s8_C_none, s7s8_C, s7s8_C_full, s7s8_perch_A, s7s8_perch_C, chosen_days = c("2018-10-18"))+ggtitle("Timestamps for all birds during one day")


# Plot timestamps for all days
AdvancedPlotTimestamps(s7s8_B_none, rbind(s7s8_BA, s7s8_BC), s7s8_B_full, s7s8_A_none, s7s8_A, s7s8_A_full, 
                       s7s8_C_none, s7s8_C, s7s8_C_full, s7s8_perch_A, s7s8_perch_C, chosen_days = "all")+ggtitle("Timestamps for all birds during all days")
ggsave(paste(path_to_pic, "all_timestamps.png", sep=""))
# ----------------------------

# Plot time across the days the juvenile was sitting on the perch
PlotPerchTimes(s7s8_perch_A, s7s8_perch_C, normalize = FALSE)+ggtitle("Time the juvenile was sitting on the perch A and perch C")
ggsave(paste(path_to_pic, "time_on_the_perch.png", sep=""))
# ----------------------------

# Plot number of vocalizations across days for juvenile without garbage (without cluster 39 and 40)
PlotElementsNumber(df1 = s7s8_BA, df_full = s7s8_B_full, df2 = s7s8_BC, type = "juvenile", normalize = FALSE,
                   chosen_clusters = c(1:38))+ggtitle("Number of elements of juvenile across days \n inside and outside communication windows")+ylim(0, 1000)
ggsave(paste(path_to_pic, "juvenile_elements_counts.png", sep=""))
# ----------------------------

# Plot number of vocalizations across days for adult A without garbage (without cluster 39 and 40)
PlotElementsNumber(df1 = s7s8_A, df_full = s7s8_A_full, type = "adult", normalize = FALSE,
                   color_adult = "indianred2", chosen_clusters = c(1:38))+ggtitle("Number of elements of adult A across days inside \n and outside communication windows")+ylim(0, 1000)
ggsave(paste(path_to_pic, "adult_A_elements_counts.png", sep=""))
# ----------------------------

# Plot number of vocalizations across days for adult C without garbage (without cluster 39 and 40)
PlotElementsNumber(df1 = s7s8_C, df_full = s7s8_C_full, type = "adult", normalize = FALSE,
                   color_adult = "mediumseagreen", chosen_clusters = c(1:38))+ggtitle("Number of elements of adult C across days inside \n and outside communication windows")+ylim(0, 1000)
ggsave(paste(path_to_pic, "adult_C_elements_counts.png", sep=""))
# ----------------------------

# perch stack plot for one day, coloring - bird A
CreatePerchStackPlot(s7s8_BA, s7s8_A, s7s8_perch_A, chosen_days = c("2018-08-14"),
                     keep="both", coloring = "bird", juvenile_chosen_clusters = c(1:38),
                     adult_chosen_clusters = c(1:38))+ggtitle("Vocalizations aligned to the beginnings of perch windows")

# perch stack plot for one day, coloring - clusters
CreatePerchStackPlot(s7s8_BA, s7s8_A, s7s8_perch_A, chosen_days = c("2018-10-18"),
                     keep="both", coloring = "clusters", juvenile_chosen_clusters = c(1:38),
                     adult_chosen_clusters = c(1:38))+ggtitle("Vocalizations aligned to the beginnings of perch windows")


# perch stack plot for all days, coloring - bird A, bird A
CreatePerchStackPlot(s7s8_BA, s7s8_A, s7s8_perch_A, chosen_days = "all",
                     keep="both", coloring = "bird", juvenile_chosen_clusters = c(1:38),
                     adult_chosen_clusters = c(1:38))+ggtitle("Vocalizations aligned to the beginnings of perch windows")

# perch stack plot for all days, coloring - bird C, bird C
CreatePerchStackPlot(s7s8_BC, s7s8_C, s7s8_perch_C, chosen_days = "all",
                     keep="both", coloring = "bird", juvenile_chosen_clusters = c(1:38),
                     adult_chosen_clusters = c(1:38), color_perch = "mediumseagreen")+ggtitle("Vocalizations aligned to the beginnings of perch windows")


# perch stack plot for all days, coloring - clusters, bird A
CreatePerchStackPlot(s7s8_BA, s7s8_A, s7s8_perch_A, chosen_days = "all",
                     keep="both", coloring = "clusters", juvenile_chosen_clusters = c(1:38),
                     adult_chosen_clusters = c(1:38))+ggtitle("Vocalizations aligned to the beginnings of perch windows")

# perch stack plot for all days, coloring - clusters, bird C
CreatePerchStackPlot(s7s8_BC, s7s8_C, s7s8_perch_C, chosen_days = "all",
                     keep="both", coloring = "clusters", juvenile_chosen_clusters = c(1:38),
                     adult_chosen_clusters = c(1:38), color_perch = "mediumseagreen")+ggtitle("Vocalizations aligned to the beginnings of perch windows")

#       For bird A:
# ----------------------------
# Create stack plot for one day BA, coloring - bird A (it will serve as an illustration of the Stack Plot)
CreateStackPlot(s7s8_BA, s7s8_A, chosen_days = c("2018-08-02"), coloring = "bird A", window_length = 1)+ggtitle("Stack plot of juvenile and adult A for one day")
ggsave(paste(path_to_pic, "stack_plot_BA_one_day.png", sep=""))

# Create stack plot for one day BA, coloring - clusters
CreateStackPlot(s7s8_BA, s7s8_A, chosen_days = c("2018-08-02"), coloring = "clusters", window_length = 1)+ggtitle("Stack plot of juvenile and adult A for one day")
ggsave(paste(path_to_pic, "stack_plot_BA_one_day_coloring_clusters.png", sep=""))


# Create stack plot for all days BA, coloring - clusters
CreateStackPlot(s7s8_BA, s7s8_A, chosen_days = "all", coloring = "clusters", window_length = 1)+ggtitle("Stack plot of juvenile and adult A for all days")
ggsave(paste(path_to_pic, "stack_plot_BA_all_days_coloring_clusters.png", sep=""))

# ----------------------------



#       For bird C:
# ----------------------------
# Create stack plot for all days, coloring - clusters
CreateStackPlot(s7s8_BC, s7s8_C, chosen_days = "all", coloring = "clusters", juvenile_chosen_clusters = c(1:38))+ggtitle("Stack plot of juvenile and adult C for all days")
ggsave(paste(path_to_pic, "stack_plot_BC_all_days_coloring_clusters.png", sep=""))


# Rates of vocalizations for all days:
# ----------------------------

# Plot rate of vocalizations for juvenile across all days
PlotHistogramElements(s7s8_B_none, rbind(s7s8_BA, s7s8_BC), chosen_days = unique(s7s8_BA$day), which_bird = "juvenile")+ggtitle("Distribution of number of vocalizations of juvenile to any adult across all days")
ggsave(paste(path_to_pic, "vocalization_rate_B.png", sep=""))


# Plot rate of vocalizations for adult A across all days
PlotHistogramElements(s7s8_A_none, s7s8_A, chosen_days = unique(s7s8_A$day), which_bird = "adult A")+ggtitle("Distribution of number of vocalizations of adult A across all days")
ggsave(paste(path_to_pic, "vocalization_rate_A.png", sep=""))


# Plot rate of vocalizations for adult A across all days
PlotHistogramElements(s7s8_C_none, s7s8_C, chosen_days = unique(s7s8_C$day), which_bird = "adult C")+ggtitle("Distribution of number of vocalizations of adult C across all days")
ggsave(paste(path_to_pic, "vocalization_rate_C.png", sep=""))

# -----------------------------






# clustering analysis B-A:
#------------------------------

path_to_save = "C:/cmatlab-Roman/DivPrograms/Individual/RomanDoronin/Data/s7s8/feature_dataframes/"
filename_to_save = "s7s8_A_feature_duration_df.csv"
path_to_pic = 'C:/cmatlab-Roman/DivPrograms/Individual/RomanDoronin/Images/s7s8/clustering interaction units/BA/'

adult_df = s7s8_A
juvenile_df = s7s8_BA
source("clustering interactions_functions.R")

# Need to create joined_df and seq_indices only to be able to retrieve conversation type labels from python. 
# To encode juvenile_df and adult_df it is enough to run PipelineCreateBirdFeatureDF
joined_df = MakeJoinedDF(juvenile_df, adult_df)
seq_indices =GetSubsequentIndices(joined_df, 0.5)
PipelineCreateBirdFeatureDF(juvenile_df, adult_df, 0.5, 20, path_to_save, filename_to_save, rank = "no")

# then after implementing code in python, you should download dataframe of labels with name interaction_file_name:
interaction_file_name = "interaction_type_labels_s7s8_BA.csv"
#interaction_file_name = "s7s8_A"
interaction_path = "~/Downloads/INI_final/RomanDoronin/Data/s7s8/feature_dataframes/"
interaction_df = CreateInteractionDF(joined_df, seq_indices)
interaction_df = AddInteractionTypes(interaction_df, interaction_path, interaction_file_name)  # assigns label of type of interaction to each element in df

#first plot all interaction units (without selecting by cluster):
CreateInteractionPerchStackPlot(interaction_df, chosen_interaction_types = unique(interaction_df$interaction_type))+
  facet_wrap(~day, scale = "free")+ggtitle("all interaction units of juvenile-adult A interaction across all days")
ggsave(paste(path_to_pic, "interaction units all clusters.png", sep=""))

# now let us select different clusters obtain via DBSCAN clustering (I will overwrite each subsequent cluster):
interaction_cluster = 17
CreateInteractionPerchStackPlot(interaction_df, chosen_interaction_types = c(interaction_cluster), chosen_days = "all")+
  ggtitle(paste("Interaction units from cluster", interaction_cluster, ", birds B-A"))
ggsave(paste(path_to_pic, "cluster_", interaction_cluster, ".png", sep=""))

#-----------------------------





# clustering analysis B-C:
#------------------------------

path_to_save = "C:/cmatlab-Roman/DivPrograms/Individual/RomanDoronin/Data/s7s8/feature_dataframes/"
filename_to_save = "s7s8_C_feature_duration_df.csv"
path_to_pic = 'C:/cmatlab-Roman/DivPrograms/Individual/RomanDoronin/Images/s7s8/clustering interaction units/BC/'

adult_df = s7s8_C
juvenile_df = s7s8_BC

# Need to create joined_df and seq_indices only to be able to retrieve conversation type labels from python. 
# To encode juvenile_df and adult_df it is enough to run PipelineCreateBirdFeatureDF
joined_df = MakeJoinedDF(juvenile_df, adult_df)
seq_indices =GetSubsequentIndices(joined_df, 0.5)
PipelineCreateBirdFeatureDF(juvenile_df, adult_df, 0.5, 20, path_to_save, filename_to_save, rank = "no")

# then after implementing code in python, you should download dataframe of labels with name interaction_file_name:
interaction_file_name = "interaction_type_labels_s7s8_BC.csv"
interaction_path = "C:/cmatlab-Roman/DivPrograms/Individual/RomanDoronin/Data/s7s8/feature_dataframes/"
interaction_df = CreateInteractionDF(joined_df, seq_indices)
interaction_df = AddInteractionTypes(interaction_df, interaction_path, interaction_file_name)  # assigns label of type of interaction to each element in df

#first plot all interaction units (without selecting by cluster):
CreateInteractionPerchStackPlot(interaction_df, chosen_interaction_types = unique(interaction_df$interaction_type))+
  facet_wrap(~day, scale = "free")+ggtitle("all interaction units of juvenile-adult C interaction across all days")
ggsave(paste(path_to_pic, "interaction units all clusters.png", sep=""))

# now let us select different clusters obtain via DBSCAN clustering (I will overwrite each subsequent cluster):
interaction_cluster = 7
CreateInteractionPerchStackPlot(interaction_df, chosen_interaction_types = c(interaction_cluster))+
  ggtitle(paste("Interaction units from cluster", interaction_cluster, ", birds B-C"))
ggsave(paste(path_to_pic, "cluster_", interaction_cluster, ".png", sep=""))

#-----------------------------



# Cross-correlation for B-A:
# ----------------------------

my_ccf = PlotCrossCorrelationAllDays(s7s8_BA, s7s8_A, 1, 0.05, "indianred2", plot = FALSE)



























