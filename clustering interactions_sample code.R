

# Main code which connects Python and R:


path_to_save = "your_path"
filename_to_save = "s7s8_A_feature_duration_df.csv"

adult_df = s7s8_A
juvenile_df = s7s8_BA

# Need to create joined_df and seq_indices only to be able to retrieve conversation type labels from python. 
# To encode juvenile_df and adult_df it is enough to run PipelineCreateBirdFeatureDF
joined_df = MakeJoinedDF(juvenile_df, adult_df)
seq_indices =GetSubsequentIndices(joined_df, 0.5)
PipelineCreateBirdFeatureDF(juvenile_df, adult_df, 0.5, 20, path_to_save, filename_to_save, rank = "no")

# Now we need to retrieve labels. For this we create interaction_df: it is usual joined_df, but with several distinctions:
# It consists only from elements participating in interactions ( = from seq_indices), and with added interaction label (counter)




interaction_file_name = "interaction_type_labels_s7s8_BA.csv"
interaction_path = "C:/cmatlab-Roman/DivPrograms/Individual/RomanDoronin/Data/s7s8/feature_dataframes/"
interaction_df = CreateInteractionDF(joined_df, seq_indices)
interaction_df = AddInteractionTypes(interaction_df, interaction_path, interaction_file_name)  # assigns label of type of interaction to each element in df



interaction_cluster = 17
CreateInteractionPerchStackPlot(interaction_df, chosen_interaction_types = c(interaction_cluster))+facet_wrap(~day, scale = "free")#+xlim(0.5, 1)


















library(ggplot2)










# Auxillary code to check the ordered sequence of interspike intervals (and then also intervals between offset and next onset)

juvenile_df = s7s8_BA
adult_df = s7s8_A


joined_df = arrange(rbind(juvenile_df, adult_df), .by_group = list(day, time_h))
isi = diff(joined_df$time_h)
isi = sort(isi[which(isi > 0)]) * 3600

ggplot()+geom_line(aes(1:length(isi), isi))+ylim(0,0.5)

n = nrow(joined_df)
isi_dur = joined_df$time_h[2:n] * 3600 - joined_df$time_h[1:n-1] * 3600 - joined_df$duration[1:n-1]
isi_dur = sort(isi_dur[which(isi_dur > 0)])
ggplot()+geom_line(aes(1:length(isi_dur), isi_dur))+ylim(0,1)
