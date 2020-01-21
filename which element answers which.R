# This is what I wanted to do (this Markov stuff - calculation of the number of answers of each cluster to each cluster)
# First we need to filter out jittering
library(plotly)
CreatePerchStackPlot(s7s8_BA, s7s8_A, s7s8_perch_A, keep = "both", chosen_days = c("2018-08-06"), coloring = "clusters")


# Need to create function for filtering out jittering


DeleteJitters = function(juvenile_df, adult_df, which_return){
  # Deletes elements from the juvenile and adult dataframes which are jittering (overlapping with each other) and
  # returns filtered dataframe (adult or juvenile, depending on which_return) without elements participating in jittering
  # Be careful to filter out garbage clusters first!!! (before feeding dfs as arguments)! Otherwise it
  #  will also delete elements jittering with garbage clusters. You don't want this!
  # IMPORTANT: Here I delete single jittering elements, if there is jittering sequence, I delete only elements in 
  # sequence which really overlap, i don't take into account that probably I should delete whole sequence!!
  juvenile_jitters = c()
  adult_jitters = c()
  chosen_days = unique(juvenile_df$day)
  for (chosen_day in chosen_days){
    print(chosen_day)
    juvenile_df_within_day = filter(juvenile_df, day == chosen_day)
    adult_df_within_day = filter(adult_df, day == chosen_day)
    if (nrow(adult_df_within_day) > 0){  # if there will be no adult elements within this day, without this condition function will throw an error
      for (i in 1:nrow(adult_df_within_day)){
        t1_adult = adult_df_within_day[i, "time_h"]  # just renaming for more understandable code
        t2_adult = adult_df_within_day[i, "time_h"] + adult_df_within_day[i, "duration"] / 3600
        juvenile_df_overlapped = filter(juvenile_df_within_day,
                                        !(time_h + duration / 3600 < t1_adult | time_h > t2_adult))  # subsets all juvenile elements which are overlapping with the given adult element i
        if (nrow(juvenile_df_overlapped) > 0){
          juvenile_jitters = c(juvenile_jitters, juvenile_df_overlapped$element_id)
          adult_jitters = c(adult_jitters, adult_df_within_day[i, "element_id"])
        }
      }
      print(length(juvenile_jitters))
    }
  }
  if (which_return == "adult"){
    return(filter(adult_df, !(element_id %in% adult_jitters))) # Note!! You can delete "!" and it will return only jitter elements! 
  } else {                                                     # This can be superhelpful in analysing how birds jitter to each other!
    return(filter(juvenile_df, !(element_id %in% juvenile_jitters)))
  }
}


s7s8_BA_ng = filter(s7s8_BA, cluster_id %in% c(1:38)) # To get rid of garbage clusters (to feed it to DeleteJitters because it requires purified df)
s7s8_A_ng = filter(s7s8_A, cluster_id %in% c(1:38))
s7s8_C_ng = filter(s7s8_C, cluster_id %in% c(1:38))
s7s8_BC_ng = filter(s7s8_BC, cluster_id %in% c(1:38))



s7s8_BA_no_jitters = DeleteJitters(s7s8_BA_ng, s7s8_A_ng, which_return = "juvenile")
s7s8_A_no_jitters = DeleteJitters(s7s8_BA_ng, s7s8_A_ng, which_return = "adult")

s7s8_BC_no_jitters = DeleteJitters(s7s8_BC_ng, s7s8_C_ng, which_return = "juvenile")
s7s8_C_no_jitters = DeleteJitters(s7s8_BC_ng, s7s8_C_ng, which_return = "adult")


CreatePerchStackPlot(s7s8_BA_no_jitters, 
                     s7s8_A_no_jitters, s7s8_perch_A, keep = "either", chosen_days = c("2018-08-06"), coloring = "clusters")

# Plot fractions of jittering elements in different combinations (B-A, B-C, A-B, C-B):
PlotElementsNumber(df1 = s7s8_BA_no_jitters, df_full = s7s8_BA_ng, type = "adult", normalize = TRUE, color_adult = "dodgerblue2")+
  ggtitle("Fraction of juvenile non-ittering elements across days during communication with adult A")

PlotElementsNumber(df1 = s7s8_BC_no_jitters, df_full = s7s8_BC_ng, type = "adult", normalize = TRUE, color_adult = "dodgerblue2")+
  ggtitle("Fraction of juvenile non-ittering elements across days during communication with adult C")


PlotElementsNumber(df1 = s7s8_A_no_jitters, df_full = s7s8_A_ng, type = "adult", normalize = TRUE, color_adult = "indianred2")+
  ggtitle("Fraction of adult A non-jittering elements across days during communication with juvenile")

PlotElementsNumber(df1 = s7s8_C_no_jitters, df_full = s7s8_C_ng, type = "adult", normalize = TRUE, color_adult = "mediumseagreen")+
  ggtitle("Fraction of adult C non-jittering elements across days during communication with juvenile")



# -----------------------
















