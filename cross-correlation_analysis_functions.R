library(gplots)


KeepFirst = function(bird_df, window){
  # For all sequence of elements which have a delay between each other less than "window" (in sec) this function
  # will keep only first element in sequence (needed for better representation of peristimulus time histogram) to avoid repetitions
  # bird_df: dataframe
  # window:  this is window during which we look ahead in time to see whether there is subsequent element
  element_id_to_delete = c()
  for (i in 1:(nrow(bird_df) - 1)){
    if (bird_df[i+1, "time_h"] - bird_df[i, "time_h"] - bird_df[i, "duration"] / 3600 < window / 3600){
      element_id_to_delete = c(element_id_to_delete, bird_df[i+1, "element_id"])
    } 
  }
  bird_df = filter(bird_df, !(element_id %in% element_id_to_delete))
  return(bird_df)
}

KeepLast = function(bird_df, window){
  # For all sequence of elements which have a delay between each other less than "window" (in sec) this function
  # will keep only last element in sequence (needed for better representation of peristimulus time histogram) to avoid repetitions
  # bird_df: dataframe
  # window:  this is window during which we look ahead in time to see whether there is subsequent element
  element_id_to_delete = c()
  for (i in 1:(nrow(bird_df) - 1)){
    if (bird_df[i+1, "time_h"] - bird_df[i, "time_h"] - bird_df[i, "duration"] / 3600 < window / 3600){
      element_id_to_delete = c(element_id_to_delete, bird_df[i, "element_id"])  # here is the only difference between Keepfirst
    } 
  }
  bird_df = filter(bird_df, !(element_id %in% element_id_to_delete))
  return(bird_df)
}


Binarize = function(x, window_t1, window_t2, dt){
  # Binarizes vector x from window_t1 to window_t2 with discretization step size dt
  t = seq(window_t1, window_t2, dt)
  t = t[-length(t)]  # to be able to feed it to apply function
  BinaryVector = function(t1){
    return(length(which(x >= t1 & x < t1 + dt)))
  }
  binary = sapply(t, BinaryVector)
  return(binary)
}




CrossCorrelationOneDay = function(juvenile_df_last, adult_df_first, chosen_day, window_length, dt, plot = TRUE){
  # Computes cross-correlation for one day for adult aligned to juvenile (like on peristimulus time histogram)
  # Note: you should feed into this function processed dataframes: juvenile with KeepLast (to keep only last juvenile vocalization in a row)
  #                                                                adult with KeepFirst (to keep only first response in a sequence)
  # 
  bird_df = DFForStackPlot(juvenile_df_last, adult_df_first, chosen_days = c(chosen_day), window_length = window_length)
  max_count = max(bird_df$counter)  # this is maximal index of row in stack plot, in some rows there can be no adult element, will need to normalize to this value  

  juvenile_binary = Binarize(c(0), -window_length, window_length, dt)  # Create juvenile binary vector - because we align to it, it is equal to one timestamp at 0
  sum_ccf = rep(0, length(juvenile_binary) + 1)  # don't know why, but in ccf function i get array which is in size on one element more than initial array if i do lag max half of size of array
  
  for (n in 1:max_count){
    if (length(filter(bird_df, counter == n, bird == "adult")$time_h) > 0){ # Change from number to variable!!!
      adult_binary = Binarize(filter(bird_df, counter == n, bird == "adult")$time_h, -window_length, window_length, dt) # and here too!!!
      temp_ccf = ccf(adult_binary, juvenile_binary, lag.max = length(juvenile_binary) %/% 2, plot = FALSE)
      sum_ccf = sum_ccf + temp_ccf$acf
    } 
  }
  
  mean_ccf = sum_ccf / max_count
  mean_ccf = as.vector(mean_ccf)  # because ccf generates vector as a matrix
  if (plot == TRUE){
    x_ccf = seq(-window_length, window_length, dt)
    ggplot()+geom_segment(aes(x = x_ccf, xend = x_ccf, y = 0, yend = mean_ccf))
  } else {
    return(mean_ccf)
  }
}


PlotCrossCorrelationAllDays = function(juvenile_df_last, adult_df_first, window_length, dt, coloring, plot = TRUE){
  all_ccf = data.frame(time = numeric(), value = numeric(), day = character())
  chosen_days = unique(juvenile_df_last$day)
  times = seq(-window_length, window_length, dt)
  for (day in chosen_days){  # Make joint df in order to be able to plot cross-correlations for all days
    #print(day)
    temp_ccf = CrossCorrelationOneDay(juvenile_df_last, adult_df_first, chosen_day = day, window_length = window_length, dt = dt, plot = FALSE)
    all_ccf = rbind(all_ccf, data.frame(time = times, value = temp_ccf, day = day)) 
  }
  if (plot == TRUE){
    # Plot cross-correlations for all days:
    ggplot(all_ccf)+geom_segment(aes(x = time, xend = time, y = 0, yend = value), color="indianred2")+facet_wrap(~day)
  } else {
    return(all_ccf)
  }
}




s7s8_A_first = KeepFirst(filter(s7s8_A, cluster_id %in% c(1:38)), 0.05)
s7s8_BA_last = KeepLast(filter(s7s8_BA, cluster_id %in% c(1:38)), 0.05)

s7s8_C_first = KeepFirst(filter(s7s8_C, cluster_id %in% c(1:38)), 0.2)
s7s8_BC_last = KeepLast(filter(s7s8_BC, cluster_id %in% c(1:38)), 0.2)

juvenile_df_last = filter(s7s8_BA_last, cluster_id %in% c(1:38))
adult_df_first = filter(s7s8_A_first, cluster_id %in% c(1:38))

# should also do the same with C



GenerateConfidenceUpperBoundary = function(juvenile_df, adult_df, n_samples = 500, quant = 0.95, window_length = 1, discr_step = 0.05){
  # Generates "quant" quantile of "n_samples" cross-correlation functions made for circularly shifted data
  # to access statistical significance of real cross-correlation function
  # WARNING: this function is not optimized and takes a loooot of time to run (~4 hours for n_samples = 500)
  # for s7s8 data (10000 obs. for A, 2800 obs for B)
  m_size = as.integer(length(unique(rbind(juvenile_df, adult_df)$day)) * (2 * window_length / discr_step + 1))  # first term is number of days, second - length of cross-correlation vector
  print(m_size)
  samples_ccf = matrix(0, nrow = m_size, ncol = 0)
  for (i in 1:n_samples){
    print(i)
    s7s8_A_shifted = MakeCircularShifts(s7s8_A)
    my_ccf = PlotCrossCorrelationAllDays(s7s8_BA, s7s8_A_shifted, window_length, discr_step, "indianred2", plot = FALSE)
    samples_ccf = cbind(samples_ccf, my_ccf$value)
  }
  confidence_ccf = apply(samples_ccf, 1, function(x) quantile(x, probs = c(quant)))  # make "quant"  quantile of your data
  return(confidence_ccf)
}







# # Make heatmap for cross-correlation functions across days:
# matrix_ccf = matrix(all_ccf$value, nrow = length(unique(all_ccf$day)), byrow = TRUE)
# rownames(matrix_ccf) = unique(all_ccf$day)
# my_palette <- colorRampPalette(c("darkblue", "white", "red"))(n = 119)
# col_breaks = c(seq(-0.02,0,length=10),  
#                seq(0.01,0.07,length=10),              
#                seq(0.075,0.25,length=100))              
# heatmap.2(matrix_ccf, scale = "none", dendrogram = "none", trace = "none", density.info = "none",
#           Rowv = FALSE, Colv = FALSE, col = my_palette, lmat=rbind(c(4,2), c(3, 1)), lhei=c(2, 8), lwid=c(1, 4), margins = c(5,7))



