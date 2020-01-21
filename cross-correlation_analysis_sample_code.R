my_ccf = PlotCrossCorrelationAllDays(s7s8_BA, s7s8_A, 1, 0.05, "indianred2", plot = FALSE)
my_ccf_confidence = my_ccf  # make copy of the same format to later fill with 95 quantile of randomized data
confidence_ccf = GenerateConfidenceUpperBoundary(s7s8_BA, s7s8_A, n_samples = 5, quant = 0.95, window_length = 1, discr_step = 0.05)
my_ccf_confidence$value = confidence_ccf
ggplot(my_ccf)+geom_segment(aes(x = time, xend = time, y = 0, yend = value), color="indianred2")+facet_wrap(~day)+geom_line(data=my_ccf_confidence, aes(time, value))







