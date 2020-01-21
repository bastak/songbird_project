# This will select only elements which were pronounced only during the corresponding communication window.
# For some reason filtering by matlab script (via extract_mydata_Roman.m by choosing effector_pulse_screen) is not enough 
# and among extracted elements some still lie outside communication window so I need manually discard those elements and return purified data

setwd('C:/cmatlab-Roman/DivPrograms/Individual/RomanDoronin/Scripts/Analysis')  
library(ggplot2)
library(spatstat)
library(dplyr)
library(plotly)
library(egg)
library(cowplot)
source("first_look_17_03.R")





#SelectElementsInPerches = function()

perch = perch_A