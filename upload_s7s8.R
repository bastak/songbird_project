# This script loads the data for bird elements and perches and does primary preprocessing
#setwd("C:/cmatlab-Roman/DivPrograms/Individual/RomanDoronin/Scripts/Analysis")
#setwd("~/Downloads/INI_final/RomanDoronin/Data/s7s8")
source("preprocessing.R")

path = "~/Documents/ETH/INI_Hahnloser_final/RomanDoronin/Data/s7s8/"
experiment = "s7s8"

        # Load perches 
s7s8_perch_A = read.csv(paste(path, "perch_A.csv", sep=""))
s7s8_perch_C = read.csv(paste(path, "perch_C.csv", sep=""))

s7s8_perch_A = PreprocessingPerches(s7s8_perch_A)
s7s8_perch_C = PreprocessingPerches(s7s8_perch_C)
s7s8_perch   = rbind(cbind(s7s8_perch_A, bird="A"), cbind(s7s8_perch_C, bird="C"))

       #Upload data for birds A (adult) and B(juvenile) during communicatin windows between them
s7s8_A = read.csv(paste(path, experiment, "_A.csv", sep=""))
s7s8_BA = read.csv(paste(path, experiment, "_BA.csv", sep=""))
s7s8_A = Preprocessing(s7s8_A, s7s8_perch_A)
s7s8_BA = Preprocessing(s7s8_BA, s7s8_perch_A)

       # Now look at the data which is outside of any communication window:
s7s8_A_none = read.csv(paste(path, experiment, "_A_none.csv", sep=""))
s7s8_B_none = read.csv(paste(path, experiment, "_B_none.csv", sep=""))
s7s8_A_none = Preprocessing(s7s8_A_none, s7s8_perch_A)
s7s8_B_none = Preprocessing(s7s8_B_none, s7s8_perch_A)

       # ... and full data:  
s7s8_A_full = read.csv(paste(path, experiment, "_A_full.csv", sep=""))
s7s8_B_full = read.csv(paste(path, experiment, "_B_full.csv", sep=""))
# Also need to map full data to the separate perches (A and C, not just to any perch) - needed for the function PlorElementsNumber 
# for calculating how many elements were pronounced inside and outside of the perches
s7s8_B_full_A = Preprocessing(s7s8_B_full, s7s8_perch_A)
s7s8_B_full_C = Preprocessing(s7s8_B_full, s7s8_perch_C)

s7s8_A_full = Preprocessing(s7s8_A_full, s7s8_perch_A)
s7s8_B_full = Preprocessing(s7s8_B_full, s7s8_perch)

       # And also upload the same for C (another adult) and B communicating with C:
s7s8_C = read.csv(paste(path, experiment, "_C.csv", sep=""))
s7s8_C_none = read.csv(paste(path, experiment, "_C_none.csv", sep=""))
s7s8_C_full = read.csv(paste(path, experiment, "_C_full.csv", sep=""))
s7s8_C = Preprocessing(s7s8_C, s7s8_perch_C)
s7s8_C_none = Preprocessing(s7s8_C_none, s7s8_perch_C)
s7s8_C_full = Preprocessing(s7s8_C_full, s7s8_perch_C)
s7s8_BC = read.csv(paste(path, experiment, "_BC.csv", sep=""))
s7s8_BC = Preprocessing(s7s8_BC, s7s8_perch_C)





