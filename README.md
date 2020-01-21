How to load dataframes:

First thing that you need to do is to install library dplyr:
type (in R console or in the main window - but in main window you need to select pieces of code you wanna run and press Ctrl+Enter)
```
install.packages("dplyr")
```
then you need to attach it:
```
library(dplyr)
```
Then you need to set the directory of preprocessing.R to be able to call it. Type:
```
setwd("directory_where_preprocessing.R is located")   
```

Then add functions from preprocessing.R:
```
source("preprocessing.R")
```
Then you need to specify the path for your data:
```
path = "C:/cmatlab-Roman/DivPrograms/Individual/RomanDoronin/Data/r1r2/"    (or another directory)
experiment = "r1r2"
```
Then load perch dataframe and preprocess it:
```
r1r2_perch_A = read.csv(paste(path, "perch_A.csv", sep=""))
r1r2_perch_A = PreprocessingPerches(r1r2_perch_A)
```
(you can set whatever variable names that you want)
 
Then load and preprocess the dataframes with vocalizations: (you need to change the end of the name of the file depending on which file you wanna open)
```
r1r2_A = read.csv(paste(path, experiment, "_A.csv", sep=""))
r1r2_A = Preprocessing(r1r2_A, r1r2_perch_A)
```
(note that for the function Preprocessing you need to provide perch data which corresponds to the given dataframe - perch C for the bird C, etc.)

Then you will have preprocessed dataframe, "window" column indicates whether element is inside of any of the perch. t1 and t2 - onsets and offsets are beginning and end timestamps of the corresponding perch for given element. You can also track individual elements by their "element_id" column
Now if you want to know the number of elements in dataframe, type:
```
nrow(r1r2_A)
```
if you wanna check number of only insiders (or only outsiders), type:
```
nrow(filter(r1r2_A, window == "inside"))
```
--------------------------------------------


first_look_12_04:

It contains plotting functions for primary plotting of the data, for preliminary analysis.

Main functions:

CreateStackPlot: creates stack plot - stack of rectangles (lines) where each line is the vocalization, on x axis there is a time. For each element from the first dataframe (first argument in function) I take small time window (its magnitude is determined by parameter window_length) and center all other elements around each element from the first dataframe and plot the resulting stack of lines.

CreateCumulativePlot: creates PeriStimulus Time Histogram (PSTH) where the stimuli are the elements from the first dataframe (as argument in function - it is specified as juvenile, but in fact it can be any bird)

AdvancedTimestamps: Created a plots where each point is vocalization and it plots for all birds for all time, so you see more global structure of the data compared to the stack plot.

------

analyze_perches_functions.R:

PlotTimeStampsAndWindows: just to plot vocalizations as points and perch windows - simply to compare if the points lie in corresponding perch windows

DFForPerchStackPlot: creates DF for PerchStackPlot

CreatePerchStackPlot: plots stack of perch lines (time is on x axis) where each line is in fact double line: on upper line I placed juvenile vocalizations as lines (colored in corresponding vocalization type) and on lower line - adult vocalizations

------

circular_shift_bootstrap_functions.R:

contains functions for creating bird dataframe with circularly shifted timestamps for accessing statistical significance of the cross-correlation functions (or PSTH)

------

clustering_interactions_functions.R:

functions for transforming juvenile and adult dataframes to the dataframe of interaction which we can later cluster
and functions which are needed to assign labels of clusters of interactions to the interaction_df: basically bird_df, but with added column of the label of interaction (several vocalizations have their own identification number)

also contains function CreateInteractionTypePlot: for vizualizing temporal structure of types of interactions (which type of vocalization follows which)

------

clustering_interaction_sample_code.R:

contains piece of code which you need to run in order to: 1) make dataframe of features (which you later cluster), 2) extract labels of types of interactions and assign them to dataframe of elements (so you can select elements from the same interaction type and plot them)

------

general_statistics_functions.R:

calculates general information about data: how many elements were pronounced across days and what was the time spent on perch at each day

------







All functions by default assume that the garbage cluster is either 39 or 40 (in many function default value of juvenile and adult clusters is from 1 to 38 to filter out garbage cluster)
