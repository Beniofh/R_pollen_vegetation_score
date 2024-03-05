# Shows the range of V score values for a vegetation group (V_score_vegetation) for each vegetation  group.

# Input:
#  It uses the ./output/modern_matix_V_values_X1_X2.csv files (separator: ";") created by the script main_1.R where
#  X1=value of vegetation_set and X2=value of data_type.

# Settings:
#  sets of vegetation groups to be used to calculate the V score
#  -> 1= the 18 vegetation groups of non-dynamic depositional environments and the 6 vegetation groups of dynamic depositional environments (not used in the article)
#  -> 2= the 18 vegetation groups of non-dynamic depositional environments and the vegetation groups We/Bd/S-river or Wcd/Fb/Wd-river
#  -> 3= only the 6 vegetation groups of dynamic depositional environments
   vegetation_set <- 3
#  the type of pollen data to be used for analysis:
#  -> 1=presence/absence
#  -> 2=relative abundance scale
#  -> 3=relative abundance
   data_type <- 2 
#  the group to be tested with the V score
#  -> indicate the label of one of the vegetation groups in the vegetation sets (e.g., vegetation_set) used
   V_score_vegetation = 'S-river'#'Bd-spring 1'

# Output:
#  There is no output, the figures are displayed directly in R.


######################################################################################################
################################ Pakage loading and functions ########################################
######################################################################################################


library(ggplot2)
source("src/Functions/Detailed_visualisation.R")



######################################################################################################
################################### Detailed_visualisation ###########################################
######################################################################################################


Detailed_visualisation(V_score_vegetation=V_score_vegetation, vegetation_set=vegetation_set, sample_type=sample_type, data_type=data_type)


