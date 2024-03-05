# Function to modify the values of the vegetation_group_inferred column in the Data Frame imported using 
# the Import_modern_data_v_score function according to sets of vegetation groups to be used to calculate 
# the V score. This function will also remove sites that are not in sets of vegetation groups to be used
# to calculate the V score. Finally, a vector is created with the values of the vgetation_group_inferred 
# column after modification of the Data Farme.
#
# Arguments:
#   vegetation_set: sets of vegetation groups to be used to calculate the V score
#       -> 1= the 18 vegetation groups of non-dynamic depositional environments and the 6 vegetation groups of dynamic depositional environments
#       -> 2= the 18 vegetation groups of non-dynamic depositional environments and the vegetation groups We/Bd/S-river or Wcd/Fb/Wd-river
#       -> 3= only the 6 vegetation groups of dynamic depositional environments
#   TAB: the Data Frame imported using the Import_modern_data_v_score function
#
# Return:
#   The new data frame
#   A vector with the values of the vegetation_group_inferred column after modification of the Data Farme.
#
Groups_creation_v_score <- function(vegetation_set, TAB){
  switch (vegetation_set,
          "1" = V_groups <-TAB$vegetation_group_inferred,
          "2" = {TAB$vegetation_group_inferred[TAB$depositional_env=="We/Bd/S-river"]="We/Bd/S-river"
          TAB$vegetation_group_inferred[TAB$depositional_env=="Wcd/Fb/Wd-river"]="Wcd/Fb/Wd-river"
          V_groups <-TAB$vegetation_group_inferred}, # because the calculation of T-values is based on the labels of TAB$vegetation_group_inferred, which must be the same as Group -> see: TAB[,second_last_col]
          "3" = {TAB <- TAB[TAB$depositional_env!="no_dynamic",]
          V_groups <-TAB$vegetation_group_inferred})
  return(list(V_groups=V_groups, TAB=TAB))
}
