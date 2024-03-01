# Function to modify the values of the vegetation_group_inferred column in the Data Frame imported using 
# the Import_modern_data_v_score function according to sets of vegetation groups to be used to calculate 
# the V score. This function will also remove sites that are not in sets of vegetation groups to be used
# to calculate the V score. Finally, a vector is created with the values of the getation_group_inferred 
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
  return(list(V_groups=V_groups, TAB=TAB))}




# This function will replace the names of the vegetation groups in the V_groups vector with numerical values 
# which will define the order in which these groups appear in tables and graphs. This function will also create
# a second vector with the name of each vegetation group used, sorted according to the order in which they 
# should appear in tables and graphs.
#
# Arguments:
#   vegetation_set: sets of vegetation groups to be used to calculate the V score
#       -> 1= the 18 vegetation groups of non-dynamic depositional environments and the 6 vegetation groups of dynamic depositional environments
#       -> 2= the 18 vegetation groups of non-dynamic depositional environments and the vegetation groups We/Bd/S-river or Wcd/Fb/Wd-river
#       -> 3= only the 6 vegetation groups of dynamic depositional environments
#   V_groups: the vector with the name of vegetation group inferred for each sample produced by the Groups_creation_v_score() function
#
# Return:
#   lev_V_groups: a vector V_groups where names are replaced by numbers
#   lev: a vector with the names of each vegetation used, where the names are sorted according to the number assigned to them when the lev_V_groups vector was created
#
Groups_order_v_score <- function(vegetation_set, V_groups){
  lev <- V_groups
  switch (vegetation_set,
          "1" = {lev[lev =="Fb/Wd-low/middle"] <-7
          lev[lev =="Fb/Wd-upper"] <-6
          lev[lev =="Bd-spring 2"] <-15
          lev[lev =="Bd-rtf"] <-10
          lev[lev =="Bd-wb 1"] <-13
          lev[lev =="Bd-wb 2"] <-12
          lev[lev =="Bd-spring 1"] <-14
          lev[lev =="A.E-e"] <-1
          lev[lev =="Fa-sc 1"] <-5
          lev[lev =="Bd-wgc"] <-9
          lev[lev =="Fa-sc 2"] <-4
          lev[lev =="Fa-upper"] <-3
          lev[lev =="Fa-w"] <-2
          lev[lev =="Bd-wb 3"] <-11
          lev[lev =="Wcd"] <-8
          lev[lev =="S-xs"] <-16
          lev[lev =="S-xds"] <-17
          lev[lev =="D"] <-18
          lev[lev =="Bd-river"] <-23
          lev[lev =="We-river"] <-22
          lev[lev =="S-river"] <-24
          lev[lev =="Wcd-river 1"] <-20
          lev[lev =="Wcd-river 2"] <-19
          lev[lev =="Fb/Wd-river"] <-21},
          "2" = {lev[lev =="Fb/Wd-low/middle"] <-7
          lev[lev =="Fb/Wd-upper"] <-6
          lev[lev =="Bd-spring 2"] <-15
          lev[lev =="Bd-rtf"] <-10
          lev[lev =="Bd-wb 1"] <-13
          lev[lev =="Bd-wb 2"] <-12
          lev[lev =="Bd-spring 1"] <-14
          lev[lev =="A.E-e"] <-1
          lev[lev =="Fa-sc 1"] <-5
          lev[lev =="Bd-wgc"] <-9
          lev[lev =="Fa-sc 2"] <-4
          lev[lev =="Fa-upper"] <-3
          lev[lev =="Fa-w"] <-2
          lev[lev =="Bd-wb 3"] <-11
          lev[lev =="Wcd"] <-8
          lev[lev =="S-xs"] <-16
          lev[lev =="S-xds"] <-17
          lev[lev =="D"] <-18
          lev[lev =="We/Bd/S-river"] <-20
          lev[lev =="Wcd/Fb/Wd-river"] <-19},
          "3" = {lev[lev =="Bd-river"] <-5
          lev[lev =="We-river"] <-4
          lev[lev =="S-river"] <-6
          lev[lev =="Wcd-river 1"] <-2
          lev[lev =="Wcd-river 2"] <-1
          lev[lev =="Fb/Wd-river"] <-3})
  # transforming lev into a numerical vector
  lev<-as.numeric(lev)
  # order groups according to the group number vector
  V_groups_reorder= reorder(V_groups,lev)
  lev_V_groups = levels(V_groups_reorder)
  # results
  return(list(lev_V_groups=lev_V_groups, lev=lev))}