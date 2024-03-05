# This function creates a matrix of T-values and saves it in./output/modern_matix_T_values_X1_X2.csv where X2=value of 
# data_type and X1=value of vegetation_set. This matrix contains the T-value of each pollen type for each vegetation group use.
#
# Arguments:
#   lev_V_groups : a vector V_groups where the names are replaced by numbers, produced by the Groups_order_v_score() function 
#   TAB : the list of pollen counts for all the sites associated with the vegetation groups used, 
#         plus information on the vegetation groups and deposit sites in the last two columns
#   data_type: the type of data to be output  ("1"= relative abundance, "2"=relative abundance scale, "3"=presence/absence)
#   vegetation_set: sets of vegetation groups to be used to calculate the V score
#       -> 1= the 18 vegetation groups of non-dynamic depositional environments and the 6 vegetation groups of dynamic depositional environments
#       -> 2= the 18 vegetation groups of non-dynamic depositional environments and the vegetation groups We/Bd/S-river or Wcd/Fb/Wd-river
#       -> 3= only the 6 vegetation groups of dynamic depositional environments
#
# Return:
#   The matrix of T-values in the form of a Data Frame
#
T_values_v_score <- function(lev_V_groups, TAB, data_type, vegetation_set){
  last_col<-length(TAB)  # position of last column in TAB
  second_last_col<-length(TAB)-1  # position of the second last column in TAB
  # recover TAB pollen counts by removing the last 2 columns
  matVT = matrix(0, nrow=length(lev_V_groups), ncol= dim(TAB) [2] -2)
  # rename lines by the name of the vegetation groups
  rownames(matVT) = lev_V_groups
  # rename columns with site names
  colnames(matVT) = colnames(TAB[,-c(second_last_col,last_col)])
  # calculate T-value of each pollen type for each vegetation group use
  for ( aa in 1:length(lev_V_groups)){
    for (bb in 1:(dim(TAB) [2] -2)){
      matVT[aa,bb] = ((mean(TAB[which(TAB[,second_last_col]==lev_V_groups[aa]),bb]))-(mean(TAB[,bb])))/
        (sqrt(((dim(TAB)[1]-dim(TAB[which(TAB[,second_last_col]==lev_V_groups[aa]),])[1])/(dim(TAB)[1]-2))*(var(TAB[,bb]) /dim(TAB[which(TAB[,second_last_col]==lev_V_groups[aa]),])[1])))
    }
  }
  # transposition (easier to code and read)
  T_val<-data.frame(check.names = FALSE,t(matVT))
  # replace Na lines (taxa with an occurrence of 0 on the abundance scale).These taxa may remain despite
  # the deletion of taxa with an occurrence of 0 on the relative concentration scale because a value of
  # 0.001 on the relative concentration scale has a value of 0 on the abundance scale.
  
  T_val[is.na(T_val)]<-0
  # save as .csv
  write.table(T_val, paste0("output/modern_matix_T_values_", vegetation_set, "_", data_type, ".csv"), row.names=TRUE, col.names = NA, sep=";",dec=".", na=" ")
  # résultats
  return(T_val)}