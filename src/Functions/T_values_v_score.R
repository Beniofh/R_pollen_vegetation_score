# This function creates a matrix of T-values and saves it in ./output/matix_T_values.csv. 
# This matrix contains the T-value of each pollen type for each vegetation group use.
#
# Arguments:
#   lev_V_groups : a vector V_groups where the names are replaced by numbers, produced by the Groups_order_v_score() function 
#   TAB : the list of pollen counts for all the sites associated with the vegetation groups used, 
#         plus information on the vegetation groups and deposit sites in the last two columns 
#
# Return:
#   The matrix of T-values in the form of a Data Frame
#
T_values_v_score <- function(lev_V_groups, TAB){
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
  write.table(T_val, "output/matix_T_values.csv", row.names=TRUE, col.names = NA, sep=";",dec=".", na=" ")
  # résultats
  return(T_val)}