# This function creates a matrix of the affinity values (A-values) and saves it in ./output/matix_A_values.csv. 
# This matrix contains affinity value of each pollen type for each vegetation group use.
#
# Arguments:
#   TAB : the list of pollen counts for all the sites associated with the vegetation groups used, 
#         plus information on the vegetation groups and deposit sites in the last two columns 
#         last_col: position of last column in TAB
#   lev_V_groups : a vector V_groups where the names are replaced by numbers, produced by the Groups_order_v_score() function 
#   T_val: the matrix of T-values in the form of a Data Frame
#   sample_type: to find out whether the data in the data frame to be converted (TAB) has been imported using the 
#                Import_modern_data_v_score function with the "fossil" or "modern" argument in order to adapt the 
#                processing.
#
# Return:
#   The matrix of A-values in the form of a Data Frame
#
Affinity_scores_v_score <- function(TAB, T_val, lev_V_groups, sample_type){
  
  if (sample_type=="modern") {
    col_a=ncol(TAB)-1
    col_b=ncol(TAB)
    #Transposition (plus simple a coder et a lire)
    taxon_as<-data.frame(check.names = FALSE, t(TAB[,-c(col_a:col_b)]))
  } else if (sample_type=="fossil") {
    taxon_as<-data.frame(check.names = FALSE, TAB)
  } else {
    print("Incorect value for sample_type.")
  } 
  
  #
  taxon_as <- taxon_as[order(rownames(taxon_as)), ]
  T_val <- T_val[order(rownames(T_val)), ]
  
  for (i in lev_V_groups){
    print(i)
    
    ### Calculation of the score for positive T-value: first part of the subtraction in the score equation to measure the affinity (A) in the article
    #T_val_max -> maximum T-value in positive
    T_val_max<-max(T_val[i][T_val[i]>0,])
    #ppxx -> the value of T-value equal to xx% of the maximum positive value (T_val_max)
    pp10<-10*T_val_max/100
    pp20<-20*T_val_max/100
    pp30<-30*T_val_max/100
    pp40<-40*T_val_max/100
    pp50<-50*T_val_max/100
    pp60<-60*T_val_max/100
    pp70<-70*T_val_max/100
    pp80<-80*T_val_max/100
    pp90<-90*T_val_max/100
    #T_val_ppXX -> taxon with a T-value between XX% and XX+10% of the maximum positive value (T_val_max)
    T_val_pp00<-taxon_as[T_val[i]>=0 & T_val[i]<pp10 ,]
    T_val_pp10<-taxon_as[T_val[i]>=pp10 & T_val[i]<pp20 ,]
    T_val_pp20<-taxon_as[T_val[i]>=pp20 & T_val[i]<pp30 ,]
    T_val_pp30<-taxon_as[T_val[i]>=pp30 & T_val[i]<pp40 ,]
    T_val_pp40<-taxon_as[T_val[i]>=pp40 & T_val[i]<pp50 ,]
    T_val_pp50<-taxon_as[T_val[i]>=pp50 & T_val[i]<pp60 ,]
    T_val_pp60<-taxon_as[T_val[i]>=pp60 & T_val[i]<pp70 ,]
    T_val_pp70<-taxon_as[T_val[i]>=pp70 & T_val[i]<pp80 ,]
    T_val_pp80<-taxon_as[T_val[i]>=pp80 & T_val[i]<pp90 ,]
    T_val_pp90<-taxon_as[T_val[i]>=pp90 ,]
    #cof_xp-> Coefficient for taxa with a T-value between x% and x+10% of the maximum positive value (T_val_max)
    cof_0p<-0
    cof_1p<-1
    cof_2p<-2
    cof_3p<-3
    cof_4p<-4
    cof_5p<-5
    cof_6p<-6
    cof_7p<-7
    cof_8p<-8
    cof_9p<-9
    #score_p-> score for positive T-value: first part of the subtraction in the score equation to measure the affinity (A) in the article
    score_p<-(colSums(T_val_pp00)*cof_0p+ colSums(T_val_pp10)*cof_1p + colSums(T_val_pp20)*cof_2p + colSums(T_val_pp30)*cof_3p + colSums(T_val_pp40)*cof_4p + colSums(T_val_pp50)*cof_5p + colSums(T_val_pp60)*cof_6p + colSums(T_val_pp70)*cof_7p + colSums(T_val_pp80)*cof_8p + colSums(T_val_pp90)*cof_9p) / (nrow(T_val_pp00)*cof_0p+nrow(T_val_pp10)*cof_1p+nrow(T_val_pp20)*cof_2p+nrow(T_val_pp30)*cof_3p+nrow(T_val_pp40)*cof_4p+nrow(T_val_pp50)*cof_5p+nrow(T_val_pp60)*cof_6p+nrow(T_val_pp70)*cof_7p+nrow(T_val_pp80)*cof_8p+nrow(T_val_pp90)*cof_9p)
    
    ### Calculation of the score for negative T-value: second part of the subtraction in the score equation to measure the affinity (A) in the article
    #T_val_min -> minimal T-value in negative
    T_val_min<-min(T_val[i][T_val[i]<0,])
    #pnxx -> the value of T-value equal to xx% of the minimum negative value (T_val_min)
    pn10<-10*T_val_min/100
    pn20<-20*T_val_min/100
    pn30<-30*T_val_min/100
    pn40<-40*T_val_min/100
    pn50<-50*T_val_min/100
    pn60<-60*T_val_min/100
    pn70<-70*T_val_min/100
    pn80<-80*T_val_min/100
    pn90<-90*T_val_min/100
    #T_val_pnXX -> taxon with a T-value between XX% and XX+10% of the minimum negative value (T_val_max)
    T_val_pn00<-taxon_as[T_val[i]<=0 & T_val[i]>pn10 ,]
    T_val_pn10<-taxon_as[T_val[i]<=pn10 & T_val[i]>pn20 ,]
    T_val_pn20<-taxon_as[T_val[i]<=pn20 & T_val[i]>pn30 ,]
    T_val_pn30<-taxon_as[T_val[i]<=pn30 & T_val[i]>pn40 ,]
    T_val_pn40<-taxon_as[T_val[i]<=pn40 & T_val[i]>pn50 ,]
    T_val_pn50<-taxon_as[T_val[i]<=pn50 & T_val[i]>pn60 ,]
    T_val_pn60<-taxon_as[T_val[i]<=pn60 & T_val[i]>pn70 ,]
    T_val_pn70<-taxon_as[T_val[i]<=pn70 & T_val[i]>pn80 ,]
    T_val_pn80<-taxon_as[T_val[i]<=pn80 & T_val[i]>pn90 ,]
    T_val_pn90<-taxon_as[T_val[i]<=pn90 ,]
    #cof_xn-> Coefficient for taxa with a T-value between x% and x+10% of the minimum negative value (T_val_max)
    cof_0n<-0
    cof_1n<-1
    cof_2n<-2
    cof_3n<-3
    cof_4n<-4
    cof_5n<-5
    cof_6n<-6
    cof_7n<-7
    cof_8n<-8
    cof_9n<-9
    #score_n-> score for negative T-value: second part of the subtraction in the score equation to measure the affinity (A) in the article
    score_n<-(colSums(T_val_pn00)*cof_0n+ colSums(T_val_pn10)*cof_1n + colSums(T_val_pn20)*cof_2n + colSums(T_val_pn30)*cof_3n + colSums(T_val_pn40)*cof_4n + colSums(T_val_pn50)*cof_5n + colSums(T_val_pn60)*cof_6n + colSums(T_val_pn70)*cof_7n + colSums(T_val_pn80)*cof_8n + colSums(T_val_pn90)*cof_9n) / (nrow(T_val_pn00)*cof_0n+nrow(T_val_pn10)*cof_1n+nrow(T_val_pn20)*cof_2n+nrow(T_val_pn30)*cof_3n+nrow(T_val_pn40)*cof_4n+nrow(T_val_pn50)*cof_5n+nrow(T_val_pn60)*cof_6n+nrow(T_val_pn70)*cof_7n+nrow(T_val_pn80)*cof_8n+nrow(T_val_pn90)*cof_9n)
    
    ### final calculation of the affinity score
    affinity_score<-score_p-score_n
    
    ### addition to the affinity score matrix
    if(i==lev_V_groups[1]){A_val <-affinity_score} else {A_val <- cbind(A_val ,affinity_score)}
  }
  # Remettre les bon noms de collonnes 
  colnames(A_val)<-lev_V_groups
  # save as .csv
  if (sample_type=="modern") {
    write.table(A_val, "output/matix_A_values.csv", row.names=TRUE, col.names = NA, sep=";",dec=".", na=" ")
  } else if (sample_type=="fossil") {
    write.table(A_val, "output/matix_A_values_identification.csv", row.names=TRUE, col.names = NA, sep=";",dec=".", na=" ")
  }
  # results
  return(A_val)
} 
  