# Function for converting relative abundance data into relative abundance scale or presence/absence.
#
# Arguments:
#   TAB: the Data Frame to be converted
#   data_type: the type of data to be output  ("1"= relative abundance, "2"=relative abundance scale, "3"=presence/absence)
#   sample_type: to find out whether the data in the data frame to be converted (TAB) has been imported using the 
#                Import_modern_data_v_score function with the "fossil" or "modern" argument in order to adapt the 
#                processing.
#
# Return:
#   The converted data frame
#
Convert_pollen_data <- function(TAB, data_type, sample_type){
  if (sample_type=="modern") {
    col_a=ncol(TAB)-1
    col_b=ncol(TAB)
  } else if (sample_type=="fossil") {
    col_a= 1
    col_b= 1
  } else {
    print("Incorect value for sample_type.")
  } 
  switch (data_type,
          "1" = TAB[, -c(col_a:col_b)][TAB[, -c(col_a:col_b)]>0] <-1,
          "2" = {TAB_bis<-TAB
          TAB_bis[TAB ==0] <-0
          TAB_bis[TAB >0 & TAB <=0.03] <-1
          TAB_bis[TAB >0.03 & TAB <=0.11] <-2
          TAB_bis[TAB >0.11 & TAB <=0.23] <-3
          TAB_bis[TAB >0.23 & TAB <=0.38] <-4
          TAB_bis[TAB >0.38 & TAB <=0.60] <-5
          TAB_bis[TAB >0.60 & TAB <=1.00] <-6
          TAB_bis[,-c(col_a:col_b)]<-round(TAB_bis[,-c(col_a:col_b)],digits=0)  # here the result of the tabl is rounded because on certain taxon, we have: 0.0000000 or 1.000000 instead of 0 or 1
          TAB <- TAB_bis},
          "3" = TAB <- TAB)
  return(TAB)
}