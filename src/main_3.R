


######################################################################################################
######################## Calculation of v scores for identification ##################################
######################################################################################################


### Importing and formatting data
# choice of file for fossil data
TAB_identification <- Import_data_v_score("fossil", "data/identification_full_Hadar_2_V2.csv")
# to calculate the number of pollen used in the annaylse compared to the total number of pollen
TAB_identification_proportion <- TAB_identification
# Conversion (or not) of pollen data into relative abundance scale or presence/absence (depends on settings)
TAB_identification <- Convert_pollen_data(TAB_identification, data_type, sample_type="fossil")
# removal of taxa not used in the calculation of the T_value
TAB_identification <- TAB_identification[TAB_identification$used_for_T_value=="yes" ,]
# add a copy of the first site (TAB_identification[7]) to the end of "TAB_identification" because if there is only one site in
# TAB_identification then we end up with a single-column table, which prevents colSums() from working.r 
TAB_identification <-cbind(TAB_identification,TAB_identification[2]) 
# By passing the data in data.frame the "/" become ".". -> to armonise with row name of taxon_as (in Affinity_scores_v_score function)
TAB_identification<-data.frame(check.names = FALSE,TAB_identification[,-c(1)] )

### Construction of matrix of the affinity values (A-values) and saves it in ./output/matix_A_values_identification.csv
A_val_identification <- Affinity_scores_v_score(TAB_identification, T_val, lev_V_groups, sample_type="fossil")

### Construction of the vegetation score matrix by standardisation of the values per row (per sample) of the A_val matrix 
V_val_identification<-t(scale(t(A_val_identification)))

### Formatting the V_val matrix
# Transform into a data frame: easier to use
V_val_identification<-data.frame(check.names = FALSE, V_val_identification)
# deletion of the last line which had been added to avoid problems if only one site is identified
V_val_identification<-V_val_identification[-nrow(V_val_identification),]
# loop for converting habitat score columns into numerical values
# eval(parse(text=a)) to execute the string "A.E-e" as a command
for (i in lev_V_groups){
  a<-paste("A=V_val_identification$`",i,"`<-as.numeric(as.character(V_val_identification$`",i,"`))",sep="")
  eval(parse(text=a))}
# Clearing data that is no longer used
remove(i)
remove(a)
### save V_val in output/matix_V_values_identification.csv
write.table(V_val_identification, "output/matix_V_values_identification.csv", row.names=TRUE, col.names = NA, sep=";",dec=".", na=" ")


V_val_ident_for_graph <- identification_visualisation(lev_V_groups, V_val_identification)

plot_3 <- Graphic_visualisation_plot(lev_V_groups, V_val, V_val_ident_for_graph, representation=1)
ggsave("output/plot_V_scores_identification.png", plot_3, width = 12, height=10, dpi = 300)  




### Information fianl sur les données d'identification utilisé par le plot final
proportion <- matrix(nrow = 4, ncol = length(identification_proportion[,-c(1:6)]))
colnames(proportion)<-colnames(identification_proportion[,-c(1:6)])
row.names(proportion) <- c("%_pollen_gains_used", "%_pollen_types_used" , "n_pollen_gains_used", "n_pollen_types_used")

for (i in colnames(identification_proportion[,-c(1:6)])) {
  sum_total<-sum(identification_proportion[i])
  sum_yes<-sum(identification_proportion[i][identification_proportion[,2]=="yes" ,])
  percent_pollen_grains<-sum_yes*100/sum_total 
  
  col_sum_total<-colSums(identification_proportion[i]!=0)
  col_sum_yes<-sum(identification_proportion[i][identification_proportion[,2]=="yes" ,]!=0)
  percent_pollen_types<-col_sum_yes*100/col_sum_total
  
  proportion["%_pollen_gains_used",i] <- round(percent_pollen_grains,1)
  proportion["%_pollen_types_used",i] <- round(percent_pollen_types,1)
  proportion["n_pollen_gains_used",i] <- round(sum_yes,0)
  proportion["n_pollen_types_used",i] <- round(col_sum_yes,0)  
}

print("")  
print("")  
print("")  
print("statistical analysis of sample use")
t(proportion)





