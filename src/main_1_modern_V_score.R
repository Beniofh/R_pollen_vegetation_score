# This script is used to calculate the vegetation scores described in the article for modern samples.

# Input:
#  It uses two csv files (separator: ";") as input. These two files correspond to
#  ->tables S2 of the article (modern pollen counts for each sites) => see path_1 in settings
#  ->tables S2 of the article (inferred vegetation and deposition conditions for the same sites) => see path_2 in settings

# Settings:
#  path of modern pollen counts for each sites
   path_1 <- "data/pollen_counts_table_S2v2.csv"
#  path of inferred vegetation and deposition conditions for the same sites
   path_2 <- "data/vegetation_group_inferred_table_S5v2.csv"
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

# Output:
#  For X1=value of vegetation_set and X2=value of data_type  
#  -> ./output/pollen_counts_percentage_0.002_X1.csv => the pollen counts after (1) conversion to relative abundance, (2) deletion of the Prosoppis and Prosopis_african pollen types, (3) deletion of occurrences less than 0.002.
#  -> ./output/modern_matix_T_values_X1_X2.csv => T-value of each pollen type for each vegetation group use
#  -> ./output/modern_matix_A_values_X1_X2.csv => Affinity value of each pollen type for each vegetation group use.
#  -> ./output/modern_matix_V_values_X1_X2.csv => Vegetation score for each sample for each vegetation group used
#  -> ./output/plot_1_modern_X1_X2.png => A multiplot to display the V-score results for each vegetation group
#  -> ./output/plot_2_modern_X1_X2.png => A graph showing the significant and non-significant values for each vegetation score for each vegetation group (figures 4 and 5 of the article)

   
   
######################################################################################################
################################ pakage loading and functions# #######################################
######################################################################################################


library(ggplot2)

source("src/Functions/Affinity_scores_v_score.R")
source("src/Functions/Convert_pollen_data.R")
source("src/Functions/Graphic_visualisation_multiplot.R")
source("src/Functions/Graphic_visualisation_plot.R")
source("src/Functions/Groups_creation_v_score.R")
source("src/Functions/Groups_order_v_score.R")
source("src/Functions/Import_data_v_score.R")
source("src/Functions/T_values_v_score.R")


######################################################################################################
############################### Importing and formatting data  #######################################
######################################################################################################


# formatting and combining two csv files in a new Data Frame saved in 
# ./output/pollen_counts_percentage_0.002_X1.csv where X1=value of vegetation_set.
TAB <- Import_data_v_score(sample_type="modern", vegetation_set=vegetation_set, path_1=path_1, path_2=path_2)

# Removal of samples excluded from the study
TAB <- TAB[TAB$depositional_env!="removed",]

# Conversion (or not) of pollen data into relative abundance scale or presence/absence (depends on settings)
TAB <- Convert_pollen_data(TAB=TAB, data_type=data_type, sample_type="modern")

# Setting up sets of vegetation groups to be used to calculate the V score (depends on settings)
res_Groups_creation_v_score <- Groups_creation_v_score(vegetation_set=vegetation_set, TAB=TAB)
V_groups <- res_Groups_creation_v_score$V_groups
TAB <- res_Groups_creation_v_score$TAB 

# Definition of the order of appearance of vegetation groups used in figures and tables
res_Groups_order_v_score <- Groups_order_v_score(vegetation_set=vegetation_set, V_groups=V_groups)
lev <- res_Groups_order_v_score$lev
lev_V_groups <- res_Groups_order_v_score$lev_V_groups


######################################################################################################
############################################ T-value  ################################################
######################################################################################################


# Create the T-values matrix and save it in ./output/modern_matix_T_values_X1_X2.csv 
# where X1=value of vegetation_set and X2=value of data_type.
T_val <- T_values_v_score(lev_V_groups=lev_V_groups, TAB=TAB, data_type=data_type, vegetation_set=vegetation_set)


######################################################################################################
####################### Calculation of vegetation scores (V scores) ##################################
######################################################################################################

### Construction of  matrix of the affinity values (A-values) and saves it in ./output/modern_matix_A_values_X1_X2.csv 
# where X1=value of vegetation_set and X2=value of data_type.
A_val <- Affinity_scores_v_score(TAB=TAB, T_val=T_val, lev_V_groups=lev_V_groups, sample_type="modern", data_type=data_type, vegetation_set=vegetation_set)

### Construction of the vegetation score matrix by standardisation of the values per row (per sample) of the A_val matrix 
V_val<-t(scale(t(A_val)))

### Formatting the V_val matrixl
# Agout of the "group" and "lev" column
V_val <- cbind(V_groups, lev, V_val)
# Transform into a data frame: easier to use
V_val<-data.frame(check.names = FALSE, V_val)
# rename the columns because by passing the data in data.frame the "/" become ".".
colnames(V_val)<-c("vegetation_group_inferred","groupe_number",lev_V_groups)
# make a "V_val" from "V_val" by reordering the lines according to the "lev" vector
V_val<-V_val[order(lev,decreasing=F), ]

### save V_val in output/modern_matix_V_values_X1_X2.csv where where X1=value of vegetation_set and X2=value of data_type.
write.table(V_val, paste0("output/modern_matix_V_values_", vegetation_set, "_", data_type, ".csv"), row.names=TRUE, col.names = NA, sep=";",dec=".", na=" ")



######################################################################################################
#######################       Representation graphiques des scores       #############################
######################################################################################################

### representation 1

#http://www.sthda.com/french/wiki/ggplot2-nuage-de-points-guide-de-d-marrage-rapide-logiciel-r-et-visualisation-de-donn-es
#http://sape.inf.usi.ch/quick-reference/ggplot2
library(ggplot2)

# loop for converting habitat score columns into numerical values
# eval(parse(text=a)) to execute the string "A.E-e" as a command
for (i in lev_V_groups){
  a<-paste("V_val$`",i,"`<-as.numeric(as.character(V_val$`",i,"`))",sep="")
  eval(parse(text=a))}
# Clearing data that is no longer used
remove(i)
remove(a)
# order the levels of V_val$vegetation_group_inferred according to lev_V_groups
V_val$vegetation_group_inferred <- factor(V_val$vegetation_group_inferred,factor(lev_V_groups))
# checking the order of groups
levels(V_val$vegetation_group_inferred)
# Creation of multiplot to display the V-score results for each vegetation group
plot_1<-Graphic_visualisation_multiplot(lev_V_groups=lev_V_groups, V_val)
# Saving the multiplot in ./output/plot_1_modern_X1_X2.png
# where X1=value of vegetation_set and X2=value of data_type.
ggsave(paste0("output/plot_1_modern_", vegetation_set, "_", data_type, ".png"), plot_1, width = 12, height=10, dpi = 300)


### representation 2
# Creation of a fake V_val_ident_for_graph_proxy because the Graphic_visualisation_plot() function cannot work without
V_val_ident_for_graph_proxy <- data.frame(labels="A01" ,test = lev_V_groups, score = as.vector(t(V_val[1, -(1:2)])))
#order the levels of A_val$vegetation_group_inferred_proxy according to lev_V_groups
lev_V_groups_fac <-factor(lev_V_groups)
V_val_ident_for_graph_proxy$test <- factor(V_val_ident_for_graph_proxy$test,lev_V_groups_fac)
#verification of group order (selected order)
levels(V_val_ident_for_graph_proxy$test)
# Creation of plot to display the V-score results for each vegetation group
plot_2 <- Graphic_visualisation_plot(lev_V_groups=lev_V_groups, V_val=V_val, V_val_ident_for_graph=V_val_ident_for_graph_proxy, representation=2)
# Saving the plot in ./output/plot_2_modern_X1_X2.png
# where X1=value of vegetation_set and X2=value of data_type.
ggsave(paste0("output/plot_2_modern_", vegetation_set, "_", data_type, ".png"), plot_2, width = 12, height=10, dpi = 300)

