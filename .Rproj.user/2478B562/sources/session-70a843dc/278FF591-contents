# Importation et transformation 
######################################################################################################################

Import_data_v_score <- function(path_1, path_2){

### Importation des donnees
pollen_counts <- read.csv(path_1, stringsAsFactors = FALSE, sep = ";", row.names = 8)
pollen_counts <- pollen_counts[ , -c(1:7)]
pollen_counts[is.na(pollen_counts)] <- 0
vegetation_group_inferred <- read.csv(path_2, stringsAsFactors = FALSE, sep = ";", row.names = 1)
vegetation_group_inferred <- vegetation_group_inferred[, "vegetation_group_inferred", drop = FALSE]

### Création du df pour les groupes de végétation et les environements de dépôts
vegetation_group_inferred <- cbind(vegetation_group_inferred, depositional_env = rep("no_dynamic", nrow(vegetation_group_inferred)))
vegetation_group_inferred$depositional_env[vegetation_group_inferred$vegetation_group_inferred %in% c("We-river","Bd-river", "S-river")]="We/Bd/S-river"
vegetation_group_inferred$depositional_env[vegetation_group_inferred$vegetation_group_inferred %in% c("Wcd-river 1", "Wcd-river 2", "Fb/Wd-river")]="Wcd/Fb/Wd-river"
vegetation_group_inferred$depositional_env[vegetation_group_inferred$vegetation_group_inferred=="removed"]="removed"

### Mettre les comptages polliniques en %
pollen_counts_percentage <- prop.table(data.matrix(pollen_counts),2) # 1 -> % par ligne ; 2 -> % par collone 
pollen_counts_percentage <- as.data.frame(pollen_counts_percentage) # prop.table donne une matrix que l'onn retransforme ici en data.farme (plus simple à manipuler)

### Traitement complementaire de taxon (après caclcule des % pour ne pas impacter les %)
# retirer Prosoppis et Prosopis_africana
pollen_counts_percentage <- pollen_counts_percentage[!(rownames(pollen_counts_percentage) %in% c("Prosopis_africana", "Prosopis")), ] 
# retrait des lignes sans pollens (somme des pollen de la ligne != 0)
n_col <- ncol(pollen_counts_percentage)
pollen_counts_percentage <-pollen_counts_percentage[rowSums(pollen_counts_percentage[,c(1:n_col)])!=0,] 

### Retrait des taxons avec faible % (< 0.002)
pollen_counts_percentage_0.002 <-pollen_counts_percentage
pollen_counts_percentage_0.002[pollen_counts_percentage_0.002 <0.002] <-0
pollen_counts_percentage_0.002 <-pollen_counts_percentage_0.002[rowSums(pollen_counts_percentage_0.002[,c(1:n_col)])!=0,]

### Combinaisons des info sur les pollens, la végétation et l'environement de dépôt
pollen_counts_percentage_0.002 <- as.data.frame(t(pollen_counts_percentage_0.002))
pollen_counts_percentage_0.002 <- merge(pollen_counts_percentage_0.002, vegetation_group_inferred, by = "row.names")
rownames(pollen_counts_percentage_0.002) <- pollen_counts_percentage_0.002$Row.names
pollen_counts_percentage_0.002$Row.names <- NULL

### Résultats
write.table(pollen_counts_percentage_0.002, "output/pollen_counts_percentage_0.002.csv", row.names=TRUE, col.names = NA, sep=";",dec=".", na=" ")
return(pollen_counts_percentage_0.002)}
