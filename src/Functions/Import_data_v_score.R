# Allows you to import two types of dataset: (1) Modern datasets consisting of a pollen count file and a vegetation and environmental file, 
# which are combined into a single Data Frame saved in ./output/pollen_counts_percentage_0.002.csv
# This function takes two CSV files as input and returns a datafram file. 
#
#Talking about percentages 

sample_type="fossil"
path_1="data/identification_full_Hadar_2_V2.csv"



# Importation des donnees (attention mettre le nom des groupes dans l avant derniere colone et le nomnero du groupe dans la derniere colonne
Import_data_v_score <- function(sample_type, path_1, path_2=''){

  ### Importation des donnees 1
  pollen_counts <- read.csv(path_1, stringsAsFactors = FALSE, sep = ";", row.names = "R_labels")

  if(sample_type=="modern"){  
    pollen_counts <- pollen_counts[ , -c(1:7)]
  } else if (sample_type=="fossil") {
    used_for_T_value <- pollen_counts[, "used_for_T_value", drop = FALSE]
    pollen_counts <- pollen_counts[ , -c(1:6)]
  } else {
    print("Incorect value for sample_type.")
  }

  pollen_counts[is.na(pollen_counts)] <- 0

  ### Mettre les comptages polliniques en %
  pollen_counts_percentage <- prop.table(data.matrix(pollen_counts),2) # 1 -> % par ligne ; 2 -> % par collone 
  pollen_counts_percentage <- as.data.frame(pollen_counts_percentage) # prop.table donne une matrix que l'onn retransforme ici en data.farme (plus simple à manipuler)

  if(sample_type=="modern"){  
    ### Traitement complementaire de taxon (après caclcule des % pour ne pas impacter les %)
    # retirer Prosoppis et Prosopis_africana
    pollen_counts_percentage <- pollen_counts_percentage[!(rownames(pollen_counts_percentage) %in% c("Prosopis_africana", "Prosopis")), ] 
    # retrait des lignes sans pollens (somme des pollen de la ligne != 0)
    n_col <- ncol(pollen_counts_percentage)
    pollen_counts_percentage <-pollen_counts_percentage[rowSums(pollen_counts_percentage[,c(1:n_col)])!=0,] 
    ### Retrait des taxons avec faible % (< 0.002)
    pollen_counts_percentage_processed <-pollen_counts_percentage
    pollen_counts_percentage_processed[pollen_counts_percentage_processed <0.002] <-0
    pollen_counts_percentage_processed <-pollen_counts_percentage_processed[rowSums(pollen_counts_percentage_processed[,c(1:n_col)])!=0,]
    pollen_counts_percentage_processed <- as.data.frame(t(pollen_counts_percentage_processed))
    ### Importation des donnees 2
    vegetation_group_inferred <- read.csv(path_2, stringsAsFactors = FALSE, sep = ";", row.names = 1)
    vegetation_group_inferred <- vegetation_group_inferred[, "vegetation_group_inferred", drop = FALSE]
    ### Création du df pour les groupes de végétation et les environements de dépôts
    vegetation_group_inferred <- cbind(vegetation_group_inferred, depositional_env = rep("no_dynamic", nrow(vegetation_group_inferred)))
    vegetation_group_inferred$depositional_env[vegetation_group_inferred$vegetation_group_inferred %in% c("We-river","Bd-river", "S-river")]="We/Bd/S-river"
    vegetation_group_inferred$depositional_env[vegetation_group_inferred$vegetation_group_inferred %in% c("Wcd-river 1", "Wcd-river 2", "Fb/Wd-river")]="Wcd/Fb/Wd-river"
    vegetation_group_inferred$depositional_env[vegetation_group_inferred$vegetation_group_inferred=="removed"]="removed"
    ### Combinaisons des info sur les pollens, la végétation et l'environement de dépôt
    pollen_counts_percentage_processed <- merge(pollen_counts_percentage_processed, vegetation_group_inferred, by = "row.names")
  } else if (sample_type=="fossil") {
    ### Combinaisons des info sur les pollens et used_for_T_value
    pollen_counts_percentage_processed <- merge(used_for_T_value, pollen_counts_percentage, by = "row.names")
  }

  ### Mise en forme de pollen_counts_percentage_processed
  rownames(pollen_counts_percentage_processed) <- pollen_counts_percentage_processed$Row.names
  pollen_counts_percentage_processed$Row.names <- NULL

  ### Résultats
  if(sample_type=="modern"){  
    write.table(pollen_counts_percentage_processed, paste0("output/modern_pollen_counts_percentage_0.002.csv"), row.names=TRUE, col.names = NA, sep=";",dec=".", na=" ")
  } else if (sample_type=="fossil") {
    write.table(pollen_counts_percentage_processed, paste0("output/fossil_pollen_counts_percentage.csv"), row.names=TRUE, col.names = NA, sep=";",dec=".", na=" ")
  }
  return(pollen_counts_percentage_processed)
}
