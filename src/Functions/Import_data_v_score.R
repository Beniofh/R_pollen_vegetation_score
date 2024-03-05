# This function allows you to import two types of data:
# - 1) Modern dataset consisting of a pollen count (count in number of occurrences) file for the different sites and 
#      a file on inferred vegetation and deposition conditions for the same sites. The pollen counts are processed as 
#      follows: (1) conversion to relative abundance, (2) deletion of the Prosoppis and Prosopis_african pollen 
#      types, (3) deletion of occurrences less than 0.002. The two files are combined into a single data frame saved 
#      in ./output/modern_pollen_counts_percentage_0.002_x1.csv where x1=value of vegetation_set.
# - 2) Fossil datasets containing a file with the pollen counts (count in number of occurrences) for each site and a 
#      column to find out which pollen types are used for the T_value ("used_for_T_value" column). These fossil 
#      datasets are formatted into a data frame and the pollen counts are passed in relative abundance. The data frame
#      is saved in ./output/fossil_pollen_counts_percentage_x1.csv where x1=value of vegetation_set.
#
# Arguments:
#   sample_type: To choose whether to print modern (sample_type="modern") or fossil data sets (sample_type="fossil")
#   vegetation_set: sets of vegetation groups to be used to calculate the V score
#       -> 1= the 18 vegetation groups of non-dynamic depositional environments and the 6 vegetation groups of dynamic depositional environments
#       -> 2= the 18 vegetation groups of non-dynamic depositional environments and the vegetation groups We/Bd/S-river or Wcd/Fb/Wd-river
#       -> 3= only the 6 vegetation groups of dynamic depositional environments
#   path_1: the path to the file with modern pollen counts or fossil pollen counts
#   path_2: To be completed only if sample_type="modern". The path to the file with the inferred vegetation and the deposition conditions for each site. 
#
# Return:
#   The new data frame
#
Import_data_v_score <- function(sample_type, vegetation_set, path_1, path_2=''){

  ### Importing the first data file
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

  ### Converts pollen counts into %.
  pollen_counts_percentage <- prop.table(data.matrix(pollen_counts),2) # 1 -> % par ligne ; 2 -> % par collone 
  pollen_counts_percentage <- as.data.frame(pollen_counts_percentage) # prop.table donne une matrix que l'onn retransforme ici en data.farme (plus simple à manipuler)

  if(sample_type=="modern"){  
    ### Additional processing after the % calculation so as not to affect the %.
    # remove Prosoppis and Prosopis_africana
    pollen_counts_percentage <- pollen_counts_percentage[!(rownames(pollen_counts_percentage) %in% c("Prosopis_africana", "Prosopis")), ] 
    # remove lines with no pollen (sum of pollens in the line != 0)
    n_col <- ncol(pollen_counts_percentage)
    pollen_counts_percentage <-pollen_counts_percentage[rowSums(pollen_counts_percentage[,c(1:n_col)])!=0,] 
    # Remove occurrences with low % (< 0.002)
    pollen_counts_percentage_processed <-pollen_counts_percentage
    pollen_counts_percentage_processed[pollen_counts_percentage_processed <0.002] <-0
    pollen_counts_percentage_processed <-pollen_counts_percentage_processed[rowSums(pollen_counts_percentage_processed[,c(1:n_col)])!=0,]
    pollen_counts_percentage_processed <- as.data.frame(t(pollen_counts_percentage_processed))
    ### Importing and formatting the second data file
    vegetation_group_inferred <- read.csv(path_2, stringsAsFactors = FALSE, sep = ";", row.names = 1)
    vegetation_group_inferred <- vegetation_group_inferred[, "vegetation_group_inferred", drop = FALSE]
    vegetation_group_inferred <- cbind(vegetation_group_inferred, depositional_env = rep("no_dynamic", nrow(vegetation_group_inferred)))
    vegetation_group_inferred$depositional_env[vegetation_group_inferred$vegetation_group_inferred %in% c("We-river","Bd-river", "S-river")]="We/Bd/S-river"
    vegetation_group_inferred$depositional_env[vegetation_group_inferred$vegetation_group_inferred %in% c("Wcd-river 1", "Wcd-river 2", "Fb/Wd-river")]="Wcd/Fb/Wd-river"
    vegetation_group_inferred$depositional_env[vegetation_group_inferred$vegetation_group_inferred=="removed"]="removed"
    ### Combination of information on pollen, vegetation and deposition environment
    pollen_counts_percentage_processed <- merge(pollen_counts_percentage_processed, vegetation_group_inferred, by = "row.names")
  } else if (sample_type=="fossil") {
    ### Combination of information on pollen and used_for_T_value
    pollen_counts_percentage_processed <- merge(used_for_T_value, pollen_counts_percentage, by = "row.names")
  }

  ### Final formatting of pollen_counts_percentage_processed
  rownames(pollen_counts_percentage_processed) <- pollen_counts_percentage_processed$Row.names
  pollen_counts_percentage_processed$Row.names <- NULL

  ### Saving results
  if(sample_type=="modern"){  
    write.table(pollen_counts_percentage_processed, paste0("output/modern_pollen_counts_percentage_0.002_", vegetation_set, ".csv"), row.names=TRUE, col.names = NA, sep=";",dec=".", na=" ")
  } else if (sample_type=="fossil") {
    write.table(pollen_counts_percentage_processed, paste0("output/fossil_pollen_counts_percentage_", vegetation_set, ".csv"), row.names=TRUE, col.names = NA, sep=";",dec=".", na=" ")
  }
  ### Return results
  return(pollen_counts_percentage_processed)
}
