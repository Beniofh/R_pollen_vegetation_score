# Function for viewing the V score of a plant in detail. Shows the range of V score values for a 
# vegetation for each vegetation group.

# Arguments:
#   V_score_vegetation: 
#   sample_type: to find out whether the data in the data frame to be converted (TAB) has been imported using the 
#                Import_modern_data_v_score function with the "fossil" or "modern" argument in order to adapt the 
#                processing.
#   data_type: the type of data to be output  ("1"= relative abundance, "2"=relative abundance scale, "3"=presence/absence)
#
# Return:
#   A ggplot function
#
Detailed_visualisation <- function(V_score_vegetation, vegetation_set, sample_type, data_type){
  if (file.exists(paste0("output/modern_matix_V_values_", vegetation_set, "_", data_type, ".csv"))) {
    # chargement modern_matix_V_values 
    V_val<- read.csv(paste0("output/modern_matix_V_values_", vegetation_set, "_", data_type, ".csv"), stringsAsFactors = FALSE, sep = ";", check.names = FALSE, row.names = 1)
    #
    if (!(V_score_vegetation %in% colnames(V_val[-c(1,2)]))) {
      stop(paste0(V_score_vegetation," is not a label from one of the vegetation groups in vegetation_sets=3. The labels available for use in the V_score_vegetation setting are: ", paste(colnames(V_val[-c(1,2)]), collapse = "; "), "."))
      }
    # recuperer la valeur de score la plus faible pour les sites du meme type d habitat que celui du test
    limite1<-min(V_val[V_score_vegetation][V_val["vegetation_group_inferred"]==V_score_vegetation])
    # recuperer la valeur de score la plus haute pour les sites de type d habitat different de celui du test
    limite2<-max(V_val[V_score_vegetation][V_val["vegetation_group_inferred"]!=V_score_vegetation])
    # définir l'ordre d'apparition des boxplots
    V_val$vegetation_group_inferred <- factor(V_val$vegetation_group_inferred, levels = colnames(V_val[-c(1,2)]))
    # configuration de la couleur des boxplots
    switch (vegetation_set,
            "1" = boxplot_color <- c("plum","dark green","dark green","dark green","dark green",
                                     "green","green","light salmon","khaki","khaki","khaki","khaki","khaki",
                                     "blue","blue","lavender","lavender","lavender","sky blue",
                                     "sky blue","sky blue","sky blue","sky blue","sky blue","sky blue"),
            "2" = boxplot_color <- c("plum","dark green","dark green","dark green","dark green",
                                     "green","green","light salmon","khaki","khaki","khaki","khaki","khaki",
                                     "blue","blue","lavender","lavender","lavender","sky blue",
                                     "sky blue","sky blue"),
            "3" = boxplot_color <- c("light salmon","light salmon","green","plum1","khaki","lavender"))
    # configuration du plot
    # eval(parse(text=V_score_vegetation)) -> permet de transformet le caractere V_score_vegetation en objet (retire les guillets)
    plot <-ggplot(V_val, aes(x=vegetation_group_inferred, y=eval(parse(text=paste0("`",V_score_vegetation,"`"))),fill=vegetation_group_inferred)) +
      xlab("vegetations groupes") + ylab(paste("V score of",V_score_vegetation, sep=" ")) + # noms des axes
      scale_fill_manual(values=boxplot_color) +  #couleur des boxplots (demande aussi d avoir fill=vegetation_group_inferred dans aes())
      theme(legend.position="none") + # supprimer la légende
      geom_boxplot() +  # pour activer les boxplots
      coord_flip()   # rotation du graphique
    #geom_hline(yintercept=limite1, linetype="dashed", color = "red", size=1) +   
    #geom_hline(yintercept=limite2 , color = "red", size=1)
    return(plot)
  } else {
    stop(paste0("The file output/modern_matix_V_values_", vegetation_set, "_", data_type, ".csv does not exist. You must run the main_1.R script with vegetation_set=",vegetation_set," and data_type=",data_type," before running this script with these parameters."))
  }
}


