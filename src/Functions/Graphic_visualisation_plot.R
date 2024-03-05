# This function creates a plot to display the significant and non-significant intervals of the V scores for each vegetation group.
# It then places points corresponding to the vegetation scores of the fossil samples (samples whose vegetation we want to determine). 
#
# Arguments:
#   lev_V_groups: a vector V_groups where the names are replaced by numbers, produced by the Groups_order_v_score() function 
#   V_val: matrix of the affinity values (A-values)
#   V_val_ident_for_graph: Matrix of V scores for fossil samples (samples whose vegetation we want to determine)  adapted to 
#   the ggplot() function.
#   representation: Argument as to whether the points representing the V scores of the fossil samples should be displayed on 
#   the plot. => 1=display, 2=do not display 
# Return:
#   A ggplot function
#
Graphic_visualisation_plot <- function(lev_V_groups, V_val, V_val_ident_for_graph, representation){
  ### Calcules pour geom_linerange dans la fonction ggplot()
  for ( X1 in lev_V_groups){
    assign(paste0(X1,"_min"), min(V_val[X1][V_val["vegetation_group_inferred"]==X1]))
    assign(paste0(X1,"_max"), max(V_val[X1][V_val["vegetation_group_inferred"]==X1]))
    assign(paste0(X1,"_min_other"), min(V_val[X1][V_val["vegetation_group_inferred"]!=X1]))
    assign(paste0(X1,"_max_other"), max(V_val[X1][V_val["vegetation_group_inferred"]!=X1]))}
  remove(X1)

  ### Création semi-automatique de la fonction ggplot
  X3 <-"ggplot(V_val_ident_for_graph, aes(x=test, y=score, shape=labels)) +"
  for ( X1 in lev_V_groups){
    X2 <- paste0("geom_linerange(mapping=aes(x='",X1,"', ymin=`",X1,"_min`, ymax=`",X1,"_max`), linewidth=10, color='turquoise3') + geom_linerange(mapping=aes(x='",X1,"', ymin=`",X1,"_min_other`, ymax=`",X1,"_max_other`), linewidth=6, color='lightsteelblue4') +")       
    X3 <- paste0(X3,X2)}
  switch (representation,
          "1" = X3 <- paste0(X3, "scale_shape_manual(values=c(97:122,49:57))+ geom_point(size=3) + scale_x_discrete(limits=lev_V_groups) + coord_flip() + ylab('Values of V score') + xlab('vegetation group tested')"), 
          "2" = X3 <- paste0(X3, "scale_shape_manual(values=c(97:122,49:57)) + scale_x_discrete(limits=lev_V_groups) + coord_flip() + ylab('Values of V score') + xlab('vegetation group tested')") )

  #note sur l expression final X3
  #scale_x_discrete(limits=lev_V_groups) -> ne reorganise pas automatiquement en fonction du level pour les donnees factor quand il y a geom_linerange
  #coord_flip() -> rotation du graphique 

  plot<-eval(parse(text=X3))
  return(plot)
}





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
detailed_visualisation <- function(V_score_vegetation, sample_type, data_type){
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
    # configuration du plot
    # eval(parse(text=V_score_vegetation)) -> permet de transformet le caractere V_score_vegetation en objet (retire les guillets)
    plot <-ggplot(V_val, aes(x=vegetation_group_inferred, y=eval(parse(text=paste0("`",V_score_vegetation,"`"))),fill=vegetation_group_inferred)) +
      xlab("vegetations groupes") + ylab(paste("V score of",V_score_vegetation, sep=" ")) + # noms des axes
      scale_fill_manual(values=c("plum","dark green","dark green","dark green","dark green","green","green","light salmon","khaki","khaki","khaki","lavender","lavender","lavender","sky blue","sky blue","sky blue","blue","blue","blue","blue","blue","blue","blue")) +  #couleur des boxplots (demande aussi d avoir fill=vegetation_group_inferred dans aes())
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


