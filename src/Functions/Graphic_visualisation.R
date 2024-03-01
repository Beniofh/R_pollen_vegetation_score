# This function creates a multiplot to display the V-score results for each vegetation group,
# separating samples that belong to the vegetation tested from those that do not.
#
# Arguments:
#   lev_V_groups : a vector V_groups where the names are replaced by numbers, produced by the Groups_order_v_score() function 
#   V_val: matrix of the affinity values (A-values)
# Return:
#   A ggplot function
#
Graphic_visualisation_multiplot <- function(lev_V_groups, V_val){
  
  ### Prepartation pour des plots
  for (i in lev_V_groups){
    # creation de multi_plots_tampon (pour creation du vrai multi_plots )
    multi_plots_tampon<-cbind(row.names(V_val),V_val["vegetation_group_inferred"],V_val["vegetation_group_inferred"],V_val[i])
    colnames(multi_plots_tampon)<-c("labels","test","veg_group_tested","score")
    # transformation des variables factor en variable character
    multi_plots_tampon$labels<-as.character(multi_plots_tampon$labels)
    multi_plots_tampon$test<-as.character(multi_plots_tampon$test)
    multi_plots_tampon$veg_group_tested<-as.character(multi_plots_tampon$veg_group_tested)
    # configuration de multi_plots_tampon["veg_group_tested"]
    multi_plots_tampon["veg_group_tested"][multi_plots_tampon["veg_group_tested"]!=i] <- "other groups"
    multi_plots_tampon["veg_group_tested"][multi_plots_tampon["veg_group_tested"]==i] <- "group tested"
    # configuration de multi_plots_tampon["test"]
    multi_plots_tampon["test"] <- i
    # Ajout de multi_plots_tampon a la matrice fianl multi_plots
    if(i==lev_V_groups[1]){multi_plots <-multi_plots_tampon} else {multi_plots<-rbind(multi_plots,multi_plots_tampon,make.row.names = F)}
  } 

  ### ordonner les levels de V_val$vegetation_group_inferred en fonction de lev_V_groups 
  lev_V_groups_fac <-factor(lev_V_groups)
  multi_plots$test <- factor(multi_plots$test,lev_V_groups_fac)
  
  ###COnstruction du multiplots
  plot <- ggplot(data=multi_plots, mapping=aes(x=veg_group_tested, y=score, color=veg_group_tested)) +
                facet_wrap(~test, scales='free') +
                geom_boxplot() +
                xlab("") + ylab("Score of habitat") + # noms des axes
                theme(legend.position="none")  # supprimer la légende
return(plot)}




# Function used to create a V-score data frame for fossil samples (samples whose vegetation is to be determined)
# adapted to the ggplot() function.
#
# Arguments:
#   lev_V_groups : a vector V_groups where the names are replaced by numbers, produced by the Groups_order_v_score() function 
#   V_val_identification: Matrix of V scores for fossil samples (samples whose vegetation we want to determine)
# Return:
#   V-score data frame
#
identification_visualisation <- function(lev_V_groups, V_val_identification){
  ### Creation de identification_final (dataset pour les plots)
  #debut de la boucle ################
  for (i in lev_V_groups){
    print(i)
    # creation de V_val_ident_for_graph_tempo (pour creation du vrai multi_plots )
    V_val_ident_for_graph_tempo<-cbind(row.names(V_val_identification),V_val_identification[lev_V_groups[1]],V_val_identification[i])
    colnames(V_val_ident_for_graph_tempo)<-c("labels","test","score")
    # transformation des variables factor en variable character
    V_val_ident_for_graph_tempo$labels<-as.character(V_val_ident_for_graph_tempo$labels)
    V_val_ident_for_graph_tempo$test<-as.character(V_val_ident_for_graph_tempo$test)
    # configuration de V_val_ident_for_graph_tempo["test"]
    V_val_ident_for_graph_tempo["test"] <- i
    # Ajout de V_val_ident_for_graph_tempo a la matrice fianl multi_plots
    if(i==lev_V_groups[1]){V_val_ident_for_graph <-V_val_ident_for_graph_tempo} else {V_val_ident_for_graph<-rbind(V_val_ident_for_graph,V_val_ident_for_graph_tempo,make.row.names = F)}
  } 
  #fin de la boucle #################
  #ordonner les levels de A_val$vegetation_group_inferred en fonction de lev_V_groups 
  lev_V_groups_fac <-factor(lev_V_groups)
  V_val_ident_for_graph$test <- factor(V_val_ident_for_graph$test,lev_V_groups_fac)
  #verification de l ordre des groupes (ordre choisi)
  levels(V_val_ident_for_graph$test)
  ### pour ordonner correctement les noms des niveaux dans la légende
  V_val_ident_for_graph$labels <-factor(V_val_ident_for_graph$labels,row.names(V_val_identification))
  return(V_val_ident_for_graph)
}





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
    X2 <- paste0("geom_linerange(mapping=aes(x='",X1,"', ymin=`",X1,"_min`, ymax=`",X1,"_max`), size=10, color='turquoise3') + geom_linerange(mapping=aes(x='",X1,"', ymin=`",X1,"_min_other`, ymax=`",X1,"_max_other`), size=6, color='lightsteelblue4') +")       
    X3 <- paste0(X3,X2)}
  switch (representation,
          "1" = X3 <- paste0(X3, "scale_shape_manual(values=c(97:122,49:57))+ geom_point(size=3) + scale_x_discrete(limits=lev_V_groups) + coord_flip()"), 
          "2" = X3 <- paste0(X3, "scale_shape_manual(values=c(97:122,49:57)) + scale_x_discrete(limits=lev_V_groups) + coord_flip()") )

  #note sur l expression final X3
  #scale_x_discrete(limits=lev_V_groups) -> ne reorganise pas automatiquement en fonction du level pour les donnees factor quand il y a geom_linerange
  #coord_flip() -> rotation du graphique 

  plot<-eval(parse(text=X3))
  return(plot)
}
  