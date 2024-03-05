Identification_visualisation <- function(lev_V_groups, V_val_identification){
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