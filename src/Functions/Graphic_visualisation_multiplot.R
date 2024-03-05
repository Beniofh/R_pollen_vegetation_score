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
                xlab("") + ylab("Vegetation score") + # noms des axes
                theme(legend.position="none")  # supprimer la légende
return(plot)}