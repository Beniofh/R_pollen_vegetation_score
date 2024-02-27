# Création de la matrice des T-values
T_values_v_score <- function(levGroupe, TAB, last_col, second_last_col){
  #recuperer les comptages de TAB en retirant les 2 dernieres colonnes (nom et chiffre des groupes)
  matVT = matrix(0, nrow=length(levGroupe), ncol= dim(TAB) [2] -2)
  #renommer les lignes par les nom de groupes 
  rownames(matVT) = levGroupe
  #renomer les colonnes par les noms des sites 
  colnames(matVT) = colnames(TAB[,-c(second_last_col,last_col)])
  # calculer les T-values
  for ( aa in 1:length(levGroupe)){
    for (bb in 1:(dim(TAB) [2] -2)){
      matVT[aa,bb] = ((mean(TAB[which(TAB[,second_last_col]==levGroupe[aa]),bb]))-(mean(TAB[,bb])))/
        (sqrt(((dim(TAB)[1]-dim(TAB[which(TAB[,second_last_col]==levGroupe[aa]),])[1])/(dim(TAB)[1]-2))*(var(TAB[,bb]) /dim(TAB[which(TAB[,second_last_col]==levGroupe[aa]),])[1])))
    }
  }
  #Transposition (plus simple a coder et a lire)
  T_val<-data.frame(check.names = FALSE,t(matVT))
  #Remplace les lignes de Na (les taxons qui ont une occurence de 0 en "abondance scale") *note : ces taxons peuvent restés malgré la suppression es taxons avec une occurence de 0 au niveaux des concentrations relatives  car une valeur de 0.001 en concentration relative à une valeur de 0 en abondance scale) 
  T_val[is.na(T_val)]<-0
  #Sauvegarde en .csv
  write.table(T_val, "output/matix_T_values.csv", row.names=TRUE, col.names = NA, sep=";",dec=".", na=" ")
  #résultats
  return(T_val)}