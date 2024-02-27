#vecteur groupe en noms
Groups_creation_v_score <- function(determination, TAB){
  switch (determination,
          "1" = Groupe <-TAB$vegetation_group_inferred,
          "2" = {TAB$vegetation_group_inferred[TAB$depositional_env=="We/Bd/S-river"]="We/Bd/S-river"
          TAB$vegetation_group_inferred[TAB$depositional_env=="Wcd/Fb/Wd-river"]="Wcd/Fb/Wd-river"
          Groupe <-TAB$vegetation_group_inferred}, #car le calcule des T-values se base sur les labels de TAB$vegetation_group_inferred qui doivent être les meme que Groupe -> cf : TAB[,second_last_col]
          "3" = {TAB <- TAB[TAB$depositional_env!="no_dynamic",]
          Groupe <-TAB$vegetation_group_inferred})
  # résultats
  return(list(Groupe=Groupe, TAB=TAB))}

Groups_order_v_score <- function(determination, Groupe){
  #vecteur groupe en chiffre (deffini l ordre d apparition dans groupe dans la matrice)
  lev <- Groupe
  switch (determination,
          "1" = {lev[lev =="Fb/Wd-low/middle"] <-7
          lev[lev =="Fb/Wd-upper"] <-6
          lev[lev =="Bd-spring 2"] <-15
          lev[lev =="Bd-rtf"] <-10
          lev[lev =="Bd-wb 1"] <-13
          lev[lev =="Bd-wb 2"] <-12
          lev[lev =="Bd-spring 1"] <-14
          lev[lev =="A.E-e"] <-1
          lev[lev =="Fa-sc 1"] <-5
          lev[lev =="Bd-wgc"] <-9
          lev[lev =="Fa-sc 2"] <-4
          lev[lev =="Fa-upper"] <-3
          lev[lev =="Fa-w"] <-2
          lev[lev =="Bd-wb 3"] <-11
          lev[lev =="Wcd"] <-8
          lev[lev =="S-xs"] <-16
          lev[lev =="S-xds"] <-17
          lev[lev =="D"] <-18
          lev[lev =="Bd-river"] <-23
          lev[lev =="We-river"] <-22
          lev[lev =="S-river"] <-24
          lev[lev =="Wcd-river 1"] <-20
          lev[lev =="Wcd-river 2"] <-19
          lev[lev =="Fb/Wd-river"] <-21},
          "2" = {lev[lev =="Fb/Wd-low/middle"] <-7
          lev[lev =="Fb/Wd-upper"] <-6
          lev[lev =="Bd-spring 2"] <-15
          lev[lev =="Bd-rtf"] <-10
          lev[lev =="Bd-wb 1"] <-13
          lev[lev =="Bd-wb 2"] <-12
          lev[lev =="Bd-spring 1"] <-14
          lev[lev =="A.E-e"] <-1
          lev[lev =="Fa-sc 1"] <-5
          lev[lev =="Bd-wgc"] <-9
          lev[lev =="Fa-sc 2"] <-4
          lev[lev =="Fa-upper"] <-3
          lev[lev =="Fa-w"] <-2
          lev[lev =="Bd-wb 3"] <-11
          lev[lev =="Wcd"] <-8
          lev[lev =="S-xs"] <-16
          lev[lev =="S-xds"] <-17
          lev[lev =="D"] <-18
          lev[lev =="We/Bd/S-river"] <-20
          lev[lev =="Wcd/Fb/Wd-river"] <-19},
          "3" = {lev[lev =="Bd-river"] <-5
          lev[lev =="We-river"] <-4
          lev[lev =="S-river"] <-6
          lev[lev =="Wcd-river 1"] <-2
          lev[lev =="Wcd-river 2"] <-1
          lev[lev =="Fb/Wd-river"] <-3})
  #transformer lev en vecteur numerique 
  lev<-as.numeric(lev)
  #reordonner les groupes selon le vecteur groupe chiffre
  GroupeBien= reorder(Groupe,lev)
  levGroupe = levels(GroupeBien)
  # résultats
  return(list(levGroupe=levGroupe, lev=lev))}