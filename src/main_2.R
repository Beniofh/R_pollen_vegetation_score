### Plots des resultats pour chaque test d habitat
# selection de l habitat a tester (cf: lev_V_groups)
b<-"Bd-spring 1"

V_val<- read.csv("output/matix_V_values.csv", stringsAsFactors = FALSE, sep = ";", check.names = FALSE, row.names = 1)

# recuperer la valeur de score la plus faible pour les sites du meme type d habitat que celui du test
limite1<-min(V_val[b][V_val["vegetation_group_inferred"]==b])
# recuperer la valeur de score la plus haute pour les sites de type d habitat different de celui du test
limite2<-max(V_val[b][V_val["vegetation_group_inferred"]!=b])
# configuration du plot
# eval(parse(text=b)) -> permet de transformet le caractere b en objet (retire les guillets)
ggplot(V_val, aes(x=vegetation_group_inferred, y=eval(parse(text=paste0("`",b,"`"))),fill=vegetation_group_inferred)) +
  xlab("vegetations groupes") + ylab(paste("V score of",b, sep=" ")) + # noms des axes
  scale_fill_manual(values=c("plum","dark green","dark green","dark green","dark green","green","green","light salmon","khaki","khaki","khaki","lavender","lavender","lavender","sky blue","sky blue","sky blue","blue","blue","blue","blue","blue","blue","blue")) +  #couleur des boxplots (demande aussi d avoir fill=vegetation_group_inferred dans aes())
  theme(legend.position="none") + # supprimer la légende
  geom_boxplot() +  # pour activer les boxplots
  coord_flip()   # rotation du graphique
#geom_hline(yintercept=limite1, linetype="dashed", color = "red", size=1) +   
#geom_hline(yintercept=limite2 , color = "red", size=1)

