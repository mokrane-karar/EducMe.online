# 0) Installation  des packages

list.des.packages <- c("FactoMineR", "factoextra", "corrplot", "tidyverse", "GGally", "car", "rgl")
new.packages <- list.des.packages[!(list.des.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# 1) Chargement des packages 
library(FactoMineR)
library(factoextra)
library(corrplot)
library(tidyverse)
library(GGally)
tab <- read.delim("tab.txt",header=TRUE, row.names=1) # il faut que la tableau soit dans le même répertoire que le script
head(tab)


# 3) exploration du tableau de données
summary(tab)
tibble(tab)           # visualisation des variables quantitatives par pairs et calcul du coefficient de corrélation de Pearson 

# 4) ACP

res <- PCA(tab, scale.unit = T)             # normalisation des variables si elles n'ont pas la même échelle  

plot(res) # visualiser les 2graphs de l'ACP
# options(ggrepel.max.overlaps = Inf)      # activer cette ligne de code si une erreur de superposition s'affiche sur la console

Factoshiny::PCAshiny(res) # génération de rapports automatiques pour l'ACP et la CLASSIFICATION

fviz_eig(res)                              # visualiser LA DISTRIBUTION Générale DE L' inertie 
fviz_pca_var(res, # objet retourné par la fonction PCA
             col.var ="cos2", # coloration PAR qualité de projection. Choix possibles:  "cord" "contrib"
             gradient.cols =c("#00AFBB","#E7B800","#FC4E07"), # palette de couleures 
             repel = TRUE)# éviter un chevauchement du texte 

fviz_pca_ind(res,col.ind ="cos2", gradient.cols =c("#00AFBB","#E7B800","#FC4E07"),
             repel =TRUE)# éviter un chevauchement du texte  pointsize = cos2

fviz_pca_biplot(res) # représentation des variables et des individus 
# contribution des variables 
fviz_contrib(res, choice = "var", axes = 1) # contribution à la composante principale n°1
fviz_contrib(res, choice = "var", axes = 2) # contribution à la composante principale n°2
fviz_contrib(res, choice = "var", axes = 1:2) # contribution aux deux

fviz_pca_biplot(res) # biplot 

# contribution des individus
fviz_contrib(res, choice = "ind", axes = 1)
fviz_contrib(res, choice = "ind", axes = 2)
fviz_contrib(res, choice = "ind", axes = 1:2)
# qualité de projection 
fviz_cos2(res, choice = "var")
fviz_cos2(res, choice = "ind", axes = 1)
fviz_cos2(res, choice = "var", axes = 1)
fviz_cos2(res, choice = "ind", axes = 1)

corrplot(res$var$cos2 , is.corr=FALSE, method = "number") # juste pour visualiser. exporter le graphique pour qu'il soit plus lisible

summary(res, nbelements=Inf)  ## donne toutes les sorties

## Graphe en coloriant les villes en fonction da la variable région. autre choix "ind" 
plot(res, habillage = "Région" )


# 5) classification hiérarchique 
res.HCPC<- HCPC(res, # objet retourné par l' ACP
                nb.clust=-1, # -1 nombre de clusters choisi automatiquement, si > 0 on choisi le nombre de clusters qu'on veut  
                consol=F, # si = TRUE une consolidation par K-MEANS clustering sera faite 
                graph=FALSE) # afiche ou non un graphique de la classification 

plot.HCPC(res.HCPC,choice='tree',title='Arbre hiérarchique')
plot.HCPC(res.HCPC,choice='map',draw.tree=FALSE,title='Plan factoriel')
plot.HCPC(res.HCPC,choice='3D.map',ind.names=TRUE,centers.plot=FALSE,angle=60,title='Arbre hi?rarchique sur le plan factoriel')

# GRAPH FINAL
fviz_dend(res.HCPC, 
          cex = 0.7,                     # Taille du text
          palette = "jco",               # Palette de couleur 
          rect = TRUE, rect_fill = FALSE, # Rectangle autour des groupes / remplir les rectangles
          rect_border = "jco",           # Couleur du rectangle
          labels_track_height = 7  # Augment l'espace pour le texte. il faut l'ajuster jusqu'à l'obtention d'un graphique lisible  
          , horiz = TRUE         # représentation horizontale ou verticale     
)




# visualisation de tous les résultats sur le plan factoriel avec ellipses 
fviz_cluster(res.HCPC, geom = "text", main = "Factor map",
             c("point", "text") , repel = TRUE, 
             ellipse = TRUE, ellipse.type="norm", ellipse.level=0.95) # visualisation des clusters sur le plan factoriel 
plot(res.HCPC, choice = "3D.map")
fviz_cluster(res.HCPC, data = adjou, ellipse.type ="euclid",geom = "text", main = "Factor map", star.plot =TRUE, repel =TRUE, ggtheme =theme_minimal())


