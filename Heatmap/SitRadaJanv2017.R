library(corrplot)
library(dplyr)
library(gplots)  # heatmap

## Import données
Compiegne <- read.csv(url("https://raw.githubusercontent.com/JGravier/sitRada/master/Heatmap/Compiegne.csv"),
                       header = TRUE,
                       sep = ";")
View(Compiegne)
row.names(Compiegne) <- Compiegne$Periode

CompM <- as.matrix(Compiegne[,2:17])  # matrice pour faire des heatmap
head(CompM)


## Heatmap sur les données brutes
my_palette2 <- colorRampPalette(c("#018571", "#f5f5f5", "#a6611a"))(n = 50)  # création de sa propre palette de couleur

heatmap.2(CompM, Rowv = FALSE, Colv = FALSE,
          dendrogram = 'none',
          col = my_palette2,
          denscol = "black")

# Données brutes : ressemblances
heatmap.2(CompM, Rowv = FALSE,  # ne change pas les lignes (les périodes d'occupation du site archéologique)
          dendrogram = 'column',  # réorganisation des colonnes (types de céramiques)
          col = my_palette2,
          denscol = "black")
# Par défaut, la distance prise en compte par la fonction heatmap.2
# est une distance euclidienne ; on peut donc la changer (voir fonction dist)
# et, bien entendu, en créer une !

# Créer sa fonction de distance khi-deux
library(ade4)
distChi2 <- function(x)
{
  coa <- dudi.coa(x, scannf = FALSE, nf = 6) # nf = nombre de facteurs
  coa <- dist.dudi(coa)
}

heatmap.2(CompM, Rowv = FALSE,
          dendrogram = 'column',
          col = my_palette2,
          denscol = "black",
          distfun = function(x) as.dist(distChi2(x))) # distance khi-deux
# Plus intéressant pour repérer des groupes mais la visualisation
# en heatmap est inutile... Autant faire simplement un dendrogramme !

## Dendrogramme sur distance khi-deux
tCompM <- t(CompM)  # tableau transposé
thc <- hclust(as.dist(distChi2(tCompM)))
plot(thc, hang = -1, cex = 0.6)


## Standardisation
StandComp <- scale(CompM, 
                   center = TRUE,  # center T = centrée (moins moyenne)
                   scale = TRUE)  # et scale T = réduite (div. par écart-type)
head(StandComp)

heatmap.2(StandComp, Rowv = FALSE, Colv = FALSE,
          dendrogram = 'none',
          col = my_palette2,
          denscol = "black")

heatmap.2(StandComp, Rowv = FALSE,
          dendrogram = 'column',
          col = my_palette2,
          denscol = "black")
