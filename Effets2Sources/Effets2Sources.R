library(tidyverse)  # dont ggplot2, tidyr, readr & dplyr
library(gplots)  # pour faire des heatmaps

### Import données
## La céramique des Halettes à Compiègne
Compiegne <- read.csv(url("https://raw.githubusercontent.com/JGravier/sitRada/master/Heatmap/Compiegne.csv"),
                      header = TRUE,
                      sep = ";",
                      stringsAsFactors = F)
View(Compiegne)
row.names(Compiegne) <- Compiegne$Periode
## PCR Plaine de Troyes : 
PCR <- read.csv(url("https://raw.githubusercontent.com/JGravier/sitRada/master/Effets2Sources/Data_PCR_Troyes.csv"),
                header = TRUE,
                sep = ";",
                dec =",",
                stringsAsFactors = F)
row.names(PCR) <- PCR$ID
View(PCR)
## Noyon : entités urbaines
Noyon <- read.csv(url("https://raw.githubusercontent.com/JGravier/These/master/Comparaison3Villes/Noyon_50ans_Fonctions.csv"),
                  header = T,
                  sep = ";",
                  stringsAsFactors = F)
row.names(Noyon) <- Noyon$X
View(Noyon)


### Observation des effets de sources pour les données du PCR : la standardisation
PCRM <- as.matrix(PCR[,2:4])  # besoin de transfo en type matrice pour obtenir des heatmaps

## Fonction de standardisation
Standar <- function(x){
  scale(x, center = TRUE,  # center T = centrée (moins moyenne)
        scale = TRUE)  # scale T = réduite (div. par écart-type)
}
# Note : dans la fonction scale() la variance est exprimée en 1/n-1 (et non en 1/n)

my_palette <- colorRampPalette(c("#018571", "#f5f5f5", "#a6611a"))(n = 50)
my_palette2 <- colorRampPalette(c("#f7f7f7", "#bdbdbd", "#636363"))(n=50)

PCRM %>%
  Standar() %>%
  heatmap.2(Rowv = FALSE,
            dendrogram = "column", # dendrogram sur colonne + reoder
            col = my_palette,
            denscol = "black",
            trace = "none")


### Rendre comparable les périodes chronologiques (éviter les effets de sources)
## Tableau transformé en fréquence des variables par ligne
FrceVar <- function(x){
  prop.table(x, margin = 1)*100  # *100 pour obtenir la fréquence en pourcentage
}
# Note : margin = 1 car on s'intéresse aux fréquences des variables par ligne (période chronologique)

PCRM %>%
  FrceVar() %>%  # mise en fréquence (= fréquence des variables)
  Standar() %>%  # standardisation de le tableau en pourcentage
  heatmap.2(Rowv = F,
            dendrogram = "column",
            col = my_palette,
            denscol = "black",
            trace = "none")


### Idem sur les données de Noyon
Noyon <- select(Noyon, F1:F10) # on ne veut pas la fonction 11 : "formations naturelles"
NoyonM <- as.matrix(Noyon)

## Données brutes
NoyonM %>%
  heatmap.2(Rowv = FALSE, Colv = F,
            dendrogram = "none",
            col = my_palette2,
            denscol = "black",
            trace = "none")

## Standardisation = effets de sources
NoyonM %>%
  Standar() %>%
  heatmap.2(Rowv = FALSE,
            dendrogram = "column",
            col = my_palette,
            denscol = "black",
            trace = "none")

## Visualisation des écarts à l'indépendance en pourcentage (souvent appelés en archéologie "écarts au pourcentage moyen")
# Tableau des écarts à l'indépendance (fonction générique)
TabEcart <- function(x){
  ni. <- margin.table(x, margin = 1)  # totaux des lignes i
  nj. <- margin.table(x, margin = 2)  # totaux des colonnes j
  n. <- sum(ni.)  # effectif total
  TabInde <- (round(nj.%*%t(ni.)/n., 2))  # tableau d'indépendance
  TabInde <- t(TabInde)
  TabEcart <- x - TabInde  # tableau des écarts à l'indépendance
}

## Visualisation des écarts au pourcentage moyen
NoyonM %>%
  FrceVar() %>%  # mise en pourcentage (= fréquence des variables)
  TabEcart() %>%  # écarts à l'indépendance sur le tableau en pourcentage
  heatmap.2(Rowv = F,
            dendrogram = "column",
            col = my_palette,
            denscol = "black",
            trace = "none")

## Quelle différence entre écart à l'indé et standardisation ?
# Peut se poser la question, au regard par ex des classes de
# périodes qui vont ressortir
blob <- NoyonM %>%
  FrceVar() %>%
  Standar() %>%
  dist() %>%
  hclust(method = "ward.D2")
blob2 <- NoyonM %>%
  FrceVar() %>%
  TabEcart() %>%
  dist() %>%
  hclust(method = "ward.D2")
plot(blob, hang = -1, cex = 0.6)
plot(blob2, hang = -1, cex = 0.6)
# il y en a (car beaucoup de lignes !), mais sont pas majeures

### Compiègne
CompM <- as.matrix(Compiegne[,2:17])
head(CompM)

CompM %>%
  Standar() %>%
  heatmap.2(Rowv = FALSE, Colv = F,
            dendrogram = "none",
            col = my_palette,
            denscol = "black",
            trace = "none")

CompM %>%
  FrceVar() %>%
  TabEcart() %>%
  heatmap.2(Rowv = FALSE, Colv = F,
            dendrogram = "none",
            col = my_palette,
            denscol = "black",
            trace = "none")

## Quelle différence entre écart à l'indé et standardisation ?
blobC <- CompM %>%
  FrceVar() %>%
  Standar() %>%
  dist() %>%
  hclust(method = "ward.D2")
blobC2 <- CompM %>%
  FrceVar() %>%
  TabEcart() %>%
  dist() %>%
  hclust(method = "ward.D2")
plot(blob, hang = -1, cex = 0.6)
plot(blob2, hang = -1, cex = 0.6)
# avec 5 périodes d'occupations, peu de différences