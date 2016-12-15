### Import données
Compiegne <- read.csv(url("https://raw.githubusercontent.com/JGravier/sitRada/master/Heatmap/Compiegne.csv"),
                       header = TRUE,
                       sep = ";")
View(Compiegne)
row.names(Compiegne) <- Compiegne$Periode

CompM <- as.matrix(Compiegne[,2:17])  # matrice pour faire des heatmap
head(CompM)


### Heatmap sur les données brutes
my_palette2 <- colorRampPalette(c("#018571", "#f5f5f5", "#a6611a"))(n = 50)  # création de sa propre palette de couleur

library(gplots)  # heatmap
heatmap.2(CompM, Rowv = FALSE, Colv = FALSE,
          dendrogram = 'none',
          col = my_palette2,
          denscol = "black")

## Données brutes : ressemblances
heatmap.2(CompM, Rowv = FALSE,  # ne change pas les lignes (les périodes d'occupation du site archéologique)
          dendrogram = 'column',  # réorganisation des colonnes (types de céramiques)
          col = my_palette2,
          denscol = "black")
# Par défaut, la distance prise en compte par la fonction heatmap.2
# est une distance euclidienne ; on peut donc la changer (voir fonction dist)
# et, bien entendu, en créer une !

## Créer sa fonction de distance khi-deux
library(ade4)
distChi2 <- function(x)
{
  coa <- dudi.coa(x, scannf = FALSE, nf = ncol(x)) # nf = nombre de facteurs
  coa <- dist.dudi(coa)
}

heatmap.2(CompM, Rowv = FALSE,
          dendrogram = 'column',
          col = my_palette2,
          denscol = "black",
          distfun = function(x) as.dist(distChi2(x))) # distance khi-deux
# Plus intéressant pour repérer des groupes mais la visualisation
# en heatmap est inutile... Autant faire simplement un dendrogramme !


### Dendrogramme sur distance khi-deux
tCompM <- t(CompM)  # tableau transposé
thc <- hclust(as.dist(distChi2(tCompM)), method = "ward.D")  # ici il n'y a pas de réinjection des poids des lignes & colonnes
plot(thc, hang = -1, cex = 0.6)

## Test de la proposition de R. Cura : http://rpubs.com/RobinC/AFC_CAH
library(FactoClass)
CAH <- ward.cluster(as.dist(distChi2(tCompM)),
                    peso = apply(X=tCompM, MARGIN=1, FUN=sum),  # réinjection des poids des lignes & colonnes selon la méthode Benzécri
                    plots = TRUE, h.clust = 1)
plot(as.dendrogram(CAH))

## Comparaison des deux dendrogrammes
par(mfrow = c(1,2))
plot(thc, hang = -1, cex = 0.6)
plot(as.dendrogram(CAH))
# Notez bien des petites différences de clusters... en toute logique, car la méthode diffère !


### Standardisation
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


### Écarts à l'indépendance
## calcul des totaux des lignes i et colonnes j et de l'effectif total
ni.<- margin.table(CompM, 1)  # 1 = sur les lignes
ni.

nj. <- margin.table(CompM, 2)  # 2 = sur colonnes
nj.

n. <- sum(ni.)  # équivaut bien entendu à sum(nj.)
n.

## calcul tablau d'indépendance
TableInde <- (round(nj.%*%t(ni.)/n., 2))
TableInde <- t(TableInde)
head(TableInde)

## calcul tablau d'écart à l'indépendance
TableEcart <- CompM - TableInde
head(TableEcart)

heatmap.2(TableEcart, Rowv = FALSE, Colv = FALSE,
          col = my_palette2,
          denscol = "black",
          dendrogram = 'none')

heatmap.2(TableEcart, Rowv = FALSE,
          col = my_palette2,
          denscol = "black",
          dendrogram = 'column')
# Pour lire pluf facilement l'ensemble, 
# on peut regarder les écarts transformés en pourcentage


### Écarts à l'indépendance en pourcentage (souvent appelés en archéologie "écarts au pourcentage moyen")
## Fréquence des types
CompMPourc <- prop.table(CompM, 1)*100  # *100 pour obtenir la fréquence en pourcentage
# Note : margin = 1 car on s'intéresse aux fréquences des types par période (chaque période/ligne i équivaut à 100%)
View(CompMPourc)

## calcul des totaux des lignes i et colonnes j et de l'effectif total
niP.<- margin.table(CompMPourc, 1)
niP.

njP. <- margin.table(CompMPourc, 2)
njP.

nP. <- sum(niP.)
nP.

## calcul tablau d'indépendance en pourcentage ("pourcentage moyen des types")
TIPourc <- (round(njP.%*%t(niP.)/nP., 2))
TIPourc <- t(TIPourc)
head(TIPourc)

## calcul tablau d'écart à l'indépendance en pourcentage
TEPourc <- CompMPourc - TIPourc
head(TEPourc)

heatmap.2(TEPourc, Rowv = FALSE, Colv = FALSE,
          col = my_palette2,
          denscol = "black",
          dendrogram = 'none')

heatmap.2(TEPourc, Rowv = FALSE,
          col = my_palette2,
          denscol = "black",
          dendrogram = 'column')

### Heatmap sur la matrice de corrélation
cor <- cor(CompM)  # création de la matrice de corrélation (par défaut, méthode Pearson)
# on regarde donc ici les corrélations de types de céramiques

## Visualisation
library(corrplot)
corrplot(cor, type="upper",  # on visualise de manière triangulaire en haut à droite
         order="hclust",  # méthode de réordonnancement selon hclust : hierarchical clustering
         col = my_palette2,
         tl.col="black", tl.srt=20)
# on a ici l'impression que le type de céramique A est corrélé négativement de manière significative
# avec les types F, J, I, etc. Mais est-ce bien le cas ?! Regardons les types de céramiques deux à deux
# sous la forme de nuages de points (scatterplot)

## Visualisation des nuages de points
pairs(CompM)
# la visualisation n'est pas extrêmement claire... On peut l'améliorer !

panel.cor <- function(x, y, digits = 2, cex.cor, ...) # selon http://www.r-bloggers.com/scatter-plot-matrices-in-r/
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # calcul coefficient de corrélation
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.7, txt)
  
  # calcul p-value
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.5, txt2)
  
  # calcul r² : ajout perso, car très utile pour nos données en SHS !
  rdeux <- lm(formula = x ~ y)
  rdeux <- summary(rdeux)$r.squared
  txt <- format(c(rdeux, 0.123456789), digits = digits)[1]
  txt <- paste("r²= ", txt, sep = "")
  text(0.5, 0.3, txt)
}

pairs(CompM, upper.panel = panel.cor)
# Au final, on note clairement qu'il n'y a pas de corrélations entre le type A et les autres...
# ...car il est toujours nécessaire de se méfier des coefficients de corrélation sans avoir vu les nuages de points.
# On peut également se demander s'il existe des corrélations entre les périodes

## Visualisation des nuages de points période à période
pairs(t(CompM), upper.panel = panel.cor)
# le type A a des valeurs extrêmes, donc le coefficient de corrélation est très "sensible" à ce type.
# Deux solutions possibles : 1) virer la valeur extrême, ou 2) transformer les valeurs en log10 et faire un ajustement puissance


## Solution 1) : extraire le type A de l'ensemble étudié
CompMSansA <- t(CompM[,2:16])
head(CompMSansA)

pairs(CompMSansA, upper.panel = panel.cor)
# ce n'est pas extrêmement satisfaisant car il existe toujours de valeurs extrêmes pour d'autres types de céramiques


## Solution 2) : transformer les valeurs en log10 et faire un ajustement puissance
panel.corlog10 <- function(x, y, digits = 2, cex.cor, ...) # selon http://www.r-bloggers.com/scatter-plot-matrices-in-r/
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # calcul coefficient de corrélation
  r <- cor(log10(x), log10(y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.7, txt)
  
  # calcul p-value
  p <- cor.test(log10(x), log10(y))$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.5, txt2)
  
  # calcul r² : ajout perso, car très utile pour nos données en SHS !
  rdeux <- lm(formula = log10(x) ~ log10(y))
  rdeux <- summary(rdeux)$r.squared
  txt <- format(c(rdeux, 0.123456789), digits = digits)[1]
  txt <- paste("r²= ", txt, sep = "")
  text(0.5, 0.3, txt)
}

pairs(log10(t(CompM)), upper.panel = panel.corlog10)
# cela pose problème, car log10(0) est indéfini !!
# Ainsi, une solution est de transformer les 0 en 2, cela ne changera pas fondamentalement les résultats, 
# mais il est nécessaire de bien le préciser :)

TableSansNul <- ifelse(CompM == 0, 2, CompM)
TableSansNul <- t(TableSansNul)
View(TableSansNul)

pairs(log10(TableSansNul), upper.panel = panel.corlog10)
