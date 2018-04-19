library(tidyverse)
library(ade4)


#### Import des données et des fonctions ####

# La céramique des Halettes à Compiègne (Oise) : le tableau est extrait (p. 44) de :
# DESACHY B., 2004, « Le sériographe EPPM: un outil informatisé de sériation graphique pour tableaux de comptages », 
# Rev. archéologique Picardie [en ligne], 3/4, pp. 39-56
# http://www.persee.fr/web/revues/home/prescript/article/pica_0752-5656_2004_num_3_1_2396
Compiegne <- read.csv(url("https://raw.githubusercontent.com/JGravier/sitRada/master/Heatmap/Compiegne.csv"),
                      header = TRUE,
                      sep = ";",
                      stringsAsFactors = F)

# lignes : périodes d’occupation de l'opération archéologique, construites selon les observations de terrain (5 périodes),
# colonnes : types techniques de céramique (16 types)

row.names(Compiegne) <- Compiegne$Periode # on renomme les lignes avec la colonne Periode

Compiegne <- Compiegne %>%
  select(TypeA:TypeP) # on sélectionne uniquement les colonnes liées aux types de céramique

## note : l'intérêt de ce jeu de données est qu'il est lié à un article sur la sériation de Bruno
## les personnes présentes pourront donc aller le lire et mieux comprendre la complémentarité
## des traitements proposés par rapport à de la sériation de type sériographe EPPM (écarts positifs au pourcentage moyen)




## Autre jeu de données => une autre échelle d'étude & une autre manière de construire le tableau

# Données de mon travail de master & thèse sur la ville de Noyon (Oise)
# Tableau de contingence du nombre d’entités urbaines qui existent 
# par fonction urbaine dans le temps long de 2000 ans.
Noyon <- read.csv("Noyon_2000_50ansEUDCDE.csv", header = T,
                  sep = ";", stringsAsFactors = F)

# pas de temps chronologique arbitraire de 50 ans ([1;50], [51;100], [101;150]… jusqu’au début du 21e s.),
# fonctions urbaines définies selon l’adapatation du thésaurus du CNAU (voir métadonnées des données en open access : https://doi.org/10.6084/m9.figshare.5630326)

Noyon <- Noyon %>%
  unite(Dates, Debut, Fin, sep = "-", remove = FALSE) # on réunit les colonnes Début et Fin par un séparateur "-"
rownames(Noyon) <- Noyon$Dates # nom des lignes selon notre nouvelle colonne appelée "Dates"

# Voir pour plus de précision : GRAVIER J., 2015, « Recognizing Temporalities in Urban Units from a Functional 
# Approach: Three Case Studies », in GILIGNY F., DJINDJIAN F., COSTA L., MOSCATI P., ROBERT S. (éd.), 
# CAA 2014, 21st Century Archaeology, Concepts, Methods and Tools. Proceedings of the 42nd Annual Conference on Computer Applications and Quantitative Methods in Archaeology [en ligne], Oxford : Archaeopress, pp. 371-380
# http://hal.archives-ouvertes.fr/hal-01474197


# On importe ensuite quelques fonctions qui ont été écrites spécifiquement pour ce script
# en particulier pour simplifier le code et le rendre plus clair pour le lecteur
source("Fonctions_VisuClasses.R")

# attention : il faut que le "chemin" soit le même que votre projet R (ici le fichier est dans le même dossier)
# pour qu'il puisse être ouvert avec ce code









## Il faudrait préalablement aux traitements suivants (dans "Chaîne de traitement") :
## 1) ou bien expliquer uniquement avec le sériographe EPPM développé par Bruno ce que l'on obtient comme type de résultats
## 2) ou implémenter (voir ci-dessous) une visualisation sous forme de heatmap des écarts au pourcentage moyen (qu'ils soient positifs ou négatifs)

# Tout d'abord, avant de faire une analyse de type écart au pourcentage moyen, on teste le khi-2
## note : je reviendrai sur toutes ses notions dans un .ppt (j'en ai déjà fait des cours et pour être sincère
## les étudiants sont généralement appeurés, donc j'ai du pédagogique sous la main normalement)
Compiegne %>%
  chisq.test()

Ecart_PM_Compiegne <- EPM(Compiegne) # on applique une des fonctions réalisées pour le script
View(Ecart_PM_Compiegne)
# ce tableau se lit : "le TypeA de céramique a un écart positif de 20.53 % pour la période 5 par rapport à ce qui
# serait attendu dans une situation aléatoire", ce qui revient à dire (de manière plus usuelle)
# que cet écart très positif correspond à un lien signifiant entre ce type de céramique et cette période du site


# Heatmap
library(gplots)

my_palette2 <- colorRampPalette(c("#5e3c99", "#f7f7f7", "#e66101"))(n = 50)  # création de sa propre palette de couleur

Ecart_PM_CompiegneM <- as.matrix(Ecart_PM_Compiegne) # nécessaire matrice pour fonction heatmap.2
heatmap.2(Ecart_PM_CompiegneM, Rowv = FALSE, Colv = FALSE,
          col = my_palette2,
          denscol = "black",
          dendrogram = 'none',
          trace = 'none')
# on visualise que la période 5 est très associée au type A, et inversement que ce type n'est que très peu présent
# pour les périodes 1 et 2
# les cases qui tendent vers le blanc sont celles dont les valeurs du tableau des écarts au pourcentage moyen sont
# proches de 0 (cf. color key and histogram). Ce sont donc des valeurs proches d'une situation aléatoire.

## note : si l'on souhaite "découper" l'histogramme selon des classes que l'on considère signifiantes
## et donc représenter les cases du heatmap selon ses classes, c'est tout à fait possible,
## voir pour le code : https://rpubs.com/JGravier/243547
## toutefois, cela n'est pas forcément super pertinent car on a ici une distribution des écarts qui est symétrique


# On peut tester sans le type A d'un point de vue visuel pour mieux apprécier les différences entre les autres types
Ecart_PM_CompiegneM_sans_type_A <- Ecart_PM_Compiegne %>%
  select(-TypeA) %>%
  as.matrix()
heatmap.2(Ecart_PM_CompiegneM_sans_type_A, Rowv = FALSE, Colv = FALSE,
          col = my_palette2,
          denscol = "black",
          dendrogram = 'none',
          trace = 'none')





#### Chaine de traitement : CAH en distance khi-2 ####
# Application sur les données de Compiègne
# Objectifs : étudier les données de manière multi-dimensionnelle

## là il y a plusieurs possibilités :
## 1) où on implémente "en dur" les AFC
## 2) où l'on considère que ce n'est qu'une étape d'exploration des données (initialement le code était pensé comme ça)
## et l'on propose d'utiliser le package explor

library(shiny)
library(explor)
AFC_Compiegne <- dudi.coa(Compiegne,
                 scannf = FALSE,
                 nf = ncol(Compiegne))
explor(AFC_Compiegne)

# ooooooooooooooh le bel effet Guttman de sériation !




# Réalisation d'une classification ascendante hiérarchique (CAH) sur les coordonnées de l'AFC
# Dans ce cas : on a pris en compte tous les axes de l'AFC. Ce qui est intéressant car cela revient à dire
# que l'on a considéré toute l'information contenue dans le tableau de données initial
CAH_k2_Compiegne <- Compiegne %>%
  CAH_DistKhi2() # CAH sur les coordonnées de l'AFC (une des fonctions réalisée pour ce script)

# visualisation sous forme d'arbre de la CAH
plot(CAH_k2_Compiegne, hang = -1, cex = 0.6, 
     main = "Dendrogramme - La fouille des Halettes de Compiègne",
     xlab = "Périodes chronologiques")

# Quels sont les clusters de la CAH ?
# Pour y répondre, on va étudier de l'inertie de la CAH
# pour plus d'info : voir par exemple http://larmarange.github.io/analyse-R/classification-ascendante-hierarchique.html
inertie_Compiegne_k <- sort(CAH_k2_Compiegne$height, decreasing = TRUE)
plot(inertie_Compiegne_k, type = "s", xlab = "Nombre de classes", ylab = "Inertie")

inertie_Compiegne_k_part <- inertie_Compiegne_k/sum(inertie_Compiegne_k)*100
barplot(inertie_Compiegne_k_part[1:4], col = "#454847", border = "#454847", names.arg = seq(1, 4, 1),
        xlab = "Nombre de classes",
        ylab = "Part de l'inertie totale (%)")
# donc pour le présent cas, on peut découper en 2 l'arbre de la CAH

# Visuellement, cela correpond à :
plot(CAH_k2_Compiegne, hang = -1, cex = 0.6, 
     main = "Dendrogramme - La fouille des Halettes de Compiègne",
     xlab = "Périodes chronologiques",
     ylab = "")
rect.hclust(CAH_k2_Compiegne, 2, border = "firebrick")






# Au final, l'objectif est de présicer comment qualifier ces clusters (classes) chronologiques selon
# la surreprésentation ou la sous-représentation des types techniques de céramiques.

# Deux procédures sont possibles :
# 1) soit, on travaille directement sur le tableau des écarts à l'indépendance. Dans ce cas, on travaillera sur des écarts
# qui sont bruts ;
# 2) soit, on travaille sur ces tableau mais avec des écarts qui sont "standardisés". L'écart va alors mesurer l'attraction (écarts positifs)
# ou la répulsion (écarts négatifs) entre la période (i) et le type de céramique (j). Sous l'hypothèse d'absence de lien,
# ces écarts sont des statistiques centrées (= 0). Attention, ces écarts dits standardisés (généralement appelés résidus standardisés),
# sont centrées (en l'hypothèse H0 d'absence de relation) mais non réduite (leur variance étant < à 1, contrairement à une véritable standardisation).
# Dès lors, on peut s'intéresser aux valeurs extrêmes mais l'on ne peut pas émettre de seuil de significativité.


# on découpe préalablement notre arbre de la CAH en deux classes
Typochrono_Compiegne_k <- cutree(CAH_k2_Compiegne, k = 2)  # k = nombre de classes


### Première possibilité : tableau des écarts à l'indépendance bruts
Ecart_Compiegne <- Compiegne %>%
  TabEcart() %>% # tableau d'indépendance (fonction réalisée pour le script)
  mutate(Cluster = factor(Typochrono_Compiegne_k, levels = 1:2)) # ici, on crée une nouvelle colonne au tableau des écarts,
# appelée Cluster, où chaque période appartient à une des classes de la CAH (par ex : les P1 et P2 appartiennent à la même classe numéro 2)

rownames(Ecart_Compiegne) <- c("P5", "P4", "P3", "P2", "P1") # on renomme les lignes
write.csv(Ecart_Compiegne, "Ecart_Compiegne.csv") # on en fait une sortie sous forme de tableau


# On va ensuite calculer les moyennes des écarts à l'indépendance par classe (cluster)
Ecart_Compiegne <- Ecart_Compiegne %>%
  group_by(Cluster) # on regroupe les périodes par classe
Ecart_Compiegne <- Ecart_Compiegne %>%
  summarise_each(funs(mean)) # on fait la moyenne des écarts par classe
write.csv(Ecart_Compiegne, "Compiegne_EcartInde_Moyenne_de_classe.csv") # on en fait une sortie


# On va enfin faire le plot de sortie !
Sortie_finale_Compiegne <- Ecart_Compiegne %>%
  gather(key = Type, value = "Data", TypeA:TypeP) # on réagence le tableau pour faire la visualisation souhaitée

ggplot(Sortie_finale_Compiegne) +
  geom_bar(aes(Type, Data, fill = Cluster), stat = "identity") +
  labs(caption = "J. Gravier | UMR Géographie-cités 2017") +
  ggtitle("Compiègne : les types techniques de céramiques de la fouille des Halettes") +
  labs(subtitle = "Classe de périodes : CAH (distance chi-deux)") +
  xlab("") +
  ylab("Moyennes des écarts à l'indépendance par classe") +
  facet_wrap(~ Cluster) +
  coord_flip()


# ici 1 correspond au premier cluster qui rassemble les périodes 3, 4 et 5 ; et 2 au cluster qui regroupe les périodes
# 1 et 2 (d'où l'importance des sorties pour vérifier quelle période appartient à quel cluster).
# On observe par exemple que la moyenne des écarts du type P est tellement faible (= 6 pour classe 1) qu'il n'est pas ou peu visible
# sur le graphique. Cela implique que ce type de céramique n'est pas du tout signifiant pour
# distinguer les classes de période les unes des autres.
# On observe par contre que le type A contribue beaucoup à la distinction : très important dans la classe 1 et très
# faible en classe 2

# le problème quand on regarde les moyennes des écarts à l'indépendance tel quel, c'est qu'ils sont bruts.
# Ils sont donc dépendants des effectifs initiaux du tableau (les sommes des lignes et les sommes des colonnes). De fait, la moyenne va surtout
# mettre en avant les différences quantitatives initiales entre les variables (ici la quantité de céramique en poids selon les types)
# Il est donc plus judicieux de normaliser les écarts à l'indépendance, pour rendre comparables les variables


## Deuxième possibilité : en normalisant les écarts à l'indépendance (dits écarts ou résidus standardisés)
# Toutes périodes
Ecart_Compiegne_Norm <- Compiegne %>%
  TabEcartPearsonResidus() %>% # calculs des résidus de Pearson (fonction réalisée pour le script)
  mutate(Cluster = factor(Typochrono_Compiegne_k, levels = 1:2)) # on crée une colonne Cluster où chaque période appartient à
# une des classes liées à la CAH

rownames(Ecart_Compiegne_Norm) <-  c("P5", "P4", "P3", "P2", "P1")
write.csv(Ecart_Compiegne_Norm, "Ecart_Compiegne_Norm.csv")

Ecart_Compiegne_Norm <- Ecart_Compiegne_Norm %>%
  group_by(Cluster)
Ecart_Compiegne_Norm <- Ecart_Compiegne_Norm %>%
  summarise_each(funs(mean))
write.csv(Ecart_Compiegne_Norm, "Noyon_EcartIndeNorm_moyenne_classe.csv")

Sortie_finale_Compiegne_Norm <- Ecart_Compiegne_Norm %>%
  gather(key = Type, value = "Data", TypeA:TypeP)

ggplot(Sortie_finale_Compiegne_Norm) +
  geom_bar(aes(Type, Data, fill = Cluster), stat = "identity") +
  labs(caption = "J. Gravier | UMR Géographie-cités 2017") +
  ggtitle("Compiègne : les types techniques de céramiques de la fouille des Halettes") +
  labs(subtitle = "Classe de périodes : CAH (distance chi-deux)") +
  xlab("") +
  ylab("Moyennes des écarts standardisés par classe (résidus de Pearson)") +
  facet_wrap(~Cluster) +
  coord_flip()









# Application sur les données de Noyon
Noyon %>%
  select(F1:F10, -F7) %>%
  chisq.test()

CAH_k2_Noyon <- Noyon %>%
  select(F1:F10, -F7) %>%
  CAH_DistKhi2() # CAH sur les coordonnées de l'AFC
# Note : comme précédemment, nous conseillons de réaliser cela après avoir déjà
# manipulé ses données (en particulier ici, après avoir réalisé des AFC et explorer les différents axes)
plot(CAH_k2_Noyon, hang = -1, cex = 0.6, 
     main = "Dendrogramme - Noyon (1er-21e s.)",
     xlab = "Périodes chronologiques")

## Clustering type cutree de la CAH (on fait apparaître les clusters)
# inertie
inertieNk <- sort(CAH_k2_Noyon$height, decreasing = TRUE)
plot(inertieNk, type = "h", xlab = "Nombre de classes", ylab = "Inertie")

inertieN2k <- inertieNk/sum(inertieNk)*100
barplot(inertieN2k[1:15], col = "#454847", border = "#454847", names.arg = seq(1, 15, 1),
        xlab = "Nombre de classes",
        ylab = "Part de l'inertie totale (%)")
# donc pour le présent cas, on peut découper en 5


TypochronoNk <- cutree(CAH_k2_Noyon, k = 5)  # k = nombre de classes

# Tableau des écarts à l'indépendance
EcartNoyon <- Noyon %>%
  select(F1:F10, -F7) %>%
  TabEcart() %>% # cette fois-ci, on travaille sur un tableau des écarts à l'indépendance
  mutate(Cluster = factor(TypochronoNk, levels = 1:5))

write.csv(EcartNoyon, "Noyon_EcartInde.csv")

EcartNoyon <- EcartNoyon %>%
  group_by(Cluster)
EcartNoyon <- EcartNoyon %>%
  select(F1:F10) %>%
  summarise_each(funs(mean))
write.csv(EcartNoyon, "Noyon_EcartInde_meanCluster.csv")
EcartNoyon$Cluster <- c("1-250", "251-650", "651-1150", "1151-1800", "1801-2016")
EcartNoyon$Reorder <- c(1,2,3,4,5)

TabPlotk <- gather(EcartNoyon, key = Fonction, value = "Data", F1:F10)

ggplot(TabPlotk) +
  geom_bar(aes(Fonction, Data, fill = Cluster), stat = "identity") +
  labs(caption = "J. Gravier | UMR Géographie-cités 2017") +
  ggtitle("Noyon : Les fonctions urbaines (1er-21e s.)") +
  labs(subtitle = "Classe de périodes : CAH (distance khi 2)") +
  xlab("Fonctions urbaines") +
  ylab("Moyennes des écarts à l'indépendance par classe") +
  facet_wrap(~reorder(Cluster, Reorder)) +
  coord_flip()

# En normalisant les écarts à l'indépendance
# Toutes périodes
EcartNoyonNorm <- Noyon %>%
  select(F1:F10, -F7) %>%
  TabEcartPearsonResidus() %>% # Résidus de Pearson
  mutate(Cluster = factor(TypochronoNk, levels = 1:5))

write.csv(EcartNoyonNorm, "Noyon_EcartIndeNorm.csv")

EcartNoyonNorm <- EcartNoyonNorm %>%
  group_by(Cluster)
EcartNoyonNorm <- EcartNoyonNorm %>%
  select(F1:F10) %>%
  summarise_each(funs(mean))
write.csv(EcartNoyonNorm, "Noyon_EcartIndeNorm_meanCluster.csv")
EcartNoyonNorm$Cluster <- c("1-250", "251-650", "651-1150", "1151-1800", "1801-2016")
EcartNoyonNorm$Reorder <- c(1,2,3,4,5)

TabPlotkNorm <- gather(EcartNoyonNorm, key = Fonction, value = "Data", F1:F10)

ggplot(TabPlotkNorm) +
  geom_bar(aes(Fonction, Data, fill = Cluster), stat = "identity") +
  labs(caption = "J. Gravier | UMR Géographie-cités 2017") +
  ggtitle("Noyon : Les fonctions urbaines (1er-21e s.)") +
  labs(subtitle = "Classe de périodes : CAH (distance khi 2)") +
  xlab("Fonctions urbaines") +
  ylab("Moyennes des écarts standardisés par classe (résidus de Pearson)") +
  facet_wrap(~reorder(Cluster, Reorder)) +
  coord_flip()


