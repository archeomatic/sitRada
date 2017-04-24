library(tidyverse)
library(ade4)
library(ggthemes)

# Import données
Noyon <- read.csv2("Noyon_2000_50ansEUDCDE.csv",
                   header = T,
                   sep = ";",
                   stringsAsFactors = F)
View(Noyon)

Noyon <- Noyon %>%
  unite(Dates, Debut, Fin, sep = "-", remove = FALSE)
rownames(Noyon) <- Noyon$Dates

# Import fonctions
source("Fonctions_VisuClasses.R")

#### Chaine de traitement 1 : CAH sur les fréquences standardisées des variables : cf. chap 7 R & Espace ####
# Application sur les données de Noyon
# Regardons les corrélations des foncitons
Noyon %>%
  select(F1:F10, -F7) %>% # On a enlevé F7 = "habitat", F11 = "Formations naturelles" et F12 = "Urbain non caractérisé"
  Frce() %>% # mise en fréquence tableau de données
  Standar() %>% # standardisation du tableau
  as.matrix() %>%
  pairs(upper.panel = panel.cor)

CAH_FS_Noyon <- Noyon %>%
  select(F1:F10, -F7) %>%
  Frce() %>%
  Standar() %>%
  CAH_Eucli() # CAH en distance euclidienne sur le tableau de données
# Note : avant d'appliquer cela, il nous semble important d'avoir préalablement
# traité ses données sous forme plus simple. Par exemple en Heatmap (voir : http://github.com/JGravier/sitRada/tree/master/Effets2Sources)

plot(CAH_FS_Noyon, hang = -1, cex = 0.6, 
     main = "Dendrogramme - Noyon (1er-21e s.)",
     xlab = "Périodes chronologiques")

inertieN <- sort(CAH_FS_Noyon$height, decreasing = TRUE)
plot(inertieN, type = "h", xlab = "Nombre de classes", ylab = "Inertie")

inertieN2 <- inertieN/sum(inertieN)*100
barplot(inertieN2[1:15], col = "#454847", border = "#454847", names.arg = seq(1, 15, 1),
        xlab = "Nombre de classes",
        ylab = "Part de l'inertie totale (%)")
# donc pour le présent cas, on peut découper en 5 classes je pense (choix du chercheur)

TypochronoN <- cutree(CAH_FS_Noyon, k = 5)  # k = nombre de classes

StandNoyon <- Noyon %>%
  select(F1:F10, -F7) %>%
  Frce() %>%
  Standar() %>%
  mutate(Cluster = factor(TypochronoN, levels = 1:5)) # nouvelle colonne avec appartenance des lignes i par classe
rownames(StandNoyon) <- Noyon$Dates
write.csv(StandNoyon, "Noyon_Stand.csv")

StandNoyon <- StandNoyon %>%
  group_by(Cluster)
StandNoyon <- StandNoyon %>%
  select(F1:F10) %>%
  summarise_each(funs(mean))
write.csv(StandNoyon, "Noyon_Stand_meanCluster.csv")
StandNoyon$Cluster <- c("1-250", "251-650","651-1000", "1001-1800", "1801-2016") # pour visualiser les ensemble chronologique dans la visualisation finale
StandNoyon$Reorder <- c(1,2,3,4,5) # nécessaire pour visualisation facet_wrap

TabPlot <- gather(StandNoyon, key = Fonction, value = "Data", F1:F10) # Tableau pour visualisation

ggplot(TabPlot) +
  geom_bar(aes(Fonction, Data, fill = Cluster), stat = "identity") +
  theme_julie() +
  scale_fill_tableau(palette = "colorblind10", breaks = NULL) +
  labs(caption = "J. Gravier | UMR Géographie-cités 2017") +
  ggtitle("Noyon : Les fonctions urbaines (1er-21e s.)") +
  labs(subtitle = "Classe de périodes : CAH (distance euclidienne)") +
  ylab("Moyennes des fréquences standardisées par classe") +
  facet_wrap(~reorder(Cluster, Reorder)) +
  coord_flip()


#### Chaine de traitement 2 : CAH en distance khi-2 ####
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
rownames(EcartNoyon) <- Noyon$Dates
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
  theme_julie() +
  scale_fill_tableau(palette = "colorblind10",  breaks = NULL) +
  labs(caption = "J. Gravier | UMR Géographie-cités 2017") +
  ggtitle("Noyon : Les fonctions urbaines (1er-21e s.)") +
  labs(subtitle = "Classe de périodes : CAH (distance khi 2)") +
  ylab("Moyennes des écarts à l'indépendance par classe") +
  facet_wrap(~reorder(Cluster, Reorder)) +
  coord_flip()

# En normalisant les écarts à l'indépendance
# Toutes périodes
EcartNoyonNorm <- Noyon %>%
  select(F1:F10, -F7) %>%
  TabEcartPearsonResidus() %>% # Résidus de Pearson
  mutate(Cluster = factor(TypochronoNk, levels = 1:5))
rownames(EcartNoyonNorm) <- Noyon$Dates
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
  theme_julie() +
  scale_fill_tableau(palette = "colorblind10",  breaks = NULL) +
  labs(caption = "J. Gravier | UMR Géographie-cités 2017") +
  ggtitle("Noyon : Les fonctions urbaines (1er-21e s.)") +
  labs(subtitle = "Classe de périodes : CAH (distance khi 2)") +
  ylab("Moyennes des écarts standardisés par classe (résidus de Pearson)") +
  facet_wrap(~reorder(Cluster, Reorder)) +
  coord_flip()
