# Fonction pour abtenir les écarts au pourcentage moyen (décomposition de la fonction pour comprendre toutes les étapes)
EPM <- function(x){
  x <- as.matrix(x) # transformation en format matriciel pour effectuer les calculs suivants
  x <- prop.table(x, margin = 1)*100  # on fait le tableau des fréquences des variables
  # c'est-à-dire : on regarde la fréquence de chaque variable (colonne) pour chaque ligne (période chronologique).
  # le total de chaque ligne est égal à 100 %
  # (on *100 pour obtenir la fréquence en pourcentage)
  ni. <- margin.table(x, margin = 1)  # totaux des lignes i (qui est = à 100 %)
  nj. <- margin.table(x, margin = 2)  # totaux des colonnes j (qui représente la part de chaque variable dans l'ensemble étudié)
  n. <- sum(ni.)  # effectif total
  # la suite : on aurait pu passer par la récupération de données à partir d'un khi 2,
  # on a ici choisi de décomposer la fonction de manière plus précise
  TabInde <- (round(nj.%*%t(ni.)/n., 2))  # tableau d'indépendance (<=> au pourcentage moyen)
  TabInde <- t(TabInde) # on transpose le tableau
  TabEcart <- x - TabInde  # tableau des écarts à l'indépendance (<=> écart au pourcentage moyen)
  TabEcart <- as.data.frame(TabEcart) # en sortie, on souhaite un data.frame
}

# Fonction pour abtenir les écarts à l'indépendance (décomposition de la fonction pour comprendre toutes les étapes)
TabEcart <- function(x){
  x <- as.matrix(x)
  ni. <- margin.table(x, margin = 1)  # totaux des lignes i
  nj. <- margin.table(x, margin = 2)  # totaux des colonnes j
  n. <- sum(ni.)  # effectif total
  TabInde <- (round(nj.%*%t(ni.)/n., 2))  # tableau d'indépendance
  TabInde <- t(TabInde)
  TabEcart <- x - TabInde  # tableau des écarts à l'indépendance
  TabEcart <- as.data.frame(TabEcart)
}

# Fonction pour abtenir les écarts standardisés
TabEcartPearsonResidus <- function(x){
  x <- as.matrix(x)
  x <- chisq.test(x)
  x <- x$residuals #  écarts de Pearson = écarts standardisés
  # <=> (tab observé - tab d'indépendance) / sqrt(d'indépendance)
  as.data.frame(x)
}

# Fonction pour abtenir une CAH pondérée à partir d'une matrice de distance khi-deux
CAH_DistKhi2 <- function(x){
  coa <- dudi.coa(x, scannf = FALSE, nf = ncol(x))  # AFC
  dist <- dist.dudi(coa)  # distance sur les coordonnées de l'AFC
  hclust(dist, method = "ward.D2", 
         members = apply(x, MARGIN = 1, FUN = sum))  # CAH sur ces distances, méthode Ward
  # on a pondéré la CAH selon les poids des lignes (d'après la méthode française mise en place par JP Benzécri)
}