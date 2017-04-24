# Theme perso pour les ggplots
theme_julie <- function(){
  julie <- theme_igray() +
    theme(plot.subtitle = element_text(face = "italic"), 
          plot.caption = element_text(size = 8), 
          axis.title = element_text(size = 12), 
          axis.text = element_text(size = 9.5), 
          plot.title = element_text(size = 15, 
                                    face = "bold", hjust = 0.5), legend.text = element_text(size = 11.5), 
          legend.title = element_text(size = 11.7), 
          plot.background = element_rect(fill = "gray97"), 
          legend.key = element_rect(fill = "gray97"), 
          legend.background = element_rect(fill = "gray97"), 
          legend.position = "bottom", legend.direction = "horizontal")
  julie
}

# calcul 
EPM <- function(x){
  x <- as.matrix(x)
  x <- prop.table(x, margin = 1)*100  # *100 pour obtenir la fréquence en pourcentage
  ni. <- margin.table(x, margin = 1)  # totaux des lignes i
  nj. <- margin.table(x, margin = 2)  # totaux des colonnes j
  n. <- sum(ni.)  # effectif total
  TabInde <- (round(nj.%*%t(ni.)/n., 2))  # tableau d'indépendance
  TabInde <- t(TabInde)
  TabEcart <- x - TabInde  # tableau des écarts à l'indépendance
  TabEcart <- as.data.frame(TabEcart)
}

Frce <- function(x){
  x <- as.matrix(x)
  x <- prop.table(x, margin = 1)*100  # mise en fréquence des variables (chaque i = 100%)
  x <- as.data.frame(x)
}

Standar <- function(x){
  ACP <- dudi.pca(x, scannf = FALSE, nf = ncol(x))
  # pas fonction scale, mais dudi.pca car ici la variance est exprimée en 1/n (et non 1/n-1)
  Stand <- ACP$tab
}

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

TabEcartPearsonResidus <- function(x){
  x <- as.matrix(x)
  x <- chisq.test(x)
  x <- x$residuals # résidus de Pearson = résidus standardisés
  # <=> (observed - expected) / sqrt(expected)
  as.data.frame(x)
}

TabEcartHabermanResidus <- function(x){
  x <- as.matrix(x)
  x <- chisq.test(x)
  x <- x$stdres # résidus de Haberman = résidus ajustés
  # <=> (observed - expected) / sqrt(V), where V is the residual cell variance
  as.data.frame(x)
}

CAH_Eucli <- function(x){
  dist <- dist(x, method = "euclidean")
  hclust(dist, method = "ward.D2")
}

CAH_DistKhi2 <- function(x){
  coa <- dudi.coa(x, scannf = FALSE, nf = ncol(x))  # AFC
  dist <- dist.dudi(coa)  # distance sur les coordonnées de l'AFC
  hclust(dist, method = "ward.D2", 
         members = apply(x, MARGIN = 1, FUN = sum))  # CAH sur ces distances, méthode Ward
}

## visualisation pour pairs
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