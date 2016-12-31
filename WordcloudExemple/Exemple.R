library(NLP)
library(tm)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)
library(bitops)
library(RCurl)
library(ggplot2)

# Récupération des données sous format wordcloud
SITRADA_URL = "https://cours.univ-paris1.fr/course/view.php?id=5437"
SITRADA <- rquery.wordcloud(SITRADA_URL, type ="url", lang = "french", 
                            colorPalette="RdBu", # choix couleur RColorBrewer
                            max.words = 50) # maximum de mot représentés

SITRADAFreq <- SITRADA$freqTable # table des effectifs des occurrences de termes
head(SITRADAFreq, 10) # montrer les 10 premiers

Graphiq <- ggplot(SITRADAFreq[1:10,], # on montre les dix premiers ici
                  aes(x = reorder(SITRADAFreq[1:10,]$word, -SITRADAFreq[1:10,]$freq), # réordonnement sinon classement par ordre alphabétique
                      y = SITRADAFreq[1:10,]$freq)) +
  geom_bar(stat = "identity") +
  ylab("Effectif") +
  xlab("Mots") +
  geom_text(aes(label = SITRADAFreq[1:10,]$freq), vjust=1.6, color="white", size=3) # ajout effectif dans geom_bar
Graphiq

SITRADA$tdm #TermDocumentMatrix
findFreqTerms(SITRADA$tdm, lowfreq = 5) # occurrence d'au moins 5 fois des termes
