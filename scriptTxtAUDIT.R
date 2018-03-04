#Installer les packages

library(readxl)
library(data.table)
library(ggplot2)
library(dplyr)
library(topicmodels)
library(tm)
library(SnowballC)
library(NLP)

#importer le DataSet
data <- read.csv("C:/Users/SKoukiAmri/Desktop/TxtAUDIT.csv" , sep=";")

#Récupération du texte par ligne 
n_ligne <- 1

text <- paste(as.character(data$Criteria[1:126]),
              as.character(data$Explanatory.Note[1:126]),
              as.character(data$Tests.for.Auditors[1:126]),
              as.character(data$Examples.of.Best.Practices[1:126]),sep = " ")

View(text)

#Charger les données dans un corpus
docs <- Corpus(VectorSource(text))
#nettoyage du texte 

clean <- function (docs)
{
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, content_transformer(tolower))  #miniscule
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, removeWords, c("check", "company","workers","employees","procedure","use","local","ensure","products","interviw","management","procedures","used","work","written","system","plan","equipment","may","taken","ask","within","can","activity","chanel","person","plans","activities","review","well","also")) 
return(docs)
}

treat <- function(docs)
{
  dtm <- TermDocumentMatrix(docs)
  
  return(dtm)
} 

#toSpace <- content_transformer(function (x, pattern ) gsub(pattern, " ", x))
#docs <- tm_map(docs, toSpace, "/")
#docs <- tm_map(docs, toSpace, "\n")
#docs <- tm_map(docs, toSpace, "*")

inspect(docs)

#Construction de la matrice dtm (matrice qui contient les mots restants ainsi que leurs fréquences)
dtm <- TermDocumentMatrix(docs)

m <- as.matrix(dtm)
View(m)
v <- sort(rowSums(m),decreasing = TRUE)
View(v)
d <- data.frame(word = names(v) ,freq=v)
head(d, 100)

#Faire un Test sur une observation en INUPUT
for (i in 1:126)
  
{
  l <- rownames (m[,i])
}
test <- "Sarra loves diamonds on her bag"
testm <- VCorpus(VectorSource(test))     # définition d'un Vcorpus pour le texte 
testmv <- treat(clean(testm))            # nettoyage du texte en utilisant les fonctions précédentes 
m_test <- as.matrix(testmv)              # construction de la matrice des fréquences pour voir s'il y a des mots qui se répétent 
m_test_mc <- rownames(m_test)            #récupérer les mots "clés" du texte
View(m_test_mc)
# calcul des pourcentages d'appartenance observation-dataset 
# s = l'ensemble des mots qui appartiennent à la fois à l'échantillon test et les mots clés de la colonne j de la matrice m 

nb_list <- rep(0,126)
s <- rep(99,126)
t_mach <- rep(0,126)
for (j in 1:126)
{ s[j]<-0
for (i in 1:length(rownames(m[which(m[,j] != 0),])))
{
  s[j] <- s[j]+ m_test_mc[i] %in% rownames(m[which(m[,j] != 0),])
}
nb_list[j] <- length(rownames(m[which(m[,j] != 0),]))
t_mach[j] <- s[j]/length(rownames(m[which(m[,j] != 0),]))
}
table_corres <- data.table(s,nb_list,t_mach)
View(table_corres)

affich <- function (corres)
{ 
  corres1 <- data.table(corres,index = seq(1:nrow(corres)))
  corres1 <- head(corres1[s != 0],5)
  corres1 <- corres1[order(s,decreasing = T)][order(t_mach,decreasing = T)]
  return(data[corres1$index,]$Criteria)
}

resultat_final <- affich(table_corres)
resultat_final
View(resultat_final)







