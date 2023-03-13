rm(list = ls()) #supprimer tous les objets 


library(tidyverse)
library(questionr)
library(RPostgres)
library(lubridate)
library(urltools)
library(TraMineR)
library(cluster)
library(seqhandbook)
library(ade4)
library(explor)
library(FactoMineR)
library(factoextra)
library(labelled)

# Connexion ----

con<-dbConnect(RPostgres::Postgres())

db <- 'SKEPTISCIENCE'  #provide the name of your db
host_db <- 'localhost' # server
db_port <- '5433'  # port DBA
db_user <- 'postgres' # nom utilisateur  
db_password <- 'Maroua1912'
con <- dbConnect(RPostgres::Postgres(), dbname = db, host=host_db, port=db_port, user=db_user, password=db_password) 
# Test connexion
dbListTables(con) 

### Récupération des données ----

reqsql= paste('select inner_id, publication, "DateCreated" as date_com, html as comm from data_commentaires')
data_comm = dbGetQuery(con,reqsql)

reqsql2= paste('select * from data_urls_comm')
data_urls = dbGetQuery(con,reqsql2)
df <- select(data_urls, c("comm", "date_com.y", "annee", "inner_id", "publication", "urls", "scheme", "domain", "port", "path", "parameter", "fragment", "sequence", "typo"))
var_label(df) <- c("commentaire", "date du commentaire", "année du commentaire", "identifiant du commentaire", "identifiant de la publication", 
                   "urls entiers", "schéma", "domaine", "port", "chemin", "filtres appliqués", "fragment", "séquence", "Typologie")

## Recuperation de la date
data_urls <- merge(data_urls, data_comm, by = c("inner_id", "publication"), all.x = TRUE)

# Transformer le format de la date du commentaire
data_urls$annee <- strptime(data_urls$date_com.y, format = "%d/%m/%Y %H:%M") %>%
  year() + 2000
# Pour 3 publications la date n'est pas bien formatée. Correction :
# Changer la valeur de la colonne ville
data_urls <- data_urls %>% mutate(annee = case_when(
  annee == 4019 ~ 2019,
  TRUE ~ annee
))


# Transformer le format de la date du commentaire
data_comm$annee <- strptime(data_comm$date_com, format = "%d/%m/%Y %H:%M") %>%
  year() + 2000
# Pour 3 publications la date n'est pas bien formatée. Correction :
# Changer la valeur de la colonne ville
data_comm <- data_comm %>% mutate(annee = case_when(
  annee == 4019 ~ 2019,
  TRUE ~ annee
))

## Quelques résultats bizarres
# freq(data_urls$annee, total = T)
# freq(data_comm$annee, total = T)
# a <- freq(data_urls$date_com.x, total = T) 
# b <- data.frame(row.names(a),a)
  
# # En 2019 il y a 17442 commentaites, mais il n'y a que 329 liens !! Faires quelques vérifications
# comm2019 <- data_comm$comm[data_comm$annee==2019]
# urls2019 <- str_extract_all(comm2019, "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+")
# urls2019 <- unlist(urls2019)
# urls2019
# # J'obtiens 630 URLs avec beaucoup de doublons. Ce qui laisse entendre qu'effectivement, en 2019, il y a moins de renvoi à des URLs
# # La raison est qu'il y a beaucoup de commentaires vides
# comm2019NV <- comm2019[nzchar(comm2019)]
# # Sur les 17442 commentaires, il y a  seulement 1937 qui ne sont pas vides !!
# 
# comm2020 <- data_comm$comm[data_comm$annee==2020]
# urls2020 <- str_extract_all(comm2020, "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+")
# urls2020 <- unlist(urls2020)
# urls2020
# J'obtiens 41646 URLs avec beaucoup de doublons. Ce qui laisse entendre qu'effectivement, en 2019, il y a moins de renvoi à des URLs

# data_comm_non_vide <- subset(data_comm, nzchar(data_comm$comm))
# au fait il y a 160,922 commentaires non vides sur 198,426, soit 81%


## Préparation des données ----
data_urls$sequence <- as.numeric(data_urls$sequence)
data_urls$publication <- as.numeric(data_urls$publication)


# Fonction pour calculer la position de chaque valeur
calculer_position <- function(x) {
  position <- seq_along(x)
  position / length(x)
}

# Regrouper les données par identifiant de publication
data_urls_grouped <- data_urls %>% group_by(publication)

# Ajouter une nouvelle colonne avec la position de chaque élément dans la séquence
data_urls_position <- data_urls_grouped %>% mutate(position = calculer_position(sequence))

# Ajouter une colonne avec la valeur maximale de "sequence" pour chaque "id" : cela correspond au nombre de liens par publication
data_max_sequence <- data_urls_position %>% 
  group_by(publication) %>% 
  mutate(max_sequence = max(sequence))


## Recoding data_max_sequence$annee into data_max_sequence$annee_rec
data_max_sequence$annee_rec <- data_max_sequence$annee %>%
  as.character() %>%
  fct_recode(
    "2013-15" = "2013",
    "2013-15" = "2014",
    "2013-15" = "2015",
    "2016-18" = "2016",
    "2016-18" = "2017",
    "2016-18" = "2018",
    "2019-21" = "2019",
    "2019-21" = "2020",
    "2019-21" = "2021"
  )


# utiliser la fonction aggregate pour calculer la moyenne, grou by domain et annee
df <- aggregate(position ~ typo + annee_rec, data = subset(data_max_sequence, max_sequence>1), mean) # se limiter aux publications avec au moins 2 liens 

# Transformer les moyennes en quartiles
df_quartiles <- df %>%
  mutate(quartile = ntile(position, 4))

# Pivoter l'annee pour n'analyse des séquences
df_pivot <- df_quartiles[,c(1,2,4)] %>% 
  pivot_wider(names_from = annee_rec, values_from = quartile, values_fill = 0)


#labels <- c("Q1", "Q2", "Q3", "Q4")
seq <- seqdef(df_pivot)
couts <- seqsubm(seq, method = "CONSTANT", cval = 2)
seq.om <- seqdist(seq, method = "OM", indel = 1, sm = couts)
seq.dist <- hclust(as.dist(seq.om), method = "ward.D2")
plot(as.dendrogram(seq.dist), leaflab = "none")
plot(sort(seq.dist$height, decreasing = TRUE)[1:20], type = "s", xlab = "nb de classes", ylab = "inertie")

nbcl <- 4
seq.part <- cutree(seq.dist, nbcl)
seq.part <- factor(seq.part, labels = paste("classe", 1:nbcl, sep = "."))

seqdplot(seq, group = seq.part, xtlab = 13:21, border = NA)
seqIplot(seq, group = seq.part, xtlab = 14:50, space = 0, border = NA, yaxis = FALSE)

seq_heatmap(seq, seq.dist, labCol = 13:50)
seqmsplot(seq, group = seq.part, xtlab = 14:50, main = "classe")
seqmtplot(seq, group = seq.part)
seqHtplot(seq, group = seq.part, xtlab = 14:50)

freq(seq.part)

## ACP
df_pivot <- mutate_if(df_pivot, is.integer, as.numeric)

row.names(df_pivot) <- df_pivot$typo

t <- df_pivot[,2:4] %>%
  as.data.frame()
row.names(t) <- df_pivot$typo
str(t)

res.pca <- PCA(t)

ind <- get_pca_ind(res.pca)
# Coordonnées des individus
head(ind$coord)
# Qualité des individus
head(ind$cos2)
# Contributions des individus
head(ind$contrib)

fviz_pca_ind (res.pca, col.ind = "cos2",
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE # Évite le chevauchement de texte
)


fviz_pca_ind (res.pca, pointsize = "cos2",
              pointshape = 21, fill = "#E7B800",
              repel = TRUE # Évite le chevauchement de texte
)



res.hcpc <- HCPC(res.pca, graph = FALSE)
plot(res.hcpc, choice = "3D.map")

