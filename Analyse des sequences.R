rm(list = ls()) #supprimer tous les objets 


library(tidyverse)
library(questionr)
# library(RPostgres)
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
library(gtsummary)
library(questionr)
library(openxlsx)
# Connexion ----

con<-dbConnect(RPostgres::Postgres())

db <- 'SKEPTISCIENCE'  #provide the name of your db
host_db <- 'localhost' # server
db_port <- '5433'  # port DBA
db_user <- 'postgres' # nom utilisateur  
db_password <- '*******'
con <- dbConnect(RPostgres::Postgres(), dbname = db, host=host_db, port=db_port, user=db_user, password=db_password) 
# Test connexion
dbListTables(con) 

### Récupération des données ----

reqsql2= paste('select * from data_urls_comm')
data_urls = dbGetQuery(con,reqsql2)
# en local :
data_urls <- readxl::read_excel("D:/bdd pubpeer/data_urls.xlsx")

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
# Analyse de la distribution du nombre de liens par publication
nb_comm_pub <- data_max_sequence %>%
  select(publication, inner_id) %>%
  #subset(., max_sequence>1) %>%
  unique()

# Analyse de la distribution du nombre de liens par publication
nb_urls_pub <- data_max_sequence %>%
  select(publication, max_sequence) %>%
  #subset(., max_sequence>1) %>%
  unique()
# summary
summary(nb_urls_pub$max_sequence)
# gtsummary
theme_gtsummary_language(language = "fr", decimal.mark = ",", big.mark = " ")
tbl_summary(nb_urls_pub,
            include = c(max_sequence))
# theme mean sd
theme_gtsummary_mean_sd()
tbl_summary(nb_urls_pub,
            include = c(max_sequence))

# describe
f <- factor(nb_urls_pub$max_sequence) |>
  fct_infreq() |> 
  questionr::freq()
freqsit <- data.frame(rownames(f),f)
names(freqsit) <- c("Nombre de liens dans les commentaires", "Nombre de publications", "Part")
write.xlsx(freqsit, "D:/Analyse/Stats/distrib nb liens pub.xlsx")


# Transformer les moyennes en quartiles
df_quartiles <- df %>%
  mutate(quartile = ntile(position, 4))

# Pivoter l'annee pour n'analyse des séquences
df_pivot <- df_quartiles[,c(1,2,4)] %>% 
  pivot_wider(names_from = annee_rec, values_from = quartile, values_fill = 0)

###
# Convertir les données en format de séquence
sequences <- as.matrix(df_pivot[,2:4])
# Supprimer les valeurs manquantes
sequences[is.na(sequences)] <- "-"

# définir les lables pour les différents états
labels <- c("Q1", "Q2", "Q3", "Q4")
seq <- seqdef(sequences, states = labels)
couts <- seqsubm(seq, method = "CONSTANT", cval = 2)
seq.om <- seqdist(seq, method = "OM", indel = 1, sm = couts)
seq.dist <- hclust(as.dist(seq.om), method = "ward.D2")
plot(as.dendrogram(seq.dist), leaflab = "none")
plot(sort(seq.dist$height, decreasing = TRUE)[1:13], type = "s", xlab = "nb de classes", ylab = "inertie")

nbcl <- 6
seq.part <- cutree(seq.dist, nbcl)
seq.part <- factor(seq.part, labels = paste("classe", 1:nbcl, sep = "."))

seqdplot(seq, group = seq.part, xtlab = c("2013-2015", "2016-2018","2019-2021"), border = NA)
seqIplot(seq, group = seq.part, xtlab = c("2013-2015", "2016-2018","2019-2021"), space = 0, border = NA, yaxis = FALSE)
seq_heatmap(seq, seq.dist, labCol = c("2013-2015", "2016-2018","2019-2021"), cexCol = 0.9)

seqfplot(seq, group = seq.part)
seqmsplot(seq, group = seq.part, xtlab = c("2013-2015", "2016-2018","2019-2021"), main = "")
seqmtplot(seq, group = seq.part)
seqrplot(seq, group = seq.part, dist.matrix = seq.om, criterion = "dist")
seqHtplot(seq, group = seq.part, xtlab = c("2013-2015", "2016-2018","2019-2021"))

## ACP
df_pivot <- mutate_if(df_pivot, is.integer, as.numeric)
row.names(df_pivot) <- df_pivot$typo

t <- df_pivot[,2:4] %>%
  as.data.frame()
row.names(t) <- df_pivot$typo
str(t)

res.pca <- PCA(t)
explor::explor(res.pca)



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


