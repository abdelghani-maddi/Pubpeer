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
library(openxlsx)
library(officer)
library(gtsummary)
library(data.table)
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

reqsql2= paste('select * from data_urls_comm')
data_urls = dbGetQuery(con,reqsql2)


## Préparation des données ----
data_urls$sequence <- as.numeric(data_urls$sequence)
data_urls$publication <- as.numeric(data_urls$publication)


# Fonction pour calculer la position de chaque url
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

# Ajouter une colonne avec la valeur maximale de "inner_id" pour chaque "publication" : cela correspond au nombre de commentaires (avec au moins un lien) par publication
data_max_sequence <- data_max_sequence %>% 
  group_by(publication, inner_id) %>% 
  summarise(nb_comm = n_distinct(inner_id)) %>% 
  group_by(publication) %>% 
  summarise(nb_comm = sum(nb_comm)) %>% 
  right_join(data_max_sequence, by = "publication")


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


# utiliser la fonction aggregate pour calculer la moyenne, group by domain et annee
data <- subset(data_max_sequence, max_sequence>1 & data_max_sequence$nb_comm>2)


# Calcul de la position des liens par rapport aux commentaires
data <- data %>% 
  group_by(publication) %>% 
  mutate(nb_comm = n_distinct(inner_id),
         position_comm = match(inner_id, unique(inner_id))/nb_comm)


## Cutting data_comm_url$nb_comm into data_comm_url$nb_comm_rec
data$nb_comm_rec <- cut(data$nb_comm,
                        include.lowest = TRUE,
                        right = FALSE,
                        dig.lab = 4,
                        breaks = c(2, 4, 10, 291)
)

# Regrouper les données par identifiant "publication" et calculer les quartiles
data <- data %>%
  #subset(., nb_comm_rec == "[10,291]") %>%
  #select(publication, typo, annee_rec, position_comm) %>%
  #unique() %>%
  group_by(publication) %>%
  mutate(quartile_discuss = case_when(ntile(position_comm, 4) == 1 ~ "Q1",
                               ntile(position_comm, 4) == 2 ~ "Q2",
                               ntile(position_comm, 4) == 3 ~ "Q3",
                               ntile(position_comm, 4) == 4 ~ "Q4"))

# Calculer la position de chaque quartile en fonction de son ordre d'apparition dans chaque groupe de données "publication"
data <- data %>%
  group_by(publication) %>%
  mutate(rank_discuss = dense_rank(quartile_discuss),
         position = 1:n())

# Calculer la position de chaque valeur de la colonne "typo" en fonction de la colonne "rank_discuss"
data <- data %>%
  group_by(publication, rank_discuss) %>%
  mutate(position_typo = 1:n(),
         quartile_position_typo = case_when(ntile(position_typo, 4) == 1 ~ "Q1",
                                            ntile(position_typo, 4) == 2 ~ "Q2",
                                            ntile(position_typo, 4) == 3 ~ "Q3",
                                            ntile(position_typo, 4) == 4 ~ "Q4"))



# Calculer la moyenne de la colonne "position_comm" en fonction des colonnes "typo" et "annee_rec"
moyennes <- aggregate(data_quartile$position_comm, by = list(data_quartile$typo, data_quartile$annee_rec), mean)
# Calculer la médiane de la colonne "position_comm" en fonction des colonnes "typo" et "annee_rec"
mediane <- aggregate(data_quartile$position_comm, by = list(data_quartile$typo, data_quartile$annee_rec), median)
# Calculer le min de la colonne "position_comm" en fonction des colonnes "typo" et "annee_rec"
min <- aggregate(data_quartile$position_comm, by = list(data_quartile$typo, data_quartile$annee_rec), min)
# Calculer le max de la colonne "position_comm" en fonction des colonnes "typo" et "annee_rec"
max <- aggregate(data_quartile$position_comm, by = list(data_quartile$typo, data_quartile$annee_rec), max)
# Distribution du nombre de liens par nombre de commentaires
data_count <- data %>%
  group_by(nb_comm) %>%
  summarise(count = n_distinct(urls))

# Donner des noms significatifs aux colonnes du résultat
colnames(moyennes) <- c("typo", "annee_rec", "moy_position_comm")

# Afficher le résultat
moyennes



# Supressions de toutes les tables intermédiaires pour alleger l'espace
rm(list = ls(pattern = "^data_"))


# Exporter les données pour envoyer aux collègues ----
write_excel_csv2(data, "/Users/maddi/Documents/Pubpeer project/Pubpeer explo/donnees_URLS.csv")
# Calcul de la fréquence des sites pour avoir une idée plus précise
f <- factor(data$domain[data$typo == "Autre"]) |>
  fct_infreq() |> 
  questionr::freq()
frequAutre <- data.frame(rownames(f),f[,1:2])
names(frequAutre) = c("site","Nombre_de_liens","Part_dans_non_classe")
write.xlsx(frequAutre, "/Users/maddi/Documents/Pubpeer project/Pubpeer explo/URLS_non_classees.xlsx")

# Calcul de la fréquence des sites pour avoir une idée plus précise
f_media <- factor(data$domain[data$typo == "Médias"]) |>
  fct_infreq() |> 
  questionr::freq()
frequ_media <- data.frame(rownames(f_media),f_media[,1:2])
names(frequ_media) = c("site","Nombre_de_liens","Part_dans_medias")
write.xlsx(frequ_media, "/Users/maddi/Documents/Pubpeer project/Pubpeer explo/URLS_media.xlsx")


f_nb_com <- factor(data$nb_comm) |>
  fct_infreq() |> 
  questionr::freq()

# Analyse des séquences ----
data_uniq <- data[data$nb_comm_rec == "[10,291]", c("inner_id", "typo", "annee_rec", "position_comm")] %>% 
  unique()
  
  
df <- aggregate(position ~ typo + annee_rec, data = data, mean) 
df <- aggregate(position_comm ~ typo + annee_rec, data = data_uniq, mean) 

# # Transformer les moyennes en quartiles
# df_quartiles <- df %>%
#   mutate(quartile = ntile(position_comm, 4))


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

nbcl <- 4
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


##

## ACP
df_pivot <- mutate_if(df_pivot, is.integer, as.numeric)
row.names(df_pivot) <- df_pivot$typo

t <- df_pivot[,2:4] %>%
  as.data.frame()
row.names(t) <- df_pivot$typo
str(t)

res.pca <- PCA(t)
# explor::explor(res.pca)


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


