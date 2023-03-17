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
data <- subset(data_max_sequence, max_sequence>1 & data_max_sequence$nb_comm>1)


# Calcul de la position des liens par rapport aux commentaires
data <- data %>% 
  group_by(publication) %>% 
  mutate(nb_comm = n_distinct(inner_id),
         position_comm = match(inner_id, unique(inner_id))/nb_comm)


# Distribution du nombre de liens par nombre de commentaires
data_count <- data %>%
  group_by(nb_comm) %>%
  summarise(count = n_distinct(urls))




# Supressions de toutes les tables intermédiaires pour alleger l'espace
rm(list = ls(pattern = "^data_"))


# Exporter les données pour envoyer aux collègues ----
write_excel_csv2(data_max_sequence, "/Users/maddi/Documents/Pubpeer project/Pubpeer explo/donnees_URLS.csv")
# Calcul de la fréquence des sites pour avoir une idée plus précise
f <- factor(data_max_sequence$domain[data_max_sequence$typo == "Autre"]) |>
  fct_infreq() |> 
  questionr::freq()
frequAutre <- data.frame(rownames(f),f[,1:2])
names(frequAutre) = c("site","Nombre_de_liens","Part_dans_non_classe")
write.xlsx(frequAutre, "/Users/maddi/Documents/Pubpeer project/Pubpeer explo/URLS_non_classees.xlsx")


## Cutting data_comm_url$nb_comm into data_comm_url$nb_comm_rec
data$nb_comm_rec <- cut(data$nb_comm,
                                 include.lowest = TRUE,
                                 right = FALSE,
                                 dig.lab = 4,
                                 breaks = c(2, 4, 10, 291)
)

# Analyse des séquences ----
data_uniq <- data[data$nb_comm_rec == "[10,291]", c("inner_id", "typo", "annee_rec", "position_comm")] %>% 
  unique()
  
  
df <- aggregate(position ~ typo + annee_rec, data = data, mean) 
df <- aggregate(position_comm ~ typo + annee_rec, data = data_uniq, mean) 

# Transformer les moyennes en quartiles
df_quartiles <- df %>%
  mutate(quartile = ntile(position_comm, 4))


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


