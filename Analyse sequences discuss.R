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
# Supressions de toutes les tables intermédiaires pour alleger l'espace
rm(list = ls(pattern = "^data_"))


# ajouter une colonne "position_lien" qui pend les valeurs "début" pour le minimum de 
# la valeur de la variable "inner_id", et prend la valeur "fin" pour le maximum de la 
# valeur de la variable "inner_id", et prend la valeur "milieu" pour le reste

data <- data %>%
  group_by(publication) %>%
  mutate(position_lien = case_when(
    inner_id == min(inner_id) ~ "début",
    inner_id == max(inner_id) ~ "fin",
    TRUE ~ "milieu"
  )) %>%
  ungroup()

# Select des variables d'intérêt

df <- data %>%
  select(publication, typo, position_lien) %>%
  unique()


data_pivoted <- data %>%
  pivot_wider(
    names_from = position_lien,
    values_from = typo,
    names_prefix = "",
    values_fn = length, # count number of occurrences of each combination
    values_fill = 0
  ) %>%
  mutate(
    début = ifelse(début > 0, 1, 0),
    milieu = ifelse(milieu > 0, 1, 0),
    fin = ifelse(fin > 0, 1, 0)
  )


##
# Add row names to data_pivoted

# Join data and data_pivoted by row name
df <- left_join(data, data_pivoted) %>%
  select(publication, inner_id, typo, début, milieu,fin)

##

df <- df %>%
  group_by(publication, typo) %>%
  summarise(début_sum = sum(début),
            milieu_sum = sum(milieu),
            fin_sum = sum(fin))

df$typo <- factor(df$typo)

sum_df <- df %>%
  group_by(typo) %>%
  summarise(début_sum = sum(début_sum),
            milieu_sum = sum(milieu_sum),
            fin_sum = sum(fin_sum))

mean_df <- df %>%
  group_by(typo) %>%
  summarise(début_mean = mean(début_sum),
            milieu_mean = mean(milieu_sum),
            fin_mean = mean(fin_sum))



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




## Analyse profile des discussions en termes de commentaires ----

extract <- data %>%
  select (publication, annee, inner_id) %>%
  unique() %>%
  group_by(publication, annee) %>%
  summarise(nb_com = n_distinct(inner_id))
  
# Pivoter l'annee pour n'analyse des séquences
df_0 <- extract %>% 
  pivot_wider(names_from = annee, values_from = nb_com, values_fill = 0) %>%
  select(sort(colnames(.))) # modifier l'ordre des colonnes de l'annee la plus petite à la plus grande

df_0$disc_nbr <-  paste0("disc ", seq(1, nrow(df_0)))

# exculre 2 valeurs extrêmes, id_publication = 1728 et 2341
`%not_in%` <- purrr::negate(`%in%`)
df_0 <- subset(df_0, publication %not_in% c(57972, 60504, 63427, 73822))

df <- df_0[,1:9]
rownames(df) <- df_0$disc_nbr

df <- df %>% mutate(nb_tot_com = rowSums(.[1:9]))
df_prop <- df %>% mutate_all(.funs = list(~./nb_tot_com))
  
rownames(df_prop) <- df_0$disc_nbr


## ACP
library(ade4)

acp <- dudi.pca(df_prop[1:9], scannf = FALSE, nf = 100)
explor::explor(acp)

# calcul de la matrice des distances de Gower
library(cluster)
md <- dist.dudi(acp)
md_gower <- daisy(df_prop, metric = "gower")

# calcul du dendrogramme
arbre <- hclust(md, method = "ward.D2")
arbre_gower <- hclust(md_gower, method = "ward.D2")

# Représenter le dendrogramme
plot(arbre, labels = FALSE)
plot(arbre_gower, labels = FALSE)
rect.hclust(arbre, 2, border = "red")
rect.hclust(arbre, 5, border = "blue")

# Une façon plus visuelle de représenter le dendogramme
library(dendextend)
color_branches(arbre_gower, k = 5) %>% ggplot(labels = FALSE)

library(factoextra)
fviz_dend(arbre_gower, k = 6, show_labels = FALSE, rect = TRUE)

# saut d'inertie
inertie <- sort(arbre$height, decreasing = TRUE)
plot(inertie[1:20], type = "s")

inertie_gower <- sort(arbre_gower$height, decreasing = TRUE)
plot(inertie_gower[1:10], type = "s")

# source(url("https://raw.githubusercontent.com/larmarange/JLutils/master/R/clustering.R"))
# best.cutree(arbre_gower, graph = TRUE)
# best.cutree(arbre, graph = TRUE)

# déterminer le nombre de classes avec des indicateurs poussés
library(WeightedCluster)
as.clustrange(arbre, md) %>% plot()
as.clustrange(arbre_gower, md_gower) %>% plot()


# Caractériser les classes ----
# df$typo <- cutree(arbre, 5) # fonction cutree : apratenance de chaque observation à chaque classe (ne pas modifier l'ordre des observations dans les différents objets !!!)
df_prop$typo_gower <- cutree(arbre_gower, 4) # même chose pour gower

acp2 <- FactoMineR::PCA(df_prop, quanti.sup = 11)
explor::explor(acp2)

fviz_pca_ind(acp2,
             geom.ind = "point", # Montre les points seulement (mais pas le "text")
             col.ind = as.character(df_prop$typo_gower), # colorer by groups
             #palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = T, # Ellipses de concentration
             legend.title = "Groups"
)

##

df2 <- df_prop %>%
  group_by(typo_gower) %>%
  summarise("2013" = mean(`2013`),
            "2014" = mean(`2014`),
            "2015" = mean(`2015`),
            "2016" = mean(`2016`),
            "2017" = mean(`2017`),
            "2018" = mean(`2018`),
            "2019" = mean(`2019`),
            "2020" = mean(`2020`),
            "2021" = mean(`2021`))
rownames(df2) <- c("groupe 1", "groupe 2","groupe 3", "groupe 4","groupe 5", "groupe 6")

df_analyse <- subset(df_0, disc_nbr %in% c("disc 9180", "disc 2751", "disc 3956", "disc 6162", "disc 4672"))


acp3 <- dudi.pca(df2[2:10], scannf = F, nf = Inf)
explor::explor(acp3)
md3 <- dist.dudi(acp3)
md_gower3 <- daisy(df2, metric = "gower")

# calcul du dendrogramme
arbre3 <- hclust(md3, method = "ward.D2")
arbre_gower3 <- hclust(md_gower3, method = "ward.D2")

# Représenter le dendrogramme
plot(arbre3, labels = T)
plot(arbre_gower3, labels = F)

df %>%
  tbl_summary(
  by ="typo_gower"
  )


expl_g4 <- subset(df_0, disc_nbr %in% c("disc 9203", "disc 8103", "disc 8079", "disc 7754", "disc 8113"))
expl_g3 <- subset(df_0, disc_nbr %in% c("disc 5268", "disc 5473", "disc 5492", "disc 5192"))
expl_g2 <- subset(df_0, disc_nbr %in% c("disc 4049", "disc 4034", "disc 3991", "disc 3838"))
expl_g1 <- subset(df_0, disc_nbr %in% c("disc 3389", "disc 3387", "disc 3374", "disc 3304"))
expl_g1B <- subset(df_0, disc_nbr %in% c("disc 2752", "disc 2964", "disc 4891", "disc 2913", "disc 2787"))
expl <- subset(df_0, disc_nbr %in% c("disc 3020", "disc 9204", "disc 8113", "disc 9203"))


df_prop$disc <- df_0$disc_nbr


# quelques stats utiles

extract_date <- data %>%
  select (publication, date_comm, inner_id) %>%
  unique() %>%
  group_by(publication) %>%
  summarise(maxdate = max(date_comm),
            mindate = min(date_comm),
            diff_date = max(date_comm) - min(date_comm))







