## Formatage espace de travail ----
rm(list = ls()) #supprimer tous les objets 

# chargement des packages ----
library(stringr)
library(tibble)
library(tidytext)
library(textdata)
library(Hmisc)
library(zoo)
library(flextable)
library(DBI)
library(data.table)
library(tidyverse)
library(trimmer)
library(DescTools)
library(questionr)
library(RPostgres)
library(lubridate)
library(timechange)
library(urltools)
library(stringr)
library(rebus)
library(Matrix)
library(plyr)
library(sjmisc)
library(regexplain)
library(gtsummary)
library(igraph)

# install.packages('remotes')
# remotes::install_github("gadenbuie/regexplain")

# Dossier travail
setwd('/Users/maddi/Documents/Pubpeer project/Pubpeer')


# Connexion ----

con<-dbConnect(RPostgres::Postgres())

db <- 'SKEPTISCIENCE'  #provide the name of your db
host_db <- 'localhost' # server
db_port <- '5433'  # port DBA
db_user <- 'postgres' # nom utilisateur  
db_password <- '********'
con <- dbConnect(RPostgres::Postgres(), dbname = db, host=host_db, port=db_port, user=db_user, password=db_password)  
# Test connexion
dbListTables(con) 

### Récupération des données ----

reqsql= paste('select inner_id, publication, html as comm from data_commentaires')
data_comm = dbGetQuery(con,reqsql)

### Récupération des commentaires avec liens hypertextes ----
# Creer la liste des liens qui se trouvent dans les commentaires gardant 
# le lien avec les publications à partir desquelles sont extraits

#### Etape 0 : Transformer le type de données pour plus de facilité/performance dans le traitement ----
URL_var <- as_tibble(data_comm) %>% # Etape 0
#### Etape 1 : Se limiter aux commentaires avec au moins un lien hypertexte  ----
  subset(., comm %like% c("%https%","%http%","%www%","%WWW%")) %>% # Etape 1
#### Etape 2 : Recupérer uniquement les commentaires (l'intérêt de garder le subset est de pouvoir selectionner par la suite les disciplines aussi) ----
  .$comm # Etape 2

#### Etape 3 : nommer le vecteur avec l'identifiant des publications ----
  names(URL_var) <- subset(data_comm$publication, data_comm$comm %like% c("%https%","%http%","%www%","%WWW%")) # Etape 3

#### Etape 4 : Créer un pattern pour l'extraction des URL ----
pat<- "//" %R% capture(one_or_more(char_class(WRD,DOT))) 
#### Etape 5 : Utiliser "rebus" pour extraire l'URL principal ----
URL_extract<-str_extract_all(URL_var, "(?<=//)[^\\s/:]+") #URL_extract<-str_match_all(URL_var, pattern = pat) 

# Attribuer aux liens, les identifiants des publications d'où ils sont issus ----
names(URL_extract) <- names(URL_var)

# Transformer en liste de dataframe
list_data <- Map(as.data.frame, URL_extract) %>%
  rbindlist(., use.names = F, idcol = names(.))
names(list_data) = c("publication", "site")

df = data.frame(list_data$publication, factor(list_data$site))
names(df) = c("publication", "site")

# list_data[list_data$publication == "106541"] -- petit test -- OK :)

# Nettoyer les données et préparer des fichiers txt pour vosviewer ----
`%not_like%` <- purrr::negate(`%like%`) # juste au cas où j'aurais besoin du not_like.
`%not_in%` <- purrr::negate(`%in%`) # juste au cas où j'aurais besoin du not_like.
donnees <- list_data[df$site %like% "%\\.%"]

# Le site doit contenir au moins un caractère alphabetique  
donnees1 <- donnees[grep("[a-zA-Z]", donnees$site), ]

# Supprimer tous les caractères spéciaux sauf le "."
donnees2 <- data.frame(donnees1$publication, stringr::str_remove_all(donnees1$site, "[\\p{P}\\p{S}&&[^.]]"))
names(donnees2) = c("publication", "site")

# Transformation des données (sites as integer)
donnees3 <- data.frame(as.integer(donnees2$publication), donnees2$site)
names(donnees3) = c("publication", "site")

## Exportation des données ----
#  dbWriteTable(conn = con, "publication_sites_comm", donnees3)

# Typologie des sites ----
## Transformer en "factor" les sites
t <- data.frame(donnees3$publication, factor(donnees3$site)) 
names(t) <- c("publication","site")

# Utiliser la fonction ave() pour ajouter une colonne avec une séquence numérique qui se réinitialise selon id
t$seq <- ave(t$publication, t$publication, FUN = function(x) seq_along(x))
# Utiliser la fonction group_by() pour regrouper les données par identifiant
grouped_df <- t %>% 
  group_by(publication) %>% 
  mutate(quartile = ntile(seq, 4))


# Calculer les fréquences pour avoir une idée de la distribution des sites
f <- grouped_df$site |>
  fct_infreq() |> 
  questionr::freq()

# juste pour rajouter la noms de lignes en tant que colonne
freqsit <- data.frame(rownames(f),f)
names(freqsit) = c("site","nb","part","freq")

# Importer les données téseaurus pour unifier et mettre en forme les sites (ceux qui sont les plus fréquents)
class_sites <- readxl::read_xlsx("classification sites.xlsx", col_names = TRUE)

t2 <- t %>% 
    left_join(class_sites, by = c("site")) %>% # un left join avec expressions régulières (contain)
    data.frame(factor(.$site), factor(.$type_sit)) %>%
    .[,c(1,3,6,7)]
   names(t2) <- c("publication", "seq","site", "sit_harm")

# Remplacer les valeurs manquantes dans col1 avec les valeurs correspondantes dans col2
t2$site <- as.character(t2$site)
t2$sit_harm <- as.character(t2$sit_harm)
t2$sit_harm[is.na(t2$sit_harm)] <- t2$site[is.na(t2$sit_harm)]
t2$site <- factor(t2$site)
t2$sit_harm <- factor(t2$sit_harm)  

#dbWriteTable(con, "data_sites_harmo", t2)

# exclure du dataframe toutes les lignes (sites) dont on sait qu'ils ne sont pas des sites de discussion

# Récupérer les données de sites à exclure
class_sites2 <- readxl::read_xlsx("classification sites2.xlsx", col_names = TRUE)

# Vecteur contenant les valeurs à exclure
search_strings <- class_sites2$site

# Extraction des lignes pour lesquelles la colonne "element" contient une partie des chaînes de caractères
new_df <- t2[!apply(sapply(search_strings, function(x) grepl(x, t2$sit_harm)), 1, any), ]

# Calculer les fréquences pour avoir une idée de la distribution des sites
f <- factor(new_df$sit_harm) |>
  fct_infreq() |> 
  questionr::freq()

f_a_analyser <- data.frame(rownames(f),f)


## Je me rends compte qu'il faut garder le lien original tel que cité dans le commentaire
## je refait donc une extraction ici


# Exemple de vecteur de caractères
text <- URL_var

# Expression régulière pour extraire les URLs
url_pattern <- "(https?://)?(www\\.)?\\w+\\.\\w+(/[\\w\\-./?%&=]*)?"

# Extraction des URLs
urls_orig <- regmatches(text, gregexpr(url_pattern, text))

urls <- str_extract_all(text, "(?<=href=\")[^\"]*")

urls <- unlist(regmatches(text, gregexpr("https?://\\S+|www\\.\\S+", gsub("\"", "'", text), perl=TRUE)))
urls


# Affichage des URLs
# unlist(urls_orig)

# Attribuer aux liens, les identifiants des publications d'où ils sont issus ----
names(urls_orig) <- names(URL_var)

# Transformer en liste de dataframe
list_data_ori <- Map(as.data.frame, urls_orig) %>%
  rbindlist(., use.names = F, idcol = names(.))
names(list_data_ori) = c("publication", "site")

df_ori = data.frame(list_data_ori$publication, factor(list_data_ori$site))
names(df_ori) = c("publication", "site")










## Calcul des cooccurrences ----
# Créer un data frame de test
df <- data.frame(id = t4$publication,
                 col = t4$type_sit)

# Récupérer les valeurs uniques de la colonne col
unique_vals <- unique(df$col)

# Générer toutes les combinaisons possibles des valeurs
combinations <- combn(unique_vals, 2)

# Initialiser un data frame pour stocker les résultats
results <- data.frame(col1 = character(), col2 = character(), count = numeric())

# Boucle sur les combinaisons
for (i in 1:ncol(combinations)) {
  # Sélectionner les lignes pour lesquelles la valeur de col correspond à la première ou la deuxième valeur de la combinaison actuelle
  temp_df <- df %>% 
    filter(col == combinations[1, i] | col == combinations[2, i])
  
  # Compter le nombre de cooccurrences de la première et de la deuxième valeur de la combinaison actuelle
  temp_result <- temp_df %>% 
    group_by(id) %>% 
    summarise(count = sum(col == combinations[1, i] & shift(col, type = "lead") == combinations[2, i] | 
                            col == combinations[2, i] & shift(col, type = "lag") == combinations[1, i]))
  
  # Ajouter les résultats temporaires au data frame de résultats
  results <- rbind(results, data.frame(col1 = combinations[1, i], col2 = combinations[2, i], 
                                       count = sum(temp_result$count)))
}

# retirer les NA
results <- results[complete.cases(results), ]

## Exporter pour utiliser dans vosviewer
write.csv(results, "cooccurrences_sites_typo.csv")

# Imprimer le résultat
# print(results)

# Représenter graphiquement le réseau ----

# Convertir les colonnes en liste de bords
edge_list <- as.matrix(results[, c("col1", "col2")])

# Ajouter le poids des bords à partir de la colonne "nb_cooccurrences"
weights <- results$count

# Créer un graphe non orienté à partir de la liste de bords
g <- graph_from_edgelist(edge_list, directed = FALSE)

# Ajouter les poids aux bords
E(g)$weight <- weights

# Visualiser le graphe
plot(g, edge.width = E(g)$weight)
