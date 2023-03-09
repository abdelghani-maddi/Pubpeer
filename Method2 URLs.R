rm(list = ls()) #supprimer tous les objets 


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
library(openxlsx2)
library(httr)
library(rvest)
library(XML)


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

# Transformer le format de la date du commentaire
data_comm$date_com <- as.Date.character(data_comm$date_com)

#### Etape 0 : Transformer le type de données pour plus de facilité/performance dans le traitement ----
URL_var <- as_tibble(data_comm) %>% # Etape 0
  #### Etape 1 : Se limiter aux commentaires avec au moins un lien hypertexte  ----
  subset(., comm %like% c("%http%","%www%","%WWW%","%HTTP%"))

# Fonction pour supprimer les balises HTML
remove_html_tags <- function(x) {
  gsub("[>\\<]", " ", x) # .*? correspond à n'importe quel caractère répété 0 ou plusieurs fois, de manière non gourmande
}

# Application de la fonction à la colonne "comm"
URL_var$comm_sans_html <- sapply(URL_var$comm, remove_html_tags)

# Créer une nouvelle colonne pour stocker les URL extraites
URL_var$urls <- NA

# Parcourir chaque ligne et extraire l'URL
for (i in 1:nrow(URL_var)) {
  # Extraire l'URL en utilisant une expression régulière
  extracted_url <- str_extract_all(URL_var$comm_sans_html[i], "(?i)\\b(?:https?://|www\\.)\\S+(?:/|\\b)")  
  # Stocker l'URL extraite dans la nouvelle colonne
  URL_var$urls[i] <- extracted_url
}

# "(?i)\\b((?:https?://|www\\d{0,3}[.]|[a-z0-9.\\-]+[.][a-z]{2,4}/)(?:[^\\s()<>]+|\\(([^\\s()<>]+|(\\([^\\s()<>]+\\)))*\\))+(?:\\(([^\\s()<>]+|(\\([^\\s()<>]+\\)))*\\)|[^\\s`!()\\[\\]{};:'\".,<>?«»“”‘’]))"

# séparer les URLs en autant de lignes
df_split <- unnest(URL_var, urls)
urls_parses <- url_parse(df_split$urls) 
df <- merge(df_split, urls_parses, by = "row.names", all = F)

# faire un select distinct
urls_v1 <- data.frame(df$Row.names,df$publication,df$inner_id,df$date_com,df[,7:13])
names(urls_v1)[1:4] <- c("rowname","publication","inner_id","date_com")
urls_unique <- subset(urls_v1, !duplicated(paste(publication, inner_id, urls)))

# Utiliser la fonction ave() pour ajouter une colonne avec une ### séquence ### numérique qui se réinitialise selon id et suit l'ordre des rownames
urls_unique <- urls_unique[order(urls_unique$publication, as.numeric(urls_unique$rowname)),]
urls_unique$sequence <- ave(urls_unique$rowname, urls_unique$publication, FUN = function(x) seq_along(x))

# Calcul de la fréquence des sites pour avoir une idée plus précise
f <- factor(urls_unique$domain) |>
  fct_infreq() |> 
  questionr::freq()
freqsit <- data.frame(rownames(f),f)
names(freqsit) = c("site","nb","part","freq")

# Typologie des sites : tableau de correspondance
class_sites <- readxl::read_xlsx("classification sites2.xlsx", sheet = "Feuil3", col_names = TRUE)

# Créer une nouvelle colonne "typo" basée sur le tableau de correspondance
urls_unique$typo <- NA  # Initialiser la colonne à NA
for (i in seq_along(class_sites$pattern)) {
  site <- class_sites$pattern[i]
  type <- class_sites$type[i]
  urls_unique$typo[str_detect(urls_unique$domain, fixed(site))] <- type
}



# Parcourir chaque élément du vecteur class_sites$pattern dans l'ordre d'apparition
for (i in seq_along(class_sites$pattern)) {
  site <- class_sites$pattern[i]
  type <- class_sites$type[i]
  
  # Trouver les éléments de urls_unique$domain qui correspondent à site
  matches <- grepl(site, urls_unique$domain)
  
  # Ne mettre à jour la colonne typo que pour les éléments non encore typés et correspondant à site
  urls_unique$typo[matches & is.na(urls_unique$typo)] <- type
}







# afficher les résultats
print(urls_unique)








# Jointure avec le dataframe d'origine
result <- merge(df, urls_unique, by = "domain", all.x = TRUE)

# Remplacement des valeurs "NA" par "autre"
result$typo[is.na(result$typo)] <- "autre"

# Affichage du résultat
result



## Recoding urls_unique$typo
urls_unique$typo <- factor(urls_unique$typo) %>%
  fct_explicit_na("Autre")


# Calcul de la fréquence des sites "autre" pour avoir une idée plus précise
f <- factor(urls_unique$domain[urls_unique$typo=="Autre"]) |>
  fct_infreq() |> 
  questionr::freq()
freqsit <- data.frame(rownames(f),f)
names(freqsit) = c("site","nb","part","freq")

# Calcul de la fréquence des sites "autre" pour avoir une idée plus précise
f <- factor(urls_unique$typo) |>
  fct_infreq() |> 
  questionr::freq()
freqsit <- data.frame(rownames(f),f)
names(freqsit) = c("site","nb","part","freq")


# Création du dataframe de données
df <- urls_unique

# Vecteur de correspondance
corresp <- class_sites


# Fonction pour obtenir le code correspondant à partir de la chaîne de caractères
get_code <- function(x) {
  for (i in seq_along(corresp)) {
    if (str_detect(x, fixed(corresp[i]))) {
      code <- str_extract(corresp[i], "\\w+")
      return(code)
    }
  }
  return(NA)
}

# Ajout de la colonne "code"
df <- df %>% mutate(code = map_chr(url, get_code))

# Affichage du résultat
df








# Calcul de la fréquence des sites pour avoir une idée plus précise
f <- factor(urls_unique$typo[urls_unique$typo != "pubpeer" & urls_unique$typo != "Editeur - revue"]) |>
  fct_infreq() |> 
  questionr::freq()
freqsit <- data.frame(rownames(f),f)
names(freqsit) = c("site","nb","part","freq")






## Commandes pas executées, mais c'est à faire pour nettoyer et récupérer les quelques 600 URLs mal parsés
a <- subset(urls_v1, urls_v1$server=="")
# Extraire le protocole (http://)
protocole <- gsub("^(.*://).*", "\\1", a$urls)
# Extraire le nom de domaine (www.example.com)
domaine <- gsub("^.*://([^/]+).*", "\\1", a$urls)
# Extraire le chemin (/path/to/)
chemin <- gsub("^.*://[^/]+(/.*/).*", "\\1", a$urls)
# Extraire le nom de fichier (file.html)
nom_fichier <- gsub("^.*://[^/]+/.*/(.*)$", "\\1", a$urls)
b <- data.frame(protocole,domaine,chemin)
#####

