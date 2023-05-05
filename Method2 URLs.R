rm(list = ls()) #supprimer tous les objets 


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
library(rebus)
library(Matrix)
library(plyr)
library(sjmisc)
library(regexplain)
library(gtsummary)
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

reqsql= paste('select inner_id, publication, "DateCreated" as date_com, html as comm from data_commentaires_2')
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



# Typologie des sites : tableau de correspondance ----
class_sites <- readxl::read_xlsx("classification sites2.xlsx", sheet = "Feuil3", col_names = TRUE)

# Créer une nouvelle colonne "typo" basée sur le tableau de correspondance  
urls_unique$typo <- NA  # Initialiser la colonne à NA

# Parcourir chaque élément du vecteur class_sites$pattern dans l'ordre d'apparition
for (i in seq_along(class_sites$pattern)) {
  site <- class_sites$pattern[i]
  type <- class_sites$type[i]
  
  # Trouver les éléments de urls_unique$domain qui correspondent à site
  matches <- grepl(site, urls_unique$domain)
  
  # Ne mettre à jour la colonne typo que pour les éléments non encore typés et correspondant à site
  urls_unique$typo[matches & is.na(urls_unique$typo)] <- type
}

# Supprimer les lignes contenant la chaîne "http" (liées à un problème de pasing)
urls_unique <- urls_unique %>% filter(!grepl("http", domain))
urls_unique <- urls_unique[grep("\\.", urls_unique$domain), ]


## Recoding urls_unique$typo
urls_unique$typo <- factor(urls_unique$typo) %>%
  fct_explicit_na("Autre")



# Chercher les lignes où "path" contient "image" et "domain" contient "pubpeer"
rows_to_modify <- which(grepl("(image|jpeg|png|jpg|imgur)", urls_unique$urls, ignore.case = TRUE))
# rows_to_modify <- which(grepl("(image|jpeg|png|jpg|imgur)", data_urls$path, ignore.case = TRUE)) pour généraliser à tout et non pubpeer only

# Modifier les valeurs de "typo" dans les lignes sélectionnées
#data_urls$typo[rows_to_modify] <- gsub("pubpeer", "image", data_urls$typo[rows_to_modify])
urls_unique$typo[rows_to_modify] <- "image" # urls_unique$path, ignore.case = TRUE)) # pour généraliser à tout et non pubpeer only


# Récupérer quelques lignes classées "Autre" :
urls_unique <- urls_unique %>%
  mutate(
    typo = case_when(
      typo == "Autre" & grepl("doi", urls) ~ "Editeur - revue",
      typo == "Autre" & grepl("api", urls) ~ "Code - Scripts",
      typo == "Autre" & grepl("data", urls) ~ "Base de données",
      typo == "Autre" & grepl("pdf", path) ~ "Editeur - revue",
      TRUE ~ typo
    )
  )

# Calcul de la fréquence des sites "autre" pour avoir une idée plus précise
f <- factor(urls_unique$domain[urls_unique$typo=="Autre"]) |>
  fct_infreq() |> 
  questionr::freq()
freqsit <- data.frame(rownames(f),f)
names(freqsit) = c("site","nb","part","freq")


# Calcul de la fréquence des sites "autre" pour avoir une idée plus précise
f2 <- factor(urls_unique$typo[urls_unique$typo!="pubpeer"]) |>
  fct_infreq() |> 
  questionr::freq()
freqsit2 <- data.frame(rownames(f2),f2)
names(freqsit2) = c("site","nb","part","freq")

## ecrire la table sur Postgresql pour calculer les cooccurrences
dbWriteTable(con, "data_urls_comm_2", urls_unique)

# Calcul de la fréquence des sites pour avoir une idée plus précise
f <- factor(urls_unique$typo[urls_unique$typo != "pubpeer" & urls_unique$typo != "Editeur - revue"]) |>
  fct_infreq() |> 
  questionr::freq()
freqsit <- data.frame(rownames(f),f)
names(freqsit) = c("site","nb","part","freq")


# Calcul de la fréquence des sites pour avoir une idée plus précise
f <- factor(urls_unique$domain[urls_unique$typo == "Médias"]) |>
  fct_infreq() |> 
  questionr::freq()
freqmed <- data.frame(rownames(f),f)
names(freqsit) = c("site","nb","part","freq")

# Calcul de la fréquence des sites pour avoir une idée plus précise
f <- factor(urls_unique$domain[urls_unique$typo == "Réseau social"]) |>
  fct_infreq() |> 
  questionr::freq()
freqresau <- data.frame(rownames(f),f)
names(freqsit) = c("site","nb","part","freq")


