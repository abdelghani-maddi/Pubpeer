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
  subset(., comm %like% c("%https%","%http%","%www%","%WWW%"))

# Créer une nouvelle colonne pour stocker les URL extraites
URL_var$urls <- NA

# Parcourir chaque ligne et extraire l'URL
for (i in 1:nrow(URL_var)) {
  # Extraire l'URL en utilisant une expression régulière
  extracted_url <- str_extract_all(URL_var$comm[i], "(?i)\\b(?:https?://|www\\.)\\S+(?:/|\\b)")  
  # Stocker l'URL extraite dans la nouvelle colonne
  URL_var$urls[i] <- extracted_url
}

# séparer les URLs en autant de lignes
df_split <- unnest(URL_var, urls)
urls_parses <- url_parse(df_split$urls)
df <- merge(df_split, urls_parses, by = "row.names", all = F)

write_xlsx(df, "urls_commentaires.xlsx")


##
df_split$urls2 <- NA

for (i in 1:nrow(df_split)) {
  # Extraire l'URL en utilisant une expression régulière : c'est pour les cas où la chaine de caractères contient un balisage HTML avec des caractères spéciaux qui ne sont pas pris en compte par l'expression régulière précédente
  extracted_url <- str_extract_all(df_split$urls[i], "(?i)\\b((?:https?://|www\\d{0,3}[.]|[a-z0-9.\\-]+[.][a-z]{2,4}/)(?:[^\\s()<>]+|\\(([^\\s()<>]+|(\\([^\\s()<>]+\\)))*\\))+(?:\\(([^\\s()<>]+|(\\([^\\s()<>]+\\)))*\\)|[^\\s`!()\\[\\]{};:'\".,<>?«»“”‘’]))")
  # Stocker l'URL extraite dans la nouvelle colonne
  df_split$urls2[i] <- extracted_url
}

# séparer les URLs en autant de lignes
df_split2 <- unnest(df_split, urls2)
urls_parses2 <- url_parse(df_split2$urls2)
df_final <- merge(df_split2, urls_parses2, by = "row.names", all = F)

