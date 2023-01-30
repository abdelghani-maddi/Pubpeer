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
list_data <- Map(as.data.frame, URL_extract) 

t <- ldply(list_data[1:2])
t <- ldply(list_data[1:2])
tt <- unite(t, t[,2], t[,2],t[,3], sep = "")

df <- as.data.frame(enframe(list_data)) 
df %>% as.double(df[["a"]])
names(df) = c("a", "value")

# Extraire les rownames
a<- str_extract(rownames(df2), regex('\\]\\[+[0-9]+L')) %>%
  str_split(., "\\[", simplify = T) %>%
  as.data.frame() %>%
  .$V3 %>%
  gsub("L", "", .) %>%
  as.numeric() %>%
  as.data.frame()
class(a)

# combiner puis faire un left join
df_unlist <-  data.frame(unlist(df$value)) %>%
  cbind(., a)
names(df_unlist)

b <- unlist(df$a) %>%
  as.double() %>%
  as.data.frame()
names(b) <- "a"
class(b$a)

df_join <- df_unlist %>% left_join(b, by = c("a"))

names(df_unlist) = c("site", "commen")

# Transformer la liste en dataframe ----

df <- as.data.frame(enframe(list_data))
names(df) <- c("publication", "liens")
#rm(liens_all) # suppression de la liste précédente (on va travailler désormais sur df)

# Préparation de données et extraction des noms des sites (pour ne pas récupérer tout le lien)
splt <- split(df$liens, df$publication) # spliter la liste ainsi créée

#####  Nettoyer les données et préparer des fichiers txt pour vosviewer   #######

df # Donnees

`%not_in%` <- purrr::negate(`%in%`) # construire la négation de "in"
`%not_like%` <- purrr::negate(`%like%`) # construire la négation de "like"


## Extraction des sites
parsed_url <- data.frame(df$publication, url_parse(df$liens))
names(parsed_url)[1] = c("publication")

parsed_url$port[parsed_url$publication == "106541"]
encode = data.frame(url_encode(pr_id_pub$markdown))


# Récupérer uniquement les variables d'intérêt
df <- data.frame(as.numeric(df$publication), df$X3)
names(df) <- c("publication", "sites")
# df$publication <- as.numeric(df$publication)


# Nettoyage supplémentaire
df$sites[rownames(df) == 19151] <- "vide"

## Harmonisation des sites "pubpeer", "twitter" et "youtube" : C'est fait avec sql
harmoniser <- c("%pubpeer%", "%twit%", "%yout%")

dbWriteTable(con, "data_sites_comm", df)

parsed_url <- url_parse(df$sites)
