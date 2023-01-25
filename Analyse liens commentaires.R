## Formatage espace de travail
rm(list = ls()) #supprimer tous les objets 

# activate packages
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

### CONNEXION

con<-dbConnect(RPostgres::Postgres())

db <- 'SKEPTISCIENCE'  #provide the name of your db
host_db <- 'localhost' # server
db_port <- '5433'  # port DBA
db_user <- 'postgres' # nom utilisateur  
db_password <- 'Maroua1912'
con <- dbConnect(RPostgres::Postgres(), dbname = db, host=host_db, port=db_port, user=db_user, password=db_password)  
# Test connexion
dbListTables(con) 

### RECUPERATION DES DONNEES

reqsql= paste('select * from commentaires_par_discipline')
data_comm = dbGetQuery(con,reqsql)

### Récupération des commentaires avec liens hypertextes
url_pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"

#################################################################################
# Creer la liste des liens qui se trouvent dans les commentaires gardant 
# le lien avec les publications à partir desquelles sont extraits
#################################################################################
# transformer le type de données pour plus de facilité/performance dans le traitement
liens_all <- as_tibble(data_comm) %>% 
  # Se limiter aux commentaires avec au moins un lien hypertexte  
     subset(., markdown %like% c("%https%","%http%","%www%","%WWW%")) %>% 
  # Recupérer uniquement les commentaires (l'intérêt de garder le subset est de pouvoir selectionner par la suite les disciplines aussi)
     .$markdown %>% 
  # Extraire les liens
     str_extract_all(., url_pattern) %>% 
  # Transformer les données
     Map(as.data.frame, .) 

#################################################################################
  # Attribuer aux liens, les identifiants des publications d'où ils sont issus.
pr_id_pub <- subset(data_comm, markdown %like% c("%https%","%http%","%www%","%WWW%")) # subset
names(liens_all) <- pr_id_pub$publication # attribution
rm(pr_id_pub) # suppression du subset
rm(data_comm) # suppression de l'import initial (pour libérer de la mémoire)

#################################################################################
#################################################################################
# Transformer la liste en dataframe
df <- as.data.frame(enframe(liens_all))
names(df) <- c("publication", "liens")
rm(liens_all) # suppression de la liste précédente (on va travailler désormais sur df)

# Préparation de données et extraction des noms des sites (pour ne pas récupérer tout le lien)
splt <- split(df$liens, df$publication) # spliter la liste ainsi créée

# Transformer en data.frame
df <- data.frame(unlist(splt)) %>%
      data.frame(str_split(row.names(.), '.d', simplify = T), .) 
names(df) <- c("publication", "ne_pas_prendre","liens") # renommer les colonnes
rm(splt) # plus besoin à présent du splt

#################################################################################
#####  Nettoyer les données et préparer des fichiers txt pour vosviewer   #######
#################################################################################
df # Donnees

`%not_in%` <- purrr::negate(`%in%`) # construire la négation de "in"
`%not_like%` <- purrr::negate(`%like%`) # construire la négation de "like"

## liens erronnés à exclure (il n'y a pas beaucoup donc inutile de développer des expressions régulières pour cela)
exclure <- c(")", 
             "^![](http:",
             "![file](https:",
             ")![file](![https:",
             ")![file](https:", 
             ").",
             "),",
             "	)![](http:",
             "2F",
             ")![](http:",
             ")2F,",
             ")https:",
             ")](http:",
             "http://)",
             "http://![](http://))![](http://)",
             "http://),",
             "http://)![](http://)![](http://)",
             "http://)![](http://)",
             "http://).",
             "http://)2F,"	
             )
## liens bizares :
liens_bizarres_pubpeer <- c("63537.dots[[1L]][[33043L]]","76578.dots[[1L]][[26350L]]",
                    "113418.dots[[1L]][[39689L]]2","113418.dots[[1L]][[39689L]]1",
                    "113420.dots[[1L]][[31132L]]1","118405.dots[[1L]][[32151L]]2",
                    "117826.dots[[1L]][[21884L]]","115998.dots[[1L]][[36866L]]",
                    "114885.dots[[1L]][[41539L]]1","67612.dots[[1L]][[43811L]]",
                    "109088.dots[[1L]][[43462L]]","114001.dots[[1L]][[19235L]]2",
                    "111799.dots[[1L]][[30785L]]","63909.dots[[1L]][[37628L]]1",
                    "88844.dots[[1L]][[35579L]]","70109.dots[[1L]][[33647L]]",
                    "83198.dots[[1L]][[35048L]]3","67963.dots[[1L]][[37872L]]1",
                    "67963.dots[[1L]][[33470L]]1","80644.dots[[1L]][[38623L]]1",
                    "80216.dots[[1L]][[27218L]]2","74268.dots[[1L]][[33960L]]",
                    "72915.dots[[1L]][[40567L]]","72295.dots[[1L]][[4591L]]",
                    "69018.dots[[1L]][[46509L]]","68341.dots[[1L]][[37910L]]1",
                    "67014.dots[[1L]][[24815L]]","66618.dots[[1L]][[2156L]]",
                    "64133.dots[[1L]][[37672L]]2","2575.dots[[1L]][[32547L]]",
                    "62970.dots[[1L]][[24101L]]1","62780.dots[[1L]][[353L]]2",
                    "62618.dots[[1L]][[40206L]]","69030.dots[[1L]][[25197L]]1",
                    "67347.dots[[1L]][[2576L]]","64991.dots[[1L]][[1743L]]1",
                    "115031.dots[[1L]][[39771L]]1","113418.dots[[1L]][[41484L]]",
                    "113420.dots[[1L]][[31132L]]2")

lien_bizarre_wiley <- c("63052.dots[[1L]][[24117L]]1")
lien_bizarre_nih <- c("54057.dots[[1L]][[32614L]]1")
lien_bizarre_revue <- ("65248.dots[[1L]][[1914L]]")

## exclusion des liens erronnés et des liens bizarres
# liens erronés
df <- df %>%   
       subset(., .$liens %not_in% exclure)
#correction liens bizarres
df$liens[rownames(df) %in% liens_bizarres_pubpeer] <- "https://pubpeer.com/"
df$liens[rownames(df) %in% lien_bizarre_nih] <- "https://www.ncbi.nlm.nih.gov/"
df$liens[rownames(df) %in% lien_bizarre_revue] <- "https://mja.com.au/"
df$liens[rownames(df) %in% lien_bizarre_wiley] <- "https://onlinelibrary.wiley.com/"
# supression vecteurs intermédiaires
rm(liens_bizarres_pubpeer)
rm(lien_bizarre_nih)
rm(lien_bizarre_revue)
rm(lien_bizarre_wiley)

## Extraction des sites
df <- df %>%
       data.frame(str_split(.$liens, "/", simplify = T)) # Extraire le site

# Récupérer uniquement les variables d'intérêt
df <- data.frame(df$publication, df$X3)
names(df) <- c("publication", "sites")

# Nettoyage supplémentaire
df$sites[rownames(df) == 19151] <- "vide"

## Harmonisation des sites "pubpeer", "twitter" et "youtube" : C'est fait avec sql
harmoniser <- c("%pubpeer%", "%twit%", "%yout%")

dbWriteTable(con, "data_sites_comm", df)

