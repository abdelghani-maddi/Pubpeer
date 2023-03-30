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
library(openxlsx2)
library(officer)

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

### donnees local : commentaires
data_comm <- readxl::read_excel("D:/bdd/data_comm.xlsx")
data_comm <- data_comm %>%
  select(inner_id, publication, DateCreated, html)
names(data_comm) <- c("inner_id", "publication", "date_com", "comm")


### donnees urls
reqsql2= paste('select * from data_urls_comm_2')
data_urls = dbGetQuery(con,reqsql2)
### En local :
data_urls <- readxl::read_excel("D:/bdd/data_urls.xlsx")


## Recuperation de la date
data_urls <- merge(data_urls, data_comm, by = c("inner_id", "publication"), all.x = TRUE)

# Transformer le format de la date du commentaire
data_urls$annee <- format(data_urls$date_com.y, "%Y")
data_urls$annee <- format(data_urls$date_com, "%Y") # local

## Select variables d'intérêt
data_urls <- select(data_urls, c("comm.y", "date_com", "annee", "inner_id", "publication", "urls", "scheme", "domain", "port", "path", "parameter", "fragment", "sequence", "typo"))
names(data_urls)[1:2] <- c("comm", "date_comm")
var_label(data_urls) <- c("Commentaire", "Date du commentaire", "Année du commentaire", "Identifiant du commentaire", "Identifiant de la publication", 
                          "Urls entiers", "Schéma", "Domaine", "Port", "Chemin", "Filtres appliqués", "Fragment", "Séquence", "Typologie")



## ecrire la table sur Postgresql pour calculer les cooccurrences
# Harmoniser à l'aide des regroupements leivenshtein, avant d'envoyer pour calculer les cooccurrences (notamment pour les "Médias")
grp_levenshtein <- readxl::read_excel("~/Documents/Pubpeer project/Pubpeer explo/grp_levenshtein.xlsx")

# Remplacer les différentes graphies des domains par une graphie unique
for (i in 1:nrow(grp_levenshtein)) {
  pattern <- grp_levenshtein$element[i]
  correct <- grp_levenshtein$remplace[i]
  data_urls$domain[grepl(pattern, data_urls$domain)] <- correct
}


# Chercher les lignes où "path" contient "image" et "domain" contient "pubpeer"
rows_to_modify <- which(grepl("(image|jpeg|png|jpg|imgur)", data_urls$path, ignore.case = TRUE) & grepl("pubpeer", data_urls$domain, ignore.case = TRUE))
# rows_to_modify <- which(grepl("(image|jpeg|png|jpg|imgur)", data_urls$path, ignore.case = TRUE)) pour généraliser à tout et non pubpeer only

# Modifier les valeurs de "typo" dans les lignes sélectionnées
data_urls$typo[rows_to_modify] <- gsub("pubpeer", "image", data_urls$typo[rows_to_modify])
# data_urls$typo[rows_to_modify] <- "image" data_urls$path, ignore.case = TRUE)) pour généraliser à tout et non pubpeer only


# Enregistrer le fichier dans postgresql
dbWriteTable(con, "data_urls_comm", data_urls)
# Enregistrer le fichier dans "Pubpeer explo"
write_xlsx(data_urls, "/Users/maddi/Documents/Pubpeer project/Pubpeer explo/donnees_URLS.xlsx")
write.xlsx(data_urls, "D:/bdd/donnees_URLS_fin.xlsx")

# stats par annee
theme_gtsummary_language(language = "fr", decimal.mark = ",", big.mark = " ")
#data_urls$publication <- as.character(data_urls$publication)

sect_properties <- prop_section(
  page_size = page_size(
    orient = "landscape",
    width = 8.3, height = 11.7
  ),
  type = "continuous",
  page_margins = page_mar()
)


nb_ann_typ <-
  data_urls %>% tbl_summary(
  include = c(typo),
  by = annee,
  percent = "column",
  sort = list(everything() ~ "frequency")
  ) %>%
  add_overall(last = TRUE, col_label = "**Ensemble** (effectif total: {N})") %>%
  as_flex_table() %>%
  flextable::save_as_docx(., path = "/Users/maddi/Documents/Pubpeer project/Pubpeer explo/urls_par_annee_type.docx",
                          pr_section = sect_properties)

