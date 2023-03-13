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

reqsql2= paste('select * from data_urls_comm')
data_urls = dbGetQuery(con,reqsql2)

## Recuperation de la date
data_urls <- merge(data_urls, data_comm, by = c("inner_id", "publication"), all.x = TRUE)

# Transformer le format de la date du commentaire
data_urls$annee <- strptime(data_urls$date_com.y, format = "%d/%m/%Y %H:%M") %>%
  year() + 2000
# Pour 3 publications la date n'est pas bien formatée. Correction :
# Changer la valeur de la colonne ville
data_urls <- data_urls %>% mutate(annee = case_when(
  annee == 4019 ~ 2019,
  TRUE ~ annee
))

## Select variables d'intérêt
data_urls <- select(data_urls, c("comm", "date_com.y", "annee", "inner_id", "publication", "urls", "scheme", "domain", "port", "path", "parameter", "fragment", "sequence", "typo"))
names(data_urls)[1:2] <- c("comm", "date_comm")
var_label(data_urls) <- c("Commentaire", "Date du commentaire", "Année du commentaire", "Identifiant du commentaire", "Identifiant de la publication", 
                          "Urls entiers", "Schéma", "Domaine", "Port", "Chemin", "Filtres appliqués", "Fragment", "Séquence", "Typologie")

# Enregistrer le fichier dans "Pubpeer explo"
write_xlsx(data_urls, "/Users/maddi/Documents/Pubpeer project/Pubpeer explo/donnees_URLS.xlsx")

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

## ecrire la table sur Postgresql pour calculer les cooccurrences
# Harmoniser à l'aide des regroupements leivenshtein, avant d'envoyer pour calculer les cooccurrences (notamment pour les "Médias")
grp_levenshtein <- readxl::read_excel("~/Documents/Pubpeer project/Pubpeer explo/grp_levenshtein.xlsx")

# Remplacer les différentes graphies des domains par une graphie unique
for (i in 1:nrow(grp_levenshtein)) {
  pattern <- grp_levenshtein$element[i]
  correct <- grp_levenshtein$remplace[i]
  data_urls$domain[grepl(pattern, data_urls$domain)] <- correct
}


dbWriteTable(con, "data_urls_comm", data_urls)

