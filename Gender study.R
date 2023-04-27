rm(list = ls()) #supprimer tous les objets 
# https://store.genderize.io/usage
# https://github.com/kalimu/genderizeR/issues/7
# https://genderize.io/
# https://journal.r-project.org/archive/2016/RJ-2016-002/index.html 
# https://kalimu.github.io/#contact

library(tidyverse)
library(questionr)
library(RPostgres)
library(gtsummary)
library(openxlsx2)
library(gender)
library(GenderInfer)
# devtools::install_github("kalimu/genderizeR")
library(genderizeR)
library(openxlsx)


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

reqsql= paste('select inner_id, publication, "DateCreated" as date_com, html as comm from data_commentaires_2')
data_comm = dbGetQuery(con,reqsql)

rtw <- readxl::read_excel("~/Documents/Pubpeer project/Pubpeer explo/Gender/RWDBDNLD04242023.xlsx",sheet = "RWDBDNLD04242023")

### Récupération des données en local
data_pub <- readxl::read_excel("D:/bdd/data_pub.xlsx")

### Extraction colonnes d'intérêt et suppression des autres données
df <- data_pub %>%
  select(publication, Auteurs, Pays_institution, `Nombre de commentaires`, Année, starts_with("Journal"))
### Supprimer les données
rm(data_pub)

# Pivoter les noms des auteurs par autant de lignes que d'auteurs et dupliquer l'identifiant "publication"
df_unnested <- df %>%
  mutate(Auteurs_extraits = str_extract_all(Auteurs, "'([\\p{L}\\s-]*)'")) %>%
  unnest(Auteurs_extraits) %>%
  select(-Auteurs)
# Renommer la nouvelle colonne "Auteurs"
names(df_unnested)[names(df_unnested) == "Auteurs_extraits"] <- "Auteur"
# Supprimer les guillemets simples des noms d'auteurs
df_unnested$Auteur <- gsub("'", "", df_unnested$Auteur)

# Extraire le prénom de chaque nom d'auteur
df_unnested$prenoms <- sapply(strsplit(df_unnested$Auteur, " "), function(x) x[1])

# Prédire le genre pour chaque prénom
# # Exemple de données avec une colonne "prenoms" contenant les prénoms
# authors <- data.frame( publication = df_unnested$publication,
#                        prenoms = df_unnested$prenoms)
# # Prédire le genre avec assign_gender
# authors <- assign_gender(data_df = authors, first_name_col = "prenoms")


# Usage de genderizeR
givenNames = findGivenNames(df_unnested$Auteur, progress = FALSE, apikey = '***********************')
names(givenNames) <- c("id", "gender", "given_name", "proba", "country_id")
#write.xlsx(givenNames, "D:/bdd/gender_proba.xlsx")

# matcher les prénoms
df_unnested$prenoms <- tolower(df_unnested$prenoms) # mettre en minuscules 
givenNames <- givenNames %>% # extraire valeurs uniques
  unique()
df_final <- merge(df_unnested, givenNames, by.x = "prenoms", by.y = "given_name", all.x = TRUE) # matcher
df_final$gender[df_final$proba < 0.6] <- "unisex" # modifier les probas < 0.6 à Unisexe
df_final$gender[is.na(df_final$gender)] <- "initials" # modifier les "NA" de gender en "initials"

## Analyse des données ----
`%not_in%` <- purrr::negate(`%in%`)

tb <- df_final %>%
  select(publication, gender, `Nombre de commentaires`, Année, starts_with("Journal")) %>%
  subset(., gender %not_in% c("initials", "unisex"))
# write.xlsx(tb, "D:/bdd/tb_finale.xlsx")

# Calcul de la proportion des femmes par publication
tbfin <- tb %>%
  group_by(publication) %>%
  summarize(female_part = mean(gender == "female", na.rm = TRUE))

# faire une jointure
tb_final <- merge(tb, tbfin, by.x = "publication", by.y = "publication", all.x = TRUE) %>% # matcher
  select(-gender) %>%
  unique()
# write.xlsx(tb_final, "D:/bdd/tb_finale.xlsx")

tb_ech <- subset(tb_final, tb_final$`Nombre de commentaires`>1)

tb_ech %>% tbl_summary(
  include = c(`Nombre de commentaires`, female_part)
)

