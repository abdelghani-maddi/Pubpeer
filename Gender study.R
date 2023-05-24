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
data_comm <- readxl::read_excel("D:/bdd/data_comm.xlsx")
rtw <- readxl::read_excel("D:/bdd/RWDBDNLD04242023.xlsx")

### Extraction colonnes d'intérêt et suppression des autres données
df <- data_pub %>%
  select(publication, Auteurs, Pays_institution, `Nombre de commentaires`, Année, starts_with("Journal"))
### Extraction colonnes d'intérêt et suppression des autres données
df <- bdd_pub %>%
  select(publication, Auteurs, Pays_institution, `Nombre de commentaires`, Année, starts_with("Journal"))
### Supprimer les données
rm(data_pub)

# Pivoter les noms des auteurs par autant de lignes que d'auteurs et dupliquer l'identifiant "publication"
df_unnested <- df %>%
  mutate(prenoms = str_extract_all(Auteurs, "(?<=')[A-Za-z]+")) %>%
  unnest(prenoms) %>%
  select(-Auteurs)

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
givenNames <- readxl::read_excel("D:/bdd/gender_proba.xlsx")

# matcher les prénoms
df_unnested$prenoms <- tolower(df_unnested$prenoms) # mettre en minuscules 
givenNames <- givenNames %>% # extraire valeurs uniques
  unique()

# ajouter une colonne ordre des auteurs
df_unnested <- df_unnested %>%
  group_by(publication) %>%
  mutate(order_auteur = row_number())

df_final <- merge(df_unnested, givenNames, by.x = "prenoms", by.y = "given_name", all.x = TRUE) # matcher
# df_final$gender[df_final$proba < 0.6] <- "unisex" # modifier les probas < 0.6 à Unisexe

# aJOUTER UNE COLONNE POUR INDIQUER SI LES FEMMES SE TROUVENT EN PREMIERE OU DERNIERE POSITION
df_final <- df_final %>%
  group_by(publication) %>%
  mutate(woman_leader = case_when(
    gender == "female" & proba >= 0.6 & (order_auteur == min(order_auteur) | order_auteur == max(order_auteur)) ~ 1,
    TRUE ~ 0
  ))

df_final <- df_final %>%
  group_by(publication) %>%
  mutate(woman_leader = ifelse(any(woman_leader == 1), 1, woman_leader))

# stats desc juste pour vérif
df_test <- df_final %>%
  select(publication, woman_leader) %>%
  unique()


df_final$g_prob_06 <- df_final$gender 
df_final$g_prob_06[df_final$proba < 0.6 & df_final$proba > 0.5] <- "unisex" # modifier les probas < 0.6 à Unisexe
# Remplacer les valeurs NA dans la colonne "gender" selon les conditions données
df_final$g_prob_06[is.na(df_final$g_prob_06) & nchar(df_final$prenoms) <= 2] <- "initials"
df_final$g_prob_06[is.na(df_final$g_prob_06) & !(nchar(df_final$prenoms) <= 2)] <- "undefined"
df_final$g_prob_06[is.na(df_final$g_prob_06)] <- "undefined"

df_final$g_prob_07 <- df_final$gender 
df_final$g_prob_07[df_final$proba < 0.7 & df_final$proba > 0.5] <- "unisex" # modifier les probas < 0.6 à Unisexe
# Remplacer les valeurs NA dans la colonne "gender" selon les conditions données
df_final$g_prob_07[is.na(df_final$g_prob_07) & nchar(df_final$prenoms) <= 2] <- "initials"
df_final$g_prob_07[is.na(df_final$g_prob_07) & !(nchar(df_final$prenoms) <= 2)] <- "undefined"
df_final$g_prob_07[is.na(df_final$g_prob_07)] <- "undefined"


df_final$g_prob_08 <- df_final$gender 
df_final$g_prob_08[df_final$proba < 0.8 & df_final$proba > 0.5] <- "unisex" # modifier les probas < 0.6 à Unisexe
df_final$g_prob_08[is.na(df_final$g_prob_08) & nchar(df_final$prenoms) <= 2] <- "initials"
df_final$g_prob_08[is.na(df_final$g_prob_08) & !(nchar(df_final$prenoms) <= 2)] <- "undefined"
df_final$g_prob_08[is.na(df_final$g_prob_08)] <- "undefined"


df_final$g_prob_09 <- df_final$gender 
df_final$g_prob_09[df_final$proba < 0.9 & df_final$proba > 0.5] <- "unisex" # modifier les probas < 0.6 à Unisexe
df_final$g_prob_09[is.na(df_final$g_prob_09) & nchar(df_final$prenoms) <= 2] <- "initials"
df_final$g_prob_09[is.na(df_final$g_prob_09) & !(nchar(df_final$prenoms) <= 2)] <- "undefined"
df_final$g_prob_09[is.na(df_final$g_prob_09)] <- "undefined"


df_final$g_prob_100 <- df_final$gender 
df_final$g_prob_100[df_final$proba > 0.5 & df_final$proba < 0.99] <- "unisex" # modifier les probas < 0.6 à Unisexe
df_final$g_prob_100[is.na(df_final$g_prob_100) & nchar(df_final$prenoms) <= 2] <- "initials"
df_final$g_prob_100[is.na(df_final$g_prob_100) & !(nchar(df_final$prenoms) <= 2)] <- "undefined"
df_final$g_prob_100[is.na(df_final$g_prob_100)] <- "undefined"


# Remplacer les valeurs NA dans la colonne "gender" selon les conditions données
df_final$gender[is.na(df_final$gender) & nchar(df_final$prenoms) <= 2] <- "initials"
df_final$gender[is.na(df_final$gender) & !(nchar(df_final$prenoms) <= 2)] <- "undefined"
df_final$gender[is.na(df_final$gender)] <- "undefined"

# Compter le nombre d'auteurs par publication
nbaut <- df_final %>%
  group_by(publication) %>%
  summarise(nb_aut = n_distinct(prenoms))

# Ajouter à la table des publications
df_nb_aut <- merge(df_final, nbaut, by.x = "publication", by.y = "publication", all.x = TRUE) # matcher



# stats desc proba et genre
df_final %>% 
  tbl_summary(
    include = c("proba", "g_prob_06", "g_prob_07", "g_prob_08", "g_prob_09", "g_prob_100")
  
)


## Analyse des données ----
`%not_in%` <- purrr::negate(`%in%`)

# Calcul de la proportion des femmes par publication
tbfin <- df_nb_aut %>%
  select(publication, gender, `Nombre de commentaires`, Année, starts_with("Journal")) %>%
  subset(., gender %not_in% c("initials", "unisex", "undefined")) %>%
  group_by(publication) %>%
  summarize(female_part = mean(gender == "female", na.rm = TRUE))

# faire une jointure
df_nb_aut <- merge(df_nb_aut, tbfin, by.x = "publication", by.y = "publication", all.x = TRUE) 



# Etudier l'évolution par type par année, toutes disciplines confondues
# Ajouter la variable
df_nb_aut$Gtype <- ifelse(df_nb_aut$female_part == 0 & df_nb_aut$nb_aut == 1, "Man alone", 
                          ifelse(df_nb_aut$female_part == 1 & df_nb_aut$nb_aut == 1, "Woman alone",
                                 ifelse(df_nb_aut$female_part == 0 & df_nb_aut$nb_aut > 1, "Collab. men only",
                                        ifelse(df_nb_aut$female_part == 1 & df_nb_aut$nb_aut > 1, "Collab. women only",
                                               ifelse(df_nb_aut$female_part > 0 & df_nb_aut$female_part < 1 & df_nb_aut$nb_aut > 1 & df_nb_aut$woman_leader==1, "Collab. men-women w lead", 
                                                      ifelse(df_nb_aut$female_part > 0 & df_nb_aut$female_part < 1 & df_nb_aut$nb_aut > 1 & df_nb_aut$woman_leader==0, "Collab. men-women m lead", NA)
                                               )
                                        )
                                 )
                          )
)





write.xlsx(df_nb_aut, "D:/bdd/tb_finale_gender.xlsx")



# write.xlsx(tb_final, "D:/bdd/tb_finale.xlsx")




tb_ech <- subset(tb_final, tb_final$`Nombre de commentaires`>1)

tb_ech %>% tbl_summary(
  include = c(`Nombre de commentaires`, female_part)
)

