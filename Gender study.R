rm(list = ls()) #supprimer tous les objets 
# https://store.genderize.io/usage
# https://github.com/kalimu/genderizeR/issues/7
# https://genderize.io/
# https://journal.r-project.org/archive/2016/RJ-2016-002/index.html 
# https://kalimu.github.io/#contact
# devtools::install_github("kalimu/genderizeR")
#library(genderizeR)

library(tidyverse)
library(questionr)
library(RPostgres)
library(gtsummary)
library(openxlsx2)
library(gender)
library(GenderInfer)
library(openxlsx)
library(readxl)


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

data_pub = read.csv2('/Users/maddi/Documents/Pubpeer project/Donnees/Bases PubPeer/PubPeer_Base publications.csv', sep=";")
data_comm = dbGetQuery(con,reqsql)
rtw <- readxl::read_excel("~/Documents/Pubpeer project/Pubpeer explo/Gender/RWDBDNLD04242023.xlsx",sheet = "RWDBDNLD04242023")

### Récupération des données en local TT
data_pub <- readxl::read_excel("D:/bdd/data_pub.xlsx")
data_comm <- readxl::read_excel("D:/bdd/data_comm.xlsx")
rtw <- readxl::read_excel("D:/bdd/RWDBDNLD04242023.xlsx")

### Extraction colonnes d'intérêt et suppression des autres données
df <- data_pub %>%
  select(publication, Auteurs, Pays_institution, Nombre.de.commentaires, Année, starts_with("Journal"))
### Extraction colonnes d'intérêt et suppression des autres données
df <- bdd_pub %>%
  select(publication, Auteurs, Pays_institution, `Nombre de commentaires`, Année, starts_with("Journal"))

### Supprimer les données inutiles
rm(data_pub)

## Prédire le genre ----
# Pivoter les noms des auteurs par autant de lignes que d'auteurs et dupliquer l'identifiant "publication"
df_unnested <- df %>%
  mutate(prenoms = str_extract_all(Auteurs, "(?<=')[A-Za-z]+")) %>%
  unnest(prenoms) %>%
  select(-Auteurs)

# Prédire le genre pour chaque prénom
# Usage de genderizeR
givenNames = findGivenNames(df_unnested$Auteur, progress = FALSE, apikey = '***********************')
names(givenNames) <- c("id", "gender", "given_name", "proba", "country_id")

write.xlsx(givenNames, "D:/bdd/gender_proba.xlsx")

givenNames <- read_excel("D:/bdd/gender_proba.xlsx")
givenNames <- read_excel("~/Documents/Pubpeer Gender/gender_proba.xlsx")

# matcher les prénoms
df_unnested$prenoms <- tolower(df_unnested$prenoms) # mettre en minuscules 

givenNames <- givenNames %>% # extraire valeurs uniques
  unique()

# ajouter une colonne ordre des auteurs
df_unnested <- df_unnested %>%
  group_by(publication) %>%
  mutate(order_auteur = row_number())

df_final <- merge(df_unnested, givenNames, by.x = "prenoms", by.y = "given_name", all.x = TRUE) # matcher


## Enrichissement des données avec des variables sur le rôle et le type de collaboration F H ----

# AJOUTER UNE COLONNE POUR INDIQUER SI LES FEMMES SE TROUVENT EN PREMIERE OU DERNIERE POSITION
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


# Définir une fonction pour remplacer les valeurs manquantes selon les conditions données
replace_missing_values <- function(df, column) {
  df[column][is.na(df[column]) & nchar(df$prenoms) <= 2] <- "initials"
  df[column][is.na(df[column]) & !(nchar(df$prenoms) <= 2)] <- "undefined"
  df[column][is.na(df[column])] <- "undefined"
  return(df)
}

# Modifier les colonnes spécifiées en fonction des conditions 
df_final <- transform(df_final, g_prob_06 = gender, g_prob_07 = gender, g_prob_08 = gender, g_prob_09 = gender, g_prob_100 = gender)

df_final$g_prob_06[df_final$proba < 0.6] <- "unisex"
df_final <- replace_missing_values(df_final, "g_prob_06")

df_final$g_prob_07[df_final$proba < 0.7] <- "unisex"
df_final <- replace_missing_values(df_final, "g_prob_07")

df_final$g_prob_08[df_final$proba < 0.8] <- "unisex"
df_final <- replace_missing_values(df_final, "g_prob_08")

df_final$g_prob_09[df_final$proba < 0.9] <- "unisex"
df_final <- replace_missing_values(df_final, "g_prob_09")

df_final$g_prob_100[df_final$proba >= 0.5 & df_final$proba < 0.99] <- "unisex"
df_final <- replace_missing_values(df_final, "g_prob_100")

df_final <- replace_missing_values(df_final, "gender")

# Compter le nombre d'auteurs par publication
nbaut <- df_final %>%
  group_by(publication) %>%
  summarise(nb_aut = n_distinct(prenoms))

# Ajouter à la table des publications
df_nb_aut <- merge(df_final, nbaut, by.x = "publication", by.y = "publication", all.x = TRUE) # matcher


###
`%not_in%` <- purrr::negate(`%in%`)

# Calcul de la proportion des femmes par publication
tbfin <- df_nb_aut %>%
  select(publication, gender, Nombre.de.commentaires, Année, starts_with("Journal")) %>%
  subset(., gender %not_in% c("initials", "unisex", "undefined")) %>%
  group_by(publication) %>%
  summarize(female_part = mean(gender == "female", na.rm = TRUE))

# Faire une jointure
df_nb_aut <- merge(df_nb_aut, tbfin, by.x = "publication", by.y = "publication", all.x = TRUE) 

# Ajouter la variable "Gtype"
df_nb_aut$Gtype <- ifelse(df_nb_aut$female_part == 0 & df_nb_aut$nb_aut == 1, "Man alone", 
                          ifelse(df_nb_aut$female_part == 1 & df_nb_aut$nb_aut == 1, "Woman alone",
                                 ifelse(df_nb_aut$female_part == 0 & df_nb_aut$nb_aut > 1, "Collab. men only",
                                        ifelse(df_nb_aut$female_part == 1 & df_nb_aut$nb_aut > 1, "Collab. women only",
                                               ifelse(df_nb_aut$female_part > 0 & df_nb_aut$female_part < 1 & df_nb_aut$nb_aut ==2 & df_nb_aut$woman_leader==1, "Collab. men-women 2 auteurs", 
                                                    ifelse(df_nb_aut$female_part > 0 & df_nb_aut$female_part < 1 & df_nb_aut$nb_aut > 1 & df_nb_aut$woman_leader==1, "Collab. men-women w lead", 
                                                          ifelse(df_nb_aut$female_part > 0 & df_nb_aut$female_part < 1 & df_nb_aut$nb_aut > 1 & df_nb_aut$woman_leader==0, "Collab. men-women m lead", NA)
                                               )
                                        )
                                 )
                            )
                       )
                  )


# Ajouter une autre variable sur le répartition du type de collab. H F. 
##
# cela consiste à :
# dupliquer la colonne Gtype en Gtype2, en modifiant les modalités selon ces conditions : 
# pour les valeurs de Gtype différentes de : ("Woman alone", "Man alone", "Collab. men only", "Collab. women only"), 
# si w_corresp=1, Gtype2="Collab. men-women . w corr", si m_corresp=1, Gtype2="Collab. men-women . m corr"
# Ajouter le flag femme auteur de correspondance (proxy : 1er auteur)
df_nb_aut <- df_nb_aut %>%
  mutate(w_corresp = ifelse(order_auteur == 1 & g_prob_06 == "female", 1, 0))

# Ajouter le flag homme auteur de correspondance (proxy : 1er auteur)
df_nb_aut <- df_nb_aut %>%
  mutate(m_corresp = ifelse(order_auteur == 1 & g_prob_06 == "male", 1, 0))

##
df_nb_aut <- df_nb_aut %>%
  group_by(publication) %>%
  mutate(w_corresp = ifelse(any(w_corresp == 1), 1, w_corresp),
         m_corresp = ifelse(any(m_corresp == 1), 1, m_corresp))


##
# Ajouter la variable "Gtype"
df_nb_aut$Gtype2 <- ifelse(df_nb_aut$female_part == 0 & df_nb_aut$nb_aut == 1, "Man alone", 
                          ifelse(df_nb_aut$female_part == 1 & df_nb_aut$nb_aut == 1, "Woman alone",
                                 ifelse(df_nb_aut$female_part == 0 & df_nb_aut$nb_aut > 1, "Collab. men only",
                                        ifelse(df_nb_aut$female_part == 1 & df_nb_aut$nb_aut > 1, "Collab. women only",
                                                ifelse(df_nb_aut$female_part > 0 & df_nb_aut$female_part < 1 & df_nb_aut$nb_aut > 1 & df_nb_aut$w_corresp==1, "Collab. men-women w lead", 
                                                             ifelse(df_nb_aut$female_part > 0 & df_nb_aut$female_part < 1 & df_nb_aut$nb_aut > 1 & df_nb_aut$m_corresp==1, "Collab. men-women m lead", 
                                                                    NA)
                                                      )
                                               )
                                        )
                                 )
                          )


## supprimer toutes les lignes pour lesquelles w_corresp et m_corresp = 0 : cela revient à garder uniquement les 
   ## publications distinctes pour lequelles tous les sexes des premiers auteurs sont identifiés
df_nb_aut2 <- df_nb_aut %>%
  filter(w_corresp != 0 | m_corresp != 0)

write.xlsx(df_nb_aut, "D:/bdd/tb_finale_gender.xlsx")
write.xlsx(df_nb_aut, "~/Documents/Pubpeer Gender/tb_finale_gender.xlsx")

write.xlsx(df_nb_aut2, "D:/bdd/tb_finale_gender_first_aut_only.xlsx")
write.xlsx(df_nb_aut, "~/Documents/Pubpeer Gender/tb_finale_gender_first_aut_only.xlsx")

# Stats desc proba et genre ----
df_final %>% 
  tbl_summary(
    include = c("proba", "g_prob_06", "g_prob_07", "g_prob_08", "g_prob_09", "g_prob_100")
    
  )

