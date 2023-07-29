######################################################
# Analyse marché édition
######################################################

## Formatage espace de travail ----
rm(list = ls()) #supprimer tous les objets 

# chargement des packages ----
library(tibble)
library(textdata)
library(Hmisc)
library(zoo)
library(flextable)
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
library(gtsummary)
library(igraph)
library(openxlsx2)
library(readxl)
library(openxlsx)

# install.packages('remotes')
# remotes::install_github("gadenbuie/regexplain")

## Chargement des données ----

######################################################
# Bdd publications
data_pub <- read_excel("D:/bdd pubpeer/data_pub.xlsx")
######################################################
# Bdd commentaires
data_comm <- read_excel("D:/bdd pubpeer/data_comm2.xlsx")
######################################################
# bdd retractions
df_retract <- read_excel("D:/Pubpeer Gender/df_gender_retract.xlsx")
######################################################
# Nombre d'auteurs et de commentaires
nb_aut_com <- df_retract %>%
  select(publication, nb_aut, Nombre.de.commentaires)
names(nb_aut_com) <- c("publication", "nb_aut", "nb_com")
######################################################
# Données sur les rétractations
data_retract <- df_retract %>%
  select(publication, is_retracted, Reason) %>%
  separate_rows(Reason, sep = ";")%>%
  filter(Reason != "") %>%
  mutate(Reason = str_replace(Reason, "\\+", ""))
######################################################
# Bdd DOAJ
doaj <- read.csv("D:/bdd pubpeer/journalcsv__doaj_20230728_0635_utf8.csv")
######################################################
# combiner issn et eissn
doaj$combined_issn <- paste(doaj$Journal.ISSN..print.version., doaj$Journal.EISSN..online.version., sep = ", ")
# Utiliser separate_rows() pour éclater la colonne issn en plusieurs lignes
doaj <- separate_rows(doaj, combined_issn, sep = ",\\s*", convert = FALSE)
######################################################
# Données OpenAlex
openalex <- read_excel("D:/bdd pubpeer/journals_openalex.xlsx")
######################################################
######################################################
# Analyse ----
######################################################

## Préparation données ----
 # Exclure les publications de 2016 qui ont un seul commentaire du commentateur n°3845,
 # plus d'infos, voir l'article de https://www-nature-com.inshs.bib.cnrs.fr/articles/540151a

pub_a_exclure <- data_comm %>%
  filter(Commentateurs_recodés == "3845" & year(DateCreated) == 2016) %>%
  select(publication) %>%
  unique() %>%
  left_join(., nb_aut_com, by = "publication") %>%
  unique() %>%
  filter(nb_com == 1) %>% # publi avec un seul commentaire généré automatiquement : 47633 publications
  select(publication) %>%
  unique()

# Nettoyer les données des publications de pubpeer
# Extraire les colonnes "publication" et "issn"
data_jnal <- data_pub %>%
  select(publication, issn)

# Fonction pour supprimer les crochets et les apostrophes dans la colonne issn
clean_issn <- function(issn_list) {
  gsub("\\[|'|\\]", "", issn_list)
}

# Appliquer la fonction clean_issn pour supprimer les crochets et les apostrophes
data_jnal$issn <- sapply(data_jnal$issn, clean_issn)

# Utiliser separate_rows() pour éclater la colonne issn en plusieurs lignes
data_jnal <- separate_rows(data_jnal, issn, sep = ",\\s*", convert = FALSE)



# Faire les calculs par revue
df_jnal_pubpeer <- data_jnal %>%
  filter(!(publication %in% pub_a_exclure$publication) & issn != "None") %>%
  left_join(., openalex, by = "issn") %>%
  select(-issn) %>%
  unique() %>%
  filter(!is.na(id))

# Comptes par édieur : PubPeer
nb_edit_pubpeer <- df_jnal_pubpeer %>%
  select(host_organization_name) %>%
  group_by(host_organization_name) %>%
  count()  


# Comptes par édieur : tout openalex
nb_edit_openalex <- openalex %>%
  select(id, host_organization_name, works_count) %>%
  unique() %>%
  filter(!is.na(host_organization_name) & !is.na(works_count)) %>%
  summarise(n = sum(works_count))


