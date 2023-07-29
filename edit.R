# Analyse marché édition

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

# Bdd publications
data_pub <- read_excel("D:/bdd pubpeer/data_pub.xlsx")

# Bdd commentaires
data_comm <- read_excel("D:/bdd pubpeer/data_comm2.xlsx")

# Nombre de commentaires avant et après la rétraction
# nb_com <- read_excel("D:/Pubpeer Gender/retraction_data.xlsx")

# bdd retractions
df_retract <- read_excel("D:/Pubpeer Gender/df_gender_retract.xlsx")

# Nombre d'auteurs et de commentaires
nb_aut_com <- df_retract %>%
  select(publication, nb_aut, Nombre.de.commentaires)
names(nb_aut_com) <- c("publication", "nb_aut", "nb_com")

# Données sur les rétractations
data_retract <- df_retract %>%
  select(publication, is_retracted, Reason) %>%
  separate_rows(Reason, sep = ";")%>%
  filter(Reason != "") %>%
  mutate(Reason = str_replace(Reason, "\\+", ""))

# Bdd DOAJ
doaj <- read.csv("D:/bdd pubpeer/journalcsv__doaj_20230728_0635_utf8.csv")

# combiner issn et eissn
doaj$combined_issn <- paste(doaj$Journal.ISSN..print.version., doaj$Journal.EISSN..online.version., sep = ", ")
# Utiliser separate_rows() pour éclater la colonne issn en plusieurs lignes
doaj <- separate_rows(doaj, combined_issn, sep = ",\\s*", convert = FALSE)




# Bona Fide journals : see https://www.qoam.org/bfj/data
bonaf <- read_excel("D:/bdd pubpeer/bfj-issns.xlsx")
# Scopus 
scopus <- read_excel("D:/bdd pubpeer/SCOPUSextlistJanuary2023.xlsx")
data_scopus <- scopus %>%
  select(`Sourcerecord ID`,`Source Title (Medline-sourced journals are indicated in Green)`,
         `Print-ISSN`,`E-ISSN`,`Active or Inactive`, `Open Access status`, `Source Type`,
         `Publisher's Name`, `Publisher imprints grouped to main Publisher`)


names(data_scopus) <- c("id_scopus","jnal_title", "issn", "eissn", "is_active", "is_oa", "type",
                        "publisher_row", "publisher")


# Fonction pour ajouter un tiret après 4 caractères
add_dash_after_4_chars <- function(text) {
  substr_part1 <- substr(text, 1, 4)
  substr_part2 <- substr(text, 5, nchar(text))
  return(paste(substr_part1, substr_part2, sep = "-"))
}

# Appliquer la fonction add_dash_after_4_chars aux colonnes "issn" et "eissn" du DataFrame
data_scopus$issn <- sapply(data_scopus$issn, add_dash_after_4_chars)
data_scopus$eissn <- sapply(data_scopus$eissn, add_dash_after_4_chars)

# combiner issn et eissn
data_scopus$combined_issn <- paste(data_scopus$issn, data_scopus$eissn, sep = ", ")
# Utiliser separate_rows() pour éclater la colonne issn en plusieurs lignes
data_scopus <- separate_rows(data_scopus, combined_issn, sep = ",\\s*", convert = FALSE)


# Enrichir les infos sur les revues avec les éditeurs ----

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

# matcher avec Scopus
data_jnal2 <- left_join(data_jnal, data_scopus, by = c("issn" = "combined_issn"))

# matcher avec Scopus
data_jnal3 <- data_jnal2 %>%
  filter(is.na(.$id_scopus))
  

data_jnal4 <- left_join(data_jnal3, doaj, by = c("issn" = "combined_issn"))

data_jnal4 <- data_jnal4 %>%
  filter(!is.na(.$Publisher))



# Calculer les fréquences pour avoir une idée de la distribution des sites
jwos <- df$Journal_title_WOS |>
  fct_infreq() |> 
  questionr::freq()




library(rcrossref)
library(openalexR)
# Liste des ISSN/EISSN pour lesquels nous voulons récupérer le nom de l'éditeur
issn_list <- data_jnal4$issn# c("1083-351X", "0021-9258", "1067-8816")

# Fonction pour récupérer le nom de l'éditeur à partir d'un ISSN/EISSN
get_publisher_name <- function(issn) {
  metadata <- openalex_fetch(issn)
  if (!is.null(metadata$journal$publisher)) {
    publisher_name <- metadata$journal$publisher
  } else {
    publisher_name <- NA
  }
  return(publisher_name)
}

# Appliquer la fonction get_publisher_name pour chaque ISSN/EISSN
publisher_names <- sapply(issn_list, get_publisher_name)

# Créer un DataFrame avec les ISSN/EISSN et les noms de l'éditeur
result_df <- data.frame(issn = issn_list, publisher_name = publisher_names)
