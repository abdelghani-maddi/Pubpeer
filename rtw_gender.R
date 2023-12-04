#####################################################################
### Analyse de données pour le papier collab h-f et retractations ###
#####################################################################
###               Toutes les données RetractionWatch              ###
#####################################################################
rm(list = ls()) #supprimer tous les objets 

### Chargement des packages ----

library(tidyverse)
library(questionr)
library(RPostgres)
library(gtsummary)
library(openxlsx2)
library(gender)
library(GenderInfer)
library(openxlsx)
library(readxl)
#library(bibliometrix)
# library(openalexR)
# library(stringr)
# Charger à nouveau le package
library(stringi)


# Pour mettre à jour le package bibliometrix 
# install.packages("remotes")
# library(remotes)
# remotes::install_github("massimoaria/bibliometrix")

## Données RetractionWatch ----
rtw <- read_excel("~/Documents/Pubpeer Gender/RWDBDNLD04242023.xlsx", sheet = "RWDBDNLD04242023") ### bdd retractations (version avril 2023)
givenNames <- read_excel("~/Documents/Pubpeer Gender/gender_proba.xlsx")


# Utiliser la fonction separate_rows pour séparer les valeurs de la colonne 'Author' en lignes
rtw_melted <- rtw %>%
  separate_rows(Author, sep = ";") %>%  
  group_by(`Record ID`) %>%
  mutate(AuthorNumber = row_number()) %>%  # Ajouter le numéro de l'auteur pour chaque groupe
  ungroup()  # Dégroupement pour réinitialiser


# Ajouter la colonne 'FirstLast' : pour premier/dernier auteur, nombre d'auteurs et prénom
rtw_melted <- rtw_melted %>%
  group_by(`Record ID`) %>%
  mutate(IsFirstLast = if_else(AuthorNumber == 1 | AuthorNumber == max(AuthorNumber), 1, 0),
         nb_aut = max(AuthorNumber),
         FirstName = str_extract(Author, "^[^\\s]+")) %>%
  ungroup()   # Dégroupement pour réinitialiser
  

