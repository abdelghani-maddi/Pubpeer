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
library(stringi)
library(gtsummary)

#library(bibliometrix)
# library(openalexR)
# library(stringr)
# Charger à nouveau le package


# Pour mettre à jour le package bibliometrix 
# install.packages("remotes")
# library(remotes)
# remotes::install_github("massimoaria/bibliometrix")

## Données RetractionWatch ----
rtw <- read_excel("~/Documents/Pubpeer Gender/RWDBDNLD04242023.xlsx", sheet = "RWDBDNLD04242023") ### bdd retractations (version avril 2023)
rtw <- read_excel("D:/Pubpeer Gender/RWDBDNLD04242023.xlsx", sheet = "RWDBDNLD04242023") ### bdd retractations (version avril 2023)

# Utiliser la fonction separate_rows pour séparer les valeurs de la colonne 'Author' en lignes
rtw_melted <- rtw %>%
  separate_rows(Subject, sep = ";") %>%  
  group_by(`Record ID`) %>%
  ungroup()  %>% # Dégroupement pour réinitialiser
  filter(!(Subject=="")) %>%
  group_by(`Record ID`) %>%
  mutate(frac = 1/n())

# extraire les valeurs entre parenthèses
extract_in_parentheses <- function(text) {
  str_extract_all(text, "\\(.*?\\)")[[1]]
}


# ajout de la colonne

rtw_melted <- rtw_melted %>%
  mutate(Grd_disc = sapply(Subject, extract_in_parentheses))


# counts

nb_disc <- rtw_melted %>%
  select(`Record ID`, Subject, frac) %>%
  group_by(Subject) %>%
  summarise(nbr = sum(frac))
 
write.xlsx(nb_disc, "D:/Pubpeer Gender/retraction_by_disc.xlsx") 
  
  
# extraire les publications pour lesquelles la raison est "out of date)

rw_out_of_date <- rtw %>%
  filter(str_detect(Reason, "out of date"))

write.xlsx(rw_out_of_date, "D:/Pubpeer Gender/rw_out_of_date.xlsx") 

