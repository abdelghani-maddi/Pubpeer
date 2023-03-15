rm(list = ls()) #supprimer tous les objets 


library(tidyverse)
library(questionr)
library(gtsummary)
library(httr)
library(jsonlite)



### Récupération des données ----

## importer les données quand elles sont en local et non sur Pstgresql
df <- readxl::read_excel("D:/bdd/data_pub.xlsx") 
# revues
jnal <- select(df, starts_with("Journal"))

## explo rapide
glimpse(df)
describe(df$concepts_mot)
describe(df$Journal_Categories_WOS)
describe(df$Journal_Domaines_WOS)
describe(df$Journal_Domaines_Scimago)


# Calculer les fréquences pour avoir une idée de la distribution des sites
jwos <- df$Journal_title_WOS |>
  fct_infreq() |> 
  questionr::freq()


# Obtenir le nombre de publications par pays dans la base openalex
response <- GET("https://api.openalex.org/works?group_by=institutions.country_code")
json_data <- content(response, as = "text")
json_data <- fromJSON(json_data)
df_publications <- as.data.frame(json_data$group_by)

