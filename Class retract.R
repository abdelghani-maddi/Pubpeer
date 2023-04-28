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
#library(genderizeR)
library(openxlsx)


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


# Récupération des données nécessaires ----
df_stats_glob_m <- read_excel("~/Documents/Pubpeer project/Pubpeer explo/df_stats_glob_m.xlsx")
df_nb_aut <- readxl::read_excel("~/Documents/Pubpeer project/Pubpeer explo/df_nb_aut.xlsx")
rtw <- readxl::read_excel("~/Documents/Pubpeer project/Pubpeer explo/Gender/RWDBDNLD04242023.xlsx",sheet = "RWDBDNLD04242023")

pub_ret <- rtw %>%
  select(`Record ID`, OriginalPaperDOI, RetractionDate, OriginalPaperDate, Reason, Continent) #%>%
names(pub_ret) = c("ID_retractionwatch", "DOI","RetractionDate","OriginalPaperDate","Reason", "Continent")



# Charger les données sur la classification
class_retract <- readxl::read_excel("~/Documents/Pubpeer project/Pubpeer explo/Gender/class retract.xlsx")

pub_ret$Reason <- tolower(pub_ret$Reason)

# Initialiser la colonne "classe" avec des chaînes vides
pub_ret$classe <- ""

# Boucle sur les publications rétractées
for (i in 1:nrow(pub_ret)) {
  # Extraire la raison de la rétractation
  reason <- pub_ret$Reason[i]
  # Boucle sur les motifs de rétraction
  for (j in 1:nrow(class_retract)) {
    pattern <- class_retract$pattern[j]
    remplacement <- class_retract$remplacement[j]
    # Vérifier si le motif est présent dans la raison de la rétractation
    if (str_detect(reason, pattern)) {
      # Affecter la valeur de la colonne "remplacement" à la colonne "classe"
      pub_ret$classe[i] <- remplacement
      # Sortir de la boucle sur les motifs
      break
    }
  }
}


table(pub_ret$classe)


