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
bdd_pub = read.csv2('/Users/maddi/Documents/Pubpeer project/Donnees/Bases PubPeer/PubPeer_Base publications.csv', sep=";")

pub_ret <- rtw %>%
  select(`Record ID`, OriginalPaperDOI, RetractionDate, OriginalPaperDate, Reason, Continent) #%>%
names(pub_ret) = c("ID_retractionwatch", "DOI","RetractionDate","OriginalPaperDate","Reason", "Continent")



# Charger les données sur la classification
class_retract <- readxl::read_excel("~/Documents/Pubpeer project/Pubpeer explo/Gender/class retract.xlsx")

pub_ret$Reason <- tolower(pub_ret$Reason)
# Remplacer les caractères spéciaux par des espaces dans la colonne "Reason"
pub_ret$Reason <- gsub("[^[:alnum:][:space:]]", " ", pub_ret$Reason)


# Créer une colonne "classe" dans le dataframe "pub_ret"
pub_ret$classe <- NA

# Boucle pour parcourir chaque ligne du dataframe "pub_ret"
for (i in 1:nrow(pub_ret)) {
  # Récupérer la valeur de la colonne "Reason" pour la ligne i
  reason <- pub_ret[i, "Reason"]
  
  # Boucle pour parcourir chaque ligne du dataframe "class_retract"
  for (j in 1:nrow(class_retract)) {
    # Récupérer la valeur de la colonne "pattern" et "remplacement" pour la ligne j
    pattern <- class_retract[j, "pattern"]
    remplacement <- class_retract[j, "remplacement"]
    
    # Vérifier si le pattern se trouve dans la valeur de la colonne "Reason"
    if (grepl(pattern, reason)) {
      # Remplacer la valeur de la colonne "classe" par la valeur de "remplacement"
      pub_ret[i, "classe"] <- remplacement
      # Sortir de la boucle interne pour ne pas prendre en compte les autres occurrences de pattern
      break
    }
  }
}





table(pub_ret$classe)




pub <- bdd_pub %>%
  select(publication, DOI) 

# Supprimer les caractères spéciaux et les espaces des colonnes "DOI" des dataframes pub et pub_ret
pub$DOI_clean <- gsub("[^[:alnum:]]", "", pub$DOI)
pub_ret$DOI_clean <- gsub("[^[:alnum:]]", "", pub_ret$DOI)

# Faire le match en fonction de la colonne "DOI_clean"
retraction_data <- merge(pub, pub_ret, by = "DOI_clean")

# Supprimer la colonne "DOI_clean" du dataframe fusionné
retraction_data$DOI_clean <- NULL

# gender data

gender_data <- df_nb_aut %>%
  select(publication, female_part, Gtype, nb_aut)

# Joindre les dataframes gender_data et retraction_data par la colonne "publication"
merged_data <- merge(gender_data, retraction_data, by = "publication")

table(merged_data$classe)
## Enregister la table pour ne pas refaire toutes les étapes plus haut pour faire la régression
write.xlsx(merged_data, "/Users/maddi/Documents/Pubpeer project/Pubpeer explo/gender_type_retraction.xlsx")

# Resumé
`%not_in%` <- purrr::negate(`%in%`)

merged_data %>% 
  filter(., classe %not_in% c("Limited or No Information","")) %>%
  .[complete.cases(.$classe), ] %>%
  tbl_summary(
    include = c(publication, Gtype, classe),
    by = Gtype,
    sort = list(everything() ~ "frequency"),
    statistic = list(
      all_continuous() ~ c("{N_obs}") 
    )
  )



# Resumé
`%not_in%` <- purrr::negate(`%in%`)

merged_data %>% 
  filter(., classe %not_in% c("Limited or No Information","")) %>%
  .[complete.cases(.$classe), ] %>%
  tbl_summary(
    include = c(publication, classe, Continent),
    by = Continent,
    sort = list(everything() ~ "frequency"),
    statistic = list(
      all_continuous() ~ c("{N_obs}") 
    )
  )

