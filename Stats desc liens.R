rm(list = ls()) #supprimer tous les objets 


library(tidyverse)
library(questionr)
library(RPostgres)
library(lubridate)
library(urltools)
library(TraMineR)
library(cluster)
library(seqhandbook)
library(ade4)
library(explor)
library(FactoMineR)
library(factoextra)
library(labelled)
library(openxlsx)
library(openxlsx2)
library(officer)

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

# donnees depuis local w
data_urls <- readxl::read_excel("~/Documents/Pubpeer project/Pubpeer explo/donnees_urls_fin.xlsx")


# stats par annee
theme_gtsummary_language(language = "fr", decimal.mark = ",", big.mark = " ")
#data_urls$publication <- as.character(data_urls$publication)

sect_properties <- prop_section(
  page_size = page_size(
    orient = "landscape",
    width = 8.3, height = 11.7
  ),
  type = "continuous",
  page_margins = page_mar()
)


nb_ann_typ <-
  data_urls %>% tbl_summary(
  include = c(typo),
  by = annee,
  percent = "column",
  sort = list(everything() ~ "frequency")
  ) %>%
  add_overall(last = TRUE, col_label = "**Ensemble** (effectif total: {N})") %>%
  as_flex_table() %>%
  flextable::save_as_docx(., path = "/Users/maddi/Documents/Pubpeer project/Pubpeer explo/urls_par_annee_type.docx",
                          pr_section = sect_properties)

