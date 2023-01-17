# Test Pubpeer
rm(list = ls()) #supprimer tous les objets 

### packages
library(dplyr)
library(tidyr)
library(Matrix)
library(data.table)
library(tidyverse)
library(trimmer)
library(DescTools)
library(questionr)
library(DBI)
library(RPostgres)
library(lubridate)
library(timechange)

### Espace de travail
setwd('/Users/maddi/Documents/Pubpeer project/Pubpeer explo')

con<-dbConnect(RPostgres::Postgres())

db <- 'SKEPTISCIENCE'  #provide the name of your db
host_db <- 'localhost' # server
db_port <- '5433'  # port DBA
db_user <- 'postgres' # nom utilisateur  
db_password <- 'Maroua1912'
con <- dbConnect(RPostgres::Postgres(), dbname = db, host=host_db, port=db_port, user=db_user, password=db_password)  
# Test connexion
dbListTables(con) 

####

### Recuperation des donnees (envoyees par Emmanuel)
bdd_pub = read.csv2('/Users/maddi/Documents/Pubpeer project/Donnees/Bases PubPeer/PubPeer_Base publications.csv', sep=";")
bdd_com = read.csv2('/Users/maddi/Documents/Pubpeer project/Donnees/Bases PubPeer/PubPeer_Base commentaires.csv', sep=";")


### Quelques statistiques descriptives
n_distinct(bdd_com$markdown) #70621 commentaires differents.

### exporter en txt les commentaires pour vosviewer
write.csv(bdd_com$markdown, file ="comm pr vosv")

############################
############################
### Dimension disciplinaire : Journal_Categories_WOS
############################
############################

row_data = data.frame(bdd_pub$publication,((bdd_pub$Journal_Categories_WOS)))
names(row_data) = c("id","wos_cat")

clean_data =  data.frame(row_data$id, gsub("  ", " ",(str_replace_all((str_split(row_data$wos_cat, '",' , simplify = TRUE)), "[[:punct:]]", ""))))
names(clean_data) = c("id","discipline")

data_JCW <- subset(clean_data, discipline != "")

describe(data_JCW)
write.csv(data_JCW, file ="data_JCW")

## Mettre la table sur PostgresSQL
names(data_JCW) = c("id","jcw")
dbWriteTable(con, "data_jcw", data_JCW)

############################
############################

############################
############################
### Dimension disciplinaire : Journal_Domaines
############################
############################

row_data = data.frame(bdd_pub$publication,((bdd_pub$Journal_Domaines_WOS)))
names(row_data) = c("id","JDW")

clean_data =  data.frame(row_data$id, gsub("  ", " ",(str_replace_all((str_split(row_data$JDW, '",' , simplify = TRUE)), "[[:punct:]]", ""))))
names(clean_data) = c("id","JDW")

data_JD <- subset(clean_data, JDW != "")

describe(data_JD)
describe(data_JD$JD)

## Mettre la table sur PostgresSQL
names(data_JD) = c("id","jdw")
dbWriteTable(con, "data_jdw", data_JD)

############################
############################

############################
############################
### Dimension affiliation : institutions
############################
############################

row_data = data.frame(bdd_pub$publication,((bdd_pub$Institutions)))
names(row_data) = c("id","institution")

clean_data =  data.frame(row_data$id, gsub("  ", " ",(str_replace_all((str_split(row_data$institution, "'," , simplify = TRUE)), "[[:punct:]]", ""))))
names(clean_data) = c("id","institution")

data_instit <- subset(clean_data, institution != "")

describe(data_instit)


## Mettre la table sur PostgresSQL
names(data_instit) = c("id","instit")
dbWriteTable(con, "data_instit", data_instit)

############################
############################

############################
############################
### Dimension affiliation : pays
############################
############################

row_data = data.frame(bdd_pub$publication,((bdd_pub$Pays_institution)))
names(row_data) = c("id","pays")

clean_data =  data.frame(row_data$id, gsub("  ", " ",(str_replace_all((str_split(row_data$pays, "'," , simplify = TRUE)), "[[:punct:]]", ""))))
names(clean_data) = c("id","pays")

data_pays <- subset(clean_data, pays != "")

describe(data_pays)

## Mettre la table sur PostgresSQL
names(data_pays) = c("id","pays")
dbWriteTable(con, "data_pays", data_pays)


############################
############################

############################
############################
### Dimension annee et infos publi comm
############################
############################

data_annee = data.frame(bdd_pub$publication, bdd_pub$Date, bdd_pub$Année, bdd_pub$Type, bdd_pub$Nombre.de.commentaires, bdd_pub$Retracted)
names(data_annee) = c("id", "date", "annee", "doctype","nb_comm", "retracred")     

## Mettre la table sur PostgresSQL
dbWriteTable(con, "data_annee", data_annee)


############################
############################

############################
############################
### Table des identifiants
############################
############################

data_ID = data.frame(bdd_pub[,1:11], bdd_pub$mag_id_Source, bdd_pub$id_ror)
names(data_ID) = c("ID_EXTRACT_COM", "ID", "ORIGINAL_ID", "PUBPEER_ID", "PMID_PUBPEER_MIRROR", "PMID_OPENALEX", "PMCID_PUBPEER_MIRROR", "PMCID_OPENALEX", "OPENALEX_ID","URL_ARTICLE", "DOI", "MAGID_SOURCE", "ID_ROR")

## Mettre la table sur PostgresSQL
dbWriteTable(con, "DATA_IDENTIFIANTS", data_ID)


############################
############################
### Dimension Citations : nombre total et citations par annee
############################
############################

row_data = data.frame(bdd_pub$publication, bdd_pub$Cité_année)
names(row_data) = c("id","cit")

clean_data =  data.frame(row_data$id, gsub("  ", " ",(str_replace_all((str_split(row_data$cit, "year" , simplify = TRUE)), "[[:punct:]]", ""))))
names(clean_data) = c("id","cit")
data_cit <- subset(clean_data, cit!= "")
data_cit_fin = separate(data_cit, cit, sep = "citedbycount", into = c("ANNEE", "NB_CIT"))

describe(data_cit_fin)

## Mettre la table sur PostgresSQL
names(data_cit_fin) = c("id","annee_citation", "nb_citations")
dbWriteTable(con, "data_citations", data_cit_fin)


############################
############################                
## Table des commentaires
############################
dbWriteTable(con, "data_commentaires", bdd_com)
dbWriteTable(con, "data_pub", bdd_pub)

## Transformer les donnees de dates de commentaires
date_comm1 = data.frame(bdd_com$publication, bdd_com$inner_id, as.Date.character(dmy_hm(bdd_com$DateCreated)), as.Date.character(dmy_hm(bdd_com$accepted_at)), as.Date.character(dmy_hm(bdd_com$updated_at)))
names(date_comm1) = c(    'id_pub',            'id_comm',             'date_creation',                          'date_acceptation',                             'date_mise_a_jour')

date_comm = data.frame(date_comm1, year(date_comm1$date_creation), year(date_comm1$date_acceptation), year(date_comm1$date_mise_a_jour))
names(date_comm)[6:8] = c("annee_comm", "annee_accept_comm", "annee_maj_com")

dbWriteTable(con, "data_commentaires_annees", date_comm)

############################






