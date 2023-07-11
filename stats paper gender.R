#####################################################################
###              Analyse de données pour le papier gender         ###
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

### Connexion à postgresql ----

con<-dbConnect(RPostgres::Postgres())

db <- 'SKEPTISCIENCE'  #provide the name of your db
host_db <- 'localhost' # server
db_port <- '5433'  # port DBA
db_user <- 'postgres' # nom utilisateur  
db_password <- 'Maroua1912'
con <- dbConnect(RPostgres::Postgres(), dbname = db, host=host_db, port=db_port, user=db_user, password=db_password) 
# Test connexion
dbListTables(con)

### Lecture des données ----
df_gender <- read_excel("~/Documents/Pubpeer Gender/tb_finale_gender.xlsx") ## bdd sur le genre

df_retract <- read_excel("/Users/maddi/Documents/Pubpeer Gender/df_gender_retract.xlsx") ## bdd sur le genre + bdd retractations (version avril 2023)
#write.xlsx(df_retract, "/Users/maddi/Documents/Pubpeer Gender/df_retract.xlsx")
reason_agr <- read_excel("~/Documents/Pubpeer Gender/reasons_retract aggreg.xlsx")

bdd_pub = read.csv2('/Users/maddi/Documents/Pubpeer project/Donnees/Bases PubPeer/PubPeer_Base publications.csv', sep=";")


# Table 1: Number and share of gender types, according to the set probability threshold
   # NB : la différence entre df_gender et df_retract est que df_retract ne contient pas les lignes pour lesquelles legender <0.6
df_gender %>%
  select(publication, order_auteur, starts_with("g_prob")) %>%
  unique() %>%
  tbl_summary(
    include = c("g_prob_06", "g_prob_07", "g_prob_08", "g_prob_09", "g_prob_100")
  )


### Stats par genre et discipline
df <- df_retract %>%
  select(publication, g_prob_06, frac_disc, disc) %>%
  subset(., !is.na(.$disc) & g_prob_06 %in% c('female','male')) 

df <- df_gender %>%
  select(publication, order_auteur, g_prob_06) %>%
  unique()


tbl_summary(df_gender,
            include = c("publication","g_prob_06")
            )  


df <- df_retract %>%
  select(publication, g_prob_06) %>%
  subset(., g_prob_06 %in% c('female','male')) 

tbl_summary(df,
            include = c("publication","g_prob_06"),
)  


df_counts <- df %>%
  group_by(disc) %>%
  summarise(moyenne_female_part = mean(female_part, na.rm = TRUE),
            somme_frac_disc = sum(frac_disc),
            nombre_lignes = n())


df_counts <- part_f_disc %>%
  group_by(disc) %>%
  summarise(moyenne = mean(female_part, na.rm = TRUE),
            nombre_lignes = n())



