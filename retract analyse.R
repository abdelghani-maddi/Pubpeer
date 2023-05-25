#####################################################################
### Analyse de données pour le papier collab h-f et retractations ###
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

df_retract <- read_excel("/Users/maddi/Documents/Pubpeer Gender/df_gender_retract.xlsx") ## bdd sur le genre + bdd retractations (version avril 2023)
reason_agr <- read_excel("~/Documents/Pubpeer Gender/reasons_retract aggreg.xlsx")

## décortiquer les raisons
# éclater les raisons
df_retract <- df_retract %>%
  separate_rows(Reason, sep = ";")%>%
  filter(Reason != "") %>%
  mutate(Reason = str_replace(Reason, "\\+", ""))

# calcul de la fréquence
freq_reasons <- freq(df_retract$Reason) %>%
  data_frame(rownames(.), .)
names(freq_reasons) = c("Reason","nb", "%", "val%")


# write.xlsx(freq_reasons, "/Users/maddi/Documents/Pubpeer Gender/reasons_retract_restreint.xlsx") # écrit directement sur le cloud
# recupérer les données après une aggrégation des raisons à la main
reason_agr <- read_excel("~/Documents/Pubpeer Gender/reasons_retract aggreg.xlsx")

# Matcher les raisons avec la bdd sur le gender

df_retract <- df_retract %>%
  select(publication, Gtype, ID_retractionwatch, Reason) %>%
  unique()
# Merger les deux pour avoir les raisons aggrégées
df_retract_agr <- merge(df_retract, reason_agr, by = "Reason")

# fair les calculs

df_retract_agr %>%
  tbl_summary(
    include = c(reason_aggreg, Gtype),
    by = Gtype,
    sort = list(everything() ~ "frequency"),
    statistic = list(
      all_continuous() ~ c("{N_obs}") 
  )
  ) %>%
  add_overall(last = TRUE) #, col_label = "**Ensemble** (effectif total: {N})")


## exporter pour calculer rapidement les double ratios

reason_agreg <- df_retract_agr %>%
  select(reason_aggreg, Gtype) %>%
  table() %>%
  data.frame()
write.xlsx(reason_agreg, "~/Documents/Pubpeer Gender/reasons_retract_stats.xlsx")


