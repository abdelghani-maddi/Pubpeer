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

df_gender <- read_excel("~/Documents/Pubpeer Gender/tb_finale_gender.xlsx") ## bdd sur le genre
df_retract <- read_excel("~/Documents/Pubpeer Gender/RWDBDNLD04242023.xlsx", sheet = "RWDBDNLD04242023") ### bdd retractations (version avril 2023)
df_pub = read.csv2('/Users/maddi/Documents/Pubpeer project/Donnees/Bases PubPeer/PubPeer_Base publications.csv', sep=";")
reqsql= paste('select inner_id, publication, "DateCreated" as date_com, html as comm from data_commentaires_2')
df_comm = dbGetQuery(con,reqsql)
# Transformer le format de la date du commentaire
df_comm$date_com <- as.Date.character(df_comm$date_com)
# extraire l'année depuis la colonne "date"
df_comm$annee <- format(df_comm$date_com, "%Y")

## vérif nb auteurs
aut <- df_gender %>%
  select(publication, nb_aut) %>%
  unique()

aut_dist <- freq(aut)

### Statistiques descriptives sur l'identification du genre ----
#`%not_in%` <- purrr::negate(`%in%`)

## Stats globales sur l'identification du sexe
df_gender %>% 
  tbl_summary(
    include = c(publication, gender),
    sort = list(everything() ~ "frequency"),
    statistic = list(
      all_continuous() ~ c("{N_obs}") 
    )
  )

df_gender %>% 
  tbl_summary(
    include = c(publication, g_prob_06),
    sort = list(everything() ~ "frequency"),
    statistic = list(
      all_continuous() ~ c("{N_obs}") 
    )
  )


### Distribution h-f selon les rétractations ----
# Matcher avec DOI la base des pubilcations commentées et la dernière verion

# de RetractionWath (24 avril 2023) - cf mail de Ivan
pub <- df_pub %>%
  select(publication, DOI) 
pub_ret <- df_retract %>%
  select(`Record ID`, OriginalPaperDOI, RetractionDate, OriginalPaperDate, Reason, Continent) #%>%
# filter(. , `Record ID` != "36438")
names(pub_ret) = c("ID_retractionwatch", "DOI","RetractionDate","OriginalPaperDate","Reason", "Continent")
# Supprimer les caractères spéciaux et les espaces des colonnes "DOI" des dataframes pub et pub_ret
pub$DOI_clean <- gsub("[^[:alnum:]]", "", pub$DOI)
pub_ret$DOI_clean <- gsub("[^[:alnum:]]", "", pub_ret$DOI)
# Faire le match en fonction de la colonne "DOI_clean"
retraction_data <- merge(pub, pub_ret, by = "DOI_clean")
# Supprimer la colonne "DOI_clean" du dataframe fusionné
retraction_data$DOI_clean <- NULL

# exclure les publications sans genre identifié
df_gender_filtr <- subset(df_gender, (publication %in% df_gender$publication[df_gender$g_prob_06 %in% c("male", "female")]))

# se limiter aux publications avec au moins un auteur dont le sexe est identifié
df <- df_gender_filtr %>%
  left_join(., retraction_data, by = "publication")

# ajouter un flag pour la rétraction
df$is_retracted <- ifelse(is.na(df$RetractionDate), 0, 1)
write.xlsx(df, "/Users/maddi/Documents/Pubpeer Gender/df_gender_retract.xlsx") # écrit directement sur le cloud



df <- df %>%
  select(publication, Gtype, is_retracted) %>%
  unique()


df %>% 
  tbl_summary(
    include = c(publication, Gtype, is_retracted),
    by = is_retracted,
    sort = list(everything() ~ "frequency"),
    statistic = list(
      all_continuous() ~ c("{N_obs}") 
    )
  ) %>%
  add_overall()  %>%
  # adding spanning header
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Is retracted**") %>%
  add_p() %>%
  separate_p_footnotes()




### Part dans les rétractations / part dans le total (par type de collab) ----

# Calculate the total number of rows in the dataframe
total <- nrow(df)

# Create a table of counts for each "Gtype" value
table_all <- table(df$Gtype)

# Create a table of counts for each "Gtype" value where "Retracted" is "True"
table_retracted <- table(df$Gtype[df$is_retracted == 1])

# Calculate the relative proportion of each "Gtype" value in the entire dataframe
prop_all <- table_all / total

# Calculate the relative proportion of each "Gtype" value for "Retracted" = TRUE
prop_retracted <- table_retracted / sum(df$is_retracted == 1)

# Divide the relative proportions for "Retracted" = TRUE by those in the entire dataframe
relative_prop <- as.data.frame(prop_retracted / prop_all)

# Print the resulting table of relative proportions
relative_prop


## représentation graphique
ggplot(relative_prop, aes(x = reorder(Var1, Freq), y = Freq)) + 
  geom_bar(stat = "identity") +
  geom_col(fill = "#2C81C9") +
  labs(
    x = "Men-women collaboration type",
    y = "% in retracted / % in overall"
  ) +
  coord_flip() +
  theme_light()

### Régression logistique ----

df_retract <- read_excel("~/Documents/Pubpeer Gender/RWDBDNLD04242023.xlsx", sheet = "RWDBDNLD04242023") ### bdd retractations (version avril 2023)


