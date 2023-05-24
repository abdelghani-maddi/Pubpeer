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

### Récupération des données ----
df <- readxl::read_excel("~/Documents/Pubpeer project/Pubpeer explo/tb_finale.xlsx")

df <- readxl::read_excel("D:/bdd/tb_finale_gender.xlsx")

bdd_pub = read.csv2('/Users/maddi/Documents/Pubpeer project/Donnees/Bases PubPeer/PubPeer_Base publications.csv', sep=";")

bdd_pub <- readxl::read_excel("D:/bdd/data_pub.xlsx")

reqsql= paste('select inner_id, publication, "DateCreated" as date_com, html as comm from data_commentaires_2')
# reqsql= paste('select * from data_commentaires_2')
data_comm = dbGetQuery(con,reqsql)

data_comm <- readxl::read_excel("D:/bdd/data_comm2.xlsx")

data_comm <- data_comm %>%
  select(inner_id, publication, DateCreated)
 names(data_comm) = c("inner_id", "publication", "date_com")

# Transformer le format de la date du commentaire
data_comm$date_com <- as.Date.character(data_comm$date_com)
# extraire l'année depuis la colonne "date"
data_comm$annee <- format(data_comm$date_com, "%Y")

# Select donnees d'interet : annee du commentaire pa
data_comm <- data_comm %>%
  select(publication, annee) %>%
  group_by(publication) %>%
  summarise(annee_min = min(annee))






# Nombre par annee de publication
nb_ann <- df_nb_aut %>%
  select(publication, Année, Gtype) %>%
  group_by(Année, Gtype) %>%
  summarise(nb_pub = n_distinct(publication))


# Nombre par annee de commentaire
df_nb_aut <- merge(df_nb_aut, data_comm, by.x = "publication", by.y = "publication", all.x = TRUE) # matcher
## Enregister la table pour ne pas refaire toutes les étapes plus haut pour faire les stats desc
write.xlsx(df_nb_aut, "/Users/maddi/Documents/Pubpeer project/Pubpeer explo/df_nb_aut.xlsx")


nb_ann_com <- df_nb_aut %>%
  select(publication, annee_min, Gtype) %>%
  group_by(df_nb_aut$annee_min, Gtype) %>%
  summarise(nb_pub = n_distinct(publication))

# première représentation graphique
ggplot(nb_ann_com) +
 aes(x = `df_nb_aut$annee_min`, fill = Gtype, weight = nb_pub) +
 geom_bar() +
 scale_fill_hue(direction = 1) +
 labs(x = "Years", y = "# Commented publications", fill = "M-F collaboration type") +
 theme_minimal()

# utiliser plutôt la part selon les types par année

# Aggregate data by "annee_min" and "Gtype" columns and calculate the sum of "nb_aut" for each group
df_nb_aut_agg <- aggregate(publication ~ annee_min + Gtype, data = df_nb_aut, n_distinct)

# Calculate the total number of "nb_aut" for each value of "annee_min"
df_nb_aut_total <- aggregate(publication ~ annee_min, data = df_nb_aut, n_distinct)

# Merge the two dataframes to calculate the proportion of each "Gtype" within each "annee_min" value
df_nb_aut_prop <- merge(df_nb_aut_agg, df_nb_aut_total, by = "annee_min", suffixes = c("_group", "_total"))
df_nb_aut_prop$prop <- (df_nb_aut_prop$publication_group / df_nb_aut_prop$publication_total)*100

# View the resulting dataframe
df_nb_aut_prop %>%
  filter(!(annee_min %in% "2012")) %>%
  ggplot() +
  aes(x = annee_min, y = prop, fill = Gtype) +
  geom_col() +
  scale_fill_viridis_d(option = "viridis", direction = 1) +
  labs(x = "Years", y = "%", fill = "M-F collaboration type") +
  theme_minimal()


# Faire les calculs en incluant le domaine

#  data 
data_dom <- "SELECT * FROM public.data_frac_disc"
data_dom <- dbGetQuery(con,data_dom)


# Aggregate data by "annee_min" and "Gtype" columns and calculate the sum of "nb_aut" for each group
df_nb_aut_agg_dom <- aggregate(publication ~ annee_min + Gtype + Journal_Domaines_WOS, data = df_nb_aut, n_distinct)

# Calculate the total number of "nb_aut" for each value of "annee_min"
df_nb_aut_total_dom <- aggregate(publication ~ annee_min + Journal_Domaines_WOS, data = df_nb_aut, n_distinct)

# Merge the two dataframes to calculate the proportion of each "Gtype" within each "annee_min" value
df_nb_aut_prop_dom <- merge(df_nb_aut_agg, df_nb_aut_total, by = "annee_min", suffixes = c("_group", "_total"))
df_nb_aut_prop_dom$prop <- (df_nb_aut_prop$publication_group / df_nb_aut_prop$publication_total)*100



## Analyser les rétractions

df_retracted <- bdd_pub %>%
  select(publication, Retracted)

df_retracted <- merge(df_nb_aut, df_retracted, by.x = "publication", by.y = "publication", all.x = TRUE) %>% # matcher
  select(publication, Gtype, Retracted)


df_retracted %>%
 filter(Retracted %in% "True") %>%
 ggplot() +
 aes(x = Gtype) +
 geom_bar(fill = "#A20606") +
 labs(x = "M-F collaboration type", 
 y = "# Retracted") +
 theme_minimal()


# Calculate the total number of rows in the dataframe
total <- nrow(df_retracted)

# Create a table of counts for each "Gtype" value
table_all <- table(df_retracted$Gtype)

# Create a table of counts for each "Gtype" value where "Retracted" is "True"
table_retracted <- table(df_retracted$Gtype[df_retracted$Retracted == "True"])

# Calculate the relative proportion of each "Gtype" value in the entire dataframe
prop_all <- table_all / total

# Calculate the relative proportion of each "Gtype" value for "Retracted" = TRUE
prop_retracted <- table_retracted / sum(df_retracted$Retracted == "True")

# Divide the relative proportions for "Retracted" = TRUE by those in the entire dataframe
relative_prop <- as.data.frame(prop_retracted / prop_all)

# Print the resulting table of relative proportions
relative_prop


ggplot(relative_prop) +
  aes(x = Var1, y = Freq) +
  geom_col(fill = "#5B0B6E") +
  labs(
    x = "M-F collaboration type",
    y = "Proportion in all Pubpeer / proportion in retracted publications"
  ) +
  theme_minimal()

###
gender_proba <- readxl::read_excel("~/Documents/Pubpeer project/Pubpeer explo/Gender/gender_proba.xlsx")

df_unnested$prenoms <- tolower(df_unnested$prenoms)

df_stats_glob <- df_unnested %>%
  select(publication, prenoms)

df_stats_glob_m <- merge(df_stats_glob, gender_proba, by.x = "prenoms", by.y = "given_name", all.x = TRUE) # matcher
df_stats_glob_m$gender[df_stats_glob_m$proba < 0.6] <- "unisex" # modifier les probas < 0.6 à Unisexe
df_stats_glob_m$gender[is.na(df_stats_glob_m$gender)] <- "initials" # modifier les "NA" de gender en "initials"

# Statistiques globales sur l'identification du genre -----
## Enregister la table pour ne pas refaire toutes les étapes plus haut pour faire les stats desc
write.xlsx(df_stats_glob_m, "/Users/maddi/Documents/Pubpeer project/Pubpeer explo/df_stats_glob_m.xlsx")

df_stats_glob_m %>% 
  tbl_summary(
    include = c(publication, gender),
    sort = list(everything() ~ "frequency"),
    statistic = list(
      all_continuous() ~ c("{N_obs}") 
    )
  )

# nombre de publications : n_distinct(df_stats_glob_m$publication)

# Statistiques selon retractation
a <- bdd_pub %>%
  select(publication, Retracted) %>%
  right_join(., df_nb_aut, by = "publication") # matcher

stat_retract_coll <- a %>%
  select(publication, Gtype, Retracted)

###
stat_retract_coll %>% 
  tbl_summary(
    include = c(publication, Gtype, Retracted),
    by = Retracted,
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

###
ggplot(relative_prop) +
  aes(x = Var1, y = Freq) +
  geom_col(fill = "#2C81C9") +
  labs(
    x = "Men-women collaboration type",
    y = "% in retracted / % in overall"
  ) +
  coord_flip() +
  theme_light()

### Regression ----

# Extraire l'information sur l'OA
row_data = data.frame(bdd_pub$publication,((bdd_pub$Open_Access)))
names(row_data) = c("publication","oa")
# Diviser la colonne "oa" en deux colonnes distinctes "is_oa" et "oa_status"
row_data <- tidyr::separate(row_data, col = oa, into = c("is_oa", "oa_status"), sep = ", ")
row_data$is_oa <- gsub("{'is_oa': ", "", row_data$is_oa, fixed = TRUE)
row_data$oa_status <- gsub("'oa_status': ", "", row_data$oa_status, fixed = TRUE)


### Ajouter les variables suivantes :
# Nombre de commentaires avant rétraction
# Nombre de commentaires après rétraction
# Raison de la rétraction


# Matcher avec DOI la base des pubilcations commentées et la dernière verion
# de RetractionWath (24 avril 2023) - cf mail de Ivan


pub <- bdd_pub %>%
  select(publication, DOI) 

pub_ret <- rtw %>%
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

## travail sur les commentaires
# Compter le nombre de "inner_id" par "publication" et par "date_com"
count_data <- aggregate(inner_id ~ publication + date_com, data_comm, length)

# Renommer la colonne "inner_id" en "count"
names(count_data)[names(count_data) == "inner_id"] <- "nb_comm"


# Joindre les dataframes count_data et retraction_data par la colonne "publication"
merged_data <- merge(count_data, retraction_data, by = "publication")

merged_data$nb_com_before_retract <- with(merged_data, ifelse(date_com < RetractionDate, nb_comm, 0))
merged_data$nb_com_after_retract <- with(merged_data, ifelse(date_com >= RetractionDate, nb_comm, 0))

retraction_data <- aggregate(cbind(nb_com_before_retract, nb_com_after_retract, nb_comm) ~ publication + ID_retractionwatch + 
                               RetractionDate + OriginalPaperDate + Continent, data = merged_data, FUN = sum)

### Bdd
bdd_regr <- bdd_pub %>%
  left_join(., df_nb_aut[,c(1,2,18,19,20)], by = "publication") %>%
  select(publication, Gtype, nb_aut, Nombre.de.commentaires, Journal_H_index_2021, Journal_Rank_SJR_2021) %>%
  left_join(., row_data, by = "publication") %>%
  left_join(., retraction_data, by = "publication")

## Opérations sur les variables
bdd_regr$is_retracted <- ifelse(is.na(bdd_regr$RetractionDate), 0, 1)


## Recoding bdd_regr$oa_status into bdd_regr$oa_status_rec
bdd_regr$oa_status <- bdd_regr$oa_status %>%
  fct_recode(
    "bronze" = "'bronze'",
    "closed" = "'closed'",
    "gold" = "'gold'",
    "green" = "'green'",
    "hybrid" = "'hybrid'"
  )

## Recoding bdd_regr$is_oa
bdd_regr$is_oa <- bdd_regr$is_oa %>%
  fct_recode(
    NULL = "",
    "0" = "False",
    "1" = "True"
  )

## Recoding bdd_regr$Gtype
bdd_regr$Gtype <- bdd_regr$Gtype %>%
  fct_explicit_na("Gtype_na")
# Pivoter le type de collabe H-F pour n'analyse
bdd_regr <- pivot_wider(bdd_regr, names_from = Gtype, values_from = Gtype, values_fn = list(Gtype = function(x) 1), 
                             values_fill = list(Gtype = 0))

## Recoding bdd_regr$Journal_Région
# bdd_regr$Journal_Région <- bdd_regr$Journal_Région %>%
#   fct_recode(
#     "None" = ""
#   )
## Recoding bdd_regr$Continent
bdd_regr$Continent <- bdd_regr$Continent %>%
  fct_explicit_na("Unknown")

# Pivoter la région pour n'analyse
bdd_regr <- pivot_wider(bdd_regr, names_from = Continent, values_from = Continent, values_fn = list(Continent = function(x) 1), 
                        values_fill = list(Continent = 0))


###
bdd_regr$nb_com_before_retract <- ifelse(is.na(bdd_regr$nb_com_before_retract), bdd_regr$Nombre.de.commentaires, bdd_regr$nb_com_before_retract)
bdd_regr$nb_com_after_retract <- ifelse(is.na(bdd_regr$nb_com_after_retract), bdd_regr$Nombre.de.commentaires, bdd_regr$nb_com_after_retract)
###

###
bdd_regr$nb_com_before_retract <- ifelse(is.na(bdd_regr$nb_com_before_retract), 0, bdd_regr$nb_com_before_retract)
bdd_regr$nb_com_after_retract <- ifelse(is.na(bdd_regr$nb_com_after_retract), 0, bdd_regr$nb_com_after_retract)
###


bdd_regr <- bdd_regr %>%
  select(., -ID_retractionwatch, -nb_comm, -RetractionDate, -OriginalPaperDate)


bdd_regr <- subset(bdd_regr, complete.cases(bdd_regr)) %>%
  select(., -Gtype_na) # toutes leurs valeurs sont nulles car filtre précédent

## récupérer les disciplines
reqsql= paste('select distinct publication, discipline from commentaires_par_discipline')
data_disc = dbGetQuery(con,reqsql)

## ajout des disciplines
bdd_regr <- bdd_regr %>%
  left_join(., data_disc, by = "publication") %>%
  subset(., !is.na(discipline))

# Pivoter la discipline pour n'analyse
bdd_regr <- pivot_wider(bdd_regr, names_from = discipline, values_from = discipline, values_fn = list(discipline = function(x) 1), 
                        values_fill = list(discipline = 0))

## Ajouter le pays de l'auteur correspondant (1er auteur dans la majorité des cas)




# Regression ----

## Enregister la table pour ne pas refaire toutes les étapes plus haut pour faire la régression
write.xlsx(bdd_regr, "/Users/maddi/Documents/Pubpeer project/Pubpeer explo/bdd_reg3.xlsx")

# Ajuster un modèle de régression logistique
modele_logit1 <- glm(is_retracted ~ `Collab. men only` + `Collab. men-women` + `Collab. women only` + `Man alone`,
                    data = bdd_regr, 
                    family = binomial)
summary(modele_logit1)
# Calculer le coefficient de détermination R^2
R2 <- 1 - (modele_logit1$deviance / modele_logit1$null.deviance)
cat("R^2 : ", R2, "\n")


## variables de controle
modele_logit2 <- glm(is_retracted ~ `Collab. men only` + `Collab. men-women` + `Collab. women only` + `Man alone`+
                       log(Nombre.de.commentaires) +
                       # log(1 + nb_com_before_retract)  +
                       # log(1 + nb_com_after_retract)  +
                       log(nb_aut) +
                       log(Journal_Rank_SJR_2021) +
                       is_oa +
                       Asia +
                       Africa
                     , 
                     data = bdd_regr, 
                     family = binomial)
summary(modele_logit2)
# Calculer le coefficient de détermination R^2
R2 <- 1 - (modele_logit2$deviance / modele_logit2$null.deviance)
cat("R^2 : ", R2, "\n")


## variables de controle : discipline
modele_logit3 <- glm(is_retracted ~ bdd_regr$`Collab. men only` + bdd_regr$`Collab. men-women` + bdd_regr$`Collab. women only` + bdd_regr$`Man alone`+
                       # log(bdd_regr$Nombre.de.commentaires) + 
                       log(1 + bdd_regr$nb_com_before_retract)  +
                       log(1 + bdd_regr$nb_com_after_retract)  +
                       log(bdd_regr$nb_aut) + 
                       log(bdd_regr$Journal_Rank_SJR_2021) +
                       is_oa +
                       bdd_regr$Asia +
                       bdd_regr$Africa +
                       bdd_regr$Europe +
                       bdd_regr$`Social Sciences` +
                       bdd_regr$`Physical Sciences` +
                       bdd_regr$Technology +
                       bdd_regr$`Arts Humanities`, 
                     data = bdd_regr, 
                     family = binomial)
summary(modele_logit3)

# Calculer le coefficient de détermination R^2
R2 <- 1 - (modele_logit3$deviance / modele_logit3$null.deviance)
cat("R^2 : ", R2, "\n")

# Afficher l'AIC et le BIC du modèle
AIC(modele_logit3)
BIC(modele_logit3)

## Evaluer la robustesse du modèle


library(rsample)
library(boot)

folds <- vfold_cv(bdd_regr, v = 5, repeats = 1)

results <- folds %>% 
  mutate(model = map(splits, ~glm(Retracted ~ bdd_regr$`Collab. men only` + bdd_regr$`Collab. men-women` + bdd_regr$`Collab. women only` + bdd_regr$`Man alone`, data = analysis(.), family = binomial)),
         predict = map(model, ~predict(object = .x, newdata = assessment(.), type = "response")),
         accuracy = map2_dbl(predict, splits$assessment, ~mean((.x > 0.5) == .y$Retracted)))

###


reqsql= paste('select inner_id, publication, "DateCreated" as date_com, html as comm, user from data_commentaires_2')
data_comm = dbGetQuery(con,reqsql)
# Transformer le format de la date du commentaire
data_comm$date_com <- as.Date.character(data_comm$date_com)
# extraire l'année depuis la colonne "date"
data_comm$annee <- format(data_comm$date_com, "%Y")
data_comm <- data_comm %>%
  select(inner_id, publication, annee) %>%
  unique()


# récupérer commentateur
reqsql2= paste('select inner_id, publication, html as comm, "Commentateurs_recodés" as commentateur from data_commentaires')
data_comm2 = dbGetQuery(con,reqsql2)


data_comm <- left_join(data_comm2, data_comm, by = c("inner_id","publication"))

gt_table <- data_comm %>%
  tbl_summary(
    include = c(publication, commentateur, annee),
    by = annee,
    sort = list(everything() ~ "frequency"),
    statistic = list(
      all_continuous() ~ c("{N_obs}") 
    )
  )

# Export to Excel ----

# convert to tibble, then write to xlsx
gt_table %>%
  gtsummary::as_tibble() %>% 
  writexl::write_xlsx(., "/Users/maddi/Documents/Pubpeer project/Pubpeer explo/distrib_commentateur.xlsx")

# Or use `as_hux_xlsx()`
gt_table %>% 
  as_hux_xlsx("example_gtsummary2.xlsx")

# Export to RTF ----
gt_table %>%
  gtsummary::as_gt() %>% 
  gt::gtsave(., "example_gtsummary3.rtf")







# nombre de publications : n_distinct(df_stats_glob_m$publication)

# Statistiques selon retractation
a <- bdd_regr %>%
  select(publication, is_retracted) %>%
  right_join(., df_nb_aut, by = "publication") # matcher

stat_retract_coll <- a %>%
  select(publication, Gtype, is_retracted)

###
## Recoding stat_retract_coll$is_retracted

stat_retract_coll$is_retracted <- factor(stat_retract_coll$is_retracted) %>%
  fct_explicit_na("0")

stat_retract_coll %>% 
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

###

# Calculate the total number of rows in the dataframe
total <- nrow(stat_retract_coll)

# Create a table of counts for each "Gtype" value
table_all <- table(stat_retract_coll$Gtype)

# Create a table of counts for each "Gtype" value where "Retracted" is "True"
table_retracted <- table(stat_retract_coll$Gtype[stat_retract_coll$is_retracted == 1])

# Calculate the relative proportion of each "Gtype" value in the entire dataframe
prop_all <- table_all / total

# Calculate the relative proportion of each "Gtype" value for "Retracted" = TRUE
prop_retracted <- table_retracted / sum(stat_retract_coll$is_retracted == 1)

# Divide the relative proportions for "Retracted" = TRUE by those in the entire dataframe
relative_prop <- as.data.frame(prop_retracted / prop_all)

# Print the resulting table of relative proportions
relative_prop



ggplot(relative_prop) +
  aes(x = Var1, y = Freq) +
  geom_col(fill = "#2C81C9") +
  labs(
    x = "Men-women collaboration type",
    y = "% in retracted / % in overall"
  ) +
  coord_flip() +
  theme_light()





###

doub_retrac <- rtw %>%
  select(`Record ID`, OriginalPaperDOI) %>%
  unique() %>%
  aggregate(`Record ID` ~ OriginalPaperDOI, data = ., FUN = length)


