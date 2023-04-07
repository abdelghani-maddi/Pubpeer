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
bdd_pub = read.csv2('/Users/maddi/Documents/Pubpeer project/Donnees/Bases PubPeer/PubPeer_Base publications.csv', sep=";")

reqsql= paste('select inner_id, publication, "DateCreated" as date_com, html as comm, user from data_commentaires_2')
# reqsql= paste('select * from data_commentaires_2')
data_comm = dbGetQuery(con,reqsql)
# Transformer le format de la date du commentaire
data_comm$date_com <- as.Date.character(data_comm$date_com)
# extraire l'année depuis la colonne "date"
data_comm$annee <- format(data_comm$date_com, "%Y")

# Select donnees d'interet : annee du commentaire pa
data_comm <- data_comm %>%
  select(publication, annee) %>%
  group_by(publication) %>%
  summarise(annee_min = min(annee))



### Extraction colonnes d'intérêt et suppression des autres données
aut <- bdd_pub %>%
  select(publication, Auteurs, Pays_institution, Nombre.de.commentaires, Année, starts_with("Journal"))

# Pivoter les noms des auteurs par autant de lignes que d'auteurs et dupliquer l'identifiant "publication"
df_unnested <- aut %>%
  mutate(Auteurs_extraits = str_extract_all(Auteurs, "'([\\p{L}\\s-]*)'")) %>%
  unnest(Auteurs_extraits) %>%
  select(-Auteurs)
# Renommer la nouvelle colonne "Auteurs"
names(df_unnested)[names(df_unnested) == "Auteurs_extraits"] <- "Auteur"
# Supprimer les guillemets simples des noms d'auteurs
df_unnested$Auteur <- gsub("'", "", df_unnested$Auteur)

# Extraire le prénom de chaque nom d'auteur
df_unnested$prenoms <- sapply(strsplit(df_unnested$Auteur, " "), function(x) x[1])


# Compter le nombre d'auteurs par publication
nbaut <- df_unnested %>%
  group_by(publication) %>%
  summarise(nb_aut = n_distinct(Auteur))

# Ajouter à la table des publications
df_nb_aut <- merge(df, nbaut, by.x = "publication", by.y = "publication", all.x = TRUE) # matcher


# Etudier l'évolution par type par année, toutes disciplines confondues
# Ajouter la variable
df_nb_aut$Gtype <- ifelse(df_nb_aut$female_part == 0 & df_nb_aut$nb_aut == 1, "Man alone", 
                          ifelse(df_nb_aut$female_part == 1 & df_nb_aut$nb_aut == 1, "Woman alone",
                                 ifelse(df_nb_aut$female_part == 0 & df_nb_aut$nb_aut > 1, "Collab. men only",
                                        ifelse(df_nb_aut$female_part == 1 & df_nb_aut$nb_aut > 1, "Collab. women only",
                                               ifelse(df_nb_aut$female_part > 0 & df_nb_aut$female_part < 1 & df_nb_aut$nb_aut > 1, "Collab. men-women", NA)
                                        )
                                 )
                          )
)
# Nombre par annee de publication
nb_ann <- df_nb_aut %>%
  select(publication, Année, Gtype) %>%
  group_by(Année, Gtype) %>%
  summarise(nb_pub = n_distinct(publication))


# Nombre par annee de commentaire
df_nb_aut <- merge(df_nb_aut, data_comm, by.x = "publication", by.y = "publication", all.x = TRUE) # matcher


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

### Bdd
bdd_regr <- bdd_pub %>%
  left_join(., df_nb_aut[,c(1,2,18,19,20)], by = "publication") %>%
  select(publication, Retracted, Gtype, nb_aut, Nombre.de.commentaires, Journal_H_index_2021, Journal_Rank_SJR_2021, Journal_Région) %>%
  left_join(., row_data, by = "publication")

## Opérations sur les variables
bdd_regr$Retracted <- factor(bdd_regr$Retracted)
bdd_regr$is_oa <- factor(bdd_regr$is_oa)

## Recoding bdd_regr$Retracted
bdd_regr$Retracted_rec <- bdd_regr$Retracted %>%
  fct_recode(
    "0" = "False",
    NULL = "None",
    "1" = "True"
  )
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
bdd_regr$Journal_Région <- bdd_regr$Journal_Région %>%
  fct_recode(
    "None" = ""
  )

# Pivoter la région pour n'analyse
bdd_regr <- pivot_wider(bdd_regr, names_from = Journal_Région, values_from = Journal_Région, values_fn = list(Journal_Région = function(x) 1), 
                        values_fill = list(Journal_Région = 0))

bdd_regr <- subset(bdd_regr, complete.cases(bdd_regr)) %>%
  select(., -None, -Gtype_na) # toutes leurs valeurs sont nulles car filtre précédent

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


# Regression ----

## Enregister la table pour ne pas refaire toutes les étapes plus haut pour faire la régression
write.xlsx(bdd_regr, "/Users/maddi/Documents/Pubpeer project/Pubpeer explo/bdd_reg.xlsx")

# Ajuster un modèle de régression logistique
modele_logit1 <- glm(Retracted ~ bdd_regr$`Collab. men only` + bdd_regr$`Collab. men-women` + bdd_regr$`Collab. women only` + bdd_regr$`Man alone`,
                    data = bdd_regr, 
                    family = binomial)
summary(modele_logit1)
# Calculer le coefficient de détermination R^2
R2 <- 1 - (modele_logit1$deviance / modele_logit1$null.deviance)
cat("R^2 : ", R2, "\n")


## variables de controle
modele_logit2 <- glm(Retracted ~ bdd_regr$`Collab. men only` + bdd_regr$`Collab. men-women` + bdd_regr$`Collab. women only` + bdd_regr$`Man alone`+
                       log(bdd_regr$Nombre.de.commentaires) + 
                       log(bdd_regr$nb_aut) + 
                       log(bdd_regr$Journal_Rank_SJR_2021) +
                       is_oa +
                       bdd_regr$`Asiatic Region` +
                       bdd_regr$`Northern America` +
                       bdd_regr$`Western Europe`, 
                     data = bdd_regr, 
                     family = binomial)
summary(modele_logit2)
# Calculer le coefficient de détermination R^2
R2 <- 1 - (modele_logit2$deviance / modele_logit2$null.deviance)
cat("R^2 : ", R2, "\n")


## variables de controle : discipline
modele_logit3 <- glm(Retracted ~ bdd_regr$`Collab. men only` + bdd_regr$`Collab. men-women` + bdd_regr$`Collab. women only` + bdd_regr$`Man alone`+
                       log(bdd_regr$Nombre.de.commentaires) + 
                       log(bdd_regr$nb_aut) + 
                       log(bdd_regr$Journal_Rank_SJR_2021) +
                       is_oa +
                       bdd_regr$`Asiatic Region` +
                       bdd_regr$`Northern America` +
                       bdd_regr$`Western Europe` +
                       bdd_regr$`Social Sciences` +
                       bdd_regr$Multidisciplinary +
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


