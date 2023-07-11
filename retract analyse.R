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

df_retract <- read_excel("/Users/maddi/Documents/Pubpeer Gender/df_gender_retract.xlsx") ## bdd sur le genre + bdd retractations (version avril 2023)
#write.xlsx(df_retract, "/Users/maddi/Documents/Pubpeer Gender/df_retract.xlsx")
reason_agr <- read_excel("~/Documents/Pubpeer Gender/reasons_retract aggreg.xlsx")

bdd_pub = read.csv2('/Users/maddi/Documents/Pubpeer project/Donnees/Bases PubPeer/PubPeer_Base publications.csv', sep=";")


## décortiquer les raisons
# éclater les raisons
df_retract_reason <- df_retract %>%
  separate_rows(Reason, sep = ";")%>%
  filter(Reason != "") %>%
  mutate(Reason = str_replace(Reason, "\\+", ""))


# calcul de la fréquence
freq_reasons <- df_retract_reason %>%
  select(publication, Reason) %>%
  unique() 
freq_reasons <- freq(freq_reasons$Reason) %>%
  data_frame(rownames(.), .)
names(freq_reasons) = c("Reason","nb", "%", "val%")


# write.xlsx(freq_reasons, "/Users/maddi/Documents/Pubpeer Gender/reasons_retract_restreint.xlsx") # écrit directement sur le cloud
# recupérer les données après une aggrégation des raisons à la main
reason_agr <- read_excel("~/Documents/Pubpeer Gender/reasons_retract aggreg.xlsx")

# Matcher les raisons avec la bdd sur le gender
df_retract_reason <- df_retract_reason %>%
  select(publication, Gtype2, ID_retractionwatch, Reason) %>%
  unique()

# Merger les deux pour avoir les raisons aggrégées
df_retract_agr <- merge(df_retract_reason, reason_agr, by = "Reason")


# coocuurences des raisons (pour aider à l'interprétation)
# Calculer les co-occurrences des raisons
df_cooccurrences <- df_retract_agr %>%
  select(publication, reason_aggreg) %>%
  unique() %>%
  inner_join(df_retract_agr, by = "publication") %>%
  filter(reason_aggreg.x != reason_aggreg.y) %>%
  group_by(reason_aggreg.x, reason_aggreg.y) %>%
  summarise(nb_publication = n()) %>%
  ungroup()

# Renommer les colonnes
colnames(df_cooccurrences) <- c("Reason_1", "Reason_2", "nb_publication")
# Exporter
write.xlsx(df_cooccurrences, "~/Documents/Pubpeer Gender/rdf_cooccurrences_reasons.xlsx")


# fair les calculs

df_retract_agr %>%
  tbl_summary(
    include = c(reason_aggreg, Gtype2),
    by = Gtype2,
    sort = list(everything() ~ "frequency"),
    statistic = list(
      all_continuous() ~ c("{N_obs}") 
  )
  ) %>%
  add_overall(last = TRUE) #, col_label = "**Ensemble** (effectif total: {N})")


## exporter pour calculer rapidement les double ratios

reason_agreg <- df_retract_agr %>%
  select(reason_aggreg, Gtype2) %>%
  table() %>%
  data.frame()

write.xlsx(reason_agreg, "~/Documents/Pubpeer Gender/reasons_retract_stats.xlsx")



######
df <- df_retract %>%
  select(publication, Gtype2, is_retracted) %>%
  filter(!is.na(Gtype2)) %>%
  unique()


df %>% 
  tbl_summary(
    include = c(publication, Gtype2, is_retracted),
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
table_all <- table(df$Gtype2)

# Create a table of counts for each "Gtype" value where "Retracted" is "True"
table_retracted <- table(df$Gtype2[df$is_retracted == 1])

# Calculate the relative proportion of each "Gtype" value in the entire dataframe
prop_all <- table_all / total

# Calculate the relative proportion of each "Gtype" value for "Retracted" = TRUE
prop_retracted <- table_retracted / sum(df$is_retracted == 1)

# Divide the relative proportions for "Retracted" = TRUE by those in the entire dataframe
relative_prop <- as.data.frame(prop_retracted / prop_all)

# Print the resulting table of relative proportions
relative_prop

write.xlsx(relative_prop, "~/Documents/Pubpeer Gender/relative_prop.xlsx")


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

#### Regression logistique ----
df_retract <- read_excel("/Users/maddi/Documents/Pubpeer Gender/df_gender_retract.xlsx") ## bdd sur le genre + bdd retractations (version avril 2023)

# bdd_regression
bdd_reg1 <- df_retract %>%
  select(publication, nb_aut, Gtype2, is_retracted)


# Extraire l'information sur l'OA
# bdd_pub = read.csv2('/Users/maddi/Documents/Pubpeer project/Donnees/Bases PubPeer/PubPeer_Base publications.csv', sep=";")
row_data = data.frame(bdd_pub$publication,((bdd_pub$Open_Access)))
names(row_data) = c("publication","oa")
# Diviser la colonne "oa" en deux colonnes distinctes "is_oa" et "oa_status"
row_data <- tidyr::separate(row_data, col = oa, into = c("is_oa", "oa_status"), sep = ", ")
row_data$is_oa <- gsub("{'is_oa': ", "", row_data$is_oa, fixed = TRUE)
row_data$oa_status <- gsub("'oa_status': ", "", row_data$oa_status, fixed = TRUE)

row_data <- row_data %>%
  select(publication, is_oa) %>%
  unique()

## 
bdd_reg <- bdd_reg1 %>%
  left_join(., row_data, by = "publication") %>%
  unique()


## Recoding bdd_regr$is_oa
bdd_reg$is_oa <- bdd_reg$is_oa %>%
  fct_recode(
    NULL = "",
    "0" = "False",
    "1" = "True"
  )

# Pivoter le type de collabe H-F pour n'analyse
bdd_regr <- pivot_wider(bdd_reg, names_from = Gtype2, values_from = Gtype2, values_fn = list(Gtype2 = function(x) 1), 
                        values_fill = list(Gtype2 = 0))


## récupérer les disciplines
# reqsql= paste('select distinct publication, discipline from commentaires_par_discipline')
# data_disc = dbGetQuery(con,reqsql)


row_data_dis = data.frame(bdd_pub$publication,((bdd_pub$Journal_Domaines_WOS)))
names(row_data_dis) = c("publication","discipline")

clean_data =  data.frame(row_data_dis$publication, gsub("  ", " ",(str_replace_all((str_split(row_data_dis$discipline, '",' , simplify = TRUE)), "[[:punct:]]", ""))))
names(clean_data) = c("publication","discipline")

data_JD <- subset(clean_data, discipline != "")


## ajout des disciplines
bdd_regr <- bdd_regr %>%
  left_join(., data_JD, by = "publication") %>%
  subset(., !is.na(.$discipline))


## Recoding bdd_regr$discipline
bdd_regr$discipline <- bdd_regr$discipline %>%
  fct_recode(
    "Arts Humanities" = " Arts Humanities",
    "Life Sciences Biomedicine" = " Life Sciences Biomedicine",
    "Multidisciplinary" = " Multidisciplinary",
    "Physical Sciences" = " Physical Sciences",
    "Social Sciences" = " Social Sciences",
    "Technology" = " Technology"
  )


# Pivoter la discipline pour n'analyse
bdd_regr <- pivot_wider(bdd_regr, names_from = discipline, values_from = discipline, values_fn = list(discipline = function(x) 1), 
                        values_fill = list(discipline = 0))



# Ajuster un modèle de régression logistique
modele_logit1 <- glm(is_retracted ~ `Collab. men-women m lead` + `Man alone` + `Collab. men only` + `Woman alone` + `Collab. men-women w lead` ,
                     data = bdd_regr, 
                     family = binomial)
summary(modele_logit1)
# Calculer le coefficient de détermination R^2
R2 <- 1 - (modele_logit1$deviance / modele_logit1$null.deviance)
cat("R^2 : ", R2, "\n")
# Afficher l'AIC et le BIC du modèle
AIC(modele_logit1)
BIC(modele_logit1)


results <- broom::tidy(modele_logit1)
write.xlsx(results, "~/Documents/Pubpeer Gender/modele_logit1.xlsx")


## variables de controle
modele_logit2 <- glm(is_retracted ~ `Collab. men-women m lead` + `Man alone` + `Collab. men only` + `Woman alone` + `Collab. men-women w lead` +
                       log(nb_aut) 
                     , 
                     data = bdd_regr, 
                     family = binomial)
summary(modele_logit2)
# Calculer le coefficient de détermination R^2
R2 <- 1 - (modele_logit2$deviance / modele_logit2$null.deviance)
cat("R^2 : ", R2, "\n")
# Afficher l'AIC et le BIC du modèle
AIC(modele_logit2)
BIC(modele_logit2)

results <- broom::tidy(modele_logit2)
write.xlsx(results, "~/Documents/Pubpeer Gender/modele_logit2.xlsx")


## variables de controle
modele_logit2b<- glm(is_retracted ~ `Collab. men-women m lead` + `Man alone` + `Collab. men only` + `Woman alone` + `Collab. men-women w lead` +
                       log(nb_aut) +
                       is_oa 
                     , 
                     data = bdd_regr, 
                     family = binomial)
summary(modele_logit2b)
# Calculer le coefficient de détermination R^2
R2 <- 1 - (modele_logit2b$deviance / modele_logit2b$null.deviance)
cat("R^2 : ", R2, "\n")
# Afficher l'AIC et le BIC du modèle
AIC(modele_logit2b)
BIC(modele_logit2b)

results <- broom::tidy(modele_logit2b)
write.xlsx(results, "~/Documents/Pubpeer Gender/modele_logit2b.xlsx")


## variables de controle : discipline
modele_logit3 <- glm(is_retracted ~ `Collab. men-women m lead` + `Man alone` + `Collab. men only` + `Woman alone` + `Collab. men-women w lead` +
                       log(nb_aut) +
                       is_oa +
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


results <- broom::tidy(modele_logit3)
write.xlsx(results, "~/Documents/Pubpeer Gender/modele_logit3b.xlsx")

## 
## Cutting bdd_regr$nb_aut into bdd_regr$nb_aut_rec
bdd_regr$nb_aut_rec <- cut(bdd_regr$nb_aut,
  include.lowest = TRUE,
  right = FALSE,
  dig.lab = 4,
  breaks = c(1, 3.5, 6.5, 10.5, 163)
)




## 
bdd_regr %>%
  select(publication, nb_aut_rec, is_retracted) %>%
  tbl_summary(
    include = c(publication, nb_aut_rec, is_retracted),
    by = is_retracted,
    sort = list(everything() ~ "frequency"),
    statistic = list(
      all_continuous() ~ c("{N_obs}") 
    )
  )

## 

# summary(modele_logit3)
# residuals <- residuals(modele_logit3, type = "deviance")
# max_residual_index <- which.max(residuals)
# observation_ID <- bdd_regr$publication[max_residual_index]

# Calculer la matrice de corrélation des variables explicatives
mcor <- cor(bdd_regr[, c("Collab. men-women . m corr" , "Man alone" , "Collab. men only" , "Woman alone" , "Collab. men-women . w corr" ,
                     "nb_aut",
                     "Social Sciences" ,
                     "Physical Sciences" ,
                     "Technology" ,
                     "Arts Humanities",
                     "Life Sciences Biomedicine")]
                  , method = c("spearman")
)

# Afficher la matrice de corrélation
corrplot::corrplot(mcor, type="upper", order="hclust", tl.col="black", tl.srt=45)






## variables de controle : discipline
bdd_regr_lim <- bdd_regr %>%
  subset(., nb_aut <100)

modele_logit3 <- glm(is_retracted ~ `Collab. men-women . m corr` + `Man alone` + `Collab. men only` + `Woman alone` + `Collab. men-women . w corr` +
                       log(nb_aut) +
                       is_oa +
                       `Social Sciences` +
                       `Physical Sciences` +
                       Technology +
                       `Arts Humanities`, 
                     data = bdd_regr_lim, 
                     family = binomial)
summary(modele_logit3)

# Calculer le coefficient de détermination R^2
R2 <- 1 - (modele_logit3$deviance / modele_logit3$null.deviance)
cat("R^2 : ", R2, "\n")

##
# Calculer les statistiques descriptives pour nb_aut selon is_retracted
stats_descriptives <- aggregate(nb_aut ~ is_retracted, data = bdd_regr, FUN = function(x) c(min = min(x), max = max(x), moyenne = mean(x), mediane = median(x), ecart_type = sd(x)))

# Afficher les statistiques descriptives
print(stats_descriptives)



###
# stats desc proba et genre
df_gender %>% 
  tbl_summary(
    include = c("proba", "g_prob_06", "g_prob_07", "g_prob_08", "g_prob_09", "g_prob_100")
    
  )


#### Faire du compte fractionnaire pour les raisons ----


## décortiquer les raisons
# éclater les raisons
# df_retract_reason <- df_retract %>%
#   separate_rows(Reason, sep = ";")%>%
#   filter(Reason != "") %>%
#   mutate(Reason = str_replace(Reason, "\\+", ""))


df_reasons <- df_retract_reason %>%
  select(publication, Reason, Gtype2) %>%
  group_by(publication) %>%
  mutate(frac_reason = 1/n())

# sum en fonction de raison et gtype2
df_sum_reason <- df_reasons %>%
  group_by(Reason, Gtype2) %>%
  summarise(sum_frac_reason = sum(frac_reason))

write.xlsx(df_sum_reason, "~/Documents/Pubpeer Gender/frac_raisons.xlsx")
