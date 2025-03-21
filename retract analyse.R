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
df_gender <- read_excel("/Users/maddi/Documents/Documents/Recherche/Pubpeer Gender/tb_finale_gender.xlsx") ## bdd sur le genre

df_retract <- read_excel("/Users/maddi/Documents/Documents/Recherche/Pubpeer Gender/df_gender_retract.xlsx") ## bdd sur le genre + bdd retractations (version avril 2023)
#write.xlsx(df_retract, "/Users/maddi/Documents/Pubpeer Gender/df_retract.xlsx")
reason_agr <- read_excel("/Users/maddi/Documents/Documents/Recherche/Pubpeer Gender/reasons_retract aggreg.xlsx")

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
  select(publication, nb_aut, Gtype2, is_retracted, disc) %>%
  subset(., !(is.na(.$disc)) & !(is.na(.$Gtype2))) %>%
  unique()


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


# Pivoter la discipline pour n'analyse
bdd_regr <- pivot_wider(bdd_regr, names_from = disc, values_from = disc, values_fn = list(disc = function(x) 1), 
                        values_fill = list(disc = 0))






#################################################
## Travail sur les commentaires pour être intégré comme variable explicative ----

# date de rétractation
rtw <- read_excel("~/Documents/Pubpeer Gender/RWDBDNLD04242023.xlsx", sheet = "RWDBDNLD04242023") ### bdd retractations (version avril 2023)
pub_ret <- rtw %>%
  select(`Record ID`, OriginalPaperDOI, RetractionDate, OriginalPaperDate, Reason, Continent)
names(pub_ret) = c("ID_retractionwatch", "DOI","RetractionDate","OriginalPaperDate","Reason", "Continent")
# data pub
pub <- bdd_pub %>%
  select(publication, DOI)

# Supprimer les caractères spéciaux et les espaces des colonnes "DOI" des dataframes pub et pub_ret
pub$DOI_clean <- gsub("[^[:alnum:]]", "", pub$DOI)
pub_ret$DOI_clean <- gsub("[^[:alnum:]]", "", pub_ret$DOI)

# Faire le match en fonction de la colonne "DOI_clean"
retraction_data <- merge(pub, pub_ret, by = "DOI_clean")

# Supprimer la colonne "DOI_clean" du dataframe fusionné
retraction_data$DOI_clean <- NULL


reqsql= paste('select inner_id, publication, "DateCreated" as date_com, html as comm from data_commentaires_2')
# reqsql= paste('select * from data_commentaires_2')
data_comm = dbGetQuery(con,reqsql)
# Transformer le format de la date du commentaire
data_comm$date_com <- as.Date.character(data_comm$date_com)

# extraire l'année depuis la colonne "date"
data_comm$annee <- format(data_comm$date_com, "%Y")

# Compter le nombre de "inner_id" par "publication" et par "date_com"
count_data <- aggregate(inner_id ~ publication + date_com, data_comm, length)

# Renommer la colonne "inner_id" en "count"
names(count_data)[names(count_data) == "inner_id"] <- "nb_comm"


# Joindre les dataframes count_data et retraction_data par la colonne "publication"
merged_data <- left_join(count_data, retraction_data, by = "publication")

merged_data$nb_com_before_retract <- with(merged_data, ifelse(date_com < RetractionDate, nb_comm, 0))
merged_data$nb_com_after_retract <- with(merged_data, ifelse(date_com >= RetractionDate, nb_comm, 0))


# Remplacer les NA de la colonne "nb_com_before_retract" par les valeurs correspondantes de la colonne "nb_comm"
# et remplacer les NA de la colonne "nb_com_after_retract" par 0
merged_data$nb_com_before_retract <- ifelse(is.na(merged_data$nb_com_before_retract),
                                            merged_data$nb_comm,
                                            merged_data$nb_com_before_retract)

merged_data$nb_com_after_retract <- ifelse(is.na(merged_data$nb_com_after_retract), 0, merged_data$nb_com_after_retract)


# Utilisation de la fonction group_by() et summarize() pour faire la somme par groupe
retraction_data <- merged_data %>%
  group_by(publication) %>%
  summarize(sum_nb_com_before_retract = sum(nb_com_before_retract, na.rm = TRUE),
            sum_nb_com_after_retract = sum(nb_com_after_retract, na.rm = TRUE),
            sum_nb_comm = sum(nb_comm, na.rm = TRUE))


write.xlsx(retraction_data, "~/Documents/Pubpeer Gender/retraction_data.xlsx")
######################################################
## Rajouter le nombre de commentaires avant la rétractation en variable explicative

retraction_data <- read_excel("~/Documents/Pubpeer Gender/retraction_data.xlsx") # voir au-dessus pour la procédure
#retract_data <- retraction_data[,c(1,3,11,12)]


bdd_regr <- left_join(bdd_regr, retraction_data, by = "publication")
write.xlsx(bdd_regr, "D:/Pubpeer Gender/bdd_regr.xlsx")



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
write.xlsx(results, "D:/Pubpeer Gender/modele_logit1.xlsx")



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
write.xlsx(results, "D:/Pubpeer Gender/modele_logit2.xlsx")


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
write.xlsx(results, "D:/Pubpeer Gender/modele_logit2b.xlsx")


## variables de controle : discipline
modele_logit3 <- glm(is_retracted ~ `Collab. men-women m lead` + `Man alone` + `Collab. men only` + `Woman alone` + `Collab. men-women w lead` +
                       log(nb_aut) +
                       is_oa +
                       `Social Sciences` +
                       `Physical Sciences` +
                       Technology +
                       `Arts Humanities`, 
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
write.xlsx(results, "D:/Pubpeer Gender/modele_logit3.xlsx")

## 

## variables de controle : nombre de commentaires avant retractation
# Calculer les valeurs seuils pour les 5% des valeurs extrêmes de la variable "nb_comm"
lower_threshold <- quantile(bdd_regr$sum_nb_comm, 0.05)
upper_threshold <- quantile(bdd_regr$sum_nb_comm, 0.95)

# Appliquer le filtre pour supprimer les valeurs extrêmes
bdd_regr_filtered <- bdd_regr[bdd_regr$sum_nb_comm >= lower_threshold & bdd_regr$sum_nb_comm <= upper_threshold, ]


modele_logit4 <- glm(is_retracted ~ `Collab. men-women m lead` + `Man alone` + `Collab. men only` + `Woman alone` + `Collab. men-women w lead` +
                       log(nb_aut) +
                       is_oa +
                       #sum_nb_com_before_retract +
                       log(sum_nb_comm) +
                       `Social Sciences` +
                       `Physical Sciences` +
                       Technology +
                       `Arts Humanities`
                       , 
                     data = bdd_regr, 
                     family = binomial)
summary(modele_logit4)

# Calculer le coefficient de détermination R^2
R2 <- 1 - (modele_logit4$deviance / modele_logit3$null.deviance)
cat("R^2 : ", R2, "\n")

# Afficher l'AIC et le BIC du modèle
AIC(modele_logit4)
BIC(modele_logit4)


results <- broom::tidy(modele_logit4)
write.xlsx(results, "~/Documents/Pubpeer Gender/modele_logit4.xlsx")

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


#### Faire du compte fractionnaire pour les raisons ----


## décortiquer les raisons
# éclater les raisons
# df_retract_reason <- df_retract %>%
#   separate_rows(Reason, sep = ";")%>%
#   filter(Reason != "") %>%
#   mutate(Reason = str_replace(Reason, "\\+", ""))


df_reasons <- df_retract_reason %>%
  select(publication, Reason, Gtype2) %>%
  subset(., !is.na(Gtype2)) %>%
  group_by(publication) %>%
  mutate(frac_reason = 1/n())

# sum en fonction de raison et gtype2
df_sum_reason <- df_reasons %>%
  group_by(Reason, Gtype2) %>%
  summarise(sum_frac_reason = sum(frac_reason), sum_reason = n_distinct(publication))

write.xlsx(df_sum_reason, "~/Documents/Pubpeer Gender/frac_raisons2.xlsx")



raisons_ratio <- read_excel("~/Documents/Pubpeer Gender/raisons_ratio.xlsx")
row.names(raisons_ratio) <- raisons_ratio$`Reasons (Retraction Watch)`

df <- raisons_ratio %>%
  subset(., select = -`Reasons (Retraction Watch)`, drop = FALSE)
row.names(df) <- raisons_ratio$`Reasons (Retraction Watch)`


install.packages("pheatmap")        
library("pheatmap")


pheatmap(df)                      
pheatmap(df, kmeans_k = 4)
pheatmap(df, cutree_rows = 4, cutree_cols = 3)



# Transformer les valeurs dans le dataframe selon les conditions spécifiées
df_transformed <- df %>%
  mutate_all(function(x) {
    case_when(
      x <= 0.3 ~ 0.3,
      x > 0.3 & x <= 0.5 ~ 0.5,
      x > 0.5 & x <= 0.7 ~ 0.7,
      x > 0.7 & x <= 0.9 ~ 0.9,
      x > 0.9 & x <= 1.1 ~ 1.1,
      x > 1.1 & x <= 1.3 ~ 1.3,
      x > 1.3 & x <= 1.5 ~ 1.5,
      x > 1.5 & x <= 1.7 ~ 1.7,
      x > 1.7 & x <= 1.9 ~ 1.9,
      x > 1.9 & x <= 2.1 ~ 2.1,
      x > 2.1 & x <= 2.3 ~ 2.3,
      x > 2.3 & x <= 2.5 ~ 2.5,
      x > 2.5 & x <= 2.7 ~ 2.7,
      x > 2.7 & x <= 2.9 ~ 2.9,
      x > 2.9 ~ 2.9,
      TRUE ~ x  # Pour conserver les autres valeurs telles quelles
    )
  })

row.names(df_transformed) <- raisons_ratio$`Reasons (Retraction Watch)`


pheatmap(df_transformed)                      
pheatmap(df_transformed, kmeans_k = 4)
pheatmap(df_transformed, cutree_rows = 4)
pheatmap(df_transformed, cutree_rows = 5, cutree_cols = 4)

