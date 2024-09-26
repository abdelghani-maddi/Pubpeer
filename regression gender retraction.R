rm(list = ls())

bdd_disc_reg2 <- readRDS("D:/Pubpeer Gender/Révisions QSS/bdd_disc_reg2.rds")
bdd_disc_reg <- readRDS("D:/Pubpeer Gender/Révisions QSS/bdd_disc_reg.rds")
oa_rtw <- readRDS("D:/Pubpeer Gender/Révisions QSS/oa_rtw.rds")
final_data <- readRDS("D:/Pubpeer Gender/Révisions QSS/final_data.rds")

givenNames <- read_excel("D:/Pubpeer Gender/gender_proba_3.xlsx")

# load("D:/Pubpeer Gender/Révisions QSS/global envir data 3.RData")

##################################################################################
##################################################################################
# Ajouter une nouvelle colonne avec le nombre d'auteurs pour oa_rtw
# Étape 1 : Extraire la liste des identifiants au_id pour chaque publication
oa_rtw$au_ids <- lapply(oa_rtw$author, function(x) {
  if (is.data.frame(x)) {
    return(x$au_id)  # Extraire les au_id si x est un data.frame
  } else {
    return(NA)  # Gérer les cas où ce n'est pas un data.frame
  }
})

# Étape 2 : Compter le nombre d'auteurs (nombre d'au_id) pour chaque publication
oa_rtw$nb_auteurs2 <- sapply(oa_rtw$au_ids, function(x) {
  if (is.null(x) || all(is.na(x))) {
    return(0)  # Si x est NULL ou tout est NA, renvoyer 0
  } else {
    return(length(x))  # Sinon, compter les éléments
  }
})
##################################################################################
##################################################################################
# Ne pas tenir compte des notices :
# retirer les notices des données : ce ne sont pas des articles mais juste des notices

a_retirer <- oa_rtw %>%
  filter(str_detect(tolower(title), "retraction not")) %>%
  select(id) %>%
  distinct()

oa_rtw <- oa_rtw %>%
  filter(!(.$id %in% a_retirer$id))

###
##################################################################################
##################################################################################
# extraire nom prenom des auteurs
oa_rtw$nom_prenoms <- lapply(oa_rtw$author, function(x) {
  if (is.data.frame(x)) {
    return(x$au_display_name)  # Extraire les au_display_name si x est un data.frame
  } else {
    return(NA)  # Gérer les cas où ce n'est pas un data.frame
  }
})

##################################################################################
##################################################################################
# extraire la position des auteurs
oa_rtw$author_position <- lapply(oa_rtw$author, function(x) {
  if (is.data.frame(x)) {
    return(x$author_position)  # Extraire les au_display_name si x est un data.frame
  } else {
    return(NA)  # Gérer les cas où ce n'est pas un data.frame
  }
})

##################################################################################
##################################################################################
# Étape 1 : Convertir la colonne 'nom_prenoms' en une colonne avec une liste
oa_rtw_long <- oa_rtw %>%
  unnest(c(nom_prenoms, author_position)) # Étape 2 : Transformer les auteurs en lignes

##################################################################################
# Fonction pour extraire le prénom
extraire_prenom <- function(nom_complet) {
  # Vérifier si le nom complet est NA
  if (is.na(nom_complet)) {
    return(NA)  # Retourner NA si le nom complet est NA
  }
  # Supprimer les caractères spéciaux au début du nom complet
  nom_complet <- str_trim(str_remove(nom_complet, "^[^A-Za-z]+"))
  
  # Diviser le nom complet en mots
  mots <- str_split(nom_complet, " ")[[1]]
  
  # Vérifier si les initiales sont présentes
  initiales <- grepl("^[A-Z]\\.$", mots)
  
  # Si le premier mot est une initiale, on récupère le mot suivant
  if (any(initiales)) {
    prenom <- mots[which(!initiales)][1]
  } else {
    # Retourner le premier mot comme prénom
    prenom <- mots[1]
  }
  
  # Si le prénom est NA (dans le cas où il n'y a pas de mots après les initiales)
  if (is.na(prenom) || prenom == "") {
    return(NA)
  }
  
  # Vérifier si le prénom contient un tiret
  if (str_detect(prenom, "-")) {
    # Garder seulement la première partie du prénom s'il contient un tiret
    prenom_part <- str_split(prenom, "-")[[1]][1]
  } else {
    prenom_part <- prenom  # Sinon, garder le prénom tel quel
  }
  
  # Dans le cas où le prénom est un composite (comme "Abdel-Haleem"), garder uniquement la première partie
  if (str_detect(prenom_part, "‐")) {
    prenom_part <- str_split(prenom_part, "‐")[[1]][1]  # Extraire la première partie avant le tiret
  }
  
  return(prenom_part)
}

# Appliquer la fonction pour extraire les prénoms
oa_rtw_long$prenoms <- sapply(oa_rtw_long$nom_prenoms, extraire_prenom)
oa_rtw_long$prenoms <- tolower(oa_rtw_long$prenoms)

##################################################################################
##################################################################################
##################################################################################
# Analyse de la composition genrée
# Distinct
givenNames <- givenNames %>%
  select(gender, given_name, proba) %>%
  unique()

# Matching
oa_rtw_long <- left_join(oa_rtw_long, givenNames, by = c("prenoms"= "given_name")) # matcher

oa_rtw_gender <- oa_rtw_long %>%
  filter(!is.na(nom_prenoms))


# AJOUTER UNE COLONNE POUR INDIQUER SI LES FEMMES SE TROUVENT EN PREMIERE 
oa_rtw_gender <- oa_rtw_gender %>%
  group_by(id) %>%
  mutate(woman_first = case_when(gender.y == "female" & proba.y >= 0.6 & author_position == "first" ~ 1, TRUE ~ 0),
         man_first = case_when(gender.y == "male" & proba.y >= 0.6 & author_position == "first" ~ 1, TRUE ~ 0))

# Généralisation par Record ID :
oa_rtw_gender <- oa_rtw_gender %>%
  group_by(id) %>%
  mutate(woman_first = ifelse(any(woman_first == 1), 1, woman_first),
         man_first = ifelse(any(man_first == 1), 1, man_first))

# stats desc juste pour vérif
df_test <- oa_rtw_gender %>%
  select(id, woman_first) %>%
  unique()
# stats desc juste pour vérif
df_test <- oa_rtw_gender %>%
  select(id, man_first) %>%
  unique()
## Perfecto !

oa_rtw_gender$gender <- oa_rtw_gender$gender.y
oa_rtw_gender$proba <- oa_rtw_gender$proba.y

# Créer une nouvelle colonne gender_pro_06 en fonction des conditions
oa_rtw_gender$gender_pro_06 <- oa_rtw_gender$gender

# Assigner "initials" pour les cas où gender est NA et FirstName a un seul caractère
oa_rtw_gender$gender_pro_06[is.na(oa_rtw_gender$gender) & nchar(oa_rtw_gender$prenoms) == 1] <- "initials"

# Assigner "undefined" pour les cas où gender est NA et FirstName a plus d'un seul caractère
oa_rtw_gender$gender_pro_06[is.na(oa_rtw_gender$gender) & nchar(oa_rtw_gender$prenoms) > 1] <- "undefined"

# Assigner "unisex" pour les cas où la colonne proba de df_final < 0.6
oa_rtw_gender$gender_pro_06[oa_rtw_gender$proba < 0.6] <- "unisex"

# nagender <- df_final %>%
#  filter(is.na(gender_pro_06))
# rm(nagender)

# Assigner "undefined" pour les cas où l'auteur est NA : 8 cas
oa_rtw_gender$gender_pro_06[is.na(oa_rtw_gender$gender_pro_06)] <- "undefined"

# Calcul de la proportion des femmes par publication
###
`%not_in%` <- purrr::negate(`%in%`)


oa_rtw_gender_def_only <- oa_rtw_gender %>%
  subset(., gender_pro_06 %not_in% c("initials", "unisex", "undefined")) %>%
  select(id, nb_auteurs2, nb_auteurs, nom_prenoms, gender_pro_06,
         publication_date,is_oa,grants,cited_by_count,
         woman_first,man_first,topics,is_retracted) %>%
  unique() %>%
  group_by(id) %>%
  mutate(female_part = mean(gender_pro_06 == "female", na.rm = TRUE))
oa_rtw_gender_def_only$is_retracted <- TRUE
write.xlsx(oa_rtw_gender_def_only, "D:/Pubpeer Gender/oa_rtw_gender_def_only.xlsx")

rtw_gender_fin <- oa_rtw_gender_def_only %>%
  select(id, nb_auteurs2, nb_auteurs,female_part,
         publication_date,is_oa,grants,cited_by_count,
         woman_first,man_first,topics,is_retracted) %>%
  unique()
##################################################################################
# Ajouter la variable "Gtype"
rtw_gender_fin$Gtype <- ifelse(rtw_gender_fin$female_part == 0 & rtw_gender_fin$nb_auteurs == 1, "Man alone", 
                          ifelse(rtw_gender_fin$female_part == 1 & rtw_gender_fin$nb_auteurs == 1, "Woman alone",
                                 ifelse(rtw_gender_fin$female_part == 0 & rtw_gender_fin$nb_auteurs > 1, "Collab. men only",
                                        ifelse(rtw_gender_fin$female_part == 1 & rtw_gender_fin$nb_auteurs > 1, "Collab. women only",
                                                      ifelse(rtw_gender_fin$female_part > 0 & rtw_gender_fin$female_part < 1 & rtw_gender_fin$nb_auteurs > 1 & rtw_gender_fin$woman_first==1, "Collab. men-women w first", 
                                                             ifelse(rtw_gender_fin$female_part > 0 & rtw_gender_fin$female_part < 1 & rtw_gender_fin$nb_auteurs > 1 & rtw_gender_fin$man_first==1, "Collab. men-women m first", "first author not identified")
                                                      )
                                               )
                                        )
                                 )
                          )


##################################################################################
## Extraire les topics
rtw_gender_fin$paper <- rtw_gender_fin$id

# Extraire les tableaux et ajouter l'id correspondant de chaque ligne
list_of_tables_with_id <- map2(rtw_gender_fin$topics, rtw_gender_fin$paper, function(tbl, paper) {
  tbl <- tbl %>%
    mutate(rtw_id = paper)  # Ajouter l'id de rtw_gender_fin à chaque tableau
  return(tbl)
})

# Combiner tous les tableaux en un seul dataframe (si même structure)
combined_tables_with_id <- bind_rows(list_of_tables_with_id)

rtw_domains <- combined_tables_with_id %>%
  filter(name == "domain") %>%
  select(name, display_name, rtw_id) %>%
  unique()

##################################################################################
##################################################################################

rtw_gender_fin$grants <- as.character(rtw_gender_fin$grants)
rtw_gender_fin$grants <- ifelse(rtw_gender_fin$grants == "NA", FALSE, TRUE)

##################################################################################

rtw_gender_fin_gd_disc <- rtw_gender_fin %>%
  left_join(., rtw_domains, by = c("id" = "rtw_id"))

##################################################################################
##################################################################################
## Recodage de rtw_gender_fin_gd_disc$Gtype en rtw_gender_fin_gd_disc$Gtype2_rec
rtw_gender_fin_gd_disc$Gtype2_rec <- rtw_gender_fin_gd_disc$Gtype %>%
  fct_recode(
    "Men-Women | M first" = "Collab. men-women m first",
    "Men-Women | W first" = "Collab. men-women w first",
    "Men only" = "Collab. men only",
    "Women only" = "Collab. women only",
    NULL = "first author not identified"
  )
######
rtw_gender_fin_gd_disc$publication_year <- year(rtw_gender_fin_gd_disc$publication_date)

################################################################

library(labelled)
library(tidyverse)
library(questionr)

look_for(bdd_disc_reg)

## Recodage de bdd_disc_reg$Gtype2 en bdd_disc_reg$Gtype2_rec
bdd_disc_reg$Gtype2_rec <- bdd_disc_reg$Gtype2 %>%
  fct_recode(
    "Men-Women | M first" = "Collab. men-women m first",
    "Men-Women | W first" = "Collab. men-women w first",
    "Men only" = "Collab. men only",
    "Women only" = "Collab. women only",
    NULL = "First author not identified"
  )

bdd_disc_reg_na_aut <- bdd_disc_reg %>%
   filter(is.na(nb_auteurs))



bdd_disc_reg_ss_notices[, 9:27] <- lapply(bdd_disc_reg_ss_notices[, 9:27], function(x) ifelse(x != 0, 1, 0))
###

# Ajout des grandes disciplines
gandes_disc <- final_data %>%
  filter(name == "domain") %>%
  mutate(id = paper, disc = display_name) %>%
  select(id, disc) %>%
  distinct()


bdd_disc_reg_ss_notices_gdisc <- bdd_disc_reg_ss_notices %>%
  left_join(., gandes_disc, by = "id") %>%
  filter(!is.na(disc))



###
var_label(bdd_disc_reg_ss_notices_gdisc$Gtype2_rec) <- "Gender type"
var_label(bdd_disc_reg_ss_notices_gdisc$is_oa) <- "Is Open Access"
var_label(bdd_disc_reg_ss_notices_gdisc$cited_by_count) <- "Log(1 + Citation count)"
var_label(bdd_disc_reg_ss_notices_gdisc$grants) <- "Grants"
var_label(bdd_disc_reg_ss_notices_gdisc$nb_aut) <- "Log(#Authors)"
var_label(bdd_disc_reg_ss_notices_gdisc$publication_year) <- "Publication year"
var_label(bdd_disc_reg_ss_notices_gdisc$disc) <- "Domain"




####

##################################################################################

## Selection data pour regression : RW
bdd_disc_reg_rtw <- rtw_gender_fin_gd_disc %>%
  select(id, nb_auteurs, publication_year, is_oa, is_retracted, grants,
         display_name, cited_by_count, Gtype2_rec) %>%
  filter(!is.na(Gtype2_rec))
names(bdd_disc_reg_rtw) <- c("id", "nb_aut", "publication_year", "is_oa", 
                             "is_retracted", "grants",
                             "disc", "cited_by_count", "Gtype2_rec")
bdd_disc_reg_rtw$bdd <- "RetractionWatch"

###
bdd_disc_reg_rtw$is_oa <- as.factor(bdd_disc_reg_rtw$is_oa)
bdd_disc_reg_rtw$is_retracted <- as.factor(bdd_disc_reg_rtw$is_retracted)
bdd_disc_reg_rtw$grants <- as.factor(bdd_disc_reg_rtw$grants)
###


## Selection data pour regression : OpenAlex
bdd_disc_reg_oax <- bdd_disc_reg_ss_notices_gdisc %>%
  select(id, nb_aut, publication_year, is_oa, is_retracted, grants,
         disc, cited_by_count, Gtype2_rec)%>%
  filter(!is.na(Gtype2_rec))
bdd_disc_reg_oax$bdd <- "OpenAlex"

bdd_disc_reg_cbind <- rbind(bdd_disc_reg_rtw, bdd_disc_reg_oax)

##################################################################################
## Réordonnancement de bdd_disc_reg_ss_notices_gdisc$disc
bdd_disc_reg_cbind$disc <- bdd_disc_reg_cbind$disc %>%
  fct_relevel(
    "Life Sciences", "Health Sciences", "Physical Sciences", "Social Sciences"
  )

# ###
## Réordonnancement de bdd_disc_reg$Gtype2_rec pour la ref dans la regression
bdd_disc_reg_cbind$Gtype2_rec <- bdd_disc_reg_cbind$Gtype2_rec %>%
  fct_relevel(
    "Man alone", "Men-Women | W first", "Men-Women | M first", 
    "Women only", "Men only", "Woman alone"
  )

bdd_disc_reg_cbind$is_retracted <- bdd_disc_reg_cbind$is_retracted %>%
  fct_relevel(
    "FALSE", "TRUE"
  )
###
bdd_disc_reg_cbind <- bdd_disc_reg_cbind %>%
  filter(!is.na(disc), !is.na(is_oa))
### 

#bdd_disc_reg_ss_notices_gdisc$is_medium_team <- ifelse(bdd_disc_reg_ss_notices_gdisc$nb_aut >1 & bdd_disc_reg_ss_notices_gdisc$nb_aut <5 , 1,0)

###

## Recodage de bdd_disc_reg_cbind$nb_aut en bdd_disc_reg_cbind$nb_aut_rec
bdd_disc_reg_cbind$nb_aut_rec <- cut(bdd_disc_reg_cbind$nb_aut,
                                     include.lowest = TRUE,
                                     right = FALSE,
                                     dig.lab = 4,
                                     breaks = c(0, 1, 3.5, 8.5, 15.5, 31.5, 200)
)

## Recodage de bdd_disc_reg_cbind$nb_aut en bdd_disc_reg_cbind$nb_aut_rec
bdd_disc_reg_cbind$nb_aut_rec <- cut(bdd_disc_reg_cbind$nb_aut,
  include.lowest = TRUE,
  right = FALSE,
  dig.lab = 4,
  breaks = c(1, 2, 5, 10, 20, 200)
)
###

# Transformer en colonnes binaires
bdd_disc_reg_cbind_pivot <- bdd_disc_reg_cbind %>%
  mutate(value = 1) %>%
  pivot_wider(names_from = disc, values_from = value, values_fill = 0)

###
res <- glm(is_retracted ~ 
             Gtype2_rec +
             #is_large_team +
             #is_medium_team + 
             is_oa + 
             log(1+cited_by_count) + 
             #log(nb_aut) +  
             nb_aut_rec +
             grants +  
             publication_year + 
             disc,
             #`Health Sciences` + `Life Sciences` + `Physical Sciences`,
           data = bdd_disc_reg_cbind,
           family =  binomial(logit))

summary(res)
###
bdd_disc_reg_cbind2 <- bdd_disc_reg_cbind %>%
  select(id, is_retracted, Gtype2_rec) %>%
  distinct()
###

res <- glm(is_retracted ~ 
             Gtype2_rec, #+
             #is_large_team +
             #is_medium_team + 
             # is_oa + 
             # log(1+cited_by_count) + 
             # #log(nb_aut) +  
             # nb_aut_rec +
             # grants +  
             # publication_year + 
             # disc,
           data = bdd_disc_reg_cbind2,
           family =  binomial(logit))
# grants +
#Biology + Medicine + Engineering + Mathematics + Psychology,

summary(res)

library(car)
vif(res)
# library(GGally)
ggcoef_model(res, exponentiate = TRUE)


library(forestmodel)
forest_model(res) +
  theme(
    text = element_text(size = 16),       # General text size
    axis.text = element_text(size = 14),  # Axis text size
    axis.title = element_text(size = 16), # Axis title size
    legend.text = element_text(size = 14) # Legend text size (if applicable)
  )


library(gtsummary)
bdd_disc_reg_cbind %>% 
  select(id, Gtype2_rec, bdd) %>%
  unique() %>%
  tbl_summary(
    include = c(Gtype2_rec),
    by = bdd,
    sort = list(everything() ~ "frequency")
  ) %>%
  add_overall(last = TRUE, col_label = "**Overall** (# {N})")  


#######################################


# Sélectionner un échantillon aléatoire des "FALSE"
false_sample <- bdd_disc_reg_cbind_pivot[bdd_disc_reg_cbind_pivot$is_retracted == "FALSE", ]
false_sample <- false_sample[sample(nrow(false_sample), 17793), ]

# Sélectionner toutes les lignes "TRUE"
true_sample <- bdd_disc_reg_cbind_pivot[bdd_disc_reg_cbind_pivot$is_retracted == "TRUE", ]

# Combiner les deux échantillons pour créer un échantillon équilibré
balanced_data <- rbind(false_sample, true_sample)

# Vérifier la répartition
table(balanced_data$is_retracted)

#############################################################
# Séparer les publications avec 0 citation
balanced_data$citation_class <- ifelse(balanced_data$cited_by_count == 0, "0 citation", NA)

# Calculer les déciles uniquement pour les publications avec > 0 citation
deciles <- quantile(balanced_data$cited_by_count[balanced_data$cited_by_count > 0], 
                    probs = seq(0, 1, by = 0.1), na.rm = TRUE)

# Ajouter un petit jitter pour s'assurer que les valeurs des déciles sont uniques
deciles_jitter <- deciles + seq(0, length(deciles) - 1) * 1e-5

# Créer des classes pour les publications avec des citations > 0
balanced_data$citation_class[balanced_data$cited_by_count > 0] <- cut(balanced_data$cited_by_count[balanced_data$cited_by_count > 0],
                                                                      breaks = c(-Inf, deciles_jitter[2], deciles_jitter[3], 
                                                                                 deciles_jitter[6], deciles_jitter[9], Inf),
                                                                      labels = c("80-100%", "50-80%", "20-50%", "10-20%", "Top 10%"))


# Vérifier la répartition des classes
table(balanced_data$citation_class)


## Recodage de balanced_data$cited_by_count en balanced_data$cited_by_count_rec
balanced_data$cited_by_count_rec <- cut(balanced_data$cited_by_count,
                                        include.lowest = TRUE,
                                        right = FALSE,
                                        dig.lab = 4,
                                        breaks = c(0,1, 18.5, 80.5, 7399)
)


#############################################################
#############################################################
#############################################################
#############################################################
# rtw
id_paper_stat_jnal_rtw <- oa_rtw %>%
  left_join(., stats_jnals, by = "so_id") %>%
  select(id, so_id, two_yr_mean_citedness, h_index, i10_index) %>%
  unique()

#oa
stats_jnals$journal_id <- gsub("https://openalex.org/", "", stats_jnals$so_id)

id_paper_stat_jnal_oa <- paper_metadat %>%
  filter(!(journal_id=="NONE")) %>%
  left_join(., stats_jnals, by = "journal_id") %>%
  select(id, journal_id, two_yr_mean_citedness, h_index, i10_index) %>%
  unique()


#############################################################
#############################################################
#############################################################
#############################################################

# Ajuster le modèle sur l'échantillon équilibré
model_balanced <- glm(is_retracted ~ 
                      Gtype2_rec +
                      #is_large_team +
                      #is_medium_team + 
                      is_oa + 
                      #log(1+cited_by_count) +
                      cited_by_count_rec +
                      #log(nb_aut) +  
                      nb_aut_rec +
                      grants +  
                      publication_year + 
                      `Health Sciences` +
                      `Social Sciences` +
                      `Physical Sciences`,
                      
                      family = binomial(logit), data = balanced_data)

summary(model_balanced)
forest_model(model_balanced)
