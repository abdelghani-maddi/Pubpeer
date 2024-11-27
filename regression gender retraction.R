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
#############################################################
#############################################################

bdd_disc_reg_cbind_pivot2 <- bdd_disc_reg_cbind_pivot %>%
  left_join(id_paper_stat_jnal, by = "id")

#############################################################
#############################################################


# Sélectionner un échantillon aléatoire des "FALSE"
false_sample <- bdd_disc_reg_cbind_pivot2[bdd_disc_reg_cbind_pivot2$is_retracted == "FALSE", ]
false_sample <- false_sample[sample(nrow(false_sample), 23462), ]

# Sélectionner toutes les lignes "TRUE"
true_sample <- bdd_disc_reg_cbind_pivot2[bdd_disc_reg_cbind_pivot2$is_retracted == "TRUE", ]

# Combiner les deux échantillons pour créer un échantillon équilibré
balanced_data <- rbind(false_sample, true_sample)

# Vérifier la répartition
table(balanced_data$is_retracted)

#############################################################

#############################################################
#############################################################
#############################################################

## Réordonnancement de balanced_data$Gtype2_rec
balanced_data$Gtype2_rec <- balanced_data$Gtype2_rec %>%
  fct_relevel(
    "Man alone", "Women only", "Men-Women | M first", "Men only", 
    "Men-Women | W first", "Woman alone"
  )

## Réordonnancement de balanced_data$Gtype2_rec
bdd_disc_reg_cbind_pivot2$Gtype2_rec <- bdd_disc_reg_cbind_pivot2$Gtype2_rec %>%
  fct_relevel(
    "Woman alone", "Men-Women | M first", "Men only", "Man alone",
    "Men-Women | W first", "Women only"
  )


balanced_data_ss_na <- balanced_data %>%
  filter(!is.na(so_id), !is.na(two_yr_mean_citedness))

bdd_disc_reg_cbind_pivot2_ss_na <- bdd_disc_reg_cbind_pivot2 %>%
  filter(!is.na(so_id), !is.na(two_yr_mean_citedness))


# is mid-large team
# balanced_data_ss_na$is_samll_mid_team <- ifelse((balanced_data_ss_na$nb_aut >1 & 
#                                              balanced_data_ss_na$nb_aut <11), 1, 0)

balanced_data_ss_na$is_mid_team <- ifelse((balanced_data_ss_na$nb_aut >1 &
                                             balanced_data_ss_na$nb_aut <9), 1, 0)

balanced_data_ss_na$is_mid2_team <- ifelse((balanced_data_ss_na$nb_aut >4 &
                                             balanced_data_ss_na$nb_aut <10), 1, 0)


balanced_data_ss_na$sup10 <- ifelse(balanced_data_ss_na$nb_aut >=10, 1, 0)

# is_retracted_nbaut <- balanced_data_ss_na %>%
#   filter(is_retracted == TRUE) %>%
#   select(nb_aut) 
# 
# freq(is_retracted_nbaut$nb_aut)

## Recodage de balanced_data_ss_na$nb_aut en balanced_data_ss_na$nb_aut_rec
balanced_data_ss_na$nb_aut_rec <- cut(balanced_data_ss_na$nb_aut,
                                      include.lowest = TRUE,
                                      right = FALSE,
                                      dig.lab = 4,
                                      breaks = c(1, 2, 5, 9, 17, 53, 100)
)

## Réordonnancement de balanced_data_ss_na$nb_aut_rec
balanced_data_ss_na$nb_aut_rec <- balanced_data_ss_na$nb_aut_rec %>%
  fct_relevel(
    "[9,17)", "[1,2)", "[2,5)", "[5,9)", "[17,53)", "[53,100]"
  )

## Réordonnancement de balanced_data_ss_na$Gtype2_rec
balanced_data_ss_na2$Gtype2_rec <- balanced_data_ss_na2$Gtype2_rec %>%
  fct_relevel(
    "Man alone", "Men-Women | M first", 
    "Men-Women | W first", "Men only", "Women only", "Woman alone"
  )

# bdd_disc_reg_cbind_pivot2_ss_na$is_mid_team <- ifelse((bdd_disc_reg_cbind_pivot2_ss_na$nb_aut >2 &
#                                                          bdd_disc_reg_cbind_pivot2_ss_na$nb_aut <10), 1, 0)

# balanced_data_ss_na$is_large_team <- ifelse((balanced_data_ss_na$nb_aut >10 & 
#                                                balanced_data_ss_na$nb_aut <60), 1, 0)

# Créer des variables indicatrices pour nb_aut_rec
# dummy_vars <- model.matrix(~ nb_aut_rec - 1, data = balanced_data_ss_na)
# 
# # Ajouter les variables indicatrices au dataframe
# balanced_data_ss_na2 <- cbind(balanced_data_ss_na, dummy_vars)
# 
# # Voir les nouvelles colonnes créées
# head(balanced_data_ss_na)

balanced_data_ss_na2$is_mid_team <- ifelse((balanced_data_ss_na2$nb_aut >2 &
                                              balanced_data_ss_na2$nb_aut <11), T, F)

# balanced_data_ss_na2$is_midlarge_team <- ifelse((balanced_data_ss_na2$nb_aut >10 &
#                                               balanced_data_ss_na2$nb_aut <21), 1, 0)

balanced_data_ss_na2$is_mid_team <- as.factor(balanced_data_ss_na2$is_mid_team)

# Ajouter des labels aux variables dans votre dataframe
var_label(balanced_data_ss_na2$Gtype2_rec) <- "Gender Type"
var_label(balanced_data_ss_na2$is_oa) <- "Is Open Access"
var_label(balanced_data_ss_na2$two_yr_mean_citedness) <- "Log(Journal impact)"
var_label(balanced_data_ss_na2$nb_aut) <- "Log(Authors number)"
var_label(balanced_data_ss_na2$is_mid_team) <- "Is Medium-sized team"
var_label(balanced_data_ss_na2$grants) <- "Is funded"
var_label(balanced_data_ss_na2$publication_year) <- "Publication year"
var_label(balanced_data_ss_na2$`Health Sciences`) <- "Health Sc. (ref. life Sc.)"
var_label(balanced_data_ss_na2$`Social Sciences`) <- "Social Sc. (ref. life Sc.)"
var_label(balanced_data_ss_na2$`Physical Sciences`) <- "Physical Sc. (ref. life Sc.)"

# Ajuster le modèle sur l'échantillon équilibré
model_balanced <- glm(is_retracted ~ 
                      Gtype2_rec + # Gender Type
                      is_oa + # Is Open Access
                      two_yr_mean_citedness + # 2 years mean journal impact (Log transformed)
                      log(nb_aut) + # Authors number (Log transformed)
                      is_mid_team + # Is midle team (between 3 and 10 authors)
                      grants +  # Is funded
                      publication_year +  # Publication year
                      `Health Sciences` + # Health Sciences (ref life sciences)
                      `Social Sciences` + # Social Sciences (ref life sciences)
                      `Physical Sciences`, # Physical Sciences (ref life sciences)
                      
                      family = binomial(logit), 
                      data = balanced_data_ss_na2
                      #data = bdd_disc_reg_cbind_pivot2_ss_na
                      )

summary(model_balanced)
forest_model(model_balanced)


################################
################################

# Appliquer les labels et générer le graphique
forest_model(model_balanced, factor_labeller = labels)


vif(model_balanced)
#alias(model_balanced)
#############################################
# Créer un dataframe à partir des valeurs VIF
vif_values <- data.frame(
  Variable = c("Gtype2_rec", "is_oa", "two_yr_mean_citedness", "log(nb_aut)",
               "is_mid_team", "grants", "publication_year", 
               "Health Sciences", "Social Sciences", "Physical Sciences"),
  VIF = c(3.235532, 1.054941, 1.171898, 3.693510,
          1.921387, 1.147261, 1.102869, 
          1.597748, 1.340383, 1.618837)
)

# Créer le graphique
ggplot(vif_values, aes(x = reorder(Variable, VIF), y = VIF)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() + # Inverser les axes pour une meilleure lisibilité
  labs(title = "Variance Inflation Factor (VIF) values by Variable",
       x = "Variable",
       y = "VIF") +
  geom_hline(yintercept = 5, linetype = "dashed", color = "red") + # Ligne pour indiquer le seuil de 5
  theme_minimal()
#############################################
#############################################
set.seed(123)
boot_model <- boot::boot(data = balanced_data_ss_na2, statistic = function(data, indices) {
  model <- glm(is_retracted ~ Gtype2_rec + is_oa + two_yr_mean_citedness + log(nb_aut) + is_mid_team + grants + publication_year + `Health Sciences` + `Social Sciences` + `Physical Sciences`,
               family = binomial(logit), data = data[indices, ])
  return(coef(model))
}, R = 1000)
boot::boot.ci(boot_model)
#############################################
#############################################


#############################################
#############################################
library(caret)
set.seed(123)
train_control <- trainControl(method = "cv", number = 10)
model_cv <- train(is_retracted ~ ., data = balanced_data_ss_na2, method = "glm", 
                  family = binomial, trControl = train_control)
print(model_cv)
#############################################
#############################################


#############################################
#############################################
# Packages
library(ggplot2)
library(caret)
library(car)
library(boot)

# Créer un sous-ensemble du jeu de données avec les variables utilisées dans le modèle
selected_vars <- c("is_retracted", "Gtype2_rec", "is_oa", "two_yr_mean_citedness", 
                   "nb_aut", "is_mid_team", "grants", "publication_year", 
                   "Health Sciences", "Social Sciences", "Physical Sciences")

# Filtrer le jeu de données
balanced_data_subset <- balanced_data_ss_na2[selected_vars]

names(balanced_data_subset) <- c("is_retracted", "Gtype2_rec", "is_oa", "two_yr_mean_citedness", 
                                    "nb_aut", "is_mid_team", "grants", "publication_year", 
                                    "Health_Sciences", "Social_Sciences", "Physical_Sciences")

balanced_data_subset$nb_aut <- log(balanced_data_subset$nb_aut)


# Modifier les niveaux de la variable is_retracted pour avoir des noms valides
balanced_data_subset$is_retracted <- factor(balanced_data_subset$is_retracted, 
                                            levels = c("FALSE", "TRUE"), 
                                            labels = c("Not_Retracted", "Retracted"))

# Vérifier les niveaux des autres variables de type facteur si nécessaire (exemple pour is_oa et grants)
balanced_data_subset$is_oa <- factor(balanced_data_subset$is_oa, 
                                     levels = c("FALSE", "TRUE"), 
                                     labels = c("Not_OA", "OA"))

balanced_data_subset$grants <- factor(balanced_data_subset$grants, 
                                      levels = c("FALSE", "TRUE"), 
                                      labels = c("No_Grant", "Grant"))

# Validation croisée avec le sous-ensemble des données
set.seed(123)
train_control <- trainControl(method = "cv", number = 10, classProbs = TRUE, summaryFunction = twoClassSummary)
model_cv <- train(is_retracted ~ ., data = balanced_data_subset, 
                  method = "glm", family = binomial, trControl = train_control, 
                  metric = "Accuracy")
# Extraire les résultats de validation croisée
cv_results <- model_cv$resample

# Boxplot des erreurs de prédiction
ggplot(cv_results, aes(x = Resample, y = ROC)) +
  geom_boxplot() +
  labs(title = "Boxplot des erreurs de prédiction (Validation croisée)",
       x = "Répliques",
       y = "Précision") +
  theme_minimal()
# Boxplot 2 des erreurs de prédiction
ggplot(cv_results) +
  aes(x = Resample, y = ROC) +
  geom_col(fill = "#112446") +
  labs(
    x = "Replicas",
    y = "Accuracy",
    title = "ROC Performance Across 10 Cross-Validation Replicas"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(face = "bold",
                               size = 15L),
    axis.text.x = element_text(size = 10L),
    legend.text = element_text(face = "bold"),
    legend.title = element_text(face = "bold")
  )
###############################################################################



# Fonction pour estimer les coefficients du modèle
boot_model <- boot::boot(data = balanced_data_subset, statistic = function(data, indices) {
  model <- glm(is_retracted ~ Gtype2_rec + is_oa + two_yr_mean_citedness + 
                 nb_aut + is_mid_team + grants + publication_year + 
                 Health_Sciences + Social_Sciences + Physical_Sciences,
               family = binomial(logit), data = data[indices, ])
  return(coef(model))
}, R = 10)  # Nombre d'itérations bootstrap

# Récupérer les noms des variables à partir des données
var_names <- colnames(boot_model[["data"]])[-1]  # On exclut la première colonne is_retracted

# Vérifiez la longueur des coefficients et des intervalles de confiance
cat("Longueur des coefficients:", length(coefficients), "\n")
cat("Longueur des intervalles de confiance:", ncol(coef_ci), "\n")

# Extraire les intervalles de confiance sans les rownames
lower_ci <- coef_ci[1, ]  # 2.5%
upper_ci <- coef_ci[2, ]  # 97.5%

# Assurez-vous que le nombre de noms de variables correspond à la longueur des coefficients et CI
if (length(coefficients) == length(lower_ci) && length(coefficients) == length(upper_ci)) {
  
  # Créer un DataFrame pour les coefficients et les intervalles de confiance
  coef_df <- data.frame(
    #Variable = var_names,  # Noms des variables
    Coefficient = coefficients,
    Lower_CI = lower_ci,
    Upper_CI = upper_ci
  )
  
  # Vérifiez le DataFrame créé
  print(coef_df)
  
  # Ajouter une colonne pour les noms des coefficients (si nécessaire)
  coef_df$Variable <- rownames(coef_df)
  
 
#############################################
  # Obtenez l'AIC
  model_aic <- AIC(model_balanced)
  print(paste("AIC:", model_aic))  
  
  # Calculer le Pseudo R² de McFadden
  null_model <- glm(is_retracted ~ 1, family = binomial(logit), data = balanced_data_subset)
  pseudo_r2 <- 1 - (logLik(model_balanced) / logLik(null_model))
  print(paste("Pseudo R² (McFadden):", pseudo_r2))
  
  # Installez le package pROC si vous ne l'avez pas
  # install.packages("pROC")
  
  library(pROC)
  
  # Prédictions sur l'ensemble de données
  predicted_probs <- predict(model_balanced, type = "response")
  roc_curve <- roc(balanced_data_ss_na2$is_retracted, predicted_probs)
  
  # Tracez la courbe ROC
  plot(roc_curve, main = "ROC Curve")
  print(paste("AUC:", auc(roc_curve)))
  

  # Seuil de classification (généralement 0.5)
  threshold <- 0.5
  predicted_classes <- ifelse(predicted_probs > threshold, "Retracted", "Not_Retracted")
  
  # Créer une table de confusion
  confusion_matrix <- table(Predicted = predicted_classes, Actual = balanced_data_subset$is_retracted)
  
  # Calculer la précision, la sensibilité et la spécificité
  accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  sensitivity <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
  specificity <- confusion_matrix[1, 1] / sum(confusion_matrix[1, ])
  
  print(paste("Précision:", accuracy))
  print(paste("Sensibilité:", sensitivity))
  print(paste("Spécificité:", specificity))
  
  
#############################################

library(gtsummary)
bdd_disc_reg_cbind_pivot2_ss_na %>% 
  select(id, Gtype2_rec, bdd) %>%
  unique() %>%
  tbl_summary(
    include = c(Gtype2_rec),
    by = bdd,
    sort = list(everything() ~ "frequency")
  ) %>%
  add_overall(last = TRUE, col_label = "**Overall** (# {N})")  


library(gtsummary)
balanced_data_ss_na %>% 
  select(id, Gtype2_rec, bdd) %>%
  unique() %>%
  tbl_summary(
    include = c(Gtype2_rec),
    by = bdd,
    sort = list(everything() ~ "frequency")
  ) %>%
  add_overall(last = TRUE, col_label = "**Overall** (# {N})")  


moy_aut_retra <- balanced_data_ss_na %>%
  group_by(is_retracted) %>%
  summarise(moy_aut = mean(nb_aut))

moy_aut_retra2 <- bdd_disc_reg_cbind_pivot2_ss_na %>%
  group_by(is_retracted) %>%
  summarise(moy_aut = mean(nb_aut))

