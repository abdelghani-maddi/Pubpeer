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

write.xlsx(oa_rtw_gender, "D:/Pubpeer Gender/oa_rtw_gender.xlsx")

oa_rtw_gender_def_only <- oa_rtw_gender %>%
  subset(., gender_pro_06 %not_in% c("initials", "unisex", "undefined")) %>%
  select(id, nb_auteurs2, nb_auteurs, gender_pro_06,)
  group_by(id) %>%
  summarize(female_part = mean(gender_pro_06 == "female", na.rm = TRUE))



##################################################################################
##################################################################################
##################################################################################


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
   filter(is.na(nb_aut))

## Réordonnancement de bdd_disc_reg$Gtype2_rec pour la ref dans la regression
bdd_disc_reg$Gtype2_rec <- bdd_disc_reg$Gtype2_rec %>%
  fct_relevel(
    "Man alone", "Men-Women | M first", "Men-Women | W first",
    "Women only", "Men only", "Woman alone"
  )


# retirer les notices des données : ce ne sont pas des articles mais juste des notices

a_retirer <- oa_rtw %>%
  filter(str_detect(tolower(title), "retraction not")) %>%
  select(id) %>%
  distinct()

bdd_disc_reg_ss_notices <- bdd_disc_reg %>%
  filter(!(.$id %in% a_retirer$id))

bdd_disc_reg_ss_notices$grants <- as.factor(bdd_disc_reg_ss_notices$grants)

###
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


## Réordonnancement de bdd_disc_reg_ss_notices_gdisc$disc
bdd_disc_reg_ss_notices_gdisc$disc <- bdd_disc_reg_ss_notices_gdisc$disc %>%
  fct_relevel(
    "Life Sciences", "Health Sciences", "Physical Sciences", "Social Sciences"
  )


####
# Récupérer nombre d'auteurs des publications rétractées

df_retract_nb_aut <- df_retract %>%
  filter(is_retracted == 1) %>%
  select(id, nb_aut)

####


bdd_disc_reg_ss_notices_gdisc <- bdd_disc_reg_ss_notices_gdisc %>%
  filter(!is.na(nb_aut), !is.na(Gtype2))


## Transform nb_aut into a categorical variable
bdd_disc_reg_ss_notices_gdisc$nb_aut_cat <- cut(bdd_disc_reg_ss_notices_gdisc$nb_aut,
                                           breaks = c(0,1,5,10,20, 200),  # Intervals
                                           # labels = c("Small team (1-5)", "Medium team (6-10)", "Large team (>10)"),
                                           right = TRUE)  # Includes upper bound

## Recodage de bdd_disc_reg_ss_notices_gdisc$nb_aut en bdd_disc_reg_ss_notices_gdisc$nb_aut_rec
bdd_disc_reg_ss_notices_gdisc$nb_aut_rec <- cut(bdd_disc_reg_ss_notices_gdisc$nb_aut,
  include.lowest = TRUE,
  right = FALSE,
  dig.lab = 4,
  breaks = c(1, 2, 5, 10, 200)
)


# ###
# 
bdd_disc_reg_ss_notices_gdisc$is_medium_team <- ifelse(bdd_disc_reg_ss_notices_gdisc$nb_aut >1 & bdd_disc_reg_ss_notices_gdisc$nb_aut <5 , 1,0)

###
res <- glm(is_retracted ~ 
             Gtype2_rec +
             is_large_team +
             is_medium_team + 
             is_oa + 
             log(1+cited_by_count) + 
             #log(nb_aut) +  
             #nb_aut_rec +
             grants +  
             publication_year + 
             disc,
             #`Health Sciences` + `Life Sciences` + `Physical Sciences`,
           data = bdd_disc_reg_ss_notices_gdisc,
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


