
library(tidyverse)
library(questionr)
library(RPostgres)
library(gtsummary)
library(openxlsx2)
library(gender)
library(GenderInfer)
library(openxlsx)
library(readxl)



# Données aléatoires OPENALEX
author_metadat <- read.csv("~/Documents/Pubpeer Gender/Révisions QSS/randomSample/author_metadata.csv")
paper2author <- read.csv("~/Documents/Pubpeer Gender/Révisions QSS/randomSample/paper2author.csv")

paper_metadat <- read.csv("~/Documents/Pubpeer Gender/Révisions QSS/randomSample/paper_metadata.csv")
paper2concept <- read.csv("~/Documents/Pubpeer Gender/Révisions QSS/randomSample/paper2concept.csv")

# Quelques stats desc pour explorer les données

# articles avec plus de 1 auteur

nb_aut_paper <- paper2author %>%
  group_by(paper) %>%
  summarise(n = n()) %>%
  mutate(is_collab = ifelse(n>1,1,0))

part_collab <- (sum(nb_aut_paper$is_collab)/length(nb_aut_paper$paper))*100 # 57,61% avec au moins 2 auteurs

# 906 552 papier écrits par 2 644 463 auteurs


# Identifier le genre des auteurs
author_metadat$prenom <- tolower(sapply(strsplit(author_metadat$name, " "), `[`, 1))

#############################
#############################
# Rajouter l'order des auteurs par papier
paper2author <- paper2author %>%
  group_by(paper) %>%
  mutate(ordre_aut = row_number(),
         link_paper = paste0("https://openalex.org/",paper),
         link_aut = paste0("https://openalex.org/",author))

# Rajouter des flags sur 1er dernier auteur + monoauteur
paper2author <- paper2author %>%
  group_by(paper) %>%
  mutate(nb_aut = max(ordre_aut),
         type = ifelse(max(ordre_aut) == 1, "monoaut","collab"))

#############################
# rajouter les prénoms des auteurs
prenom <- author_metadat %>%
  select(id, prenom) %>%
  unique()

oadata <- left_join(paper2author, prenom, by = c("author" = "id"))
  
#############################
#############################

# Matching
oadata_gender <- left_join(oadata, givenNames, by = c("prenom"= "given_name")) # matcher

# Ajouter un flag pour indiquer si l'auteur est en 1re ou dernière position
oadata_gender$IsFirstLast = if_else(oadata_gender$ordre_aut == 1 | oadata_gender$ordre_aut == oadata_gender$nb_aut, 1, 0)

# AJOUTER UNE COLONNE POUR INDIQUER SI LES FEMMES SE TROUVENT EN PREMIERE OU DERNIERE POSITION
oadata_gender <- oadata_gender %>%
  group_by(paper) %>%
  mutate(woman_leader = case_when(gender == "female" & proba >= 0.6 & IsFirstLast == 1 ~ 1, TRUE ~ 0),
         man_leader = case_when(gender == "male" & proba >= 0.6 & IsFirstLast == 1 ~ 1, TRUE ~ 0))

# Généralisation par paper :
oadata_gender <- oadata_gender %>%
  group_by(paper) %>%
  mutate(woman_leader = ifelse(any(woman_leader == 1), 1, woman_leader),
         man_leader = ifelse(any(man_leader == 1), 1, man_leader))

# AJOUTER plusieurs colonnes pour distinguer quand c'est uniquement des femmes qui sont leader, ou uniquement des hommes, ou les deux.
oadata_gender$only_women_leader <- case_when(oadata_gender$woman_leader == 1 & oadata_gender$man_leader == 0 ~ 1, TRUE ~ 0)
oadata_gender$only_men_leader = case_when(oadata_gender$woman_leader == 0 & oadata_gender$man_leader == 1 ~ 1, TRUE ~ 0)
oadata_gender$woman_and_man_leader = case_when(oadata_gender$woman_leader == 1 & oadata_gender$man_leader == 1 ~ 1, TRUE ~ 0)

# Généralisation par paper :
oadata_gender <- oadata_gender %>%
  group_by(paper) %>%
  mutate(only_women_leader = ifelse(any(only_women_leader == 1), 1, only_women_leader),
         only_men_leader = ifelse(any(only_men_leader == 1), 1, only_men_leader),
         woman_and_man_leader = ifelse(any(woman_and_man_leader == 1), 1, woman_and_man_leader))



# stats desc juste pour vérif
df_test <- oadata_gender %>%
  select(paper, woman_leader) %>%
  unique()
# stats desc juste pour vérif
df_test <- oadata_gender %>%
  select(paper, man_leader) %>%
  unique()
## Perfecto !


# Créer une nouvelle colonne gender_pro_06 en fonction des conditions
oadata_gender$gender_pro_06 <- oadata_gender$gender

# Assigner "initials" pour les cas où gender est NA et FirstName a un seul caractère
oadata_gender$gender_pro_06[is.na(oadata_gender$gender) & (grepl("\\.", oadata_gender$prenom) | nchar(oadata_gender$prenom) < 3)] <- "initials"

# Assigner "undefined" pour les cas où gender est NA et FirstName a plus d'un seul caractère
oadata_gender$gender_pro_06[is.na(oadata_gender$gender) & nchar(oadata_gender$prenom) >= 3] <- "undefined"

# Assigner "unisex" pour les cas où la colonne proba de df_final < 0.6
oadata_gender$gender_pro_06[oadata_gender$proba < 0.6] <- "unisex"

# nagender <- df_final %>%
#   filter(is.na(gender_pro_06))
# Assigner "undefined" pour les cas où l'auteur est NA : 8 cas
oadata_gender$gender_pro_06[is.na(oadata_gender$gender_pro_06)] <- "undefined"

describe(oadata_gender$gender_pro_06)
# Calcul de la proportion des femmes par publication
###
`%not_in%` <- purrr::negate(`%in%`)

oadata_tbfin <- oadata_gender %>%
  subset(., gender_pro_06 %not_in% c("initials", "unisex", "undefined")) %>%
  group_by(paper) %>%
  summarize(female_part = mean(gender_pro_06 == "female", na.rm = TRUE))



# Faire une jointure
df_oa <- oadata_gender

# Matcher
df_oa <- merge(df_oa, oadata_tbfin, by.x = "paper", by.y = "paper", all.x = TRUE) 

df_oa <- df_oa %>%
  subset(., gender_pro_06 %not_in% c("initials", "unisex", "undefined"))



# Ajouter la variable "Gtype"
df_oa$Gtype <- ifelse(df_oa$female_part == 0 & df_oa$nb_aut == 1, "Man alone", 
                          ifelse(df_oa$female_part == 1 & df_oa$nb_aut == 1, "Woman alone",
                                 ifelse(df_oa$female_part == 0 & df_oa$nb_aut > 1, "Collab. men only",
                                        ifelse(df_oa$female_part == 1 & df_oa$nb_aut > 1, "Collab. women only",
                                               ifelse(df_oa$female_part > 0 & df_oa$female_part < 1 & df_oa$nb_aut ==2 & df_oa$woman_and_man_leader==1, "Collab. men-women 2 auteurs", 
                                                      ifelse(df_oa$female_part > 0 & df_oa$female_part < 1 & df_oa$nb_aut > 1 & df_oa$only_women_leader==1, "Collab. men-women w lead", 
                                                             ifelse(df_oa$female_part > 0 & df_oa$female_part < 1 & df_oa$nb_aut > 1 & df_oa$only_men_leader==1, "Collab. men-women m lead",
                                                                  ifelse(df_oa$female_part > 0 & df_oa$female_part < 1 & df_oa$nb_aut > 1 & df_oa$woman_and_man_leader==1, "Collab. men-women mw lead", "First-Last authors not identified")
                                                      )
                                               )
                                        )
                                 )
                          )
                    ) 
             )

####################################
df_final <- left_join(rtw_melted, givenNames, by = c("FirstName"= "given_name")) # matcher

# Application sur les données RetractionWatch de la même méthode

colnames(df_final)[colnames(df_final) == "Record ID"] <- "paper"
colnames(df_final)[colnames(df_final) == "FirstName"] <- "prenom"

# AJOUTER UNE COLONNE POUR INDIQUER SI LES FEMMES SE TROUVENT EN PREMIERE OU DERNIERE POSITION
df_final <- df_final %>%
  group_by(paper) %>%
  mutate(woman_leader = case_when(gender == "female" & proba >= 0.6 & IsFirstLast == 1 ~ 1, TRUE ~ 0),
         man_leader = case_when(gender == "male" & proba >= 0.6 & IsFirstLast == 1 ~ 1, TRUE ~ 0))

# Généralisation par paper :
df_final <- df_final %>%
  group_by(paper) %>%
  mutate(woman_leader = ifelse(any(woman_leader == 1), 1, woman_leader),
         man_leader = ifelse(any(man_leader == 1), 1, man_leader))

# AJOUTER plusieurs colonnes pour distinguer quand c'est uniquement des femmes qui sont leader, ou uniquement des hommes, ou les deux.
df_final$only_women_leader <- case_when(df_final$woman_leader == 1 & df_final$man_leader == 0 ~ 1, TRUE ~ 0)
df_final$only_men_leader = case_when(df_final$woman_leader == 0 & df_final$man_leader == 1 ~ 1, TRUE ~ 0)
df_final$woman_and_man_leader = case_when(df_final$woman_leader == 1 & df_final$man_leader == 1 ~ 1, TRUE ~ 0)

# Généralisation par paper :
df_final <- df_final %>%
  group_by(paper) %>%
  mutate(only_women_leader = ifelse(any(only_women_leader == 1), 1, only_women_leader),
         only_men_leader = ifelse(any(only_men_leader == 1), 1, only_men_leader),
         woman_and_man_leader = ifelse(any(woman_and_man_leader == 1), 1, woman_and_man_leader))



# stats desc juste pour vérif
df_test <- df_final %>%
  select(paper, woman_leader) %>%
  unique()
# stats desc juste pour vérif
df_test <- df_final %>%
  select(paper, man_leader) %>%
  unique()
## Perfecto !


# Créer une nouvelle colonne gender_pro_06 en fonction des conditions
df_final$gender_pro_06 <- df_final$gender

# Assigner "initials" pour les cas où gender est NA et FirstName a un seul caractère
df_final$gender_pro_06[is.na(df_final$gender) & (grepl("\\.", df_final$prenom) | nchar(df_final$prenom) < 3)] <- "initials"

# Assigner "undefined" pour les cas où gender est NA et FirstName a plus d'un seul caractère
df_final$gender_pro_06[is.na(df_final$gender) & nchar(df_final$prenom) >= 3] <- "undefined"

# Assigner "unisex" pour les cas où la colonne proba de df_final < 0.6
df_final$gender_pro_06[df_final$proba < 0.6] <- "unisex"

# nagender <- df_final %>%
#   filter(is.na(gender_pro_06))
# Assigner "undefined" pour les cas où l'auteur est NA : .. cas
df_final$gender_pro_06[is.na(df_final$gender_pro_06)] <- "undefined"

describe(df_final$gender_pro_06)
# Calcul de la proportion des femmes par publication
###
`%not_in%` <- purrr::negate(`%in%`)

df_final_tbfin <- df_final %>%
  subset(., gender_pro_06 %not_in% c("initials", "unisex", "undefined")) %>%
  group_by(paper) %>%
  summarize(female_part = mean(gender_pro_06 == "female", na.rm = TRUE))



# Faire une jointure
df <- df_final

# Matcher
df <- merge(df, df_final_tbfin, by.x = "paper", by.y = "paper", all.x = TRUE) 

df <- df %>%
  subset(., gender_pro_06 %not_in% c("initials", "unisex", "undefined"))



# Ajouter la variable "Gtype"
df$Gtype <- ifelse(df$female_part == 0 & df$nb_aut == 1, "Man alone", 
                      ifelse(df$female_part == 1 & df$nb_aut == 1, "Woman alone",
                             ifelse(df$female_part == 0 & df$nb_aut > 1, "Collab. men only",
                                    ifelse(df$female_part == 1 & df$nb_aut > 1, "Collab. women only",
                                           ifelse(df$female_part > 0 & df$female_part < 1 & df$nb_aut ==2 & df$woman_and_man_leader==1, "Collab. men-women 2 auteurs", 
                                                  ifelse(df$female_part > 0 & df$female_part < 1 & df$nb_aut > 1 & df$only_women_leader==1, "Collab. men-women w lead", 
                                                         ifelse(df$female_part > 0 & df$female_part < 1 & df$nb_aut > 1 & df$only_men_leader==1, "Collab. men-women m lead",
                                                                ifelse(df$female_part > 0 & df$female_part < 1 & df$nb_aut > 1 & df$woman_and_man_leader==1, "Collab. men-women mw lead", "First-Last authors not identified")
                                                         )
                                                  )
                                           )
                                    )
                             )
                      ) 
)

describe(df$Gtype)
describe(df_oa$Gtype)

df %>%
  select(paper, Gtype) %>%
  unique() %>%
  tbl_summary(
    include = c(Gtype),
    sort = list(everything() ~ "frequency")
    )


df_oa %>%
  select(paper, Gtype) %>%
  unique() %>%
  tbl_summary(
    include = c(Gtype),
    sort = list(everything() ~ "frequency")
  )






df %>%
  tbl_summary(
    include = c(paper, Gtype),
    by = (Gtype),
    sort = list(everything() ~ "frequency"),
    statistic = list(
      all_continuous() ~ c("{N_obs}") 
    )
  ) %>%
  add_overall(last = TRUE) #, col_label = "**Ensemble** (effectif total: {N})")


## Récupérer métadonnées OpenAlex

id <- df_oa$paper %>%
  unique()

library(openalexR)

oa_metadata_1 <- oa_fetch(
  entity = "works",
  id = id[1:100000],
  verbose = TRUE
)

oa_metadata_2 <- oa_fetch(
  entity = "works",
  id = id[100001:200000],
  verbose = TRUE
)

oa_metadata_3 <- oa_fetch(
  entity = "works",
  id = id[200001:300000],
  verbose = TRUE
)


oa_metadata_4 <- oa_fetch(
  entity = "works",
  id = id[300001:400000],
  verbose = TRUE
)

oa_metadata_5 <- oa_fetch(
  entity = "works",
  id = id[400001:500000],
  verbose = TRUE
)


oa_metadata_6 <- oa_fetch(
  entity = "works",
  id = id[500001:600000],
  verbose = TRUE
)

oa_metadata_7 <- oa_fetch(
  entity = "works",
  id = id[600001:600002],
  verbose = TRUE
)

oa_metadata_all <- rbind(oa_metadata_1, oa_metadata_2, oa_metadata_3, oa_metadata_4, oa_metadata_5, oa_metadata_6)
saveRDS(oa_metadata_all, "/Users/maddi/Documents/Pubpeer Gender/Révisions QSS/oa_metadata_all.rds")  

ids <- id[500001:500005]
ids2 <- c("W2755950973", "W2741809807")
ids3 <- c("W3121474013", "W3121475943") #, "W3121476185", "W3121476484", "W3121479212")

works_from_ids <- oa_fetch(
  entity = "works",
  id = ids2,
  verbose = TRUE
)


works_from_ids3 <- oa_fetch(
  entity = "works",
  id = ids3,
  verbose = TRUE
)






# Liste des identifiants d'œuvres à vérifier
ids <- id[600001:600005]

# Vérifier la validité des identifiants d'œuvres
valid_ids <- character()
invalid_ids <- character()

for (work_id in ids) {
  # Effectuer une requête pour vérifier si l'identifiant d'œuvre est valide
  response <- GET(paste0("https://api.openalex.org/works/", work_id))
  
  # Vérifier le code de statut HTTP
  if (response$status_code == 200) {
    # L'identifiant d'œuvre est valide
    valid_ids <- c(valid_ids, work_id)
  } else {
    # L'identifiant d'œuvre est invalide
    invalid_ids <- c(invalid_ids, work_id)
  }
}

# Afficher les identifiants d'œuvres valides et invalides
cat("Identifiants d'œuvres valides :", valid_ids, "\n")
cat("Identifiants d'œuvres invalides :", invalid_ids, "\n")

# Si des identifiants d'œuvres invalides ont été trouvés, les corriger ou les supprimer de la liste
if (length(invalid_ids) > 0) {
  # Corriger ou supprimer les identifiants d'œuvres invalides de la liste ids
  ids <- setdiff(ids, invalid_ids)
}

# Réessayer de récupérer les métadonnées en utilisant la liste corrigée d'identifiants d'œuvres
works_from_ids <- oa_fetch(entity = "works", id = ids, verbose = TRUE)

