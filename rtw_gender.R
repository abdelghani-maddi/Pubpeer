#####################################################################
### Analyse de données pour le papier collab h-f et retractations ###
#####################################################################
###               Toutes les données RetractionWatch              ###
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
library(stringi)
library(gtsummary)

#library(bibliometrix)
# library(openalexR)
# library(stringr)
# Charger à nouveau le package


# Pour mettre à jour le package bibliometrix 
# install.packages("remotes")
# library(remotes)
# remotes::install_github("massimoaria/bibliometrix")

## Données RetractionWatch ----
rtw <- read_excel("~/Documents/Pubpeer Gender/RWDBDNLD04242023.xlsx", sheet = "RWDBDNLD04242023") ### bdd retractations (version avril 2023)
rtw <- read_excel("D:/Pubpeer Gender/RWDBDNLD04242023.xlsx", sheet = "RWDBDNLD04242023") ### bdd retractations (version avril 2023)

givenNames <- read_excel("~/Documents/Pubpeer Gender/gender_proba.xlsx")
givenNames <- read_excel("D:/Pubpeer Gender/gender_proba.xlsx")

givenNames2 <- read_excel("~/Documents/Pubpeer Gender/gender_proba_2.xlsx")
givenNames2 <- read_excel("D:/Pubpeer Gender/gender_proba_2.xlsx")

givenNames <- givenNames %>%
  select(given_name, gender, proba) %>%
  unique()

givenNames2 <- givenNames2 %>%
  select(given_name, gender, proba) %>%
  unique()

givenNames <- bind_rows(givenNames, givenNames2)
rm(givenNames2)

# Utiliser la fonction separate_rows pour séparer les valeurs de la colonne 'Author' en lignes
rtw_melted <- rtw %>%
  separate_rows(Author, sep = ";") %>%  
  group_by(`Record ID`) %>%
  mutate(AuthorNumber = row_number()) %>%  # Ajouter le numéro de l'auteur pour chaque groupe
  ungroup()  # Dégroupement pour réinitialiser


# Ajouter la colonne 'FirstLast' : pour premier/dernier auteur, nombre d'auteurs et prénom
rtw_melted <- rtw_melted %>%
  group_by(`Record ID`) %>%
  mutate(IsFirstLast = if_else(AuthorNumber == 1 | AuthorNumber == max(AuthorNumber), 1, 0),
         nb_aut = max(AuthorNumber),
         FirstName = word(trimws(Author), 1))
  
# Supprimer la partie qui vient après le tiret quand il y en a
rtw_melted$FirstName <- sub("-.*", "", rtw_melted$FirstName)

# minuscule
rtw_melted$FirstName <- tolower(rtw_melted$FirstName)

# Distinct
givenNames <- givenNames %>%
  select(gender, given_name, proba) %>%
  unique()

# Matching
df_final <- left_join(rtw_melted, givenNames, by = c("FirstName"= "given_name")) # matcher

# Prénoms dans RetractionWatch qui sont absents dans Pubpeer
# no_matched_names <- df_final %>%
#   filter(is.na(gender) & nchar(FirstName)>1 & str_detect(FirstName, "^[[:alpha:]]+$")) %>%
#   select(FirstName) %>%
#   unique()
# write.csv(no_matched_names, "no_matched_names.csv")

# Usage de genderizeR
# devtools::install_github("kalimu/genderizeR")
# library(genderizeR)
# givenNames_2 = findGivenNames(no_matched_names$FirstName, progress = FALSE, apikey = '******************')
# names(givenNames_2) <- c("id", "given_name", "gender",  "proba", "country_id")
# write.xlsx(givenNames_2, "~/Documents/Pubpeer Gender/gender_proba_2.xlsx")

# AJOUTER UNE COLONNE POUR INDIQUER SI LES FEMMES SE TROUVENT EN PREMIERE OU DERNIERE POSITION
df_final <- df_final %>%
  group_by(`Record ID`) %>%
  mutate(woman_leader = case_when(gender == "female" & proba >= 0.6 & IsFirstLast == 1 ~ 1, TRUE ~ 0),
         man_leader = case_when(gender == "male" & proba >= 0.6 & IsFirstLast == 1 ~ 1, TRUE ~ 0))

# Généralisation par Record ID :
df_final <- df_final %>%
  group_by(`Record ID`) %>%
  mutate(woman_leader = ifelse(any(woman_leader == 1), 1, woman_leader),
         man_leader = ifelse(any(man_leader == 1), 1, man_leader))

# stats desc juste pour vérif
df_test <- df_final %>%
  select(`Record ID`, woman_leader) %>%
  unique()
# stats desc juste pour vérif
df_test <- df_final %>%
  select(`Record ID`, man_leader) %>%
  unique()
## Perfecto !


# Créer une nouvelle colonne gender_pro_06 en fonction des conditions
df_final$gender_pro_06 <- df_final$gender

# Assigner "initials" pour les cas où gender est NA et FirstName a un seul caractère
df_final$gender_pro_06[is.na(df_final$gender) & nchar(df_final$FirstName) == 1] <- "initials"

# Assigner "undefined" pour les cas où gender est NA et FirstName a plus d'un seul caractère
df_final$gender_pro_06[is.na(df_final$gender) & nchar(df_final$FirstName) > 1] <- "undefined"

# Assigner "unisex" pour les cas où la colonne proba de df_final < 0.6
df_final$gender_pro_06[df_final$proba < 0.6] <- "unisex"

# nagender <- df_final %>%
#  filter(is.na(gender_pro_06))
# rm(nagender)

# Assigner "undefined" pour les cas où l'auteur est NA : 8 cas
df_final$gender_pro_06[is.na(df_final$gender_pro_06)] <- "undefined"

# Calcul de la proportion des femmes par publication
###
`%not_in%` <- purrr::negate(`%in%`)

tbfin <- df_final %>%
  subset(., gender_pro_06 %not_in% c("initials", "unisex", "undefined")) %>%
  group_by(`Record ID`) %>%
  summarize(female_part = mean(gender_pro_06 == "female", na.rm = TRUE))

################################################################################
################################################################################
################################################################################

# Faire une jointure
df_nb_aut <- df_final
# Renommer la colonne dans df_nb_aut
colnames(df_nb_aut)[colnames(df_nb_aut) == "Record ID"] <- "Record_ID"

# Renommer la colonne dans tbfin
colnames(tbfin)[colnames(tbfin) == "Record ID"] <- "Record_ID"

# Matcher
df_nb_aut <- merge(df_nb_aut, tbfin, by.x = "Record_ID", by.y = "Record_ID", all.x = TRUE) 

# Ajouter la variable "Gtype"
df_nb_aut$Gtype <- ifelse(df_nb_aut$female_part == 0 & df_nb_aut$nb_aut == 1, "Man alone", 
                          ifelse(df_nb_aut$female_part == 1 & df_nb_aut$nb_aut == 1, "Woman alone",
                                 ifelse(df_nb_aut$female_part == 0 & df_nb_aut$nb_aut > 1, "Collab. men only",
                                        ifelse(df_nb_aut$female_part == 1 & df_nb_aut$nb_aut > 1, "Collab. women only",
                                               ifelse(df_nb_aut$female_part > 0 & df_nb_aut$female_part < 1 & df_nb_aut$nb_aut ==2 & df_nb_aut$woman_leader==1, "Collab. men-women 2 auteurs", 
                                                    ifelse(df_nb_aut$female_part > 0 & df_nb_aut$female_part < 1 & df_nb_aut$nb_aut > 1 & df_nb_aut$woman_leader==1, "Collab. men-women w lead", 
                                                          ifelse(df_nb_aut$female_part > 0 & df_nb_aut$female_part < 1 & df_nb_aut$nb_aut > 1 & df_nb_aut$woman_leader==0, "Collab. men-women m lead", NA)
                                               )
                                        )
                                 )
                            )
                       )
                  )

####################################
# Ajouter une autre variable sur le répartition du type de collab. H F. 
##
# cela consiste à :
# dupliquer la colonne Gtype en Gtype2, en modifiant les modalités selon ces conditions : 
# pour les valeurs de Gtype différentes de : ("Woman alone", "Man alone", "Collab. men only", "Collab. women only"), 
# si w_corresp=1, Gtype2="Collab. men-women . w corr", si m_corresp=1, Gtype2="Collab. men-women . m corr"
# Ajouter le flag femme auteur de correspondance (proxy : 1er auteur)
df_nb_aut <- df_nb_aut %>%
  mutate(w_corresp = ifelse(AuthorNumber == 1 & gender_pro_06 == "female", 1, 0))

# Ajouter le flag homme auteur de correspondance (proxy : 1er auteur)
df_nb_aut <- df_nb_aut %>%
  mutate(m_corresp = ifelse(AuthorNumber == 1 & gender_pro_06 == "male", 1, 0))

##
df_nb_aut <- df_nb_aut %>%
  group_by(Record_ID) %>%
  mutate(w_corresp = ifelse(any(w_corresp == 1), 1, w_corresp),
         m_corresp = ifelse(any(m_corresp == 1), 1, m_corresp))


##
# Ajouter la variable "Gtype2"
df_nb_aut$Gtype2 <- ifelse(df_nb_aut$female_part == 0 & df_nb_aut$nb_aut == 1, "Man alone", 
                           ifelse(df_nb_aut$female_part == 1 & df_nb_aut$nb_aut == 1, "Woman alone",
                                  ifelse(df_nb_aut$female_part == 0 & df_nb_aut$nb_aut > 1, "Collab. men only",
                                         ifelse(df_nb_aut$female_part == 1 & df_nb_aut$nb_aut > 1, "Collab. women only",
                                                ifelse(df_nb_aut$female_part > 0 & df_nb_aut$female_part < 1 & df_nb_aut$nb_aut > 1 & df_nb_aut$w_corresp==1, "Collab. men-women w lead", 
                                                       ifelse(df_nb_aut$female_part > 0 & df_nb_aut$female_part < 1 & df_nb_aut$nb_aut > 1 & df_nb_aut$m_corresp==1, "Collab. men-women m lead", 
                                                              NA)
                                                )
                                         )
                                  )
                           )
                    )


####################################
df_retract <- read_excel("/Users/maddi/Documents/Pubpeer Gender/df_gender_retract.xlsx") ## bdd sur le genre + bdd retractations (version avril 2023)
df_retract <- read_excel("D:/Pubpeer Gender/df_gender_retract.xlsx") ## bdd sur le genre + bdd retractations (version avril 2023)
 
df_retract_nb_comm <- read_excel("/Users/maddi/Documents/Pubpeer Gender/retraction_data.xlsx")
df_retract_nb_comm <- read_excel("D:/Pubpeer Gender/retraction_data.xlsx")

###
df_retract_nb_comm <- df_retract_nb_comm %>%
  select(publication, sum_nb_com_before_retract, sum_nb_com_after_retract, sum_nb_comm, retract_before_pubpeer)


retract_pubpeer <- df_retract %>%
  filter(is_retracted == 1 & !is.na(Gtype2)) %>%
  select(publication, ID_retractionwatch, Gtype2) %>%
  unique() %>%
  merge(., df_retract_nb_comm, by = "publication")


# Est retractée avant qu'elle soit commentée dans Pubpeer
df <- df_nb_aut %>%
  left_join(retract_pubpeer, by = c("Record_ID" = "ID_retractionwatch")) 

####################################
# Ajouter une colonne in_pubpeer
df <- df %>%
  mutate(in_pubpeer = ifelse(Record_ID %in% retract_pubpeer$ID_retractionwatch, 1, 0))

verif <- df %>%
  select(Record_ID, in_pubpeer) %>%
  unique() 

####################################
df <- df %>%
  mutate(retract_before_pubpeer = ifelse(is.na(sum_nb_com_after_retract) & is.na(sum_nb_comm) | sum_nb_com_after_retract == sum_nb_comm, 1, 0))

df$Gtype3 <- ifelse(!is.na(df$Gtype2.y), df$Gtype2.y, df$Gtype2.x)
##

##
## 57479 

retractionwatch_gender <- write.xlsx(df, "/Users/maddi/Documents/Pubpeer Gender/retraction_data.xlsx")
#######################################
#######################################
#######################################
#######################################

######## Analyse des données ##########

#######################################
#######################################
#######################################
#######################################

# Distribution des collab H-F dans Pubpeer et dans RW au sein des publications retractées
######

##
data <- retraction_data %>%
  select(Record_ID, Gtype, FirstName, nb_aut, proba, gender_pro_06, Gtype2, in_pubpeer, retract_before_pubpeer) %>%
  unique()

##
df %>%
  select(Record_ID, Gtype3, in_pubpeer, retract_before_pubpeer) %>%
  unique() %>%
  filter(!is.na(Gtype3)) %>%
  tbl_summary(
    include = c(Gtype3, in_pubpeer),
    by = (in_pubpeer),
    sort = list(everything() ~ "frequency"),
    statistic = list(
      all_continuous() ~ c("{N_obs}") 
    )
  ) %>%
  add_overall(last = TRUE) #, col_label = "**Ensemble** (effectif total: {N})")


groupes_gender <- df %>%
  select(Record_ID, Gtype3, in_pubpeer, sum_nb_com_after_retract, sum_nb_com_before_retract, sum_nb_comm) %>%
  unique() %>%
  mutate(com_avant_retract = ifelse(in_pubpeer == 1 & sum_nb_com_before_retract > 0, "Retracted after Pubpeer",
                                    ifelse(in_pubpeer == 1 & sum_nb_com_before_retract == 0, "Retracted before Pubpeer",
                                           ifelse(in_pubpeer == 0, "Retracted not in Pubpeer", NA))))

groupes_gender %>%
  select(Record_ID, Gtype3, in_pubpeer, com_avant_retract) %>%
  unique() %>%
  tbl_summary(
    include = c(Gtype3, in_pubpeer, com_avant_retract),
    by = (com_avant_retract),
    sort = list(everything() ~ "frequency"),
    statistic = list(
      all_continuous() ~ c("{N_obs}") 
    )
  ) %>%
  add_overall(last = TRUE) #, col_label = "**Ensemble** (effectif total: {N})")


write.xlsx(groupes_gender, "D:/Pubpeer Gender/groupes_gender.xlsx")
