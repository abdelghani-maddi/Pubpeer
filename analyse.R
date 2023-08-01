######################################################
# Analyse marché édition
######################################################

## Formatage espace de travail ----
rm(list = ls()) #supprimer tous les objets 

# chargement des packages ----
library(tibble)
library(textdata)
library(Hmisc)
library(zoo)
library(flextable)
library(data.table)
library(tidyverse)
library(trimmer)
library(DescTools)
library(questionr)
library(RPostgres)
library(lubridate)
library(timechange)
library(urltools)
library(stringr)
library(rebus)
library(Matrix)
library(plyr)
library(sjmisc)
library(gtsummary)
library(igraph)
library(openxlsx2)
library(readxl)
library(openxlsx)

# install.packages('remotes')
# remotes::install_github("gadenbuie/regexplain")

## Chargement des données ----

######################################################
# Bdd publications
data_pub <- read_excel("D:/bdd pubpeer/data_pub.xlsx")
######################################################
# Bdd commentaires
data_comm <- read_excel("D:/bdd pubpeer/data_comm2.xlsx")
######################################################
# bdd retractions
df_retract <- read_excel("D:/Pubpeer Gender/df_gender_retract.xlsx")
######################################################
# Nombre d'auteurs et de commentaires
nb_aut_com <- df_retract %>%
  select(publication, nb_aut, Nombre.de.commentaires)
names(nb_aut_com) <- c("publication", "nb_aut", "nb_com")
######################################################
# Données sur les rétractations
data_retract <- df_retract %>%
  select(publication, is_retracted, Reason) %>%
  separate_rows(Reason, sep = ";")%>%
  filter(Reason != "") %>%
  mutate(Reason = str_replace(Reason, "\\+", ""))
######################################################
# Bdd DOAJ
doaj <- read.csv("D:/bdd pubpeer/journalcsv__doaj_20230728_0635_utf8.csv")
######################################################
# combiner issn et eissn
doaj$combined_issn <- paste(doaj$Journal.ISSN..print.version., doaj$Journal.EISSN..online.version., sep = ", ")
# Utiliser separate_rows() pour éclater la colonne issn en plusieurs lignes
doaj <- separate_rows(doaj, combined_issn, sep = ",\\s*", convert = FALSE)
######################################################
# Données OpenAlex
openalex <- read_excel("D:/bdd pubpeer/journals_openalex.xlsx")
######################################################
######################################################
# Analyse ----
######################################################

## Préparation données ----
 # Exclure les publications de 2016 qui ont un seul commentaire du commentateur n°3845,
 # plus d'infos, voir l'article de https://www-nature-com.inshs.bib.cnrs.fr/articles/540151a

pub_a_exclure <- data_comm %>%
  filter(Commentateurs_recodés == "3845" & year(DateCreated) == 2016) %>%
  select(publication) %>%
  unique() %>%
  left_join(., nb_aut_com, by = "publication") %>%
  unique() %>%
  filter(nb_com == 1) %>% # publi avec un seul commentaire généré automatiquement : 47633 publications
  select(publication) %>%
  unique()

# Nettoyer les données des publications de pubpeer
# Extraire les colonnes "publication" et "issn"
data_jnal <- data_pub %>%
  select(publication, issn)

# Fonction pour supprimer les crochets et les apostrophes dans la colonne issn
clean_issn <- function(issn_list) {
  gsub("\\[|'|\\]", "", issn_list)
}

# Appliquer la fonction clean_issn pour supprimer les crochets et les apostrophes
data_jnal$issn <- sapply(data_jnal$issn, clean_issn)

# Utiliser separate_rows() pour éclater la colonne issn en plusieurs lignes
data_jnal <- separate_rows(data_jnal, issn, sep = ",\\s*", convert = FALSE)



# Faire les calculs par revue
df_jnal_pubpeer <- data_jnal %>%
  filter(!(publication %in% pub_a_exclure$publication) & issn != "None") %>%
  left_join(., openalex, by = "issn") %>%
  select(-issn) %>%
  unique() %>%
  filter(!is.na(id))

#######################################################
#######################################################
# Comptes par édieur : PubPeer
nb_edit_pubpeer <- df_jnal_pubpeer %>%
  select(host_organization_name) %>%
  group_by(host_organization_name) %>%
  count()  

# Calculer la somme totale de tous les travaux commentés dans pubpeer
pubpeer_works_count <- sum(nb_edit_pubpeer$freq)

# Utiliser la fonction group_by et summarise pour regrouper et effectuer les calculs
nb_edit_pubpeer <- nb_edit_pubpeer %>%
  mutate(part_pubpeer = (freq / pubpeer_works_count)*100)


# Comptes par édieur : tout openalex
nb_edit_openalex <- openalex %>%
  select(display_name, host_organization_name, works_count) %>%
  filter(!is.na(host_organization_name) & !is.na(works_count)) %>%
  unique() 

# Utiliser la fonction aggregate pour regrouper et sommer les valeurs
nb_edit_openalex <- aggregate(works_count ~ host_organization_name, data = nb_edit_openalex, sum)

# Renommer la colonne "works_count" en "n" pour correspondre à la sortie souhaitée
colnames(nb_edit_openalex_sum)[colnames(nb_edit_openalex_sum) == "works_count"] <- "n"

# Calculer la somme totale de tous les travaux
total_works_count <- sum(nb_edit_openalex$works_count)

# claculer la part dans le total monde
nb_edit_openalex <- nb_edit_openalex %>%
  mutate(part_tot = (works_count / total_works_count)*100)

##left joint de pubpeer et tot

distribu_edit <- left_join(nb_edit_pubpeer, nb_edit_openalex, by = "host_organization_name") %>%
  mutate(double_ratio = part_pubpeer / part_tot)

# les plus discutés
## Recodage de most_discussed$freq en most_discussed$freq_rec
most_discussed$freq_rec <- cut(most_discussed$freq,
                               include.lowest = TRUE,
                               right = FALSE,
                               dig.lab = 4,
                               breaks = c(1, 117, 498.5, 1160, 3062.5, 6852, 9377)
)
most_discussed <- distribu_edit %>%
  subset(., freq>117 & double_ratio >= 2) # 117 par rapport à la descritisation Fisher
#######################################################
#######################################################

#######################################################
#######################################################
# Comptes par édieur : PubPeer
# Faire les calculs par revue
df_jnal_pubpeer <- data_jnal %>%
  filter(!(publication %in% pub_a_exclure$publication) & issn != "None") %>%
  left_join(., openalex, by = "issn") %>%
  select(-issn) %>%
  unique() %>%
  filter(!is.na(id))

# Comptes par édieur : PubPeer
df_jnal_pubpeer <- df_jnal_pubpeer %>%
  select(id, display_name, works_count) %>%
  group_by(display_name) %>%
  count()

# Calculer la somme totale de tous les travaux commentés dans pubpeer
pubpeer_works_count <- sum(df_jnal_pubpeer$freq)

# Utiliser la fonction group_by et summarise pour regrouper et effectuer les calculs
df_jnal_pubpeer <- df_jnal_pubpeer %>%
  mutate(part_pubpeer = (freq / pubpeer_works_count)*100)


# Comptes par revue : tout openalex
df_jnal_openalex <- openalex %>%
  select(id, display_name,is_oa, works_count) %>%
  filter(!is.na(display_name) & !is.na(works_count)) %>%
  unique() 

# Utiliser la fonction aggregate pour regrouper et sommer les valeurs
# nb_jnal_openalex <- aggregate(works_count ~ display_name + is_oa, data = nb_jnal_openalex, sum)

# Calculer la somme totale de tous les travaux
# total_works_count <- sum(nb_jnal_openalex$works_count)

# claculer la part dans le total monde
df_jnal_openalex <- df_jnal_openalex %>%
  mutate(part_tot = (works_count / total_works_count)*100)

##left joint de pubpeer et tot

distribu_jnal <- left_join(df_jnal_pubpeer, df_jnal_openalex, by = "id") %>%
  mutate(double_ratio = part_pubpeer / part_tot)

## Recodage de distribu_jnal$freq en distribu_jnal$freq_rec
distribu_jnal$freq_rec <- cut(distribu_jnal$freq,
                              include.lowest = T,
                              right = T,
                              dig.lab = 4,
                              breaks = c(1, 23.5, 96, 264.5, 515, 1199, 1951)
)


## Recodage de distribu_jnal$double_ratio en distribu_jnal$double_ratio_rec
distribu_jnal$double_ratio_rec <- cut(distribu_jnal$double_ratio,
                                      include.lowest = TRUE,
                                      right = FALSE,
                                      dig.lab = 4,
                                      breaks = c(0.00852595269592414, 30.7551787478147, 164.148376354728, 910.603561181027, 2167.4458766214, 3604.89656366619, 4092.0447479454)
)

## Recodage de distribu_jnal$double_ratio en distribu_jnal$double_ratio_rec
distribu_jnal$double_ratio_rec2 <- cut(distribu_jnal$double_ratio,
                                      include.lowest = TRUE,
                                      right = FALSE,
                                      dig.lab = 4,
                                      breaks = c(0.0159784089011037, 12.42000827206, 42.2442860115367, 90.4578963419474, 158.880378547988, 545.335328512565, 755.079685632782)
)
# les plus discutés

most_discussed_jnal <- distribu_jnal %>%
  subset(., freq>=23.5 & double_ratio >= 12.42) # top 20 discutés

describe(most_discussed_jnal$is_oa)
describe(distribu_jnal$is_oa)
describe(df_jnal_openalex$is_oa)

# Calculer les fréquences pour avoir une idée de la distribution des oa dans pubpeer
distribu_jnal$is_oa <- factor(distribu_jnal$is_oa)

oa_jnl_pubpeer <- distribu_jnal$is_oa |>
  fct_infreq() |> 
  questionr::freq() %>%
  mutate(dataset = "PubPeer")

# Calculer les fréquences pour avoir une idée de la distribution des oa dans pubpeer
most_discussed_jnal$is_oa <- factor(most_discussed_jnal$is_oa)

oa_jnl_most_pubpeer <- most_discussed_jnal$is_oa |>
  fct_infreq() |> 
  questionr::freq() %>%
  mutate(dataset = "Most commented in PubPeer")

# Calculer les fréquences pour avoir une idée de la distribution des oa dans total
df_jnal_openalex$is_oa <- factor(df_jnal_openalex$is_oa)

oa_jnl_tot <- df_jnal_openalex$is_oa |>
  fct_infreq() |> 
  questionr::freq() %>%
  mutate(dataset = "OpenAlex")

oa_stat <- rbind(oa_jnl_tot, oa_jnl_pubpeer, oa_jnl_most_pubpeer) %>%
  mutate(is_oa = row.names(.))
write.xlsx(oa_stat, "D:/bdd pubpeer/oa_stat_jnals.xlsx")

## Parmi les OA lesquels sont le plus discutés ?

most_discussed <- most_discussed %>%
  mutate(part_discus_in_all = (freq/works_count)*100)


# exports
write.xlsx(nb_edit_openalex, "D:/bdd pubpeer/distrib editeurs openalex.xlsx")

# calcul de l'indice de gini
library(ineq)

Gini(nb_edit_openalex$works_count)
Gini(nb_edit_pubpeer$freq)


# Les plus discutés dans pubpeer : éditeurs
write.xlsx(most_discussed, "D:/bdd pubpeer/most commented editors.xlsx")
# Les plus discutés dans pubpeer : éditeurs
write.xlsx(most_discussed_jnal, "D:/bdd pubpeer/most commented journals.xlsx")



# Calcul du NOAI pour PubPeer

pub_pubpeer <- data_pub %>%
  select(publication, Journal_Categories_WOS, Open_Access) %>%
  filter(!is.na(Journal_Categories_WOS)) %>%
  unique() %>%
  mutate(is_oa = grepl("True", Open_Access)) %>%
  separate_rows(Journal_Categories_WOS, sep = '",') %>%
  mutate(Journal_Categories_WOS = str_remove_all(Journal_Categories_WOS, '["\\[\\]]')) %>%
  mutate(Journal_Categories_WOS = str_trim(Journal_Categories_WOS, side = "left")) %>%
  mutate(Journal_Categories_WOS = toupper(Journal_Categories_WOS))

# nombre qui ont matché avec wos hors les exclues
match_wos <- pub_pubpeer %>%
  select(publication) %>%
  filter(!(publication %in% pub_a_exclure$publication)) %>%
  unique()

# Nombres oa et non  oa par discipline dans pubpeer

nb_oa_disc_pubpeer <- pub_pubpeer %>%
  select(Journal_Categories_WOS, is_oa) %>%
  group_by(Journal_Categories_WOS, is_oa) %>%
  count() %>%
  pivot_wider(names_from = is_oa, values_from = freq, names_prefix = "is_oa_") %>%
  mutate(is_oa_TRUE = ifelse(is.na(is_oa_TRUE), 0, is_oa_TRUE),
         is_oa_FALSE = ifelse(is.na(is_oa_FALSE), 0, is_oa_FALSE)) %>%
  mutate(part_oa_pubpeer = (is_oa_TRUE / (is_oa_TRUE+is_oa_FALSE))*100) %>%
  mutate(nb_doc_pubpeer = is_oa_TRUE+is_oa_FALSE)

# Télécharger les données pour WoS Incites Research Areas
wos_oa <- read.csv("D:/bdd pubpeer/Incites Research Areas.csv")

wos_oa <- wos_oa %>%
  select(Name, Web.of.Science.Documents, X..All.Open.Access.Documents) %>%
  mutate(Journal_Categories_WOS = Name,
         nb_doc_wos = Web.of.Science.Documents,
         part_oa_wos = X..All.Open.Access.Documents) %>%
  select(Journal_Categories_WOS, nb_doc_wos, part_oa_wos)

# données pour le noai
noai <- left_join(nb_oa_disc_pubpeer, wos_oa, by = "Journal_Categories_WOS")

# 5 disciplines ne matchent pas
corr_disc <- noai %>%
  filter(is.na(part_oa_wos))

# Utilisation de recode pour remplacer les valeurs dans la colonne Journal_Categories_WOS
nb_oa_disc_pubpeer <- nb_oa_disc_pubpeer %>%
  mutate(Journal_Categories_WOS = recode(
    Journal_Categories_WOS,
    "PSYCHOLOGY, DEVELOPMENT" = "PSYCHOLOGY, DEVELOPMENTAL",
    "CARDIAC & CARDIOVASCULAR SYSTEM" = "CARDIAC & CARDIOVASCULAR SYSTEMS",
    "MATERIALS SCIENCE, CHARACTERIZATION, TESTING" = "MATERIALS SCIENCE, CHARACTERIZATION & TESTING",
    "PERIPHERAL VASCULAR DISEASES" = "PERIPHERAL VASCULAR DISEASE",
    "WOMEN'S STUDIES" = "Women's Studies"
  ))

# données pour le noai : 2 essai
noai <- left_join(nb_oa_disc_pubpeer, wos_oa, by = "Journal_Categories_WOS")

noai <- noai %>%
  mutate(doubl_ratio = part_oa_pubpeer/part_oa_wos) %>%
  mutate(pond_nb_pubpeer = doubl_ratio*nb_doc_pubpeer,
         pond_nb_wos = doubl_ratio*nb_doc_wos) %>%
  filter(nb_doc_pubpeer > 1)

noai1 <- (sum(noai$pond_nb_pubpeer) / sum(noai$nb_doc_pubpeer))
noai2 <- (sum(noai$pond_nb_wos) / sum(noai$nb_doc_wos))

# récupérer les nouvelles données OA sur pubpeer sinon ce serait pas correct car wos extrait 31-07-2023
doi_pubpeer <- data_pub %>%
  filter(!is.na(Journal_Categories_WOS)) %>%
  select(DOI) %>%
  filter(DOI != "None")

# 
library(openalexR)

works_from_dois_openalex <- oa_fetch(
  entity = "works",
  doi = doi_pubpeer$DOI,
  verbose = TRUE
)


# Créer une liste pour stocker les résultats
results_list <- list()

# Boucle pour extraire les données DOI par DOI
for (i in 1:nrow(doi_pubpeer)) {
  doi <- doi_pubpeer$DOI[i]
  works_from_dois_openalex <- oa_fetch(
    entity = "works",
    doi = doi,
    verbose = TRUE
  )
  results_list[[i]] <- works_from_dois_openalex
}

# Combinez les résultats en un seul dataframe (si nécessaire)
combined_results <- do.call(rbind, results_list)

# Afficher les résultats
print(combined_results)

# pub par année
pub_a_exclure2 <- pub_a_exclure %>%
  mutate(flag_exclu = 1)

com_ann <- data_comm %>%
  select(publication, DateCreated) %>%
  mutate(Année = year(DateCreated)) %>%
  unique() %>%
  left_join(., pub_a_exclure2, by = "publication") %>%
  mutate(flag_exclu = ifelse(is.na(flag_exclu), 0, flag_exclu)) %>%
  select(Année, flag_exclu) %>%
  filter(Année>=2000) %>%
  group_by(Année, flag_exclu) %>%
  count()
  
# exporter
write.xlsx(com_ann, "D:/bdd pubpeer/com_ann.xlsx")
