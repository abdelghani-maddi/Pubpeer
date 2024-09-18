## Formatage espace de travail ----
rm(list = ls()) #supprimer tous les objets 

# chargement des packages ----
library(readxl)
library(tidyverse)
library(gtsummary)
library(lubridate)
library(questionr)
library(openxlsx2)

# 
# library(stringr)
# library(tibble)
# library(tidytext)
# library(textdata)
# library(Hmisc)
# library(zoo)
# library(flextable)
# library(data.table)
# library(trimmer)
# library(DescTools)
# library(timechange)
# library(urltools)
# library(rebus)
# library(Matrix)
# library(plyr)
# library(sjmisc)
# library(regexplain)
# library(igraph)


data_comm <- read_excel("D:/bdd pubpeer/data_comm2.xlsx")
data_pub <- read_excel("D:/bdd pubpeer/data_pub.xlsx")

# Transformer le format de la date du commentaire
data_comm$date_com <- data_comm$DateCreated
data_comm$comm <- data_comm$html
data_comm$date_com <- as.Date.character(data_comm$date_com)
data_comm$commentateur <- data_comm$Commentateurs_recodés

# Calculer la courbe de Lorenz
commentateurs <- data_comm %>%
  select(publication, commentateur, comm, DateCreated) %>%
  filter(!(commentateur=="3845")) %>% # +de 50000 commentaires automatiques
  group_by(commentateur) %>%
  summarize(nb_com = n()) %>%
  mutate(part = nb_com / sum(nb_com)) %>%
  arrange(part) %>%
  mutate(part_cum = cumsum(part))

# Ajouter une colonne pour la ligne de 45 degrés
commentateurs <- commentateurs %>%
  mutate(quantile = cumsum(part) / sum(part))

#write_xlsx(commentateurs, "D:/bdd pubpeer/Analyse commentaires/commentateurs.xlsx")
library(DescTools)

gini_index <- 1 - 2 * sum(commentateurs$part_cum) / nrow(commentateurs)
Gini(commentateurs$nb_com) # Gini = 0.630214

# Calcul des percentiles
commentateurs <- commentateurs %>%
  mutate(percentile = cut(part_cum, breaks = quantile(part_cum, probs = seq(0, 1, by = 0.1)), labels = FALSE))


# Définir les conditions pour chaque modalité
commentateurs <- commentateurs %>%
  mutate(modalite = case_when(
    percentile >= 9 ~ "top1-10%",
    percentile >= 8 ~ "top11-20%",
    percentile >= 7 ~ "top21-30%",
    TRUE ~ "autres"
  ))



# extraires les lanceurs de discussion
nb_com_pub <- data_comm %>%
  select(publication, inner_id) %>%
  group_by(publication) %>%
  summarise(nb_com_pub = n_distinct(inner_id)) %>%
  filter(nb_com_pub>1)

lanceurs <- data_comm %>%
  select(publication, inner_id, commentateur) %>%
  filter(inner_id == 1 & !(commentateur=="3845") & publication %in% nb_com_pub$publication)


# Distribution 75.6% des premiers commentaires sont écrits par 20% des commentateurs
profil_lanceurs <- left_join(lanceurs, commentateurs, by = "commentateur")
questionr::describe(profil_lanceurs$modalite)

# Extraire les commentaires des lanceurs - premiers commentateurs
comm_lanceurs <- left_join(lanceurs, data_comm, 
                           by = c("commentateur", "publication", "inner_id")) %>% unique()


comm_lanceurs_select <- comm_lanceurs %>%
  select(commentateur, comm) %>%
  mutate(commentateur_comm = paste0(commentateur,"-",comm))
# write_xlsx(comm_lanceurs, "D:/bdd pubpeerAnalyse commentaires/comm_lanceurs.xlsx")
# write_csv(comm_lanceurs, "comm_lanceurs.csv")

comm_all <- data_comm %>%
  select(commentateur, comm) %>%
  mutate(commentateur_comm = paste0(commentateur,"-",comm))
# write_xlsx(comm_all, "comm_all.xlsx")
# write_csv(comm_all, "comm_all.csv")


  
comm_all_sans_3845 <- data_comm %>%
  select(commentateur, comm) %>% 
  filter(!(commentateur=="3845")) %>%
  mutate(commentateur_comm = paste0(commentateur,"-",comm))
# write_xlsx(comm_all, "comm_all.xlsx")
# write_csv(comm_all_sans_3845, "comm_all_sans_3845.csv")
  
    
# Créer une combinaison de tous les commentateurs pour chaque publication
pairs <- data_comm %>%
  select(publication, commentateur) %>%
  distinct() %>%
  inner_join(data_comm %>% select(publication, commentateur) %>% distinct(), by = "publication") %>%
  filter(commentateur.x != commentateur.y)

# Compter le nombre de publications pour chaque paire de commentateurs
pairs_count <- pairs %>%
  group_by(commentateur.x, commentateur.y) %>%
  summarise(nb_pub = n_distinct(publication))



# Filtrer les publications avec au moins deux commentateurs uniques
valid_publications <- data_comm %>%
  group_by(publication) %>%
  filter(n_distinct(commentateur) >= 2) %>%
  pull(publication)


# Générer toutes les combinaisons de commentateurs pour chaque publication valide
all_combinations <- data_comm %>%
  filter(publication %in% valid_publications) %>%
  group_by(publication) %>%
  summarize(commentateurs = list(combn(unique(commentateur), 2, simplify = FALSE))) %>%
  unnest(commentateurs) %>%
  mutate(commentateur1 = sapply(commentateurs, `[`, 1),
         commentateur2 = sapply(commentateurs, `[`, 2)) %>%
  select(-commentateurs)

# Compter le nombre de publications pour chaque paire de commentateurs
pair_counts <- all_combinations %>%
  group_by(commentateur1, commentateur2) %>%
  summarize(nb_pub = n_distinct(publication))


#write_xlsx(pair_counts, "D:/bdd pubpeer/Analyse commentaires/cocommentaires.xlsx")



###############################################################
###############################################################

dico_misconduct <- read_excel("D:/bdd pubpeer/dico_misconduct.xlsx")

com <- data_comm %>%
  select(publication, commentateur, comm, DateCreated) %>%
  unique()

# Supprimer les espaces au début de chaque ligne dans la colonne 'terms'
dico_misconduct$terms <- trimws(dico_misconduct$terms, which = "left")

################################################################################
# Fonction pour nettoyer et mettre en minuscule la colonne 'comm'
clean_comm <- function(comment) {
  comment <- tolower(comment) # Convertir en minuscules
  comment <- str_remove_all(comment, "<[^>]+>") # Supprimer les balises HTML
  comment <- str_replace_all(comment, "[^a-z\\s]", "") # Supprimer les caractères spéciaux et chiffres
  comment <- str_squish(comment) # Supprimer les espaces en trop
  return(comment)
}

# Appliquer la fonction sur la colonne 'comm'
com <- com %>%
  mutate(cleaned_comm = sapply(comm, clean_comm))


################################################################################
# # Fonction pour nettoyer un vecteur de commentaires
# clean_comm_vector <- function(comments) {
#   comments <- tolower(comments) # Convertir en minuscules
#   comments <- str_remove_all(comments, "<[^>]+>") # Supprimer les balises HTML
#   comments <- str_replace_all(comments, "[^a-z\\s]", "") # Supprimer les caractères spéciaux et chiffres
#   comments <- str_squish(comments) # Supprimer les espaces en trop
#   return(comments)
# }
# 
# # Nettoyer tous les commentaires
# data_comm <- data_comm %>%
#   mutate(cleaned_comm = clean_comm_vector(comm))
# 
# # Créer une expression régulière combinée une seule fois
# pattern <- str_c("\\b", dico_misconduct$terms, "\\b", collapse = "|")
# 
# # Fonction pour trouver les termes correspondant dans les commentaires
# match_terms_vector <- function(comments, pattern) {
#   matches <- str_extract_all(comments, pattern)
#   matches <- sapply(matches, function(x) if(length(x) > 0) paste(unique(x), collapse = ", ") else NA)
#   return(matches)
# }
# 
# # Appliquer la fonction de matching sur tous les commentaires nettoyés
# data_comm <- data_comm %>%
#   mutate(matched_terms = match_terms_vector(cleaned_comm, pattern))
# 
# 
# 
# ################################################################################
# # Nettoyer tous les commentaires
# data_comm <- data_comm %>%
#   mutate(cleaned_comm = clean_comm_vector(comm))
# 
# # Créer une expression régulière combinée une seule fois
# pattern <- str_c("\\b", dico_misconduct$terms, "\\b", collapse = "|")
# 
# # Extraire les termes correspondant dans les commentaires
# extract_terms <- function(comment, pattern) {
#   matches <- str_extract_all(comment, regex(pattern, ignore_case = TRUE))
#   unique_matches <- unique(unlist(matches))
#   if (length(unique_matches) > 0) {
#     return(paste(unique_matches, collapse = ", "))
#   } else {
#     return(NA)
#   }
# }
# 
# # Appliquer la fonction d'extraction sur tous les commentaires nettoyés
# data_comm <- data_comm %>%
#   rowwise() %>%
#   mutate(matched_terms = extract_terms(cleaned_comm, pattern)) %>%
#   ungroup()


# Fonction pour vérifier les termes dans les commentaires
find_terms_in_comments <- function(comment, terms) {
  matched_terms <- terms[sapply(terms, function(term) str_detect(comment, fixed(term)))]
  if (length(matched_terms) > 0) {
    return(paste(matched_terms, collapse = ", "))
  } else {
    return(NA)
  }
}

dico_misconduct <- read_excel("D:/bdd pubpeer/dico_misconduct.xlsx")
# Supprimer les espaces au début de chaque ligne dans la colonne 'terms'
dico_misconduct$terms <- trimws(dico_misconduct$terms, which = "left")


# Appliquer la fonction de vérification sur tous les commentaires nettoyés
com <- com %>%
  rowwise() %>%
  mutate(matched_terms = find_terms_in_comments(cleaned_comm, dico_misconduct$terms)) %>%
  ungroup()


library(lubridate)

com$annee <- year(com$DateCreated)

na_match <- com %>%
  
  filter(is.na(.$matched_terms))


# Séparer les termes correspondants en plusieurs lignes
com2 <- com %>%
  separate_rows(matched_terms, sep = ", ")

# Faire un left join avec dico_misconduct
com2 <- com2 %>%
  left_join(dico_misconduct, by = c("matched_terms" = "terms"))


# write_xlsx(com2, "com2.xlsx")
com2 <- read_excel("com2.xlsx")

# Compter le nombre de lignes par publication
com2 <- com2 %>%
  group_by(publication) %>%
  mutate(line_count = n()) %>%
  ungroup()

# Calculer le compte fractionnaire pour chaque terme
com2 <- com2 %>%
  mutate(fractional_count = 1 / line_count)

com2 <- com2 %>%
  filter(!is.na(.$matched_terms))

NA_com <- com2 %>%
  filter(is.na(.$matched_terms)) %>%
  filter(!is.na(.$cleaned_comm)) %>%
  unique()

# write_xlsx(NA_com, "D:/bdd pubpeer/NA_com.xlsx")

######################################################
######################################################
######################################################
options(scipen = 999)

nbr_com_type_annee <- com2 %>%
  filter(!(commentateur=="3845")) %>%
  select(annee, Type, fractional_count) %>%
  filter(!is.na(.$Type)) %>%
  group_by(annee, Type) %>%
  summarise(n = sum(fractional_count)) # compte fractionnaire

# nbr_com_type_annee <- com2 %>%
#   filter(!(commentateur=="3845")) %>%
#   select(annee, Type, fractional_count) %>%
#   group_by(annee, Type) %>%
#   summarise(n = n()) # compte entier


# Calculer le total des comptes fractionnaires par année
total_com_par_annee <- nbr_com_type_annee %>%
  group_by(annee) %>%
  summarise(total_n = sum(n), .groups = 'drop')

# Calculer la part de chaque type de méconduite par année
nbr_com_type_annee <- nbr_com_type_annee %>%
  left_join(total_com_par_annee, by = "annee") %>%
  mutate(part = n / total_n) %>%
  select(annee, Type, part)

# write_xlsx(nbr_com_type_annee, "D:/bdd pubpeer/nbr_com_type_annee.xlsx")


nbr_com_type_annee %>%
  filter(!is.na(Type)) %>%
  ggplot() +
  aes(x = annee, y = part) +
  geom_line(colour = "#112446") +
  theme_minimal() +
  facet_wrap(vars(Type), scales = "free_y")

library(alluvial)

nbr_com_type_annee$Type <- as.factor(nbr_com_type_annee$Type)

nbr_com_type_annee <- data.frame(nbr_com_type_annee$Type, 
                                    nbr_com_type_annee$annee, nbr_com_type_annee$part)

names(nbr_com_type_annee) <- c("Type", "annee", "part")

alluvial_ts(nbr_com_type_annee, wave = NA, ygap = 1, col = NA, alpha = NA,
            plotdir = "up", rankup = FALSE, lab.cex = 1, lab.col = "black",
            xmargin = 0.1, axis.col = "black", title = NA, title.cex = 1,
            axis.cex = 1, grid = FALSE, grid.col = "grey80", grid.lwd = 1,
            leg.mode = TRUE, leg.x = 0.1, leg.y = 0.9, leg.cex = 1,
            leg.col = "black", leg.lty = NA, leg.lwd = NA, leg.max = NA,
            xlab = NA, ylab = NA, xlab.pos = 2, ylab.pos = 1, lwd = 1)
