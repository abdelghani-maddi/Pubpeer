# Analyse des médias
rm(list = ls()) #supprimer tous les objets 


library(tidyverse)
library(questionr)
library(RPostgres)
library(lubridate)
library(urltools)
library(TraMineR)
library(cluster)
library(seqhandbook)
library(ade4)
library(explor)
library(FactoMineR)
library(factoextra)
library(labelled)
library(openxlsx)
library(openxlsx2)
library(officer)

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


# donnees depuis local w
data_urls <- readxl::read_excel("~/Documents/Pubpeer project/Pubpeer explo/donnees_urls_fin.xlsx")

# Récupération des données

dfMed <- subset(data_urls, typo == 'Médias') %>%
  unique()

# Données sur la classification
classification_em <- readxl::read_excel("~/Documents/Pubpeer project/Pubpeer explo/classification-em.xlsx", 
                                sheet = "Sheet 1")



# Préparation des données avec la nouvelle classification
# selectionner uniquement les deux colonnes "domain" et "typo" pour matcher avec la classification d'Emmanuel
domains <- data_urls %>%
  select(domain, typo) %>%
  unique()
# Classification d'Emmanuel
cl_em <- classification_em %>%
  select(site, Type_3_Catégrories2) %>%
  data.frame()
#cl_em$site <- tolower(cl_em$site)
names(cl_em) <- c("domain", "typoC2")
# l'ancienne classification utilisée pour le fichier d'EM
old_class <- readxl::read_excel("classification sites.xlsx")
# matcher old_class avec le fichier d'EM pour récupérer le domaine original et non celui de l'unification
cl_em2 <- merge(cl_em, old_class, by.x = "domain", by.y = "type_sit", all.x = T)


# Calcul de la fréquence des sites pour avoir une idée plus précise
f <- factor(dfMed$domain) |>
  fct_infreq() |> 
  questionr::freq()
freqmed <- data.frame(rownames(f),f)
names(freqmed) = c("site","nb","part","freq")

# Calcul de la distance Levenshtein et récupération du fichier
# pour le calcul le fichier "Levenshtein distance.R" est utilisé
# fichier de sortie : voir dossier bdd
grp_leven <- readxl::read_excel("D:/bdd/grp_levenshtein.xlsx")
grp_leven <- readxl::read_excel("~/Documents/Pubpeer project/Pubpeer explo/grp_levenshtein 2.xlsx")
# Parcourir chaque élément de la colonne "pattern" du dataframe "grp_leven"
for (i in 1:nrow(grp_leven)) {
  # Trouver les indices des éléments de la colonne "domain" qui contiennent la chaîne de caractères spécifiée dans "pattern"
  idx <- grep(grp_leven$pattern[i], dfMed$domain)
  # Remplacer les valeurs correspondantes dans la colonne "domain" par la valeur correspondante dans la colonne "remplacement"
  dfMed$domain[idx] <- grp_leven$remplacement[i]
}

## Nombre d'apparitions par année
nb <- dfMed %>%
  select(domain, annee)
  
nb <- nb %>%
  group_by(domain, annee) %>%
  summarise(n = n())


nb2 <- nb %>%
  group_by(annee) %>%
  mutate(total = sum(n)) %>%
  group_by(domain) %>%
  mutate(part = n/total*100) %>%
  ungroup()

# Calculer le nombre total de chaque domaine
total_domain <- nb2 %>% 
  group_by(domain) %>% 
  summarise(total_domain = sum(n))

# Calculer la part de chaque domaine dans le total de toutes les années
nb3 <- nb2 %>%
  left_join(total_domain, by = "domain") %>%
  mutate(part_annee = n/sum(n)*100,
         part_total = n/total_domain*100) %>%
  select(domain, annee, n, part_annee, part_total)

write.xlsx(nb3, "D:/bdd/nombres_par_annee_site.xlsx")

# pivoter l'annee
# Pivoter l'annee pour n'analyse des séquences
nb4 <- nb3 %>% 
  select(domain, annee, n) %>%
  pivot_wider(names_from = annee, values_from = n, values_fill = 0) %>% 
  # Triez les colonnes en ordre croissant de l'année
  select(domain, order(names(.)[-1]) + 1)

nb5 <- nb4[,2:10]
row.names(nb5) <- nb4$domain

## select 
`%not_in%` <- purrr::negate(`%in%`)

med_top12 <- nb3 %>%
  subset(., domain %in% c("retractionwatch","forbetterscience","scienceintegritydigest",
                          "mythsofvisionscience","wikipedia","sanchak","dovepress",
                          "raphazlab","publicationethics","content.iospress.com",
                          "scholarlyoa", "nytimes"))

med_autre <- nb3 %>%
  subset(., domain %not_in% c("retractionwatch","forbetterscience","scienceintegritydigest",
                          "mythsofvisionscience","wikipedia","sanchak","dovepress",
                          "raphazlab","publicationethics","content.iospress.com",
                          "scholarlyoa", "nytimes"))

# Pivoter l'annee pour n'analyse des séquences
nb_autre <- med_autre %>% 
  select(domain, annee, n) %>%
  pivot_wider(names_from = annee, values_from = n, values_fill = 0)

autr_pr_acp <- nb_autre[,2:10]
row.names(autr_pr_acp) <- nb_autre$domain



library(ggplot2)

ggplot(med_top12) +
  aes(x = annee, y = n) +
  geom_col(fill = "#3256C4") +
  labs(
    x = "Années des commentaires",
    y = "Nombre d'apparitions",
    title = "Nombre d'apparitions dans les commentaires Pubpeer,
    par année"
  ) +
  theme_minimal() +
  facet_wrap(vars(domain), scales = "free")

ggplot(med_top12) +
 aes(x = annee, y = n) +
 geom_col(fill = "#710C89") +
 labs(x = "Années des commentaires", 
 y = "Nombre d'apparitions", title = "Nombre d'apparition des sites dans les commentaires Pubpeer, par année ", 
 subtitle = "(même échelle pour tous les sites)", caption = "AM - Données Pubpeer") +
 theme_light() +
 facet_wrap(vars(domain))



## ACP
library(ade4)
library(FactoMineR)
acp <- PCA(autr_pr_acp)
acp <- dudi.pca(autr_pr_acp, scannf = F, nf = Inf)
explor::explor(acp)

# calcul de la matrice des distances de Gower
library(cluster)
md_gower <- daisy(autr_pr_acp, metric = "gower")

arbre_gower <- hclust(md_gower, method = "ward.D2")

# Une façon plus visuelle de représenter le dendogramme
library(dendextend)
color_branches(arbre_gower, k = 3) %>% ggplot(labels = FALSE)

library(factoextra)
fviz_dend(arbre_gower, k = 5, show_labels = FALSE, rect = TRUE)

