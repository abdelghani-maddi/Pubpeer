# Typologie des liens 

rm(list = ls()) #supprimer tous les objets 


library(tidyverse)
library(questionr)
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
library(gtsummary)
library(questionr)
library(openxlsx)


### Récupération des données ----

data_urls <- readxl::read_excel("D:/bdd pubpeer/data_urls.xlsx")
data_comm <- readxl::read_excel("D:/bdd pubpeer/data_comm2.xlsx")


### domains distincts

d_dis <- data_urls %>%
  select(domain) %>%
  unique()

write.xlsx(d_dis, "D:/bdd pubpeer/domains_distincts.xlsx")

t <- read_tsv("E:/g_inventor_disambiguated.tsv/g_inventor_disambiguated.tsv")
head_t <- head(t)

install.packages("patentsview")
library(patentsview)

search_pv(query = '{"_gte":{"patent_date":"2007-01-01"}}')


asgn_flds <- c("assignee_id", "assignee_organization")
subent_flds <- get_fields("assignees", c("applications", "gov_interests"))
fields <- c(asgn_flds, subent_flds)

res <- search_pv(
  query = qry_funs$contains(inventor_last_name = "smith"), 
  endpoint = "assignees", 
  fields = fields
)

resu <- res$data$assignees %>% 
  unnest(applications) %>%
  head()

### Tentative de classification

# Charger les bibliothèques nécessaires
library(dplyr)
library(openxlsx)

# Charger le fichier existant

df <- d_dis

# Liste de mots-clés pour les différentes catégories
editor_keywords <- c("plos", "sciencedirect", "springer", "wiley", "bmc", "elsevier", "oxford", "springerlink", "cambridge")
pppr_keywords <- c("retractionwatch", "scienceintegritydigest", "post-publication", "integrity", "peer-review")
db_keywords <- c("pubmed", "scholar.google.com", "scopus", "doi.org", "pubs.rsc.org", "biofibre.com")
blog_keywords <- c("wordpress", "blog", "tumblr", "medium", "wordpress.com")
specialized_media_keywords <- c("scienceintegritydigest", "retractionwatch", "scientificamerican", "nature.com", "scientificnews")
discussion_platforms_keywords <- c("pubpeer", "researchgate", "arxiv.org", "researcher-app.com", "quora.com")

# Fonction pour classifier les domaines
classify_sites <- function(domain) {
  domain_lower <- tolower(domain)
  # Vérification pour les éditeurs scientifiques
  if (any(sapply(editor_keywords, grepl, domain_lower))) {
    return("Éditeur Scientifique")
  }
  # Vérification pour PPPR et intégrité scientifique
  if (any(sapply(pppr_keywords, grepl, domain_lower))) {
    return("PPPR / Intégrité Scientifique")
  }
  # Vérification pour base de données scientifiques
  if (any(sapply(db_keywords, grepl, domain_lower))) {
    return("Base de Données Scientifique")
  }
  # Vérification pour les blogs
  if (any(sapply(blog_keywords, grepl, domain_lower))) {
    return("Blog")
  }
  # Vérification pour les médias spécialisés
  if (any(sapply(specialized_media_keywords, grepl, domain_lower))) {
    return("Média Spécialisé")
  }
  # Vérification pour les plateformes de discussion scientifique
  if (any(sapply(discussion_platforms_keywords, grepl, domain_lower))) {
    return("Plateforme de Discussion Scientifique")
  }
  # Vérification pour redirection DOI
  if (grepl("doi.org", domain_lower)) {
    return("Redirection DOI")
  }
  return("Autre")
}

# Appliquer la classification
df$Classification <- sapply(df$domain, classify_sites)

# Exporter le fichier mis à jour
output_path <- "classified_domains_updated.xlsx"
write.xlsx(df, output_path)

# Message pour indiquer que le fichier a été exporté
cat("Fichier mis à jour exporté avec succès :", output_path)
