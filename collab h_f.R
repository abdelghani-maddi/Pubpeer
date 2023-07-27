#####################################################################
### Analyse des pratiques de collab h-f  ###
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

df_retract <- read_excel("/Users/maddi/Documents/Pubpeer Gender/df_gender_retract.xlsx") ## bdd sur le genre + bdd retractations (version avril 2023)

# ici je prends Gtype au lieu de Gtype2 car dans ce dernier, je n'ai pas pris en compte les publi 
# pour lesquelles le premier auteur n'est pas identifié (car c'est une catégorie dans les classes)
# pour cette analyse il n'y a pas besoin de cette restriction
copub <- df_retract %>%
  select(publication, nb_aut, Gtype, female_part, w_corresp, woman_leader, DOI.x) %>%
  subset(., nb_aut > 1) %>%
  unique()

copub$woman_leader <- factor(copub$woman_leader)
## Recodage de copub$woman_leader
copub$woman_leader <- copub$woman_leader %>%
  fct_recode(
    "No" = "0",
    "Yes" = "1"
  )

# distribution genre selon la taille des équipes

distrib <- copub %>%
  select(nb_aut, Gtype) %>%
  group_by(nb_aut, Gtype) %>%
  count()
# Exporter
write.xlsx(distrib, "D:/Pubpeer Gender/distrib_selon_taille_equip.xlsx")


distrib2 <- copub %>%
  select(nb_aut, woman_leader) %>%
  group_by(nb_aut, woman_leader) %>%
  count()
# Exporter
write.xlsx(distrib2, "D:/Pubpeer Gender/woman_leader_taille_equip.xlsx")


# Distribution de la structure de collaboration

# Create the new column 'flag_femme' with values based on the condition
df_retract$flag_femme <- ifelse(df_retract$female_part > 0, 1, 0)
df_retract$flag_homme <- ifelse(df_retract$female_part < 1, 1, 0)

# créer flag collab
df_retract$flag_collab <- ifelse(df_retract$nb_aut > 1, 1, 0)

df_struct <- df_retract %>%
  select(publication, flag_femme, flag_homme, flag_collab, woman_leader, w_corresp) %>%
  unique()

# Calculer le nombre de 1 pour chaque variable
counts_ones <- colSums(df_struct)

# Calculer le nombre de 0 pour chaque variable (en soustrayant le nombre de 1 de la taille du DataFrame)
counts_zeros <- nrow(df_struct) - counts_ones

# Créer un nouveau DataFrame contenant les comptes
counts_df <- data.frame(
  Variable = names(df_struct),
  Count_0 = counts_zeros,
  Count_1 = counts_ones
)


# Ajouter une colonne "Total" avec la somme de Count_0 et Count_1
counts_df <- transform(counts_df, Total = Count_0 + Count_1)

# Calculer les pourcentages en ligne
counts_df$Percent_0 <- (counts_df$Count_0 / counts_df$Total) * 100
counts_df$Percent_1 <- (counts_df$Count_1 / counts_df$Total) * 100

write.xlsx(counts_df, "D:/Pubpeer Gender/structure_collab.xlsx")


library(reshape2)


# Calculer les comptes en fonction de flag_femme et flag_homme
counts_flag_collab <- aggregate(flag_collab ~ flag_femme + flag_homme, data = df_struct, function(x) c(Count_0 = sum(x == 0), Count_1 = sum(x == 1)))

counts_flag_collab <- as.data.frame(counts_flag_collab)

write.xlsx(counts_flag_collab, "D:/Pubpeer Gender/counts_flag_collab.xlsx")

publication_bdd_pub <- bdd_pub %>%
  select(publication) %>%
  unique()

publication_bdd_retrac <- df_retract %>%
  select(publication) %>%
  unique()


diffir <- setdiff(publication_bdd_pub, publication_bdd_retrac)


publi_exclues <- bdd_pub %>%
  subset(., publication %in% diffir$publication)
