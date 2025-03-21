# The package ggtext needs to be installed to run this chunk
# library(ggtext)
library(openalexR)
library(tidyverse)
library(questionr)

library(openxlsx2)
library(readxl)
library(openxlsx)



jours_all <- oa_fetch(
  entity = "sources",
  works_count = ">200",
  verbose = TRUE
)


jours_all_inf200 <- oa_fetch(
  entity = "sources",
  works_count = "<200",
  verbose = TRUE
) 

# save.image("mon_espace_de_travail.RData")


# Union des deux DataFrames jours_all et jours_all_inf200
openalex_venues <- rbind(jours_all, jours_all_inf200)
saveRDS(openalex_venues, "D:/Sherpa/sherpa/openalex_sources.rds")

# filtrer revues uniquement
journals_openalex <- openalex_venues %>%
  select(id, display_name, host_organization_name, issn,
         is_oa, is_in_doaj, works_count, cited_by_count, type) %>%
  filter(type == "journal")

# Utilisez la fonction unnest pour éclater la colonne issn
journals_openalex <- unnest(journals_openalex, issn) 
write.xlsx(journals_openalex, file = "D:/bdd pubpeer/journals_openalex.xlsx")

journals_openalex <- journals_openalex %>%
  filter(!is.na(issn) & !is.na(host_organization_name))

data_jnal <- data_jnal %>%
  filter(issn != "None")
# matcher avec openalex
data_jnal2 <- left_join(data_jnal, data_openalex, by = "issn")

match <- data_jnal2 %>%
  select(publication, display_name, host_organization_name) %>%
  unique()

count_editeurs_pubpper <- match %>%
  group_by(host_organization_name) %>%
  count()

count_editeurs_tot <- data_openalex %>%
  select(host_organization_name, works_count) %>%
  group_by(host_organization_name) %>%
  summarise(nb = sum(works_count))


## analyse : voir https://docs.ropensci.org/openalexR/
jours <- jours_all |>
  filter(!is.na(x_concepts), type != "ebook platform") |>
  slice_max(cited_by_count, n = 9) |>
  distinct(display_name, .keep_all = TRUE) |>
  select(jour = display_name, x_concepts) |>
  tidyr::unnest(x_concepts) |>
  filter(level == 0) |>
  left_join(concept_abbrev, by = join_by(id, display_name)) |>
  mutate(
    abbreviation = gsub(" ", "<br>", abbreviation),
    jour = gsub("Journal of|Journal of the", "J.", gsub("\\(.*?\\)", "", jour))
  ) |>
  tidyr::complete(jour, abbreviation, fill = list(score = 0)) |>
  group_by(jour) |>
  mutate(
    color = if_else(score > 10, "#1A1A1A", "#D9D9D9"), # CCCCCC
    label = paste0("<span style='color:", color, "'>", abbreviation, "</span>")
  ) |>
  ungroup()

jours |>
  ggplot() +
  aes(fill = jour, y = score, x = abbreviation, group = jour) +
  facet_wrap(~jour) +
  geom_hline(yintercept = c(45, 90), colour = "grey90", linewidth = 0.2) +
  geom_segment(
    aes(x = abbreviation, xend = abbreviation, y = 0, yend = 100),
    color = "grey95"
  ) +
  geom_col(color = "grey20") +
  coord_polar(clip = "off") +
  theme_bw() +
  theme(
    plot.background = element_rect(fill = "transparent", colour = NA),
    panel.background = element_rect(fill = "transparent", colour = NA),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.text = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  ggtext::geom_richtext(
    aes(y = 120, label = label),
    fill = NA, label.color = NA, size = 3
  ) +
  scale_fill_brewer(palette = "Set1", guide = "none") +
  labs(y = NULL, x = NULL, title = "Journal clocks")