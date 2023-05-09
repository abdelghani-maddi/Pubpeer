rm(list = ls()) #supprimer tous les objets 

# https://github.com/ropensci/openalexR 

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
library(ggplot2)
library(ggtext)

# install.packages("remotes")
# remotes::install_github("ropensci/openalexR")
library(openalexR)
options(openalexR.mailto = "abdelghani.maddi@cnrs.fr")



# Chargement des données
bdd_pub <- readxl::read_excel("D:/bdd/data_pub.xlsx")



# 
works_from_dois <- oa_fetch(
  entity = "works",
  doi = data_pub$DOI[1:10],
  verbose = TRUE
)
#> Requesting url: https://api.openalex.org/works?filter=doi%3A10.1016%2Fj.joi.2017.08.007%7Chttps%3A%2F%2Fdoi.org%2F10.1007%2Fs11192-013-1221-3
#> Getting 1 page of results with a total of 2 records...
#> 
#> 
#> 
library(gghighlight)
concept_df <- oa_fetch(
  entity = "concepts",
  level = 1,
  ancestors.id = "https://openalex.org/C86803240", # Biology
  works_count = ">1000000"
)

concept_df |>
  select(display_name, counts_by_year) |>
  tidyr::unnest(counts_by_year) |>
  filter(year < 2022) |>
  ggplot() +
  aes(x = year, y = works_count, color = display_name) +
  facet_wrap(~display_name) +
  geom_line(linewidth = 0.7) +
  scale_color_brewer(palette = "Dark2") +
  labs(
    x = NULL, y = "Works count",
    title = "Virology spiked in 2020."
  ) +
  guides(color = "none") +
  gghighlight(
    max(works_count) > 244000,
    label_params = list(nudge_y = 10^5, segment.color = NA)
  )
#> label_key: display_name
