rm(list = ls()) #supprimer tous les objets 
# https://store.genderize.io/usage
# https://github.com/kalimu/genderizeR/issues/7
# https://genderize.io/
# https://journal.r-project.org/archive/2016/RJ-2016-002/index.html 
# https://kalimu.github.io/#contact

library(tidyverse)
library(questionr)
library(RPostgres)
library(gtsummary)
library(openxlsx2)
library(gender)
library(GenderInfer)
# devtools::install_github("kalimu/genderizeR")
#library(genderizeR)
library(openxlsx)

# data

df <- tb_finale_gender %>%
  select(publication, prenoms, g_prob_06, proba) %>%
  left_join(., bdd_pub, by = 'publication') %>%
  select(DOI,publication, prenoms, g_prob_06, proba) %>%
  subset(., publication < 340 & proba < 0.7 & g_prob_06 %in% c("male", "female"))

write.xlsx(df, "D:/APC Jaime Texiera/echantillon gender.xlsx")


df <- tb_finale_gender %>%
  select(publication, prenoms, g_prob_06, proba) %>%
  left_join(., bdd_pub, by = 'publication') %>%
  select(DOI,publication, prenoms, g_prob_06, proba) %>%
  subset(., proba == 0.5 & g_prob_06 %in% c("male", "female")) %>%
  select(publication) %>%
  unique()


