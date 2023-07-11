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


tb_finale_gender <- read_excel("~/Documents/Pubpeer Gender/tb_finale_gender.xlsx") ## bdd sur le genre
bdd_pub = read.csv2('/Users/maddi/Documents/Pubpeer project/Donnees/Bases PubPeer/PubPeer_Base publications.csv', sep=";")


df <- tb_finale_gender %>%
  select(publication, prenoms, g_prob_06, proba) %>%
  left_join(., bdd_pub, by = 'publication') %>%
  select(DOI,publication, prenoms, g_prob_06, proba) %>%
  subset(., g_prob_06 %in% c("male", "female") & DOI != "None") %>%
  sample_n(100)

write.xlsx(df, "~/Documents/Pubpeer Gender/echantillon gender.xlsx")





