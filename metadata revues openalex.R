library(tidyverse)
library(openalexR)

source_id <- paper_metadat %>%
  filter(!(journal_id=="NONE")) %>%
  select(journal_id) %>%
  unique()


jnal_metadata1 <- oa_fetch(
  entity = "sources",
  id = source_id[1:10000,],
  verbose = TRUE
)

jnal_metadata2 <- oa_fetch(
  entity = "sources",
  id = source_id[10001:20000,],
  verbose = TRUE
)

jnal_metadata3 <- oa_fetch(
  entity = "sources",
  id = source_id[20001:30000,],
  verbose = TRUE
)

jnal_metadata4 <- oa_fetch(
  entity = "sources",
  id = source_id[30001:40000,],
  verbose = TRUE
)

jnal_metadata5 <- oa_fetch(
  entity = "sources",
  id = source_id[40001:50000,],
  verbose = TRUE
)

jnal_metadata6 <- oa_fetch(
  entity = "sources",
  id = source_id[50001:60000,],
  verbose = TRUE
)

jnal_metadata7 <- oa_fetch(
  entity = "sources",
  id = source_id[60001:70000,],
  verbose = TRUE
)

jnal_metadata8 <- oa_fetch(
  entity = "sources",
  id = source_id[70001:80000,],
  verbose = TRUE
)

jnal_metadata9 <- oa_fetch(
  entity = "sources",
  id = source_id[80001:90000,],
  verbose = TRUE
)

jnal_metadata10 <- oa_fetch(
  entity = "sources",
  id = source_id[90001:100000,],
  verbose = TRUE
)

jnal_metadata11 <- oa_fetch(
  entity = "sources",
  id = source_id[100001:111100,],
  verbose = TRUE
)

openalex_jnal_metadata <- rbind(jnal_metadata1, jnal_metadata2, jnal_metadata3,
                                jnal_metadata4, jnal_metadata5, jnal_metadata6,
                                jnal_metadata7, jnal_metadata8, jnal_metadata9,
                                jnal_metadata10, jnal_metadata11)
#library(openxlsx)
#write.xlsx(openalex_jnal_metadata, "D:/Pubpeer Gender/openalex_jnal_metadata.xlsx")
saveRDS(openalex_jnal_metadata, "D:/Pubpeer Gender/openalex_jnal_metadata.rds")

###
# récupérer les métadonnées retractionwatch

source_id_rtw <- oa_rtw %>%
  filter(!is.na(so_id)) %>%
  select(so_id) %>%
  unique() %>%
  as.data.frame()

# Corriger l'ordre des arguments dans gsub
source_id_rtw$so_id <- gsub("https://openalex.org/", "", source_id_rtw$so_id)


jnal_metadata_rtw <- oa_fetch(
  entity = "sources",
  id = source_id_rtw[1:5447,],
  verbose = TRUE
)

saveRDS(jnal_metadata_rtw, "D:/Pubpeer Gender/rtw_jnal_metadata.rds")


jnal_metadata_rtw_oa <- rbind(jnal_metadata_rtw, openalex_jnal_metadata)
saveRDS(jnal_metadata_rtw_oa, "D:/Pubpeer Gender/jnal_metadata_rtw_oa.rds")


# Convertir la liste en dataframe avec chaque élément structuré en lignes et colonnes
summary_stats_df <- do.call(rbind, lapply(jnal_metadata_rtw_oa$summary_stats, function(x) t(as.data.frame(x))))

# Renommer les colonnes
colnames(summary_stats_df) <- c("2yr_mean_citedness", "h_index", "i10_index")

summary_stats_df <- summary_stats_df %>%
  as.data.frame()

stats_jnals <- cbind(jnal_metadata_rtw_oa$id, summary_stats_df)
colnames(stats_jnals) <- c("so_id", "two_yr_mean_citedness", "h_index", "i10_index")

saveRDS(stats_jnals, "D:/Pubpeer Gender/stats_jnals.rds")


########################################################""

#############################################################
# rtw
id_paper_stat_jnal_rtw <- oa_rtw %>%
  left_join(., stats_jnals, by = "so_id") %>%
  select(id, so_id, two_yr_mean_citedness, h_index, i10_index) %>%
  unique()

#oa
stats_jnals$journal_id <- gsub("https://openalex.org/", "", stats_jnals$so_id)

id_paper_stat_jnal_oa <- paper_metadat %>%
  filter(!(journal_id=="NONE")) %>%
  left_join(., stats_jnals, by = "journal_id") %>%
  select(id, journal_id, two_yr_mean_citedness, h_index, i10_index) %>%
  unique()

id_paper_stat_jnal_oa$id <- paste0("https://openalex.org/",id_paper_stat_jnal_oa$id)

id_paper_stat_jnal_oa <- rename(id_paper_stat_jnal_oa, so_id = journal_id)


id_paper_stat_jnal <- rbind(id_paper_stat_jnal_rtw, id_paper_stat_jnal_oa)
saveRDS(id_paper_stat_jnal, "D:/Pubpeer Gender/id_paper_stat_jnal.rds")
