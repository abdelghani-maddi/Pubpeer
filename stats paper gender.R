#####################################################################
###              Analyse de données pour le papier gender         ###
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

### Connexion à postgresql ----

con<-dbConnect(RPostgres::Postgres())

db <- 'SKEPTISCIENCE'  #provide the name of your db
host_db <- 'localhost' # server
db_port <- '5433'  # port DBA
db_user <- 'postgres' # nom utilisateur  
db_password <- '****************'
con <- dbConnect(RPostgres::Postgres(), dbname = db, host=host_db, port=db_port, user=db_user, password=db_password) 
# Test connexion
dbListTables(con)

### Lecture des données ----
df_gender <- read_excel("~/Documents/Pubpeer Gender/tb_finale_gender.xlsx") ## bdd sur le genre

df_retract <- read_excel("/Users/maddi/Documents/Pubpeer Gender/df_gender_retract.xlsx") ## bdd sur le genre + bdd retractations (version avril 2023)
#write.xlsx(df_retract, "/Users/maddi/Documents/Pubpeer Gender/df_retract.xlsx")
reason_agr <- read_excel("~/Documents/Pubpeer Gender/reasons_retract aggreg.xlsx")

bdd_pub = read.csv2('/Users/maddi/Documents/Pubpeer project/Donnees/Bases PubPeer/PubPeer_Base publications.csv', sep=";")


# Table 1: Number and share of gender types, according to the set probability threshold
   # NB : la différence entre df_gender et df_retract est que df_retract ne contient pas les lignes pour lesquelles legender <0.6
df_gender %>%
  select(publication, order_auteur, starts_with("g_prob")) %>%
  unique() %>%
  tbl_summary(
    include = c("g_prob_06", "g_prob_07", "g_prob_08", "g_prob_09", "g_prob_100")
  )


### Stats par genre et discipline
df <- df_gender %>%
  select(publication, g_prob_06, frac_disc, disc, female_part) %>%
  subset(., !is.na(.$disc) & g_prob_06 %in% c('female','male')) 

df <- df_retract %>%
  select(publication, order_auteur, g_prob_06, disc)  %>%
  subset(., g_prob_06 %in% c('female','male')) %>%
  unique()


tbl_summary(df,
            include = c("g_prob_06")
            )  


df %>% 
  tbl_summary(
    include = c(g_prob_06, disc),
    by = disc,
    sort = list(everything() ~ "frequency")
) %>%
  
  add_overall(last = TRUE, col_label = "**Overall** (# {N})")  


n_distinct(df$publication)


df_counts <- df %>%
  group_by(disc) %>%
  summarise(moyenne_female_part = mean(female_part, na.rm = TRUE),
            somme_frac_disc = sum(frac_disc),
            nombre_lignes = n())



# Distribution of publications according to the type of men-women collaboration in the overall Pubpeer dataset and within the retracted publications
df <- df_retract %>% 
  subset(., !is.na(Gtype2)) %>% # & !is.na(disc)) %>%
  select(publication, Gtype2, is_retracted) %>%
  unique() %>%
  tbl_summary(
    include = c(Gtype2, is_retracted),
    by = is_retracted,
    sort = list(everything() ~ "frequency"),
    statistic = list(
      all_continuous() ~ c("{N_obs}") 
    )
  ) %>%
  add_overall()  %>%
  # adding spanning header
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Is retracted**") %>%
  add_p() %>%
  separate_p_footnotes()

### Part dans les rétractations / part dans le total (par type de collab) ----

# Calculate the total number of rows in the dataframe
total <- nrow(df)

# Create a table of counts for each "Gtype" value
table_all <- table(df$Gtype2)

# Create a table of counts for each "Gtype" value where "Retracted" is "True"
table_retracted <- table(df$Gtype2[df$is_retracted == 1])

# Calculate the relative proportion of each "Gtype" value in the entire dataframe
prop_all <- table_all / total

# Calculate the relative proportion of each "Gtype" value for "Retracted" = TRUE
prop_retracted <- table_retracted / sum(df$is_retracted == 1)

# Divide the relative proportions for "Retracted" = TRUE by those in the entire dataframe
relative_prop <- as.data.frame(prop_retracted / prop_all)

# Print the resulting table of relative proportions
t <- as.data.frame(relative_prop)


## Représentation graphique
ggplot(relative_prop, aes(x = reorder(Var1, Freq), y = Freq)) + 
  geom_bar(stat = "identity") +
  geom_col(fill = "#2C81C9") +
  labs(
    x = "Men-women collaboration type",
    y = "% in retracted / % in overall"
  ) +
  coord_flip() +
  theme_light()
