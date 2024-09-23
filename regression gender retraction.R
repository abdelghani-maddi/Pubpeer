bdd_disc_reg2 <- readRDS("D:/Pubpeer Gender/Révisions QSS/bdd_disc_reg2.rds")
bdd_disc_reg <- readRDS("D:/Pubpeer Gender/Révisions QSS/bdd_disc_reg.rds")

library(labelled)
library(tidyverse)

var_label(bdd_disc_reg$Gtype2) <- "Gender type"
var_label(bdd_disc_reg$is_oa) <- "Is Open Access"
var_label(bdd_disc_reg$cited_by_count) <- "Log(1 + Citation count)"
var_label(bdd_disc_reg$grants) <- "Grants"

look_for(bdd_disc_reg)

## Recodage de bdd_disc_reg$Gtype2 en bdd_disc_reg$Gtype2_rec
bdd_disc_reg$Gtype2_rec <- bdd_disc_reg$Gtype2 %>%
  fct_recode(
    "Men-Women | M first" = "Collab. men-women m first",
    "Men-Women | W first" = "Collab. men-women w first",
    "Men only" = "Collab. men only",
    "Women only" = "Collab. women only",
    NULL = "First author not identified"
  )

## Réordonnancement de bdd_disc_reg$Gtype2_rec pour la ref dans la regression
bdd_disc_reg$Gtype2_rec <- bdd_disc_reg$Gtype2_rec %>%
  fct_relevel(
    "Man alone", "Men-Women | M first", "Men-Women | W first",
    "Women only", "Men only", "Woman alone"
  )


# retirer les notices des données : ce ne sont pas des articles mais juste des notices

a_retirer <- oa_rtw %>%
  filter(str_detect(tolower(title), "retraction not")) %>%
  select(id) %>%
  distinct()

bdd_disc_reg_ss_notices <- bdd_disc_reg %>%
  filter(!(.$id %in% a_retirer$id))



bdd_disc_reg_ss_notices$grants <- as.factor(bdd_disc_reg_ss_notices$grants)

###
bdd_disc_reg_ss_notices[, 9:27] <- lapply(bdd_disc_reg_ss_notices[, 9:27], function(x) ifelse(x != 0, 1, 0))
###


###
bdd_disc_reg_disc <- bdd_disc_reg2 %>%
  select(paper, `Health Sciences`, `Life Sciences`, `Social Sciences`, `Physical Sciences`) %>%
  unique()
###

bdd_disc_reg_ss_notices2 <- bdd_disc_reg_ss_notices %>%
  left_join(., bdd_disc_reg_disc, by = c("id" = "paper")) %>%
  filter(!is.na(`Life Sciences`), !is.na(Gtype2_rec))

bdd_disc_reg_ss_notices2[, 30:33] <- lapply(bdd_disc_reg_ss_notices2[, 30:33], function(x) ifelse(x != 0, 1, 0))

###
## Réordonnancement de bdd_disc_reg$Gtype2_rec pour la ref dans la regression
bdd_disc_reg_ss_notices2$Gtype2_rec <- bdd_disc_reg_ss_notices2$Gtype2_rec %>%
  fct_relevel(
    "Man alone", "Men-Women | M first", "Women only" , "Men-Women | W first",
    "Men only", "Woman alone" 
  )

###
var_label(bdd_disc_reg_ss_notices2$Gtype2_rec) <- "Gender type"
var_label(bdd_disc_reg_ss_notices2$is_oa) <- "Is Open Access"
var_label(bdd_disc_reg_ss_notices2$cited_by_count) <- "Log(1 + Citation count)"
var_label(bdd_disc_reg_ss_notices2$grants) <- "Grants"
var_label(bdd_disc_reg_ss_notices2$nb_aut) <- "Log(#Authors)"
var_label(bdd_disc_reg_ss_notices2$publication_year) <- "Publication year"


# Transform nb_aut into a categorical variable
bdd_disc_reg_ss_notices2$nb_aut_cat <- cut(bdd_disc_reg_ss_notices2$nb_aut,
                                           breaks = c(0, 5, 10, 20, 200),  # Intervals
                                           labels = c("Small team (1-5)", "Medium team (6-10)", 
                                                      "Medium-Large team (11-20)", "Large team (>20)"),
                                           right = TRUE)  # Includes upper bound
###

bdd_disc_reg_ss_notices2$is_large_team <- ifelse(bdd_disc_reg_ss_notices2$nb_aut >10, 1,0)

###
res <- glm(is_retracted ~ Gtype2_rec + is_oa + log(1+cited_by_count) + log(nb_aut) +# nb_aut_cat +
             # is_large_team +
             publication_year + `Health Sciences` + `Life Sciences` + `Physical Sciences`,
           data = bdd_disc_reg_ss_notices2,
           family =  binomial(logit))
# grants +
#Biology + Medicine + Engineering + Mathematics + Psychology,

summary(res)

library(car)
vif(res)
# library(GGally)
# ggcoef_model(res, exponentiate = TRUE)


library(forestmodel)
forest_model(res)


