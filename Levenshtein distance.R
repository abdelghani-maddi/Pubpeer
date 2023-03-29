## Calcul de la distance Levenshtein pour regrouper les sites qui sont les mêmes -----

# Créer un vecteur textuel
v <- unique(as.character(data_urls$domain[data_urls$typo=="Médias"]))
v <- freqmed$site
# Harmoniser un peu pour éviter d'embouriller Levenshtein 
for (i in 1:length(v)) {
  v[i] <- gsub(".com", "", v[i])
  v[i] <- gsub(".org", "", v[i])
  v[i] <- gsub("www.", "", v[i])
  v[i] <- gsub("wordpress", "", v[i])
  v[i] <- gsub("blogspot", "", v[i])
  v[i] <- gsub("blog", "", v[i])
  v[i] <- gsub(".files", "", v[i])
  v[i] <- gsub(".edu", "", v[i])
  v[i] <- gsub("^\\.", "", v[i])
  v[i] <- gsub("[.]+$", "", v[i])
  v[i] <- gsub("\\.+\\b", "", v[i])
}

v <- na.omit(replace(v, v == "", NA))

# Définir une fonction pour calculer la distance de Levenshtein entre deux chaînes
levenshtein_distance <- function(s1, s2) {
  m <- nchar(s1)
  n <- nchar(s2)
  d <- matrix(0, n + 1, m + 1) 
  d[1, ] <- 0:m
  d[, 1] <- 0:n
  for (j in 2:(m + 1)) {
    for (i in 2:(n + 1)) {
      if (substr(s1, j - 1, j - 1) == substr(s2, i - 1, i - 1)) {
        d[i, j] <- d[i - 1, j - 1]
      } else {
        d[i, j] <- min(d[i - 1, j], d[i, j - 1], d[i - 1, j - 1]) + 1
      }
    }
  }
  d[n + 1, m + 1]
}

# Définir une fonction pour regrouper les éléments d'un vecteur selon leur ressemblance
group_similar_elements <- function(v, threshold) {
  groups <- list()
  for (i in seq_along(v)) {
    matched <- FALSE
    for (j in seq_along(groups)) {
      if (levenshtein_distance(v[i], groups[[j]][1]) <= threshold) {
        groups[[j]] <- c(groups[[j]], v[i])
        matched <- TRUE
        break
      }
    }
    if (!matched) {
      groups[[length(groups) + 1]] <- c(v[i])
    }
  }
  groups
}

# Utiliser la fonction pour regrouper les éléments de v selon leur ressemblance
groups <- group_similar_elements(v, threshold = 2)

# Afficher les groupes 
groups

# Créer un vecteur d'identifiants pour chaque groupe
ids <- seq_along(groups)

# Créer un dataframe à partir de la liste groups et des identifiants
df_levensh <- data.frame(id = rep(ids, lengths(groups)),
                 element = unlist(groups))

# Afficher le dataframe 
df_levensh

write_xlsx(df_levensh, "/Users/maddi/Documents/Pubpeer project/Pubpeer explo/grp_levenshtein.xlsx")
write.xlsx(df_levensh, "D:/bdd/grp_levenshtein.xlsx")
write.xlsx(freqmed, "D:/bdd/freqmed.xlsx")


# Calculer les fréquences pour avoir une idée de la distribution des sites
f <- factor(df_levensh$id) |>
  fct_infreq() |> 
  questionr::freq()


f <- factor(df$id[df$id!=27 & df$id!=2]) |>
  fct_infreq() |> 
  questionr::freq()
