# Charger le package "tm"
library(tidyverse)
library(tm)
library(topicmodels)

# Créer un vecteur de textes
documents <- data_max_sequence %>%
  select(publication, comm) %>%
  unique()

# Créer un corpus à partir du vecteur de textes
corpus <- Corpus(VectorSource(documents$comm))

# Nettoyer le corpus
corpus_clean <- corpus %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(stripWhitespace) %>%
  tm_map(tolower) %>%
  tm_map(removeWords, stopwords("en"))

# Créer une matrice terme-document à partir du corpus nettoyé
dtm <- DocumentTermMatrix(corpus_clean)

# Définir le nombre de topics à extraire
num_topics <- 10

# Effectuer le Topic Modeling avec le modèle LDA
lda_model <- LDA(dtm, k = num_topics)

# Afficher les termes les plus représentatifs pour chaque topic
terms <- terms(lda_model, 5)
terms
