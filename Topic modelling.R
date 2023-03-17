# Charger le package "tm"
library(tidyverse)
library(tm)
library(topicmodels)
library(LDAvis)

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
terms <- terms(lda_model, 200)
terms


# Visualisation ----

# Charger les données et créer la matrice document-terme
data("AssociatedPress", package = "topicmodels")
dtm <- AssociatedPress
dtm <- dtm[ , col_sums(dtm) > 0]  # Supprimer les mots vides

# Créer le modèle LDA
num_topics <- 10
lda_model <- LDA(dtm, k = num_topics)

# Obtenir les termes les plus fréquents pour chaque topic
terms_per_topic <- terms(lda_model, 20)

# Visualiser les termes pour chaque topic
terms_per_topic %>%
  as.data.frame() %>%
  rownames_to_column(var = "Topic") %>%
  pivot_longer(cols = -Topic, names_to = "Rank", values_to = "Term") %>%
  ggplot(aes(x = Topic, y = Rank, fill = Term, label = Term)) +
  geom_tile(color = "white") +
  scale_fill_viridis_d(option = "magma", direction = -1) +
  geom_text(size = 3) +
  xlab("Topic") +
  ylab("Terme") +
  ggtitle(paste("Termes les plus fréquents pour chaque topic (", num_topics, "topics)"))

# interactive

library(topicmodels)
library(LDAvis)


# Charger les données et créer la matrice document-terme
data("AssociatedPress", package = "topicmodels")
dtm <- AssociatedPress
dtm <- dtm[, colSums(dtm) > 0]  # Supprimer les mots vides

# Créer le modèle LDA
num_topics <- 5
lda_model <- LDA(dtm, k = num_topics)

# Créer la visualisation interactive
LDAvis_prepared <- createJSON(phi = t(topics(lda_model)),
                              theta = lda_model@gamma,
                              doc.length = colSums(dtm),
                              vocab = colnames(dtm),
                              term.frequency = colSums(dtm))
serVis(LDAvis_prepared)


termes <- dtm$dimnames$Terms


