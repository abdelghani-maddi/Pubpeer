# ------------------------------------------------------------------------------
#  Packages
#  Les packages suivant doivent etre installés
# ------------------------------------------------------------------------------
# library(devtools)
# install_github("mroberts/stmBrowser",dependencies=TRUE)
## Formatage espace de travail ----
rm(list = ls()) #supprimer tous les objets 

# install.packages(
#   c("stm", "tm", "stmBrowser", 'wordcloud',
#     "stringr",'igraph',  "RColorBrewer",
#     'Rtsne','SnowballC','GetoptLong'), dependencies = TRUE
# )

library(topicmodels)
library('stringr')
library('stm')
library('stmBrowser')
library('wordcloud')
library('igraph')
library('geometry')
library('Rtsne')
library('SnowballC')
library('GetoptLong')
library(rsvd)
# if(!require(devtools)) install.packages("devtools")
# library(usethis)
# library(devtools)
# install_github("mroberts/stmBrowser",dependencies=TRUE)

# Indique l'heure avant chaque print
qq.options("cat_prefix" = function(x) format(Sys.time(), "\n[%H:%M:%S] "))

# ------------------------------------------------------------------------------
#  Initialization: adapter les path des repertoires suivant votre environnement
# ------------------------------------------------------------------------------

setwd("D:/bdd pubpeer/Analyse commentaires")
data_path <- '~/upem-topic-modeling/data/'

input_file    <- qq("@{data_path}techno.csv")

# ------------------------------------------------------------------------------
#  Chargement des données dans une dataframe
# ------------------------------------------------------------------------------
qqcat("Load data from @{input_file}")

# data ----

com2 <- read_excel("D:/Pubpeer/com2.xlsx")
com2 <- read_excel("D:/bdd pubpeer/com2.xlsx")

dico_misconduct <- read_excel("D:/bdd pubpeer/dico_misconduct.xlsx")
stopwords <- read_table("D:/bdd pubpeer/stopwords-en.txt", col_names = FALSE)

stopwords <- c(stopwords, "paper", "article", "doi", "link", "study",
               "author", "pubpeer","comment", "los", "las", "del", "una",
               "texto", "como", "por", "len", "textos", "para",
               "httpsdoiorg", "india", "nov", "wiley", "cite",
               "february", "can", "example", "find", "work", "say",
               "scientist", "day") %>%
  unlist() %>%
  as.data.frame() 

# Renommer la colonne avec `names()`
names(stopwords) <- "stopwords"

# Préparer le corpus
com3 <- com2 %>%
  select(publication, commentateur, annee, cleaned_comm) %>%
  unique() %>%
  filter(!(commentateur == "3845") & !is.na(.$cleaned_comm)) %>%
  unique() %>%
  as.data.frame()

# Concaténer les commentaires par publication
com3 <- com3 %>%
  group_by(publication) %>%
  summarise(cleaned_comm = paste(cleaned_comm, collapse = " "),
            annee = round(mean(annee), digits = 0)) %>%
  ungroup()
# ------------------------------------------------------------------------------
# PRE-PROCESSING des textes
#   lowercase        : tout en minsucule
#   removestopwords  : filtrer les stopwords
#   removenumbers    : retirer les chiffres,
#   removepunctuation : enlever les signes de ponctuation
#   wordLengths      : ne garder que les mots de 3 characteres ou plus
#   striphtml        : enlever les tags HTML,
#   stem             : ne garder que la racine des mots (non utilisé)
#   metadata         : la sources des meta donnees: (journal et catégorie pour ce corpus)
# ------------------------------------------------------------------------------

qqcat("pre processing\n")

com4 <- com3

################################################
################################################
library(textstem) # Pour la lemmatisation

# Nettoyer et limatiser le texte
clean_text <- com4 %>%
  mutate(clean_comm = tolower(cleaned_comm),                                     # Convertir en minuscules
         clean_comm = str_replace_all(clean_comm, "[[:punct:]]", " "),   # Supprimer la ponctuation
         clean_comm = str_remove_all(clean_comm, "<[^>]+>"),             # Supprimer les balises HTML
         clean_comm = str_replace_all(clean_comm, "[^a-z\\s]", ""),      # Supprimer les caractères spéciaux et chiffres
         clean_comm = str_replace_all(clean_comm, "\\d+", ""),           # Supprimer les chiffres
         clean_comm = str_replace_all(clean_comm, "\\b\\w{1,2}\\b", ""), # Supprimer les mots très courts (<3 caractères)
         clean_comm = lemmatize_strings(clean_comm),                     # Limatiser le texte
         clean_comm = removeWords(clean_comm, stopwords$stopwords),      # Supprimer les stopwords
         clean_comm = str_squish(clean_comm)                             # Supprimer les espaces superflus
  )

# # Nettoyer et limatiser le texte
# clean_text <- com4 %>%
#   mutate(clean_comm = tolower(cleaned_comm),                             # Convertir en minuscules
#          clean_comm = str_replace_all(clean_comm, "[[:punct:]]", " "),   # Supprimer la ponctuation
#          clean_comm = str_replace_all(clean_comm, "\\d+", ""),           # Supprimer les chiffres
#          clean_comm = str_replace_all(clean_comm, "\\b\\w{1,2}\\b", ""), # Supprimer les mots très courts (<3 caractères)
#          clean_comm = lemmatize_strings(clean_comm), # Limatiser le texte
#          clean_comm = removeWords(clean_comm, stopwords$stopwords),  # Supprimer les stopwords
#          clean_comm = str_squish(clean_comm)                            # Supprimer les espaces superflus
#   )

# write_xlsx(clean_text, "D:/bdd pubpeer/Analyse commentaires/clean_text.xlsx")

################################################
################################################

processed <- textProcessor(clean_text$clean_comm,
                           #lowercase        = TRUE,
                           #removestopwords  = TRUE,
                           #removenumbers    = TRUE,
                           #removepunctuation = TRUE,
                           #wordLengths      = c(3,Inf),
                           striphtml        = TRUE,
                           stem             = FALSE,
                           metadata         = com4)

# ------------------------------------------------------------------------------
# Quelques parametres pour filtrer les mots.
# Ne seront pris en compte que
#   * les mots qui apparaissent plus souvent que thresh.lower dans les documents
#   * les mots qui apparaissent moins souvent que thresh.upper dans les documents (non utilisé)
# ------------------------------------------------------------------------------

thresh.lower  <- 30
# thresh.upper  <- 300 # non utilisé

out   <- prepDocuments(processed$documents,
                       processed$vocab,
                       processed$meta,
                       lower.thresh = thresh.lower)
# 
# Removing 210527 of 218438 terms (482881 of 2789441 tokens) due to frequency 
# Removing 456 Documents with No Words 
# Your corpus now has 50000 documents, 7911 terms and 2306560 tokens.

# ------------------------------------------------------------------------------
# out$documents est la matrice documents-mots
# out$vocab contient le vocabulaire, correspondance entre chaque mot et son ID
# ------------------------------------------------------------------------------


# Transformer les variables externes en Factor
meta          <- out$meta
meta$annee  <- as.factor(meta$annee)
# meta$rubrique <- as.factor(meta$rubrique)

# ------------------------------------------------------------------------------
#  Recherche du nombre optimal de topics avec searchK
#  Grid search en faisant varier le nombre de topics
#    * max.em.its permet de limiter le nombre d'itérations
#    * emtol fixe une limite inférieure en dessous de laquelle le modele est estimé avoir convergé
# ------------------------------------------------------------------------------

# sequence du nombre de topics
n_topics = seq(from = 5, to = 10, by = 2)

# grid search
gridsearch <- searchK(out$documents, out$vocab,
                      K = n_topics,
                      #prevalence  =~ journal+ rubrique ,
                      reportevery = 10,
                      # max.em.its = maxemits,
                      emtol       = 1.5e-4,
                      data = meta)

plot(gridsearch$results)

a <- print(gridsearch$results) %>%
  as.data.frame() 
write_xlsx(a, "D:/bdd pubpeer/Analyse commentaires/gridsearch.xlsx")


# Select the best number of topics that maximizes both exclusivity and semantic coherence
# Ces plots permettent de choisir le nombre de topics qui maximise a la fois
# la cohérence sémantique et l'exclusivité
# En général, l'exclusivité croit avec les nombre de topics (la distribution de dirichlet devient pointue)
# la cohérence sémantique correspond a des topics plus généraux (la distribution de dirichlet est plus aplatie)
plot(gridsearch$results$exclus, gridsearch$results$semcoh)
text(gridsearch$results$exclus, gridsearch$results$semcoh, labels=gridsearch$results$K, cex= 0.7, pos = 2)

plot(gridsearch$results$semcoh, gridsearch$results$exclus)
text(gridsearch$results$semcoh, gridsearch$results$exclus, labels=gridsearch$results$K, cex= 0.7, pos = 2)

# ------------------------------------------------------------------------------
# Appliquer STM
#  www.structuraltopicmodel.com
#  avec k=0, l'algo trouve de lui-meme un nombre de topics optimal, souvent assez grand
#   * prevalence: quelles sont les variables externes que l'on souhaite prendre en compte
#   * emtol: en dessous de ce niveau de changement entre 2 iterations, on arrete
#   * max.em.its: nombre maximal d'iterations
#  Si chaque itération prends trop de temps (plsu de quelques secondes)
#  essayer de réduire le nombre valeur prises par les variables externes
# ------------------------------------------------------------------------------
qqcat("fit stm\n")

fit <- stm(out$documents, out$vocab, 10,
           prevalence  =~ annee,
           data        = meta,
           reportevery = 10,
           # max.em.its  = 100,
           emtol       = 1.6e-4,
           init.type   = "Spectral",
           seed        = 1)

qqcat("stm done\n")

toLDAvis(mod=fit, docs=out$documents)

toLDAvis(mod=fit, docs=out$documents)
serVis(fit, out.dir = 'LDAvis v2', open.browser = FALSE)


library(stm)
library(LDAvis)

# Extraire les probabilités des termes par sujet
sage_labels <- stm::sageLabels(fit)
phi <- do.call(rbind, lapply(sage_labels$probs, as.matrix))

# Extraire la distribution des sujets par document (theta)
theta <- fit$theta

# Obtenir la longueur des documents, le vocabulaire, et la fréquence des termes
doc.length <- rowSums(out$documents)
vocab <- colnames(out$documents)
term.frequency <- colSums(out$documents)




setwd("D:/bdd pubpeer/Analyse commentaires")
library(LDAvis)
library(servr)

phi <- exp(fit$beta$logbeta[[1]])  # Matrice terme-thème
theta <- fit$theta                 # Matrice thème-document
doc.length <- sapply(out$documents, function(x) sum(x[2, ]))  # Longueur de chaque document
vocab <- out$vocab                 # Vocabulaire utilisé
term.frequency <- colSums(phi)     # Fréquence totale de chaque terme dans les documents

# Créer le JSON nécessaire pour LDAvis
json_lda <- createJSON(phi = phi,
                       theta = theta,
                       doc.length = doc.length,
                       vocab = vocab,
                       term.frequency = term.frequency)

# Créer le répertoire de sortie si nécessaire
dir.create('LDAvis', showWarnings = FALSE)

# Écrire le fichier JSON
writeLines(json_lda, con = file.path('LDAvis', "lda.json"))

# Créer la visualisation LDAvis
serVis(json_lda, out.dir = 'LDAvis', open.browser = FALSE)

library(servr)

# Servir le dossier LDAvis
httd('LDAvis')

servr::httd("D:/bdd pubpeer/Analyse commentaires/LDAvis")



# Charger les bibliothèques nécessaires
library(LDAvis)
library(servr)

# Calculer les matrices nécessaires pour LDAvis
phi <- exp(fit$beta$logbeta[[1]])  # Matrice terme-thème
theta <- fit$theta                 # Matrice thème-document
doc.length <- sapply(out$documents, function(x) sum(x[2, ]))  # Longueur de chaque document
vocab <- out$vocab                 # Vocabulaire utilisé
term.frequency <- colSums(phi)     # Fréquence totale de chaque terme dans les documents

# Créer le JSON nécessaire pour LDAvis
json_lda <- createJSON(phi = phi,
                       theta = theta,
                       doc.length = doc.length,
                       vocab = vocab,
                       term.frequency = term.frequency)

# Spécifier le répertoire de sortie
output_dir <- "D:/Pubpeer/LDAvis"  # Remplacez par le chemin réel sur votre machine

# Créer le répertoire de sortie si nécessaire
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Sauvegarder le fichier JSON
writeLines(json_lda, con = file.path(output_dir, "lda.json"))

# Créer la visualisation LDAvis et sauvegarder le fichier HTML dans le répertoire spécifié

library(httpuv)
startServer("127.0.0.1", 8080, list(
  call = function(req) {
    list(
      status = 200,
      headers = list(
        "Content-Type" = "text/html"
      ),
      body = readLines("D:/Projets/Pubpeer/LDAvis/index.html")
    )
  }
))

serVis(json_lda, out.dir = output_dir, open.browser = T)


# ----------------------------------- ------------------------------------------
# TADAAAA!
# Les topics avec 10 mots (n=10) chacun
#  Frex, proba, ... sont différentes façon d'associer les mots dans les topics
#  En general, Frex donne de bons résultats
# ------------------------------------------------------------------------------
print(labelTopics(fit, n=10) )

# ------------------------------------------------------------------------------
# Importance des topics
# ------------------------------------------------------------------------------

# Réduire les marges de la figure

plot.STM(fit,
         type = "summary",
         labeltype = 'frex',
         main = 'Importance des topics',
         n = 10)

# ------------------------------------------------------------------------------
# Corrélation des topics
# Réduire cutoff pour augmenter les liens entre topics dans le graph
# ------------------------------------------------------------------------------

topic_corr = topicCorr(fit, method = "simple", cutoff = 0.1)
plot(topic_corr, topics = c())

# ------------------------------------------------------------------------------
# Qualité des topics
# ------------------------------------------------------------------------------
topicQuality(model=fit, documents=out$documents, main='Topic Quality',bty="n")

plot(fit, labeltype=c("frex"), main = 'Topic Most Frequent Words',bty="n", n= 5)

# ------------------------------------------------------------------------------
# Nuage Nuage ... cloud(fit, numero du topic)
# ------------------------------------------------------------------------------
cloud(fit, 1)

library(RColorBrewer)
# Définir les paramètres graphiques pour afficher plusieurs nuages de mots

# Ajuster les marges pour réduire l'espace entre les graphiques
par(mfrow = c(2, 2),            # Disposition en grille 3x3
    mar = c(0.5, 0, 0.5, 0),    # Marges autour de chaque graphique (bas, gauche, haut, droite)
    oma = c(0, 0, 0, 0),        # Marges extérieures (bas, gauche, haut, droite)
    mgp = c(3, 1, 0),           # Position des axes (ne pas trop les modifier ici)
    xpd = NA,                   # Permet de dessiner en dehors de la région des marges
    mai = c(0.1, 0.1, 0.1, 0.1) # Taille des marges (bas, gauche, haut, droite)
)

# Palette de couleurs personnalisée
my_colors <- brewer.pal(8, "Dark2")  # Choisissez une palette de couleurs esthétiques

# Génération des nuages de mots
cloud(fit, 
      topic = 10,                  # Nombre de sujets à afficher
      max.words = 250,              # Limite de mots affichés par nuage
      scale = c(2, 0.5),           # Taille maximale et minimale des mots
      colors = my_colors,          # Palette de couleurs pour les mots
      random.order = FALSE,        # Les mots les plus fréquents sont affichés en premier
      random.color = FALSE,        # Les couleurs sont attribuées par fréquence
      rot.per = 0.2)               # Pourcentage de mots en rotation verticale


par(mfrow = c(1, 1))
# ------------------------------------------------------------------------------
# Visualisation interactive avec stmBrowser
# ------------------------------------------------------------------------------

stmBrowser(fit, data=out$meta, c("annee"), text="cleaned_comm", labeltype='frex', n = 900)

# ------------------------------------------------------------------------------
# Quels sont les documents les plus representatifs d'un topic?
# La matrice de répartition des topics par document est accessible dans fit$theta
# ------------------------------------------------------------------------------

findThoughts(fit, texts = out$meta$content, topics = 4, n = 3 )

# ------------------------------------------------------------------------------
# Quels topics contiennent un mot ou une serie de mots
# ------------------------------------------------------------------------------

findTopic(fit, c("autonomous"), n = 20)

# ------------------------------------------------------------------------------
# Sauvegarder l'environnement avec toutes les variables
# ------------------------------------------------------------------------------

# save.image('techno_topic_models.Rdata')

# Appliquer le modèle LDA
  lda_model <- fit
  
  # Extraire les résultats
  theta <- fit$theta 

  theta_df <- as.data.frame(theta)
  colnames(theta_df) <- paste0("topic", 1:ncol(theta_df))  # Renommer les colonnes
  
  # Ajouter l'information des années aux proportions de topics
  doc_topic_data <- cbind(meta, theta_df)
  
  # Calculer la proportion moyenne de chaque topic par année
  topic_by_year <- doc_topic_data %>%
    group_by(annee) %>%
    summarize(across(starts_with("topic"), mean))
  
  # Restructurer les données pour un tracé ou une analyse plus facile
  topic_by_year_long <- topic_by_year %>%
    pivot_longer(cols = starts_with("topic"), names_to = "topic", values_to = "proportion")
  
  # Visualiser les proportions de topics par année
  ggplot(topic_by_year_long, aes(x = annee, y = proportion, color = topic)) +
    geom_line() +
    theme_minimal() +
    labs(title = "Proportion des topics par année", x = "Année", y = "Proportion", color = "Topic") +
    theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))

  ggplot(topic_by_year_long) +
    aes(x = annee, fill = topic, weight = proportion) +
    geom_bar() +
    scale_fill_hue(direction = 1) +
    theme_minimal()  
  
  ggplot(topic_by_year_long) +
    aes(x = annee, y = proportion, fill = topic) +
    geom_col() +
    scale_fill_brewer(palette = "Paired", direction = 1) +
    theme_minimal()
  
  
  ggplot(topic_by_year_long) +
    aes(x = annee, weight = proportion) +
    geom_bar(fill = "#112446") +
    theme_minimal() +
    facet_wrap(vars(topic), scales = "free_y")
  