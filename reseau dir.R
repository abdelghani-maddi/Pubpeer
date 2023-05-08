library(tidyverse)
library(igraph)
library(ggplot2)
library(ggraph)
library(GGally)
library(network)
library(sna)
library(intergraph)
library(ggforce)

# créer le dataframe initial
df <- data_urls %>%
  select(publication, typo, sequence) %>%
  unique()

df$sequence <- as.numeric(df$sequence)

# utiliser la fonction lead pour récupérer la valeur suivante de la colonne domain, selon la valeur de sequence
df2 <- df %>%
  arrange(publication, sequence) %>%
  mutate(domain_2 = lead(typo)) %>%
  filter(!is.na(domain_2)) %>%
  select(publication, domain_1 = typo, domain_2)

df3 <- df2 %>%
  select(domain_1, domain_2)

# reseau
# Calculer le nombre de fois que chaque paire de domaines apparaît dans le dataframe df2
edge_counts <- table(df3$domain_1, df3$domain_2)
edge_counts <- as.matrix(edge_counts)
g <- graph.adjacency(edge_counts, mode='directed', weighted=TRUE)

# Définir l'épaisseur des arêtes en fonction du nombre de fois que chaque paire de domaines apparaît
edge_sizes <- sqrt(edge_counts)
edge_sizes <- edge_sizes / max(edge_sizes) # ajuster la plage de l'épaisseur
# Affecter les tailles d'arêtes au graphique
set_edge_attr(g, "edge.size", index = E(g), value = edge_sizes)
# plot
plot(g, vertex.label.color = "black", vertex.label.cex = 0.9, edge.arrow.size = 0.2, edge.curved=.1)







# Aggregate links
df2_agg <- aggregate(cbind(weight=rep(1, nrow(df2))) ~ domain_1 + domain_2, df2, sum)


`%not_in%` <- purrr::negate(`%in%`)
tst <-  df2_agg[trimws(df2_agg$domain_1) != trimws(df2_agg$domain_2), ]
tst2 <-  df2_agg[trimws(df2_agg$domain_1) != trimws(df2_agg$domain_2), ] %>%
  filter(., domain_1 %not_in% c("image","pubpeer", "Editeur - revue", "Autre") & domain_2 %not_in% c("image","pubpeer", "Editeur - revue", "Autre"))

library(openxlsx)
write.xlsx(tst, "/Users/maddi/Documents/Pubpeer project/Pubpeer explo/reseau seq.xlsx")

# Create graph
g <- graph_from_data_frame(tst2, directed = TRUE)

# Create vertex labels
vertices <- data.frame(label = unique(c(tst2$domain_1, tst2$domain_2)))

# Set edge width based on weight
E(g)$width <- tst2$weight / max(tst2$weight) * 10

plot(g, vertex.label.color = "black", vertex.label.cex = 0.9, edge.arrow.size = 0.2, edge.curved=.2, edge.width = E(g)$width)



#####
df4 <- df3 %>%
  filter(., domain_1 %not_in% c("image","pubpeer", "Editeur - revue", "Autre") & domain_2 %not_in% c("image","pubpeer", "Editeur - revue", "Autre"))
edge_counts2 <- table(df4$domain_1, df4$domain_2)


m <- as.matrix.data.frame(edge_counts2)
mdf <- as.data.frame.matrix(edge_counts2)
# Convertir le tableau croisé en matrice
mat <- as.matrix(mdf)

heatmap(mat, scale = "row")

heatmap(mat,                                     # Draw heatmap without dendrograms
        Rowv = NA,
        Colv = NA)

