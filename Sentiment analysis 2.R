## source : https://ladal.edu.au/sentiment.html
# install packages
install.packages("stringr")
install.packages("tibble")
install.packages("tidytext")
install.packages("textdata")
install.packages("Hmisc")
install.packages("sentimentr")
install.packages("zoo")
install.packages("flextable")

# activate packages
library(dplyr)
library(stringr)
library(tidyr)
library(tibble)
library(tidytext)
library(textdata)
library(Hmisc)
library(sentimentr)
library(zoo)
library(flextable)
library(DBI)
# activate klippy for copy-to-clipboard button
klippy::klippy()

### CONNEXION

con<-dbConnect(RPostgres::Postgres())

db <- 'SKEPTISCIENCE'  #provide the name of your db
host_db <- 'localhost' # server
db_port <- '5433'  # port DBA
db_user <- 'postgres' # nom utilisateur  
db_password <- 'Maroua1912'
con <- dbConnect(RPostgres::Postgres(), dbname = db, host=host_db, port=db_port, user=db_user, password=db_password)  
# Test connexion
dbListTables(con) 

### RECUPERATION DES DONNEES

reqsql= paste('select * from commentaires_par_discipline')
text = dbGetQuery(con,reqsql)

txtclean <- function(x, title){
  require(dplyr)
  require(stringr)
  require(tibble)
  x <- x %>%
    iconv(to = "UTF-8") %>%
    base::tolower() %>%
    paste0(collapse = " ") %>%
    stringr::str_squish()%>%
    stringr::str_split(" ") %>%
    unlist() %>%
    tibble::tibble() %>%
    dplyr::select(word = 1, everything()) %>%
    dplyr::mutate(novel = title) %>%
    dplyr::anti_join(stop_words) %>%
    dplyr::mutate(word = str_remove_all(word, "\\W")) %>%
    dplyr::filter(word != "")
}

# selectionner les disciplines
arthum = subset.data.frame(text, text$discipline == 'Arts Humanities', select = markdown)
sdv    = subset.data.frame(text, text$discipline == 'Life Sciences Biomedicine', select = markdown)
multi  = subset.data.frame(text, text$discipline == 'Multidisciplinary', select = markdown)
physi  = subset.data.frame(text, text$discipline == 'Physical Sciences', select = markdown)
socisc = subset.data.frame(text, text$discipline == 'Social Sciences', select = markdown)
techno = subset.data.frame(text, text$discipline == 'Technology', select = markdown)


# process text data
arthum_clean <- txtclean(arthum, "arthum")
sdv_clean    <- txtclean(sdv, "sdv")
multi_clean  <- txtclean(multi, "multi")
physi_clean  <- txtclean(physi, "physi")
socisc_clean <- txtclean(socisc, "socisc")
techno_clean <- txtclean(techno, "techno")


## Basic Sentiment Analysis
novels_anno <- rbind(arthum_clean, sdv_clean, multi_clean, physi_clean, socisc_clean, techno_clean) %>%
  dplyr::group_by(novel) %>%
  dplyr::mutate(words = n()) %>%
  dplyr::left_join(tidytext::get_sentiments("nrc")) %>%
  dplyr::mutate(novel = factor(novel),
                sentiment = factor(sentiment))

###
novels <- novels_anno %>%
  dplyr::group_by(novel) %>%
  dplyr::group_by(novel, sentiment) %>%
  dplyr::summarise(sentiment = unique(sentiment),
                   sentiment_freq = n(),
                   words = unique(words)) %>%
  dplyr::filter(is.na(sentiment) == F) %>%
  dplyr::mutate(percentage = round(sentiment_freq/words*100, 1))

###
novels %>%
  dplyr::filter(sentiment != "positive",
                sentiment != "negative") %>%
  ggplot(aes(sentiment, percentage, fill = novel)) +    
  geom_bar(stat="identity",   
           position=position_dodge()) + 
  scale_fill_manual(name = "", values=c("orange", "gray70", "red", "grey30", "navy", "darkgreen")) +
  theme_bw() +
  theme(legend.position = "top")

###
novels %>%
  dplyr::filter(sentiment != "positive",
                sentiment != "negative") %>%
  dplyr::mutate(sentiment = factor(sentiment, 
                                   levels = c("anger", "fear", "disgust", "sadness",
                                              "surprise", "anticipation", "trust", "joy"))) %>%
  ggplot(aes(novel, percentage, fill = sentiment)) +    
  geom_bar(stat="identity", position=position_dodge()) + 
  scale_fill_brewer(palette = "RdBu") +
  theme_bw() +
  theme(legend.position = "right") +
  coord_flip()

##
novels %>%
  dplyr::filter(sentiment != "positive",
                sentiment != "negative") %>%
  dplyr::mutate(sentiment = factor(sentiment, 
                                   levels = c("anger", "fear", "disgust", "sadness",
                                              "surprise", "anticipation", "trust", "joy"))) %>%
  ggplot(aes(novel, percentage, fill = sentiment)) +    
  geom_bar(stat="identity", position=position_dodge()) + 
  scale_fill_brewer(palette = "RdBu") +
  theme_bw() +
  theme(legend.position = "right") +
  coord_flip()

###
novels_impw <- novels_anno %>%
  dplyr::filter(!is.na(sentiment),
                sentiment != "anticipation",
                sentiment != "surprise",
                sentiment != "disgust",
                sentiment != "negative",
                sentiment != "sadness",
                sentiment != "positive") %>%
  dplyr::mutate(sentiment = factor(sentiment, levels = c("anger", "fear",  "trust", "joy"))) %>%
  dplyr::group_by(novel) %>%
  dplyr::count(word, sentiment, sort = TRUE) %>%
  dplyr::group_by(novel, sentiment) %>%
  dplyr::top_n(3) %>%
  dplyr::mutate(score = n/sum(n))

##
novels_impw %>%
  dplyr::group_by(novel) %>%
  slice_max(score, n = 20) %>%
  dplyr::arrange(desc(score)) %>%
  dplyr::ungroup() %>%
  ggplot(aes(x = reorder(word, score), y = score, fill = word)) +
  facet_wrap(novel~sentiment, ncol = 4, scales = "free_y") +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(x = "Words")

###
novels %>%
  dplyr::filter(sentiment == "positive" | sentiment == "negative") %>%
  dplyr::select(-percentage, -words) %>%
  dplyr::mutate(sentiment_sum = sum(sentiment_freq),
                positive = sentiment_sum-sentiment_freq) %>%
  dplyr::filter(sentiment != "positive") %>%
  dplyr::rename(negative = sentiment_freq) %>%
  dplyr::select(novel, positive, negative) %>%
  dplyr::group_by(novel) %>%
  dplyr::summarise(polarity = positive/negative) %>%
  ggplot(aes(reorder(novel, polarity, mean), polarity, fill = novel)) +    
  geom_bar(stat = "identity") + 
  geom_text(aes(y = polarity-0.1, label = round(polarity, 2)), 
            color = "white", size = 4) + 
  theme_bw() +
  labs(y = "Polarity\n(ration of positive to negative emitives)",
       x = "") +
  coord_cartesian(y= c(0,2.5)) +
  scale_y_continuous(breaks = seq(0,2,1),
                     labels = c("more negative", "neutral", "more positive")) +
  theme(legend.position = "none")
 
###
novels_bin <- novels_anno %>%
  dplyr::group_by(novel) %>%
  dplyr::filter(is.na(sentiment) | sentiment == "negative" | sentiment == "positive") %>%
  dplyr::mutate(sentiment = as.character(sentiment),
                sentiment = case_when(is.na(sentiment) ~ "0", 
                                      TRUE ~ sentiment),
                sentiment= case_when(sentiment == "0" ~ 0,
                                     sentiment == "positive" ~ 1,
                                     TRUE ~ -1), 
                id = 1:n(),
                index = as.numeric(cut2(id, m=100))) %>%
  dplyr::group_by(novel, index) %>%
  dplyr::summarize(index = unique(index),
                   polarity = mean(sentiment))

###

ggplot(novels_bin, aes(index, polarity)) + 
  facet_wrap(vars(novel), scales="free_x") +
  geom_smooth(se = F, col = "black") + 
  theme_bw() +
  labs(y = "polarity ratio (mean by bin)",
       x = "index (bin)")
