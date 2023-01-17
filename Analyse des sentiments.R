## test de modification
# Load the package
library(sentiment.ai)
library(SentimentAnalysis)
library(sentimentr)

### Espace de travail
setwd('/Users/maddi/Documents/Pubpeer/R')

library(alluvial)
library(DBI)
library(ggplot2)

library(sentimentr)
library(tidyverse)

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


debates <- data.frame(subset((str_replace_all((str_split(text$markdown, "'," , simplify = TRUE)), "[[:punct:]]", " ")), text != ""))
names(debates)=c("commentaires")

                 
###
debates_with_pol <- debates %>% 
  get_sentences() %>% 
  sentiment() %>% 
  mutate(polarity_level = ifelse(sentiment < 0.15, "Negative",
                                 ifelse(sentiment > 0.15, "Positive","Neutral")))
###
debates_with_pol %>% filter(polarity_level == "Negative") %>% View()
debates_with_pol %>% filter(polarity_level == "Positive") %>% View()

###
debates_with_senti %>% 
  ggplot() + geom_boxplot(aes(y = person, x = sentiment))
###
debates$dialogue %>% 
  get_sentences() %>% 
  sentiment_by() %>% #View()
  highlight()
###
debates %>% 
  get_sentences() %>% 
  sentiment_by(by = NULL) %>% #View()
  ggplot() + geom_density(aes(ave_sentiment))


###
library(sentimentr)
text = c("this video is awesom. the author is boring","I am very sad","very intersting!")
sentiment(text)
sentiment_by(text)
view(emotion(text))
