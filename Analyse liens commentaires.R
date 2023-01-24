
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

library(Matrix)
library(data.table)

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

### Récupération des commentaires avec liens hypertextes

url_pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"

s <- subset(text, markdown %like% c("%https%","%http%","%www%","%WWW%"))
                                   
s2 <- str_extract_all(s$markdown, url_pattern)
names(s2) = s$publication

# Creating the large list
list_data <- s2

# Mapping -> converting the list to 
# dataframe
list_data <- Map(as.data.frame, list_data)
s3 <- as.data.frame(enframe(list_data))
names(s3) = c("publication", "liens")
class(s3)

s4 = split(s3$liens, s3$publication)
s5 = data.frame(unlist(s4))
s6 = data.frame(str_split(row.names(s5), '.d', simplify = T), s5)
names(s6) <- c("publication", "ne_pas_prendre","liens")

dbWriteTable(con, "liens_com", s6)


##### Extraire nom du site
s7 <- data.frame(s6$publication, str_split(s6$liens, "/", simplify = T))

s8 <- data.frame(as.numeric(s7$s6.publication), s7$X3) %>%
names() = c("publication","site")

`%not_in%` <- purrr::negate(`%in%`)
exclure <- c(")", 
             "![](http:", 
             "![file](https:", 
             ")![file](![https:", 
             ")![file](https:", 
             ").",
             "),",
             "	)![](http:",
             "2F",
             ")![](http:",
             ")2F,",
             ")https:",
             ")](http:"
             )
exclure

df <- s8 %>%   
    subset(., .$site %not_in% exclure) %>%
    subset(., row.names(.) != 19160)

harmoniser <- c("%pubpeer%", "%twit%", "%yout%")

a <- subset(df, df$site %like% harmoniser)

dbWriteTable(con, "liens_com_nettoyes", df)

