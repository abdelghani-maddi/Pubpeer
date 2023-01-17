ffff
rm(list = ls()) #supprimer tous les objets 
## Graphique alluvial publications commentees dans Pubpeer : pays


### Espace de travail
setwd('/Users/maddi/Documents/Pubpeer/R')

library(alluvial)
library(DBI)

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

reqsql= paste('select  
                Z."lib_en",
                Y."ANNEE",
                sum(X."frac_geo") AS Nombre_de_publications_pays
		
		    from public.data_pays_frac_geo X,
		    public."DATA_ANNEES" Y,
			  public."PAYS_LIB" Z
			 
			   where X."ID" = Y."ID"
			     and X."PAYS" = Z."iso2"
				   and Y."ANNEE" between 2000 and 2021
				   and X."PAYS" in (', 
				   "'BR','SE','TW','BE','CH','KR','IR','ES','IL','JP','NL','FR','AU','IT','DE','IN','CA','GB','CN','US'", ")",
				   'Group by Y."ANNEE", Z."lib_en"', collapse = " ")

data = dbGetQuery(con,reqsql)

cols3 <- c(		
  "royalblue",	    #	Australia
  "darkslategray",	#	Belgium
  "royalblue4",	    #	Brazil
  "gray7",	        #	Canada
  "navy",	          #	China
  "red",	          #	France
  "ivory4",	        #	Germany
  "orange",         #	India
  "darkgreen",      #	Iran
  "turquoise3",	    #	Israel
  "violetred",	    #	Italy
  "gold4",	        #	Japan
  "pink4",	        #	Netherlands
  "royalblue4",	    #	Republic-of-Korea
  "gray7",	        #	Spain
  "navy",	          #	Sweden
  "deeppink2",	    #	Switzerland
  "ivory4",	        #	Taiwan
  "royalblue",      #	United-Kingdom
  "deeppink2"     	#	United-States
  )		


pdf(file = "TOP20 PAYS COMM.pdf",  width=14,height=7,  pointsize = .8)
png(file = "TOP20 PAYS COMM.png", width = 2000, height = 1200, pointsize = 9)

########################
#### Publications ######
########################

#alluvial_ts(d.f_pub)
alluvial_ts(data, wave = .3, ygap = 5, col = cols3, plotdir = 'centred', alpha=.9,
            grid = TRUE, grid.lwd = 1.5, xmargin = 0.5, lab.cex = 2.5, xlab = "",
            ylab = "", border = NA, axis.cex = 2.5, leg.cex = 2,
            leg.col='white', 
            title = "")

dev.off()



