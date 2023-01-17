
rm(list = ls()) #supprimer tous les objets 
## Graphique alluvial publications commentees dans Pubpeer : pays


### Espace de travail
setwd('/Users/maddi/Documents/Pubpeer project/Pubpeer explo')

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

reqsql= "/* Evolution pub commentées par pays *//* Année de commentaire utilisée */
         select Z.lib_en,
	             	Y.annee_comm,
                sum(X.frac_geo) AS Nombre_de_publications_pays
		
	      	from public.data_pays_frac_geo X,
		           public.data_commentaires_annees Y,
			         public.pays_lib Z
			 
			     where X.id = Y.id_pub
			           and X.pays = Z.iso2
				         and Y.annee_comm between 2000 and 2021
				         and X.pays in ('BR','SE','TW','BE','CH','KR','IR','ES','IL','JP','NL',
								              	'FR','AU','IT','DE','IN','CA','GB','CN','US')
				   Group by Y.annee_comm,
							Z.lib_en
							
					  Order by 1"
reqsql2 = "-- Evolution nombre de commentaires par année par discipline ## WoS domains
    select  jdw as discipline,
        Y.annee_comm,
        sum(X.frac_disc) AS Nombre_de_publications_disc
		
		from public.data_frac_disc X,
		     public.data_commentaires_annees Y
			 
			 where X.id = Y.id_pub
				   and Y.annee_comm between 2000 and 2021
				   
				   Group by Y.annee_comm,
							jdw
							
							order by 1
							;"

data = dbGetQuery(con,reqsql) # donnees des pays
data_disc = dbGetQuery(con,reqsql2) # donnees des disciplines


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

cols4 <- c("royalblue", #Arts Humanities
           "navy",      #Life Sciences Biomedicine
           "orange",    #Multidisciplinary
           "deeppink2", #Physical Sciences
           "red",	      #Social Sciences
           "darkgreen"  #Technology
           )
########################
## Alluvial des pays  ##
########################

pdf(file = "TOP20 PAYS COMM - annee comm.pdf",  width=14,height=7,  pointsize = .8)
png(file = "TOP20 PAYS COMM - annee comm.png", width = 2000, height = 1200, pointsize = 9)

#alluvial_ts(d.f_pub)
alluvial_ts(data, wave = .3, ygap = 5, col = cols3, plotdir = 'centred', alpha=.9,
            grid = TRUE, grid.lwd = 1.5, xmargin = 0.5, lab.cex = 2.5, xlab = "",
            ylab = "", border = NA, axis.cex = 2.5, leg.cex = 2,
            leg.col='white', 
            title = "")

dev.off()

###############################
## Alluvial des disciplines  ##
###############################

pdf(file = "nb comm en annee de comm.pdf",  width=14,height=7,  pointsize = .8)
png(file = "nb comm en annee de comm.png", width = 2000, height = 1200, pointsize = 9)

#alluvial_ts(d.f_pub)
alluvial_ts(data_disc, wave = .3, ygap = 5, col = cols4, plotdir = 'centred', alpha=.9,
            grid = TRUE, grid.lwd = 1.5, xmargin = 0.5, lab.cex = 2.5, xlab = "",
            ylab = "", border = NA, axis.cex = 2.5, leg.cex = 2,
            leg.col='white', 
            title = "")

dev.off()

