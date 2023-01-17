/* Vérification nombre ID dans les bases */
   
-- ID des publications dans la base publications
select count (distinct publication) as id,
       count (distinct original_id) as original_id

       from public.data_pub;

-- ID des publications dans la base commentaires
select count (distinct id) as id,
       count (distinct publication) as id_pub
	   
	   from public.data_commentaires
;
-- ID dans la table data.commentaire.annee

select count (distinct id_pub) from public.data_commentaires_annees ;

-- ID bdd annees
select "ANNEE", count (distinct "ID") as id 
      from public."DATA_ANNEES" 
	      --where "ANNEE" between 2012 and 2020
		     group by "ANNEE"
			    order by 1
		  ;
/* Créer une table de libellés des pays */
create table PAYS_LIB
(ORDRE	INTEGER,
 NUM	INTEGER,
 ISO2	VARCHAR(50),
 ISO3	VARCHAR(50),
 LIB_FR	VARCHAR(150),
 LIB_EN VARCHAR(150),
 
 PRIMARY KEY (ORDRE)
)
/* les données sont importées directement sur le fichier dans le chemin ci-après  
COPY public.PAYS_LIB
FROM '/Users/maddi/Documents/Pubpeer/Excel CSV/PAYS lib.csv'
DELIMITER ','
CSV HEADER;*/

/* Calcul du fractionnelent géographique */
drop table data_pays_frac_geo;
create table data_pays_frac_geo as

SELECT X.id,
       X.pays,
	   (X.nb_adress_pays/Y.nb_tot_adress::float) AS frac_geo

FROM

(select DISTINCT id,
		pays, 
		COUNT (*) AS nb_adress_pays
		FROM (SELECT id, TRIM(pays) AS pays 
			  FROM public.data_pays) AA
                  GROUP BY id,
		                   pays) X,

(select id,
		COUNT(pays) AS nb_tot_adress 
		    FROM public.data_pays
        		GROUP BY id) Y

WHERE X.id = Y.id

;
/* test fractionnement */
--SELECT "ID", SUM(FRAC_GEO) FROM public.data_pays_frac_geo GROUP BY "ID" ORDER BY 1 DESC; --ok

/* Calcul du fractionnement disciplinaire */

drop table DATA_PAYS_FRAC_DISC;
create table DATA_PAYS_FRAC_DISC as

SELECT X."ID",
       X."JD",
	   (1/Y."nb_tot_jd"::float) AS FRAC_DISC

FROM

(select * FROM public."DATA_JD") X,

(select "ID",
		COUNT("JD") AS nb_tot_jd 
		    FROM public."DATA_JD"
        		GROUP BY "ID") Y

WHERE X."ID" = Y."ID"
      --AND X."ID"='52676'

;
-- Test fractionnement disciplinaire.
--SELECT "ID", SUM(FRAC_DISC) FROM public.DATA_PAYS_FRAC_DISC GROUP BY "ID" ORDER BY 1 desc; --ok

/* Nombre total de publications commentées */
select count (distinct "ID") AS Nombre_de_publications_commentees from public."DATA_ANNEES"
-- Sur les 101271 publications, 90434 ont au moins une discipline (WoS). Soit 89,3 %
;
/* Nombre de publications commentées par année */
select "ANNEE", count (distinct "ID") AS Nombre_de_publications_commentees
       from public."DATA_ANNEES"
	      WHERE "ANNEE" between 2012 and 2020-->'1984'
	        GROUP BY "ANNEE" order by 1;

/* Nombre de publications commentées par année */
select "ANNEE", count ("ID") AS Nombre_de_publications_commentees
       from public."DATA_ANNEES"
	      WHERE "ANNEE" between 2012 and 2020-->'1984'
	        GROUP BY "ANNEE" order by 1;
			
/* Distribution disciplinaire */
select "JD",
       SUM("frac_disc") nombre_publications_frac
          from public.data_pays_frac_disc
		          group by "JD"
				     order by 2 desc;
	
/* selectionner le top 20 des pays avec le plus de pub commentées */
select  "PAYS",
        sum("frac_geo") AS Nombre_de_publications_pays
		   from public.data_pays_frac_geo
		       where "PAYS" <> 'None'
				   Group by "PAYS"
				   order by 2 desc;
/* sur les 101271, il y a 20069 publications sans information sur le pays (veleurs = 'None'), 
   et on a 80765 dont l'information sur la pays existe*/


/* Evolution pub commentées par pays */
select  Y."ANNEE",
		Z."lib_en",
        sum(X."frac_geo") AS Nombre_de_publications_pays
		
		from public.data_pays_frac_geo X,
		     public."DATA_ANNEES" Y,
			 public."PAYS_LIB" Z
			 
			 where X."ID" = Y."ID"
			       and X."PAYS" = Z."iso2"
				   and Y."ANNEE" between 2000 and 2021
				   and X."PAYS" in ('BR','SE','TW','BE','CH','KR','IR','ES','IL','JP','NL',
									'FR','AU','IT','DE','IN','CA','GB','CN','US')
				   
				   Group by Y."ANNEE",
							Z."lib_en";
							
/* Extraction données commentaires par discipline */ 		
drop table commentaires_par_discipline;
create table commentaires_par_discipline as

select distinct markdown ,
       'Arts Humanities' as discipline
         from public."DATA_COMMENTAIRES" x,
		      public."DATA_JD" y
			      where  x.publication = y."ID"
				     and "JD" = 'Arts Humanities'
union			  

select distinct markdown ,
       'Life Sciences Biomedicine' as discipline
         from public."DATA_COMMENTAIRES" x,
		      public."DATA_JD" y
			      where  x.publication = y."ID"
				     and "JD" = 'Life Sciences Biomedicine'
union	


select distinct markdown ,
       'Multidisciplinary' as discipline
         from public."DATA_COMMENTAIRES" x,
		      public."DATA_JD" y
			      where  x.publication = y."ID"
				     and "JD" = 'Multidisciplinary'
union		


select distinct markdown ,
       'Physical Sciences' as discipline
         from public."DATA_COMMENTAIRES" x,
		      public."DATA_JD" y
			      where  x.publication = y."ID"
				     and "JD" = 'Physical Sciences'
union

select distinct markdown ,
       'Social Sciences' as discipline
         from public."DATA_COMMENTAIRES" x,
		      public."DATA_JD" y
			      where  x.publication = y."ID"
				     and "JD" = 'Social Sciences'
union	

select distinct markdown ,
       'Technology' as discipline
         from public."DATA_COMMENTAIRES" x,
		      public."DATA_JD" y
			      where  x.publication = y."ID"
				     and "JD" = 'Technology'
;	



/* Nombre de commentaires par année */
select annee_comm,
       nb_comm,
	   nb_pub,
	   nb_comm/nb_pub::float as comm_par_pub

from

(select annee_comm,
       count (id_comm) as nb_comm,
	   count (distinct id_pub) as nb_pub

       from public.data_commentaires_annees
	   group by annee_comm) x
	   order by 1
;
/**/

/* Citations par annee */
select * from public."DATA_CITATIONS" 
       where "ANNEE_CITATION" <> 'None' 
	         and "ID"='106986'
			    ORDER BY "ANNEE_CITATION"
				;

		