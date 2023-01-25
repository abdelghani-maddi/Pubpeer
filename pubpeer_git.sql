/*******************************************/
/*******************************************/
/*           ##### Recette ####            */
/*******************************************/
/*******************************************/
-- ID des publications dans la base commentaires
select *	   
	   from public.data_commentaires∑
	     where publication ='106541'

/* Vérification nombre ID dans les bases */

-- ID des publications dans la base publications
select count (distinct publication) as id,
       count (distinct original_id) as original_id
⁄
       from public.data_pub;

-- ID des publications dans la base commentaires
select count (distinct id) as id,
       count (distinct publication) as id_pub
	   
	   from public.data_commentaires
;
-- ID dans la table data.commentaire.annee

select count (distinct id_pub) from public.data_commentaires_annees ; --101272 -- ok
select count (id_comm) from public.data_commentaires_annees ; --189416 -- ok
-- 1,87 commentaire par publication en moyenne.
		 
/* Vérification nombre de commentaires */ -- ATTENTION !!! 1 commentaire en plus dans la table des "Publications"
select *,
       (x.nb_com_table_pub - y.nb_com_table_com_ann) as diff
from

 (select sum (nb_comm) as nb_com_table_pub
	   from public.data_annee) x,
	   
  (select count (id_comm) as nb_com_table_com_ann
	   from public.data_commentaires_annees) y	   
;

/* Test fractionnement */
SELECT id, SUM(frac_geo) FROM public.data_pays_frac_geo GROUP BY id ORDER BY 1 desc; --ok
SELECT id, SUM(frac_geo) FROM public.data_pays_frac_geo GROUP BY id ORDER BY 1 asc; --ok


-- Test fractionnement disciplinaire.
SELECT id, SUM(frac_disc) FROM public.data_frac_disc GROUP BY id ORDER BY 1 desc; --ok
SELECT id, SUM(frac_disc) FROM public.data_frac_disc GROUP BY id ORDER BY 1 asc; --ok	


-- Test fractionnement disciplinaire.
SELECT id, SUM(frac_disc) FROM public.data_frac_disc_jwc GROUP BY id ORDER BY 1 desc; --ok
SELECT id, SUM(frac_disc) FROM public.data_frac_disc_jwc GROUP BY id ORDER BY 1 asc; --ok	

/*******************************************/
/*******************************************/
/*   ##### Préparation des données ####    */
/*******************************************/
/*******************************************/

/* Créer une table de libellés des pays */
drop table pays_lib;
create table pays_lib
(ordre	VARCHAR(50),
 num	VARCHAR(50),
 iso2	VARCHAR(50),
 iso3	VARCHAR(50),
 lib_fr	VARCHAR(150),
 lib_en VARCHAR(150),
 
 PRIMARY KEY (ordre)
)
;
/* Les données sont importées directement sur le fichier dans le chemin ci-après (récupéré sur internet : code iso des pays) : 
-- Commande utilisée : " "\\copy public.pays_lib (ordre, num, iso2, iso3, lib_fr, lib_en) 
                 FROM '/Users/maddi/Documents/Pubpeer project/Pubpeer explo/Excel CSV/PAYS lib.csv' DELIMITER ';' 
				 CSV HEADER ENCODING 'UTF8' QUOTE '\"' ESCAPE '''';""
*/

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

/* Calcul du fractionnement disciplinaire des grands domaines */

drop table data_frac_disc;
create table data_frac_disc as

SELECT X.id,
       trim(X.jdw) as jdw,
	   (1/Y.nb_tot_jd::float) AS frac_disc

FROM

(select * FROM public.data_jdw) X,

(select id,
		COUNT(trim(jdw)) AS nb_tot_jd 
		    FROM public.data_jdw
        		GROUP BY id) Y

WHERE X.id = Y.id
      --AND X.id='52676' -- juste pour le test

;


/* Calcul du fractionnement disciplinaire des WoS subject categories */

drop table data_frac_disc_jwc;
create table data_frac_disc_jwc as

SELECT X.id,
       trim(X.jcw) as jcw,
	   (1/Y.nb_tot_jd::float) AS frac_disc

FROM

(select * FROM public.data_jcw) X,

(select id,
		COUNT(trim(jcw)) AS nb_tot_jd 
		    FROM public.data_jcw
        		GROUP BY id) Y

WHERE X.id = Y.id
      --AND X.id='52676' -- juste pour le test

;


/*******************************************/
/*******************************************/
/*     ##### Analyse des données ####      */
/*******************************************/
/*******************************************/


/* Nombre total de publications commentées */
select count (distinct id) AS Nombre_de_publications_commentees from public.data_annee
;

-- Nombres par discipline
select jdw as jdw,
       sum(frac_disc) nbr_pub
	      from public.data_pays_frac_disc
		     group by jdw
		      order by 2 desc
-- Sur les 101,271 publications, 90,434 ont au moins une discipline (WoS). Soit 89,3 %
;
/**********************************/
/* Nombre de commentaires par année, nombre de publications commentées et
   nombre moyen de commentaires par publication (NB : c'est l'année de commentaire 
   qui est utilisée et non l'année de publication des papier commentés) */
select annee_comm,
       nb_comm,
	   nb_pub,
	   ROUND(comm_par_pub,2) as comm_par_pub
from

(select annee_comm,
       nb_comm,
	   nb_pub,
	   nb_comm/nb_pub::numeric as comm_par_pub

from

(select annee_comm,
       count (id_comm) as nb_comm,
	   count (distinct id_pub) as nb_pub

       from public.data_commentaires_annees
	   group by annee_comm) x) aa
	   order by 1
;
/**********************************/

-- Nombre de publications commentées par année de publication
select annee, count (distinct id) as nb_par_ann_pub 
      from public.data_annee 
	      where annee between 2000 and 2020
		     group by annee
			    order by 1
;

/* Nombre de commentaires par année */
select annee_comm, count(id_comm) AS Nombre_de_commentaires
       from public.data_commentaires_annees
	      WHERE annee_comm between 2010 and 2020-->'1984'
	        GROUP BY annee_comm order by 1;
			
/* Distribution disciplinaire */
select jdw,
       SUM("frac_disc") nombre_publications_frac
          from public.data_pays_frac_disc
		          group by jdw
				     order by 2 desc;
	
/* selectionner le top 20 des pays avec le plus de pub commentées */
select  pays,
        sum(frac_geo) AS Nombre_de_publications_pays
		   from public.data_pays_frac_geo
		       where pays <> 'None'
				   Group by pays
				   order by 2 desc;
				   
/* sur les 101271, il y a 20069 publications sans information sur le pays (veleurs = 'None'), 
   et on a 80765 dont l'information sur la pays existe*/

/* Evolution pub commentées par pays *//* Année de commentaire utilisée */
select  Y.annee_comm,
		Z.lib_en,
        sum(X."frac_geo") AS Nombre_de_publications_pays
		
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
							
							order by 1
							;

/*******************************************/
/* Analyse des commentaires par discipline */
/*******************************************/

-- Evolution nombre de commentaires par année par discipline ## WoS domains
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
							;
-- Faire un zoom sur l'annee 2016 et surtout pour les Sciences sociales qui ont causé le pic
select discipline,
       annee_comm,
	   Nombre_de_publications_disc,
	   Nombre_de_publications_disc/nb_tot::real as part_discipline
from

(select  jcw as discipline,
        Y.annee_comm,
        sum(X.frac_disc) AS Nombre_de_publications_disc
		
		from public.data_frac_disc_jwc X,
		     public.data_commentaires_annees Y
			 
			 where X.id = Y.id_pub
				   and Y.annee_comm = 2016
				       and X.id in (select distinct id from public.data_jdw where jdw = 'Social Sciences') 
				   
				   Group by Y.annee_comm,
							jcw) aa,

(select count (distinct id_pub) AS nb_tot
		from public.data_commentaires_annees 
			 where annee_comm = 2016
				       and id_pub in (select distinct id from public.data_jdw where jdw = 'Social Sciences') 
							) bb
							
							order by 3 desc
							;


/*******************************************/
/*******************************************/
/*# Extractions pour l'analyse textuelle #*/
/*******************************************/
/*******************************************/
 
/* Extraction données commentaires par discipline */ 		
drop table commentaires_par_discipline;
create table commentaires_par_discipline as

select distinct inner_id, publication, markdown ,
       'Arts Humanities' as discipline
         from public.data_commentaires x,
		      public.data_jdw y
			      where  x.publication = y.id
				     and jdw = 'Arts Humanities'
union			  

select distinct inner_id, publication, markdown ,
       'Life Sciences Biomedicine' as discipline
         from public.data_commentaires x,
		      public.data_jdw y
			      where  x.publication = y.id
				     and jdw = 'Life Sciences Biomedicine'
union	


select distinct inner_id, publication, markdown ,
       'Multidisciplinary' as discipline
         from public.data_commentaires x,
		      public.data_jdw y
			      where  x.publication = y.id
				     and jdw = 'Multidisciplinary'
union		


select distinct inner_id, publication, markdown ,
       'Physical Sciences' as discipline
         from public.data_commentaires x,
		      public.data_jdw y
			      where  x.publication = y.id
				     and jdw = 'Physical Sciences'
union

select distinct inner_id, publication, markdown ,
       'Social Sciences' as discipline
         from public.data_commentaires x,
		      public.data_jdw y
			      where  x.publication = y.id
				     and jdw = 'Social Sciences'
union	

select distinct inner_id, publication, markdown ,
       'Technology' as discipline
         from public.data_commentaires x,
		      public.data_jdw y
			      where  x.publication = y.id
				     and jdw = 'Technology'
;	


/* Citations par annee */
select * from public."DATA_CITATIONS" 
       where "ANNEE_CITATION" <> 'None' 
	         and id='106986'
			    ORDER BY "ANNEE_CITATION"
;

/* Préparation des données sur les liens qui figurent dans les commentaires pour les représenter sur VosViewer */
drop table liens_com_disc;
create table liens_com_disc as
select x.*,
       jdw
        from public.liens_com_nettoyes x,
		     public.data_jdw y
			 
			   where id = publication
			   order by x.site
;

select distinct x.site_1,
       y.site_2,
	   count (distinct x.publication)
	from  (select publication, site as site_1 from liens_com_nettoyes) x,
		  (select publication, site as site_2 from liens_com_nettoyes) y
		where x.publication = y.publication
		   group by x.site_1,
       				y.site_2
					
					order by 3 desc


    
;



		