#*************************************  Import des packages
install.packages("plm")
install.packages("stargazer")
install.packages("tseries")
install.packages("gplots")
install.packages("pglm")
install.packages("margins")
install.packages("lme4")
install.packages("survival")
install.packages(c("sandwich","clubSandwich","pROC"))
install.packages("Matrix")
install.packages("glmmTMB")
install.packages("reformulas")
install.packages("Rcpp")
install.packages("installr")
install.packages("haven")
install.packages("janitor")
install.packages("ggplot2")
install.packages("farver")
install.packages("labeling")
install.packages("Hmisc")
install.packages(c("DT","kableExtra"))

packageVersion("Rcpp")
packageVersion("lme4")
packageVersion("Matrix")


library(lme4)
library(installr)
library(tidyr)
library(haven)
library(dplyr)
library(ggplot2)
library(survey)
library(stats)
library(Hmisc)
library(forcats)  
library(plotly)
library(janitor)
library(DT)
library(kableExtra)
library(glmmTMB)
library(haven)
library(foreign)
library(dplyr)
library(plm)
library(stargazer)
library(lmtest)
library(tseries)
library(gplots) 
library(pglm)
library(margins)
library(lme4)
library(survival)
library(sandwich)
library(clubSandwich)
library(pROC)
library(Matrix)
library(reformulas)
library(janitor)
library(farver)
library(ggplot2)
library(labeling)
library(glmmTMB)
print("Packages chargés avec succès")

# Package modélisation PANEL

pdata$POND_LONG_IND_2014
packageVersion("Matrix")
packageVersion("lme4")
sessionInfo()
R.version

#********************* Selection des variables pour la fusion **********************************************
#* Idée : Garder certaine variables au niveau ménage sinon l'information individuelle n'est pas disponible *
#* Prendre les personnes plus de 18 ans qui sont en age de décider 
#* Fait correspondre les noms des variables                                                                *
#* Contruire la base de données PANEL (2014-2021)                                                          *
#* *********************************************************************************************************

# *********************************  SELECTION (RIRE, ça en fait trop non ?)********************************
View(BASE_2014)

V=data.frame(nom_variable=names(BASE_2014))

View(V)

# ***************** Ajout de la variable decile patrimoine *************************************************
# 2020
V=menage_db %>% 
  select(IDENT,decile_patrimoine) 

View(V)

BASE_2020 = BASE_2020 %>% 
  left_join(V,by="IDENT")

View(BASE_2020)

BASE_2020$decile_patrimoine.x = NULL 
BASE_2020$decile_patrimoine.y = NULL 

# 2017
W=menage_db_2017 %>% 
  select(IDENT,decile_patrimoine) 

View(W)

BASE_2017 = BASE_2017 %>% 
  left_join(W,by="IDENT")

View(BASE_2017)

BASE_2017$decile_patrimoine.x = NULL 
BASE_2017$decile_patrimoine.y = NULL  

# 2014
X=menage_db_2014 %>% 
  select(IDENT,decile_patrimoine) 

View(X)

BASE_2014 = BASE_2014 %>% 
  left_join(X,by="IDENT")

View(BASE_2014)

BASE_2014$decile_patrimoine.x = NULL
BASE_2014$decile_patrimoine.y = NULL 

#************************* Selection des variables finales *******************************************
#*** 2014
BASE_2014F=BASE_2014 %>% 
  select(
    IDENT, POND, IDENTINDL, IDENTIND14, LIVABL,
    LDD, LEP, MMLA_I, MMLDDS_I, MMLEP_I,
    NPERS, NENFANTS, NACTIFS, ACTIF, ETAMATRI,
    AGE_RELATIF_I, CATAGE_RELATIF_I, NIVEDUC_I, CS8_I, TYPEMPLOI,
    SITUA, STATUT, ZREVDEC_I, decile_revenu_ind, decile_patrimoine,
    PATFISOM, PATIMM, PATRIPRO, PATRIRES, MTDETTES,
    PATFINANC, CNDUR, DIEXP, DIF, AVI,
    MAVI, ASDEC, CEL, MCEL_I, PEL,
    MPEL_I, RETRAIT, MEPRETRAITE_I, EPSALA, MEPSALAR_I,
    MLEE_I, VMOB, MVMOB, ACTION, PEA,
    TERM, MCOMPTERM, LIVJEUN, MLIVJEUNE_I, R_PATIMMO,
    R_RESPRIN, MRESPRIN, R_DETRE, DETIMM, NBLOG,
    NE, DETPRO, HERTABH_I, Heritage_I, HERTABD_I,
    Donation_R_I, CONSEP, RISKFI
    )

View(BASE_2014F)

nrow(BASE_2014F)

#*** 2017
BASE_2017F=BASE_2017 %>% 
  select(
    IDENT, pond_trans, IDENTINDL, IDENTIND17, LIVABL,
    LDD, LEP, MMLA_I, MMLDDS_I, MMLEP_I,
    NPERS, NENFANTS, NACTIFS, ACTIF, ETAMATRI,
    AGE_RELATIF_I, CATAGE_RELATIF_I, NIVEDUC_I, CS8_I, TYPEMPLOI,
    SITUA, STATUT, ZREVDEC_I, decile_revenu_ind, decile_patrimoine,
    PATFISOM, PATIMM, PATRIPRO, PATRIRES, MTDETTES,
    PATFINANC, CNDUR, DIEXP, DIF, AVI,
    MAVI, ASDEC, CEL, MCEL_I, PEL,
    MPEL_I, RETRAIT, MEPRETRAITE_I, EPSALA, MEPSALAR_I,
    MLEE_I, VMOB, MVMOB, ACTION, PEA,
    TERM, MCOMPTERM, LIVJEUN, MLIVJEUNE_I, R_PATIMMO,
    R_RESPRIN, MRESPRIN, R_DETRE, DETIMM, NBLOG,
    NE, DETPRO, HERTABH_I, Heritage_I, HERTABD_I,
    Donation_R_I, CONSEP, RISKFI
  )

View(BASE_2017F)

#*** 2020
BASE_2020F=BASE_2020 %>% 
  select(
    IDENT, POND_TRANS, IDENTINDL, IDENTIND20, LIVABL,
    LDD, LEP, MMLA_I, MMLDDS_I, MMLEP_I,
    NPERS, NENFANTS, NACTIFS, ACTIF, ETAMATRI,
    AGE_RELATIF_I, CATAGE_RELATIF_I, NIVEDUC_I, CS8_I, TYPEMPLOI,
    SITUA, STATUT, ZREVDEC_I, decile_revenu_ind, decile_patrimoine,
    PATFISOM, PATIMM, PATRIPRO, PATRIRES, MTDETTES,
    PATFINANC, CNDUR, DIEXP, DIF, AVI,
    MAVI, ASDEC, CEL, MCEL_I, PEL,
    MPEL_I, RETRAIT, MEPRETRAITE_I, EPSALA, MEPSALAR_I,
    MLEE_I, VMOB, MVMOB, ACTION, PEA,
    TERM, MCOMPTERM, LIVJEUN, MLIVJEUNE_I, R_PATIMMO,
    R_RESPRIN, MRESPRIN, R_DETRE, DETIMM, NBLOG,
    NE, DETPRO, HERTABH_I, Heritage_I, HERTABD_I,
    Donation_R_I, CONSEP, RISKFI
  )

View(BASE_2020F)

#*********************** Variable année pour le Panel
BASE_2014F$ANNEE_ENQUETE = 2014
View(BASE_2014F)

BASE_2017F$ANNEE_ENQUETE = 2017
View(BASE_2017F)

BASE_2020F$ANNEE_ENQUETE = 2020
View(BASE_2020F)

#****************** Individu Panel dans chaque base *****************************************
dim(BASE_2014F)
BASE_2014F <- BASE_2014F[substr(BASE_2014F$IDENTINDL, 1, 1) == "1", ] # DEbut IDENTINDL par 1
View(BASE_2014F)
dim(BASE_2014F)

BASE_2014F %>% 
  summarise(n_distinct(IDENT),n_distinct(IDENTINDL))  # 5784 menages et 13872 individus

dim(BASE_2017F)
BASE_2017F <- BASE_2017F[substr(BASE_2017F$IDENTINDL, 1, 1) == "1", ] # DEbut IDENTINDL par 1
View(BASE_2017F)
dim(BASE_2017F)

BASE_2017F %>% 
  summarise(n_distinct(IDENT),n_distinct(IDENTINDL)) # 13121 menages et 30957 individus

dim(BASE_2020F)
BASE_2020F <- BASE_2020F[substr(BASE_2020F$IDENTINDL, 1, 1) == "1", ] # DEbut IDENTINDL par 1
View(BASE_2020F)
dim(BASE_2020F)

BASE_2020F %>% 
  summarise(n_distinct(IDENT),n_distinct(IDENTINDL)) # 10194 menages et 23487 individus

#******************** Les individus suivi sur les trois vagues *********************************
IdInd2014 = BASE_2014F %>% 
  select(IDENTINDL) %>% 
  distinct()

IdInd2017 = BASE_2017F %>% 
  select(IDENTINDL) %>% 
  distinct()

IdInd2020 = BASE_2020F %>% 
  select(IDENTINDL) %>% 
  distinct()

View(IdInd2020)

# Les suivis sur les trois années : 4342 individus sur 2014, 2017 et 2020
Pan_Id = IdInd2014 %>% 
  inner_join(IdInd2017, by="IDENTINDL") %>% 
  inner_join(IdInd2020, by="IDENTINDL")

View(Pan_Id)

#********************* Variable poids individu longitidunal***********************************************************************************
#*************** Vérifier s'il y a de poids dans les bases 
# Vérifier l'année de sortie du PANEL : ANSORTIE_PANEL 2014 ou 2023
# Individu Panel ou NON : INDIV_PANEL
#* Le chamlps PANEL est restreint aux outres MER en 2014, reouvert aux territoires DOM à partir de 2020
#* Dans ce sens mles anlyses longitidunales sontb representatives uniquement de la France Metropolitaine
#*  Les menages ne sont pas suivi en PANEL, pour l'analyse transversale, c'est le menage l'unité statistique
#*  Pour l'analyse longitidunale, ce sont les individus 
#*  PANEL ROTATIF à partir de 2020, Split PANEL en 2014, enquêté une seule fois, l'autre moitié dans les 4 vagues d'interrrogations suivantes
#*  Année d'entrée de le PANEL : ANNEE
#********************************************************************************************************************************************

BASE_2014F$POND_MENAGE=BASE_2014F$POND
BASE_2014F$POND = NULL 

BASE_2017F$POND_MENAGE=BASE_2017F$pond_trans
BASE_2017F$pond_trans = NULL 

BASE_2020F$POND_MENAGE=BASE_2020F$POND_TRANS
BASE_2020F$POND_TRANS = NULL 

#****************** Fusion *********************************************************************
names(BASE_2014F)

BASE_2014F = BASE_2014F %>% 
  select("IDENT","POND_MENAGE","ANNEE_ENQUETE","IDENTINDL","LIVABL","LDD","LEP","MMLA_I","MMLDDS_I","MMLEP_I","NPERS","NENFANTS","NACTIFS",          
         "ACTIF","ETAMATRI","AGE_RELATIF_I","CATAGE_RELATIF_I","NIVEDUC_I","CS8_I","TYPEMPLOI","SITUA","STATUT","ZREVDEC_I", "decile_revenu_ind","decile_patrimoine",
         "PATFISOM","PATIMM","PATRIPRO","PATRIRES","MTDETTES","PATFINANC","CNDUR","DIEXP",           
         "DIF","AVI","MAVI","ASDEC","CEL","MCEL_I","PEL" ,"MPEL_I" ,"RETRAIT","MEPRETRAITE_I","EPSALA","MEPSALAR_I",       
         "MLEE_I","VMOB","MVMOB","ACTION" ,"PEA" ,"TERM" ,"MCOMPTERM","LIVJEUN" ,"MLIVJEUNE_I" ,"R_PATIMMO","R_RESPRIN","MRESPRIN",         
        "R_DETRE","DETIMM" ,"NBLOG", "NE" ,"DETPRO","HERTABH_I" ,"Heritage_I","HERTABD_I","Donation_R_I","CONSEP","RISKFI")

View(BASE_2014F)

BASE_2017F = BASE_2017F %>% 
  select("IDENT","POND_MENAGE","ANNEE_ENQUETE","IDENTINDL","LIVABL","LDD","LEP","MMLA_I","MMLDDS_I","MMLEP_I","NPERS","NENFANTS","NACTIFS",          
         "ACTIF","ETAMATRI","AGE_RELATIF_I","CATAGE_RELATIF_I","NIVEDUC_I","CS8_I","TYPEMPLOI","SITUA","STATUT","ZREVDEC_I", "decile_revenu_ind","decile_patrimoine",
         "PATFISOM","PATIMM","PATRIPRO","PATRIRES","MTDETTES","PATFINANC","CNDUR","DIEXP",           
         "DIF","AVI","MAVI","ASDEC","CEL","MCEL_I","PEL" ,"MPEL_I" ,"RETRAIT","MEPRETRAITE_I","EPSALA","MEPSALAR_I",       
         "MLEE_I","VMOB","MVMOB","ACTION" ,"PEA" ,"TERM" ,"MCOMPTERM","LIVJEUN" ,"MLIVJEUNE_I" ,"R_PATIMMO","R_RESPRIN","MRESPRIN",         
         "R_DETRE","DETIMM" ,"NBLOG", "NE" ,"DETPRO","HERTABH_I" ,"Heritage_I","HERTABD_I","Donation_R_I","CONSEP","RISKFI")

View(BASE_2017F)

BASE_2020F = BASE_2020F %>% 
  select("IDENT","POND_MENAGE","ANNEE_ENQUETE","IDENTINDL","LIVABL","LDD","LEP","MMLA_I","MMLDDS_I","MMLEP_I","NPERS","NENFANTS","NACTIFS",          
         "ACTIF","ETAMATRI","AGE_RELATIF_I","CATAGE_RELATIF_I","NIVEDUC_I","CS8_I","TYPEMPLOI","SITUA","STATUT","ZREVDEC_I", "decile_revenu_ind","decile_patrimoine",
         "PATFISOM","PATIMM","PATRIPRO","PATRIRES","MTDETTES","PATFINANC","CNDUR","DIEXP",           
         "DIF","AVI","MAVI","ASDEC","CEL","MCEL_I","PEL" ,"MPEL_I" ,"RETRAIT","MEPRETRAITE_I","EPSALA","MEPSALAR_I",       
         "MLEE_I","VMOB","MVMOB","ACTION" ,"PEA" ,"TERM" ,"MCOMPTERM","LIVJEUN" ,"MLIVJEUNE_I" ,"R_PATIMMO","R_RESPRIN","MRESPRIN",         
         "R_DETRE","DETIMM" ,"NBLOG", "NE" ,"DETPRO","HERTABH_I" ,"Heritage_I","HERTABD_I","Donation_R_I","CONSEP","RISKFI")

View(BASE_2020F)

mean(is.na(individu$POND_LONG_IND_2014)) * 100 

#************************* Vérifier si certianes indetifiant apparaissent plusieurs fois ************************************************
#* 2020
BASE_2020F %>%
  count(IDENTINDL) %>%
  filter(n > 1)

BASE_2020F %>%
  count(IDENTINDL) %>%
  filter(n > 1) %>%
  nrow()

#* Suppresion des lignes NA 
BASE_2020F = BASE_2020F %>% 
  filter(!is.na(IDENTINDL))

Doublons <- BASE_2020F %>%
  add_count(IDENTINDL) %>%
  filter(n > 1)

View(Doublons)

# Je garde l'observation ayant le poids d'enquete menage le plus important pour les cas ou 
# l'individu est dans plusieurs menage

BASE_2020F <- BASE_2020F %>%
  group_by(IDENTINDL) %>%
  slice_max(POND_MENAGE, n = 1, with_ties = FALSE) %>%
  ungroup()

# Vérifier 
Doublons <- BASE_2020F %>%
  add_count(IDENTINDL) %>%
  filter(n > 1)

View(Doublons)

# Verifier les autres vagues 
#* 2014
BASE_2014F %>%
  count(IDENTINDL) %>%
  filter(n > 1)

BASE_2014F %>%
  count(IDENTINDL) %>%
  filter(n > 1) %>%
  nrow()

#* Suppresion des lignes NA 
BASE_2014F = BASE_2014F %>% 
  filter(!is.na(IDENTINDL))

Doublons <- BASE_2014F %>%
  add_count(IDENTINDL) %>%
  filter(n > 1)

View(Doublons)

# Pas de soucis dans cette vague 

#* 2017
BASE_2017F %>%
  count(IDENTINDL) %>%
  filter(n > 1)

BASE_2017F %>%
  count(IDENTINDL) %>%
  filter(n > 1) %>%
  nrow()

#* Suppresion des lignes NA 
BASE_2017F = BASE_2017F %>% 
  filter(!is.na(IDENTINDL))

Doublons <- BASE_2017F %>%
  add_count(IDENTINDL) %>%
  filter(n > 1)

View(Doublons)

BASE_2017F <- BASE_2017F %>%
  group_by(IDENTINDL) %>%
  slice_max(POND_MENAGE, n = 1, with_ties = FALSE) %>%
  ungroup()

# Vérifier 
Doublons <- BASE_2017F %>%
  add_count(IDENTINDL) %>%
  filter(n > 1)

View(Doublons)

#************** Faire joindre la ponderation longitudinale 2014 pour exploiter tout le PANEL ************************ 
View(BASE_2014F)
View(BASE_2017F)
View(BASE_2020F)

#* Anticper les NA dans les diffrentes bases 
Id = individu %>% 
  select(IDENTINDL,POND_LONG_IND_2014)

View(Id)

# Fusion 
IdInd2014 = BASE_2014F %>% 
  select(IDENTINDL) %>% 
  distinct()

IdInd2017 = BASE_2017F %>% 
  select(IDENTINDL) %>% 
  distinct()

IdInd2020 = BASE_2020F %>% 
  select(IDENTINDL) %>% 
  distinct()

View(IdInd2020)

# 4341 Individus suivi sur les trois vagues 
Pan_Id = IdInd2014 %>% 
  inner_join(IdInd2017, by="IDENTINDL") %>% 
  inner_join(IdInd2020, by="IDENTINDL")

View(Pan_Id)

# Vérifcation du poids 
Pan_Id = Pan_Id %>% 
  left_join(Id,by="IDENTINDL")

View(Pan_Id)

mean(is.na(Pan_Id$POND_LONG_IND_2014)) * 100 

# So GOOD end of our suffreing time, let's go modeling 

# Garder dans chaque base que les individus suivi sur les trois vagues
#* 2014
BASE_2014F_PANEL = BASE_2014F %>% 
  inner_join(Pan_Id, by="IDENTINDL") 

View(BASE_2014F_PANEL)

#* 2017
BASE_2017F_PANEL = BASE_2017F %>% 
  inner_join(Pan_Id, by="IDENTINDL") 

View(BASE_2017F_PANEL)

#* 2020
BASE_2020F_PANEL = BASE_2020F %>% 
  inner_join(Pan_Id, by="IDENTINDL") 

View(BASE_2020F_PANEL)

#************************ 
BASE_2014F_PANEL %>% 
  filter(!is.na(POND_LONG_IND_2014)) %>% 
  summarise(n_distinct(IDENT),n_distinct(IDENTINDL))  # 5784 menages et 13872 individus

#************************ Append **************************************************************
BASE_ETUDE_FINAL <- bind_rows(BASE_2014F_PANEL,BASE_2017F_PANEL,BASE_2020F_PANEL)

View(BASE_ETUDE_FINAL)
dim(BASE_ETUDE_FINAL)

names(BASE_ETUDE_FINAL)

BASE_ETUDE_FINAL = BASE_ETUDE_FINAL %>% 
  select("IDENT","ANNEE_ENQUETE","IDENTINDL","POND_LONG_IND_2014","LIVABL","LDD","LEP","MMLA_I","MMLDDS_I","MMLEP_I","NPERS","NENFANTS","NACTIFS",          
         "ACTIF","ETAMATRI","AGE_RELATIF_I","CATAGE_RELATIF_I","NIVEDUC_I","CS8_I","TYPEMPLOI","SITUA","STATUT","ZREVDEC_I", "decile_revenu_ind","decile_patrimoine",
         "PATFISOM","PATIMM","PATRIPRO","PATRIRES","MTDETTES","PATFINANC","CNDUR","DIEXP",           
         "DIF","AVI","MAVI","ASDEC","CEL","MCEL_I","PEL" ,"MPEL_I" ,"RETRAIT","MEPRETRAITE_I","EPSALA","MEPSALAR_I",       
         "MLEE_I","VMOB","MVMOB","ACTION" ,"PEA" ,"TERM" ,"MCOMPTERM","LIVJEUN" ,"MLIVJEUNE_I" ,"R_PATIMMO","R_RESPRIN","MRESPRIN",         
         "R_DETRE","DETIMM" ,"NBLOG", "NE" ,"DETPRO","HERTABH_I" ,"Heritage_I","HERTABD_I","Donation_R_I","CONSEP","RISKFI")

dim(BASE_ETUDE_FINAL)
View(BASE_ETUDE_FINAL)

mean(is.na(BASE_ETUDE_FINAL$POND_LONG_IND_2014)) * 100 

# Les personnes suivies sur les trois vagues ne presentent pas de manquants de poids;

#********************  Base panel de tous les individus tenant compte de l'attrition **************
ID=individu %>% 
  select(IDENT, POND_TRANS, IDENTINDL, POND_LONG_IND_2014)

View(ID)

sum(duplicated(ID$IDENTINDL))

ID %>%
  count(IDENTINDL) %>%
  filter(n > 1)

ID %>% 
  filter(IDENTINDL %in% c(
    "114201420119556103",
    "114201420119556104",
    "117170535067677903",
    "117171484566532404"
  )) %>% 
  arrange(IDENTINDL)

# Meme principe, les individus appartenant à plusieurs ménages, les mettre dans le menage avec 
# le plus grand poids d'enquête

ID <- ID %>%
  group_by(IDENTINDL) %>%
  slice_max(POND_TRANS, n = 1, with_ties = FALSE) %>%
  ungroup()

#******** 2014 **********
BASE_2014F_PANEL1 <- BASE_2014F %>%
  left_join(ID %>% select(IDENTINDL, POND_LONG_IND_2014),
    by = "IDENTINDL")

View(BASE_2014F_PANEL1)

BASE_2014F_PANEL1 = BASE_2014F_PANEL1 %>% 
  filter(!is.na(POND_LONG_IND_2014))

nrow(BASE_2014F_PANEL1)
View(BASE_2014F_PANEL1)

nrow(BASE_2014F_PANEL)  # Je gagne des observations de plus 

#******** 2017 **********
BASE_2017F_PANEL1 <- BASE_2017F %>%
  left_join(ID %>% select(IDENTINDL, POND_LONG_IND_2014),
            by = "IDENTINDL")

View(BASE_2017F_PANEL1)

BASE_2017F_PANEL1 = BASE_2017F_PANEL1 %>% 
  filter(!is.na(POND_LONG_IND_2014))

nrow(BASE_2017F_PANEL1)  
View(BASE_2017F_PANEL1)

nrow(BASE_2017F_PANEL)  # Je gagne des observations de plus aussi

#******** 2020 **********
BASE_2020F_PANEL1 <- BASE_2020F %>%
  left_join(ID %>% select(IDENTINDL, POND_LONG_IND_2014),
            by = "IDENTINDL")

View(BASE_2020F_PANEL1)

BASE_2020F_PANEL1 = BASE_2020F_PANEL1 %>% 
  filter(!is.na(POND_LONG_IND_2014))

nrow(BASE_2020F_PANEL1)  
View(BASE_2020F_PANEL1)

nrow(BASE_2020F_PANEL)  # Je gagne des observations de plus aussi

#********************* BASE PANEL TENANT COMPTE DES ATTRITIONS ET ENTREES DANS LE PANEL *****************
BASE_ETUDE_FATTRI <- bind_rows(BASE_2014F_PANEL1,BASE_2017F_PANEL1,BASE_2020F_PANEL1)

View(BASE_ETUDE_FATTRI)
dim(BASE_ETUDE_FINAL) # Suivi sur les trois vagues : 13029 observations sur les trois vagues 
dim(BASE_ETUDE_FATTRI) # Prenant les entrées et sorties : 41979 observations sur les trois vagues

names(BASE_ETUDE_FATTRI)

BASE_ETUDE_FATTRI = BASE_ETUDE_FATTRI %>% 
  select("IDENT","ANNEE_ENQUETE","IDENTINDL","POND_LONG_IND_2014","LIVABL","LDD","LEP","MMLA_I","MMLDDS_I","MMLEP_I","NPERS","NENFANTS","NACTIFS",          
         "ACTIF","ETAMATRI","AGE_RELATIF_I","CATAGE_RELATIF_I","NIVEDUC_I","CS8_I","TYPEMPLOI","SITUA","STATUT","ZREVDEC_I", "decile_revenu_ind","decile_patrimoine",
         "PATFISOM","PATIMM","PATRIPRO","PATRIRES","MTDETTES","PATFINANC","CNDUR","DIEXP",           
         "DIF","AVI","MAVI","ASDEC","CEL","MCEL_I","PEL" ,"MPEL_I" ,"RETRAIT","MEPRETRAITE_I","EPSALA","MEPSALAR_I",       
         "MLEE_I","VMOB","MVMOB","ACTION" ,"PEA" ,"TERM" ,"MCOMPTERM","LIVJEUN" ,"MLIVJEUNE_I" ,"R_PATIMMO","R_RESPRIN","MRESPRIN",         
         "R_DETRE","DETIMM" ,"NBLOG", "NE" ,"DETPRO","HERTABH_I" ,"Heritage_I","HERTABD_I","Donation_R_I","CONSEP","RISKFI")

dim(BASE_ETUDE_FATTRI)
View(BASE_ETUDE_FATTRI)

mean(is.na(BASE_ETUDE_FATTRI$POND_LONG_IND_2014)) * 100 

#********************** Je ne toucherai plus aucunement à la base de donnée FINI *************************
names(BASE_ETUDE_FATTRI)

BASE_ETUDE_FATTRI %>% 
 # filter(ANNEE_ENQUETE) %>% 
  summarise(n_distinct(IDENT),n_distinct(IDENTINDL))  # 18402 menages et 23523 individus


#********************* Labelisation des qualitatives ****************************************************
unique(BASE_ETUDE_FATTRI$ACTIF)
BASE_RESERVE_PANEL = BASE_ETUDE_FATTRI
View(BASE_RESERVE_PANEL)

# LIVABL
BASE_ETUDE_FATTRI$LIVABL = ifelse(BASE_ETUDE_FATTRI$LIVABL == "1",1,0)
BASE_ETUDE_FATTRI$LIVABL = labelled(BASE_ETUDE_FATTRI$LIVABL,labels = c(Non=0,Oui=1))
BASE_ETUDE_FATTRI$LIVABL

# LDD
BASE_ETUDE_FATTRI$LDD = ifelse(BASE_ETUDE_FATTRI$LDD == "1",1,0)
BASE_ETUDE_FATTRI$LDD = labelled(BASE_ETUDE_FATTRI$LDD,labels = c(Non=0,Oui=1))
BASE_ETUDE_FATTRI$LDD

# LEP
BASE_ETUDE_FATTRI$LEP = ifelse(BASE_ETUDE_FATTRI$LEP == "1",1,0)
BASE_ETUDE_FATTRI$LEP = labelled(BASE_ETUDE_FATTRI$LEP,labels = c(Non=0,Oui=1))
BASE_ETUDE_FATTRI$LEP

# ACTIF 
BASE_ETUDE_FATTRI$ACTIF = ifelse(BASE_ETUDE_FATTRI$ACTIF == "1",1,0)
BASE_ETUDE_FATTRI$ACTIF = labelled(BASE_ETUDE_FATTRI$ACTIF,labels = c(Non=0,Oui=1))
BASE_ETUDE_FATTRI$ACTIF

# ETAMATRI
BASE_ETUDE_FATTRI$ETAMATRI = factor(BASE_ETUDE_FATTRI$ETAMATRI,
                                    levels = c(1,2,3,4), labels = c("Célibataire","Marié","Veuf","Divorcé"))
BASE_ETUDE_FATTRI$ETAMATRI

# CATAGE_RELATIF_I
BASE_ETUDE_FATTRI$CATAGE_RELATIF_I = factor(BASE_ETUDE_FATTRI$CATAGE_RELATIF_I,
                                    levels = c(1,2,3,4,5,6), 
                                    labels = c("0 à 14 ans","15 à 29 ans","30 à 44 ans",
                                               "45 à 59 ans","60 à 74 ans","75 ans ou plus"))
BASE_ETUDE_FATTRI$CATAGE_RELATIF_I

# NIVEDUC_I
BASE_ETUDE_FATTRI$NIVEDUC_I = factor(BASE_ETUDE_FATTRI$NIVEDUC_I,
                                            levels = c(0,1,2,3,4,5,6,7,8), 
                                            labels = c("Non dclaré","Sans diplome ou CEP","brevet",
                                                       "bac ou bac+1","bac+2","bac+3","bac+4",
                                                       "bac+5","bac+8"))
# CS8_I
BASE_ETUDE_FATTRI$CS8_I = factor(BASE_ETUDE_FATTRI$CS8_I,
                                     levels = c(0,1,2,3,4,5,6,7,8), 
                                     labels = c("Non dclaré","Agriculteurs exploitants ","Artisans, commerçants et chefs d'entreprise",
                                                "Cadres et professions intellectuelles supérieures","Professions Intermédiaires","Employés",
                                                "Ouvriers","Retraités","Autres personnes sans activité professionnelle"))

View(BASE_RESERVE_PANEL)

# TYPEMPLOI
unique(BASE_ETUDE_FATTRI$TYPEMPLOI)

BASE_ETUDE_FATTRI$TYPEMPLOI[BASE_ETUDE_FATTRI$TYPEMPLOI == ""] <- "0"

BASE_ETUDE_FATTRI$TYPEMPLOI = 
  factor(BASE_ETUDE_FATTRI$TYPEMPLOI,
                                 levels = c(0,1,2,3,4,5,6), 
                                 labels = c("Non dclaré","Apprenti","Intérim",
                                            "Stagiaire","Contrat aidé (CUI, CAE, CIE)","CDD",
                                            "CDI et Fonctionnaire"))
BASE_ETUDE_FATTRI$TYPEMPLOI

# SITUA
unique(BASE_ETUDE_FATTRI$SITUA)

BASE_ETUDE_FATTRI$SITUA[BASE_ETUDE_FATTRI$SITUA == "moins de 15-16 ans"] <- "0"

BASE_ETUDE_FATTRI$SITUA = 
  factor(BASE_ETUDE_FATTRI$SITUA,
         levels = c(0,1,2,3,4,5,6,7,8), 
         labels = c("moins de 15-16 ans","Employé","Apprenti/stagiaire",
                    "Etudiant/Elève","Chômeur","Rétraité",
                    "Homme au foyer","Invalidité","Inactif"))

BASE_ETUDE_FATTRI$SITUA

# STATUT
unique(BASE_ETUDE_FATTRI$STATUT)

BASE_ETUDE_FATTRI$STATUT[BASE_ETUDE_FATTRI$STATUT == "moins de 15-16 ans"] <- "0"

BASE_ETUDE_FATTRI$STATUT = 
  factor(BASE_ETUDE_FATTRI$STATUT,
         levels = c(0,1,2,3,4,5,6,7,8), 
         labels = c("moins de 15-16 ans","Salarié de l'Etat","Salarié collectivité territoriale",
                    "Salarié entreprise, artisan, association","Salarié d’un particulier","Aide non remunéré au membre famille",
                    "PDG/Gérant","Indépendant","Salarié secteur médical"))

BASE_ETUDE_FATTRI$STATUT

class(BASE_ETUDE_FATTRI$decile_patrimoine)

# décile_revenu_ind
class(BASE_ETUDE_FATTRI$decile_revenu_ind)

BASE_ETUDE_FATTRI$decile_revenu_ind <- factor(BASE_ETUDE_FATTRI$decile_revenu_ind,levels = 1:7,ordered = TRUE)

unique(BASE_ETUDE_FATTRI$decile_revenu_ind)

# decile_patrimoine
class(BASE_ETUDE_FATTRI$decile_patrimoine)

BASE_ETUDE_FATTRI$decile_patrimoine <- factor(BASE_ETUDE_FATTRI$decile_patrimoine,levels = 1:10,ordered = TRUE)

unique(BASE_ETUDE_FATTRI$decile_patrimoine)

# PATFINANC
unique(BASE_ETUDE_FATTRI$PATFINANC)

BASE_ETUDE_FATTRI$PATFINANC = ifelse(BASE_ETUDE_FATTRI$PATFINANC == "1",1,0)
BASE_ETUDE_FATTRI$PATFINANC = labelled(BASE_ETUDE_FATTRI$PATFINANC,labels = c(Non=0,Oui=1))
BASE_ETUDE_FATTRI$PATFINANC

# DIEXP
unique(BASE_ETUDE_FATTRI$DIEXP)

BASE_ETUDE_FATTRI$DIEXP[BASE_ETUDE_FATTRI$DIEXP == ""] <- "0"

BASE_ETUDE_FATTRI$DIEXP = 
  factor(BASE_ETUDE_FATTRI$DIEXP,
         levels = c(0,1,2,3), 
         labels = c("Aucun","Augmentation PA","Diminution PA","Identique PA"))

BASE_ETUDE_FATTRI$DIEXP

# DIF
unique(BASE_ETUDE_FATTRI$DIF)

BASE_ETUDE_FATTRI$DIF[BASE_ETUDE_FATTRI$DIF == ""] <- "0"

BASE_ETUDE_FATTRI$DIF = 
  factor(BASE_ETUDE_FATTRI$DIF,
         levels = c(0,1,2,3,4,5), 
         labels = c("Non déclaré","Jamais arrivé","Arrivé récemment",
                    "Déja arrivé","Depuis plusieurs années","Depuis moins d'un an"))

BASE_ETUDE_FATTRI$DIF

# AVI
unique(BASE_ETUDE_FATTRI$AVI)

BASE_ETUDE_FATTRI$AVI = ifelse(BASE_ETUDE_FATTRI$AVI == "1",1,0)
BASE_ETUDE_FATTRI$AVI = labelled(BASE_ETUDE_FATTRI$AVI,labels = c(Non=0,Oui=1))
BASE_ETUDE_FATTRI$AVI

# ASDEC
unique(BASE_ETUDE_FATTRI$ASDEC)

BASE_ETUDE_FATTRI$ASDEC = ifelse(BASE_ETUDE_FATTRI$ASDEC == "1",1,0)
BASE_ETUDE_FATTRI$ASDEC = labelled(BASE_ETUDE_FATTRI$ASDEC,labels = c(Non=0,Oui=1))
BASE_ETUDE_FATTRI$ASDEC

# CEL
unique(BASE_ETUDE_FATTRI$CEL)

BASE_ETUDE_FATTRI$CEL = ifelse(BASE_ETUDE_FATTRI$CEL == "1",1,0)
BASE_ETUDE_FATTRI$CEL = labelled(BASE_ETUDE_FATTRI$CEL,labels = c(Non=0,Oui=1))
BASE_ETUDE_FATTRI$CEL

# PEL
unique(BASE_ETUDE_FATTRI$PEL)

BASE_ETUDE_FATTRI$PEL = ifelse(BASE_ETUDE_FATTRI$PEL == "1",1,0)
BASE_ETUDE_FATTRI$PEL = labelled(BASE_ETUDE_FATTRI$PEL,labels = c(Non=0,Oui=1))
BASE_ETUDE_FATTRI$PEL

# RETRAIT
unique(BASE_ETUDE_FATTRI$RETRAIT)

BASE_ETUDE_FATTRI$RETRAIT = ifelse(BASE_ETUDE_FATTRI$RETRAIT == "1",1,0)
BASE_ETUDE_FATTRI$RETRAIT = labelled(BASE_ETUDE_FATTRI$RETRAIT,labels = c(Non=0,Oui=1))
BASE_ETUDE_FATTRI$RETRAIT

# EPSALA
unique(BASE_ETUDE_FATTRI$EPSALA)

BASE_ETUDE_FATTRI$EPSALA = ifelse(BASE_ETUDE_FATTRI$EPSALA == "1",1,0)
BASE_ETUDE_FATTRI$EPSALA = labelled(BASE_ETUDE_FATTRI$EPSALA,labels = c(Non=0,Oui=1))
BASE_ETUDE_FATTRI$EPSALA

# VMOB
unique(BASE_ETUDE_FATTRI$VMOB)

BASE_ETUDE_FATTRI$VMOB = ifelse(BASE_ETUDE_FATTRI$VMOB == "1",1,0)
BASE_ETUDE_FATTRI$VMOB = labelled(BASE_ETUDE_FATTRI$VMOB,labels = c(Non=0,Oui=1))
BASE_ETUDE_FATTRI$VMOB

# ACTION
unique(BASE_ETUDE_FATTRI$ACTION)

BASE_ETUDE_FATTRI$ACTION = ifelse(BASE_ETUDE_FATTRI$ACTION == "1",1,0)
BASE_ETUDE_FATTRI$ACTION = labelled(BASE_ETUDE_FATTRI$ACTION,labels = c(Non=0,Oui=1))
BASE_ETUDE_FATTRI$ACTION

# PEA
unique(BASE_ETUDE_FATTRI$PEA)

BASE_ETUDE_FATTRI$PEA = ifelse(BASE_ETUDE_FATTRI$PEA == "1",1,0)
BASE_ETUDE_FATTRI$PEA = labelled(BASE_ETUDE_FATTRI$PEA,labels = c(Non=0,Oui=1))
BASE_ETUDE_FATTRI$PEA

# TERM
unique(BASE_ETUDE_FATTRI$TERM)

BASE_ETUDE_FATTRI$TERM = ifelse(BASE_ETUDE_FATTRI$TERM == "1",1,0)
BASE_ETUDE_FATTRI$TERM = labelled(BASE_ETUDE_FATTRI$TERM,labels = c(Non=0,Oui=1))
BASE_ETUDE_FATTRI$TERM

# LIVJEUN
unique(BASE_ETUDE_FATTRI$LIVJEUN)

BASE_ETUDE_FATTRI$LIVJEUN = ifelse(BASE_ETUDE_FATTRI$LIVJEUN == "1",1,0)
BASE_ETUDE_FATTRI$LIVJEUN = labelled(BASE_ETUDE_FATTRI$LIVJEUN,labels = c(Non=0,Oui=1))
BASE_ETUDE_FATTRI$LIVJEUN

# R_PATIMMO
unique(BASE_ETUDE_FATTRI$R_PATIMMO)

BASE_ETUDE_FATTRI$R_PATIMMO = ifelse(BASE_ETUDE_FATTRI$R_PATIMMO == "1",1,0)
BASE_ETUDE_FATTRI$R_PATIMMO = labelled(BASE_ETUDE_FATTRI$R_PATIMMO,labels = c(Non=0,Oui=1))
BASE_ETUDE_FATTRI$R_PATIMMO

# R_RESPRIN
unique(BASE_ETUDE_FATTRI$R_RESPRIN)

BASE_ETUDE_FATTRI$R_RESPRIN = ifelse(BASE_ETUDE_FATTRI$R_RESPRIN == "1",1,0)
BASE_ETUDE_FATTRI$R_RESPRIN = labelled(BASE_ETUDE_FATTRI$R_RESPRIN,labels = c(Non=0,Oui=1))
BASE_ETUDE_FATTRI$R_RESPRIN

# R_DETRE
unique(BASE_ETUDE_FATTRI$R_DETRE)

BASE_ETUDE_FATTRI$R_DETRE = ifelse(BASE_ETUDE_FATTRI$R_DETRE == "1",1,0)
BASE_ETUDE_FATTRI$R_DETRE = labelled(BASE_ETUDE_FATTRI$R_DETRE,labels = c(Non=0,Oui=1))
BASE_ETUDE_FATTRI$R_DETRE

# DETIMM
unique(BASE_ETUDE_FATTRI$DETIMM)

BASE_ETUDE_FATTRI$DETIMM = ifelse(BASE_ETUDE_FATTRI$DETIMM == "1",1,0)
BASE_ETUDE_FATTRI$DETIMM = labelled(BASE_ETUDE_FATTRI$DETIMM,labels = c(Non=0,Oui=1))
BASE_ETUDE_FATTRI$DETIMM

# DETPRO
unique(BASE_ETUDE_FATTRI$DETPRO)

BASE_ETUDE_FATTRI$DETPRO = ifelse(BASE_ETUDE_FATTRI$DETPRO == "1",1,0)
BASE_ETUDE_FATTRI$DETPRO = labelled(BASE_ETUDE_FATTRI$DETPRO,labels = c(Non=0,Oui=1))
BASE_ETUDE_FATTRI$DETPRO

# HERTABH_I
unique(BASE_ETUDE_FATTRI$HERTABH_I)

BASE_ETUDE_FATTRI$HERTABH_I[BASE_ETUDE_FATTRI$HERTABH_I == ""] <- "2"

BASE_ETUDE_FATTRI$HERTABH_I = ifelse(BASE_ETUDE_FATTRI$HERTABH_I == "1",1,0)
BASE_ETUDE_FATTRI$HERTABH_I = labelled(BASE_ETUDE_FATTRI$HERTABH_I,labels = c(Non=0,Oui=1))
BASE_ETUDE_FATTRI$HERTABH_I

# Heritage_I
unique(BASE_ETUDE_FATTRI$HERTABD_I)

BASE_ETUDE_FATTRI$HERTABD_I[BASE_ETUDE_FATTRI$HERTABD_I == ""] <- "2"

BASE_ETUDE_FATTRI$HERTABD_I = ifelse(BASE_ETUDE_FATTRI$HERTABD_I == "1",1,0)
BASE_ETUDE_FATTRI$HERTABD_I = labelled(BASE_ETUDE_FATTRI$HERTABD_I,labels = c(Non=0,Oui=1))
BASE_ETUDE_FATTRI$HERTABD_I

# CONSEP
unique(BASE_ETUDE_FATTRI$CONSEP)

BASE_ETUDE_FATTRI$CONSEP[BASE_ETUDE_FATTRI$CONSEP == ""] <- "00"

BASE_ETUDE_FATTRI$CONSEP <- factor(
  BASE_ETUDE_FATTRI$CONSEP,
  levels = c("00","01","02","03","04","05","06","07","08","09","10","11","12","13"),
  labels = c(
    "Non déclaré",
    "Achat résidence principale",
    "Autres achats (Mobilier, Véhicules...)",
    "Créer/Investir une entreprise",
    "Actifs financiers",
    "Précaution pour imprévus",
    "Remboursement",
    "Retraite",
    "Voyage, vacances",
    "Frais d'études",
    "Succession",
    "Investir pour aides publiques",
    "Autres",
    "N'épargne pas"))

BASE_ETUDE_FATTRI$CONSEP

# RISKFI
unique(BASE_ETUDE_FATTRI$RISKFI)

BASE_ETUDE_FATTRI$RISKFI[BASE_ETUDE_FATTRI$RISKFI == ""] <- "0"

BASE_ETUDE_FATTRI$RISKFI = 
  factor(BASE_ETUDE_FATTRI$RISKFI,
         levels = c(0,1,2,3,4), 
         labels = c("Non déclaré","Risque élevé","Risque modéré à élevé",
                    "Risque moyen","Sans risque financier"))

BASE_ETUDE_FATTRI$RISKFI

# Fin du travail sur les bases de données

#********************* Modelisation PANEL *****************************************************
View(BASE_ETUDE_FATTRI)
dim(BASE_ETUDE_FATTRI)

# Type des variables
BASE_ETUDE_FATTRI %>% str

# Un panel de 41,979 observations sur 2014-2017-2020

#******** Combien d'individu et de ménage dans la base finale **********************************
resultat <- BASE_ETUDE_FATTRI %>%
  distinct(IDENTINDL, ANNEE_ENQUETE) %>%
  group_by(IDENTINDL) %>%
  summarise(
    premiere_annee = min(ANNEE_ENQUETE),
    derniere_annee = max(ANNEE_ENQUETE),
    .groups = "drop")

entrants <- resultat %>%
  count(premiere_annee, name = "nb_entrants")

sortants <- resultat %>%
  count(derniere_annee, name = "nb_sortants")

full_join(entrants,sortants,by = c("premiere_annee" = "derniere_annee")) %>%
  rename(ANNEE_ENQUETE = premiere_annee)

BASE_ETUDE_FATTRI %>% 
  group_by(ANNEE_ENQUETE) %>%
  summarise(nb_individus=n_distinct(IDENTINDL))

View(BASE_ETUDE_FATTRI)

# Je ne suis pas sur un Panel cylindré, c'est un Panel rotatif ou panel non équilibré (unbalanced panel):
# 4396 individus sint suivis en 2014,dans ce lot 37 personnes sortent en 2017, 9739 entrants dans le PANEL en 2017
# Tout le monde sera suivi en 2020 avec 9388 nouveaux individus qui entrent dans le PANEL 2020

# A voir après 
# Tester la multicolinearité (VIF) :

# Pour choisir le modele pour chaque dependante : Effet fixe et effet aleatoire, test de Hausman (lineaire) pour specificié
## Quantitative : Effet fixe et effet aleatoire, puis Hausman (Choix du modele) puis réestimer avec erreur clusterisé
## Binaire : Procedure 

## Test de robustesse : Hausman, Test d'autocorelation de Woolridge et des tests de Brusch Pagan et White 

# Learning by doing 

#********************** Correction de la variable âge et separation des bases : majeures, et personnes références des ménages *******************
View(BASE_ETUDE_FATTRI)

V=individu %>% 
  filter(IDENTINDL %in% "114201400008303101") %>% 
  select(ANAIS)

View(V)

#***** Correction de l'âge des personnes *******
BASE_ETUDE_FATTRI <- BASE_ETUDE_FATTRI %>%
  mutate(
    AGE_RELATIF_I = case_when(
      ANNEE_ENQUETE == 2014 ~ AGE_RELATIF_I - 6,
      ANNEE_ENQUETE == 2017 ~ AGE_RELATIF_I - 3,
      TRUE ~ AGE_RELATIF_I))

# Correction des CATAGE_RELATIF_I
unique(BASE_ETUDE_FATTRI$CATAGE_RELATIF_I)

View(BASE_ETUDE_FATTRI)
BASE_ETUDE_FATTRI$CATAGE_RELATIF=NA

BASE_ETUDE_FATTRI$CATAGE_RELATIF[BASE_ETUDE_FATTRI$AGE_RELATIF_I <= 14] = 1                              #0 à 14 ans
BASE_ETUDE_FATTRI$CATAGE_RELATIF[BASE_ETUDE_FATTRI$AGE_RELATIF_I > 14 & BASE_ETUDE_FATTRI$AGE_RELATIF_I <=29] = 2 #15 à 29 ans
BASE_ETUDE_FATTRI$CATAGE_RELATIF[BASE_ETUDE_FATTRI$AGE_RELATIF_I > 29 & BASE_ETUDE_FATTRI$AGE_RELATIF_I <=44] = 3 #30 à 44 ans
BASE_ETUDE_FATTRI$CATAGE_RELATIF[BASE_ETUDE_FATTRI$AGE_RELATIF_I > 44 & BASE_ETUDE_FATTRI$AGE_RELATIF_I <=59] = 4 #45 à 59 ans
BASE_ETUDE_FATTRI$CATAGE_RELATIF[BASE_ETUDE_FATTRI$AGE_RELATIF_I > 59 & BASE_ETUDE_FATTRI$AGE_RELATIF_I <=74] = 5 #60 à 74 ans
BASE_ETUDE_FATTRI$CATAGE_RELATIF[BASE_ETUDE_FATTRI$AGE_RELATIF_I > 74] = 6                               #75 ans ou plus 

BASE_ETUDE_FATTRI$CATAGE_RELATIF_I = BASE_ETUDE_FATTRI$CATAGE_RELATIF

BASE_ETUDE_FATTRI$CATAGE_RELATIF = NULL 

unique(BASE_ETUDE_FATTRI$CATAGE_RELATIF_I)
mean(is.na(BASE_ETUDE_FATTRI$CATAGE_RELATIF_I)) * 100 

View(BASE_ETUDE_FATTRI)

# ***** Faire revenir la varibale car détruit par erreur ****************************
BASE_ETUDE_FATTRI <- BASE_ETUDE_FATTRI %>%
  left_join(
    BASE_RESERVE_PANEL %>%
      select(IDENTINDL, ANNEE_ENQUETE, CS8_I),
    by = c("IDENTINDL", "ANNEE_ENQUETE"))

unique(BASE_ETUDE_FATTRI$CS8_I)

#*** Création d'une nouvelle variable pour la CSP ***********************************
unique(individu$CYACT1)

mean(is.na(individu_t$CYACT1) | individu_t$CYACT1 == "" & individu_t$AGE_RELATIF >= 18 ) *100 # 35.47% de non déclaré
mean(is.na(individu_2014$CYACT1) | individu_2014$CYACT1 == "" & individu_2014$AGE_RELATIF >=18 ) *100 # 35.47% de non déclaré
mean(is.na(individu_2017$CYACT1) | individu_2017$CYACT1 == "" & individu_2017$AGE_RELATIF >=18 ) *100 # 35.47% de non déclaré

individu_2014$AGE_RELATIF = 2015-individu_2014$ANAIS

individu_t$STATUT_ACT <- dplyr::case_when(
  individu_t$CYACT1 %in% c("01", "12") ~ "Etudiant/formation",
  individu_t$CYACT1 %in% c("02", "03", "04", "05", "06") ~ "Salarié",
  individu_t$CYACT1 %in% c("07", "08") ~ "Independant",
  individu_t$CYACT1 %in% c("09", "10") ~ "Chômeur",
  individu_t$CYACT1 == "11" ~ "Emploi precaire",
  individu_t$CYACT1 == "13" ~ "Service national",
  individu_t$CYACT1 == "14" ~ "Retraité",
  individu_t$CYACT1 == "15" ~ "Inactif",
  TRUE ~ "Non déclaré")

mean(is.na(individu_t$STATUT_ACT)) * 100
View(individu_t)

individu_2014$STATUT_ACT <- dplyr::case_when(
  individu_2014$CYACT1 %in% c("01", "12") ~ "Etudiant/formation",
  individu_2014$CYACT1 %in% c("02", "03", "04", "05", "06") ~ "Salarié",
  individu_2014$CYACT1 %in% c("07", "08") ~ "Independant",
  individu_2014$CYACT1 %in% c("09", "10") ~ "Chômeur",
  individu_2014$CYACT1 == "11" ~ "Emploi precaire",
  individu_2014$CYACT1 == "13" ~ "Service national",
  individu_2014$CYACT1 == "14" ~ "Retraité",
  individu_2014$CYACT1 == "15" ~ "Inactif",
  TRUE ~ "Non déclaré")

mean(is.na(individu_2014$STATUT_ACT)) * 100
View(individu_2014)

individu_2017$STATUT_ACT <- dplyr::case_when(
  individu_2017$CYACT1 %in% c("01", "12") ~ "Etudiant/formation",
  individu_2017$CYACT1 %in% c("02", "03", "04", "05", "06") ~ "Salarié",
  individu_2017$CYACT1 %in% c("07", "08") ~ "Independant",
  individu_2017$CYACT1 %in% c("09", "10") ~ "Chômeur",
  individu_2017$CYACT1 == "11" ~ "Emploi precaire",
  individu_2017$CYACT1 == "13" ~ "Service national",
  individu_2017$CYACT1 == "14" ~ "Retraité",
  individu_2017$CYACT1 == "15" ~ "Inactif",
  TRUE ~ "Non déclaré")

mean(is.na(individu_2017$STATUT_ACT)) * 100
View(individu_2017)

# *** Ramener à la base actuelle *****
base_statut <- bind_rows(
  individu_2014 %>%
    select(IDENTINDL, STATUT_ACT) %>%
    mutate(ANNEE_ENQUETE = 2014),
  
  individu_2017 %>%
    select(IDENTINDL, STATUT_ACT) %>%
    mutate(ANNEE_ENQUETE = 2017),
  
  individu_t %>%
    select(IDENTINDL, STATUT_ACT) %>%
    mutate(ANNEE_ENQUETE = l2020))

View(base_statut)

BASE_ETUDE_FATTRI <- BASE_ETUDE_FATTRI %>%
  left_join(
    base_statut %>%
      select(ANNEE_ENQUETE, IDENTINDL, STATUT_ACT),
    by = c("ANNEE_ENQUETE", "IDENTINDL"))

View(BASE_ETUDE_FATTRI)


# ******** Garder les personnes majeures ********************************************************************************************************
# Identifiants des individus majeurs en 2014
id_18_2014 <- BASE_ETUDE_FATTRI %>%
  filter(ANNEE_ENQUETE == 2014, AGE_RELATIF_I >= 18) %>%
  pull(IDENTINDL)

id_18_2017 <- BASE_ETUDE_FATTRI %>%
  filter(ANNEE_ENQUETE == 2017, AGE_RELATIF_I >= 18) %>%
  pull(IDENTINDL)

id_18_2020 <- BASE_ETUDE_FATTRI %>%
  filter(ANNEE_ENQUETE == 2020, AGE_RELATIF_I >= 18) %>%
  pull(IDENTINDL)

# Conservation de toutes leurs observations
BASE_ETUDE_FATTRI <- BASE_ETUDE_FATTRI %>%
  filter(IDENTINDL %in% id_18_2014)

id_18 <- unique(c(
  id_18_2014,
  id_18_2017,
  id_18_2020
))

BASE_ETUDE_FATTRI <- BASE_ETUDE_FATTRI %>%
  filter(IDENTINDL %in% id_18)

View(BASE_ETUDE_FATTRI)

View(BASE_RESERVE_PANEL)

# Pas efficace donc faire ceci : AGE_RELATIF > = 18
BASE_ETUDE_FATTRI <- BASE_ETUDE_FATTRI %>%
  filter(AGE_RELATIF_I >=18)

View(BASE_ETUDE_FATTRI)

# ***** Vérifier les doublons *****************
doublons <- BASE_ETUDE_FATTRI %>%
  count(ANNEE_ENQUETE, IDENTINDL) %>%
  filter(n > 1)

V=BASE_ETUDE_FATTRI %>%
  semi_join(
    doublons,
    by = c("ANNEE_ENQUETE", "IDENTINDL")
  ) %>%
  arrange(IDENTINDL, ANNEE_ENQUETE)

View(V)

#*** Enlver les doublons ***
BASE_ETUDE_FATTRI <- BASE_ETUDE_FATTRI %>%
  distinct(ANNEE_ENQUETE, IDENTINDL, .keep_all = TRUE)

View(BASE_ETUDE_FATTRI %>%
  count(ANNEE_ENQUETE, IDENTINDL) %>%
  filter(n > 1))

# *** Proportion de non déclaré parmi les plus de 18 ans sur les variables dans la base finale ******************
mean(BASE_ETUDE_FATTRI$CS8_I == "Non dclaré") * 100
mean(BASE_ETUDE_FATTRI$STATUT_ACT == "Non déclaré") * 100
mean(BASE_ETUDE_FATTRI$SITUA == "Non déclaré") * 100
mean(BASE_ETUDE_FATTRI$NIVEDUC_I == "Non dclaré") * 100

unique(BASE_ETUDE_FATTRI$SITUA)
nrow(BASE_ETUDE_FATTRI)

V=BASE_ETUDE_FATTRI %>% 
  filter(BASE_ETUDE_FATTRI$NIVEDUC_I == "Non dclaré" | BASE_ETUDE_FATTRI$CS8_I == "Non dclaré")

View(V)

# La CSP est la meilleure car moins de declaré, je peux même les supprimées
BASE_ETUDE_FATTRI$STATUT = NULL 

# Corriger CS8_I et NIVEDUC_I 
id_non_declares <- BASE_ETUDE_FATTRI %>%
  filter(NIVEDUC_I == "Non dclaré") %>%
  pull(IDENTINDL) %>%
  unique()

V=BASE_ETUDE_FATTRI %>%
  filter(IDENTINDL %in% id_non_declares) %>%
  arrange(IDENTINDL, ANNEE_ENQUETE)

View(V)

#***** Imputation longitidunale ************
BASE_ETUDE_FATTRI1 <- BASE_ETUDE_FATTRI %>%
arrange(IDENTINDL, ANNEE_ENQUETE) %>%
  group_by(IDENTINDL) %>%
  mutate(NIVEDUC_I = ifelse(NIVEDUC_I == "Non dclaré",
                       NA,as.character(NIVEDUC_I))) %>%
  fill(NIVEDUC_I, .direction = "down") %>%
  ungroup()

mean(is.na(BASE_ETUDE_FATTRI1$NIVEDUC_I)) * 100

View(BASE_ETUDE_FATTRI1)
# Exemple de celui ci : ça marche 114201402843693101

# BASE_ETUDE_FATTRI2 <- BASE_ETUDE_FATTRI1 %>%
#   arrange(IDENTINDL, ANNEE_ENQUETE) %>%
#   group_by(IDENTINDL) %>%
#   fill(NIVEDUC_I, .direction = "up") %>%
#   ungroup()


sum(is.na(BASE_ETUDE_FATTRI2$NIVEDUC_I))

id_non_declares <- BASE_ETUDE_FATTRI1 %>%
  filter(is.na(NIVEDUC_I)) %>%
  pull(IDENTINDL) %>%
  unique()

V=BASE_ETUDE_FATTRI1 %>%
  filter(IDENTINDL %in% id_non_declares) %>%
  arrange(IDENTINDL, ANNEE_ENQUETE)

View(V)   # Juste 5 observations 

# Amputer par la mediane des 

# Mode de NIVEDUC_I par âge
mode_age <- BASE_ETUDE_FATTRI1 %>%
  filter(!is.na(NIVEDUC_I)) %>%
  count(AGE_RELATIF_I, NIVEDUC_I) %>%
  group_by(AGE_RELATIF_I) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(AGE_RELATIF_I, NIVEDUC_mode = NIVEDUC_I)

View(mode_age)

# Imputation
BASE_ETUDE_FATTRI2 <- BASE_ETUDE_FATTRI2 %>%
  left_join(mode_age, by = "AGE_RELATIF_I") %>%
  mutate(
    NIVEDUC_I = ifelse(
      is.na(NIVEDUC_I),
      NIVEDUC_mode,
      NIVEDUC_I)) %>%
  select(-NIVEDUC_mode)

View(BASE_ETUDE_FATTRI2)

#***** Imputation longitidunale  ************
BASE_ETUDE_FATTRI3 <- BASE_ETUDE_FATTRI2 %>%
  arrange(IDENTINDL, ANNEE_ENQUETE) %>%
  group_by(IDENTINDL) %>%
  mutate(CS8_I = ifelse(CS8_I == "Non dclaré",
                            NA,as.character(CS8_I))) %>%
  fill(CS8_I, .direction = "down") %>%
  ungroup()

mean(is.na(BASE_ETUDE_FATTRI3$CS8_I)) * 100

# Exemple de celui ci : ça marche 114201402843693101

BASE_ETUDE_FATTRI4 <- BASE_ETUDE_FATTRI3 %>%
  arrange(IDENTINDL, ANNEE_ENQUETE) %>%
  group_by(IDENTINDL) %>%
  fill(CS8_I, .direction = "up") %>%
  ungroup()

sum(is.na(BASE_ETUDE_FATTRI4$CS8_I)) 

id_non_declares <- BASE_ETUDE_FATTRI4 %>%
  filter(is.na(CS8_I)) %>%
  pull(IDENTINDL) %>%
  unique()

V=BASE_ETUDE_FATTRI4 %>%
  filter(IDENTINDL %in% id_non_declares) %>%
  arrange(IDENTINDL, ANNEE_ENQUETE)

View(V)   # Juste 39 observations 

# Amputer par la mediane des 

# Mode de CS8_I par âge
mode_age <- BASE_ETUDE_FATTRI4 %>%
  filter(!is.na(CS8_I)) %>%
  count(AGE_RELATIF_I, CS8_I) %>%
  group_by(AGE_RELATIF_I) %>%
  arrange(desc(n)) %>%
  slice(1) %>%
  ungroup() %>%
  rename(CS8_I_mode = CS8_I)

View(mode_age)

# Imputation
BASE_ETUDE_FATTRI5 <- BASE_ETUDE_FATTRI4 %>%
  left_join(mode_age, by = "AGE_RELATIF_I") %>%
  mutate(CS8_I = ifelse(is.na(CS8_I),CS8_I_mode,CS8_I)) %>% select(-CS8_I_mode)

mean(is.na(BASE_ETUDE_FATTRI5$CS8_I))

# ******
class(BASE_ETUDE_FATTRI5)

unique(BASE_ETUDE_FATTRI5$CATAGE_RELATIF_I)

# *** Correction des noms des modalités Catage_relatif ********************

View(BASE_ETUDE_FATTRI5) # Pour vérifier : 114201400030089101

BASE_ETUDE_FATTRI5$CATAGE_RELATIF_I <- dplyr::recode(BASE_ETUDE_FATTRI5$CATAGE_RELATIF_I,
  "15 à 29 ans" = "18 à 29 ans")

# *** Regrouper le niveau d'éducation en LMD *****************************
unique(BASE_ETUDE_FATTRI5$NIVEDUC_I)

BASE_ETUDE_FATTRI5$NIVEDUC_LMD <- dplyr::recode(
  BASE_ETUDE_FATTRI5$NIVEDUC_I,
  "Sans diplome ou CEP" = "Sans diplome ou brevet",
  "brevet"              = "Sans diplome ou brevet",
  "bac ou bac+1"        = "Bac",
  "bac+2"               = "Bac+2",
  "bac+3"               = "Licence (L)",
  "bac+4"               = "Master (M)",
  "bac+5"               = "Master (M)",
  "bac+8"               = "Doctorat (D)")

View(BASE_ETUDE_FATTRI5)

# *** Je passe au recodage finale du NIVEDUC_I *******
BASE_ETUDE_FATTRI5$NIVEDUC_LMDF <- dplyr::recode(
  BASE_ETUDE_FATTRI5$NIVEDUC_I,
  "Sans diplome ou CEP" = "Sans diplome ou brevet",
  "brevet"              = "Sans diplome ou brevet",
  "bac ou bac+1"        = "Bac",
  "bac+2"               = "Bac+2",
  "bac+3"               = "Licence (L)",
  "bac+4"               = "Master (M) et plus",
  "bac+5"               = "Master (M) et plus",
  "bac+8"               = "Master (M) et plus")

View(BASE_ETUDE_FATTRI5)
mean(is.na(BASE_ETUDE_FATTRI5$NIVEDUC_LMDF))
unique(BASE_ETUDE_FATTRI5$NIVEDUC_LMDF)

# **** Ajouter le genre à ma base de donnée **********************************
tab_identindl <- data.frame(IDENTINDL = unique(BASE_ETUDE_FATTRI5$IDENTINDL))
View(tab_identindl)

#**** Aller chercher le genre dans les autres bases de données ***************
mean(is.na(individu$SEXE)) * 100 
mean(is.na(individu_2017$SEXE)) * 100 
mean(is.na(individu_2014$SEXE)) * 100

table_sexe <- bind_rows(
  individu %>% select(IDENTINDL, SEXE),
  individu_2017 %>% select(IDENTINDL, SEXE),
  individu_2014 %>% select(IDENTINDL, SEXE)) %>%
  distinct(IDENTINDL, .keep_all = TRUE)

unique(table_sexe$SEXE)

#**** Recoder les modalités ****
table_sexe$SEXE <- factor(
  table_sexe$SEXE,
  levels = c("1", "2"),
  labels = c("Homme", "Femme"))

View(table_sexe)

tab_identindl <- tab_identindl %>%
  left_join(table_sexe, by = "IDENTINDL")

View(tab_identindl)

nrow(tab_identindl)
length(unique(tab_identindl$IDENTINDL))

# **** Ramner dans la base de donnée PANEL **********
BASE_ETUDE_FATTRI5 <- BASE_ETUDE_FATTRI5 %>%
  left_join(tab_identindl %>% select(IDENTINDL, SEXE),
    by = "IDENTINDL")

View(BASE_ETUDE_FATTRI5)

mean(is.na(BASE_ETUDE_FATTRI5$SEXE))

BASE_ETUDE_FATTRI5$n = NULL 

#****** Recoder RISKFI et DIF *********************************************
unique(BASE_ETUDE_FATTRI5$DIEXP)
unique(BASE_ETUDE_FATTRI5$RISKFI)

# **** RISKFI ****
BASE_ETUDE_FATTRI5$RISKFI_PROFIL <- dplyr::recode(
  BASE_ETUDE_FATTRI5$RISKFI,
  "Sans risque financier" = "Riscophobe",
  "Risque moyen" = "Riscophile",
  "Risque modéré à élevé" = "Riscophile",
  "Risque élevé" = "Riscophile",
  "Non déclaré" = "Non déclaré")

View(BASE_ETUDE_FATTRI5)

# **** DIF ******
unique(BASE_ETUDE_FATTRI5$DIF)c

BASE_ETUDE_FATTRI5$DIF_PROFIL <- dplyr::recode(
  BASE_ETUDE_FATTRI5$DIF,
  "Jamais arrivé" = "Sans difficulté face aux échéances",
  "Déja arrivé" = "Difficultés face aux échéances",
  "Arrivé récemment" = "Difficultés face aux échéances",
  "Depuis moins d'un an" = "Difficultés face aux échéances",
  "Depuis plusieurs années" = "Difficultés face aux échéances",
  "Non déclaré" = "Non déclaré")

# ****** Base finale ******************************************************
pdb = BASE_ETUDE_FATTRI5

View(pdb)

min(pdata$PATFISOM)

#*****************************************************  Preparation des données ***************************************************************** 
#*** Identifier les deux variables ********************
pdata = pdata.frame(pdb, index=c("IDENTINDL","ANNEE_ENQUETE"))
View(pdata)

#*** Vérifier si le poids est constant par individu ********
verif_poids=tapply(pdata$POND_LONG_IND_2014,pdata$IDENTINDL, function(x) length(unique(x)))
stopifnot(all(verif_poids ==1))
# On est bon pour les poids 

#***** Package supplement pour les modeles ponderes ********************
install.packages(c("survey","WeMix")) # Pour les pooled pondere et # RE logit/probit 
library("survey")
library("WeMix")

#*** 1. Diagnostic preliminaire ********************************
#*** logit à effets fixes (FE logit), les individus dont la variable dépendante est constante sur *********
#*** toute la période ne contribuent pas à l'estimation et sont automatiquement éliminés. Le FE logit *****
#*** repose sur la variation intra-individu. Il cherche à expliquer : Pourquoi un même individu passe *****
#*** de 0 à 1 (ou de 1 à 0) lorsque ses caractéristiques changent ? ***************************************
variation_y=tapply(pdata$LIVABL,pdata$IDENTINDL, function(x) length(unique(x)))
table(variation_y)

part_constants = mean(variation_y == 1)
cat("Part d'individus avec LIVABL constant (perdus en FE):", round(part_constants *100,1),"%\n")
#******** 86.8% très élévé, argement en faveur du RE********************************************************* 

#*** 2. MODELE 1 : LOGIT POOLE PONDERE ********************************************************************
#*** le logit poolé (pooled logit) consiste à empiler toutes les observations des années 2014, 2017 *******
#*** et 2020 et à estimer un logit classique comme si les observations étaient homogène : ************
#*** On déclare le design : pondéré chaque individu dans son ensemble *************************************

# ********** Ajout un à un pour voir les variables qui pose problème ***************************************
# Ecarté : NENFANTS, MMLDDS_I, MMLA_I, MMLEP_I, NACTIFS,TYPEMPLOI, SITUA, 
# STATUT, ZREVDEC_I, DIEXP, CNDUR, DIEXP,DIF, RETRAIT
# Ordre d'ajout des variables : LDD, LEP, MMLA_I, MMLDDS_I, MMLEP_I, NPERS,NENFANTS,NACTIFS,ACTIF,ETAMATRI,AGE_RELATIF_I, CATAGE_RELATIF_I,TYPEMPLOI
# NIVEDUC_I, CS8_I, PATRIRES, PATFINANC

# *** Définir les modalités de référence *****************
pdata$CATAGE_RELATIF_I <- relevel(factor(pdata$CATAGE_RELATIF_I),  ref = "30 à 44 ans")

pdata$NIVEDUC_LMD <- relevel(factor(pdata$NIVEDUC_LMD), ref = "Bac")

pdata$CS8_I <- relevel(factor(pdata$CS8_I), ref = "Autres personnes sans activité professionnelle")

pdata$SITUA <- relevel(factor(pdata$SITUA), ref = "Inactif")

pdata$RISKFI_PROFIL <- relevel(factor(pdata$RISKFI_PROFIL), ref = "Riscophobe")

pdata$DIEXP <- relevel(factor(pdata$DIEXP), ref = "Identique PA")

pdata$DIF_PROFIL <- relevel(factor(pdata$DIF_PROFIL), ref = "Sans difficulté face aux échéances")

# Vérification
levels(pdata$CATAGE_RELATIF_I)
levels(pdata$NIVEDUC_LMD)
levels(pdata$CS8_I)
levels(pdata$SITUA)
levels(pdata$RISKFI_PROFIL)
levels(pdata$DIEXP)
levels(pdata$DIF_PROFIL)

# *** Imputation des patrimoines financiers négatifs *** 
pdata <- pdata %>%
  mutate(
    PATFISOM = ifelse(PATFISOM < 0, 0, PATFISOM))

mean(pdata$PATFISOM <0)

#*********** Diagnostic idée de normaliser les poids ******************************************************
summary(pdata$POND_LONG_IND_2014) 
quantile(pdata$POND_LONG_IND_2014,c(.01,.05,.25,.5,.75,.95,.99))
table(pdata2$POND_LONG_IND_2014 == 0)
summary(pdata2$POND_LONG_IND_2014[pdata2$POND_LONG_IND_2014 > 0])

pdata$poids_norm =pdata$POND_LONG_IND_2014/mean(pdata$POND_LONG_IND_2014, na.rm =TRUE)

design_pool = svydesign(ids= ~1, weights = ~poids_norm, data = pdata)

names(design_pool$variables)

View(pdata)

logit_pooled_new <- svyglm(LIVABL~LDD+LEP+NPERS+NENFANTS+NENFANTS+NACTIFS+ACTIF+ETAMATRI+SEXE+
  CATAGE_RELATIF_I+NIVEDUC_LMD+CS8_I+SITUA+log(ZREVDEC_I+1)+decile_revenu_ind+decile_patrimoine+
  PATFISOM+PATIMM+PATRIPRO+PATRIRES+MTDETTES+PATFINANC+CNDUR+DIEXP+DIF_PROFIL+AVI+MAVI+ASDEC
  +CEL+MCEL_I+PEL+MPEL_I+RETRAIT+MEPRETRAITE_I+EPSALA+MEPSALAR_I+MLEE_I+VMOB+MVMOB+
ACTION+PEA+TERM+MCOMPTERM+LIVJEUN+MLIVJEUNE_I+R_PATIMMO+R_RESPRIN+MRESPRIN+R_DETRE+DETIMM+
NBLOG+NE+DETPRO+HERTABH_I+Heritage_I+HERTABD_I+Donation_R_I+CONSEP+RISKFI_PROFIL+STATUT_ACT, 
                       design = design_pool,family = quasibinomial(link = "logit")) 

summary(logit_pooled_new)

names(pdata)


unique(BASE_ETUDE_FATTRI5$NIVEDUC_LMD)

# *** Eliminer les multiclinéarité ***************************************
library(car)

vif(logit_pooled_new)

# Ordre de suppression des variables : NBENFANTS, ACTIF, STATUT_ACT, NBACTIF, decile_revenu_ind, decile_patrimoine, R_PATIMMO, PATFINANC

logit_pooled2 <- update(
  logit_pooled2,
  . ~ . - PATFINANC)

summary(logit_pooled2)

vif(logit_pooled2)

# *** Je retire les variables montants *** 
# MAVI, MCEL_I, MPEL_I, MEPRETRAITE_I, MEPSALAR_I, MVMOB, MCOMPTERM, MLIVJEUNE_I, MRESPRIN, log(Heritage_I+1), log(Donation_R_I+1)

# **** Modele final pooled ***
logit_pooled_final_LA =
  svyglm(formula = LIVABL ~ factor(ANNEE_ENQUETE) + LDD + LEP + NPERS + NACTIFS
         + SEXE
         + ETAMATRI
         + CATAGE_RELATIF_I 
         + NIVEDUC_LMDF 
         + CS8_I
         + log(ZREVDEC_I+1) + log(CNDUR+1)
         +log(PATFISOM + 1) + log(PATIMM) + log(PATRIPRO+1) + log(PATRIRES) + log(MTDETTES+1) 
         + AVI + ASDEC 
         + CEL + PEL 
         + RETRAIT 
         + EPSALA + MLEE_I 
         + VMOB 
         + ACTION + PEA 
         + TERM 
         + LIVJEUN 
         + R_RESPRIN + R_DETRE 
         + NBLOG 
         + NE + DETPRO 
         + HERTABH_I + HERTABD_I 
         + RISKFI_PROFIL
         + DIEXP
         + DIF_PROFIL
       ,design = design_pool, family = quasibinomial(link = "logit"))

summary(logit_pooled_final_LA)

# *** Ajout de l'âge et âge ***
pdata$AGE_RELATIF_I2 = pdata$AGE_RELATIF_I^2

View(pdata)
  
logit_pooled_final_LA1 =
  svyglm(formula = LIVABL ~ factor(ANNEE_ENQUETE) + LDD + LEP + NPERS + NACTIFS
         + SEXE
         + ETAMATRI
         + AGE_RELATIF_I
         + AGE_RELATIF_I2
         + NIVEDUC_LMDF 
         + CS8_I
         + log(ZREVDEC_I+1) + log(CNDUR+1)
         +log(PATFISOM + 1) + log(PATIMM) + log(PATRIPRO+1) + log(PATRIRES) + log(MTDETTES+1) 
         + AVI + ASDEC 
         + CEL + PEL 
         + RETRAIT 
         + EPSALA + MLEE_I 
         + VMOB 
         + ACTION + PEA 
         + TERM 
         + LIVJEUN 
         + R_RESPRIN + R_DETRE 
         + NBLOG 
         + NE + DETPRO 
         + HERTABH_I + HERTABD_I 
         + RISKFI_PROFIL
         + DIEXP
         + DIF_PROFIL
         ,design = design_pool, family = quasibinomial(link = "logit"))

summary(logit_pooled_final_LA1)


# *** Modele LDDS ****
logit_pooled_final_LDD =
  svyglm(formula = LDD ~ factor(ANNEE_ENQUETE) + LIVABL + LEP + NPERS + NACTIFS
         + SEXE
         + ETAMATRI
         + CATAGE_RELATIF_I 
         + NIVEDUC_LMDF 
         + CS8_I
         + log(ZREVDEC_I+1) + log(CNDUR+1)
         +log(PATFISOM + 1) + log(PATIMM) + log(PATRIPRO+1) + log(PATRIRES) + log(MTDETTES+1) 
         + AVI + ASDEC 
         + CEL + PEL 
         + RETRAIT 
         + EPSALA + MLEE_I 
         + VMOB 
         + ACTION + PEA 
         + TERM 
         + LIVJEUN 
         + R_RESPRIN + R_DETRE 
         + NBLOG 
         + NE + DETPRO 
         + HERTABH_I + HERTABD_I 
         + RISKFI_PROFIL
         + DIEXP
         + DIF_PROFIL
         ,design = design_pool, family = quasibinomial(link = "logit"))

summary(logit_pooled_final_LDD)

# *** Ajout de l'âge *** 
logit_pooled_final_LDD1 =
  svyglm(formula = LDD ~ factor(ANNEE_ENQUETE) + LIVABL + LEP + NPERS + NACTIFS
         + SEXE
         + ETAMATRI
         + AGE_RELATIF_I
         + AGE_RELATIF_I2
         + NIVEDUC_LMDF 
         + CS8_I
         + log(ZREVDEC_I+1) + log(CNDUR+1)
         +log(PATFISOM + 1) + log(PATIMM) + log(PATRIPRO+1) + log(PATRIRES) + log(MTDETTES+1) 
         + AVI + ASDEC 
         + CEL + PEL 
         + RETRAIT 
         + EPSALA + MLEE_I 
         + VMOB 
         + ACTION + PEA 
         + TERM 
         + LIVJEUN 
         + R_RESPRIN + R_DETRE 
         + NBLOG 
         + NE + DETPRO 
         + HERTABH_I + HERTABD_I 
         + RISKFI_PROFIL
         + DIEXP
         + DIF_PROFIL
         ,design = design_pool, family = quasibinomial(link = "logit"))

summary(logit_pooled_final_LDD1)

# *** Modele LEP ****
logit_pooled_final_LEP =
  svyglm(formula = LEP ~ factor(ANNEE_ENQUETE) + LIVABL + LDD + NPERS + NACTIFS
         + SEXE
         + ETAMATRI
         + CATAGE_RELATIF_I 
         + NIVEDUC_LMDF 
         + CS8_I
         + log(ZREVDEC_I+1) + log(CNDUR+1)
         +log(PATFISOM + 1) + log(PATIMM) + log(PATRIPRO+1) + log(PATRIRES) + log(MTDETTES+1) 
         + AVI + ASDEC 
         + CEL + PEL 
         + RETRAIT 
         + EPSALA + MLEE_I 
         + VMOB 
         + ACTION + PEA 
         + TERM 
         + LIVJEUN 
         + R_RESPRIN + R_DETRE 
         + NBLOG 
         + NE + DETPRO 
         + HERTABH_I + HERTABD_I 
         + RISKFI_PROFIL
         + DIEXP
         + DIF_PROFIL
         ,design = design_pool, family = quasibinomial(link = "logit"))

summary(logit_pooled_final_LEP)

# *** Ajout de l'âge *** 
logit_pooled_final_LEP1 =
  svyglm(formula = LEP ~ factor(ANNEE_ENQUETE) + LIVABL + LDD + NPERS + NACTIFS
         + SEXE
         + ETAMATRI
         + AGE_RELATIF_I
         + AGE_RELATIF_I2
         + NIVEDUC_LMDF 
         + CS8_I
         + log(ZREVDEC_I+1) + log(CNDUR+1)
         +log(PATFISOM + 1) + log(PATIMM) + log(PATRIPRO+1) + log(PATRIRES) + log(MTDETTES+1) 
         + AVI + ASDEC 
         + CEL + PEL 
         + RETRAIT 
         + EPSALA + MLEE_I 
         + VMOB 
         + ACTION + PEA 
         + TERM 
         + LIVJEUN 
         + R_RESPRIN + R_DETRE 
         + NBLOG 
         + NE + DETPRO 
         + HERTABH_I + HERTABD_I 
         + RISKFI_PROFIL
         + DIEXP
         + DIF_PROFIL
         ,design = design_pool, family = quasibinomial(link = "logit"))

summary(logit_pooled_final_LEP1)

AIC(logit_pooled_final_LEP1,logit_pooled_final_LEP)

#*********** Adequation du FE ? ***************************************************************
#*** de 0 à 1 (ou de 1 à 0) lorsque ses caractéristiques changent ? ***************************************
variation_y=tapply(pdata$LIVABL,pdata$IDENTINDL, function(x) length(unique(x)))
table(variation_y)

part_constants = mean(variation_y == 1)
cat("Part d'individus avec LIVABL constant (perdus en FE):", round(part_constants *100,1),"%\n")

# Donc FE ne serait pas adapté (limite pratique des données)

# ******** Estimer le RE **************************************************************************
logit_re_final_LA <- glmmTMB(
  formula = LIVABL ~ factor(ANNEE_ENQUETE) + LDD + LEP + NPERS + NACTIFS
  + SEXE
  + ETAMATRI
  + AGE_RELATIF_I
  + AGE_RELATIF_I2
  + NIVEDUC_LMDF 
  + CS8_I
  + log(ZREVDEC_I+1) + log(CNDUR+1)
  +log(PATFISOM + 1) + log(PATIMM) + log(PATRIPRO+1) + log(PATRIRES) + log(MTDETTES+1) 
  + AVI + ASDEC 
  + CEL + PEL 
  + RETRAIT 
  + EPSALA + MLEE_I 
  + VMOB 
  + ACTION + PEA 
  + TERM 
  + LIVJEUN 
  + R_RESPRIN + R_DETRE 
  + NBLOG 
  + NE + DETPRO 
  + HERTABH_I + HERTABD_I 
  + RISKFI_PROFIL
  + DIEXP
  + DIF_PROFIL
  + (1 | IDENTINDL),
  data = pdata,
  weights = poids_norm,
  family = binomial(link = "logit"))

summary(logit_re_final_LA)

logit_re_final_LDD <- glmmTMB(
  formula = LDD ~ factor(ANNEE_ENQUETE) + LIVABL + LEP + NPERS + NACTIFS
  + SEXE
  + ETAMATRI
  + AGE_RELATIF_I
  + AGE_RELATIF_I2
  + NIVEDUC_LMDF 
  + CS8_I
  + log(ZREVDEC_I+1) + log(CNDUR+1)
  +log(PATFISOM + 1) + log(PATIMM) + log(PATRIPRO+1) + log(PATRIRES) + log(MTDETTES+1) 
  + AVI + ASDEC 
  + CEL + PEL 
  + RETRAIT 
  + EPSALA + MLEE_I 
  + VMOB 
  + ACTION + PEA 
  + TERM 
  + LIVJEUN 
  + R_RESPRIN + R_DETRE 
  + NBLOG 
  + NE + DETPRO 
  + HERTABH_I + HERTABD_I 
  + RISKFI_PROFIL
  + DIEXP
  + DIF_PROFIL
  + (1 | IDENTINDL),
  data = pdata,
  weights = poids_norm,
  family = binomial(link = "logit"))

summary(logit_re_final_LDD)

logit_re_final_LEP <- glmmTMB(
  formula = LEP ~ factor(ANNEE_ENQUETE) + LIVABL + LDD + NPERS + NACTIFS
  + SEXE
  + ETAMATRI
  + AGE_RELATIF_I
  + AGE_RELATIF_I2
  + NIVEDUC_LMDF 
  + CS8_I
  + log(ZREVDEC_I+1) + log(CNDUR+1)
  +log(PATFISOM + 1) + log(PATIMM) + log(PATRIPRO+1) + log(PATRIRES) + log(MTDETTES+1) 
  + AVI + ASDEC 
  + CEL + PEL 
  + RETRAIT 
  + EPSALA + MLEE_I 
  + VMOB 
  + ACTION + PEA 
  + TERM 
  + LIVJEUN 
  + R_RESPRIN + R_DETRE 
  + NBLOG 
  + NE + DETPRO 
  + HERTABH_I + HERTABD_I 
  + RISKFI_PROFIL
  + DIEXP
  + DIF_PROFIL
  + (1 | IDENTINDL),
  data = pdata,
  weights = poids_norm,
  family = binomial(link = "logit"))

summary(logit_re_final_LEP)

#*** Comparer pooled vs RE *********************************************************************
AIC(logit_pooled_final_LA1,logit_re_final_LA)
AIC(logit_pooled_final_LDD1,logit_re_final_LDD)
AIC(logit_pooled_final_LEP1,logit_re_final_LEP)

# Choix du Modèle aléatoire dans les trois modèles

#************ FE ***************************************************************************
#**** Pas utile car ne prends pas en compte le poids d'enquête ******
fe_logit <- clogit(
  formula = LIVABL ~ factor(ANNEE_ENQUETE) + LDD + LEP + NPERS + NACTIFS
  + ETAMATRI
  + CATAGE_RELATIF_I 
  + NIVEDUC_I 
  + CS8_I
  + log(ZREVDEC_I+1) + log(CNDUR+1)
  +log(PATFISOM + 1) + log(PATIMM) + log(PATRIPRO+1) + log(PATRIRES) + log(MTDETTES+1) 
  + AVI + ASDEC 
  + CEL + PEL 
  + RETRAIT 
  + EPSALA + MLEE_I 
  + VMOB 
  + ACTION + PEA 
  + TERM 
  + LIVJEUN 
  + R_RESPRIN + R_DETRE 
  + NBLOG 
  + NE + DETPRO 
  + HERTABH_I + HERTABD_I 
  + RISKFI
  + DIEXP
  + DIF
  + strata(IDENTINDL),
  data = pdata)

summary(fe_logit)

#********** Robustesse du modèle RE **********************************************************
# Test de Chow pour FE vs Pooled (Pas adapté ici) (Le modele Fixed ne prend pas en compte le poids: Pas de package)
# Test de Hausman pour RE vs FE (Pas adapté)

#********* Validation *************************
# Test de multicolinéarité (Déjà fait dans la selection : Peut pas vérifier car package pas adapté)

# ****** Qualité de la prédiction 
# ** LA
library(pROC)

pred <- predict(logit_re_final_LA, type = "response")

roc_obj <- roc(response = pdata$LIVABL, predictor = pred)
auc(roc_obj)

plot(roc_obj)

# ** LDD
pred_LDD <- predict(logit_re_final_LDD, type = "response")

roc_obj_LDD <- roc(response = pdata$LDD, predictor = pred_LDD)
auc(roc_obj_LDD)

plot(roc_obj_LDD)

# ** LEP 
pred_LEP <- predict(logit_re_final_LEP, type = "response")

roc_obj_LEP <- roc(response = pdata$LEP, predictor = pred_LEP)
auc(roc_obj_LEP)

plot(roc_obj_LEP)

# La courbe ROC présente une aire sous la courbe (AUC) de 0,796. Cette valeur indique une 
# bonne capacité du modèle à distinguer les détenteurs des non-détenteurs du Livret A.

# **** Estimation des odds et des effets margniaux des ces modèles ***********************

# *** Odd Ratio ***
exp(fixef(logit_re_final_LA)$cond)
exp(fixef(logit_re_final_LDD)$cond)
exp(fixef(logit_re_final_LEP)$cond)

# *** Effets marginaux moyens ***
p <- predict(logit_re_final_LA,type = "response",re.form = NA)
mean_factor <- mean(p * (1 - p))
AME_LA <- fixef(logit_re_final_LA)$cond * mean_factor
AME_LA

v <- predict(logit_re_final_LDD,type = "response",re.form = NA)
mean_factor_LDD <- mean(v * (1 - v))
AME_LDD <- fixef(logit_re_final_LDD)$cond * mean_factor_LDD
AME_LDD

# Probabilités prédites
s <- predict(logit_re_final_LEP,type = "response",re.form = NA)

# Facteur moyen p(1-p)
mean_factor_LEP <- mean(s * (1 - s))

# Effets marginaux moyens
AME_LEP <- fixef(logit_re_final_LEP)$cond * mean_factor_LEP
AME_LEP

# Courbe des probabilités prédites 
install.packages("ggeffects")
library(ggeffects)
library(ggplot2)

pred_age <- ggpredict(
  logit_pooled_final_LA1,
  terms = "AGE_RELATIF_I [20:100]"
)

plot(pred_age)

plot(pred_age, ci = FALSE, connect_lines = TRUE)

# ********* Modele arbitrage **************************************************************
# # Objectif
#   
# 1. Construction de la base des détenteurs
# 2. Estimation pooled (MCO)
# 3. Vérification de la multicolinéarité (VIF)
# 4. Estimation à effets fixes (FE)
# 5. Comparaison FE vs pooled (test de Chow / test F conjoint)
# 6. Modèle à effets aléatoires (RE) + AIC
# 7. Comparaison FE vs RE (test de Hausman)
# 8. Choix du modèle final
# 9. Tests de robustesse : hétéroscédasticité (Breusch-Pagan + erreurs robustes White), 
# autocorrélation (test de Wooldridge + erreurs clusterisées Arellano), 
# piste endogénéité

#*** 1. Package ***
packages <- c("plm", "car", "lmtest", "sandwich", "AER", "broom")

to_install <- packages[!packages %in% installed.packages()[, "Package"]]
if (length(to_install) > 0) install.packages(to_install)

lapply(packages, library, character.only = TRUE)

View(pdata)
names(design)

names(pdata)

# Restriction aux détenteurs uniquement : plus de censure à partir d'ici
library(haven)
pdata <- pdata %>%
  mutate(LIVABL = haven::zap_labels(LIVABL))

class(pdata$LIVABL)

# **** Je garde les personne qui ont un livret A sur la période ******************
pdata2 <- pdata %>%
  filter(ANNEE_ENQUETE %in% c(2014, 2017, 2020)) %>%
  group_by(IDENTINDL) %>%
  filter(any(LIVABL == 1, na.rm = TRUE)) %>%
  ungroup()

pdata2 <- pdata2[order(pdata2$IDENTINDL, pdata2$ANNEE_ENQUETE), ]

View(pdata2)

nrow(pdata)
nrow(pdata2)

cat("Nombre d'observations (détenteurs) :", nrow(pdata2), "\n")
cat("Nombre d'individus distincts :", length(unique(pdata2$IDENTINDL)), "\n")

# Panel data frame pour plm
pdata_det <- pdata.frame(pdata2, index = c("IDENTINDL", "ANNEE_ENQUETE"))

# Variable dépendante : envisager le log si distribution asymétrique
hist(pdata_det$MMLA_I, main = "Distribution du montant (détenteurs)", xlab = "MMLA_I")
hist(log(pdata_det$MMLA_I), main = "Distribution du log(montant)", xlab = "log(MMLA_I)")

# *** Livret A 
pooled_LA <- 
  plm(formula = log(MMLA_I+1) ~ 
              factor(ANNEE_ENQUETE) 
              + log(MMLDDS_I+1) 
              + log(MMLEP_I+1) 
              + NPERS + NACTIFS
              + SEXE
              + ETAMATRI
              + AGE_RELATIF_I
              + AGE_RELATIF_I2
              + NIVEDUC_LMDF
              + CS8_I
              + log(ZREVDEC_I+1) + log(CNDUR+1)
              + log(PATFISOM + 1) + log(PATIMM) + log(PATRIPRO+1) + log(PATRIRES) + log(MTDETTES+1) 
              + log(MAVI+1) 
              + log(MCEL_I+1)
              + log(MPEL_I+1)
              + log(MEPRETRAITE_I+1)
              + log(MEPSALAR_I+1)
              + log(MLEE_I+1)
              + log(MVMOB+1)
              + log(MCOMPTERM+1)
              + log(MLIVJEUNE_I+1)
              + log(MRESPRIN+1)
              + log(Heritage_I+1)
              + log(Donation_R_I+1)
              + R_DETRE + DETPRO + CONSEP + RISKFI_PROFIL
              , data = pdata_det
              , model = "pooling"
              ,weights = poids_norm)

summary(pooled_LA)

# *** LDDS 
pooled_LDD <- 
  plm(formula = log(MMLDDS_I+1) ~ 
        factor(ANNEE_ENQUETE) 
      + log(MMLA_I+1) 
      + log(MMLEP_I+1) 
      + NPERS + NACTIFS
      + SEXE
      + ETAMATRI
      + AGE_RELATIF_I
      + AGE_RELATIF_I2
      + NIVEDUC_LMDF
      + CS8_I
      + log(ZREVDEC_I+1) + log(CNDUR+1)
      + log(PATFISOM + 1) + log(PATIMM) + log(PATRIPRO+1) + log(PATRIRES) + log(MTDETTES+1) 
      + log(MAVI+1) 
      + log(MCEL_I+1)
      + log(MPEL_I+1)
      + log(MEPRETRAITE_I+1)
      + log(MEPSALAR_I+1)
      + log(MLEE_I+1)
      + log(MVMOB+1)
      + log(MCOMPTERM+1)
      + log(MLIVJEUNE_I+1)
      + log(MRESPRIN+1)
      + log(Heritage_I+1)
      + log(Donation_R_I+1)
      + R_DETRE + DETPRO + CONSEP + RISKFI_PROFIL
      , data = pdata_det
      , model = "pooling"
      ,weights = poids_norm)

summary(pooled_LDD)

View(pdata)

# *** LEP
pooled_LEP <- 
  plm(formula = log(MMLEP_I+1) ~ 
        factor(ANNEE_ENQUETE) 
      + log(MMLA_I+1) 
      + log(MMLDDS_I+1) 
      + NPERS + NACTIFS
      + SEXE
      + ETAMATRI
      + AGE_RELATIF_I
      + AGE_RELATIF_I2
      + NIVEDUC_LMDF
      + CS8_I
      + log(ZREVDEC_I+1) + log(CNDUR+1)
      + log(PATFISOM + 1) + log(PATIMM) + log(PATRIPRO+1) + log(PATRIRES) + log(MTDETTES+1) 
      + log(MAVI+1) 
      + log(MCEL_I+1)
      + log(MPEL_I+1)
      + log(MEPRETRAITE_I+1)
      + log(MEPSALAR_I+1)
      + log(MLEE_I+1)
      + log(MVMOB+1)
      + log(MCOMPTERM+1)
      + log(MLIVJEUNE_I+1)
      + log(MRESPRIN+1)
      + log(Heritage_I+1)
      + log(Donation_R_I+1)
      + R_DETRE + DETPRO + CONSEP + RISKFI_PROFIL
      , data = pdata_det
      , model = "pooling"
      ,weights = poids_norm)

summary(pooled_LEP)

# 4. Vérification de la multicolinéarité (VIF)
vif(pooled_LA)
vif(pooled_LDD)
vif(pooled_LEP)

# On est OK avec le VIF 

# 5. Estimation à effets fixes (FE, within)
# *** Livret A
fe_LA <- plm(formula = log(MMLA_I+1) ~ 
               factor(ANNEE_ENQUETE) 
             + log(MMLDDS_I+1) 
             + log(MMLEP_I+1) 
             + NPERS + NACTIFS
             + SEXE
             + ETAMATRI
             + AGE_RELATIF_I
             + AGE_RELATIF_I2
             + NIVEDUC_LMDF
             + CS8_I
             + log(ZREVDEC_I+1) + log(CNDUR+1)
             + log(PATFISOM + 1) + log(PATIMM) + log(PATRIPRO+1) + log(PATRIRES) + log(MTDETTES+1) 
             + log(MAVI+1) 
             + log(MCEL_I+1)
             + log(MPEL_I+1)
             + log(MEPRETRAITE_I+1)
             + log(MEPSALAR_I+1)
             + log(MLEE_I+1)
             + log(MVMOB+1)
             + log(MCOMPTERM+1)
             + log(MLIVJEUNE_I+1)
             + log(MRESPRIN+1)
             + log(Heritage_I+1)
             + log(Donation_R_I+1)
             + R_DETRE + DETPRO + CONSEP + RISKFI_PROFIL
             , data = pdata_det, model = "within",
          weights = poids_norm)

summary(fe_LA)

# *** LDDS
fe_LDDS <- plm(formula = log(MMLDDS_I+1) ~ 
                 factor(ANNEE_ENQUETE) 
               + log(MMLA_I+1) 
               + log(MMLEP_I+1) 
               + NPERS + NACTIFS
               + SEXE
               + ETAMATRI
               + AGE_RELATIF_I
               + AGE_RELATIF_I2
               + NIVEDUC_LMDF
               + CS8_I
               + log(ZREVDEC_I+1) + log(CNDUR+1)
               + log(PATFISOM + 1) + log(PATIMM) + log(PATRIPRO+1) + log(PATRIRES) + log(MTDETTES+1) 
               + log(MAVI+1) 
               + log(MCEL_I+1)
               + log(MPEL_I+1)
               + log(MEPRETRAITE_I+1)
               + log(MEPSALAR_I+1)
               + log(MLEE_I+1)
               + log(MVMOB+1)
               + log(MCOMPTERM+1)
               + log(MLIVJEUNE_I+1)
               + log(MRESPRIN+1)
               + log(Heritage_I+1)
               + log(Donation_R_I+1)
               + R_DETRE + DETPRO + CONSEP + RISKFI_PROFIL
               , data = pdata_det, model = "within",
             weights = poids_norm)

summary(fe_LDDS)

# *** LEP
fe_LEP <- plm(formula = log(MMLEP_I+1) ~ 
                factor(ANNEE_ENQUETE) 
              + log(MMLA_I+1) 
              + log(MMLDDS_I+1) 
              + NPERS + NACTIFS
              + SEXE
              + ETAMATRI
              + AGE_RELATIF_I
              + AGE_RELATIF_I2
              + NIVEDUC_LMDF
              + CS8_I
              + log(ZREVDEC_I+1) + log(CNDUR+1)
              + log(PATFISOM + 1) + log(PATIMM) + log(PATRIPRO+1) + log(PATRIRES) + log(MTDETTES+1) 
              + log(MAVI+1) 
              + log(MCEL_I+1)
              + log(MPEL_I+1)
              + log(MEPRETRAITE_I+1)
              + log(MEPSALAR_I+1)
              + log(MLEE_I+1)
              + log(MVMOB+1)
              + log(MCOMPTERM+1)
              + log(MLIVJEUNE_I+1)
              + log(MRESPRIN+1)
              + log(Heritage_I+1)
              + log(Donation_R_I+1)
              + R_DETRE + DETPRO + CONSEP + RISKFI_PROFIL
              , data = pdata_det, model = "within",
               weights = poids_norm)

summary(fe_LEP)

# 6. Comparaison FE vs pooled (test de Chow / test F conjoint)

# $H_0$ : pas d'effets individuels significatifs (le pooled suffit).
pFtest(fe_LA, pooled_LA) # NRHO : Le modèle Pooled est mieux
pFtest(fe_LDDS, pooled_LDD) # NRHO : Le modèle FE est mieux
pFtest(fe_LEP, pooled_LEP) # RHO : Le modèle FE est mieux

# 7. Modèle à effets aléatoires (RE)

# *** Livret A
re_LA <- plm(formula = log(MMLA_I+1) ~ 
               factor(ANNEE_ENQUETE) 
             + log(MMLDDS_I+1) 
             + log(MMLEP_I+1) 
             + NPERS + NACTIFS
             + SEXE
             + ETAMATRI
             + AGE_RELATIF_I
             + AGE_RELATIF_I2
             + NIVEDUC_LMDF
             + CS8_I
             + log(ZREVDEC_I+1) + log(CNDUR+1)
             + log(PATFISOM + 1) + log(PATIMM) + log(PATRIPRO+1) + log(PATRIRES) + log(MTDETTES+1) 
             + log(MAVI+1) 
             + log(MCEL_I+1)
             + log(MPEL_I+1)
             + log(MEPRETRAITE_I+1)
             + log(MEPSALAR_I+1)
             + log(MLEE_I+1)
             + log(MVMOB+1)
             + log(MCOMPTERM+1)
             + log(MLIVJEUNE_I+1)
             + log(MRESPRIN+1)
             + log(Heritage_I+1)
             + log(Donation_R_I+1)
             + R_DETRE + DETPRO + CONSEP + RISKFI_PROFIL
             , data = pdata_det, model = "random",
          weights = poids_norm)

summary(re_LA)

# *** LDDS
re_LDD <- plm(formula = log(MMLDDS_I+1) ~ 
                factor(ANNEE_ENQUETE) 
              + log(MMLA_I+1) 
              + log(MMLEP_I+1) 
              + NPERS + NACTIFS
              + SEXE
              + ETAMATRI
              + AGE_RELATIF_I
              + AGE_RELATIF_I2
              + NIVEDUC_LMDF
              + CS8_I
              + log(ZREVDEC_I+1) + log(CNDUR+1)
              + log(PATFISOM + 1) + log(PATIMM) + log(PATRIPRO+1) + log(PATRIRES) + log(MTDETTES+1) 
              + log(MAVI+1) 
              + log(MCEL_I+1)
              + log(MPEL_I+1)
              + log(MEPRETRAITE_I+1)
              + log(MEPSALAR_I+1)
              + log(MLEE_I+1)
              + log(MVMOB+1)
              + log(MCOMPTERM+1)
              + log(MLIVJEUNE_I+1)
              + log(MRESPRIN+1)
              + log(Heritage_I+1)
              + log(Donation_R_I+1)
              + R_DETRE + DETPRO + CONSEP + RISKFI_PROFIL
             , data = pdata_det, model = "random",
             weights = poids_norm)

summary(re_LDD)

# *** LEP
re_LEP <- plm(formula = log(MMLEP_I+1) ~ 
                factor(ANNEE_ENQUETE) 
              + log(MMLA_I+1) 
              + log(MMLDDS_I+1) 
              + NPERS + NACTIFS
              + SEXE
              + ETAMATRI
              + AGE_RELATIF_I
              + AGE_RELATIF_I2
              + NIVEDUC_LMDF
              + CS8_I
              + log(ZREVDEC_I+1) + log(CNDUR+1)
              + log(PATFISOM + 1) + log(PATIMM) + log(PATRIPRO+1) + log(PATRIRES) + log(MTDETTES+1) 
              + log(MAVI+1) 
              + log(MCEL_I+1)
              + log(MPEL_I+1)
              + log(MEPRETRAITE_I+1)
              + log(MEPSALAR_I+1)
              + log(MLEE_I+1)
              + log(MVMOB+1)
              + log(MCOMPTERM+1)
              + log(MLIVJEUNE_I+1)
              + log(MRESPRIN+1)
              + log(Heritage_I+1)
              + log(Donation_R_I+1)
              + R_DETRE + DETPRO + CONSEP + RISKFI_PROFIL
              , data = pdata_det, model = "random",
              weights = poids_norm)

summary(re_LEP)

# 8. Comparaison FE vs RE (test de Hausman)
# $H_0$ : les effets individuels ne sont pas corrélés aux régresseurs (RE cohérent et plus efficient).
phtest(fe_LA, re_LA)
phtest(fe_LDDS, re_LDD)
phtest(fe_LEP, re_LEP)

# Le modèle FE est retenu
# Comparaison des AIC : AIC=−2×log(L)+2k : nombre de paramètres estimés
aic_manual <- function(model) {
  res <- residuals(model)
  n <- length(res)
  k <- length(coef(model)) + 1
  ll <- sum(dnorm(res, sd = sd(res), log = TRUE))
  -2 * ll + 2 * k
}

cat("AIC RE :", aic_manual(re_LA), "\n")
cat("AIC FE     :", aic_manual(fe_LA), "\n")  # FE is better 

cat("AIC RE :", aic_manual(re_LDD), "\n")
cat("AIC FE     :", aic_manual(fe_LDDS), "\n") # FE is better

cat("AIC RE :", aic_manual(re_LEP), "\n")
cat("AIC FE     :", aic_manual(fe_LEP), "\n") # FE is better

# 9. Modèle retenu : FE 

# 10. Tests de robustesse
## 10.1 Hétéroscédasticité

bptest(fe_LA)
# Le modèle retenu présentent des résidus Hétéroscédastique variance robuste de White comme solution 

bptest(fe_LDDS)
# Le modèle retenu présentent des résidus Hétéroscédastique variance robuste de White comme solution 

bptest(fe_LEP)
# Le modèle retenu présentent des résidus Hétéroscédastique variance robuste de White comme solution 

# *** Estimation robuste White ****
install.packages("fixest")
library(fixest)

MLA_FE = feols(fml = log(MMLA_I+1) ~ 
                 factor(ANNEE_ENQUETE) 
               + log(MMLDDS_I+1) 
               + log(MMLEP_I+1) 
               + NPERS + NACTIFS
           #    + SEXE
               + ETAMATRI
               + CATAGE_RELATIF_I
               + NIVEDUC_LMD
               + CS8_I
               + log(ZREVDEC_I+1) + log(CNDUR+1)
               + log(PATFISOM + 1) + log(PATIMM) + log(PATRIPRO+1) + log(PATRIRES) + log(MTDETTES+1) 
               + log(MAVI+1) 
               + log(MCEL_I+1)
               + log(MPEL_I+1)
               + log(MEPRETRAITE_I+1)
               + log(MEPSALAR_I+1)
               + log(MLEE_I+1)
               + log(MVMOB+1)
               + log(MCOMPTERM+1)
               + log(MLIVJEUNE_I+1)
               + log(MRESPRIN+1)
               + log(Heritage_I+1)
               + log(Donation_R_I+1)
               + R_DETRE + DETPRO + CONSEP + RISKFI_PROFIL | IDENTINDL,
         data = pdata_det,
         weights = ~ poids_norm)

summary(MLA_FE, cluster = ~ IDENTINDL)

etable(MLA_FE)

MLDD_FE = feols(fml = log(MMLDDS_I+1) ~ 
               #  factor(ANNEE_ENQUETE) 
               + log(MMLEP_I+1) 
               + log(MMLA_I+1) 
               + NPERS + NACTIFS
               #    + SEXE
               + ETAMATRI
               + CATAGE_RELATIF_I
               + NIVEDUC_LMD
               + CS8_I
               + log(ZREVDEC_I+1) + log(CNDUR+1)
               + log(PATFISOM + 1) + log(PATIMM) + log(PATRIPRO+1) + log(PATRIRES) + log(MTDETTES+1) 
               + log(MAVI+1) 
               + log(MCEL_I+1)
               + log(MPEL_I+1)
               + log(MEPRETRAITE_I+1)
               + log(MEPSALAR_I+1)
               + log(MLEE_I+1)
               + log(MVMOB+1)
               + log(MCOMPTERM+1)
               + log(MLIVJEUNE_I+1)
               + log(MRESPRIN+1)
               + log(Heritage_I+1)
               + log(Donation_R_I+1)
               + R_DETRE + DETPRO + CONSEP + RISKFI_PROFIL | IDENTINDL,
               data = pdata_det,
               weights = ~ poids_norm)

summary(MLDD_FE)

MLEP_FE = feols(fml = log(MMLEP_I+1) ~ 
                  factor(ANNEE_ENQUETE) 
                + log(MMLDDS_I+1) 
                + log(MMLA_I+1) 
                + NPERS + NACTIFS
                #    + SEXE
                + ETAMATRI
                + CATAGE_RELATIF_I
                + NIVEDUC_LMD
                + CS8_I
                + log(ZREVDEC_I+1) + log(CNDUR+1)
                + log(PATFISOM + 1) + log(PATIMM) + log(PATRIPRO+1) + log(PATRIRES) + log(MTDETTES+1) 
                + log(MAVI+1) 
                + log(MCEL_I+1)
                + log(MPEL_I+1)
                + log(MEPRETRAITE_I+1)
                + log(MEPSALAR_I+1)
                + log(MLEE_I+1)
                + log(MVMOB+1)
                + log(MCOMPTERM+1)
                + log(MLIVJEUNE_I+1)
                + log(MRESPRIN+1)
                + log(Heritage_I+1)
                + log(Donation_R_I+1)
                + R_DETRE + DETPRO + CONSEP + RISKFI_PROFIL | IDENTINDL,
                data = pdata_det,
                weights = ~ poids_norm)

summary(MLEP_FE, cluster = ~ IDENTINDL)

# Autocorrélation (test de Wooldridge, panel) : H0 : pas d'autocorrélation des résidus dans le temps ;
pwartest(fe_LA, data = pdata_det) # CECI CONFIRME UN PEU L'utilisation des erreurs cleuteurisé 
pwartest(fe_LDDS, data = pdata_det)
pwartest(fe_LEP, data = pdata_det)

# Modèle final
logit_re_final_LA
logit_re_final_LDD
logit_re_final_LEP



MLEP_FE_ESSAI = feols(fml = log(MMLEP_I+1) ~ 
                  factor(ANNEE_ENQUETE) 
                + log(MMLDDS_I+1) 
                + log(MMLA_I+1) 
                + log(ZREVDEC_I+1) + log(CNDUR+1)
                + log(PATFISOM + 1) + log(PATIMM) + log(PATRIPRO+1) + log(PATRIRES) + log(MTDETTES+1) 
                + log(MAVI+1) 
                + log(MCEL_I+1)
                + log(MPEL_I+1)
                + log(MEPRETRAITE_I+1)
                + log(MEPSALAR_I+1)
                + log(MLEE_I+1)
                + log(MVMOB+1)
                + log(MCOMPTERM+1)
                + log(MLIVJEUNE_I+1)
                + log(MRESPRIN+1)
                + log(Heritage_I+1)
                + log(Donation_R_I+1) | IDENTINDL,
                data = pdata_det,
                weights = ~ poids_norm)

summary(MLEP_FE_ESSAI)

summary(MLEP_FE_ESSAI, cluster = ~ IDENTINDL)

MLEP_FE_ESSAI1 = feols(fml = MMLEP_I ~ 
                        factor(ANNEE_ENQUETE) 
                      + MMLDDS_I 
                      + MMLA_I 
                      + ZREVDEC_I + CNDUR
                      + PATFISOM + PATIMM + PATRIPRO + PATRIRES + MTDETTES
                      + MAVI 
                      + MCEL_I
                      + MPEL_I
                      + MEPRETRAITE_I
                      + MEPSALAR_I
                      + MLEE_I
                      + MVMOB
                      + MCOMPTERM
                      + MLIVJEUNE_I
                      + MRESPRIN
                      + Heritage_I
                      + Donation_R_I | IDENTINDL
                      ,data = pdata_det
                      ,weights = ~ poids_norm)

summary(MLEP_FE_ESSAI1)


unique(pdata$CS8_I)

# ***** Tous les modèles de l'arbitrage *******************

# Modèle pooled
summary(pooled_LA)
summary(pooled_LDD)
summary(pooled_LEP)

# Modèle FE
summary(fe_LA)
summary(fe_LDD)
summary(fe_LEP)

# Modèle RE
summary(re_LA)
summary(re_LDD)
summary(re_LEP)

summary(logit_re_final_LA)

# ******** FIN DU MEMOIRE ESTIMATION : PLACE A LA REDACTION **********************
names(pdata)

unique(pdata$DIF_PROFIL)


# **** Les tableaux des modèles de la detention ***
nrow(pdata)


#*** LA ***
summary(logit_pooled_final_LA1)
summary(logit_re_final_LA)
exp(fixef(logit_re_final_LA)$cond)
AME_LA

#***LDDS*** 
summary(logit_pooled_final_LDD1)
summary(logit_re_final_LDD)
exp(fixef(logit_re_final_LDD)$cond)
AME_LDD

#***LEP*** 
summary(logit_pooled_final_LEP1)
summary(logit_re_final_LEP)
exp(fixef(logit_re_final_LEP)$cond)
AME_LEP

# Modèle RE
summary(re_LA)
summary(re_LDD)
summary(re_LEP)




