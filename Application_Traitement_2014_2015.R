#========================================================   Applications aux bases de 2014==========================================================================
#======================================================== 2014 =====================================================================================================
# Importation des bases de données *
install.packages(c("evd","evir","ismev","POT","DescTools"))

# Nouveau package
library(evd)
library(evir)
library(ismev)
library(POT)
library(DescTools)

# Rappel 
library(tidyr)
library(haven)
library(dplyr)
library(ggplot2)
library(survey)
library(stats)
library(Hmisc)
library(labelled)
library(forcats)  # Pour fct_recode() ou fct_relabel()
library(plotly)
library(janitor)
library(DT)
library(kableExtra)
print("Packages chargés avec succès")


# Vérification et Importation des données 
data_paths <- c(
  "produit_2014" = "C:/Users/gessessinou/Downloads/BDD/2014_2015/produit.sas7bdat",
  "menage_2014" = "C:/Users/gessessinou/Downloads/BDD/2014_2015/menage.sas7bdat",
  "individu_2014" = "C:/Users/gessessinou/Downloads/BDD/2014_2015/individu.sas7bdat",
  "transmission_2014" = "C:/Users/gessessinou/Downloads/BDD/2014_2015/transmission.sas7bdat",
  "entreprise_2014" = "C:/Users/gessessinou/Downloads/BDD/2014_2015/entreprise.sas7bdat",
  "qualite_2014" = "C:/Users/gessessinou/Downloads/BDD/2014_2015/qualite.sas7bdat")

for (name in names(data_paths)) {
  if(!exists(name, envir = .GlobalEnv)) {
    assign(name, read_sas(data_paths[name]))
    print(paste("Import de la table", name, "effectué"))
  } else {
    print(paste(name, "existe déjà - pas de réimportation"))
  }
}

View(menage_2014)

# 1) Selection des variables : 
menage_db_2014 <- menage_2014 %>%
  select(
    # Variables principales déjà incluses
    IDENT, POND,
    
    # Caractéristiques démographiques
    ANAISPR, AGECJ, AGEPR, ALGMEN, ALGNB, COUPLEPR,
    NENFANTS, MATRICJ, MATRIPR, MNAISCJ, MNAISPR, NPERS, SEXECJ, SEXEPR,
    
    # Situation professionnelle et revenus
    AUTSAL, CNDUR, CSCJ, CSPR, NACTIFS, NATIO7CJ, NATIO7PR,
    SITUACJ, SITUAPR, STATUTCJ, STATUTECLBISCJ, STATUTECLBISPR, STATUTPR,
    ITOTREV, TOTREVENC, TRANCHPRE, ZREVDISP, ZNIVVIE, ZREVDEC,
    
    # Patrimoine et actifs
    AVIAUT,
    AVIINFO, AVIPLAN, DET, DIEXP, DIPLOMCJ, DIPLOMPR,
    DONHOD, DONHOD_AUT, DONHOD_DON, DONHOD_ETU, DONHOD_INST, DONHOD_LOG,
    DONHOD_PRET, DONHOD_RECH, DONHOD_REG, DONPAR, DONPAR_AUT, DONPAR_DON,
    DONPAR_LOG, DONPAR_PRET, DONPAR_REG, DONPAR_RET, DONVERS, ENT_ENDET,
    PATFIVAR, PRET, PROPRI, RCHO, RESPRIACH, RISKFI, RMAL, RMODIF, RMODIF_CHO, 
    RMODIF_DC, RMODIF_DEP, RMODIF_EVOL,
    RMODIF_FORM, RMODIF_MAL, RMODIF_NAI, RMODIF_SEP,
    RRET, RSAL, R_ACTNPROF, R_AVI, R_AVIEURO, R_AVIMULT, R_AVIRENTE,
    R_BON, R_BXBAT, R_CAPI, R_CCA, R_CCQ, R_CEL, R_COMP, R_CTIT, R_DCGR,
    R_DCPE, R_DCPR, R_DETDI, R_DETEQ, R_DETETU, R_DETIM, R_DETPR, R_DETRE,
    R_DETRES, R_DETTR, R_DETVO, R_EDUC, R_ENT, R_ENTNEXP, R_EPALO, R_EPSAL,
    R_EPSALA, R_LDD, R_LEE, R_LEP, R_LIVABL, R_LIVDF, R_LIVEP, R_LIVF, R_LIVJEUN,
    R_LOGAUT, R_LOGGRAT, R_LOGLAN, R_LOGLOC, R_LOGLPART, R_LOGRAP, R_LOGSEC,
    R_LOGTER, R_LOGVAC, R_OBLI, R_PANIER, R_PARSOC, R_PATIMMO, R_PATPROF, R_PEA,
    R_PEAACT, R_PEASIC, R_PEATRE, R_PEL, R_PEP, R_PERP, R_POA, R_PROEXP,
    R_PRONEXP, R_RESPRIN, R_RETRAIT, R_SALCCB, R_SALSIC, R_SALSICPE, R_SALTIT,
    R_SALTITPE, R_SICPEA, R_SUR, R_TERHEXP, R_TERM, R_TERNEEXP, R_TERPRO,
    R_TNXBAT, R_TNXBOI, R_TNXLAB, R_TNXLOC, R_TNXNBA, R_TNXVIA, R_TXBAT,
    R_TXBOI, R_TXLAB, R_TXNBA, R_TXVIA, R_VEHIC, R_VMOB, STOCPP,STOCPRO,SURFTR,
    
    # Événements de vie
    EVEAUGM, EVEAUGM_AUT, EVEAUGM_BOU, EVEAUGM_HER, EVEAUGM_JEU, EVEAUGM_LOG,
    EVEAUGM_PRIM, EVEAUGM_PROF, EVEAUGM_REV, EVEAUGM_TER, EVEBAIS, EVEBAIS_ACC,
    EVEBAIS_AUT, EVEBAIS_BOU, EVEBAIS_CHO, EVEBAIS_CONS, EVEBAIS_DIV, EVEBAIS_FAIL,
    EVEBAIS_INFL, EVEBAIS_JEU, EVEBAIS_LOG, EVEBAIS_MAL, EVEBAIS_PROF, EVEBAIS_REV,
    EVEBAIS_VEU,
    
    # Patrimoine détaillé
    GMTDETC, GMTELO, GMTELOC, GMTPEP, GMTPERP, GMTPERPC,
    GMTRET, GMTRETC, GMTSTOC, GPRKDU, HERPAT, HERTABD, HERTABH, HODAN1, HODAN10,
    HODAN2, HODAN3, HODAN4, HODAN5, HODAN6, HODAN7, HODAN8,
    HODAN9, INDEPA, LOGPROPRI, LOYER, MOBACH, MOBAUT, MOBINFO, MOBPLAN, MPANEL,
    NBASS, NBCC, NBLOG, NBUC, NCOUPLES, NE, PATFISOM, PATIMM,
    PATPROFENT, PATPROFHENT, MTRESTE, MTDETTES, MTDETPRIV, MTDETPROFENT, MTDETPROFHENT,
    
    # Variables spécifiques
    SALACH, SALAUT, SALINFO, SALPAR, SALPLAN, SCOLACJ, SCOLAPR, TU, TYPLOG,
    TYPMEN15, TYPMEN5, VIAG, ZEAT, HERDON,
    CONSEP, INDEP, CONSDEP,
    GMTLIVC, GMTAVIC, GMTSALC, GMTLIV, GMTAVI, GMTSAL,
    R_ACTION, R_ACT, R_ACTNCT, R_PRONEEXP, R_PEPBANC, R_PEPASSUR,
    R_PROHEXP, R_PROPRO, DETPRO, DETPROFENT, R_ACTPEA, R_ACTTOT, R_ACTN,
    R_SCPI, R_SICAV, R_BNXBAT, R_BNXCHE, R_BNXFDS, R_BNXMAT, R_BNXSTO, R_ASDEC,
    R_ASDECN, R_ASDECV, STOC,
    
    # Variables déclaratives (version déclarée des variables)
    # Absente dans cette base 
    
    # Variables de pondération et métadonnées
    MOENQ, ANENQ, PATFI, PATRIB, PATRIA,
    PATFISOM, PATIMM,
    PATPROFHENT, MTPRETS, MTVEHIC, MTVOIT, MTPATRIAC, MTRESTE, PATPROFENT,
    PATRI_BRUT, PATRI_BRUT_HORSRESTE, MTDETPROFHENT, MTDETPRIV, MTDETTES,
    PATRI_NET, PATRI_NET_HORSRESTE, ZRAC, ZETRANGER, ZPENALIV, ZIMPOM, ZTHAB,
    ZVAMM, ZFONCIER, ZVALM, ZPENALIR, ZCHOMAGE, ZRAG, ZRIC, ZRNC,
    ZRENTES, ZSALAIRES, ZRETRAITES,
    ZPSOCM, ZCSGIM, ZCSGDM, ZCRDSM, ZIMPVALM, ZCSGREVPAT, ZREVDEC, ZPRODFIN,
    ZREVDISP, ZNIVVIE, ZREVFIN, ZCSGCRDS, 
    
    DETIMM, DETPRIV, R_PATFINANC, CALIM, CCHARG, DIF)

# Passage de 922 varibales à  332 variables
names(menage_2014)
# Menage 
# 1) Expliquer les taux de detention des livrets LA, LDDS, LEP par les ménages à partir des carractéristiques individuelles : Tri-probit, 
# prévention du taux de detention via des modèles de ML (test)
# 2) Expliquer le montant de detention dans les différents compte LA, LDDS, LEP à partir des caractéristiques individuelles : Tobit, avec les seuils de montant
# 3) Abitrage entre les produits (modèle de choix de portefueille)
# 4) Explorer l'évolution des comportements d'épargne dans le temps (Panel)

# Construction de la base de donnée des d'études : MENAGE-PRODUIT

# Les dépéndantes, y compris les var montants 
menage_db_2014$R_LIVABL
menage_db_2014$R_LDD
menage_db_2014$R_LEP

# ================================================================================================================
# Age : AGE_RELATIF, AGE_REEL
# Taux d'intérêt : TLA, TLDDS, TLEP
# Désir de possession, norme sociale : Position dans la distribution du patrimoine : DECILE_PATRIMOINE
# Revenu : Revenu disponible : ZREVDISP
# Revenu permanent anticipée : 
# Revenu relatif : Position dans la ditrsibution du revenu des menages : DECILE_REVENU
# Transmission : Heritage et retraite
# Liquidité : 
# Aversion au risque, aversion à la perte : 
# Detention de la résidence principale: 
# Niveau de diplôme : NIVEDUC 
# Nombre d'enfant dans le menage : 
# Situation matrimoniale : Personne de référence
# Situation emploi 
# CSP : 
# Raison d'épargner : CONSEP
# Nationalité : 
# ==================================================================================================================
# Nettoyage de la base de donnée pour les manquants
Taux_manquant = round(colMeans(is.na(menage_db_2014)) * 100,2)

# Transformer en tableau
table_na <- data.frame(Variable = names(Taux_manquant),Taux_NA = Taux_manquant)
table_na <- table_na[order(-table_na$Taux_NA), ]
View(table_na)

# Les explicatives d'après la littérature
# AGE
menage_db_2014$ANAISPR

# Age relatif
menage_db_2014$AGE_RELATIF = 2015-menage_db_2014$ANAISPR

# Age REEL 
menage_db_2014$AGE_REEL= 2026-menage_db_2014$ANAISPR

# RECODAGE DE L'AGE
menage_db_2014$CATAGE_RELATIF=NA

menage_db_2014$CATAGE_RELATIF[menage_db_2014$AGE_RELATIF <= 14] = 1                              #0 à 14 ans
menage_db_2014$CATAGE_RELATIF[menage_db_2014$AGE_RELATIF > 14 & menage_db_2014$AGE_RELATIF <=29] = 2 #15 à 29 ans
menage_db_2014$CATAGE_RELATIF[menage_db_2014$AGE_RELATIF > 29 & menage_db_2014$AGE_RELATIF <=44] = 3 #30 à 44 ans
menage_db_2014$CATAGE_RELATIF[menage_db_2014$AGE_RELATIF > 44 & menage_db_2014$AGE_RELATIF <=59] = 4 #45 à 59 ans
menage_db_2014$CATAGE_RELATIF[menage_db_2014$AGE_RELATIF > 59 & menage_db_2014$AGE_RELATIF <=74] = 5 #60 à 74 ans
menage_db_2014$CATAGE_RELATIF[menage_db_2014$AGE_RELATIF > 74] = 6                               #75 ans ou plus 

mean(is.na(menage_db_2014$CATAGE_RELATIF)) * 100 # Pas de NA 

# Suppression de l'année de naissance
menage_db_2014$ANAISPR<-NULL

# Decile patrimoine : Définis le niveau de richesse 
decile_patri_brut=round(with(menage_db_2014, wtd.quantile(menage_db_2014$PATRI_BRUT, weights = menage_db_2014$POND, probs = (1:9)/10),type = 'quantile'),0) 
decile_patri_brut

decile_df_patri_brut <- data.frame(decile = 1:9, Patribrut = decile_patri_brut)

seuils <- c(-Inf, decile_df_patri_brut$Patribrut, Inf)

menage_db_2014 <- menage_db_2014 %>%
  mutate(decile_patrimoine = cut(PATRI_BRUT,breaks = seuils,labels = 1:10,include.lowest = TRUE,include.upper = FALSE )) # Borne supérieure exclue
menage_db_2014$decile_patrimoine

# Revenu disponible : ZREVDISP    
sum(is.na(menage_db_2014$ZREVDISP))
mean(is.na(menage_db_2014$ZREVDISP))*100 # Pas de manquantss

# Calcul du revenu disponible (2017-2018)
# ZREVDISP = ZREVDEC + ZPRODFIN + ZPSOCM + ZM_PPEM - ZIMPOM - ZTHAB - ZIMPVALM - ZCSGIM - ZCSGDM - ZCSGREVPAT

sum(is.na(menage_db_2014$ZREVDEC)) 
sum(is.na(menage_db_2014$ZPRODFIN)) 
sum(is.na(menage_db_2014$ZPSOCM)) 
sum(is.na(menage_db_2014$ZM_PPEM)) 
sum(is.na(menage_db_2014$ZIMPOM))
sum(is.na(menage_db_2014$ZTHAB))  
sum(is.na(menage_db_2014$ZIMPVALM)) 
sum(is.na(menage_db_2014$ZCSGIM)) 
sum(is.na(menage_db_2014$ZCSGDM)) 
sum(is.na(menage_db_2014$ZCSGREVPAT)) 

#================ Formule du revenu disponible pour 2017 =================================================
#=============== BASE MENAGE 2017===================================================================
# Corriger les valeurs manquantes DE zrevdisp par zrevdisp1, 
# reconsituer ZREVDISP à partir de la table individu et comparer avec les valeurs de l'INSEE
# Supprimer les menages qui ont des ZREVDISP négatif 

#=================== Correction des valeurs negatives du revenu disponible===================================================
# Taux de manquants 
mean(is.na(menage_db_2014$ZREVDISP)) * 100

menage_db_2014 <- menage_db_2014 %>%
  mutate(revenus_activite = rowSums(cbind(
    coalesce(ZSALAIRES, 0),
    coalesce(ZCHOMAGE, 0),
    coalesce(ZRETRAITES, 0),
    coalesce(ZRENTES, 0),
    coalesce(ZPENALIR, 0),
    coalesce(ZRAG, 0),
    coalesce(ZRIC, 0),
    coalesce(ZRNC, 0),
    coalesce(ZFONCIER, 0),
    coalesce(ZVAMM, 0),
    coalesce(ZVALM, 0),
    coalesce(ZRAC, 0),
    coalesce(ZETRANGER, 0)),na.rm = TRUE),
    ZREVDISP1 =
      coalesce(ZREVDEC, revenus_activite - coalesce(ZPENALIV, 0)) +
      coalesce(ZPRODFIN, 0) +
      coalesce(ZPSOCM, 0) -
      coalesce(ZIMPOM, 0) -
      coalesce(ZTHAB, 0) -
      coalesce(ZIMPVALM, 0) -
      coalesce(ZCSGIM, 0) -
      coalesce(ZCSGDM, 0) -
      coalesce(ZCSGREVPAT, 0))

Voir2 = menage_db_2014 %>% 
  filter(ZREVDISP<0) %>% 
  select(ZREVDISP, ZREVDISP1, ZREVDEC, ZPRODFIN, ZPSOCM, ZIMPOM, ZTHAB, ZIMPVALM, ZCSGIM, ZCSGDM, ZCSGREVPAT)

View(Voir2) # 62 observations

individu_2014$ZSALAIRES_I # Salaire
individu_2014$ZCHOMAGE_I # CHOMAGE
individu_2014$ZRETRAITES_I # RETRAITE
individu_2014$ZRETRAITES_STRICTES_I # 
individu_2014$ZPENALIR_I # 
individu_2014$ZRIC_I # 
individu_2014$ZRNC_I
individu_2014$ZRAG_I 
individu_2014$ZRENTES_I

vars <- c("ZSALAIRES_I","ZCHOMAGE_I","ZRETRAITES_I","ZRETRAITES_STRICTES_I",
          "ZPENALIR_I","ZRIC_I","ZRNC_I","ZRAG_I","ZRENTES_I")

summary(individu_2014[, vars])

VoirI=individu_2014 %>% 
  filter(ZRIC_I < 0 | ZRNC_I < 0 | ZRAG_I < 0 ) %>% 
  # filter(ZREVDEC_I < 0) %>% 
  select("ZSALAIRES_I","ZCHOMAGE_I","ZRETRAITES_I","ZRETRAITES_STRICTES_I",
         "ZPENALIR_I","ZRIC_I","ZRNC_I","ZRAG_I","ZRENTES_I")

View(VoirI)

dim(individu_2014)

mean(is.na(individu_2014$ZSALAIRES_I)) * 100
mean(is.na(individu_2014$ZCHOMAGE_I)) * 100
mean(is.na(individu_2014$ZRETRAITES_I)) * 100
mean(is.na(individu_2014$ZRETRAITES_STRICTES_I)) * 100

# Reconstitution du revenu des menages
individu_2014$ZREVDEC_I = ifelse(is.na(individu_2014$ZSALAIRES_I), 0, individu_2014$ZSALAIRES_I) +
  ifelse(is.na(individu_2014$ZCHOMAGE_I), 0, individu_2014$ZCHOMAGE_I) +
  ifelse(is.na(individu_2014$ZRETRAITES_I), 0, individu_2014$ZRETRAITES_I) +
  ifelse(is.na(individu_2014$ZPENALIR_I), 0, individu_2014$ZPENALIR_I) +
  ifelse(is.na(individu_2014$ZRIC_I), 0, individu_2014$ZRIC_I) +
  ifelse(is.na(individu_2014$ZRNC_I), 0, individu_2014$ZRNC_I) +
  ifelse(is.na(individu_2014$ZRAG_I), 0, individu_2014$ZRAG_I) +
  ifelse(is.na(individu_2014$ZRENTES_I), 0, individu_2014$ZRENTES_I) 

# Je vais degager les observations ayant un revenu négatif ZREVDEC_I
mean(is.na(individu_2014$ZREVDEC_I)) # Pas de manquants 

# Voir les revenus négatifs 
Voir3=individu_2014%>%
  filter(ZREVDEC_I <0) %>% 
  select(ZREVDEC_I,"ZSALAIRES_I","ZCHOMAGE_I","ZRETRAITES_I","ZRETRAITES_STRICTES_I",
         "ZPENALIR_I","ZRIC_I","ZRNC_I","ZRAG_I","ZRENTES_I")

View(Voir3)

# Virer
individu_2014 <- individu_2014 %>%
  filter(ZREVDEC_I >=0)

# Calcul du revenu des ménages
View(individu_2014)

REV_MENAGE=individu_2014 %>% 
  group_by(IDENT) %>% 
  summarise(ZREVDEC1=sum(ZREVDEC_I)) %>% 
  select(IDENT,ZREVDEC1)

View(REV_MENAGE)

comparaison_rev <- REV_MENAGE %>%
  left_join(menage_db_2014 %>% select(IDENT, ZREVDISP, ZREVDISP1),
            by = "IDENT") %>%
  select(IDENT, ZREVDISP, ZREVDISP1, ZREVDEC1)

View(comparaison_rev) 

# Dernière correction
# - Si ZREVDISP est <0 et ZREVDEC1 > 0 alors conserver ZREVDEC1 à la place de ZREVDISP
# - Si ZREVDISP est NA et ZREVDEC1 > 0 alors remplacer par ZREVDEC1

menage_db_2014<- menage_db_2014 %>%
  left_join(REV_MENAGE %>% select(IDENT, ZREVDEC1), by = "IDENT") 

menage_db_2014 = menage_db_2014 %>%
  mutate(ZREVDISP = case_when(is.na(ZREVDISP) & ZREVDEC1 > 0 ~ ZREVDEC1,
                              ZREVDISP < 0 & ZREVDEC1 > 0 ~ ZREVDEC1,TRUE ~ ZREVDISP))

sum(is.na(menage_db_2014$ZREVDISP)|menage_db_2014$ZREVDISP < 0) # 32 

# Recupérer les valeurs negatives 
Voir2 = menage_db_2014 %>% 
  filter(ZREVDISP < 0) %>% 
  select(ZREVDISP,ZREVDISP1, ZREVDEC1)

View(Voir2)

# Je vais shooter les ZREVDISP négatif, et NA
sum(is.na(menage_db_2014$ZREVDISP) | menage_db_2014$ZREVDISP < 0) # Nombre 32

mean(is.na(menage_db_2014$ZREVDISP) | menage_db_2014$ZREVDISP < 0) * 100 # Percent : 0.27%

menage_db_2014 = menage_db_2014 %>% 
  filter(!is.na(menage_db_2014$ZREVDISP) & menage_db_2014$ZREVDISP >= 0)

mean(menage_db_2014$ZREVDISP<0) * 100

#========================== Suppression des variables qui entrent dans le calcul du Revenu disponible =======================
# Idée : Eviter les autocorrelations
# ZREVDISP = ZREVDEC + ZPRODFIN + ZPSOCM + ZM_PPEM - ZIMPOM - ZTHAB - ZIMPVALM - ZCSGIM - ZCSGDM - ZCSGREVPAT
# ZREVDEC = sum (ZSALAIRES, ZCHOMAGE, ZRETRAITES, ZRENTES, ZPENALIR, ZRAG, ZRIC,
# ZRNC, ZFONCIER, ZVAMM, ZVALM, ZRAC, ZETRANGER, – ZPENALIV)

vars_a_supprimer <- c(
  # composantes directes de ZREVDISP
  "ZREVDEC", "ZPRODFIN", "ZPSOCM", "ZM_PPEM",
  "ZIMPOM", "ZTHAB", "ZIMPVALM", "ZCSGIM", "ZCSGDM", "ZCSGREVPAT",
  
  # composantes de ZREVDEC
  "ZSALAIRES", "ZCHOMAGE", "ZRETRAITES", "ZRENTES",
  "ZPENALIR", "ZRAG", "ZRIC", "ZRNC", "ZFONCIER",
  "ZVAMM", "ZVALM", "ZRAC", "ZETRANGER", "ZPENALIV")

menage_db_2014 <- menage_db_2014[, !(names(menage_db_2014) %in% vars_a_supprimer)]

#============================================================================================================================
# Revenu relatif : Position dans la ditrsibution du revenu des menages : DECILE_REVENU
decile_revenu=with(menage_db_2014, wtd.quantile(menage_db_2014$ZREVDISP, weights = menage_db_2014$POND, probs = (1:10)/10), 
                   type = 'quantile') #Calcul du décile des revenus

decile_df_revenu <- as.data.frame(decile_revenu[1:9])
decile_df_revenu

seuils <- c(-Inf, decile_df_revenu$decile_revenu[1:9], Inf)
seuils

# YES 
menage_db_2014 <- menage_db_2014 %>%
  mutate(
    decile_revenu = cut(
      ZREVDISP,
      breaks = seuils,
      labels = 1:10,
      include.lowest = TRUE,
      include.upper = FALSE # Borne supérieure exclue 
    )
  )

menage_db_2014$decile_revenu

#======================== Transmission : Heritage et retraite
# DONS : MTDONRC
transmission_2014$MTDONRC

# HERITAGE : 
View(transmission_2014)

men_transmission <-transmission_2014 %>% 
  filter(!is.na(IDENT) & !(IDENT == "")) %>% 
  group_by(IDENT) %>% 
  summarise(Heritage=sum(MTHERC,na.rm = TRUE),
            Donation_R=sum(MTDONRC,na.rm=TRUE),
            Donation_V=sum(MTDONRC,na.rm=TRUE)) %>% 
  select(IDENT,Heritage,Donation_R,Donation_V)

View(men_transmission) 

menage_tert_2014=menage_db_2014 %>% 
  left_join(men_transmission,by="IDENT")

View(menage_tert_2014)

dim(menage_db_2014)
dim(menage_tert_2014)

#========================= Tous les produits de detention au niveau menage : 
# Endettement prive ou pro :  DET
menage_tert_2014$DET 

# Endettement pro : DETPRO *
menage_tert_2014$DETPRO

# Patrimoine professionnel
menage_tert_2014$PATRIPRO = (menage_tert_2014$PATPROFHENT + menage_tert_2014$PATPROFENT) 

menage_tert_2014$PATPROFHENT = NULL
menage_tert_2014$PATPROFENT = NULL

# Patrimoine résiduel
menage_tert_2014$PATRIRES= menage_tert_2014$MTRESTE 

menage_tert_2014$MTRESTE = NULL

# Résidence principale
menage_tert_2014$R_RESPRIN

# Autres logements : R_LOGAUT
menage_tert_2014$R_LOGAUT

# Assurance vie : R_AVI
menage_tert_2014$R_AVI

# Epargne logement : 
menage_tert_2014$R_LEE

# Livret Jeune 
menage_tert_2014$R_LIVJEUN

# Valeurs mobilières
menage_tert_2014$R_VMOB

# Livret Impot
menage_tert_2014$R_LIVF

# PEL
menage_tert_2014$R_PEL

# CEL
menage_tert_2014$R_CEL

# AVI
menage_tert_2014$R_AVI

# Epargne retraite 
menage_tert_2014$R_PERP

# PEP
menage_tert_2014$R_PEP

# Valeurs mobilières 
menage_tert_2014$R_VMOB

# Epargne salariale 
menage_tert_2014$R_EPSAL

# Niveau de diplôme : NIVEDUC
menage_tert_2014$NIVEDUC=NA

menage_tert_2014$NIVEDUC[menage_tert_2014$DIPLOMPR %in% c(00,70,71,"","00")] = 1 #Sans diplome ou CEP
menage_tert_2014$NIVEDUC[menage_tert_2014$DIPLOMPR %in% c(45,51,52,53,60)] = 2 #brevet
menage_tert_2014$NIVEDUC[menage_tert_2014$DIPLOMPR %in% c(41,42,43,44)] = 3 #bac ou bac+1
menage_tert_2014$NIVEDUC[menage_tert_2014$DIPLOMPR %in% c(31,32,33,34,35,36)] = 4 #bac+2
menage_tert_2014$NIVEDUC[menage_tert_2014$DIPLOMPR %in% c(22,23,24)] = 5 #bac+3
menage_tert_2014$NIVEDUC[menage_tert_2014$DIPLOMPR %in% c(21)] = 6 #bac+4 
menage_tert_2014$NIVEDUC[menage_tert_2014$DIPLOMPR %in% c(13,14,15,16,17)] = 7 #bac+5
menage_tert_2014$NIVEDUC[menage_tert_2014$DIPLOMPR %in% c(11,12)] = 8    #bac+8

mean(is.na(menage_tert_2014$NIVEDUC)) * 100 

V=menage_tert_2014 %>% 
#  filter(is.na(NIVEDUC)) %>%  
  select(DIPLOMPR, NIVEDUC) 

View(V)

menage_tert_2014$DIPLOMPR<-NULL 

# Nombre d'enfant dans le menage : 
menage_tert_2014$NENFANTS

# Situation matrimoniale : Personne de référence
menage_tert_2014$MATRIPR

# Situation emploi 
menage_tert_2014$SITUAPR

# CSP : 
menage_tert_2014$CSPR8=NA

menage_tert_2014$CSPR8[menage_tert_2014$CSPR %in% c(10:13)] = 1 #Agriculteurs exploitants 
menage_tert_2014$CSPR8[menage_tert_2014$CSPR %in% c(21:23)] = 2 #Artisans, commerçants et chefs d'entreprise
menage_tert_2014$CSPR8[menage_tert_2014$CSPR %in% c(31,33:35,37:38)] = 3 #Cadres et professions intellectuelles supérieures
menage_tert_2014$CSPR8[menage_tert_2014$CSPR %in% c(42:48)] = 4 #Professions Intermédiaires
menage_tert_2014$CSPR8[menage_tert_2014$CSPR %in% c(50,52:56)] = 5 #Employés
menage_tert_2014$CSPR8[menage_tert_2014$CSPR %in% c(60,62:69)] = 6 #Ouvriers
menage_tert_2014$CSPR8[menage_tert_2014$CSPR %in% c(71,72,74,75,77,78)] = 7 #Retraités
menage_tert_2014$CSPR8[menage_tert_2014$CSPR %in% c("00",81,83,84,85,86,"")] = 8 #Autres personnes sans activité professionnelle

menage_tert_2014$CSPR8

menage_tert_2014$CSPR<- NULL

# Raison d'épargner : CONSEP
menage_tert_2014$CONSEP

# Nationalité : 
menage_tert_2014$NATIO7PR

# Calcul des montants des produits financiers et rapatriés dans la table menage
# Mettre le montant des livrets au plafond afin d'avoir des montants corrects
# LA (177)
Vue = produit_2014 %>%
  filter(FINNA %in% c("02","03") & MONTCLA > 28000) %>% 
  select(IDENT,MONTCLA)

View(Vue)

# Coiser avecle revenu des menages
Revenu_menage = menage_tert_2014 %>% 
  select(IDENT, ZREVDISP,PATFISOM, PATRI_NET) 

View(Revenu_menage)

LA_REV=Vue %>% 
  left_join(Revenu_menage,by="IDENT")

View(LA_REV)

nrow(LA_REV)/nrow(menage_tert_2014) * 100 # 1,30% 

# LDDS
Vue = produit_2014 %>%
  filter(FINNA %in% c("04") & MONTCLA > 15000) %>% 
  select(IDENT,MONTCLA)

View(Vue)

# Coiser avecle revenu des menages
Revenu_menage = menage_tert_2014 %>% 
  select(IDENT, ZREVDISP,PATFISOM, PATRI_NET) 

View(Revenu_menage)

LDDS_REV=Vue %>% 
  left_join(Revenu_menage,by="IDENT")

View(LDDS_REV)

nrow(LDDS_REV)/nrow(menage_tert_2014) * 100 # 1,77% 

# LEP 
Vue = produit_2014 %>%
  filter(FINNA %in% c("05") & MONTCLA > 13000) %>% 
  select(IDENT,MONTCLA)

View(Vue)

# Coiser avecle revenu des menages
Revenu_menage = menage_tert_2014 %>% 
  select(IDENT, ZREVDISP,PATFISOM, PATRI_NET) 

View(Revenu_menage)

LEP_REV=Vue %>% 
  left_join(Revenu_menage,by="IDENT")

View(LEP_REV)

nrow(LEP_REV)/nrow(menage_tert) * 100 # 0.51%

# Mettre les valeurs de ces individus au plafond
produit_2014 <- produit_2014 %>%
  mutate(
    MONTCLA = if_else(
      FINNA %in% c("02", "03") & MONTCLA > 28000, 28000,
      if_else(
        FINNA == "04" & MONTCLA > 15000, 15000,
        if_else(
          FINNA == "05" & MONTCLA > 13000, 13000,
          MONTCLA))))

Vue <- produit_2014 %>%
  filter((FINNA %in% c("02", "03") & MONTCLA == 28000)) %>%
  select(IDENT, FINNA, MONTCLA)

View(Vue)

# On est bon sur la vague 2020 par rapport aux valeurs négatives
produit_t_2014 = as_tibble(produit_2014)

Montant_produit = produit_t_2014 %>%
  group_by(IDENT) %>% 
  summarise(MRESPRIN=round(sum(MONTCLA * (LOGNA %in% c("01")), na.rm = TRUE), 0),          # Résidence Principale
            MAUTLOG=round(sum(MONTCLA * (LOGNA %in% c("02","03","04")), na.rm = TRUE), 0), # Autres logements
            MLA = round(sum(MONTCLA * (FINNA %in% c("02","03")), na.rm = TRUE), 0),        # LA ou LBLEU
            MLDDS=round(sum(MONTCLA * (FINNA %in% c("04")),na.rm = TRUE), 0),              # LDDS
            MLEP=round(sum(MONTCLA * (FINNA %in% c("05")),na.rm = TRUE), 0),               # LEP
            MLIVJEUNE=round(sum(MONTCLA * (FINNA %in% c("06")),na.rm = TRUE), 0),          # Livret Jeune
            MLIVIMPOT=round(sum(MONTCLA * (FINNA %in% c("07")),na.rm = TRUE), 0),          # Livrets soumis à l’impôt
            MPEL=round(sum(MONTCLA * (FINNA %in% c("08")),na.rm = TRUE), 0),               # PEL
            MCEL=round(sum(MONTCLA * (FINNA %in% c("09")),na.rm = TRUE), 0),               # CEL
            MAVI=round(sum(MONTCLA * (FINNA %in% c("20")),na.rm = TRUE), 0),               # Assurance-vie
            MEPRETRAITE=round(sum(MONTCLA*(FINNA %in% c("16","18","19")),na.rm = TRUE), 0),# Épargne retraite (y c. PERP)
            MPEP=round(sum(MONTCLA*(FINNA %in% c("17")),na.rm = TRUE), 0),                 # PEP
            MVMOB=round(sum(MONTCLA*
                              (FINNA %in% c("11","12","13","14","22","23","24","25","26","27","28","29","30")),
                            na.rm = TRUE), 0),                                                              # Valeurs mobilières
            MEPSALAR=round(sum(MONTCLA*(FINNA %in% c("10")),na.rm = TRUE), 0),              # Épargne salariale
            MBONCAISSE=round(sum(MONTCLA*(FINNA %in% c("31")),na.rm = TRUE), 0),            # Bons d’épargne, bons de caisse ou bons du trésor
            MCOMPTERM=round(sum(MONTCLA * (FINNA %in% c("32")), na.rm = TRUE), 0),          # Compte à terme
            MCASSOCIE=round(sum(MONTCLA * (FINNA %in% c("33")), na.rm = TRUE), 0),          # Compte courant d’associé
            MLEE=round(sum(MONTCLA * (FINNA %in% c("34")), na.rm = TRUE), 0) )              # LEE

View(Montant_produit)
dim(Montant_produit)

# Par précaution vérifier les variables qui ont une valeur dans ces variables 
Y=Montant_produit %>%
  filter( MRESPRIN < 0 |MAUTLOG < 0 |   MLA < 0 |   MLDDS < 0 | MLEP < 0 |
            MLIVJEUNE < 0 | MLIVIMPOT < 0 |MPEL < 0 | MCEL < 0 |MAVI < 0 |MEPRETRAITE < 0 |
            MPEP < 0 |MVMOB < 0 | MEPSALAR < 0 |MBONCAISSE < 0 |MCOMPTERM < 0 |MCASSOCIE < 0 |MLEE < 0)

View(Y)

# Ramener dans la table en construction 
menage_tert_2014=menage_tert_2014 %>% 
  left_join(Montant_produit,by="IDENT")

# Les autres detentions des ménages
# Detention de la Dette Pro : 
menage_tert_2014$DET

# Detention de la Dette Immo : 
menage_tert_2014$DETIMM

# Detention de la Dette privé : 
menage_tert_2014$DETPRIV

# Nombre d'entreprise detenu par le menage (Corrigé les NA)
menage_tert_2014$NE

# Pret d'argent consenti par le menage
menage_tert_2014$PRET

# Detention d'action côté
menage_tert_2014$R_ACT

# Detention d'action côté et parts de banques
menage_tert_2014$R_ACTION 

# Detention d'action non côté 
menage_tert_2014$R_ACTN

# Detention d('assurance décès)
menage_tert_2014$R_ASDEC

# Detention de batiment
menage_tert_2014$R_BNXBAT

# Detention de stoCKS 
menage_tert_2014$R_BNXSTO

# Detention bon de Capi
menage_tert_2014$R_CAPI

# Detention retraite complémentaire
menage_tert_2014$R_COMP

# Detention de compte titre
menage_tert_2014$R_CTIT

# Detention d'emprunt pour les Etudes 
menage_tert_2014$R_DETETU 

# Detention d'emprunt pour autre achat immobilier que la RP
menage_tert_2014$R_DETIM

# Detention d('emprunt pour la RP
menage_tert_2014$R_DETRE

# Detention pour la restructuration de la Dette 
menage_tert_2014$R_DETRES

# Detention de la rente education 
menage_tert_2014$R_EDUC

# Detention d'entrepise :
menage_tert_2014$R_ENT

# La région ou la zone géographique du menage
menage_tert_2014$ZEAT

# Detention des résidences secondaires 
menage_tert_2014$R_LOGSEC

# Detention de logements vacants
menage_tert_2014$R_LOGVAC

# Detention d'obligation sur un comptre titre 
menage_tert_2014$R_OBLI

# Detention d'actif financier hors compte courant 
menage_tert_2014$R_PATFINANC

# Detention patrimone Immobilier
menage_tert_2014$R_PATIMMO

# Detention de patrimoine professionnelle
menage_tert_2014$R_PATPROF

# Detention de PEA 
menage_tert_2014$R_PEA

# Detention Sicav, FCP 
menage_tert_2014$R_PEASIC

# Detention de stock-options 
menage_tert_2014$R_POA

# Taille de l'Unité Urbaine
menage_tert_2014$TU

# Tyep de Menage 
menage_tert_2014$TYPMEN5

# Attitude face au risque (Aversion au risque, aversion à la perte)
menage_tert_2014$RISKFI

# Montant des dépenses mensuelles pour l'alimentation 
menage_tert_2014$CALIM

# Charges mensuelles (eaux, éléctricité, gaz, chauffage et communication)
menage_tert_2014$CCHARG

# Dépenses courantes (Charges totales des ménages)
menage_tert_2014$CNDUR

# Attentes en terme de pouvoir d'achat futur(année suivante) (anticipation du revenu futur)
menage_tert_2014$DIEXP

# Difficulté à faire face aux échéances 
menage_tert_2014$DIF

# Changement de le ménage 
menage_tert_2014$RMODIF_CHO
menage_tert_2014$RMODIF_DC
menage_tert_2014$RMODIF_DEP
menage_tert_2014$RMODIF_EVOL
menage_tert_2014$RMODIF_SEP
menage_tert_2014$RMODIF_MAL
menage_tert_2014$RMODIF_FORM

# Montant total des ressources du menage du mois 
menage_tert_2014$TOTREVENC

# 2) Correction des valeurs manquantes
Taux_manquant = round(colMeans(is.na(menage_tert_2014)) * 100,2)
table_na <- data.frame(Variable = names(Taux_manquant),Taux_NA = Taux_manquant)


# Trier par ordre :
table_na <- table_na[order(-table_na$Taux_NA), ]
View(table_na)

# Suppression des variables qui ont que des manquants
menage_tert_2014$HODAN10 <- NULL
menage_tert_2014$GMTPERPC <- NULL
menage_tert_2014$GMTSALC<- NULL
menage_tert_2014$HODAN9 <- NULL
menage_tert_2014$GMTELOC<- NULL
menage_tert_2014$GMTSTOC<- NULL
menage_tert_2014$GMTRETC<- NULL
menage_tert_2014$HODAN8 <- NULL
menage_tert_2014$GMTDETC<-NULL
menage_tert_2014$GMTVALMOBC <- NULL
menage_tert_2014$GMTAVIC<- NULL
menage_tert_2014$HODAN7<- NULL
menage_tert_2014$HODAN6<- NULL
menage_tert_2014$HODAN5<- NULL
menage_tert_2014$GMTLIVC<-NULL
menage_tert_2014$ALGMEN<- NULL
menage_tert_2014$HODAN4<- NULL
menage_tert_2014$HODAN3<- NULL
menage_tert_2014$HODAN2<- NULL
menage_tert_2014$HODAN1<- NULL
menage_tert_2014$AGECJ<- NULL
menage_tert_2014$MNAISCJ<-NULL

V=menage_tert_2014 %>% 
  filter(R_ENT== 2 & is.na(NE)) %>% 
  select(NE, R_ENT) 

View(V)

W=menage_tert_2014 %>% 
  filter(is.na(LOYER)) %>% 
  select(LOYER, R_RESPRIN, STOC,STOCPRO,SURFTR, ZEAT, TYPMEN15,SITUAPR)

View(W)

sum(is.na(menage_tert_2014$LOYER)) 

W=menage_tert_2014 %>% 
  # filter(is.na(RESPRIACH)) %>% 
  select(RESPRIACH,MRESPRIN, R_RESPRIN)

View(W)

menage_tert_2014$MRESPRIN

# Corection des NA PAR 0 (Revoir les 0, repecher certains)
menage_tert_2014$GPRKDU = ifelse(is.na(menage_tert_2014$GPRKDU), 0, menage_tert_2014$GPRKDU) 

menage_tert_2014$NE = ifelse(is.na(menage_tert_2014$NE), 0, menage_tert_2014$NE) # Seul les personnes qui deteinnent une entreprise ont declaré le nb 

menage_tert_2014$LOYER <- ifelse(is.na(menage_tert_2014$LOYER) & menage_tert_2014$STOC %in% c(1, 2, 3, 5, 6), 0,menage_tert_2014$LOYER) # Traité

# Imputation par la médiane par region et par m² carré
sum(is.na(menage_tert_2014$LOYER)) 

menage_tert_2014 <- menage_tert_2014 %>%
  group_by(ZEAT, SURFTR) %>%
  mutate(LOYER = if_else(STOC==4 & is.na(LOYER),median(LOYER, na.rm = TRUE),LOYER)) %>%
  ungroup()

menage_tert_2014$RESPRIACH = NULL # Montant déjà disponble et au mieux à partir de produit 

W=menage_tert_2014 %>% 
  # filter(is.na(RESPRIACH)) %>% 
  select(TOTREVENC,ZREVDISP)

View(W)

W=menage_tert_2014 %>% 
  filter(CNDUR == 0) %>% 
  select(CNDUR,CCHARG,CALIM,TYPMEN5)

View(W)

menage_tert_2014$TOTREVENC = NULL

mean(is.na(menage_tert_2014$CNDUR)) * 100
mean(is.na(menage_tert_2014$CCHARG)) * 100 
mean(is.na(menage_tert_2014$CALIM)) * 100 

menage_tert_2014$CNDUR <- ifelse(
  is.na(menage_tert_2014$CNDUR),
  ifelse(is.na(menage_tert_2014$CCHARG), 0, menage_tert_2014$CCHARG) +
    ifelse(is.na(menage_tert_2014$CALIM), 0, menage_tert_2014$CALIM), menage_tert_2014$CNDUR)

menage_tert_2014$CNDUR 
menage_tert_2014$CCHARG = NULL
menage_tert_2014$CALIM = NULL

# Correction des montants
menage_tert_2014 <- menage_tert_2014 %>%
  mutate(Heritage   = ifelse(is.na(Heritage), 0, Heritage),
         Donation_R = ifelse(is.na(Donation_R), 0, Donation_R),
         Donation_V = ifelse(is.na(Donation_V), 0, Donation_V))

vars_montant <- c(
  "MLA", "MLDDS", "MLEP", "MRESPRIN", "MAUTLOG",
  "MLIVJEUNE", "MLIVIMPOT", "MPEL", "MCEL", "MAVI",
  "MEPRETRAITE", "MPEP", "MVMOB", "MEPSALAR",
  "MBONCAISSE", "MCOMPTERM", "MCASSOCIE", "MLEE")

menage_tert_2014 <- menage_tert_2014 %>%
  mutate(across(all_of(vars_montant), ~ ifelse(is.na(.), 0, .)))

#====================== OK =============== Fin de la correction des valeurs manquantes================================================  
View(menage_tert_2014)

#======================================== Vérifier les valeurs manquantes =======================================================#
Taux_na <- round(colSums(is.na(menage_tert_2014)) * 100, 2)
Taux_na

table_na <- data.frame(
  Variable = names(Taux_na),
  Taux_NA = Taux_na)

table_na <- table_na[order(-table_na$Taux_NA), ]
View(table_na)

menage_tert_2014$ZREVDEC1 <- NULL # Plus besoin 
menage_tert_2014$MNAISPR <- NULL # Plus besoin
menage_tert_2014$NBCC <- NULL # Plus besoin
menage_tert_2014$NCOUPLES <- NULL # Plus besoin

#============================== Correction des valeurs aberrantes et vérifications des variables====================#
#===== Information
# 90 % au centre → z=1,645
# 95 % au centre → z=1,960
# 98 % au centre → z=2,326
# 99 % au centre → z=2,576
#=========================

#============================== Quantitatives =======================================================================
#============ Le revenu disponible
# Pondération
design <- svydesign(
  id = ~1,                # Pas de grappes
  weights = ~menage_tert_2014$POND, # Variable de poids
  data = menage_tert_2014)

# Moyenne 
weighted.mean(menage_tert_2014$ZREVDISP,menage_tert_2014$POND)

# Median
svyquantile(~menage_tert_2014$ZREVDISP,design,quantiles = 0.5,na.rm = TRUE)        

# Quantile
Q=wtd.quantile(menage_tert_2014$ZREVDISP, weights = menage_tert_2014$POND, probs = c(0.25, 0.5, 0.75))
Q

# Décile 
with(menage_tert_2014, wtd.quantile(menage_tert_2014$ZREVDISP, weights = menage_tert_2014$POND, probs = (1:10)/10),type = 'quantile') 

# Distribution 
ggplot(menage_tert_2014, aes(x = ZREVDISP, weight = POND)) + geom_histogram(bins = 30)

ggplot(menage_tert_2014, aes(x = log(ZREVDISP +1 ), weight = POND)) + geom_histogram(bins = 30) 

boxplot(menage_tert_2014$ZREVDISP) # Avant

ggplot(menage_tert_2014,aes(x = ZREVDISP, weight = POND)) + geom_density(fill = "steelblue", alpha = 0.4) +
  labs(title = "Densité pondérée du revenu disponible", x = "Revenu disponible",y = "Densité") +
  theme_minimal()

# Log pour voir
ggplot(menage_tert_2014, aes(x = ZREVDISP, weight=POND)) +
  geom_density(fill = "steelblue", alpha = 0.4) +
  scale_x_log10() +
  labs(x = "Revenu disponible (échelle log)", y = "Densité")

# Enlever les valeurs très extrêmes selon un principe de distribution normale
menage_tert_2014$ZREVDISP1= NULL 

# #=============== Approche pas adapté car revenu asymétrique

V=menage_tert_2014 %>% 
  filter(ZREVDISP > 1160000) %>%
  select(ZREVDISP)
#   summarise(n())

View(V)

# Suppression des valeurs extrêmes
menage_tert1_2014 <- subset(menage_tert_2014,ZREVDISP <= 1160000)

# Log pour voir
ggplot(menage_tert1_2014, aes(x = ZREVDISP, weight=POND)) +
  geom_density(fill = "steelblue", alpha = 0.4) +
  scale_x_log10() +
  labs(x = "Revenu disponible (échelle log)", y = "Densité")

# ================== Observer
# Avant
ggplot(menage_tert_2014,aes(x = ZREVDISP,weight = POND)) +  geom_density(fill = "steelblue", alpha = 0.4) +
  labs(title = "Densité pondérée du revenu disponible",x = "Revenu disponible",y = "Densité") + theme_minimal()

# Après correction des extrêmes
ggplot(menage_tert1_2014,aes(x = ZREVDISP,weight = POND)) +  geom_density(fill = "steelblue", alpha = 0.4) +
  labs(title = "Densité pondérée du revenu disponible",x = "Revenu disponible",y = "Densité") + theme_minimal()

#================= TEST DE NORMALITE SUR LA CORRIGE VIA QQPLOT====================================================================
# Probabilités
p <- ppoints(sum(!is.na(menage_tert1_2014$ZREVDISP)))

# Variable et poids
x <- menage_tert1_2014$ZREVDISP
w <- menage_tert1_2014$POND

# Moyenne et écart-type pondérés
m <- weighted.mean(x, w, na.rm = TRUE)
s <- sqrt(wtd.var(x, weights = w, na.rm = TRUE))

# Quantiles observés pondérés
q_obs <- wtd.quantile(x,weights = w,probs = p,na.rm = TRUE)

# Quantiles théoriques de la normale N(m, s²)
q_theo <- qnorm(p,mean = m,sd = s)

# QQ-plot
plot(q_theo,q_obs,xlab = "Quantiles théoriques normaux",ylab = "Quantiles observés pondérés", main = "QQ-plot pondéré de ZREVDISP")

# Droite théorique de normalité
abline(0, 1, col = "red", lwd = 2)

# Le revenu n'est pas normal est lui même, passage au log pour stabiliser, reduire l'asymétrie de la distribution

# ========================== PASSAGE AU LOGARITHME ===================================================================================
ggplot(menage_tert1_2014,aes(x = log(ZREVDISP),weight = POND)) +  geom_density(fill = "steelblue", alpha = 0.4) +
  labs(title = "logarithme de la Densité du revenu disponible pondéré",x = "Revenu disponible",y = "Densité") + theme_minimal()

menage_tert1_2014$logZREVDISP = log(menage_tert1_2014$ZREVDISP+1)

ggplot(menage_tert1_2014,aes(x = logZREVDISP,weight = POND)) +  geom_density(fill = "steelblue", alpha = 0.4) +
  labs(title = "logarithme du revenu disponible pondéré",x = "Revenu disponible",y = "Densité") + theme_minimal()

#======================== PATRIMOINE===================================================================================================
# ============== Net =============================
MENAGE=menage_tert1_2014 %>% 
  filter(PATRI_NET<=0) %>% 
  select(IDENT, PATRI_NET , PATRI_BRUT, PATIMM, PATFISOM, PATRIPRO, PATRIRES, MTDETTES)

View(MENAGE) 
nrow(MENAGE)

MENAGE = MENAGE %>% 
  mutate(PATRI_NET1=PATIMM+PATFISOM+PATRIPRO+PATRIRES-MTDETTES) 

View(MENAGE)

MENAGE = MENAGE %>% 
  select(IDENT, PATRI_NET,PATRI_NET1, PATRI_BRUT, PATIMM, PATFISOM, PATRIPRO, PATRIRES, MTDETTES)

View(MENAGE)

nrow(MENAGE)/nrow(menage_tert_2014) * 100

summary(MENAGE)

# 50% des menages ont une dette inférieur 20,760; dette moyenne de 118540

MENAGE1=menage_tert1_2014 %>% 
  filter(PATRI_NET<=0) %>% 
  select(IDENT, PATRI_NET, PATRI_BRUT, SITUAPR, TYPMEN5)

View(MENAGE1)

MENAGE1 %>%
  mutate(
    SITUAPR = factor(
      SITUAPR,
      levels = 1:8,
      labels = c(
        "Occupe un emploi",
        "Apprenti(e) sous contrat ou stage rémunéré",
        "Étudiant(e) ou élève",
        "Chômeur",
        "Retraité(e) ou préretraité(e)",
        "Femme ou homme au foyer",
        "Inactif(ve) pour invalidité",
        "Autre situation d'inactivité"
      )
    )
  ) %>%
  count(SITUAPR) %>%
  mutate(
    pourcentage = round(100 * n / sum(n), 1)
  ) %>%
  arrange(desc(n))

# 73.5% occupe un emploi, les personnes référent sont en emploi, au chômage et en retraite ou pré-retraite

# Type de menage des personnes PR en emploi
MENAGE1 %>%
  filter(SITUAPR == 1) %>%
  mutate(
    TYPMEN5 = factor(
      TYPMEN5,
      levels = 1:5,
      labels = c(
        "Personne seule",
        "Famille monoparentale",
        "Couple sans enfant",
        "Couple avec enfants",
        "Autre type de ménage (ménage complexe)"
      )
    )
  ) %>%
  count(TYPMEN5) %>%
  mutate(
    pourcentage = round(100 * n / sum(n), 1)
  ) %>%
  arrange(desc(n))

# Ce sont des personnes seules, couples sans ou avec enfants 

# Type de 
MENAGE1 %>%
  left_join(
    menage_tert %>% select(IDENT, CSPR8),
    by = "IDENT"
  ) %>%
  filter(SITUAPR == 1) %>%
  mutate(
    CSPR8 = factor(
      CSPR8,
      levels = 1:8,
      labels = c(
        "Agriculteurs exploitants",
        "Artisans, commerçants et chefs d'entreprise",
        "Cadres et professions intellectuelles supérieures",
        "Professions intermédiaires",
        "Employés",
        "Ouvriers",
        "Retraités",
        "Autres personnes sans activité professionnelle"
      )
    )
  ) %>%
  count(CSPR8) %>%
  mutate(
    pourcentage = round(100 * n / sum(n), 1)
  ) %>%
  arrange(desc(n))

# Distribution 
ggplot(menage_tert1_2014, aes(x = PATRI_NET, weight = POND)) + geom_histogram(bins = 30)

ggplot(menage_tert1_2014, aes(x = log(PATRI_NET), weight = POND)) + geom_histogram(bins = 30) # Ignore les valeurs négatives (163)

boxplot(log(menage_tert1$PATRI_NET)) #Avant, avantage de prendre le log 

ggplot(menage_tert1_2014,
       aes(x = log(PATRI_NET),
           weight = POND)) +
  geom_density(fill = "steelblue", alpha = 0.4) +
  labs(
    title = "Densité pondérée du revenu disponible",
    x = "Revenu disponible",
    y = "Densité"
  ) +
  theme_minimal()  # Intérêt de prendre encore la log

# Je ne mettrai pas le patrimoine dans le modèle pour éviter les problème des correlations avec ces composantes
# Etudier les 5 composants du patrimoine financier des menages (Immo, Pro, Res, financier, Dette )

#=============== Immobilier ==========================================
# Pondération
design <- svydesign(
  id = ~1,                # Pas de grappes
  weights = ~menage_tert1_2014$POND, # Variable de poids
  data = menage_tert1_2014)

# Valeurs patrimoine 
# Moyenne 
weighted.mean(menage_tert1_2014$PATIMM,menage_tert1_2014$POND)

# Median
svyquantile(~menage_tert1_2014$PATIMM,design,quantiles = 0.5,na.rm = TRUE )        

# Quantile
Q=wtd.quantile(menage_tert1_2014$PATIMM, weights = menage_tert1_2014$POND, probs = c(0.25, 0.5, 0.75))
Q

# Décile 
with(menage_tert1_2014, wtd.quantile(menage_tert1_2014$PATIMM, weights = menage_tert1_2014$POND, probs = (1:10)/10),type = 'quantile') 

# Distribution 
ggplot(menage_tert1_2014, aes(x = PATIMM, weight = POND)) + geom_histogram(bins = 30)


# Vu que les personnes 40% ont 0 en terme de patrimoine immo, je rajoute 1 pour les pouvoir prendre le lograithme
menage_tert1_2014$PATIMM = menage_tert1_2014$PATIMM + 1 

ggplot(menage_tert1_2014, aes(x = log(PATIMM), weight = POND)) + geom_histogram(bins = 30) # Ignore les valeurs négatives

# Les non-propriétaires : masse en 0
# Les propriétaires 

V=menage_2014 %>% 
  select(PATIMM) 

View(V)

max(menage_2014$PATIMM)

boxplot(menage_tert1_2014$PATIMM) #Avant

ggplot(menage_tert1_2014,
       aes(x = log(PATIMM),
           weight = POND)) +
  geom_density(fill = "steelblue", alpha = 0.4) +
  labs(
    title = "Densité pondérée du patrimoine immobilier",
    x = "Patrimoine immobilier",
    y = "Densité"
  ) +
  theme_minimal()

# Je partirai sur le log aussi ;
menage_tert1_2014$logPATIMM = log(menage_tert1_2014$PATIMM)

#=============== Financier ============================================
Class=menage_2014 %>% 
  filter(PATRI_NET < 0 , CSPR == "22") %>% 
  select(IDENT, PATRI_NET , PATRI_BRUT, PATIMM, PATFISOM, MTDETTES, CSPR)

View(Class)

unique(Class$CSPR)

# Valeurs patrimoine 
# Moyenne 
weighted.mean(menage_tert1_2014$PATFISOM,menage_tert1_2014$POND)

# Median
svyquantile(~menage_tert1_2014$PATFISOM,design,quantiles = 0.5,na.rm = TRUE)        

# Quantile
Q=wtd.quantile(menage_tert1_2014$PATFISOM, weights = menage_tert1_2014$POND, probs = c(0.25, 0.5, 0.75))
Q

# Décile 
with(menage_tert1_2014, wtd.quantile(menage_tert1_2014$PATFISOM, weights = menage_tert1_2014$POND, probs = (1:10)/10),type = 'quantile') 

# Distribution 
ggplot(menage_tert1_2014, aes(x = PATFISOM, weight = POND)) + geom_histogram(bins = 30)

#=================
# asinh(x)=ln(x+sqrt(x²+1))

ggplot(menage_tert1_2014, aes(x = asinh(PATFISOM), weight = POND)) + geom_histogram(bins = 30) 

# Prend en compte les négatives et les zéros, agit exactement comme le log

ggplot(menage_tert1_2014, aes(x = log(PATFISOM), weight = POND)) + geom_histogram(bins = 30) # Ignore les valeurs négatives ; 125 

boxplot(menage_tert1_2014$PATFISOM) # Avant

ggplot(menage_tert1_2014,
       aes(x = asinh(PATFISOM),
           weight = POND)) +
  geom_density(fill = "steelblue", alpha = 0.4) +
  labs(
    title = "Asinh du patrimoine financier",
    x = "Patrimoine financier",
    y = "Densité"
  ) +
  theme_minimal()

menage_tert1_2014$ashPATFISOM = asinh(menage_tert1_2014$PATFISOM)

#=============== Professionnel =======================================
# Valeurs patrimoine 
# Moyenne 
weighted.mean(menage_tert1_2014$PATRIPRO,menage_tert1_2014$POND)

# Median
svyquantile(~menage_tert1_2014$PATRIPRO,design,quantiles = 0.5,na.rm = TRUE )        

# Quantile
Q=wtd.quantile(menage_tert1_2014$PATRIPRO, weights = menage_tert1_2014$POND, probs = c(0.25, 0.5, 0.75))
Q
# 90% des menages n'ont pas de patrimoine professionnel en 2020-2021

# Décile 
with(menage_tert1_2014, wtd.quantile(menage_tert1_2014$PATRIPRO, weights = menage_tert1_2014$POND, probs = (1:10)/10),type = 'quantile') 

# Distribution 
ggplot(menage_tert1_2014, aes(x = PATRIPRO, weight = POND)) + geom_histogram(bins = 30) 

ggplot(menage_tert1_2014, aes(x = asinh(PATRIPRO), weight = POND)) + geom_histogram(bins = 30) # C'est pareil avec le log 
ggplot(menage_tert1_2014, aes(x = log(PATRIPRO +1), weight = POND)) + geom_histogram(bins = 30) # Log(x+1) car 90% n'ont pas patrimoine pro

# Je pars sur du log(x+1)
menage_tert1_2014$logPATRIPRO = log(menage_tert1_2014$PATRIPRO + 1)

#=============== Residuel =======================================
# Valeurs patrimoine 
# Moyenne 
weighted.mean(menage_tert1_2014$PATRIRES,menage_tert1_2014$POND)

# Median
svyquantile(~menage_tert1_2014$PATRIRES,design,quantiles=0.5,na.rm = TRUE)        

# Quantile
Q=wtd.quantile(menage_tert1_2014$PATRIRES, weights = menage_tert1_2014$POND, probs = c(0.25, 0.5, 0.75))
Q

# Décile 
with(menage_tert1_2014, wtd.quantile(menage_tert1_2014$PATRIRES, weights = menage_tert1_2014$POND, probs = (1:10)/10),type = 'quantile') 

# Distribution 
ggplot(menage_tert1_2014, aes(x = PATRIRES, weight = POND)) + geom_histogram(bins = 30) 

ggplot(menage_tert1_2014, aes(x = log(PATRIRES), weight = POND)) + geom_histogram(bins = 30) 
# Je pars sur le log reduit l'asymétrie

boxplot(log(menage_tert1_2014$PATRIRES)) #Avant

ggplot(menage_tert1_2014,
       aes(x = log(PATRIRES),
           weight = POND)) +
  geom_density(fill = "steelblue", alpha = 0.4) +
  labs(
    title = "Densité pondérée du patrimoine residuel",
    x = "Patrimoine professionel",
    y = "Densité") +
  theme_minimal()

menage_tert1_2014$logPATRIRES = log(menage_tert1_2014$PATRIRES)





#======================== MONTANT SUR LES LIVRETS ===================================================================================
# ============== LA
# Pondération
design <- svydesign(
  id = ~1,                # Pas de grappes
  weights = ~menage_tert1_2014$POND, # Variable de poids
  data = menage_tert1_2014)

# Valeur extrême
# Moyenne 
weighted.mean(menage_tert1_2014$MLA,menage_tert1_2014$POND)

# Median
svyquantile(~menage_tert1_2014$MLA,design,quantiles = 0.5,na.rm = TRUE)        

# Quantile
Q=wtd.quantile(menage_tert1_2014$MLA, weights = menage_tert1_2014$POND, probs = c(0.25, 0.5, 0.75))
Q

# Décile 
with(menage_tert1_2014, wtd.quantile(menage_tert1_2014$MLA, weights = menage_tert1_2014$POND, probs = (1:10)/10),type = 'quantile') 

summary(menage_tert1_2014$MLA)

# Valeur extreme : Les menages qui ont au délà du plafond
MENAGE=menage_tert1_2014 %>% 
  filter(MLA > 25000) %>% 
  select(IDENT,POND, MLA, MLDDS, MLEP)

View(MENAGE) 
nrow(MENAGE)

Q=wtd.quantile(MENAGE$MLA, weights = MENAGE$POND, probs = c(0.25, 0.5, 0.75))
Q

#=============================  Distribution du log du montant des livrets ==================================================
# LA 
ggplot(menage_tert1_2014,
       aes(x = log(MLA + 1),
           weight = POND)) +
  geom_density(fill = "steelblue", alpha = 0.4) +
  labs(
    title = "Densité pondérée montant du Livret A",
    x = "Montant dclaré sur LA",
    y = "Densité") +
  theme_minimal()

# LDDS
ggplot(menage_tert1_2014,
       aes(x = log(MLDDS + 1),
           weight = POND)) +
  geom_density(fill = "steelblue", alpha = 0.4) +
  labs(
    title = "Densité pondérée montant du LDDS",
    x = "Montant dclaré sur LDDS",
    y = "Densité") +
  theme_minimal()

# LEP 
ggplot(menage_tert1_2014,
       aes(x = log(MLEP + 1),
           weight = POND)) +
  geom_density(fill = "steelblue", alpha = 0.4) +
  labs(
    title = "Densité pondérée montant du LEP",
    x = "Montant dclaré sur LEP",
    y = "Densité") +
  theme_minimal()


# ============== Trouver le nombre de livret par menage et detreminer le montant moyen par livret dans un menage
individu_2014$NBLA=ifelse(individu_2014$LIVABL==1,1,0) # LA
individu_2014$NBLA

individu_2014$NBLDD=ifelse(individu_2014$LDD==1,1,0) # LDD
individu_2014$NBLDD

individu_2014$NBLEP=ifelse(individu_2014$LEP==1,1,0) # LEP
individu_2014$NBLEP

# Nombre de livret par menage
Nb_livret = individu_2014 %>% 
  group_by(IDENT) %>% 
  summarise(NBLA1=sum(NBLA),NBLDD1=sum(NBLDD),NBLEP1=sum(NBLEP)) 

View(Nb_livret)

# Ramener dans la table menage_tert1_2014
menage_tert1_2014 <- menage_tert1_2014 %>% 
  left_join(Nb_livret, by = "IDENT") 


# Vérifier le montant des livrets et le nombre avant le calcul du nombre 
Vue = menage_tert1_2014 %>% 
  filter(MLA>0 & NBLA1 < 1 ) %>% 
  select(MLA,NBLA1)

View(Vue)

Vue1 = menage_tert1_2014 %>% 
  filter(MLDDS>0 & NBLDD1 < 1 ) %>% 
  select(MLDDS,NBLDD1)

View(Vue1)

Vue2 = menage_tert1_2014 %>% 
  filter(MLEP>0 & NBLEP1 < 1 ) %>% 
  select(MLEP,NBLEP1)

View(Vue2)

# Correction des nombres de livrets (Pas très effectuives les declarations dans les ménages)
menage_tert_clean <- menage_tert1_2014 %>%
  mutate(
    NBLA1  = if_else(MLA > 0 & NBLA1 < 1, 1, NBLA1),
    NBLDD1 = if_else(MLDDS > 0 & NBLDD1 < 1, 1, NBLDD1),
    NBLEP1 = if_else(MLEP > 0 & NBLEP1 < 1, 1, NBLEP1))

# Calcul du MMLA1, MMLDDS1 et MMLEP1, premièrev approche
menage_tert_clean <- menage_tert_clean %>%
  mutate(MMLA1   = if_else(NBLA1 > 0, MLA / NBLA1, 0),
         MMLDDS1 = if_else(NBLDD1 > 0, MLDDS / NBLDD1, 0), 
         MMLEP1  = if_else(NBLEP1 > 0, MLEP / NBLEP1, 0))

Vue3 = menage_tert_clean %>% 
  filter(MMLA1 > 28000) %>% 
  select(IDENT, MMLA1, NBLA1, MLA, MMLDDS1, MMLEP1)

View(Vue3)

#=========== Approche 2 pour le calcul du nombre de livret
#====================================== Calcul du nombre de livret par les montants
produit_2014 = produit_2014 %>% 
  mutate(NBLA = if_else(FINNA %in% c("02","03") & MONTCLA > 0,1,0),
         NBLDD = if_else(FINNA %in% c("04") & MONTCLA > 0,1,0), 
         NBLEP = if_else(FINNA %in% c("05") & MONTCLA > 0,1,0))

View(produit)

table_NBLA <- produit_2014 %>%
  group_by(IDENT, NOP) %>%
  summarise(
    NBLA = sum(NBLA, na.rm = TRUE),
    .groups = "drop")

View(table_NBLA)

# Table intermédiaire NBLA, NBLDD, NBLEP
NBLIVRET = produit_2014 %>% 
  group_by(IDENT,NOP) %>%
  summarise(
    NBLA = sum(NBLA, na.rm = TRUE),
    NBLDDS=sum(NBLDD,na.rm = TRUE),
    NBLEP=sum(NBLEP,na.rm=TRUE),
    .groups = "drop")

View(NBLIVRET)

# Correction 
NBLIVRET <- NBLIVRET %>%
  mutate(
    NBLA = ifelse(NBLA > 2, 2, NBLA),
    NBLDDS = ifelse(NBLDDS > 2, 2, NBLDDS),
    NBLEP = ifelse(NBLEP > 2, 2, NBLEP))

# Nombre de LA par menage 
table_NBLA_menage <- NBLIVRET %>%
  group_by(IDENT) %>%
  summarise(
    NBLA2 = sum(NBLA, na.rm = TRUE),
    NBLDDS2=sum(NBLDDS,na.rm = TRUE),
    NBLEP2=sum(NBLEP,na.rm=TRUE),
    .groups = "drop")

View(table_NBLA_menage)

# Ramener dans menage 
menage_tert_clean = menage_tert_clean %>% 
  left_join(table_NBLA_menage,by="IDENT")

menage_tert_clean <- menage_tert_clean %>%
  mutate(MMLA2   = round(if_else(NBLA2 > 0, MLA / NBLA2, 0),0), 
         MMLDDS2 = round(if_else(NBLDDS2 > 0, MLDDS / NBLDDS2, 0),0), 
         MMLEP2  = round(if_else(NBLEP2 > 0, MLEP / NBLEP2, 0),0))

# Comparaison déclaration par detention et montant 
Vue = menage_tert_clean %>% 
  filter(NBLA1 != NBLA2) %>% 
  select(IDENT,NBLA1,NBLA2)

View(Vue)

# Nombre de personnes dans le menage
NBIND=individu_2014 %>%
  group_by(IDENT) %>%
  summarise(NBIND = n_distinct(IDENTIND14))

View(NBIND)

# Ramener dans menage 
menage_tert_clean = menage_tert_clean %>% 
  left_join(NBIND,by="IDENT")

# Voir
Vue = menage_tert_clean %>% 
  filter(NBLA1 != NBLA2) %>% 
  select(IDENT,NBLA1,NBLA2,NBIND)

View(Vue)

# Voir les menages ont les montants au dessus du plafond 
Vue = menage_tert_clean %>% 
  filter(MMLEP2 == 13000) %>% 
  select(IDENT,NBLA1,NBLA2,NBIND,NPERS, MMLA1, MMLA2)

View(Vue)

nrow(Vue)/nrow(menage_tert_clean) * 100

# Mettre les montants négatifs des livrets à 0 (Corriger la vague 2017) 
#******************************* Faire attention par ici ***************************

# Finalement, on va partir sur MMLA2, MMLDDS2, MMLEP2 
menage_tert_clean$MMLA1 = NULL
menage_tert_clean$MMLDDS1 = NULL
menage_tert_clean$MMLEP1 = NULL

menage_tert_clean = menage_tert_clean %>% 
  rename(MMLA=MMLA2,MMLDDS=MMLDDS2,MMLEP=MMLEP2)

#*********************** Ce que je cherche à faire ***********************************
#                                                                                    *
#  Logit/Probit effet fixe, ou effet alétoire ? Decider entre                        *  
#  Abitrage instantané  et dynamique (Plus important) (chaque coupe et le panel)     *
#                                                                                    *
#*************************************************************************************
menage_tert_clean$MMLA
menage_tert_clean$MMLDDS
menage_tert_clean$MMLEP

dim(menage_tert_clean)
dim(menage_2014)

mean(is.na(menage_tert_clean$ZREVDISP)) * 100 

#============ Selection des variables les plus importantes selon la Littérature et logique eco

#============== Les 6 variables à modeliser
# Detention 
menage_tert_clean$R_LIVABL
menage_tert_clean$R_LDD
menage_tert_clean$R_LEP

# Montant 
menage_tert_clean$MMLA
menage_tert_clean$MMLDDS
menage_tert_clean$MMLEP

# Ponderation et ID 
menage_tert_clean$IDENT
menage_tert_clean$POND

#============= Les explicatives
menage_tert_clean$ZREVDISP  # Revenu disponible *
menage_tert_clean$ALGNB  # Nombre autres logements
menage_tert_clean$NENFANTS # Nombre d'enfants dans le menage *
menage_tert_clean$MATRIPR # Situation matrimoniale PR *
menage_tert_clean$NPERS # Nombre de personne dans le menage *
menage_tert_clean$SEXEPR # Genre PR 
menage_tert_clean$AUTSAL # Exercice activité secondaire
menage_tert_clean$CNDUR # Dépenses usuelles (alimentation, habillement…) sans les remboursements (loyers)
menage_tert_clean$NACTIFS # Nombre d'actif dans le menage *
menage_tert_clean$SITUAPR # Situation par rapport à l'emploi PR *
menage_tert_clean$STATUTPR # Statut pro PR 
menage_tert_clean$DET # Detention de dette privée ou professionelle *
menage_tert_clean$DIEXP # Anticipation pouvoir d'achat *
menage_tert_clean$DONVERS # Exitence de don versée par le Couple PR
menage_tert_clean$ENT_ENDET # Existence d'un endettement pour les entreprises
menage_tert_clean$PRET # Pret à une personne hors ménage
menage_tert_clean$RESPRIACH # Valeur de la Residence principale *
menage_tert_clean$RFIN # Existence de Revenu d'interets, dividendes, revenus d'épargne *
menage_tert_clean$RIMM # Perception loyer et fermage 
menage_tert_clean$RISKFI # Attitude face au risque financier *
menage_tert_clean$RLOG # Perception d'allocation logement
menage_tert_clean$RMODIF_CHO # Perte d'emploi d'une personne active du menage
menage_tert_clean$RMODIF_DC # Deces 
menage_tert_clean$RMODIF_MAL # Longue Maladie d'une apporteur de ressources
menage_tert_clean$RMODIF_RET # Passage à la Retraite
menage_tert_clean$RMODIF_SEP # Separation
menage_tert_clean$RNSAL # Perception de revenue non salarié (indépendant et libéral) 
menage_tert_clean$RRET # Perception de rente, de retraite *
menage_tert_clean$RRMI # Perception du RSA, RSO et RSTI
menage_tert_clean$RSAL # Perception salaire, traitement et prime d'activité
menage_tert_clean$RTRA # Perception pension alimentaire, aides financières
menage_tert_clean$R_ACTNPROF # Perception action non côtés ou parts d'entreprise
menage_tert_clean$R_AVI # Detention Assurance Vie *
menage_tert_clean$R_AVIEURO # Detention Assurance Vie Euro *
menage_tert_clean$R_AVIMULT # Detention Assurance Multi-support *
menage_tert_clean$R_AVIRENTE # Detention Assurance en phase de Rente *
menage_tert_clean$R_BON # Detention Bon autres que capitalisation
menage_tert_clean$R_BNXBAT # Detention Batiement Locaux exploité professionnellement 
menage_tert_clean$R_CAPI # Detention Bon de capitalisation
menage_tert_clean$R_CCA # Detention Compte courant d'associé
menage_tert_clean$R_CCQ # Detention Compte Cheque 
menage_tert_clean$R_CEL # Detention CEL *
menage_tert_clean$R_COMP # Detention Retraite complementaire 
menage_tert_clean$R_CTIT # Detention COmpte Titre *
menage_tert_clean$DET # Detention d'une dette *
menage_tert_clean$R_DETDI # Detention d'emprunt autre motif privé
menage_tert_clean$R_DETEQ # Detention d'emprunt privé pour d'autres biens d'équipement 
menage_tert_clean$R_DETETU # Detention d'emprunt privé pour motif financer les études 
menage_tert_clean$R_DETIM # Detention d'emprunt privé pour acaht immobilier autre que la RP 
menage_tert_clean$R_DETPR # Detention d'emprunt pour actif ou tresorerie professionnelle
menage_tert_clean$R_DETRE # Detention d'emprunt pour la residence principale
menage_tert_clean$R_DETRES # Detention d'emprunt pour restructuration de dettes privées 
menage_tert_clean$R_DETTR # Detention d'emprunt pour gros travaux immobiliers
menage_tert_clean$R_EDUC # Detention de rente d'education
menage_tert_clean$R_EPALO # Detention d'epargne logement 
menage_tert_clean$R_EPSALA # Detention d'epargne salariale *
menage_tert_clean$R_LEE # Detention LEE 
menage_tert_clean$R_LIVDF # Detention Defiscalisé *
menage_tert_clean$R_LIVEP # Detention livret d'epargne *
menage_tert_clean$R_LIVF # Detention livret soumis à l'impôt *
menage_tert_clean$R_LIVJEUN # Detention livret jeune *
menage_tert_clean$R_LOGAUT # Detention logement autres que la RP *
menage_tert_clean$R_LOGGRAT # Detention d'un logement gratuit 
menage_tert_clean$R_LOGLOC # Detention d'un logement en location
menage_tert_clean$R_LOGSEC # Detention d'une residence secondaire 
menage_tert_clean$R_LOGVAC # Detention d'un loegement vacant
menage_tert_clean$R_OBLI # Detention d'obligations 
menage_tert_clean$R_PANIER # Detention autres produit d'épargne 
menage_tert_clean$R_PARSOC # Detention sociales de Banques
menage_tert_clean$R_PATIMMO # Detention patrimoine immobilier
menage_tert_clean$R_PATPROF # Detention patrimoine professionnel  
menage_tert_clean$R_PEA # Detention de PEA  *
menage_tert_clean$R_PEAACT # Detention d'action sur PEA 
menage_tert_clean$R_PEASIC # Detention de SICAV ou FCP sur PEA
menage_tert_clean$R_PEATRE # Detention de tresorerie sur PEA
menage_tert_clean$R_PEL # Detention de PEL *
menage_tert_clean$R_PEP # Detention de PEP 
menage_tert_clean$R_PERP # Detention de PERP
menage_tert_clean$R_POA # Detention de Stock Options
menage_tert_clean$R_PRO # Detention d'actif productif
menage_tert_clean$R_RESPRIN # Proprietaire *
menage_tert_clean$R_RETRAIT # Detention d'epargne retraite *
menage_tert_clean$R_PRO # Detention d'actif productif
menage_tert_clean$R_SUR # Detention de retraite surcomplementaire
menage_tert_clean$R_TERM # Detention de compte à terme *
menage_tert_clean$R_VMOB # Valeurs mobilières *
menage_tert_clean$HERPAT # Part Héritage et donation dans le patrimoine 
menage_tert_clean$HERTABD # Detention donation reçue *
menage_tert_clean$HERTABH # Detention heritage reçue *
menage_tert_clean$LOGPROPRI # Propriétaire *
menage_tert_clean$LOYER # Loyer *
menage_tert_clean$MPANEL # Menage contenant un ind panel
menage_tert_clean$NBASS # Nombre d'assurance Vie par le menage *
menage_tert_clean$NBCC # Nombre compte cheque par le menage
menage_tert_clean$NBLA # Nombre de LA *
menage_tert_clean$NBLDD # Nombre de LDD *
menage_tert_clean$NBLEP # Nombre de LEP *
menage_tert_clean$NBLOG # Nombre de Logement y compris la RP *
menage_tert_clean$NBUC # Nombre d'unité de Consommation
menage_tert_clean$NE # Nombre entreprise
menage_tert_clean$PATFISOM # Patrimoine financier *
menage_tert_clean$PATIMM # Patrimoine Immobilier *
menage_tert_clean$PATRIRES # Patrimoine residuel *
menage_tert_clean$MTDETTES # Dettes *
menage_tert_clean$VIAG # Detention de Viager
menage_tert_clean$ZEAT # Zeat de résidence
menage_tert_clean$HERDON # Heritage ou donation reçus *
menage_tert_clean$CONSEP # Raison d'épargner *
menage_tert_clean$R_ACTION # Detention d'action 
menage_tert_clean$R_ACT # Detention d'actions cotés *
menage_tert_clean$R_ACTNCT # Detention d'actions non cotés
menage_tert_clean$R_PEPBANC # Detention PEP Bancaire 
menage_tert_clean$R_PEPASSUR # Detention PEP Assurance
menage_tert_clean$R_PROHEXP # Actif productifs exploités professionnellement 
menage_tert_clean$DETPRO # Endettement professionel
menage_tert_clean$R_ACTPEA # Action sur PEA 
menage_tert_clean$R_SCPI # Detention de SCPI
menage_tert_clean$R_SICAV # Detention de SICAV
menage_tert_clean$PATRI_BRUT # Patrimoine Brut *
menage_tert_clean$PATRI_NET # Patrimoine Net *
menage_tert_clean$ZIMPOM # Impôt sur le revenu 
menage_tert_clean$ZTHAB # Taxe d'habitation
menage_tert_clean$ZCHOMAGE # Revenu de chomage ou preretraite
menage_tert_clean$ZRAG # Revenu agricole 
menage_tert_clean$DETIMM # Endettement pour achat immobilier 
menage_tert_clean$DETPRIV # Endettement pour motif privé 
menage_tert_clean$CALIM # Montant mensuel moyen des dépenses alimentaires 
menage_tert_clean$CCHARG # Dépenses mensuelles mooyennes pour l'eau, l'électricité, le gaz, le chauffage et la communication
menage_tert_clean$DIF # Dificultés à faire face à ses échénances *
menage_tert_clean$AGE_REEL # Age reel de la PR *
menage_tert_clean$AGE_RELATIF # Age relatif de la PR  *
menage_tert_clean$CATAGE_RELATIF # Categorie d'age (Age realtif) de lma PR *
menage_tert_clean$decile_patrimoine # position dans la distribution du patrimoine
menage_tert_clean$ZREVFIN # Revenu des capitaux mobiliers 
menage_tert_clean$R_PATFINANC # Detention d'un actif financier
menage_tert_clean$decile_revenu # position dans la distribution du revenu *
menage_tert_clean$Heritage # Montant heritage 
menage_tert_clean$Donation_R # Montant donation reçu
menage_tert_clean$Donation_V # Montant donation versé
menage_tert_clean$NIVEDUC # Niveau d'education *
menage_tert_clean$CSPR8 # Catégorie socio-pro *
menage_tert_clean$decile_patrimoine # Position dans la distribution du patrimoine *
menage_tert_clean$MRESPRIN # Montant de la residence principale
menage_tert_clean$MAUTLOG # Montant autre logement *
menage_tert_clean$MLIVJEUNE # Montant Livret Jeune *
menage_tert_clean$MLIVIMPOT # Montant Livret soumis à l'impot *
menage_tert_clean$MPEL # Montant PEL *
menage_tert_clean$MAVI # MOnatnt AVI *
menage_tert_clean$MCEL # Monatnt CEL *
menage_tert_clean$MEPRETRAITE # Montant Epargne retraite 
menage_tert_clean$MPEP # Montant PEP
menage_tert_clean$MEPSALAR # Montant Epsalaire
menage_tert_clean$MVMOB # Montant VMOB
menage_tert_clean$MBONCAISSE # Montant BON CAISSE
menage_tert_clean$MCOMPTERM # Montant COMPTE TERME
menage_tert_clean$MCASSOCIE # Montant COMPTE ASSOCIE 
menage_tert_clean$MLEE # Montant LEE
menage_tert_clean$MVMOB # Montant VMOB *
menage_tert_clean$TYPMEN5  # Type de menage *
menage_tert_clean$DIREVY # Attente par rapport au revenu * 
menage_tert_clean$PATRIPRO # Patrimoine professionel 

menage_tert_clean$EVEAUGM # Augmentation patrimoine
menage_tert_clean$EVEAUGM_AUT # Autres raisons
menage_tert_clean$EVEAUGM_BOU # Gain en bourse 
menage_tert_clean$EVEAUGM_HER # Heritage ou donation
menage_tert_clean$EVEAUGM_JEU # Gain art, lotterie et jeux
menage_tert_clean$EVEAUGM_LOG # Logement ayant pris de la valeur
menage_tert_clean$EVEAUGM_PRIM # Prime, indemnités  et traitements
menage_tert_clean$EVEAUGM_PROF # Outil professionnel ayant pris de la valeurs
menage_tert_clean$EVEAUGM_REV # Activité professionnelle prospère  
menage_tert_clean$EVEAUGM_TER # Terrain devenu constructible 

# Je vais en revenir pour ces variables
menage_tert_clean$EVEBAIS # Baisse patrimoine
menage_tert_clean$EVEBAIS_ACC # Accidents, Catastrophes, Guerres
menage_tert_clean$EVEBAIS_AUT # Autres raisons
menage_tert_clean$EVEBAIS_BOU # Perte en Bourse
menage_tert_clean$EVEBAIS_CHO # Chômage
menage_tert_clean$EVEBAIS_CONS # Augementation de la consommation 
menage_tert_clean$EVEBAIS_DIV # Divorce, Separation  
menage_tert_clean$EVEBAIS_FAIL # Faillite
menage_tert_clean$EVEBAIS_INFL # Inflation, Conditions économiques 
menage_tert_clean$EVEBAIS_JEU # Perte art, lotterie et jeux
menage_tert_clean$EVEBAIS_LOG # Logement ayant perdu de la valeur
menage_tert_clean$EVEBAIS_MAL # Maladie, Invalidité
menage_tert_clean$EVEBAIS_PROF # Moins value professionnel
menage_tert_clean$EVEBAIS_REV # Baisse de revenus 
menage_tert_clean$EVEBAIS_VEU # Veuvage
menage_tert_clean$EVEBAIS_REV # Baisse de revenus 

View(menage_tert_clean) 

names(autres_var_2020)

# Les variables en étoiles sont les plus recherchés (à garder)
variables_essentielles <- 
  c("ZREVDISP", "NENFANTS", "MATRIPR", "NPERS", "NACTIFS", "SITUAPR", 
    "DET", "DIEXP", "RESPRIACH", "RFIN", "RISKFI", "RRET", "R_AVI", 
    "R_AVIEURO", "R_AVIMULT", "R_AVIRENTE", "R_CEL", "R_CTIT", 
    "R_EPSALA", "R_LIVDF", "R_LIVEP", "R_LIVF", "R_LIVJEUN", 
    "R_LOGAUT", "R_PEA", "R_PEL", "R_RESPRIN", "R_RETRAIT", 
    "R_TERM", "R_VMOB", "HERTABD", "HERTABH", "LOGPROPRI", "LOYER", 
    "NBASS", "NBLA", "NBLDD", "NBLEP", "NBLOG", "PATFISOM", "PATIMM", 
    "PATRIRES", "MTDETTES", "HERDON", "CONSEP", "R_ACT", "PATRI_BRUT", 
    "PATRI_NET", "DIF", "AGE_REEL", "AGE_RELATIF", "CATAGE_RELATIF", 
    "decile_revenu", "NIVEDUC", "CSPR8", "decile_patrimoine", "MAUTLOG", 
    "MLIVJEUNE", "MLIVIMPOT", "MPEL", "MAVI", "MCEL", "MVMOB", "TYPMEN5",
    "DIREVY", "PATRIPRO")


# Je ne vais pas recoder car un menage peut lister plusieurs problèmes
Voir=menage_tert_clean %>% 
  select(EVEAUGM,EVEAUGM_AUT,EVEAUGM_BOU,EVEAUGM_HER,EVEAUGM_JEU,EVEAUGM_LOG,EVEAUGM_PRIM,EVEAUGM_PROF,EVEAUGM_REV,EVEAUGM_TER)

View(Voir)


# Suppresion des variables inutiles 
vars_a_supprimer <- c(
  "AGEPR", "COUPLEPR", "COUPLEIDEM", "MATRICJ", "MNAISPR", "SEXECJ",
  "CSCJ", "NATIO7CJ", "SITUACJ", "STATUTCJ", "STATUTECLBISCJ",
  "STATUTECLBISPR", "ITOTREV", "TRANCHPRE", "ZREVDEC", "AVIAUT",
  "AVIINFO", "AVIPLAN", "DIPLOMCJ", "DONHOD", "DONHOD_AUT",
  "DONHOD_DON", "DONHOD_ETU", "DONHOD_INST", "DONHOD_LOG",
  "DONHOD_PRET", "DONHOD_RECH", "DONHOD_REG", "DONPAR",
  "DONPAR_AUT", "DONPAR_DON", "DONPAR_LOG", "DONPAR_PRET",
  "DONPAR_REG", "DONPAR_RET", "DONVERS", "PATFIVAR", "PFAVI",
  "PFLIVA", "PFLOGAUT", "PROPRI", "RAIDAUT", "RBOU", "RCHO",
  "RMAIDAUT", "RMAIDAUTC", "RMODIF", "R_EPSAL", "R_TERNEEXP",
  "R_TERPRO", "R_TNXBAT", "R_TNXBOI", "R_TNXLAB", "R_TNXLOC",
  "R_TNXNBA", "R_TNXVIA", "R_TXBAT", "R_TXBOI", "R_TXLAB",
  "R_TXNBA", "R_TXVIA", "STOCPP", "EVOACTENT", "EVOANC",
  "EVOAVI", "AVOAP", "EVOBON", "EVOEL", "EVOER", "EVOES",
  "EVOLIV", "EVOPRETRES", "GMTELO", "GMTPEP", "GMTPERP",
  "GMTRET", "GPRKDU", "INDEPA", "MOBACH", "MOBAUT",
  "MOBINFO", "MOBPLAN", "NBIENS", "NBIENSR", "NCOUPLES",
  "PATPROFENT", "PATPROFHENT", "MTDETPRIV", "MTDETPROFENT",
  "MTDETPROFHENT", "SALACH", "SALAUT", "SALINFO", "SALPAR",
  "SALPLAN", "SCOLACJ", "SCOLAPR", "TYPMEN15", "INDEP",
  "MOUVEMENT", "GMTLIV", "GMTAVI", "GMTSAL", "GMTVMOB",
  "R_PRONEEXP", "R_PROPRO", "PATFI", "PATRIB", "PATRIA",
  "MTPRETS", "MTVEHIC", "MTVOIT", "MTPATRIAC", "ZRAC",
  "ZETRANGER", "ZPENALIV") 

# Suppression des variables
menage_tert_clean <- menage_tert_clean %>%
  select(-any_of(vars_a_supprimer))

dim(menage_tert_clean)

# Création de la base pour les ménages de 2020 
vars_base_menage_2014 <- c(
  # Identifiants et pondérations
  "IDENT", "POND", "MPANEL_NEXT",
  
  # Variables à modéliser
  "R_LIVABL", "R_LDD", "R_LEP",
  "MMLA", "MMLDDS", "MMLEP",
  
  # Explicatives
  "ZREVDISP", "ALGNB", "NENFANTS", "MATRIPR", "NPERS",
  "SEXEPR", "AUTSAL", "CNDUR", "NACTIFS", "SITUAPR",
  "STATUTPR", "DET", "DIEXP", "DONVERS", "ENT_ENDET",
  "PRET", "RESPRIACH", "RFIN", "RIMM", "RISKFI",
  "RLOG", "RMODIF_CHO", "RMODIF_DC", "RMODIF_MAL",
  "RMODIF_RET", "RMODIF_SEP", "RNSAL", "RRET",
  "RRMI", "RSAL", "RTRA", "R_ACTNPROF",
  "R_AVI", "R_AVIEURO", "R_AVIMULT", "R_AVIRENTE",
  "R_BON", "R_BNXBAT", "R_CAPI", "R_CCA", "R_CCQ",
  "R_CEL", "R_COMP", "R_CTIT", "R_DETDI", "R_DETEQ",
  "R_DETETU", "R_DETIM", "R_DETPR", "R_DETRE",
  "R_DETRES", "R_DETTR", "R_EDUC", "R_EPALO",
  "R_EPSALA", "R_LEE", "R_LIVDF", "R_LIVEP",
  "R_LIVF", "R_LIVJEUN", "R_LOGAUT", "R_LOGGRAT",
  "R_LOGLOC", "R_LOGSEC", "R_LOGVAC", "R_OBLI",
  "R_PANIER", "R_PARSOC", "R_PATIMMO", "R_PATPROF",
  "R_PEA", "R_PEAACT", "R_PEASIC", "R_PEATRE",
  "R_PEL", "R_PEP", "R_PERP", "R_POA", "R_PRO",
  "R_RESPRIN", "R_RETRAIT", "R_SUR", "R_TERM",
  "R_VMOB", "HERPAT", "HERTABD", "HERTABH",
  "LOGPROPRI", "LOYER", "MPANEL", "NBASS",
  "NBCC", "NBLA", "NBLDD", "NBLEP", "NBLOG",
  "NBUC", "NE", "PATFISOM", "PATIMM",
  "PATRIRES", "MTDETTES", "VIAG", "ZEAT",
  "HERDON", "CONSEP", "R_ACTION", "R_ACT",
  "R_ACTNCT", "R_PEPBANC", "R_PEPASSUR",
  "R_PROHEXP", "DETPRO", "R_ACTPEA",
  "R_SCPI", "R_SICAV", "PATRI_BRUT",
  "PATRI_NET", "ZIMPOM", "ZTHAB",
  "ZCHOMAGE", "ZRAG", "DETIMM",
  "DETPRIV", "CALIM", "CCHARG",
  "DIF", "AGE_REEL", "AGE_RELATIF",
  "CATAGE_RELATIF", "decile_patrimoine",
  "ZREVFIN", "R_PATFINANC",
  "decile_revenu", "Heritage",
  "Donation_R", "Donation_V",
  "NIVEDUC", "CSPR8",
  "MRESPRIN", "MAUTLOG",
  "MLIVJEUNE", "MLIVIMPOT",
  "MPEL", "MAVI", "MCEL",
  "MEPRETRAITE", "MPEP",
  "MEPSALAR", "MVMOB",
  "MBONCAISSE", "MCOMPTERM",
  "MCASSOCIE", "MLEE",
  "TYPMEN5", "DIREVY","PATRIPRO",
  
  # Evénements d'augmentation du patrimoine
  "EVEAUGM", "EVEAUGM_AUT", "EVEAUGM_BOU",
  "EVEAUGM_HER", "EVEAUGM_JEU", "EVEAUGM_LOG",
  "EVEAUGM_PRIM", "EVEAUGM_PROF",
  "EVEAUGM_REV", "EVEAUGM_TER")

base_menage_2014 <- menage_tert_clean %>%
  select(any_of(vars_base_menage_2014))

dim(base_menage_2014)
View(base_menage_2014)

# Supprimer les doublons
vars_base_menage_2014 <- unique(vars_base_menage_2014)

# Il n'y avait pas de doublons de toutes façon 
base_menage_2014 <- menage_tert_clean %>%
  dplyr::select(any_of(vars_base_menage_2014))

dim(base_menage_2014)

# Corriger les variables EVEAUGM
vars_eveaugm <- c(
  "EVEAUGM", "EVEAUGM_AUT", "EVEAUGM_BOU",
  "EVEAUGM_HER", "EVEAUGM_JEU", "EVEAUGM_LOG",
  "EVEAUGM_PRIM", "EVEAUGM_PROF",
  "EVEAUGM_REV", "EVEAUGM_TER")

base_menage_2014 <- base_menage_2014 %>%
  mutate(across(
    all_of(vars_eveaugm),
    ~ ifelse(as.character(.) == "", "2", as.character(.))
  ))

# Vérification 
sapply(base_menage_2014[vars_eveaugm],
       function(x) sum(x == "", na.rm = TRUE))


# Variables présentes dans base_menage_2014
vars_base <- names(base_menage_2014)

# Variables à mettre dans la base complémentaire
vars_autres <- setdiff(names(menage_tert_clean), vars_base)

# Création de la base avec IDENT en première variable
autres_var_2014 <- menage_tert_clean[, c("IDENT", vars_autres)]

View(autres_var_2014)
dim(autres_var_2014)

#======================== Correlation et selection définitives des variables
# A ce stade, j'ai une bases de données, il faut ensuite faire les correlations et selectionner
# les variables qui resteront
base_menage_2014

# Définir les types
# Nombre de modalités par variables 
sapply(base_menage_2014, function(x)
  if(is.numeric(x)) length(unique(x)) else NA)

vars_quali <- c("CATAGE_RELATIF", "decile_patrimoine","decile_revenu", "NIVEDUC","CSPR8")

base_menage_2014[vars_quali] <- lapply(base_menage_2014[vars_quali],as.character)

# autres_var_2020
sapply(base_menage_2014, function(x)
  if(is.numeric(x)) length(unique(x)) else NA)

#========================================== MLA, MLDDS, MLEP 
# Correlations pour les quantitatives 
#============ base_menage_2014
# Variables d'intérêt
vars_cibles <- c("MMLA", "MMLDDS", "MMLEP")

# Sélection des variables quantitatives dans la base 
vars_quant <- names(base_menage_2014)[sapply(base_menage_2014, is.numeric)]

# Autres variables quantitatives
autres_vars <- setdiff(vars_quant, vars_cibles)

# Tableau des résultats
resultats <- data.frame()

# Test de corrélations Pearson et Spearman
for (v1 in vars_cibles) {
  for (v2 in autres_vars) {
    
    tmp <- base_menage_2014[, c(v1, v2)]
    tmp <- na.omit(tmp)
    
    if (nrow(tmp) > 3) {
      
      pearson <- cor.test(tmp[[v1]], tmp[[v2]], method = "pearson")
      spearman <- cor.test(tmp[[v1]], tmp[[v2]], method = "spearman")
      
      resultats <- rbind(
        resultats,
        data.frame(
          Variable_cible = v1,
          Variable = v2,
          Pearson_r = unname(pearson$estimate),
          Pearson_p = pearson$p.value,
          Spearman_rho = unname(spearman$estimate),
          Spearman_p = spearman$p.value))}}}

head(warnings(), 5) # Les avertissements c'est pour les variables qui contiennent les mêmes valeurs

# Affichage
View(resultats)

# Ajout des significativité
resultats$Pearson_sig <- ifelse(resultats$Pearson_p < 0.01, "***",
                                ifelse(resultats$Pearson_p < 0.05, "**",
                                       ifelse(resultats$Pearson_p < 0.10, "*", "")))


resultats$Spearman_sig <- ifelse(resultats$Spearman_p < 0.01, "***",
                                 ifelse(resultats$Spearman_p < 0.05, "**",
                                        ifelse(resultats$Spearman_p < 0.10, "*", "")))


View(resultats)

# Qu'est ce que retient à ce stade, deux conditions pour selectionner
# - Significaticité quelque soit le seuil 
# - Taille minimale des corrélations :  ∣r∣≥0,10 ou ∣ρ∣≥010
# - Intuition economique

#=============== Pour MMLA : (Les résulats sont actualisés mais pas ces variables)

# Retenues : (A actualiser)
# ZREVDISP, ALGNB, CNDUR, ZNIVVIE, LOYER, NBASS, NBCC, NBLOG,
# PATFISOM, PATRI_BRUT, PATRI_NET, AGE_REEL, ZREVFIN, Heritage,
# Donation_R, Donation_V, MRESPRIN, MAUTLOG, MLIVIMPOT, MPEL,
# MAVI, MCEL, MEPRETRAITE, MEPSALAR, MVMOB, PATRIRES, PATRIPRO

# Ecartées :
# RISKFI, NENFANTS, NPERS, NACTIFS, NBUC, NE,
# MTDETTES, AGE_RELATIF, MLIVJEUNE, MPEP, MBONCAISSE,
# MCOMPTERM, MCASSOCIE, MLEE

# Réintroduction possible selon la logique économique :
# NPERS, NENFANTS, NACTIFS, MTDETTES, RISKFI, AGE_RELATIF, MLIVJEUNE, MCOMPTERM

# Donc MMLA pour les quantitatives revient à :
# ZREVDISP, ALGNB, CNDUR, ZNIVVIE, LOYER, NBASS, NBCC, NBLOG,
# PATFISOM, PATRI_BRUT, PATRI_NET, AGE_REEL, ZREVFIN, Heritage,
# Donation_R, Donation_V, MRESPRIN, MAUTLOG, MLIVIMPOT, MPEL,
# MAVI, MCEL, MEPRETRAITE, MEPSALAR, MVMOB, PATRIRES,PATRIPRO,
# NPERS, NENFANTS, NACTIFS, MTDETTES, RISKFI, AGE_RELATIF, MLIVJEUNE, MCOMPTERM

#=============== Pour MMLDD :

# Retenues :
# ZREVDISP, ALGNB, CNDUR, ZNIVVIE, LOYER, NBASS, NBCC, NBLOG,
# PATFISOM, PATRI_BRUT, PATRI_NET, AGE_REEL, ZREVFIN, Heritage,
# Donation_R, Donation_V, MRESPRIN, MAUTLOG, MLIVIMPOT, MPEL,
# MAVI, MCEL, MEPRETRAITE, MEPSALAR, MVMOB, PATRIRES, PATRIPRO

# Ecartées :
# NENFANTS, NPERS, NACTIFS, RISKFI, NBUC, NE,
# MTDETTES, AGE_RELATIF, MLIVJEUNE, MPEP, MBONCAISSE,
# MCOMPTERM, MCASSOCIE, MLEE

# Réintroduction possible selon la logique économique :
# NPERS, NENFANTS, NACTIFS, MTDETTES, RISKFI, AGE_RELATIF, MLEE

# Donc MMLDD pour les quantitatives revient à :
# ZREVDISP, ALGNB, CNDUR, ZNIVVIE, LOYER, NBASS, NBCC, NBLOG,
# PATFISOM, PATRI_BRUT, PATRI_NET, AGE_REEL, ZREVFIN, Heritage,
# Donation_R, Donation_V, MRESPRIN, MAUTLOG, MLIVIMPOT, MPEL,
# MAVI, MCEL, MEPRETRAITE, MEPSALAR, MVMOB, PATRIRES,PATRIPRO,
# NPERS, NENFANTS, NACTIFS, MTDETTES, RISKFI, AGE_RELATIF, MLEE

#=============== Pour MMLEP :

# Retenues :
# ZREVDISP, ALGNB, CNDUR, ZNIVVIE, PATRI_BRUT, PATRI_NET, decile_revenu,
# MRESPRIN, MAUTLOG, PATRIPRO

# Ecartées :
# NENFANTS, NPERS, NACTIFS, RISKFI, LOYER, NBASS, NBCC, NBLOG, NBUC, NE, PATFISOM, MTDETTES,
# AGE_REEL, AGE_RELATIF, ZREVFIN, Heritage, Donation_R, Donation_V, MLIVJEUNE, MLIVIMPOT,
# MPEL, MAVI, MCEL, MEPRETRAITE, MPEP, MEPSALAR, MVMOB, MBONCAISSE, MCOMPTERM, MCASSOCIE, MLEE, PATRIRES

# Réintroduction possible selon la logique économique :
# NPERS, NENFANTS, NACTIFS, MTDETTES, AGE_REEL, AGE_RELATIF, ZREVFIN, RISKFI, NBASS, 
# NBCC, MTDETTES, PATFISOM, MLIVIMPOT, MPEL, MAVI, MCEL, MLEE

# Donc MMLEP pour les quantitatives revient à :
# ZREVDISP, ALGNB, CNDUR, ZNIVVIE, PATRI_BRUT, PATRI_NET, decile_revenu,
# MRESPRIN, MAUTLOG, NPERS, NENFANTS, NACTIFS, MTDETTES, AGE_REEL, PATRIPRO
# AGE_RELATIF, ZREVFIN, RISKFI, NBASS, NBCC, MTDETTES, PATFISOM, MLIVIMPOT, MPEL, MAVI, MCEL, MLEE

#=============== Variables quantitatives uniques pour MMLA, MMLEP, MMLDD (34 variables)

#=============== Variables quantitatives uniques pour MMLA, MMLEP, MMLDD

# ZREVDISP, ALGNB, CNDUR, ZNIVVIE, LOYER, NBASS, NBCC, NBLOG,
# PATFISOM, MAUTLOG, PATRI_BRUT, PATRI_NET, ZREVFIN, Heritage,
# Donation_R, Donation_V, MRESPRIN, MLIVIMPOT, MPEL, MAVI, MCEL,
# MEPRETRAITE, MEPSALAR, MVMOB, PATRIRES, NPERS, NENFANTS,
# NACTIFS, MTDETTES, AGE_REEL, AGE_RELATIF, MLIVJEUNE,PATRIPRO,
# RISKFI, MCOMPTERM, MLEE, decile_revenu, decile_patrimoine

#=======================================================================================

#================= Correlations pour les quantitatives-qualitatives 
# Variables d'intérêt (Rappel)
vars_cibles <- c("MMLA", "MMLDDS", "MMLEP")

# Sélection des variables qualitatives dans la base 
vars_qualit <- names(base_menage_2014)[!names(base_menage_2014) %in% vars_quant]
vars_qualit

# Vérifier le type
sapply(base_menage_2014[vars_qualit], class)

# Nombre de modalités
nb_modalites <- sapply(base_menage_2014[vars_qualit],function(x) length(unique(na.omit(x))))
sort(nb_modalites)

data.frame(Variable = names(nb_modalites),Modalites = as.numeric(nb_modalites))

# Les tests selon les catégories
resultats_qualit <- data.frame()

# 2 modalités : Test de Wilcoxon
vars_wilcox <- c(
  "SEXEPR","DET","ENT_ENDET","PRET","RSAL","RTRA","R_ACTNPROF",
  "R_AVI","R_AVIEURO","R_AVIMULT","R_AVIRENTE","R_BON","R_BNXBAT",
  "R_CAPI","R_CCA","R_CCQ","R_CEL","R_COMP","R_CTIT",
  "R_DETDI","R_DETEQ","R_DETETU","R_DETIM","R_DETPR","R_DETRE",
  "R_DETRES","R_DETTR","R_EDUC","R_EPALO","R_EPSALA","R_LEE",
  "R_LIVDF","R_LIVEP","R_LIVF","R_LIVJEUN","R_LIVABL","R_LDD","R_LEP",
  "R_LOGAUT","R_LOGGRAT","R_LOGLOC","R_LOGSEC","R_LOGVAC",
  "R_OBLI","R_PANIER","R_PARSOC",
  "R_PATIMMO","R_PATPROF","R_PATFINANC",
  "R_PEA","R_PEAACT","R_PEASIC","R_PEATRE",
  "R_PEL","R_PEP","R_PERP","R_POA","R_PRO",
  "R_RESPRIN","R_RETRAIT","R_SUR","R_TERM","R_VMOB",
  "R_ACTION","R_ACT","R_ACTNCT","R_ACTPEA",
  "R_PEPBANC","R_PEPASSUR","R_PROHEXP",
  "R_SCPI","R_SICAV",
  "DETIMM","DETPRIV","DETPRO","VIAG",
  "EVEAUGM","EVEAUGM_AUT","EVEAUGM_BOU","EVEAUGM_HER",
  "EVEAUGM_JEU","EVEAUGM_LOG","EVEAUGM_PRIM",
  "EVEAUGM_PROF","EVEAUGM_REV","EVEAUGM_TER")

niveaux <- unique(tmp[[v2]])

if(length(niveaux) == 2){
  test <- wilcox.test(
    as.formula(paste(v1, "~", v2)),
    data = tmp
  )
  
  resultats_qualit <- rbind(
    resultats_qualit,
    data.frame(
      Variable_cible = v1,
      Variable = v2,
      Test = "Wilcoxon",
      Statistique = as.numeric(test$statistic),
      p_value = test$p.value
    )
  )
}

# Ordinales : Test de Spearman
vars_spearman <- c(
  "RISKFI","CATAGE_RELATIF",
  "NIVEDUC","decile_patrimoine",
  "decile_revenu")

base_menage_2020[vars_spearman] <- lapply(
  base_menage_2020[vars_spearman],
  function(x) as.numeric(as.character(x)))

for(v1 in vars_cibles){
  for(v2 in vars_spearman){
    tmp <- na.omit(base_menage_2020[, c(v1, v2)])
    if(nrow(tmp) > 3){
      
      test <- cor.test(
        tmp[[v1]],
        tmp[[v2]],
        method = "spearman",
        exact = FALSE)
      
      resultats_qualit <- rbind(
        resultats_qualit,
        data.frame(
          Variable_cible = v1,
          Variable = v2,
          Test = "Spearman",
          Statistique = unname(test$estimate),
          p_value = test$p.value))}}}

# Plusieurs modalités : Test de Kruskal-Wallis 
vars_kw <- c(
  "DIEXP","AUTSAL", "RFIN", "RIMM","RLOG","RMODIF_CHO",
  "RMODIF_DC","RMODIF_MAL","RMODIF_RET","RMODIF_SEP", "RNSAL",
  "RRET", "RRMI","HERTABD","HERTABH","HERDON","DIF","SITUAPR",
  "STATUTPR","CSPR8","ZEAT","LOGPROPRI","CONSEP","MATRIPR")

for(v1 in vars_cibles){
  for(v2 in vars_kw){
    tmp <- na.omit(base_menage_2020[, c(v1, v2)])
    if(nrow(tmp) > 3){
      test <- kruskal.test(
        as.formula(paste(v1, "~", v2)),
        data = tmp)
      resultats_qualit <- rbind(
        resultats_qualit,
        data.frame(
          Variable_cible = v1,
          Variable = v2,
          Test = "Kruskal-Wallis",
          Statistique = as.numeric(test$statistic),
          p_value = test$p.value))}}}

# Controle des seuils 
resultats_qualit$Signif <- ifelse(
  resultats_qualit$p_value < 0.05, "***",
  ifelse(
    resultats_qualit$p_value < 0.10, "**",
    ifelse(
      resultats_qualit$p_value < 0.15, "*",
      "")))

View(resultats_qualit)

#================== MMLA
# MATRIPR, SEXEPR, AUTSAL, SITUAPR, STATUTPR, DET, DIEXP, PRET,
# RFIN, RIMM, RISKFI, RLOG, RMODIF_CHO, RMODIF_MAL, RMODIF_SEP,
# RRET, RRMI, RTRA, R_ACTNPROF, R_AVI, R_AVIEURO, R_AVIMULT,
# R_AVIRENTE, R_BNXBAT, R_CAPI, R_CCA, R_CCQ, R_CEL, R_COMP,
# R_CTIT, R_DETDI, R_DETEQ, R_DETPR, R_DETIM, R_DETRES,
# R_EDUC, R_EPALO, R_EPSALA, R_LIVDF, R_LIVEP, R_LIVF,
# R_LOGAUT, R_LOGGRAT, R_LOGLOC, R_LOGSEC, R_LOGVAC,
# R_OBLI, R_PARSOC, R_PATIMMO, R_PATPROF, R_PEA, R_PEAACT,
# R_PEASIC, R_PEATRE, R_PEL, R_PEP, R_PERP, R_POA,
# R_PRO, R_RESPRIN, R_RETRAIT, R_SUR, R_TERM, R_VMOB,
# HERPAT, HERTABD, HERTABH, LOGPROPRI, ZEAT, HERDON,
# CONSEP, R_ACTION, R_ACT, R_PEPBANC, R_PEPASSUR,
# R_ACTPEA, R_SCPI, R_SICAV, DETIMM, DETPRIV, DETPRO,
# DIF, CATAGE_RELATIF, decile_patrimoine, R_PATFINANC,
# decile_revenu, NIVEDUC, CSPR8,
# EVEAUGM, EVEAUGM_AUT, EVEAUGM_BOU, EVEAUGM_HER,
# EVEAUGM_LOG, EVEAUGM_PRIM, EVEAUGM_PROF, EVEAUGM_REV,
# EVEBAIS, EVEBAIS_BOU, EVEBAIS_CHO, EVEBAIS_FAIL,
# EVEBAIS_INFL, EVEBAIS_MAL, EVEBAIS_PROF

# Rejetées
# ENT_ENDET, RMODIF_DC, RMODIF_RET, RNSAL, RSAL,
# R_BON, R_DETETU, R_DETRE, R_DETTR, R_LEE,
# R_LIVJEUN, R_PANIER, VIAG, R_ACTNCT,
# R_PROHEXP, EVEAUGM_JEU, EVEAUGM_TER,
# EVEBAIS_ACC, EVEBAIS_AUT, EVEBAIS_CONS,
# EVEBAIS_DIV, EVEBAIS_JEU, EVEBAIS_LOG,
# EVEBAIS_REV

#================== MMLDD
# Retenues
# MATRIPR, SEXEPR, DET, DIEXP, ENT_ENDET, PRET,
# RFIN, RIMM, RISKFI, RLOG, RMODIF_CHO, RMODIF_DC,
# RMODIF_MAL, RMODIF_RET, RMODIF_SEP,
# RNSAL, RRET, RRMI, RTRA, R_ACTNPROF,
# R_AVI, R_AVIEURO, R_AVIMULT, R_AVIRENTE,
# R_BNXBAT, R_CAPI, R_CCA, R_CCQ, R_CEL,
# R_COMP, R_CTIT, R_DETDI, R_DETEQ,
# R_DETPR, R_DETIM, R_DETRE, R_DETRES,
# R_EDUC, R_EPALO, R_EPSALA, R_LEE,
# R_LIVDF, R_LIVEP, R_LIVF,
# R_LOGAUT, R_LOGGRAT, R_LOGLOC, R_LOGSEC,
# R_LOGVAC, R_OBLI, R_PARSOC, R_PATIMMO,
# R_PATPROF, R_PEA, R_PEAACT, R_PEASIC,
# R_PEATRE, R_PEL, R_PEP, R_PERP, R_POA,
# R_PRO, R_RESPRIN, R_RETRAIT, R_SUR,
# R_TERM, R_VMOB, HERTABD, HERTABH,
# HERDON, LOGPROPRI, ZEAT, CONSEP,
# R_ACTION, R_ACT, R_ACTNCT, R_PEPBANC,
# R_PEPASSUR, R_PROHEXP, DETPRO,
# R_ACTPEA, R_SCPI, R_SICAV,
# DETIMM, DETPRIV, DIF, CATAGE_RELATIF,
# decile_patrimoine, R_PATFINANC,
# decile_revenu, NIVEDUC, CSPR8,
# EVEAUGM, EVEAUGM_AUT, EVEAUGM_BOU,
# EVEAUGM_HER, EVEAUGM_JEU,
# EVEAUGM_LOG, EVEAUGM_PRIM,
# EVEAUGM_PROF, EVEAUGM_REV,
# EVEBAIS_BOU, EVEBAIS_CHO,
# EVEBAIS_LOG, EVEBAIS_MAL

# Rejetées
# AUTSAL, RSAL, R_BON, R_DETETU,
# R_DETTR, R_LIVJEUN, R_PANIER,
# VIAG, HERPAT,
# EVEAUGM_TER,
# EVEBAIS, EVEBAIS_ACC,
# EVEBAIS_AUT, EVEBAIS_CONS,
# EVEBAIS_DIV, EVEBAIS_FAIL,
# EVEBAIS_INFL, EVEBAIS_JEU,
# EVEBAIS_PROF, EVEBAIS_REV

#================== MMLEP

# Retenues
# MATRIPR, SEXEPR, AUTSAL, SITUAPR, STATUTPR, DET, PRET, RFIN, RIMM, RISKFI, RLOG,
# RNSAL, RRET, RRMI, RSAL, RTRA, R_ACTNPROF, R_AVI, R_AVIEURO, R_AVIMULT, R_BON, R_BNXBAT, R_CCA,
# R_COMP, R_CTIT, R_DETIM, R_EPALO, R_EPSALA, R_LIVDF, R_LIVEP, R_LIVF, R_LIVJEUN, R_LIVABL,
# R_LDD, R_LEP, R_LOGAUT, R_LOGGRAT, R_LOGLOC, R_LOGSEC, R_OBLI, R_PARSOC, R_PATIMMO, 
# R_PATPROF, R_PATFINANC, R_PEA, R_PEAACT, R_PEASIC, R_PEATRE, R_PEL, R_PEP, R_POA, R_RETRAIT, R_SUR,
# R_VMOB, HERTABD, ZEAT, CONSEP, R_ACTION, R_ACT, R_ACTNCT, R_PEPBANC, R_ACTPEA, R_SICAV, DETIMM, DETPRIV,
# DIF, CATAGE_RELATIF, decile_patrimoine, decile_revenu,
# NIVEDUC, CSPR8, EVEAUGM, EVEAUGM_BOU, EVEAUGM_HER, EVEAUGM_LOG,
# EVEAUGM_PRIM, EVEAUGM_PROF, EVEAUGM_REV,
# EVEBAIS_AUT, EVEBAIS_BOU,
# EVEBAIS_INFL, EVEBAIS_MAL

# Rejetées
# DIEXP, ENT_ENDET,
# RMODIF_CHO, RMODIF_DC, RMODIF_MAL,
# RMODIF_RET, RMODIF_SEP,
# R_AVIRENTE, R_CAPI, R_CCQ,
# R_CEL, R_DETDI, R_DETEQ,
# R_DETETU, R_DETPR, R_DETRE,
# R_DETRES, R_DETTR, R_EDUC,
# R_LEE, R_LOGVAC, R_PANIER,
# R_PERP, R_PRO,
# R_RESPRIN, R_TERM,
# HERPAT, HERTABH,
# HERDON, LOGPROPRI,
# VIAG, R_PEPASSUR,
# R_PROHEXP, DETPRO,
# R_SCPI,
# EVEAUGM_AUT, EVEAUGM_JEU,
# EVEAUGM_TER,
# EVEBAIS, EVEBAIS_ACC,
# EVEBAIS_CHO, EVEBAIS_CONS,
# EVEBAIS_DIV, EVEBAIS_FAIL,
# EVEBAIS_JEU, EVEBAIS_LOG,
# EVEBAIS_PROF, EVEBAIS_REV


#=========== Variables qualitatives uniques pour MMLA, MMLEP, MMLDD et les Quants (122 variables)

# MATRIPR, SEXEPR, AUTSAL, SITUAPR, STATUTPR, DET, DIEXP,
# ENT_ENDET, PRET, RFIN, RIMM, RISKFI, RLOG,
# RMODIF_CHO, RMODIF_DC, RMODIF_MAL, RMODIF_RET, RMODIF_SEP,
# RNSAL, RRET, RRMI, RSAL, RTRA, R_ACTNPROF,
# R_AVI, R_AVIEURO, R_AVIMULT, R_AVIRENTE,
# R_BON, R_BNXBAT, R_CAPI, R_CCA, R_CCQ,
# R_CEL, R_COMP, R_CTIT,
# R_DETDI, R_DETEQ, R_DETPR, R_DETIM,
# R_DETRES, R_DETRE, R_EDUC, R_EPALO, R_EPSALA,
# R_LEE, R_LIVDF, R_LIVEP, R_LIVF, R_LIVJEUN,
# R_LIVABL, R_LDD, R_LEP,
# R_LOGAUT, R_LOGGRAT, R_LOGLOC, R_LOGSEC, R_LOGVAC,
# R_OBLI, R_PARSOC,
# R_PATIMMO, R_PATPROF, R_PATFINANC,
# R_PEA, R_PEAACT, R_PEASIC, R_PEATRE,
# R_PEL, R_PEP, R_PERP, R_POA, R_PRO,
# R_RESPRIN, R_RETRAIT, R_SUR, R_TERM, R_VMOB,
# HERPAT, HERTABD, HERTABH, HERDON,
# LOGPROPRI, ZEAT, CONSEP,
# R_ACTION, R_ACT, R_ACTNCT,
# R_PEPBANC, R_PEPASSUR, R_PROHEXP,
# R_ACTPEA, R_SCPI, R_SICAV,
# DETIMM, DETPRIV, DETPRO,
# DIF, CATAGE_RELATIF,
# decile_patrimoine, decile_revenu,
# NIVEDUC, CSPR8,
# EVEAUGM, EVEAUGM_AUT, EVEAUGM_BOU,
# EVEAUGM_HER, EVEAUGM_JEU, EVEAUGM_LOG,
# EVEAUGM_PRIM, EVEAUGM_PROF, EVEAUGM_REV,
# EVEAUGM_TER,
# EVEBAIS, EVEBAIS_AUT, EVEBAIS_BOU,
# EVEBAIS_CHO, EVEBAIS_FAIL, EVEBAIS_INFL,
# EVEBAIS_LOG, EVEBAIS_MAL, EVEBAIS_PROF

#========================================== R_LIVABL, R_LDD, R_LEP 
# Variables cibles
vars_cibles <- c("R_LIVABL", "R_LDD", "R_LEP")

# Variables qualitatives explicatives
vars_qualit <- names(base_menage_2014)[!names(base_menage_2014) %in% vars_quant]

vars_qualit <- setdiff(vars_qualit, vars_cibles)

# Tableau de résultats
resultats_quali <- data.frame()

for(v1 in vars_cibles){
  for(v2 in vars_qualit){
    tmp <- na.omit(base_menage_2014[, c(v1, v2)])
    tab <- table(tmp[[v1]], tmp[[v2]])
    
    # On ne teste que si au moins 2 modalités de chaque côté
    if(nrow(tab) >= 2 & ncol(tab) >= 2){
      test <- suppressWarnings(chisq.test(tab))
      resultats_quali <- rbind(
        resultats_quali,
        data.frame(
          Variable_cible = v1,
          Variable = v2,
          Test = "Chi2",
          Statistique = as.numeric(test$statistic),
          p_value = test$p.value))}}}

# Ajout des seuils 
resultats_quali$Signif <- ifelse(
  resultats_quali$p_value < 0.01, "***",
  ifelse(
    resultats_quali$p_value < 0.05, "**",
    ifelse(resultats_quali$p_value < 0.1, "*","")))

View(resultats_quali)

#================== R_LIVABL
# Retenues 
# MATRIPR, SEXEPR, AUTSAL, SITUAPR, STATUTPR, DET, DIEXP, ENT_ENDET,
# RFIN, RIMM, RISKFI, RLOG, RMODIF_MAL, RMODIF_SEP, RNSAL,
# RRET, RRMI, RSAL, R_ACTNPROF, R_AVI, R_AVIEURO, R_AVIMULT,
# R_AVIRENTE, R_CCQ, R_CEL, R_COMP, R_CTIT, R_DETDI, R_DETIM,
# R_DETRE, R_DETTR, R_EDUC, R_EPALO, R_EPSALA, R_LIVDF,
# R_LIVEP, R_LIVF, R_LIVJEUN, R_LOGAUT, R_LOGGRAT, R_LOGLOC,
# R_LOGSEC, R_LOGVAC, R_OBLI, R_PARSOC, R_PATIMMO, R_PATPROF,
# R_PEA, R_PEAACT, R_PEASIC, R_PEATRE, R_PEL, R_PEP,
# R_PERP, R_POA, R_PRO, R_RESPRIN, R_RETRAIT, R_SUR,
# R_TERM, R_VMOB, HERPAT, HERTABD, HERTABH, LOGPROPRI,
# MPANEL, ZEAT, HERDON, CONSEP, R_ACTION, R_ACT,
# R_PEPBANC, R_PEPASSUR, R_ACTPEA, R_SCPI, R_SICAV,
# DETIMM, DETPRIV, DIF, CATAGE_RELATIF, decile_patrimoine,
# R_PATFINANC, NIVEDUC, CSPR8, EVEAUGM, EVEAUGM_AUT,
# EVEAUGM_HER, EVEAUGM_LOG, EVEAUGM_PRIM, EVEAUGM_REV,
# EVEBAIS_BOU, EVEBAIS_CONS, EVEBAIS_DIV, EVEBAIS_FAIL,
# EVEBAIS_MAL

# Rejetées 
# PRET, RMODIF_CHO, RMODIF_DC, RMODIF_RET, RTRA,
# R_BON, R_BNXBAT, R_CAPI, R_CCA, R_DETEQ,
# R_DETETU, R_DETPR, R_DETRES, R_LEE,
# R_PANIER, VIAG, R_ACTNCT, R_PROHEXP,
# DETPRO, EVEAUGM_BOU, EVEAUGM_JEU,
# EVEAUGM_PROF, EVEAUGM_TER, EVEBAIS,
# EVEBAIS_ACC, EVEBAIS_AUT, EVEBAIS_CHO,
# EVEBAIS_INFL, EVEBAIS_JEU, EVEBAIS_LOG,
# EVEBAIS_PROF, EVEBAIS_REV

#================== R_LDD

# Retenues
# MATRIPR, SEXEPR, AUTSAL, SITUAPR, STATUTPR,
# DET, DIEXP, ENT_ENDET, PRET, RFIN, RIMM, RLOG,
# RMODIF_CHO, RMODIF_RET, RMODIF_SEP, RNSAL, RRET,
# RRMI, RTRA, R_ACTNPROF, R_AVI, R_AVIEURO, R_AVIMULT,
# R_AVIRENTE, R_BNXBAT, R_CAPI, R_CCA, R_CCQ, R_CEL,
# R_COMP, R_CTIT, R_DETDI, R_DETPR, R_DETIM, R_DETRE,
# R_DETRES, R_DETTR, R_EDUC, R_EPALO, R_EPSALA,
# R_LEE, R_LIVDF, R_LIVEP, R_LIVF, R_LIVJEUN,
# R_LOGAUT, R_LOGGRAT, R_LOGLOC, R_LOGSEC, R_LOGVAC,
# R_OBLI, R_PARSOC, R_PATIMMO, R_PATPROF, R_PEA,
# R_PEAACT, R_PEASIC, R_PEATRE, R_PEL, R_PEP,
# R_PERP, R_POA, R_PRO, R_RESPRIN, R_RETRAIT,
# R_SUR, R_TERM, R_VMOB, HERPAT, HERTABD, HERTABH,
# LOGPROPRI, ZEAT, HERDON, CONSEP, R_ACTION, R_ACT,
# R_ACTNCT, R_PEPBANC, R_PEPASSUR, R_PROHEXP,
# DETPRO, R_ACTPEA, R_SCPI, R_SICAV, DETIMM,
# DETPRIV, DIF, CATAGE_RELATIF, decile_patrimoine,
# R_PATFINANC, NIVEDUC, CSPR8, EVEAUGM,
# EVEAUGM_BOU, EVEAUGM_HER, EVEAUGM_LOG,
# EVEAUGM_PRIM, EVEAUGM_PROF, EVEAUGM_REV,
# EVEBAIS_BOU, EVEBAIS_CHO, EVEBAIS_LOG,
# EVEBAIS_MAL

# Rejetées
# RMODIF_DC, RMODIF_MAL, RSAL, R_BON,
# R_DETEQ, R_DETETU, R_PANIER, MPANEL,
# VIAG, EVEAUGM_AUT, EVEAUGM_JEU,
# EVEAUGM_TER, EVEBAIS, EVEBAIS_ACC,
# EVEBAIS_AUT, EVEBAIS_CONS, EVEBAIS_DIV,
# EVEBAIS_FAIL, EVEBAIS_INFL,
# EVEBAIS_JEU, EVEBAIS_PROF, EVEBAIS_REV

#================== R_LEP

# Retenues
# MATRIPR, SEXEPR, AUTSAL, SITUAPR, STATUTPR,
# PRET, RFIN, RIMM, RLOG, RNSAL, RRET,
# RRMI, RSAL, RTRA, R_ACTNPROF, R_AVI,
# R_AVIMULT, R_BNXBAT, R_CCA, R_COMP,
# R_CTIT, R_DETIM, R_EPALO, R_EPSALA,
# R_LIVDF, R_LIVEP, R_LIVF, R_LIVJEUN,
# R_LOGAUT, R_LOGGRAT, R_LOGLOC, R_LOGSEC,
# R_PATIMMO, R_PATPROF, R_PEA, R_PEAACT,
# R_PEASIC, R_PEATRE, R_PEL, R_PEP,
# R_POA, R_RESPRIN, R_RETRAIT, R_SUR,
# R_VMOB, HERTABD, LOGPROPRI, MPANEL,
# ZEAT, CONSEP, R_ACTION, R_ACT,
# R_ACTNCT, R_PEPBANC, R_ACTPEA,
# R_SICAV, DETIMM, DETPRIV, DIF,
# CATAGE_RELATIF, decile_patrimoine,
# R_PATFINANC, NIVEDUC, CSPR8,
# EVEAUGM, EVEAUGM_BOU, EVEAUGM_HER,
# EVEAUGM_LOG, EVEAUGM_PRIM,
# EVEAUGM_PROF, EVEAUGM_REV,
# EVEBAIS_AUT, EVEBAIS_BOU,
# EVEBAIS_INFL, EVEBAIS_MAL,
# EVEBAIS_REV

# Rejetées
# DET, DIEXP, ENT_ENDET, R_AVIEURO,
# R_AVIRENTE, R_BON, R_CAPI, R_CCQ,
# R_CEL, R_DETDI, R_DETEQ, R_DETETU,
# R_DETPR, R_DETRE, R_DETRES, R_DETTR,
# R_EDUC, R_LEE, R_LOGVAC, R_OBLI,
# R_PANIER, R_PARSOC, R_PERP, R_PRO,
# R_TERM, HERPAT, HERTABH, HERDON,
# VIAG, R_PEPASSUR, R_PROHEXP,
# DETPRO, R_SCPI, RMODIF_CHO,
# RMODIF_DC, RMODIF_MAL, RMODIF_RET,
# RMODIF_SEP, EVEAUGM_AUT,
# EVEAUGM_JEU, EVEAUGM_TER,
# EVEBAIS, EVEBAIS_ACC, EVEBAIS_CHO,
# EVEBAIS_CONS, EVEBAIS_DIV,
# EVEBAIS_FAIL, EVEBAIS_JEU,
# EVEBAIS_LOG, EVEBAIS_PROF

# Variables qualitatives uniques pour R_LIVABL, R_LDD, R_LEP 

# MPANEL, MPANEL_NEXT, MATRIPR, SEXEPR, AUTSAL, SITUAPR, STATUTPR, DET,
# DIEXP, ENT_ENDET, PRET, RFIN, RIMM, RISKFI, RLOG, RMODIF_CHO,
# RMODIF_MAL, RMODIF_RET, RMODIF_SEP, RNSAL, RRET, RRMI, RSAL, RTRA,
# R_ACTNPROF, R_AVI, R_AVIEURO, R_AVIMULT, R_AVIRENTE, R_BNXBAT,
# R_CAPI, R_CCA, R_CCQ, R_CEL, R_COMP, R_CTIT, R_DETDI, R_DETPR,
# R_DETIM, R_DETRE, R_DETRES, R_DETTR, R_EDUC, R_EPALO, R_EPSALA,
# R_LEE, R_LIVDF, R_LIVEP, R_LIVF, R_LIVJEUN, R_LOGAUT, R_LOGGRAT,
# R_LOGLOC, R_LOGSEC, R_LOGVAC, R_OBLI, R_PARSOC, R_PATIMMO,
# R_PATPROF, R_PATFINANC, R_PEA, R_PEAACT, R_PEASIC, R_PEATRE,
# R_PEL, R_PEP, R_PERP, R_POA, R_PRO, R_RESPRIN, R_RETRAIT, R_SUR,
# R_TERM, R_VMOB, HERPAT, HERTABD, HERTABH, LOGPROPRI, ZEAT,
# HERDON, CONSEP, R_ACTION, R_ACT, R_ACTNCT, R_PEPBANC,
# R_PEPASSUR, R_PROHEXP, R_ACTPEA, R_SCPI, R_SICAV, DETIMM,
# DETPRIV, DETPRO, DIF, CATAGE_RELATIF, decile_patrimoine,
# NIVEDUC, CSPR8, EVEAUGM, EVEAUGM_AUT, EVEAUGM_BOU, EVEAUGM_HER,
# EVEAUGM_LOG, EVEAUGM_PRIM, EVEAUGM_PROF, EVEAUGM_REV,
# EVEBAIS_AUT, EVEBAIS_BOU, EVEBAIS_CONS, EVEBAIS_DIV,
# EVEBAIS_FAIL, EVEBAIS_INFL, EVEBAIS_LOG, EVEBAIS_MAL,
# EVEBAIS_REV

#==================== Correlations pour les qualitatives-quantitatives
vars_cibles

# Variables quantitatives
vars_quant_exp <- setdiff(vars_quant, vars_cibles)

# Tableau résultats
resultats_quant <- data.frame()

for(v1 in vars_cibles){
  for(v2 in vars_quant_exp){
    tmp <- na.omit(base_menage_2014[, c(v1, v2)])
    if(nrow(tmp) > 3){
      test <- wilcox.test(
        as.formula(paste(v2, "~", v1)),
        data = tmp)
      
      resultats_quant <- rbind(
        resultats_quant,
        data.frame(
          Variable_cible = v1,
          Variable = v2,
          Test = "Wilcoxon",
          Statistique = as.numeric(test$statistic),
          p_value = test$p.value))}}}

# Ajout des seuils 
resultats_quant$Signif <- ifelse(
  resultats_quant$p_value < 0.05, "***",
  ifelse(
    resultats_quant$p_value < 0.10, "**",
    ifelse(
      resultats_quant$p_value < 0.15, "*",
      "")))

View(resultats_quant)

#=========== R_LIVABL
# Retenues
# MMLA, MMLDD, MMLEP, ZREVDISP, ALGNB, NENFANTS,
# NPERS, CNDUR, NACTIFS, ZNIVVIE, RISKFI, LOYER, NBASS,
# NBCC, NBLOG, NBUC, NE, PATFISOM, MTDETTES,
# PATRI_BRUT, PATRI_NET, AGE_REEL, AGE_RELATIF,
# ZREVFIN, decile_revenu, Heritage, Donation_R,
# Donation_V, MRESPRIN, MAUTLOG, MLIVJEUNE,
# MLIVIMPOT, MPEL, MAVI, MCEL, MEPRETRAITE,
# MPEP, MEPSALAR, MVMOB, MBONCAISSE,
# MCOMPTERM, PATRIRES, PATRIPRO

# Rejetées
# MCASSOCIE, MLEE

#=========== R_DD
# Retenues
# MMLA, MMLDD, MMLEP, ZREVDISP, ALGNB,
# NENFANTS, CNDUR, NACTIFS, ZNIVVIE, RISKFI,
# LOYER, NBASS, NBCC, NBLOG, NBUC, NE,
# PATFISOM, MTDETTES, PATRI_BRUT, PATRI_NET,
# AGE_REEL, AGE_RELATIF, ZREVFIN, decile_revenu,
# Heritage, Donation_R, Donation_V, MRESPRIN,
# MAUTLOG, MLIVJEUNE, MLIVIMPOT, MPEL, MAVI,
# MCEL, MEPRETRAITE, MPEP, MEPSALAR, MVMOB,
# MCOMPTERM, MCASSOCIE, MLEE, PATRIRES, PATRIPRO

# Rejetées 
# NPERS, MBONCAISSE

#========== R_LEP
# Retenues
# MMLA, MMLDD, MMLEP,
# ZREVDISP, ALGNB, CNDUR, ZNIVVIE,
# RISKFI, NBASS, NBCC, NBLOG,
# PATFISOM, MTDETTES, PATRI_BRUT,
# PATRI_NET, AGE_REEL, AGE_RELATIF,
# decile_revenu, Donation_R, Donation_V,
# MRESPRIN, MAUTLOG, MLIVJEUNE,
# MLIVIMPOT, MPEL, MAVI,
# MEPRETRAITE, MPEP, MEPSALAR,
# MVMOB, MBONCAISSE, MCASSOCIE,
# PATRIRES, PATRIPRO

# Rejetées
# NENFANTS, NPERS, NACTIFS, LOYER,
# NBUC, NE, ZREVFIN, Heritage,
# MCEL, MCOMPTERM, MLEE

# Variable uniques pour R_LIVABL, R_LEP, R_LDDS
# POND, MMLA, MMLDD, MMLEP,
# ZREVDISP, ALGNB, NENFANTS, NPERS,
# CNDUR, NACTIFS, ZNIVVIE, RISKFI,
# LOYER, NBASS, NBCC, NBLOG,
# NBUC, NE, PATFISOM, MTDETTES,
# PATRI_BRUT, PATRI_NET, AGE_REEL,
# AGE_RELATIF, ZREVFIN, decile_revenu,
# Heritage, Donation_R, Donation_V,
# MRESPRIN, MAUTLOG, MLIVJEUNE,
# MLIVIMPOT, MPEL, MAVI, MCEL,
# MEPRETRAITE, MPEP, MEPSALAR,
# MVMOB, MBONCAISSE, MCOMPTERM,
# MCASSOCIE, MLEE, PATRIRES, PATRIPRO

#============================ Toutes les variables=================================#
# Pour reconstituer la base
# IDENT, poids et variables de panel conservés devant

vars_finales <- c(
  "IDENT",
  "POND",
  "MPANEL",
  "MPANEL_NEXT",
  
  # Les variables expliquées 
  "R_LIVABL","R_LDD","R_LEP",
  "NBLA","NBLDD","NBLEP",
  "MMLA","MMLEP","MMLDDS",
  
  # Quantitatives retenues
  "ZREVDISP", "ALGNB", "NENFANTS", "NPERS", "CNDUR",
  "NACTIFS", "ZNIVVIE", "RISKFI", "LOYER", "NBASS",
  "NBCC", "NBLOG", "NBUC", "NE", "PATFISOM","PATRIPRO",
  "MTDETTES", "PATRI_BRUT", "PATRI_NET", "PATRIRES",
  "ZREVFIN", "AGE_REEL", "AGE_RELATIF",
  "Heritage", "Donation_R", "Donation_V",
  "MRESPRIN", "MAUTLOG", "MLIVJEUNE", "MLIVIMPOT",
  "MPEL", "MAVI", "MCEL", "MEPRETRAITE",
  "MPEP", "MEPSALAR", "MVMOB", "MBONCAISSE",
  "MCOMPTERM", "MCASSOCIE", "MLEE",
  "decile_revenu", "decile_patrimoine","PATIMM",
  
  # Qualitatives retenues
  "MATRIPR", "SEXEPR", "AUTSAL", "SITUAPR", "STATUTPR",
  "DET", "DIEXP", "ENT_ENDET", "PRET", "RFIN", "RIMM", "RLOG",
  "RMODIF_CHO", "RMODIF_DC", "RMODIF_MAL",
  "RMODIF_RET", "RMODIF_SEP", "RNSAL", "RRET", "RRMI", "RSAL", "RTRA",
  "R_ACTNPROF", "R_AVI", "R_AVIEURO", "R_AVIMULT", "R_AVIRENTE",
  "R_BON", "R_BNXBAT", "R_CAPI", "R_CCA", "R_CCQ", "R_CEL", "R_COMP", "R_CTIT",
  "R_DETDI", "R_DETEQ", "R_DETPR", "R_DETIM", "R_DETRES", "R_DETRE", "R_DETTR",
  "R_EDUC", "R_EPALO", "R_EPSALA", "R_LEE","R_LIVDF", "R_LIVEP", "R_LIVF", 
  "R_LIVJEUN","R_LOGAUT", "R_LOGGRAT", "R_LOGLOC","R_LOGSEC", "R_LOGVAC",
  "R_OBLI", "R_PARSOC","R_PATIMMO", "R_PATPROF", "R_PATFINANC",
  "R_PEA", "R_PEAACT", "R_PEASIC", "R_PEATRE",
  "R_PEL", "R_PEP", "R_PERP", "R_POA", "R_PRO", "R_RESPRIN", "R_RETRAIT",
  "R_SUR", "R_TERM", "R_VMOB", "HERPAT", "HERTABD", "HERTABH", "HERDON",
  "LOGPROPRI", "ZEAT", "CONSEP", "R_ACTION", "R_ACT", "R_ACTNCT",
  "R_PEPBANC", "R_PEPASSUR", "R_PROHEXP", "R_ACTPEA", "R_SCPI", "R_SICAV",
  "DETIMM", "DETPRIV", "DETPRO", "VIAG", "DIF", "CATAGE_RELATIF",
  "NIVEDUC", "CSPR8", "TYPMEN5","DIREVY")

# J'ai laissé tombé les variables evenement de hausse et de baisse de patrimoine

# Les variables de essentielles qui ne se retrouvent pas dans ce lot c'est : 
# NBLA, NBLDD, NBLEP, TYPMEN5, DIREVY

# Conserver uniquement les variables présentes dans la base
vars_finales <- intersect(vars_finales, names(base_menage_2014))

# Extraction
base_modele <- base_menage_2014[, vars_finales]

# Vérification
dim(base_modele)
dim(base_menage_2014)

base_modele$HERPAT = NULL

dim(base_modele) # Reste 142 variables à traiter

variables_essentielles

#======================== Eliminer les redondances par construction
names(base_modele)

# A supprimer car utilisé pour calculer une autre ou liée
base_modele$ALGNB = NULL  # Car NBLOG est nombre de logement y compris la residence principale
base_modele$NBUC = NULL # Nombre unité de consommation pas facile à interpreter 
base_modele$PATRI_BRUT = NULL # Je garde les elements du patrimoine 
base_modele$PATRI_NET = NULL # Je garde les composants du patrimoine 
base_modele$ZREVFIN = NULL # Contient ZREVPROD utilisé dans ZREVDISP
base_modele$AUTSAL = NULL # Pas primordiale 
base_modele$ENT_ENDET = NULL # Je garde la variable DET  globale
base_modele$RIMM = NULL # Pas très interessant puisse que j'ai la variable loyer 
base_modele$RNSAL= NULL # Pas très intéressant 
base_modele$R_BNXBAT = NULL # Detention de batiment, pas très interessant 
base_modele$HERPAT = NULL # Part des donations dans le patrimoine 
base_modele$R_PROHEXP = NULL # Détention d actifs productifs (autres que terres) exploités
# professionnellement par le ménage (hors entreprise)
base_modele$VIAG = NULL # Pas interessant 

dim(base_modele)

#======================== Eliminer par autocorrelation les redondances
vars_num <- names(base_modele)[sapply(base_modele, is.numeric)]

vars_num <- setdiff(vars_num,
                    c("IDENT","POND","MMLA","MMLDDS","MMLEP","R_LIVABL","R_LDD","R_LEP"))  # Pas concerné

corr <- cor(
  base_modele[, vars_num],
  use = "pairwise.complete.obs",
  method = "spearman")

View(corr)

# Retrouver les couples de variables 
high_corr <- which(abs(corr) > 0.79 & upper.tri(corr),arr.ind = TRUE)

high_corr_df <- data.frame(
  Variable_1 = rownames(corr)[high_corr[,1]],
  Variable_2 = colnames(corr)[high_corr[,2]],
  Correlation = corr[high_corr])

high_corr_df <- high_corr_df[
  order(abs(high_corr_df$Correlation),
        decreasing = TRUE),]

high_corr_dft

all(base_modele$Donation_R==base_modele$Donation_V) # Vérifier si c'est les mêmes choses

# Elimination des redondances 
vars_suppr <- c("Donation_V", "AGE_REEL", "NBASS")

base_reduite <- base_modele[,!names(base_modele) %in% vars_suppr]

# Refaire le même processus 
vars_num <- names(base_reduite)[sapply(base_reduite, is.numeric)]

vars_num <- setdiff(vars_num,c("IDENT","POND","MLA","MLDDS","MLEP","R_LIVABL","R_LDD","R_LEP"))

corr <- cor(base_reduite[, vars_num], use = "pairwise.complete.obs",method = "spearman")

high_corr <- which( abs(corr) > 0.79 & upper.tri(corr), arr.ind = TRUE)

high_corr_df <- data.frame(
  Variable_1 = rownames(corr)[high_corr[,1]],
  Variable_2 = colnames(corr)[high_corr[,2]],
  Correlation = corr[high_corr])

high_corr_df <- high_corr_df[order(abs(high_corr_df$Correlation),decreasing = TRUE),]

high_corr_df

#=========== Je me suis arrêté a cette étape du processus et je passe aux qualitatives
#********************** Par LASSO pour pallier aux multicolinearité, et selectionner aussi **********************
#********************** les variables les plus importantes, et la stabilité du modèle  **************************

# Vérification et correction 
length(base_reduite$POND)
nrow(base_reduite)

sort(colSums(is.na(base_reduite)),decreasing = TRUE)

V=base_reduite %>% 
  filter(is.na(RISKFI)) %>% 
  select(RISKFI)

View(V)

base_reduite$MMLA[is.na(base_reduite$MMLA)] <- 0 # Les NA
base_reduite$MMLDDS[is.na(base_reduite$MMLDDS)] <- 0
base_reduite$MMLEP[is.na(base_reduite$MMLEP)] <- 0

colSums(is.na(base_reduite[, c("MMLA", "MMLDDS", "MMLEP")]))

# Remplacer les NA manquants par le mode 
mode_riskfi <- names(
  which.max(
    table(base_reduite$RISKFI)))

mode_riskfi

base_reduite$RISKFI[is.na(base_reduite$RISKFI)] <- mode_riskfi


#================  Selection des variables par LASSO
install.packages("glmnet")
library(glmnet)

# Reproductibilité
set.seed(123)

# Variables à exclure
vars_exclues <- c(
  "IDENT",
  "POND",
  "MPANEL",
  "MPANEL_NEXT",
  "R_LIVABL",
  "R_LDD",
  "R_LEP",
  "MMLA",
  "MMLDDS",
  "MMLEP")

# Variables explicatives candidates
vars_x <- setdiff(
  names(base_reduite),
  vars_exclues)

# Construction de la matrice explicative
X <- model.matrix(
  ~ .,
  data = base_reduite[, vars_x]
)[, -1]

# Vérification du nombre de variables réellement utilisées
dim(X)

# Variables dépendantes binaires
cibles_binaires <- c(
  "R_LIVABL",
  "R_LDD",
  "R_LEP")

# Variables dépendantes continues
cibles_continues <- c(
  "MMLA",
  "MMLDDS",
  "MMLEP")

# Liste destinée à stocker les résultats
resultats <- list()

# --------------------------
# LASSO pour les variables binaires
# --------------------------

for (y_name in cibles_binaires) {
  
  cat("\nTraitement de :", y_name, "\n")
  
  y <- base_reduite[[y_name]]
  
  cvfit <- cv.glmnet(
    x = X,
    y = y,
    family = "binomial",
    alpha = 1,
    weights = base_reduite$POND,
    nfolds = 5,
    trace.it = 1
  )
  
  coefs <- coef(
    cvfit,
    s = "lambda.1se"
  )
  
  vars_sel <- rownames(coefs)[
    as.vector(coefs != 0)
  ]
  
  vars_sel <- setdiff(
    vars_sel,
    "(Intercept)"
  )
  
  resultats[[y_name]] <- list(
    lambda_min = cvfit$lambda.min,
    lambda_1se = cvfit$lambda.1se,
    variables = vars_sel
  )
}

# --------------------------
# LASSO pour les variables continues
# --------------------------

for (y_name in cibles_continues) {
  
  cat("\nTraitement de :", y_name, "\n")
  
  y <- base_reduite[[y_name]]
  
  cvfit <- cv.glmnet(
    x = X,
    y = y,
    family = "gaussian",
    alpha = 1,
    weights = base_reduite$POND,
    nfolds = 10
  )
  
  coefs <- coef(
    cvfit,
    s = "lambda.1se"
  )
  
  vars_sel <- rownames(coefs)[
    as.vector(coefs != 0)
  ]
  
  vars_sel <- setdiff(
    vars_sel,
    "(Intercept)"
  )
  
  resultats[[y_name]] <- list(
    lambda_min = cvfit$lambda.min,
    lambda_1se = cvfit$lambda.1se,
    variables = vars_sel
  )
}

# --------------------------
# Résultats détaillés
# --------------------------

for (v in names(resultats)) {
  
  cat("\n----------------------------------\n")
  cat("Variable dépendante :", v, "\n")
  cat("----------------------------------\n")
  
  cat("Lambda min :", resultats[[v]]$lambda_min, "\n")
  cat("Lambda 1se :", resultats[[v]]$lambda_1se, "\n")
  
  cat(
    "Nombre de variables retenues :",
    length(resultats[[v]]$variables),
    "\n\n"
  )
  
  print(resultats[[v]]$variables)}



# --------------------------
# Variables communes aux 6 modèles
# --------------------------

variables_communes <- Reduce(
  intersect,
  lapply(
    resultats,
    function(x) x$variables
  )
)

cat("\nVariables communes aux 6 modèles :\n")
print(variables_communes)

# --------------------------
# Fréquence de sélection des variables
# --------------------------

freq <- sort(
  table(
    unlist(
      lapply(
        resultats,
        function(x) x$variables
      )
    )
  ),
  decreasing = TRUE
)

cat("\nFréquence de sélection :\n")
print(freq)

vars_retenues <- c("ZREVDISP", "NENFANTS", "NPERS", "CNDUR", "NACTIFS",
                   "RISKFI", "LOYER", "NE",
                   "PATFISOM", "PATRIPRO", "MTDETTES", "PATRIRES", "AGE_RELATIF",
                   "Donation_R", "MRESPRIN", "MPEL", "MAVI", "MCEL",
                   "MEPRETRAITE", "MEPSALAR", "MVMOB", "MBONCAISSE", "MCOMPTERM",
                   "MCASSOCIE", "decile_revenu", "decile_patrimoine", "MATRIPR",
                   "SEXEPR", "SITUAPR", "STATUTPR", "DIEXP", "PRET",
                   "RMODIF_CHO", "RMODIF_MAL","NBLOG",
                   "R_AVI", "R_AVIMULT", "R_AVIRENTE",
                   "R_CAPI", "R_CCA", "R_CCQ", "R_CEL", "R_COMP",
                   "R_DETDI", "R_DETEQ", "R_DETRES", "R_DETTR", "R_EPSALA",
                   "R_LIVDF", "R_LIVF", "R_LIVJEUN", "R_LOGAUT", "R_LOGGRAT",
                   "R_LOGSEC", "R_PARSOC", "R_PATFINANC", "R_PATIMMO","PATIMM",
                   "R_PEATRE", "R_PEL", "R_PEP", "R_PERP",
                   "R_RESPRIN", "R_RETRAIT", "R_TERM", "R_VMOB",
                   "HERTABD", "HERTABH", "HERDON", "ZEAT", "CONSEP",
                   "R_ACTNCT", "R_SCPI", "R_SICAV", "DETIMM", "DETPRO",
                   "DIF", "CATAGE_RELATIF", "NIVEDUC", "CSPR8", "TYPMEN5",
                   "Heritage", "MLEE", "R_DETRE", "R_ACTION", "R_AVIEURO")



base_selection <- base_reduite[, c(
  "IDENT","POND", "R_LIVABL", "R_LDD", "R_LEP", "MMLA", "MMLDDS", "MMLEP",vars_retenues )]

dim(base_reduite)
dim(base_selection)

# Quelles sont les variables qui sont tombés à l"eau, et qu'est ce qui me reste 
var_eliminees=setdiff(names(base_reduite), names(base_selection))

var_eliminees

# Reintroduit : Heritage, MLEE, R_DETRE, R_ACTION, R_AVIEURO

names(base_selection)

# les variables retenues que je choisis d'ignorer par choix : R_SICAV, R_SCPI, HERDON, R_PARSOC, R_PRO
# R_LOGGRAT, R_DETTR, R_CCQ, R_CCA, R_AVIRENTE, PRET, MCASSOCIE, MBONCAISSE, NBCC, SEXEPR, ZEAT, R_ACTNCT

vars_a_supprimer <- c(
  "R_SICAV", "R_SCPI", "HERDON", "R_PARSOC",
  "R_LOGGRAT", "R_DETTR", "R_CCQ", "R_CCA",
  "R_AVIRENTE", "PRET", "MCASSOCIE", "MBONCAISSE",
   "SEXEPR", "ZEAT", "R_ACTNCT")

base_selection <- base_selection %>%
  select(-all_of(vars_a_supprimer))

dim(base_selection)

# Faire des groupes de variables 
names(base_selection)

# ================================================

# Description du ménage
# NENFANTS, NPERS, NACTIFS, TYPMEN5

# État civil / Situation familiale de la personne de référence
# MATRIPR, AGE_RELATIF, CATAGE_RELATIF

# Activité professionnelle personne de référence 
# SITUAPR, STATUTPR, CSPR8

# TCM - Ressources culturelles personne de référence
# NIVEDUC

# Revenus 
# ZREVDISP, decile_revenu, RFIN, RLOG, RTRA, RRMI

# Habitudes de consommation
# CNDUR, CONSEP, DIEXP, DIF, LOYER

# Detention 
## Assurance vie
## MAVI, R_AVI, R_AVIMULT, R_AVIEURO

## Épargne logement
## MCEL, MPEL, R_CEL, R_PEL

## Épargne retraite
## MEPRETRAITE, R_COMP, R_PERP, R_RETRAIT, R_PEP

## Épargne salariale
## MEPSALAR, MLEE, R_EPSALA

## Valeurs mobilières
## MVMOB, R_VMOB, R_ACTION, R_CAPI, R_PEATRE

## Comptes à terme
## MCOMPTERM, R_TERM

## Livrets 
## R_LIVDF, R_LIVF, R_LIVJEUN

## Agrégat financier
## R_PATFINANC, PATFISOM

# Actifs immobiliers 
## Résidence Principale 
## MRESPRIN, R_RESPRIN

## Autres logements
## NBLOG, R_LOGAUT, R_LOGSEC

## Agrégat immobilier
## PATRIRES, R_PATIMMO, PATIMM

# Actifs professionnels
# NE, PATRIPRO, DETPRO

# Endettement
# MTDETTES, DETIMM, R_DETDI, R_DETEQ, R_DETRES, R_DETRE, 

# Donations, héritages et aides
# Heritage, Donation_R, HERTABD, HERTABH

# Comportement financier
# RISKFI

# Événements de vie / Parcours
# RMODIF_CHO, RMODIF_MAL, RMODIF_RET

# 1. Préparation des données
library(FactoMineR)

data_afdm <- base_selection[, !names(base_selection) %in% c(
  "IDENT",
  "POND")]

# 2 : Realisation de AFMD
res_afdm <- FAMD(
  data_afdm,sup.var = c("R_LIVABL","R_LDD","R_LEP","MMLA","MMLDDS","MMLEP"),
  row.w = base_selection$POND,
  ncp = 60,
  graph = FALSE)

# 3. Analyse de la variance expliquée
res_afdm$eig

plot(res_afdm$eig[,1], type="b")

# Forte decroissance des valeurs propres jusqu'à la dimension 4-5 
# puis la courbe devient plate. Donc autour de 5 dims


# Contributions et cosinus carré 
round(res_afdm$var$contrib[,1:5],2) # Contributions

# Visulaisation 
library(ggplot2)

# Dimension 1 
contrib1 <- data.frame(
  variable = rownames(res_afdm$var$contrib),
  contrib = res_afdm$var$contrib[,1]
)

contrib1 <- contrib1[order(contrib1$contrib, decreasing = TRUE), ]
contrib1 <- head(contrib1, 15)

ggplot(contrib1,
       aes(x = reorder(variable, contrib),
           y = contrib)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Contributions à la Dimension 1",
    x = "",
    y = "Contribution (%)"
  ) +
  theme_minimal()

# Dimension 2
contrib2 <- data.frame(
  variable = rownames(res_afdm$var$contrib),
  contrib = res_afdm$var$contrib[,2])

contrib2 <- contrib2[order(contrib2$contrib, decreasing = TRUE), ]
contrib2 <- head(contrib2, 15)

ggplot(contrib2,
       aes(x = reorder(variable, contrib),
           y = contrib)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Contributions à la Dimension 2",
    x = "",
    y = "Contribution (%)"
  ) +
  theme_minimal()

# Dimension 3
contrib3 <- data.frame(
  variable = rownames(res_afdm$var$contrib),
  contrib = res_afdm$var$contrib[,3])

contrib3 <- contrib3[order(contrib3$contrib, decreasing = TRUE), ]
contrib3 <- head(contrib3, 15)

ggplot(contrib3,
       aes(x = reorder(variable, contrib),
           y = contrib)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Contributions à la Dimension 3",
    x = "",
    y = "Contribution (%)"
  ) +
  theme_minimal()

# Dimension 4
contrib4 <- data.frame(
  variable = rownames(res_afdm$var$contrib),
  contrib = res_afdm$var$contrib[,4])

contrib4 <- contrib4[order(contrib4$contrib, decreasing = TRUE), ]
contrib4 <- head(contrib4, 15)

ggplot(contrib4,
       aes(x = reorder(variable, contrib),
           y = contrib)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Contributions à la Dimension 4",
    x = "",
    y = "Contribution (%)"
  ) +
  theme_minimal()

# Dimension 5
contrib5 <- data.frame(
  variable = rownames(res_afdm$var$contrib),
  contrib = res_afdm$var$contrib[,5])

contrib5 <- contrib5[order(contrib5$contrib, decreasing = TRUE), ]
contrib5 <- head(contrib5, 15)

ggplot(contrib5,
       aes(x = reorder(variable, contrib),
           y = contrib)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Contributions à la Dimension 5",
    x = "",
    y = "Contribution (%)"
  ) +
  theme_minimal()

# Plan factoriel variables (Dim1-Dim2)
coord <- data.frame(
  variable = rownames(res_afdm$var$coord),
  Dim1 = res_afdm$var$coord[,1],
  Dim2 = res_afdm$var$coord[,2])

ggplot(coord,
       aes(x = Dim1,
           y = Dim2,
           label = variable)) +
  geom_hline(yintercept = 0,
             colour = "grey70") +
  geom_vline(xintercept = 0,
             colour = "grey70") +
  geom_point(colour = "red") +
  geom_text(size = 3,
            check_overlap = TRUE) +
  theme_minimal() +
  labs(
    title = "Variables sur le plan factoriel",
    x = "Dimension 1",
    y = "Dimension 2")

# Cosinus carré 
cos2 <- data.frame(
  variable = rownames(res_afdm$var$cos2),
  cos2 = res_afdm$var$cos2[,1])

cos2 <- cos2[order(cos2$cos2, decreasing = TRUE), ]

ggplot(head(cos2, 20),
       aes(x = reorder(variable, cos2),
           y = cos2)) +
  geom_col(fill = "orange") +
  coord_flip() +
  labs(
    title = "Qualité de représentation sur l'axe 1",
    x = "",
    y = expression(cos^2)
  ) +
  theme_minimal()

round(res_afdm$var$coord[,1:5],2) # Coordonnées
round(res_afdm$var$cos2[,1:5],2) # cosinus carré

# Recuperation des corrdonnées des dependantes 
round(res_afdm$var$coord[,1:5],2) 
round(res_afdm$quanti.sup$coord[,1:5],2)
round(res_afdm$quanti.sup$cor[,1:5],2)
round(res_afdm$quanti.sup$cos2[,1:5],2)
res_afdm$quali.sup$coord

#******************************************* Abirtrage ************************************************
#************************************ Se concentrer sur l'essentiel ***********************************
 
# Description du ménage
# NENFANTS, NPERS, NACTIFS

# État civil / Situation familiale de la personne de référence
# MATRIPR, AGE_RELATIF, CATAGE_RELATIF

# Activité professionnelle personne de référence 
# CSPR8

# TCM - Ressources culturelles personne de référence
# NIVEDUC

# Revenus 
# ZREVDISP, decile_revenu

# Habitudes de consommation
# CNDUR, CONSEP, DIEXP, DIF

# Detention 
## Assurance vie
## MAVI, R_AVI

## Épargne logement
## MCEL, MPEL, R_CEL, R_PEL

## Épargne retraite
## MEPRETRAITE, R_RETRAIT

## Épargne salariale
## MEPSALAR, MLEE, R_EPSALA

## Valeurs mobilières
## MVMOB, R_VMOB, R_ACTION

## Comptes à terme
## MCOMPTERM, R_TERM

## Livrets 
## R_LIVJEUN

## Agrégat financier
## R_PATFINANC, PATFISOM

# Actifs immobiliers 
## Résidence Principale 
## MRESPRIN, R_RESPRIN

## Autres logements
## NBLOG

## Agrégat immobilier
## PATRIRES, R_PATIMMO, PATIMM

# Actifs professionnels
# NE, PATRIPRO, DETPRO

# Endettement
# MTDETTES, DETIMM, R_DETRE

# Donations, héritages et aides
# Heritage, Donation_R, HERTABD, HERTABH

# Comportement financier
# RISKFI

View(base_selection)

#==================== base finale menage 
vars_modele <- c(
  # Variables d'identification et cibles
  "IDENT", "POND",
  "R_LIVABL", "R_LDD", "R_LEP",
  "MMLA", "MMLDDS", "MMLEP",
  
  # Description du ménage
  "NENFANTS", "NPERS", "NACTIFS",
  
  # État civil / Situation familiale
  "MATRIPR", "AGE_RELATIF", "CATAGE_RELATIF",
  
  # Activité professionnelle
  "CSPR8",
  
  # Niveau d'études
  "NIVEDUC",
  
  # Revenus
  "ZREVDISP", "decile_revenu",
  
  # Habitudes de consommation
  "CNDUR", "CONSEP", "DIEXP", "DIF",
  
  # Assurance vie
  "MAVI", "R_AVI",
  
  # Épargne logement
  "MCEL", "MPEL", "R_CEL", "R_PEL",
  
  # Épargne retraite
  "MEPRETRAITE", "R_RETRAIT",
  
  # Épargne salariale
  "MEPSALAR", "MLEE", "R_EPSALA",
  
  # Valeurs mobilières
  "MVMOB", "R_VMOB", "R_ACTION",
  
  # Comptes à terme
  "MCOMPTERM", "R_TERM",
  
  # Livrets
  "R_LIVJEUN",
  
  # Agrégat financier
  "R_PATFINANC", "PATFISOM",
  
  # Actifs immobiliers
  "MRESPRIN", "R_RESPRIN",
  "NBLOG",
  "PATRIRES", "R_PATIMMO", "PATIMM",
  
  # Actifs professionnels
  "NE", "PATRIPRO", "DETPRO",
  
  # Endettement
  "MTDETTES", "DETIMM", "R_DETRE",
  
  # Donations, héritages et aides
  "Heritage", "Donation_R", "HERTABD", "HERTABH",
  
  # Comportement financier
  "RISKFI")

Menage_2014 <- base_selection[, vars_modele]
View(Menage_2014)
dim(Menage_2014)


#===================
#=== Ressortir les variables qui sont rejetées et faire un arbitrage avec la théorie économique

#=== Faire l'analyse des données pour réduire davantage la taille de la base

#=== Rentrer les variables une a une à partir des modèles linéaires
#===================  

# # Determiner le coefficient de Correlation entre les dependnates et les explicatives
#======================== ACP, ACH etc..., KHI-DEUX

# ================= 11/08/2026 ==========================================

#=========================== Traitement de la table individu

# Selection des variables à partir du questionnaire
individu_db_2014 <- individu_2014 %>% 
  select(IDENT, IDENTINDL, IDENTIND14,INDIV_PANEL,ANSORTIE_PANEL, 
         EPSALA, HERTABD_I, HERTABH_I, JEAINE, PRODEP, ACT, ACTIF, 
         ACTION, ACTIVANTE, ACTN, ACTNCT, ACTNPROF, ACTOCCUP, ACTPEA, ACTTOT, AGE, AGEQ, AIDEREC_I, 
         ANAIS, ASDEC, ASDECN, ASDECV, ASSUR, AVI, AVIEURO, AVIMULT, AVIRENTE, BON, CAPI, CCQ, 
         CEL, CLASSIF, CLASSIFANTE, COMP, COUPLE, COUPLRP, CS24, CS42, CS_ANTE, CTIT, 
         CYDER, DCGR, DCPE, DCPR, DIPDET, DURCHO, DUREE, EDUC, 
         EDUCSTAT, ENFRP, EPALO, ETAMATRI, FORTER, GESTSEP_I, JAGEACT, JAGEQUIT, 
         JEACTIF, JEARGT, JEFRSO, JEGRAVE_AUT, JEGRAVE_DCAS, JEGRAVE_DCFR, JEGRAVE_DISP, 
         JEGRAVE_DIV, JEGRAVE_MALM, JEGRAVE_MALP, JEMNAIS, JEMPROF, 
         JEPNAIS, JEPPROF, JEPRO_IMMO, JEPRO_RP, JEPRO_TER, JEPRO_TRAV, JEQUIT, LDD, LEE, 
         LEP, LIVABL, LIVDF, LIVEP, LIVF, LIVJEUN, NAIS7, NATIO7, OBLI, PACS, PANIER, 
         PARSOC, PATFINANC, PEA, PEAACT_I, PEASIC_I, PEATRE_I, PEL, PEP, PEPASSUR, 
         PEPBANC, PERP, POA, RETRAIT, RETRAIT2, SALCCB, SALSIC, 
         SALSICPE, SALTIT, SALTITPE, SCPI, SEXE, SICAV, SICPEA, SITUA, 
         STATUT, STATUTANTE, SUR, TITULAIREFP, 
         TRAVAIL, TYPEMPLOI, VMOB, AUTNATIO7, 
         RANG_PANEL, PEPAVI, CCA, TERM, 
         ZSALAIRES_I, ZCHOMAGE_I, ZRETRAITES_STRICTES_I, 
         ZPENALIR_I, ZINVALIDITE_I, ZRENTES_I, ZRAG_I, ZRIC_I, ZRNC_I, ZRETRAITES_I)

dim(individu_db_2014)
View(individu_db_2014)

# Rselection, parcourir la base 
individu_db_2014$JEAINE = NULL # Ainé de la famille 
individu_db_2014$ACTIVANTE = NULL # Exercice d'une activté dans le passé 
individu_db_2014$ACTOCCUP = NULL # Indidvu actif occupé 
individu_db_2014$AGE = NULL # Age au jour de l'enquête 
individu_db_2014$AGEQ = NULL # Indidvu actif occupé 

vars_a_supprimer <- c(
  "ASDECN","ASDECV","ASSUR","CLASSIF","CLASSIFANTE","COUPLE",
  "CS42","CSANTE","CYACTDER","CYCAUSDER","CYDEBDER","CYDER",
  "CYTPTDER","EDUCSTAT","FORTER","JAGEACT","JAGEQUIT","JEACTIF",
  "JEFRSO","JEMNAIS","JEMPROF","JEPNAIS","JEPPROF","JEPRO_IMMO",
  "JEPRO_RP","JEPRO_TER","JEPRO_TRAV","JEQUIT","PACS","PANIER",
  "PERD","RETRAIT2","STATUTANTE","S_AVEIMMO","S_AVEPRO","S_AVERP",
  "S_COMMU","S_CONTRATM","S_CONTRATP","S_DIVO","S_FINMT","S_FINMTR",
  "S_IMEMP","S_IMEMPMT","S_IMMO","S_IMMOMT","S_IMMOMTR",
  "S_IMNOUVEMP","S_IMNOUVMT","S_NETAMV","S_PART","S_PENSALI",
  "S_PRESTCO","S_PRO","S_PROEMP","S_PROMT","S_PROMTR","S_RESTMT",
  "S_RESTMTR","S_RP","S_RPMT","S_RPMTR","S_RPVENT","S_SEPA",
  "TITULAIREFP","TROUV","AUTNATIO7","S_IMMOCED","S_IMMOCONS",
  "S_ACQIMMOEXCJ","S_ACQIMMOPANEL","S_PROVEND","S_PROCED",
  "S_PROCONS","S_ACQPROFEXCJ","S_ACQPROFAUTRE","S_ACQPROFPANEL",
  "S_RPVQI_AUT","S_RPVQI_PR","S_DDECO","S_DSEPA"
)
dim(individu_db_2014)

individu_db_2014 <- individu_db_2014 %>%
  select(-any_of(vars_a_supprimer))

dim(individu_db_2014)
dim(individu_2014)

#==================== Recodage de certaines variables 
#=============== Revenu 
individu_db_2014$ZSALAIRES_I # Salaire
individu_db_2014$ZCHOMAGE_I # CHOMAGE
individu_db_2014$ZRETRAITES_I # RETRAITE
individu_db_2014$ZRETRAITES_STRICTES_I # 
individu_db_2014$ZPENALIR_I # 
individu_db_2014$ZRIC_I # 
individu_db_2014$ZRNC_I
individu_db_2014$ZRAG_I 
individu_db_2014$ZRENTES_I

vars <- c("ZSALAIRES_I","ZCHOMAGE_I","ZRETRAITES_I","ZRETRAITES_STRICTES_I",
          "ZPENALIR_I","ZRIC_I","ZRNC_I","ZRAG_I","ZRENTES_I")

summary(individu_db_2014[, vars])

dim(individu_2014)

mean(is.na(individu_db_2014$ZSALAIRES_I)) * 100
mean(is.na(individu_db_2014$ZCHOMAGE_I)) * 100
mean(is.na(individu_db_2014$ZRETRAITES_I)) * 100
mean(is.na(individu_db_2014$ZRETRAITES_STRICTES_I)) * 100

# Reconstitution dunrevenu des individus 
individu_db_2014$ZREVDEC_I = ifelse(is.na(individu_db_2014$ZSALAIRES_I), 0, individu_db_2014$ZSALAIRES_I) +
  ifelse(is.na(individu_db_2014$ZCHOMAGE_I), 0, individu_db_2014$ZCHOMAGE_I) +
  ifelse(is.na(individu_db_2014$ZRETRAITES_I), 0, individu_db_2014$ZRETRAITES_I) +
  ifelse(is.na(individu_db_2014$ZPENALIR_I), 0, individu_db_2014$ZPENALIR_I) +
  ifelse(is.na(individu_db_2014$ZRIC_I), 0, individu_db_2014$ZRIC_I) +
  ifelse(is.na(individu_db_2014$ZRNC_I), 0, individu_db_2014$ZRNC_I) +
  ifelse(is.na(individu_db_2014$ZRAG_I), 0, individu_db_2014$ZRAG_I) +
  ifelse(is.na(individu_db_2014$ZRENTES_I), 0, individu_db_2014$ZRENTES_I) 

# Vérification (Aucune valeur négative)
VoirI=individu_db_2014 %>% 
  #  filter(ZSALAIRES_I <0 | ZCHOMAGE_I < 0 | ZRETRAITES_I < 0 | ZRETRAITES_STRICTES_I < 0 |
  #        ZPENALIR_I < 0 | ZRIC_I < 0 | ZRNC_I < 0 | ZRAG_I < 0 | ZRENTES_I < 0) %>% 
  filter(ZREVDEC_I < 0) %>% 
  select("ZREVDEC_I","ZSALAIRES_I","ZCHOMAGE_I","ZRETRAITES_I","ZRETRAITES_STRICTES_I",
         "ZPENALIR_I","ZRIC_I","ZRNC_I","ZRAG_I","ZRENTES_I")

View(VoirI)

summary(individu_db_2014$ZREVDEC_I)

# Me debarraser des variables intermédiares
individu_db_2014 <- individu_db_2014[, !(names(individu_db_2014) %in% c(
  "ZSALAIRES_I","ZCHOMAGE_I", "ZRETRAITES_I", "ZPENALIR_I", "ZRIC_I",
  "ZRNC_I", "ZRAG_I", "ZRENTES_I"))]

#============= AGE 
# Age relatif
individu_db_2014$AGE_RELATIF_I = 2021-individu_db_2014$ANAIS

# Age REEL 
individu_db_2014$AGE_REEL_I= 2026-individu_db_2014$ANAIS

# RECODAGE DE L'AGE
individu_db_2014$CATAGE_RELATIF_I=NA

individu_db_2014$CATAGE_RELATIF_I[individu_db_2014$AGE_RELATIF_I <= 14] = 1                              #0 à 14 ans
individu_db_2014$CATAGE_RELATIF_I[individu_db_2014$AGE_RELATIF_I > 14 & individu_db_2014$AGE_RELATIF_I <=29] = 2 #15 à 29 ans
individu_db_2014$CATAGE_RELATIF_I[individu_db_2014$AGE_RELATIF_I > 29 & individu_db_2014$AGE_RELATIF_I <=44] = 3 #30 à 44 ans
individu_db_2014$CATAGE_RELATIF_I[individu_db_2014$AGE_RELATIF_I > 44 & individu_db_2014$AGE_RELATIF_I <=59] = 4 #45 à 59 ans
individu_db_2014$CATAGE_RELATIF_I[individu_db_2014$AGE_RELATIF_I > 59 & individu_db_2014$AGE_RELATIF_I <=74] = 5 #60 à 74 ans
individu_db_2014$CATAGE_RELATIF_I[individu_db_2014$AGE_RELATIF_I > 74] = 6                               #75 ans ou plus 

# Suppression de l'année de naissance
individu_db_2014$ANAIS<-NULL

individu_db_2014$AGE_RELATIF_I
individu_db_2014$AGE_REEL_I
individu_db_2014$CATAGE_RELATIF_I

#============ NIVEAU D'EDUCATION 
individu_db_2014$NIVEDUC_I=NA

individu_db_2014$NIVEDUC_I[trimws(individu_db_2014$DIPDET) == ""] = 0       # Non déclaré
individu_db_2014$NIVEDUC_I[individu_db_2014$DIPDET %in% c("00",00,70,71)] = 1       # Sans diplome ou CEP
individu_db_2014$NIVEDUC_I[individu_db_2014$DIPDET %in% c(45,51,52,53,60)] = 2    # brevet
individu_db_2014$NIVEDUC_I[individu_db_2014$DIPDET %in% c(41,42,43,44)] = 3       # bac ou bac+1
individu_db_2014$NIVEDUC_I[individu_db_2014$DIPDET %in% c(31,32,33,34,35,36)] = 4 # bac+2
individu_db_2014$NIVEDUC_I[individu_db_2014$DIPDET %in% c(22,23,24)] = 5          # bac+3
individu_db_2014$NIVEDUC_I[individu_db_2014$DIPDET %in% c(21)] = 6                # bac+4 
individu_db_2014$NIVEDUC_I[individu_db_2014$DIPDET %in% c(13,14,15,16,17)] = 7    # bac+5
individu_db_2014$NIVEDUC_I[individu_db_2014$DIPDET %in% c(11,12)] = 8             # bac+8

mean(is.na(individu_db_2014$NIVEDUC_I)) * 100 

V=individu_db_2014 %>%
  filter(trimws(DIPDET) == "" | is.na(DIPDET)) %>%
  select(DIPDET, STATUT, SITUA)

View(V)

individu_db_2014 %>%
  filter(trimws(DIPDET) == "") %>%
  nrow()

individu_db_2014$DIPDET = NULL 

#=========== CSP 
# Catégorie socio professionnelle 
# Recoder la variable pour avoir les catégories en 8 niveaux plus facile à interpreter ensuite
individu_db_2014$CS8_I=NA

individu_db_2014$CS8_I[trimws(individu_db_2014$CS24) == ""] = 0 # Non déclaré
individu_db_2014$CS8_I[individu_db_2014$CS24 %in% c(10)] = 1 #Agriculteurs exploitants 
individu_db_2014$CS8_I[individu_db_2014$CS24 %in% c(21,22,23)] = 2 #Artisans, commerçants et chefs d'entreprise
individu_db_2014$CS8_I[individu_db_2014$CS24 %in% c(31,32,36)] = 3 #Cadres et professions intellectuelles supérieures
individu_db_2014$CS8_I[individu_db_2014$CS24 %in% c(41,46,47,48)] = 4 #Professions Intermédiaires
individu_db_2014$CS8_I[individu_db_2014$CS24 %in% c(51,54,55,56)] = 5 #Employés
individu_db_2014$CS8_I[individu_db_2014$CS24 %in% c(61,66,69)] = 6 #Ouvriers
individu_db_2014$CS8_I[individu_db_2014$CS24 %in% c(71,72,73,76)] = 7 #Retraités
individu_db_2014$CS8_I[individu_db_2014$CS24 %in% c("00",81,82)] = 8 #Autres personnes sans activité professionnelle

individu_db_2014$CS24 = NULL
individu_db_2014$CS42 = NULL 

unique(individu_db_2014$CS8_I)


#========= Decile revenu 
mean(is.na(individu_db_2014$ZREVDEC_I)) * 100 

decile_revenu_ind <- with(individu_db_2014,quantile(ZREVDEC_I,probs = (1:10)/10,type = 7,na.rm = TRUE))
decile_revenu_ind

decile_df_revenu_ind <- as.data.frame(decile_revenu_ind[4:9])
decile_df_revenu_ind

seuils <- c(-Inf, decile_df_revenu_ind$decile_revenu_ind[1:6], Inf)
seuils

individu_db_2014 <- individu_db_2014 %>%
  mutate(decile_revenu_ind = cut(ZREVDEC_I,breaks = seuils,labels = 1:7,include.lowest = TRUE,
                                 include.upper = FALSE))

individu_db_2014$decile_revenu_ind

#======================== Transmission : Heritage et retraite
# DONS : MTDONRC
transmission_2014$MTDONRC

# HERITAGE : J'attribue le montant de la donation ou de l'héritage au premier individu 
View(transmission_2014)

ind_transmission <-transmission_2014 %>% 
  filter(!is.na(IDENTPOS) & !(IDENTPOS == "")) %>% 
  group_by(IDENTPOS) %>% 
  summarise(Heritage_I=sum(MTHERC,na.rm = TRUE),
            Donation_R_I=sum(MTDONRC,na.rm=TRUE),
            Donation_V_I=sum(MTDONRC,na.rm=TRUE)) %>% 
  select(IDENTPOS,Heritage_I,Donation_R_I,Donation_V_I)

View(ind_transmission)

View(individu)

individu_db_2014$Heritage_I.x = NULL 
individu_db_2014$Heritage_I.y = NULL
individu_db_2014$Donation_R_I.x = NULL
individu_db_2014$Donation_V_I.x = NULL 
individu_db_2014$Donation_R_I.y = NULL 
individu_db_2014$Donation_V_I.y = NULL 

individu_db_2014 <- individu_db_2014 %>%
  left_join(ind_transmission, by = c("IDENTIND14" = "IDENTPOS"))

View(individu_db_2014)

individu_db_2014$Heritage_I

# Correction des NA  : 
individu_db_2014 <- individu_db_2014 %>%
  mutate(
    Heritage_I   = tidyr::replace_na(Heritage_I, 0),
    Donation_R_I = tidyr::replace_na(Donation_R_I, 0),
    Donation_V_I = tidyr::replace_na(Donation_V_I, 0))

# Vérification 
colSums(is.na(individu_db_2014[, c("Heritage_I", "Donation_R_I", "Donation_V_I")]))

#======================== Fini la création de variable==============================

# Traitement des NA
Taux_na <- round(colSums(is.na(individu_db_2014)) * 100, 2)
Taux_na

table_na <- data.frame(
  Variable = names(Taux_na),
  Taux_NA = Taux_na)

table_na <- table_na[order(-table_na$Taux_NA), ]
View(table_na)

individu_db_2014$DURCHO = NULL # Je choisis de laisser tomber la varible 
individu_db_2014$DUREE = NULL
individu_db_2014$RETRAGE = NULL # Estimation de l'age de depart à la retraite 
individu_db_2014$PRODEP = NULL  # Pas très intéressé par la variable 
individu_db_2014$ANSORTIE_PANEL = NULL # Pas besoin pour le moment, je vais en revenir

View(individu_db_2014)

# Correction des valeurs vides dans les variables 
unique(individu_db_2014$ACTIF)
individu_db_2014$ACTIF

V <- individu_db_2014 %>%
  group_by(ACTIF) %>%
  summarise(nb = n()) %>%
  ungroup()

View(V)

# Vérifier l'age des personnes pour amputer
W=individu_db_2014 %>% 
  filter(ACTIF=="") %>% 
  select(ACTIF,AGE_REEL_I,AGE_RELATIF_I,NIVEDUC_I, SITUA)

View(W)

# Age d'activité selon le BIT : 15 ou plus 
individu_db_2014 <- individu_db_2014 %>%
  mutate(
    ACTIF = case_when(
      ACTIF == "" & SITUA == 1 ~ "1", # Les personnes en emploi
      ACTIF == ""             ~ "2",
      TRUE                    ~ ACTIF))

individu_db_2014$CS_ANTE = NULL
individu_db_2014$GESTSEP_I = NULL
individu_db_2014$TRAVAIL = NULL
individu_db_2014$NOUVEAUNE = NULL
individu_db_2014$ZSALAIRES_I = NULL 
individu_db_2014$ZCHOMAGE_I = NULL 
individu_db_2014$ZRETRAITES_STRICTES_I = NULL 
individu_db_2014$ZPENALIR_I = NULL 
individu_db_2014$ZINVALIDITE_I = NULL 
individu_db_2014$ZRENTES_I = NULL 
individu_db_2014$ZRAG_I = NULL 
individu_db_2014$ZRIC_I = NULL
individu_db_2014$ZRNC_I = NULL 
individu_db_2014$ZRETRAITES_I = NULL 

# Les variables à corriger 
V=individu_db_2014 %>% 
  filter(SITUA=="") %>% 
  select(AGE_RELATIF_I, SITUA,STATUT)


View(V)

# Corriger la variable couple 
individu_db_2014 <- individu_db_2014 %>%
  mutate(COUPLRP = ifelse(COUPLRP == "", "2", COUPLRP))

individu_db_2014 <- individu_db_2014 %>%
  mutate(NAIS7 = ifelse(NAIS7 == "", "Pas precisé", NAIS7))

individu_db_2014 <- individu_db_2014 %>%
  mutate(NATIO7 = ifelse(NATIO7 == "", "Pas precisé", NATIO7))

individu_db_2014 = individu_db_2014 %>% 
  mutate(SITUA= ifelse(SITUA=="","moins de 15-16 ans",SITUA))

individu_db_2014 = individu_db_2014 %>% 
  mutate(STATUT= ifelse(STATUT=="","moins de 15-16 ans",SITUA))

# Eliminer les variables sur les problemes dans la jeunesse 
individu_db_2014 <- individu_db_2014 %>%
  dplyr::select(
    -AIDEREC_I,
    -JEARGT,
    -JEGRAVE_AUT,
    -JEGRAVE_DCAS,
    -JEGRAVE_DCFR,
    -JEGRAVE_DISP,
    -JEGRAVE_DIV,
    -JEGRAVE_MALM,
    -JEGRAVE_MALP)

dim(individu_db_2014)
unique(individu_db_2014$STATUT)

# Correction des valeurs aberrantes et vérifications des variables 
#============== Revenu declaré des individus
# Moyenne 
mean(individu_db_2014$ZREVDEC_I)

# Median
median(individu_db_2014$ZREVDEC_I)        

# Quantile
Q=quantile(individu_db_2014$ZREVDEC_I, probs = c(0.25, 0.5, 0.75))
Q

# Décile 
quantile(individu_db_2014$ZREVDEC_I,probs = (1:10)/10)

# Distribution 
ggplot(individu_db_2014, aes(x = ZREVDEC_I)) + geom_histogram(bins = 30)

ggplot(individu_db_2014, aes(x = log(ZREVDEC_I +1 ))) + geom_histogram(bins = 30) 

boxplot(individu_db_2014$ZREVDEC_I) # Avant

ggplot(individu_db_2014,aes(x = ZREVDEC_I)) + geom_density(fill = "steelblue", alpha = 0.4) +
  labs(title = "Densité du revenu declaré", x = "Revenu declaré",y = "Densité") +
  theme_minimal()

# Log pour voir
ggplot(individu_db_2014, aes(x = ZREVDEC_I)) +
  geom_density(fill = "steelblue", alpha = 0.4) +
  scale_x_log10() +
  labs(x = "Revenu declaré (échelle log)", y = "Densité")

#================= TEST DE NORMALITE SUR LA CORRIGE VIA QQPLOT====================================================================
# Variable
x <- individu_db_2014$ZREVDEC_I

# Suppression des valeurs manquantes
x <- x[!is.na(x)]

# Probabilités
p <- ppoints(length(x))

# Moyenne et écart-type
m <- mean(x)
s <- sd(x)

# Quantiles observés
q_obs <- quantile(x, probs = p)

# Quantiles théoriques de la normale N(m, s²)
q_theo <- qnorm(p, mean = m, sd = s)

# QQ-plot
plot(
  q_theo, q_obs,
  xlab = "Quantiles théoriques normaux",
  ylab = "Quantiles observés",
  main = "QQ-plot de ZREVDEC_I"
)

# Droite de référence
abline(0, 1, col = "red", lwd = 2)

# Le revenu n'est pas normal est lui même, passage au log pour stabiliser, reduire l'asymétrie de la distribution

# ========================== PASSAGE AU LOGARITHME ===================================================================================
ggplot(individu_db_2014,aes(x = log(ZREVDEC_I+1))) +  geom_density(fill = "steelblue", alpha = 0.4) +
  labs(title = "logarithme de la Densité du revenu declaré",x = "Revenu declaré",y = "Densité") + theme_minimal()

individu_db_2014$logZREVDEC_I = log(individu_db_2014$ZREVDEC_I+1)

ggplot(individu_db_2014,aes(x = logZREVDEC_I)) +  geom_density(fill = "steelblue", alpha = 0.4) +
  labs(title = "logarithme de la Densité du revenu declaré",x = "Revenu declaré",y = "Densité") + theme_minimal()

#== Selection des variables via le LASSO pour eliminer aussi les multicolinéarité et les variables qui expliquent le mieux les detentions

# ==========================================================
# Sélection de variables par LASSO
# Objectif : identifier les variables expliquant le mieux
# la détention des produits d'épargne
# ==========================================================

library(glmnet)

# Reproductibilité
set.seed(123)

# ----------------------------------------------------------
# Variables dépendantes binaires
# ----------------------------------------------------------

cibles_binaires <- c("LIVABL", "LDD", "LEP")

# ----------------------------------------------------------
# Variables à exclure systématiquement
# ----------------------------------------------------------
dim(individu_db_2014)

vars_exclues <- c(
  "IDENT",
  "IDENTINDL",
  "IDENTIND14",
  "INDIV_PANEL",
  "POND_LONG_IND_2017",
  "POND_LONG_IND_2014",
  "RANG_PANEL",
  "INDIV_REINTERRO",
  "logZREVDEC_I")

# ----------------------------------------------------------
# Stockage des résultats
# ----------------------------------------------------------

resultats <- list()

# ----------------------------------------------------------
# Boucle sur les variables cibles
# ----------------------------------------------------------

for (y_name in cibles_binaires) {
  
  cat("\n=========================================\n")
  cat("Traitement de :", y_name, "\n")
  cat("=========================================\n")
  
  # Exclure :
  # - la cible courante
  # - les autres variables cibles
  # - les identifiants et variables non explicatives
  
  vars_x <- setdiff(
    names(individu_db_2014),
    c(vars_exclues, cibles_binaires)
  )
  
  # Construction de la matrice explicative
  
  X <- model.matrix(
    ~ .,
    data = individu_db_2014[, vars_x, drop = FALSE]
  )[, -1]
  
  # Variable cible
  
  y <- individu_db_2014[[y_name]]
  
  # Suppression des lignes avec NA
  
  donnees_completes <- complete.cases(X, y)
  
  X_clean <- X[donnees_completes, ]
  y_clean <- y[donnees_completes]
  
  cat("Observations utilisées :", nrow(X_clean), "\n")
  cat("Variables explicatives :", ncol(X_clean), "\n")
  
  # Validation croisée LASSO
  
  cvfit <- cv.glmnet(
    x = X_clean,
    y = y_clean,
    family = "binomial",
    alpha = 1,
    nfolds = 10,
    trace.it = 1
  )
  
  # Coefficients retenus
  
  coefs <- coef(
    cvfit,
    s = "lambda.1se"
  )
  
  vars_sel <- rownames(coefs)[
    as.vector(coefs != 0)
  ]
  
  vars_sel <- setdiff(
    vars_sel,
    "(Intercept)"
  )
  
  # Stockage
  
  resultats[[y_name]] <- list(
    lambda_min = cvfit$lambda.min,
    lambda_1se = cvfit$lambda.1se,
    variables = vars_sel
  )
}

names(individu_db_2014)
# ----------------------------------------------------------
# Résultats détaillés
# ----------------------------------------------------------

for (v in names(resultats)) {
  
  cat("\n----------------------------------\n")
  cat("Variable dépendante :", v, "\n")
  cat("----------------------------------\n")
  
  cat(
    "Lambda min :",
    resultats[[v]]$lambda_min,
    "\n"
  )
  
  cat(
    "Lambda 1se :",
    resultats[[v]]$lambda_1se,
    "\n"
  )
  
  cat(
    "Nombre de variables retenues :",
    length(resultats[[v]]$variables),
    "\n\n"
  )
  
  print(resultats[[v]]$variables)
}

# ----------------------------------------------------------
# Variables communes aux 3 modèles
# ----------------------------------------------------------

variables_communes <- Reduce(
  intersect,
  lapply(
    resultats,
    function(x) x$variables
  )
)

cat("\nVariables communes aux 3 modèles :\n")
print(variables_communes)

# ----------------------------------------------------------
# Fréquence de sélection
# ----------------------------------------------------------
freq <- sort(
  table(
    unlist(
      lapply(
        resultats,
        function(x) x$variables))),
  decreasing = TRUE)

cat("\nFréquence de sélection des variables :\n")
print(freq)


vars_lasso <- c("EPSALA","HERTABD_I","HERTABH_I","ACT","ACTIF","ACTNCT",
                "ACTPEA","ACTTOT","ASDEC","AVI","AVIMULT","AVIRENTE","BON",
                "CCQ","CEL","COUPLRP","DCGR","DCPE","DCPR","EDUC","ENFRP",
                "EPALO","ETAMATRI","LIVDF","LIVF","LIVJEUN","NAIS7","NATIO7",
                "PARSOC","PEA","PEASIC_I","PEATRE_I","PEL","PEP","PEPAVI",
                "SALSICPE","SALTITPE","SCPI","SITUA","STATUT","SUR","TERM",
                "TYPEMPLOI","VMOB","AGE_RELATIF_I","AGE_REEL_I","CATAGE_RELATIF_I",
                "NIVEDUC_I","CS8_I","decile_revenu_ind","Heritage_I",
                "Donation_R_I","Donation_V_I","ACTION","LEE","SEXE","PATFINANC", "RETRAIT")

base_reduite_ind <- individu_db_2014[, c("IDENT",
                                    "IDENTINDL",
                                    "IDENTIND14",
                                    "ZREVDEC_I","LIVABL","LDD","LEP", vars_lasso)]



dim(base_reduite_ind)

vars_non_lasso <- setdiff(names(individu_db_2014), names(base_reduite_ind))

vars_non_lasso

# Les variables que je decide de réintegrer : PATFINANC, RETRAIT, NIVEDUC
View(base_reduite_ind)

# Choix et arbitrage
names(base_reduite_ind)

# =========================== Montant des prduits detenus par livret
#======================= Vérification
# Livret A, possible 
V <- produit_2014 %>% 
  filter(FINNA %in% c("02", "03")) %>% 
  distinct(NOP)

View(V)

# LDDS, possible 
V <- produit_2014 %>% 
  filter(FINNA %in% c("04")) %>% 
  distinct(NOP)

View(V)

# LEP, possible 
V <- produit_2014 %>% 
  filter(FINNA %in% c("05")) %>% 
  distinct(NOP)

View(V)

# RESIDENCE PRINCIPALE, detention jointe uniquement
V <- produit_2014 %>% 
  filter(LOGNA %in% c("01")) %>% 
  distinct(NOP)

View(V)

# Autres logement, detention jointe uniquement
V <- produit_2014 %>% 
  filter(LOGNA %in% c("02","03","04")) %>% 
  distinct(NOP)

View(V)

# Livret Jeune, possible
V <- produit_2014 %>% 
  filter(FINNA %in% c("06")) %>% 
  distinct(NOP)

View(V)

# Livret soumis à l'impot, pas possible / detention individuelle et collective
V <- produit_2014 %>% 
  filter(FINNA %in% c("07")) %>% 
  distinct(NOP)

View(V)

# PEL, possible
V <- produit_2014 %>% 
  filter(FINNA %in% c("08")) %>% 
  distinct(NOP)

View(V)

# CEL, possible 
V <- produit_2014 %>% 
  filter(FINNA %in% c("09")) %>% 
  distinct(NOP)

View(V)


# AVI, detention individuelle et jointe
V <- produit_2014 %>% 
  filter(FINNA %in% c("20")) %>% 
  distinct(NOP)

View(V)

# Epargne retraite, possible 
V <- produit_2014 %>% 
  filter(FINNA %in% c("16","18","19")) %>% 
  distinct(NOP)

View(V)

# PEP, possible 
V <- produit_2014 %>% 
  filter(FINNA %in% c("17")) %>% 
  distinct(NOP)

View(V)

# VMOB, pas possible, individuelle et collective 
V <- produit_2014 %>% 
  filter(FINNA %in% c("11","12","13","14","22","23","24","25","26","27","28","29","30")) %>% 
  distinct(NOP)

View(V)

# Epargne salariale, possiBLE 
V <- produit_2014 %>% 
  filter(FINNA %in% c("10")) %>% 
  distinct(NOP)

View(V)

# Caisse et bon, possible 
V <- produit_2014 %>% 
  filter(FINNA %in% c("31")) %>% 
  distinct(NOP)

View(V)

# Compte à terme, detention individuelle et collective 
V <- produit_2014 %>% 
  filter(FINNA %in% c("32")) %>% 
  distinct(NOP)

View(V)

# Compte à associé, detention individuelle et collective 
V <- produit_2014 %>% 
  filter(FINNA %in% c("33")) %>% 
  distinct(NOP)

View(V)

# Livret d'Epargne Entreprise, possible detention individuelle
V <- produit_2014 %>% 
  filter(FINNA %in% c("34")) %>% 
  distinct(NOP)

View(V)


# Determiner les montants des detentions individuelles
View(produit_2014)

produit_2014 = produit_2014 %>% 
  mutate(IDENTIND14=paste0(IDENT,NOP))

View(produit_2014)

# Calcul du montant des livrets par individus
Montant_produit_I = produit_2014 %>%
  group_by(IDENTIND14) %>% 
  summarise(MLA_I = round(sum(MONTCLA * (FINNA %in% c("02","03")), na.rm = TRUE), 0),        # LA ou LBLEU
            NBLA_I = round(sum(NBLA),0), # NBLA 
            MLDDS_I=round(sum(MONTCLA * (FINNA %in% c("04")),na.rm = TRUE), 0),              # LDDS
            NBLDDS_I = round(sum(NBLDD),0), # NBLDDS 
            MLEP_I=round(sum(MONTCLA * (FINNA %in% c("05")),na.rm = TRUE), 0),               # LEP
            NBLEP_I = round(sum(NBLEP),0), # NBLEP 
            MLIVJEUNE_I=round(sum(MONTCLA * (FINNA %in% c("06")),na.rm = TRUE), 0),          # Livret Jeune
            MPEL_I=round(sum(MONTCLA * (FINNA %in% c("08")),na.rm = TRUE), 0),               # PEL
            MCEL_I=round(sum(MONTCLA * (FINNA %in% c("09")),na.rm = TRUE), 0),               # CEL
            MEPRETRAITE_I=round(sum(MONTCLA*(FINNA %in% c("16","18","19")),na.rm = TRUE), 0),  # Épargne retraite (y c. PERP)
            MPEP_I=round(sum(MONTCLA*(FINNA %in% c("17")),na.rm = TRUE), 0),                   # PEP
            MEPSALAR_I=round(sum(MONTCLA*(FINNA %in% c("10")),na.rm = TRUE), 0),               # Épargne salariale
            MBONCAISSE_I=round(sum(MONTCLA*(FINNA %in% c("31")),na.rm = TRUE), 0),             # Bons d’épargne, bons de caisse ou bons du trésor
            MLEE_I=round(sum(MONTCLA * (FINNA %in% c("34")), na.rm = TRUE), 0))               # LEE


View(Montant_produit_I)


#****************************** Correction des nombres de livrets ***********************************************************
Montant_produit_I <- Montant_produit_I %>%
  mutate(
    NBLA_I  = if_else(MLA_I > 0 & NBLA_I < 1, 1, NBLA_I),
    NBLDDS_I = if_else(MLDDS_I > 0 & NBLDDS_I < 1, 1, NBLDDS_I),
    NBLEP_I = if_else(MLEP_I > 0 & NBLEP_I < 1, 1, NBLEP_I))

# Les plus de deux livrets 
Montant_produit_I <- Montant_produit_I %>%
  mutate(
    NBLA_I = ifelse(NBLA_I > 2, 2, NBLA_I),
    NBLDDS_I = ifelse(NBLDDS_I > 2, 2, NBLDDS_I),
    NBLEP_I = ifelse(NBLEP_I > 2, 2, NBLEP_I))

# Calcul du montant moyen par livret
Montant_produit_I <- Montant_produit_I %>%
  mutate(
    MMLA_I = ifelse(NBLA_I > 0, round(MLA_I/NBLA_I,0), 0),
    MMLDDS_I = ifelse(NBLDDS_I > 0, round(MLDDS_I/NBLDDS_I,0), 0),
    MMLEP_I = ifelse(NBLEP_I > 0,round(MLEP_I/NBLEP_I,0), 0))

View(Montant_produit_I)

Montant_produit_I$NBLA_I = NULL 
Montant_produit_I$MLA_I = NULL 
Montant_produit_I$NBLDDS_I = NULL 
Montant_produit_I$MLDDS_I = NULL
Montant_produit_I$NBLEP_I = NULL 
Montant_produit_I$MLEP_I = NULL 

# Ramener ces infos dans la table individu
base_reduite_ind = base_reduite_ind %>% 
  left_join(Montant_produit_I,by="IDENTIND14")

View(base_reduite_ind)

# Correction des NA 
names(Montant_produit_I)

vars <- c("MLIVJEUNE_I", "MPEL_I", "MCEL_I", "MEPRETRAITE_I", "MPEP_I",
          "MEPSALAR_I", "MBONCAISSE_I", "MLEE_I", "MMLA_I", "MMLDDS_I", "MMLEP_I")

base_reduite_ind[vars] <- lapply(
  base_reduite_ind[vars],
  function(x) {
    x[is.na(x)] <- 0
    x
  }
)

#==== Selection des variables par arbitrage 
names(base_reduite_ind)

# Variables identifiantes et cibles 
# IDENT, IDENTINDL, IDENTIND20, LIVABL, LDD, LEP, MMLA_I, MMLDDS_I, MMLEP_I

# État civil
# SEXE, NAIS7, AGE_RELATIF_I, AGE_REEL_I, CATAGE_RELATIF_I

# Situation familiale
# COUPLRP, ENFRP, ETAMATRI

# Ressources culturelles / Capital humain 
# NATIO7, NIVEDUC_I

# Activité professionnelle
# ACTIF, SITUA, STATUT, TYPEMPLOI, CS8_I 

# Donations, héritages et aides
# HERTABD_I, HERTABH_I, Heritage_I, Donation_R_I, Donation_V_I

# Patrimoine financier / Détention de produits financiers
## Actions et valeurs mobilières
## ACT, ACTION, ACTNCT, ACTPEA, ACTTOT, PARSOC, SCPI, VMOB

## Assurance-vie et assurance-décès
## ASDEC, AVI, AVIMULT, AVIRENTE, DCGR, DCPE, DCPR

## Livrets 
## MLIVJEUNE_I, MLEE_I, LEE, LIVDF, LIVF, LIVJEUN

## Produits d'épargne bancaire
## BON, MBONCAISSE_I, CCQ, TERM

## Épargne logement
## EPALO, PEL, CEL, MPEL_I, MCEL_I

## Épargne retraite
## RETRAIT, MEPRETRAITE_I, SUR, PEP, PEPAVI, MPEP_I

## Épargne salariale
## EPSALA, MEPSALAR_I, SALSICPE, SALTITPE

## PEA
## PEA, PEASIC_I, PEATRE_I

## Rente éducation
## EDUC

## Indicateur synthétique de patrimoine financier
## PATFINANC

# Revenu 
# ZREVDEC_I, decile_revenu_ind

unique(base_reduite_ind$ACTTOT)
##====================================== Choix des variables par choix =======================
# Variables identifiantes et cibles 
# IDENT, IDENTINDL, IDENTIND20, LIVABL, 
# LDD, LEP, MMLA_I, MMLDDS_I, MMLEP_I

# État civil
# AGE_RELATIF_I, CATAGE_RELATIF_I

# Situation familiale
# ETAMATRI

# Ressources culturelles / Capital humain 
# NIVEDUC_I

# Activité professionnelle
# ACTIF, SITUA, STATUT, TYPEMPLOI, CS8_I 

# Donations, héritages et aides
# HERTABD_I, HERTABH_I, Heritage_I, Donation_R_I, Donation_V_I

# Patrimoine financier / Détention de produits financiers
## Actions et valeurs mobilières
## ACTION, ACTTOT, VMOB

## Assurance-vie et assurance-décès
## ASDEC, AVI

## Livrets 
## MLIVJEUNE_I, LIVJEUN

## Produits d'épargne bancaire
## TERM

## Épargne logement
## PEL, CEL, MPEL_I, MCEL_I

## Épargne retraite
## RETRAIT, MEPRETRAITE_I

## Épargne salariale
## EPSALA, MEPSALAR_I, MLEE_I, 

##=== PEA
## PEA

## Indicateur synthétique de patrimoine financier
## PATFINANC

# Revenu 
# ZREVDEC_I, decile_revenu_ind

# ================ Base individu =====================================================
Individu_2014 <- base_reduite_ind %>%
  select(
    # Identifiants et cibles
    IDENT, IDENTINDL, IDENTIND14,
    LIVABL, LDD, LEP, MMLA_I, MMLDDS_I, MMLEP_I,
    
    # État civil
    AGE_RELATIF_I, CATAGE_RELATIF_I,
    
    # Situation familiale
    ETAMATRI,
    
    # Capital humain
    NIVEDUC_I,
    
    # Activité professionnelle
    ACTIF, SITUA, STATUT, TYPEMPLOI, CS8_I,
    
    # Donations, héritages et aides
    HERTABD_I, HERTABH_I, Heritage_I,
    Donation_R_I, Donation_V_I,
    
    # Patrimoine financier
    ACTION, ACTTOT, VMOB,
    ASDEC, AVI,
    LIVJEUN, MLIVJEUNE_I,
    TERM,
    PEL, CEL, MPEL_I, MCEL_I,
    RETRAIT, MEPRETRAITE_I,
    EPSALA, MEPSALAR_I, MLEE_I,
    PEA,
    PATFINANC,
    
    # Revenu
    ZREVDEC_I,
    decile_revenu_ind)

dim(Individu_2014)

# Les variables que j'ai laissé tombé dans menage et individu 
var_eliminees=setdiff(names(base_reduite), names(menage_2014))
var_eliminees

var_eliminees_ind=setdiff(names(individu_db_2014), names(Individu_2014))
var_eliminees_ind


#================== Fusion de la table MENAGE-INDIVIDU 2020
dim(Menage_2014) # Menage
dim(Individu_2014) # Individu 

# Verifier si pas de doublons de ligne dans menage 
V=Menage_2014 %>%
  count(IDENT) %>%
  filter(n > 1)

View(V)

# Combien d'individu par menage 
W=individu_2014 %>%
  count(IDENT) %>%
  arrange(desc(n))

View(W)

# Jointure ménage -indvidus 2020
BASE_2014 <- Menage_2014 %>%
  left_join(
    Individu_2014,
    by = "IDENT",
    relationship = "one-to-many")


dim(Menage_2014)
dim(Individu_2014)
dim(BASE_2014)

View(BASE_2014)

#=============== Reste à faire l'année 2017 et fusionner les variables qui partent definitivement dans le modele ======================
















