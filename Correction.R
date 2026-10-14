#================ Formule du revenu disponible ============================================================
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

menage_2017=read_sas("C:/Users/gessessinou/Downloads/BDD/2017_2018/menage.sas7bdat")
menage_2014=read_sas("C:/Users/gessessinou/Downloads/BDD/2014_2015/menage.sas7bdat")

individu_2017=read_sas("C:/Users/gessessinou/Downloads/BDD/2017_2018/individu.sas7bdat")
individu_2014=read_sas("C:/Users/gessessinou/Downloads/BDD/2014_2015/individu.sas7bdat")

produit_2017=read_sas("C:/Users/gessessinou/Downloads/BDD/2017_2018/produit.sas7bdat")
produit_2014=read_sas("C:/Users/gessessinou/Downloads/BDD/2014_2015/produit.sas7bdat")

View(menage_2017)

mean(is.na(menage_2017$ZREVDISP)) * 100
mean(is.na(menage_2014$ZREVDISP)) * 100

menage_tert$GPRKDU = ifelse(is.na(menage_tert$GPRKDU), 0, menage_tert$GPRKDU) 

menage_2017=menage_2017 %>% 
  mutate(ZREVDISP1 = ZREVDEC + ZPRODFIN + ZPSOCM + ZM_PPEM - ZIMPOM - ZTHAB - ZIMPVALM - ZCSGIM - ZCSGDM - ZCSGREVPAT)


Voir2 = menage_2017 %>% 
  select(ZREVDISP, ZREVDISP1)

View(Voir2)

menage_2017$ZSALAIRES
menage_2017$ZCHOMAGE

#======= ZREVDISP
# Formule du revenu du disponible
ZREVDISP = ZREVDEC + ZPRODFIN + ZPSOCM + ZM_PPEM - ZIMPOM - ZTHAB - ZIMPVALM - ZCSGIM - ZCSGDM - ZCSGREVPAT

#======== ZREVDEC
# ZREVDEC = sum (ZSALAIRES, ZCHOMAGE, ZRETRAITES, ZRENTES, ZPENALIR, ZRAG, ZRIC,
#                ZRNC, ZFONCIER, ZVAMM, ZVALM, ZRAC, ZETRANGER, – ZPENALIV)

# ZSALAIRES : Source fiscale, 3.35% de maqnuants 
mean(is.na(menage_2017$ZSALAIRES)) * 100

# ZCHOMAGE : Source fiscale, 3.35% de maqnuants
mean(is.na(menage_2017$ZCHOMAGE)) * 100

# ZRETRAITES : Source fiscale, 3.35% de maqnuants
mean(is.na(menage_2017$ZRETRAITES)) * 100

# ZRENTES : Source fiscale, 3.35% de maqnuants
mean(is.na(menage_2017$ZRENTES)) * 100

# ZPENALIR : Source fiscale, 3.35% de maqnuants
mean(is.na(menage_2017$ZPENALIR)) * 100

# ZRAG : Source fiscale, 3.35% de maqnuants
mean(is.na(menage_2017$ZRAG)) * 100

# ZRIC : Source fiscale, 3.35% de maqnuants
mean(is.na(menage_2017$ZRIC)) * 100

# ZRNC : Source fiscale, 3.35% de maqnuants
mean(is.na(menage_2017$ZRNC)) * 100

# ZFONCIER : Source fiscale, 72.63% de maqnuants
mean(is.na(menage_2017$ZFONCIER)) * 100
menage_2017$ZFONCIER 

# ZVAMM : Source fiscale, 42.88% de maqnuants
mean(is.na(menage_2017$ZVAMM)) * 100
menage_2017$ZVAMM 

# ZVALM : Source fiscale, 94.1% de maqnuants
mean(is.na(menage_2017$ZVALM)) * 100
menage_2017$ZVALM 

# ZRAC : Source fiscale, 95.64% de maqnuants
mean(is.na(menage_2017$ZRAC)) * 100
menage_2017$ZRAC 

# ZETRANGER : Source fiscale, 98.83% de maqnuants
mean(is.na(menage_2017$ZETRANGER)) * 100
menage_2017$ZETRANGER 

# ZPENALIV : Source fiscale, 91.66% de maqnuants
mean(is.na(menage_2017$ZPENALIV)) * 100
menage_2017$ZPENALIV

#========= ZPRODFIN
mean(is.na(menage_2017$ZPRODFIN)) * 100
menage_2017$ZPRODFIN

#========= ZPRODFIN
mean(is.na(menage_2017$ZPRODFIN)) * 100
menage_2017$ZPRODFIN

#========= ZPSOCM, 0.59% manquants : ZPSOCM = ZPFAMILLE + ZPLOGEMENT + ZPSOCIAL
mean(is.na(menage_2017$ZPSOCM)) * 100
menage_2017$ZPSOCM

# ZPFAMILLE : Source fiscale, PAS DISPONIBLE 
mean(is.na(menage_2017$ZPFAMILLE)) * 100
menage_2017$ZPFAMILLE

# ZPLOGEMENT : Source fiscale, PAS DISPONIBLE 
mean(is.na(menage_2017$ZPLOGEMENT)) * 100
menage_2017$ZPLOGEMENT

# ZPSOCIAL : Source fiscale, PAS DISPONIBLE 
mean(is.na(menage_2017$ZPSOCIAL)) * 100
menage_2017$ZPSOCIAL


#==== Calcul et comparaison avec la valeur trouvé avec la formule
menage_2017=menage_2017 %>% 
  mutate(ZREVDISP1 = ZREVDEC + ZPRODFIN + ZPSOCM - ZIMPOM - ZTHAB - ZIMPVALM - ZCSGIM - ZCSGDM - ZCSGREVPAT)

Voir2 = menage_2017 %>% 
  select(ZREVDISP, ZREVDISP1)

mean(is.na(menage_2017$ZREVDISP1))
mean(is.na(menage_2017$ZREVDISP))

View(Voir2)

menage_2014$ZM_PPEM

menage_2017$ZREVDISP < menage_2017$ZREVDISP1

Vue = menage_2017 %>% 
#  filter(is.na(ZREVDISP)) %>% 
  select(ZREVDISP, ZREVDISP1, ZREVDEC, ZPRODFIN, ZPSOCM, ZIMPOM, ZTHAB, ZIMPVALM, ZCSGIM, ZCSGDM, ZCSGREVPAT)

View(Vue)

menage_2017 %>% 
  filter(is.na(ZREVDISP) & !is.na(ZREVDISP1)) %>% 
  summarise(n())

325/1111


# Voir si imputation (repecher certaines variables pour corriger certains 0)
menage_db$ZREVDISP=ifelse(is.na(menage_db$ZREVDISP), 
                          ifelse(is.na(menage_db$ZREVDEC), 0, menage_db$ZREVDEC) +
                            ifelse(is.na(menage_db$ZPRODFIN), 0, menage_db$ZPRODFIN) +
                            ifelse(is.na(menage_db$ZPSOCM), 0, menage_db$ZPSOCM) +
                            ## ifelse(is.na(menage_db$ZM_PPEM), 0, menage_db$ZM_PPEM) -
                            ifelse(is.na(menage_db$ZIMPOM), 0, menage_db$ZIMPOM) -
                            ifelse(is.na(menage_db$ZTHAB), 0, menage_db$ZTHAB) -
                            ifelse(is.na(menage_db$ZIMPVALM), 0, menage_db$ZIMPVALM) -
                            ifelse(is.na(menage_db$ZCSGIM), 0, menage_db$ZCSGIM) -
                            ifelse(is.na(menage_db$ZCSGDM), 0, menage_db$ZCSGDM) -
                            ifelse(is.na(menage_db$ZCSGREVPAT), 0, menage_db$ZCSGREVPAT)
                          , menage_db$ZREVDISP)

individu$ZSALAIRES_I

# Distribution 
ggplot(individu, aes(x = log(ZSALAIRES_I))) + geom_histogram(bins = 30)

ggplot(individu, aes(x = log(ZSALAIRES_I))) + geom_histogram(bins = 30)

ggplot(menage, aes(x = log(ZREVDISP))) + geom_histogram(bins = 30)

# Moyenne 
mean(individu$ZSALAIRES_I) 

ggplot(menage_tert, aes(x = log(ZREVDISP), weight = POND_TRANS)) + geom_histogram(bins = 30) # Ignore les valeurs négatives

boxplot(menage_tert$ZREVDISP) # Avant








#================================== Correction de la base de donnée 2017===================================
# Taux de manquants 
mean(is.na(menage_2017$ZREVDISP)) # Taux de NA à la base 

menage_2017 <- menage_2017 %>%
  mutate(
    revenus_activite = rowSums(
      cbind(
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


Voir2 = menage_2017 %>% 
  select(ZREVDISP, ZREVDISP1)

View(Voir2)

mean(is.na(menage_2017$ZREVDISP1))
mean(is.na(menage_2017$ZREVDISP))

# Remplacer le revenu par ZREVDISP1 si ZREVDISP est NA et ZREVDISP1 > 0 et non NA 
menage_2017$ZREVDISP=
  ifelse((is.na(menage_2017$ZREVDISP) & !is.na(menage_2017$ZREVDISP1) & menage_2017$ZREVDISP1>0),
                            menage_2017$ZREVDISP1, menage_2017$ZREVDISP)

mean(is.na(menage_2017$ZREVDISP)) * 100  # J'ai recupéré assez

# Reconstitution du revenu dispo de la table individu
# Equivalent du revenu disponible des menages
# ZREVDISP1 = ZREVDEC + ZPRODFIN + ZPSOCM - ZIMPOM - ZTHAB - ZIMPVALM - ZCSGIM - ZCSGDM - ZCSGREVPAT

# ZREVDEC = SUM (ZSALAIRES, ZCHOMAGE, ZRETRAITES, ZRENTES, ZPENALIR, ZRAG, ZRIC,
#               ZRNC, ZFONCIER, ZVAMM, ZVALM, ZRAC, ZETRANGER, – ZPENALIV)

individu_2017$ZSALAIRES_I # Salaire
individu_2017$ZCHOMAGE_I # CHOMAGE
individu_2017$ZRETRAITES_I # RETRAITE
individu_2017$ZRETRAITES_STRICTES_I # 
individu_2017$ZPENALIR_I # 
individu_2017$ZRIC_I # 
individu_2017$ZRNC_I
individu_2017$ZRAG_I 
individu_2017$ZRENTES_I

vars <- c("ZSALAIRES_I","ZCHOMAGE_I","ZRETRAITES_I","ZRETRAITES_STRICTES_I",
  "ZPENALIR_I","ZRIC_I","ZRNC_I","ZRAG_I","ZRENTES_I")

summary(individu_2017[, vars])

dim(individu_2017)

mean(is.na(individu_2017$ZSALAIRES_I)) * 100
mean(is.na(individu_2017$ZCHOMAGE_I)) * 100
mean(is.na(individu_2017$ZRETRAITES_I)) * 100
mean(is.na(individu_2017$ZRETRAITES_STRICTES_I)) * 100

individu_2017$ZREVDEC_I = ifelse(is.na(individu_2017$ZSALAIRES_I), 0, individu_2017$ZSALAIRES_I) +
                            ifelse(is.na(individu_2017$ZCHOMAGE_I), 0, individu_2017$ZCHOMAGE_I) +
                            ifelse(is.na(individu_2017$ZRETRAITES_I), 0, individu_2017$ZRETRAITES_I) +
                            ifelse(is.na(individu_2017$ZPENALIR_I), 0, individu_2017$ZPENALIR_I) +
                            ifelse(is.na(individu_2017$ZRIC_I), 0, individu_2017$ZRIC_I) +
                            ifelse(is.na(individu_2017$ZRNC_I), 0, individu_2017$ZRNC_I) +
                            ifelse(is.na(individu_2017$ZRAG_I), 0, individu_2017$ZRAG_I) +
                            ifelse(is.na(individu_2017$ZRENTES_I), 0, individu_2017$ZRENTES_I) 

# Vérification 
VoirI=individu_2017 %>% 
  #  filter(ZSALAIRES_I <0 | ZCHOMAGE_I < 0 | ZRETRAITES_I < 0 | ZRETRAITES_STRICTES_I < 0 |
  #        ZPENALIR_I < 0 | ZRIC_I < 0 | ZRNC_I < 0 | ZRAG_I < 0 | ZRENTES_I < 0) %>% 
  filter(ZREVDEC_I < 0) %>% 
  select("ZREVDEC_I","ZSALAIRES_I","ZCHOMAGE_I","ZRETRAITES_I","ZRETRAITES_STRICTES_I",
         "ZPENALIR_I","ZRIC_I","ZRNC_I","ZRAG_I","ZRENTES_I")

View(VoirI)

# Je vais degager les observations ayant un revenu négatif ZREVDEC_I
mean(is.na(individu_2017$ZREVDEC_I))

individu_2017 <- individu_2017 %>%
  filter(ZREVDEC_I >=0)

# Calcul du revenu des ménages à partir des revenus individuels
View(individu_2017)

REV_MENAGE=individu_2017 %>% 
  group_by(IDENT) %>% 
  summarise(ZREVDEC1=sum(ZREVDEC_I)) %>% 
  select(IDENT,ZREVDEC1)

View(REV_MENAGE)

comparaison_rev <- REV_MENAGE %>%
  left_join(menage_2017 %>% select(IDENT, ZREVDISP, ZREVDISP1),
    by = "IDENT") %>%
    select(IDENT, ZREVDISP, ZREVDISP1, ZREVDEC1)

View(comparaison_rev) 

NA_restant = comparaison_rev %>% 
  filter(is.na(ZREVDISP))

View(NA_restant)

# Dernière correction
# - Si ZREVDISP est < 0 et ZREVDEC1 > 0 alors conserver ZREVDEC1 à la place de ZREVDISP
# - Si ZREVDISP est NA et ZREVDEC1 > 0 alors remplacer par ZREVDEC1

menage_2017 <- menage_2017 %>%
  left_join(REV_MENAGE %>% select(IDENT, ZREVDEC1), by = "IDENT") 

menage_2017$ZREVDEC1

menage_2017 = menage_2017 %>%
  mutate(ZREVDISP = case_when(is.na(ZREVDISP) & ZREVDEC1 > 0 ~ ZREVDEC1,
                              ZREVDISP < 0 & ZREVDEC1 > 0 ~ ZREVDEC1,TRUE ~ ZREVDISP))

mean(is.na(menage_2017$ZREVDISP) | menage_2017$ZREVDISP < 0 ) * 100

# Recupérer les valeurs negatives 
Voir2 = menage_2017 %>% 
  filter(ZREVDISP < 0) %>% 
  select(ZREVDISP,ZREVDISP1, ZREVDEC1)
  
View(Voir2)

# Je vais shooter les ZREVDISP négatif, et NA
sum(is.na(menage_2017$ZREVDISP) | menage_2017$ZREVDISP < 0) # Nombre :243

mean(is.na(menage_2017$ZREVDISP) | menage_2017$ZREVDISP < 0) * 100 # Percent : 1.77

mean(is.na(menage_2017$ZREVDISP)) * 100 # Nombre 

# Shoot
menage_2017 = menage_2017 %>% 
  filter(!is.na(menage_2017$ZREVDISP) & menage_2017$ZREVDISP >= 0)

mean(is.na(menage_2017$ZREVDISP)) * 100 



#================================== Correction des montants des livrets
produit_t = as_tibble(produit_2017)

Montant_produit = produit_t %>%
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
            MEPRETRAITE=round(sum(MONTCLA*(FINNA %in% c("16","18","19")),na.rm = TRUE), 0),  # Épargne retraite (y c. PERP)
            MPEP=round(sum(MONTCLA*(FINNA %in% c("17")),na.rm = TRUE), 0),                   # PEP
            MVMOB=round(sum(MONTCLA*
                              (FINNA %in% c("11","12","13","14","22","23","24","25","26","27","28","29","30")),
                            na.rm = TRUE), 0),                                                                          # Valeurs mobilières
            MEPSALAR=round(sum(MONTCLA*(FINNA %in% c("10")),na.rm = TRUE), 0),               # Épargne salariale
            MBONCAISSE=round(sum(MONTCLA*(FINNA %in% c("31")),na.rm = TRUE), 0),             # Bons d’épargne, bons de caisse ou bons du trésor
            MCOMPTERM=round(sum(MONTCLA * (FINNA %in% c("32")), na.rm = TRUE), 0),          # Compte à terme
            MCASSOCIE=round(sum(MONTCLA * (FINNA %in% c("33")), na.rm = TRUE), 0),           # Compte courant d’associé
            MLEE=round(sum(MONTCLA * (FINNA %in% c("34")), na.rm = TRUE), 0)               # LEE
  ) 

View(Montant_produit)
dim(Montant_produit)

# Ramener dans la table en construction 
menage_2017=menage_2017 %>% 
  left_join(Montant_produit,by="IDENT")

individu_2017$NBLA=ifelse(individu_2017$LIVABL==1,1,0) # LA
individu_2017$NBLA

individu_2017$NBLDD=ifelse(individu_2017$LDD==1,1,0) # LDD
individu_2017$NBLDD

individu_2017$NBLEP=ifelse(individu_2017$LEP==1,1,0) # LEP
individu_2017$NBLEP

# Nombre de livret par menage
Nb_livret = individu_2017 %>% 
  group_by(IDENT) %>% 
  summarise(NBLA=sum(NBLA),NBLDD=sum(NBLDD),NBLEP=sum(NBLEP)) 

View(Nb_livret)

# Ramener dans la table menage_tert1
menage_2017 <- menage_2017 %>% 
  left_join(Nb_livret, by = "IDENT") 

# Vérifier le montant des livrets et le nombre avant le calcul du nombre 
Vue = menage_2017 %>% 
  filter(MLA>0 & NBLA < 1 ) %>% 
  select(MLA,NBLA)

View(Vue)

Vue1 = menage_2017 %>% 
  filter(MLDDS>0 & NBLDD < 1 ) %>% 
  select(MLDDS,NBLDD)

View(Vue1)

Vue2 = menage_2017 %>% 
  filter(MLEP>0 & NBLEP < 1 ) %>% 
  select(MLEP,NBLEP)

View(Vue2)

# Correction des nombres de livrets
menage_2017 <- menage_2017 %>%
  mutate(
    NBLA  = if_else(MLA > 0 & NBLA < 1, 1, NBLA),
    NBLDD = if_else(MLDDS > 0 & NBLDD < 1, 1, NBLDD),
    NBLEP = if_else(MLEP > 0 & NBLEP < 1, 1, NBLEP))

menage_2017$MMLA = round(ifelse(menage_2017$NBLA>0,menage_2017$MLA / menage_2017$NBLA,0),0) #LA
menage_2017$MMLDD = round(ifelse(menage_2017$NBLDD>0,menage_2017$MLDDS / menage_2017$NBLDD,0),0) # LDD
menage_2017$MMLEP = round(ifelse(menage_2017$NBLEP>0,menage_2017$MLEP / menage_2017$NBLEP,0),0) #LEP

Vue3 = menage_2017 %>% 
  filter(MMLA >22950 | MMLDD >12000 | MMLEP > 10000) %>% 
  select(MMLA,MMLDD,MMLEP)

View(Vue3) 

1914/nrow(menage_2017) *100

# 2020-2021

# Plus de 28000 sur MMLA : 115
# plus de 15000 sur LDDS : 203
# Plus de 13000 sur MMLEP : 34

# 2017-2018

# Plus de 28000 sur MMLA : 247
# plus de 15000 sur LDDS : 365
# Plus de 13000 sur MMLEP : 96

# De façon unique, 660 menages soit 2.01% de la base de donnée menage 2017

# ========================
produit_t = as_tibble(produit_2014)

Montant_produit = produit_t %>%
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
            MEPRETRAITE=round(sum(MONTCLA*(FINNA %in% c("16","18","19")),na.rm = TRUE), 0),  # Épargne retraite (y c. PERP)
            MPEP=round(sum(MONTCLA*(FINNA %in% c("17")),na.rm = TRUE), 0),                   # PEP
            MVMOB=round(sum(MONTCLA*
                              (FINNA %in% c("11","12","13","14","22","23","24","25","26","27","28","29","30")),
                            na.rm = TRUE), 0),                                                                          # Valeurs mobilières
            MEPSALAR=round(sum(MONTCLA*(FINNA %in% c("10")),na.rm = TRUE), 0),               # Épargne salariale
            MBONCAISSE=round(sum(MONTCLA*(FINNA %in% c("31")),na.rm = TRUE), 0),             # Bons d’épargne, bons de caisse ou bons du trésor
            MCOMPTERM=round(sum(MONTCLA * (FINNA %in% c("32")), na.rm = TRUE), 0),          # Compte à terme
            MCASSOCIE=round(sum(MONTCLA * (FINNA %in% c("33")), na.rm = TRUE), 0),           # Compte courant d’associé
            MLEE=round(sum(MONTCLA * (FINNA %in% c("34")), na.rm = TRUE), 0))               # LEE
 

View(Montant_produit)
dim(Montant_produit)

# Ramener dans la table en construction 
menage_2014=menage_2014 %>% 
  left_join(Montant_produit,by="IDENT")

individu_2014$NBLA=ifelse(individu_2014$LIVABL==1,1,0) # LA
individu_2014$NBLA

individu_2014$NBLDD=ifelse(individu_2014$LDD==1,1,0) # LDD
individu_2014$NBLDD

individu_2014$NBLEP=ifelse(individu_2014$LEP==1,1,0) # LEP
individu_2014$NBLEP

# Nombre de livret par menage
Nb_livret = individu_2014 %>% 
  group_by(IDENT) %>% 
  summarise(NBLA=sum(NBLA),NBLDD=sum(NBLDD),NBLEP=sum(NBLEP)) 

View(Nb_livret)

# Ramener dans la table menage_tert1
menage_2014 <- menage_2014 %>% 
  left_join(Nb_livret, by = "IDENT") 

# Vérifier le montant des livrets et le nombre avant le calcul du nombre 
Vue = menage_2014 %>% 
  filter(MLA>0 & NBLA < 1 ) %>% 
  select(MLA,NBLA)

View(Vue)

Vue1 = menage_2014 %>% 
  filter(MLDDS>0 & NBLDD < 1 ) %>% 
  select(MLDDS,NBLDD)

View(Vue1)

Vue2 = menage_2014 %>% 
  filter(MLEP>0 & NBLEP < 1 ) %>% 
  select(MLEP,NBLEP)

View(Vue2)

# Correction des nombres de livrets
menage_2014 <- menage_2014 %>%
  mutate(
    NBLA  = if_else(MLA > 0 & NBLA < 1, 1, NBLA),
    NBLDD = if_else(MLDDS > 0 & NBLDD < 1, 1, NBLDD),
    NBLEP = if_else(MLEP > 0 & NBLEP < 1, 1, NBLEP))

menage_2014$MMLA = round(ifelse(menage_2014$NBLA>0,menage_2014$MLA / menage_2014$NBLA,0),0) #LA
menage_2014$MMLDD = round(ifelse(menage_2014$NBLDD>0,menage_2014$MLDDS / menage_2014$NBLDD,0),0) # LDD
menage_2014$MMLEP = round(ifelse(menage_2014$NBLEP>0,menage_2014$MLEP / menage_2014$NBLEP,0),0) #LEP

Vue3 = menage_2014 %>% 
  filter(MMLA >22950) %>% 
  select(MMLA,MMLDD,MMLEP)

View(Vue3) 

865/nrow(menage_2014) *100

# 2014-2015

# Plus de 28000 sur MMLA : 116
# plus de 15000 sur LDDS : 174
# Plus de 13000 sur MMLEP : 46

# De façon unique, 308 menages soit 2.6% de la base de donnée menage 2014
View(produit)
View(individu)

#===================  Autres corrections à faire 
# Justifier pourquoi on a choisit de diviser : 
# On comparant la stat du menage et de l'individu, il ya  tel nb de personnes declarent des
# encours correspondant à celui du menage

# Vérifier si les montants 

nrow(produit_2017)

View(produit_2017)

# Les problèmes des produits : 
# Croisé avec les revenus 
# Eliminer 
# 28000
# 15000
# 13000
# Corriger au niveau de la base produit (Cohérent ou pas)

#============= Correction de la table produit
Vue = produit %>%
  filter(FINNA %in% c("02","03") & MONTCLA > 28000) %>% 
  select(IDENT,MONTCLA, NOP)

View(Vue)

# Coiser avecle revenu des menages
Revenu_menage = menage_tert_clean %>% 
  select(IDENT, ZREVDISP) 

View(Revenu_menage)

LA_REV=Vue %>% 
  left_join(Revenu_menage,by="IDENT")

View(LA_REV)

produit$NOP

# Pas normal, je vais mettre le montant de ces produits au plafond

# =========================== Montant des prduits detenus par livret
# Livret A, possible 
V <- produit %>% 
  filter(FINNA %in% c("02", "03")) %>% 
  distinct(NOP)

View(V)

# LDDS, possible 
V <- produit %>% 
  filter(FINNA %in% c("04")) %>% 
  distinct(NOP)

View(V)

# LEP, possible 
V <- produit %>% 
  filter(FINNA %in% c("05")) %>% 
  distinct(NOP)

View(V)

# RESIDENCE PRINCIPALE, detention jointe uniquement
V <- produit %>% 
  filter(LOGNA %in% c("01")) %>% 
  distinct(NOP)

View(V)

# Autres logement, detention jointe uniquement
V <- produit %>% 
  filter(LOGNA %in% c("02","03","04")) %>% 
  distinct(NOP)

View(V)

# Livret Jeune, possible
V <- produit %>% 
  filter(FINNA %in% c("06")) %>% 
  distinct(NOP)

View(V)

# Livret soumis à l'impot, pas possible / detention individuelle et collective
V <- produit %>% 
  filter(FINNA %in% c("07")) %>% 
  distinct(NOP)

View(V)

# PEL, possible
V <- produit %>% 
  filter(FINNA %in% c("08")) %>% 
  distinct(NOP)

View(V)

# CEL, possible 
V <- produit %>% 
  filter(FINNA %in% c("09")) %>% 
  distinct(NOP)

View(V)


# AVI, detention individuelle et jointe
V <- produit %>% 
  filter(FINNA %in% c("20")) %>% 
  distinct(NOP)

View(V)

# Epargne retraite, possible 
V <- produit %>% 
  filter(FINNA %in% c("16","18","19")) %>% 
  distinct(NOP)

View(V)

# PEP, possible 
V <- produit %>% 
  filter(FINNA %in% c("17")) %>% 
  distinct(NOP)

View(V)

# VMOB, pas possible, individuelle et collective 
V <- produit %>% 
  filter(FINNA %in% c("11","12","13","14","22","23","24","25","26","27","28","29","30")) %>% 
  distinct(NOP)

View(V)

# Epargne salariale, possiBLE 
V <- produit %>% 
  filter(FINNA %in% c("10")) %>% 
  distinct(NOP)

View(V)

# Caisse et bon, possible 
V <- produit %>% 
  filter(FINNA %in% c("31")) %>% 
  distinct(NOP)

View(V)

# Compte à terme, detention individuelle et collective 
V <- produit %>% 
  filter(FINNA %in% c("32")) %>% 
  distinct(NOP)

View(V)

# Compte à associé, detention individuelle et collective 
V <- produit %>% 
  filter(FINNA %in% c("33")) %>% 
  distinct(NOP)

View(V)

# Livret d'Epargne Entreprise, possible detention individuelle
V <- produit %>% 
  filter(FINNA %in% c("34")) %>% 
  distinct(NOP)

View(V)


# Determiner les montants des detentions individuelles
View(produit_t)

produit_t = produit_t %>% 
  mutate(IDENTIND20=paste0(IDENT,NOP))

View(produit_t)

# Calcul du montant des livrets par individus
Montant_produit_I = produit_t %>%
  group_by(IDENTIND20) %>% 
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

#=============
View()

# Correction des nombres de livrets 
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

View(base_reduite_ind)

base_reduite_ind = base_reduite_ind

# Fusion avec la Table individu
base_reduite_ind = base_reduite_ind %>% 
  left_join(Montant_produit_I,by="IDENTIND20")

View(base_reduite_ind)

# Les produits à detentions multiples 
# Residence principale, autres logements, Livrets soumis à l'impôt, AVI, VMOB, Compte à terme, Compte associé
View(produit_t)

# Les produits à detention 
# Verifier l'age des personnes 1 et 2 des menages 
V <- produit %>% 
  filter(NOP %in% c("00")) %>% 
  distinct(NOP)

View(V)

View(produit_t)

View(individu)

V=individu %>%
  mutate(substr(IDENTIND20, nchar(IDENTIND20) - 1, nchar(IDENTIND20)) %in% c("01","02")) %>%
  distinct(AGE)

View(V)

#****************** Correction de la base des données *********************************************
names(BASE_ETUDE_FATTRI)

unique(BASE_ETUDE_FATTRI$NPERS)
unique(BASE_ETUDE_FATTRI$NIVEDUC_I)

V=BASE_ETUDE_FINAL %>% 
  filter(NIVEDUC_I == "Non déclaré" & ANNEE_ENQUETE == 2017)

View(V)

unique(individu_t$NIVEDUC)


# 2017 
individu_2017$NIVEDUC_I=NA

individu_2017$NIVEDUC_I[trimws(individu_2017$DIPDET) == ""] = 0       # Non déclaré
individu_2017$NIVEDUC_I[individu_2017$DIPDET %in% c("00",00,70,71)] = 1       # Sans diplome ou CEP
individu_2017$NIVEDUC_I[individu_2017$DIPDET %in% c(45,51,52,53,60)] = 2    # brevet
individu_2017$NIVEDUC_I[individu_2017$DIPDET %in% c(41,42,43,44)] = 3       # bac ou bac+1
individu_2017$NIVEDUC_I[individu_2017$DIPDET %in% c(31,32,33,34,35,36)] = 4 # bac+2
individu_2017$NIVEDUC_I[individu_2017$DIPDET %in% c(22,23,24)] = 5          # bac+3
individu_2017$NIVEDUC_I[individu_2017$DIPDET %in% c(21)] = 6                # bac+4 
individu_2017$NIVEDUC_I[individu_2017$DIPDET %in% c(13,14,15,16,17)] = 7    # bac+5
individu_2017$NIVEDUC_I[individu_2017$DIPDET %in% c(11,12)] = 8             # bac+8

mean(is.na(individu_2017$NIVEDUC_I)) * 100 

individu_2017$AGE_RELATIF = 2018-individu_2017$ANAIS

unique(individu_2017$NIVEDUC_I)

V=individu_2017 %>% 
  filter(NIVEDUC_I == 0) %>% 
  select(DIPDET, NIVEDUC_I, AGE_RELATIF)

View(V)


#****** Corriger la CSP ***************************








