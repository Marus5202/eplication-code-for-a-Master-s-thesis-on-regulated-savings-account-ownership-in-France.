#==================================  Mes brouillons=====================================================================
# Corrections des valeurs extrêmes 
MENAGE=menage_tert %>% 
  filter(ZREVDISP<=0) %>% 
  select(IDENT, ZREVDISP,ZREVDEC,ZPRODFIN,ZPSOCM,ZIMPOM,ZTHAB,ZIMPVALM,ZCSGIM,ZCSGDM,ZCSGREVPAT)

dim(menage)
View(MENAGE)
nrow(MENAGE)

View(menage_tert)
# Vérifier les anomalies, à partir des variables utilisées dans le calcul du revenu disponible
nrow(MENAGE)

nrow(MENAGE)/nrow(menage_tert) * 100

summary(MENAGE)

mena

# 103 menages soit 1% de notre base
# 50% des menages ont declarés un revenu de 0, vérifier leur situation, ces ménages paient en moyenne une somme 
# d'impôt plus élévée ( 3,15 M€). 75% de ces ménagesont un revenu inférieur à -90 euros. Vérifions leur situation $
# professionnelle  et le type de ménage, la situation à l'emploi 

MENAGE1=menage_tert %>% 
  filter(ZREVDISP<0) %>% 
  select(IDENT, ZREVDISP, SITUAPR, TYPMEN5)

View(MENAGE1)

MENAGE1=menage_2017 %>% 
  filter(ZREVDISP<0) %>% 
  select(ZREVDISP, ZREVDISP1, ZREVDEC, ZPRODFIN, ZPSOCM, ZIMPOM, ZTHAB, ZIMPVALM, ZCSGIM, ZCSGDM, ZCSGREVPAT)

View(MENAGE1)


menage_tert %>% 
  filter(ZREVDISP<0) %>% 
  summarise(n())


x <- menage_2017$ZREVDISP
w <- menage_2017$pond_trans
moy_pond <- weighted.mean(x, w, na.rm = TRUE)
moy_pond

ecart_type_pond <- sqrt(
  sum(w * (x - moy_pond)^2, na.rm = TRUE) /
    sum(w[!is.na(x)]))

ecart_type_pond

Queues = moy_pond + 2*1.960*ecart_type_pond
Queues

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

# Les ménages ayant des revenus négatifou nuls sont majoritairement des Retraité 
# ou préraitraité, etudiant et chômeur 

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
menage_tert$CSPR8

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

# Sur_representation des iondependants (artisans, chef d'entreprise, agriculteurs, commerçants) près de la 
# moitié, C'est plutôt cohérent. 

# Correction des valeurs extrêmes : Choix du revenu comme variable de correction 
# Car indicateur du niveau de vie, détermine ma conso des menages indirectement l'épargne

#====================================================================================================
# Traitement des valeurs aberrantes pour la distribution de revenu : Approches par les valeurs extrêmes
# Detrminer le seuil des valeurs aberrantes

# Je suis entrain de detester un truic de nouveau

# Determiner à partir de quel seuil la queue de la distribution devient lourde, idée trouvée le seuil des queues à droite
# Faire coincider avec l'IQR adéquat (pour assurer la robustesse du seuil choisit)

# Tri des revenus
ord=order(menage_tert$ZREVDISP)
menage_tert$ZREVDISP=menage_tert$ZREVDISP[ord]


## QQ plot (Construction de la Fonction de repartition pondéré)
W=sum(menage_tert$POND_TRANS)
Fhat=cumsum(menage_tert$POND_TRANS)/(W+max(menage_tert$POND_TRANS))

# QQ-plot exponentiel pôndéré (Vérifier si les données suivent une loi exponentielle)
plot(-log(1-Fhat),menage_tert$ZREVDISP,xlab="Quantiles exponentiels théoriques (pondérées)",
     ylab="Revenu",
     main="QQ-plot exponenytiel pondéré")

# les revenus ne suivent clairement pas une loi exponentielle et la présence d'une queue très lourde à droite est manifeste

# QQ-plot de Pareto (Vérifier si la distribution est de type Pareto)
plot(-log(1-Fhat),log(menage_tert$ZREVDISP),xlab="Quantiles exponentiels théoriques (pondérées)",
     ylab="Revenu",
     main="QQ-plot Pareto pondéré")

# La queue à droite de la distribution semble aligner avec la distribution de pareto, il y a des extr^mes mais pas des valeurs abrerrantes
# Par contre la queue gauche indique de très faible valeurs (négatives)

## Mean Excess plot 
mean_excess =function(x,w,u) {
  idx=which(x>u)
  if (length(idx)==0) return(NA)
  sum(w[idx]*(x[idx]-u))/sum(w[idx])
}

seuils =wtd.quantile(menage_tert$ZREVDISP, weights = menage_tert$POND_TRANS, probs = seq(0.5, 0.99, 0.01))
me_vals=sapply(seuils,function(u) mean_excess(menage_tert$ZREVDISP,menage_tert$POND_TRANS,u))


plot(seuils, me_vals, type="l",xlab="Seuil U",ylab="Excès moyen pondéré",main="Mean excess plot pondéré")

# courbe décroissante → queue légère (type normale) ;
# courbe approximativement constante → queue exponentielle ;
# courbe croissante → queue lourde (type Pareto). (On est claisrement dans ce cas de figure)

p <- plot_ly(x = seuils,y = me_vals,type = "scatter",mode = "lines") %>%
  layout(title = "Mean excess plot pondéré",xaxis = list(title = "Seuil U"),yaxis = list(title = "Excès moyen pondéré"))

p

# La courbe deveint Croissante, donc que lourde, la courbe devient croissante à partir de 112k, et libéaire à partir de 214k qui sera suspceté 
# comme seuil des extrêmes à droite seuil u des extrêmes 

# Estimateur de Hill adapté au poids des observations (Calcul de l'indice de queue de la distribution de la Pareto)
hill_pondere=function(x,W,k_range) {
  n=length(x)
  ord <- order(menage$ZREVDISP, decreasing = TRUE)
  x_desc=rev(x)
  w_desc=rev(W)
  sapply(k_range,function(k) {
    xk1=x_desc[k+1]
    num=sum(w_desc[1:k] * log(x_desc[1:k]/xk1))
    den=sum(w_desc[1:k])
    num/den
  })
}


k_range=10:300
gamma_hill=hill_pondere(menage_tert$ZREVDISP,menage_tert$POND_TRANS,k_range)


plot(k_range, gamma_hill,
     type = "l",
     xlab = "k (nombre d'observations extrêmes retenues)",
     ylab = expression(hat(gamma)),
     main = "Hill plot pondéré")

# Le seuil 214 k€ est le seuil de modélisation de la queue, toute valeurs supérieures est dans la queue 
#=========================================================
# Le Mean Excess Plot met en évidence une croissance de l'excès moyen à partir d'environ 110 k€, 
# indiquant la présence d'une queue lourde. Toutefois, le Hill plot révèle une stabilisation plus marquée de 
# l'estimateur de Hill pour des seuils plus élevés. Afin de privilégier la robustesse de l'ajustement dans 
# la région asymptotique, le seuil de 214 k€ a été retenu pour définir la queue des revenus extrêmes.
# =========================================================

# Faire l'approche IQR et voir celui quyi correspond à celui pour extraitre la parti gauche et droite des valeurs extrêmes

#===========================================================

# Ecart IQR
Q1=Q[1]  
Q3=Q[3]
IQR = Q3-Q1
born_inf <- Q1 -6.0*IQR
born_inf
born_sup <- Q3 + 6.0*IQR
born_sup

sum(menage_tert$ZREVDISP < Q1 - 1.5*IQR | menage_tert$ZREVDISP > Q3 + 1.5*IQR)
round(sum(menage_tert$ZREVDISP < Q1 - 1.5*IQR | menage_tert$ZREVDISP > Q3 + 1.5*IQR)/nrow(menage_tert) * 100,2)

sum(menage_tert$ZREVDISP < Q1 - 2.5*IQR | menage_tert$ZREVDISP > Q3 + 2.5*IQR)
round(sum(menage_tert$ZREVDISP < Q1 - 2.5*IQR | menage_tert$ZREVDISP > Q3 + 2.5*IQR)/nrow(menage_tert) * 100,2)

sum(menage_tert$ZREVDISP < Q1 - 6.0*IQR | menage_tert$ZREVDISP > Q3 + 6.0*IQR)
round(sum(menage_tert$ZREVDISP < Q1 - 6.0*IQR | menage_tert$ZREVDISP > Q3 + 6.0*IQR)/nrow(menage_tert) * 100,2)
# Perte de 301 observations


# Identifier les valeurs extrêmes 
outliers_revenu=subset(menage_tert,ZREVDISP <born_inf | ZREVDISP > born_sup)

dim(outliers_revenu)

# Base sans valeur extrême
menage_tert_clean <- subset(menage_tert,!(IDENT %in% outliers_revenu$IDENT))

ggplot(menage_tert_clean, aes(x = ZREVDISP, weight = POND_TRANS)) + geom_histogram(bins = 30) 

dim(menage)  

# Le revenu n'est pas normal mais faire le test de Normalité avant et après, pas obligé d'avoir le revenu normal car pas obligé

#======================== MONTANT SUR LES LIVRETS 
# Ecart IQRpour les personnes au délà du seuil de LA encours (Enlever les plus extrêmes)

Q1=Q[1]  
Q3=Q[3]
IQR = Q3-Q1
born_inf <- Q1 - 1.5*IQR
born_inf
born_sup <- Q3 + 1.5*IQR
born_sup

sum(menage_tert_clean$MLA < Q1 - 3.0*IQR | menage_tert_clean$MLA > Q3 + 3.0*IQR)
round(sum(menage_tert_clean$MLA < Q1 - 3.0*IQR | menage_tert_clean$MLA > Q3 + 3.0*IQR)/nrow(menage_tert) * 100,2)
# Perte de 29 observations, qui sont très extrêmes


# Identifier les valeurs extrêmes 
outliers_LA=subset(menage_tert_clean,MLA <born_inf | MLA > born_sup)

dim(outliers_LA)

# Base sans valeur extrême LA 
menage_tert_clean <- subset(menage_tert_clean,!(IDENT %in% outliers_LA$IDENT))

ggplot(menage_tert_clean, aes(x = MLA, weight = POND_TRANS)) + geom_histogram(bins = 30) 

# On partira sur le log aussi log(x+1) afin d'éviter les valeurs nulles
menage_tert_clean$logMLA = log(menage_tert_clean$MLA+1)

# Distribution 
ggplot(menage_tert_clean, aes(x = MLA, weight = POND_TRANS)) + geom_histogram(bins = 30) 

ggplot(menage_tert_clean, aes(x = logMLA, weight = POND_TRANS)) + geom_histogram(bins = 30) 

boxplot(menage_tert_clean$MLA) # Avant
boxplot(menage_tert_clean$logMLA) #Après

ggplot(menage_tert_clean,
       aes(x = logMLA,
           weight = POND_TRANS)) +
  geom_density(fill = "steelblue", alpha = 0.4) +
  labs(
    title = "Densité pondérée du de l'encours du LA",
    x = "Encours LA",
    y = "Densité"
  ) +
  theme_minimal()

# ============== LDDS
# Pondération
design <- svydesign(
  id = ~1,                # Pas de grappes
  weights = ~menage_tert_clean$POND_TRANS, # Variable de poids
  data = menage_tert_clean)

# Valeur extrême
# Moyenne 
weighted.mean(menage_tert_clean$MLDDS,menage_tert_clean$POND_TRANS)

# Median
svyquantile(~menage_tert_clean$MLDDS,design,quantiles = 0.5,na.rm = TRUE)        

# Quantile
Q=wtd.quantile(menage_tert_clean$MLDDS, weights = menage_tert_clean$POND_TRANS, probs = c(0.25, 0.5, 0.75))
Q

# Décile 
with(menage_tert_clean, 
     wtd.quantile(menage_tert_clean$MLDDS, weights = menage_tert_clean$POND_TRANS, probs = (1:10)/10),type = 'quantile') 

summary(menage_tert_clean$MLDDS)

# Valeur extreme : Les menages qui ont au délà du plafond
MENAGE=menage_tert_clean %>% 
  filter(MLDDS > 12000) %>% 
  select(IDENT,POND_TRANS, MLA, MLDDS, MLEP)

View(MENAGE) 
nrow(MENAGE) # 1264 livrets

Q=wtd.quantile(MENAGE$MLDDS, weights = MENAGE$POND_TRANS, probs = c(0.25, 0.5, 0.75))
Q

# Ecart IQR pour les personnes au délà du seuil de LA encours (Enlever les plus extrêmes)

Q1=Q[1]  
Q3=Q[3]
IQR = Q3-Q1
born_inf <- Q1 - 3.0*IQR
born_inf
born_sup <- Q3 + 3.0*IQR
born_sup

sum(menage_tert_clean$MLDDS < Q1 - 3.0*IQR | menage_tert_clean$MLDDS > Q3 + 3.0*IQR)
round(sum(menage_tert_clean$MLDDS < Q1 - 3.0*IQR | menage_tert_clean$MLDDS > Q3 + 3.0*IQR)/nrow(menage_tert) * 100,2)
# Perte de 27 observations, qui sont très extrêmes


# Identifier les valeurs extrêmes 
outliers_LDDS=subset(menage_tert_clean,MLDDS <born_inf | MLDDS > born_sup)

dim(outliers_LDDS)

# Base sans valeur extrême LDDS 
menage_tert_clean <- subset(menage_tert_clean,!(IDENT %in% outliers_LDDS$IDENT))

ggplot(menage_tert_clean, aes(x = MLDDS, weight = POND_TRANS)) + geom_histogram(bins = 30) 

# On partira sur le log aussi log(x+1) afin d'éviter les valeurs nulles
menage_tert_clean$logMLDDS = log(menage_tert_clean$MLDDS+1)

# Distribution 
ggplot(menage_tert_clean, aes(x = MLDDS, weight = POND_TRANS)) + geom_histogram(bins = 30) 

ggplot(menage_tert_clean, aes(x = logMLDDS, weight = POND_TRANS)) + geom_histogram(bins = 30) 

boxplot(menage_tert_clean$MLDDS) # Avant
boxplot(menage_tert_clean$logMLDDS) #Après

ggplot(menage_tert_clean,
       aes(x = logMLDDS,
           weight = POND_TRANS)) +
  geom_density(fill = "steelblue", alpha = 0.4) +
  labs(
    title = "Densité pondérée du de l'encours du LDDS",
    x = "Encours LDDS",
    y = "Densité"
  ) +
  theme_minimal()

(nrow(menage)/ nrow(menage_tert_clean) - 1) * 100  # Perte de 3.6% de la base de donnée de base

# ============== LEP
# Pondération
design <- svydesign(
  id = ~1,                # Pas de grappes
  weights = ~menage_tert_clean$POND_TRANS, # Variable de poids
  data = menage_tert_clean)

# Valeur extrême
# Moyenne 
weighted.mean(menage_tert_clean$MLEP,menage_tert_clean$POND_TRANS)

# Median
svyquantile(~menage_tert_clean$MLEP,design,quantiles = 0.5,na.rm = TRUE)        

# Quantile
Q=wtd.quantile(menage_tert_clean$MLEP, weights = menage_tert_clean$POND_TRANS, probs = c(0.25, 0.5, 0.75))
Q

# Décile 
with(menage_tert_clean, 
     wtd.quantile(menage_tert_clean$MLEP, weights = menage_tert_clean$POND_TRANS, probs = (1:10)/10),type = 'quantile') 

summary(menage_tert_clean$MLEP)

# Valeur extreme : Les menages qui ont au délà du plafond
MENAGE=menage_tert_clean %>% 
  filter(MLEP > 10000) %>% 
  select(IDENT,POND_TRANS, MLA, MLDDS, MLEP)

View(MENAGE) 
nrow(MENAGE) # 1264 livrets

Q=wtd.quantile(MENAGE$MLEP, weights = MENAGE$POND_TRANS, probs = c(0.25, 0.5, 0.75))
Q

# Ecart IQR pour les personnes au délà du seuil de LA encours (Enlever les plus extrêmes)

Q1=Q[1]  
Q3=Q[3]
IQR = Q3-Q1
born_inf <- Q1 - 3.0*IQR
born_inf
born_sup <- Q3 + 3.0*IQR
born_sup

sum(menage_tert_clean$MLEP < Q1 - 3.0*IQR | menage_tert_clean$MLEP > Q3 + 3.0*IQR)
round(sum(menage_tert_clean$MLEP < Q1 - 3.0*IQR | menage_tert_clean$MLEP > Q3 + 3.0*IQR)/nrow(menage_tert) * 100,2)
# Perte de 14 observations, qui sont très extrêmes


# Identifier les valeurs extrêmes 
outliers_LEP=subset(menage_tert_clean,MLEP <born_inf | MLEP > born_sup)

dim(outliers_LEP)

# Base sans valeur extrême LEP 
menage_tert_clean <- subset(menage_tert_clean,!(IDENT %in% outliers_LEP$IDENT))

ggplot(menage_tert_clean, aes(x = MLEP, weight = POND_TRANS)) + geom_histogram(bins = 30) 

# On partira sur le log aussi log(x+1) afin d'éviter les valeurs nulles
menage_tert_clean$logMLEP = log(menage_tert_clean$MLEP+1)

# Distribution 
ggplot(menage_tert_clean, aes(x = MLEP, weight = POND_TRANS)) + geom_histogram(bins = 30) 

ggplot(menage_tert_clean, aes(x = logMLEP, weight = POND_TRANS)) + geom_histogram(bins = 30) 

boxplot(menage_tert_clean$MLEP) # Avant
boxplot(menage_tert_clean$logMLEP) #Après

ggplot(menage_tert_clean,
       aes(x = logMLEP,
           weight = POND_TRANS)) +
  geom_density(fill = "steelblue", alpha = 0.4) +
  labs(
    title = "Densité pondérée du de l'encours LEP",
    x = "Encours LEP",
    y = "Densité"
  ) +
  theme_minimal()

(nrow(menage)/ nrow(menage_tert_clean) - 1) * 100  # Perte de 3.75% de la base de donnée de base

dim(menage_tert_clean)

#======================= Calcul du revenu disponible================================
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
                          ,menage_db$ZREVDISP)

sum(is.na(menage_db$ZREVDISP)) # Il n'y a plus de manquants


=======================================
# Partie sur l'autre approche
bornes <- wtd.quantile(
  menage_tert$ZREVDISP,
  weights = menage_tert$POND_TRANS,
  probs = c(8e-03, 0.9999),
  na.rm = TRUE)

bornes

v=c(9e-07, 0.9999991)
8e-03+0.99


menage_tert1 <- subset(menage_tert,ZREVDISP >= bornes[1] & ZREVDISP <= bornes[2])

ggplot(menage_tert1,aes(x = log(ZREVDISP+1), weight = POND_TRANS)) + geom_density(fill = "steelblue", alpha = 0.4) +
  labs(title = "Densité pondérée du revenu disponible", x = "Revenu disponible",y = "Densité") +
  theme_minimal()

# Log pour voir
ggplot(menage_tert1, aes(x = ZREVDISP, weight=POND_TRANS)) +
  geom_density(fill = "steelblue", alpha = 0.4) +
  scale_x_log10() +
  labs(x = "Revenu disponible (échelle log)", y = "Densité")

# Combien j'ai rétiré : 58 observations
sum(menage_tert$ZREVDISP < bornes[1] | menage_tert$ZREVDISP > bornes[2],na.rm = TRUE)

Extreme_revenue = menage_tert %>%
  filter(menage_tert$ZREVDISP < bornes[1] | menage_tert$ZREVDISP > bornes[2]) %>%
  select(ZREVDISP)

View(Extreme_revenue)
#====================================================================================================================

#=============== ACP pour les quantitatives
install.packages("FactoMineR")

library("FactoMineR")

# Remettre les ordinales en qualitatives
vars_ordinales <- c("RISKFI","NIVEDUC","CATAGE_RELATIF","decile_patrimoine","decile_revenu")

base_modele[vars_ordinales] <- lapply(base_modele[vars_ordinales],factor)

sapply(base_modele[vars_ordinales], class)

# Identifier les variables quantitatives 
vars_quant_acp <- names(base_modele)[
  sapply(base_modele, is.numeric)]

vars_quant_acp

#  Exclure automatiquement ce qui ne doit pas entrer dans l'ACP
vars_quant_acp <- setdiff(
  vars_quant_acp,c("IDENT","POND_TRANS","MLA", "MLDDS", "MLEP","R_LIVABL", "R_LDD", "R_LEP", "MPANEL", "MPANEL_NEXT"))

# Vérifier les variables retenues
vars_quant_acp
length(vars_quant_acp)

base_acp <- base_modele[, vars_quant_acp]

# Vérifier les NA
sort(colSums(is.na(base_acp)),decreasing = TRUE)

# ACP
res_acp <- PCA( base_acp,scale.unit = TRUE, graph = FALSE)


res_acp <- PCA(
  base_acp,
  scale.unit = TRUE,
  ncp = 15,
  graph = FALSE
)

# Variance expliqué : Critère de Kaisen : Valeur propre > 1  donc ici 1- 15 comp, 66,06% des informations
res_acp$eig

# Contribution des variables
contrib_acp <- sort(
  rowSums(res_acp$var$contrib[,1:15]),
  decreasing = TRUE
)

contrib_acp
dim(res_acp$var$contrib)


contrib_acp_df <- data.frame(
  Variable = names(contrib_acp),
  Contribution = as.numeric(contrib_acp)
) |>
  dplyr::arrange(desc(Contribution))

contrib_acp_df

#============================================
# PATRI_NET, ZREVFIN, NPERS, NBASS
vars_suppr2 <- c(
  "PATRI_NET",
  "ZREVFIN",
  "NPERS", "NBASS")

base_reduite <- base_reduite[ ,!names(base_reduite) %in% vars_suppr2]

# Again 

vars_num <- names(base_reduite)[
  sapply(base_reduite, is.numeric)
]

vars_a_exclure <- c(
  "IDENT",
  "POND_TRANS",
  "MLA",
  "MLDDS",
  "MLEP",
  "R_LIVABL",
  "R_LDD",
  "R_LEP"
)

# Ignore automatiquement les variables absentes
vars_num <- setdiff(vars_num, vars_a_exclure)

corr <- cor(
  base_reduite[, vars_num, drop = FALSE],
  use = "pairwise.complete.obs",
  method = "spearman"
)

high_corr <- which(
  abs(corr) >= 0.7 &
    upper.tri(corr),
  arr.ind = TRUE
)

high_corr_df <- data.frame(
  Variable_1 = rownames(corr)[high_corr[,1]],
  Variable_2 = colnames(corr)[high_corr[,2]],
  Correlation = corr[high_corr]
)

high_corr_df <- high_corr_df[
  order(abs(high_corr_df$Correlation),
        decreasing = TRUE),]

high_corr_df

# Nombre de quantitative 
vars_num <- names(base_reduite)[
  sapply(base_reduite, is.numeric)
]

vars_num <- setdiff(
  vars_num,
  c(
    "IDENT",
    "POND_TRANS",
    "MLA",
    "MLDDS",
    "MLEP",
    "R_LIVABL",
    "R_LDD",
    "R_LEP"))

length(vars_num)  # 37 explicatives quant

base_reduite$ZINIVVIE =NULL 

# ===============  Brouillons LASSO =======================================

vars_exclues <- c(
  "IDENT",
  "POND_TRANS",
  "MPANEL",
  "MPANEL_NEXT",
  "R_LIVABL",
  "R_LDD",
  "R_LEP",
  "MMLA",
  "MMLDDS",
  "MMLEP")

vars_x <- setdiff(names(base_reduite), vars_exclues)

# Construire la matrice explicative

set.seed(123)

# Variables à exclure
vars_exclues <- c(
  "IDENT",
  "POND_TRANS",
  "MPANEL",
  "MPANEL_NEXT",
  "R_LIVABL",
  "R_LDD",
  "R_LEP",
  "MMLA",
  "MMLDDS",
  "MMLEP")

# Variables explicatives
vars_x <- setdiff(names(base_reduite), vars_exclues)

# Matrice des X
X <- model.matrix(
  ~ .,
  data = base_reduite[, vars_x]
)[, -1]

# Variables cibles
cibles_binaires <- c(
  "R_LIVABL",
  "R_LDD",
  "R_LEP"
)

cibles_continues <- c(
  "MMLA",
  "MMLDDS",
  "MMLEP"
)

# Stockage des résultats
resultats <- list()

#############################################
# VARIABLES BINAIRES
#############################################

for(y_name in cibles_binaires){
  
  y <- base_reduite[[y_name]]
  
  cvfit <- cv.glmnet(
    x = X,
    y = y,
    family = "binomial",
    alpha = 1,
    weights = base_reduite$POND_TRANS,
    nfolds = 10
  )
  
  coefs <- coef(cvfit, s = "lambda.1se")
  
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

#############################################
# VARIABLES CONTINUES
#############################################

for(y_name in cibles_continues){
  
  y <- base_reduite[[y_name]]
  
  cvfit <- cv.glmnet(
    x = X,
    y = y,
    family = "gaussian",
    alpha = 1,
    weights = base$POND_TRANS,
    nfolds = 10,
    
  )
  
  coefs <- coef(cvfit, s = "lambda.1se")
  
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

# Affichage des resultats retenues 
for(v in names(resultats)){
  
  cat("\n============================\n")
  cat(v,"\n")
  cat("============================\n")
  
  cat("Lambda min :", resultats[[v]]$lambda_min, "\n")
  cat("Lambda 1se :", resultats[[v]]$lambda_1se, "\n")
  
  cat("Nb variables retenues :",
      length(resultats[[v]]$variables),
      "\n\n")
  
  print(resultats[[v]]$variables)
}

# Identificatiuon des variables communes aux 6 modèles 
variables_communes <- Reduce(
  intersect,
  lapply(resultats, function(x) x$variables)
)

variables_communes

# Classement de ces variables 
freq <- sort(
  table(
    unlist(
      lapply(resultats, function(x) x$variables)
    )
  ),
  decreasing = TRUE
)

freq

# ====================== Brouillon sur l'elimination des autocorelations 


#============== Selection des qualitatives 
table(sapply(base_reduite, class))

vars_char <- names(base_reduite)[
  sapply(base_reduite, is.character)]

base_reduite[vars_char] <- lapply(
  base_reduite[vars_char],
  factor)

vars_quali <- names(base_reduite)[
  sapply(base_reduite, function(x)
    is.factor(x) || is.character(x))]

length(vars_quali)

vars_ordinales <- c(
  "RISKFI",
  "NIVEDUC",
  "CATAGE_RELATIF",
  "decile_patrimoine",
  "decile_revenu"
)

base_reduite[vars_ordinales] <- lapply(
  base_reduite[vars_ordinales],
  factor)


vars_quali <- names(base_reduite)[
  sapply(base_reduite, is.factor)]


vars_quali <- setdiff(vars_quali,"IDENT")

n <- length(vars_quali)

cramer_mat <- matrix(
  NA,
  nrow = n,
  ncol = n
)

rownames(cramer_mat) <- vars_quali
colnames(cramer_mat) <- vars_quali

for(i in 1:n){
  
  for(j in i:n){
    
    tab <- table(
      base_reduite[[vars_quali[i]]],
      base_reduite[[vars_quali[j]]]
    )
    
    v <- suppressWarnings(
      CramerV(tab)
    )
    
    cramer_mat[i,j] <- v
    cramer_mat[j,i] <- v
  }
}

# Récuperer les associations fortes 
high_cramer <- which(
  cramer_mat > 0.80 &
    upper.tri(cramer_mat),
  arr.ind = TRUE
)

high_cramer_df <- data.frame(
  Variable_1 = rownames(cramer_mat)[high_cramer[,1]],
  Variable_2 = colnames(cramer_mat)[high_cramer[,2]],
  CramerV = cramer_mat[high_cramer]
)

high_cramer_df <- high_cramer_df[
  order(high_cramer_df$CramerV,
        decreasing = TRUE),]

high_cramer_df

View(cramer_mat)

View(base_reduite)

base_modele$LOGPROPRI 


# A supprimer 
vars_suppr_quali <- c(
  "R_DETPR",
  "LOGPROPRI",
  "DET",
  "R_LIVEP", # LIVEP = R_LIVDF ou R_LIVF
  "R_PEAACT",
  "R_ACTPEA", # R_PEAACT ou R_ACT
  "R_EPALO", # R_PEL ou R_CEL
  "R_CTIT", # Contenu dans R_VMOB
  "RSAL")

base_reduite <- base_reduite[,!names(base_reduite) %in% vars_suppr_quali]

# AGAIN 
vars_quali <- names(base_reduite)[
  sapply(base_reduite, function(x)
    is.factor(x) || is.character(x))]

vars_quali <- setdiff(vars_quali, "IDENT")

n <- length(vars_quali)

cramer_mat <- matrix(
  NA, nrow = n, ncol = n)

rownames(cramer_mat) <- vars_quali
colnames(cramer_mat) <- vars_quali

for(i in 1:n){
  
  for(j in i:n){
    tab <- table(
      base_reduite[[vars_quali[i]]],
      base_reduite[[vars_quali[j]]])
    
    v <- suppressWarnings(
      CramerV(tab))
    
    cramer_mat[i,j] <- v
    cramer_mat[j,i] <- v}}

# Seuil 
high_cramer <- which(
  cramer_mat > 0.5 &
    upper.tri(cramer_mat),
  arr.ind = TRUE)

high_cramer_df <- data.frame(
  Variable_1 = rownames(cramer_mat)[high_cramer[,1]],
  Variable_2 = colnames(cramer_mat)[high_cramer[,2]],
  CramerV = cramer_mat[high_cramer])

high_cramer_df <- high_cramer_df[
  order(high_cramer_df$CramerV,
        decreasing = TRUE),]

high_cramer_df

base_reduite$        
  # Suppression 
  vars_suppr_quali2 <- c(
    "R_PEA",
    "DETPRIV",
    "R_ACTPEA",
    "DETPRIV",
    "R_SUR",
    "R_LOGLOC",
    "R_PEPASSUR",
    "RMODIF_DC",
    "R_ACT",
    "R_PEPBANC")

base_reduite <- base_reduite[ ,!names(base_reduite) %in% vars_suppr_quali2]

vars_quali <- names(base_reduite)[
  sapply(base_reduite, function(x)
    is.factor(x) || is.character(x))]

vars_quali <- setdiff(vars_quali, "IDENT")

vars_quali <- names(base_reduite)[
  sapply(base_reduite, function(x)
    is.factor(x) || is.character(x))
]

vars_quali <- setdiff(vars_quali, "IDENT")

# Le V de CRamer 3
library(DescTools)

n <- length(vars_quali)

cramer_mat <- matrix( NA, nrow = n, ncol = n)

rownames(cramer_mat) <- vars_quali
colnames(cramer_mat) <- vars_quali

for(i in 1:n){
  
  for(j in i:n){
    
    tab <- table(
      base_reduite[[vars_quali[i]]],
      base_reduite[[vars_quali[j]]]
    )
    
    v <- suppressWarnings(CramerV(tab))
    
    cramer_mat[i,j] <- v
    cramer_mat[j,i] <- v}}

# Seuil 
high_cramer <- which(
  cramer_mat > 0.50 &
    upper.tri(cramer_mat),
  arr.ind = TRUE
)

high_cramer_df <- data.frame(
  Variable_1 = rownames(cramer_mat)[high_cramer[,1]],
  Variable_2 = colnames(cramer_mat)[high_cramer[,2]],
  CramerV = cramer_mat[high_cramer]
)

high_cramer_df <- high_cramer_df[
  order(high_cramer_df$CramerV,
        decreasing = TRUE),
]

high_cramer_df

nrow(high_cramer_df)

View(base_reduite)

dim(base_reduite)
names(base_reduite)

names(base_reduite)



# ==================== Exemple de Modele : A revenir ==============================================================
# Exemple de modèle pour la detention
vars_modele <- c(
  "AGE_RELATIF",
  "SEXEPR",
  "NIVEDUC",
  "SITUAPR",
  "decile_revenu",
  "decile_patrimoine",
  "DETPRO",
  "LOGPROPRI",
  "RISKFI",
  "R_AVI")

# LOGIT PONDERE 
library(survey)

design <- svydesign(
  ids = ~1,
  weights = ~POND_TRANS,
  data = base_reduite)

# modele LA detention 
mod_livabl <- svyglm(
  R_LIVABL ~
    AGE_RELATIF +
    SEXEPR +
    NIVEDUC +
    SITUAPR +
    decile_revenu +
    decile_patrimoine +
    DETPRO +
    LOGPROPRI +
    RISKFI +
    R_AVI,
  design = design,
  family = quasibinomial())

summary(mod_livabl)

# Odd ratio
summary(mod_livabl)
exp(coef(mod_livabl))  # Odd RATIO 

exp(cbind(
  OR = coef(mod_livabl),
  confint(mod_livabl)))


#================== LDDS
mod_ldd <- svyglm(
  R_LDD ~
    AGE_RELATIF +
    SEXEPR +
    NIVEDUC +
    SITUAPR +
    decile_revenu +
    decile_patrimoine +
    DETPRO +
    LOGPROPRI +
    RISKFI +
    R_AVI,
  design = design,
  family = quasibinomial()
)

summary(mod_ldd)

# Odd ratio
summary(mod_ldd)
exp(coef(mod_ldd))  # Odd RATIO 

exp(cbind(
  OR = coef(mod_ldd),
  confint(mod_ldd)))

#================== LEP
mod_lep <- svyglm(
  R_LEP ~
    AGE_RELATIF +
    SEXEPR +
    NIVEDUC +
    SITUAPR +
    decile_revenu +
    decile_patrimoine +
    DETPRO +
    LOGPROPRI +
    RISKFI +
    R_AVI,
  design = design,
  family = quasibinomial()
)

summary(mod_lep)

# Odd ratio
summary(mod_lep)
exp(coef(mod_lep))  # Odd RATIO 

exp(cbind(
  OR = coef(mod_lep),
  confint(mod_lep)))




















modele_essai <- glm(
  R_LIVABL ~ AGE_REEL + ZREVDISP + NENFANTS + NIVEDUC,
  data = MENAGE_MODEL,
  family = binomial(link = "probit"))

summary(modele_essai)


# ======================  Analyse des correspondances multiples====================================

# Indication des vecteurs de groupe
group <- c(
  4,  # Description du ménage
  3,  # Etat civil / situation familiale
  3,  # Activité professionnelle
  1,  # Ressources culturelles
  6,  # Revenus
  5,  # Habitudes de consommation
  4,  # Assurance-vie
  4,  # Epargne logement
  5,  # Epargne retraite
  3,  # Epargne salariale
  5,  # Valeurs mobilières
  2,  # Comptes à terme
  3,  # Livrets
  2,  # Agrégat financier
  2,  # Résidence principale
  3,  # Autres logements
  3,  # Agrégat immobilier
  3,  # Actifs professionnels
  6,  # Endettement
  4,  # Héritages et donations
  1,  # Risque financier
  3,  # Evénements de vie
  1 ) # Décile patrimoine

type = c(
  "m",  # Description du ménage
  "m",  # Etat civil / situation familiale
  "n",  # Activité professionnelle
  "n",  # Ressources culturelles
  "m",  # Revenus
  "m",  # Habitudes de consommation
  "m",  # Assurance-vie
  "m",  # Epargne logement
  "m",  # Epargne retraite
  "m",  # Epargne salariale
  "m",  # Valeurs mobilières
  "m",  # Comptes à terme
  "n",  # Livrets
  "m",  # Agrégat financier
  "m",  # Résidence principale
  "m",  # Autres logements
  "m",  # Agrégat immobilier
  "m",  # Actifs professionnels
  "m",  # Endettement
  "m",  # Héritages et donations
  "n",  # Risque financier
  "n",  # Evénements de vie
  "n")  # Décile patrimoine

data_mfa$NIVEDUC <- as.factor(data_mfa$NIVEDUC)
data_mfa$CSPR8 <- as.factor(data_mfa$CSPR8)

# Convertir les les colonnes textes
data_mfa <- data_mfa %>%
  mutate(across(where(is.character), as.factor))

sum(group) == ncol(data_mfa)
str(data_mfa)

sapply(data_mfa, function(x) {
  if (is.numeric(x)) "num"
  else if (is.factor(x)) "factor"
  else class(x)
})

# MFA
res_mfa <- MFA(
  data_mfa,
  group = group,
  type = type,
  name.group = c(
    "Description du ménage",
    "Etat civil / situation familiale",
    "Activité professionnelle",
    "Ressources culturelles",
    "Revenus",
    "Habitudes de consommation",
    "Assurance-vie",
    "Epargne logement",
    "Epargne retraite",
    "Epargne salariale",
    "Valeurs mobilières",
    "Comptes à terme",
    "Livrets",
    "Agrégat financier",
    "Résidence principale",
    "Autres logements",
    "Agrégat immobilier",
    "Actifs professionnels",
    "Endettement",
    "Héritages et donations",
    "Risque financier",
    "Evénements de vie",
    "Décile patrimoine"
  ),
  graph = FALSE
)

traceback()
# Checking de l'erreur 
sum(group) == ncol(data_mfa)

class(data_mfa$R_AVI)

str(data_mfa[, c("R_AVI", "R_AVIMULT", "R_AVIEURO")])

packageVersion("FactoMineR")

class(data_mfa$MAVI)


sapply(
  data_mfa[, c("R_AVI","R_AVIMULT","R_AVIEURO","MAVI")],
  class
)

data_mfa$MAVI

lapply(data_mfa[,c("MAVI")],table,useNA="ifany")

data.frame(position=seq_along(data_mfa),
           variable=names(data_mfa),
           groupe=rep(seq_along(group),group))

test_g7=data_mfa[,c("MAVI","R_AVI","R_AVIMULT","R_AVIEURO")]

FactoMineR::FAMD(test_g7,graph=FALSE)

length(group)
sum(group)
length(type)
ncol(data_mfa)

data.frame(variable=names(data_mfa),
           groupe=rep(seq_along(group),group),
           type_MFA =rep(type,group),
           classe_R=sapply(data_mfa,class))

class(data_mfa$TYPMEN5)

sapply(data_mfa, function(x) sum(is.na(x)))

sapply(data_mfa, function(x) {
  if (is.factor(x))
  {nlevels(droplevels(x)) 
  } else if (is.numeric(X)) {
    var(x)
  } else {
    NA
  }
})

g7=data_mfa[,c("MAVI","R_AVI","R_AVIMULT","R_AVIEURO")]

str(g7)

lapply(g7, function(x) {
  if (is.factor(x)) nlevels(x)
})

for (g in which(type=="m")) {
  
  debut=if (g==1) 1 else sum(group[1:(g-1)]) +1
  fin=sum(group[1:g])
  
  cat("")
}

# =======================================
install.packages("factoextra")
install.packages(
  "ggpubr",
  repos = "https://mirror.ibcp.fr/pub/CRAN/")

install.packages(
  "factoextra",
  repos = "https://mirror.ibcp.fr/pub/CRAN/")

install.packages(
  "ggpubr",
  dependencies = TRUE,
  repos = "https://mirror.ibcp.fr/pub/CRAN/"
)

packageVersion("ggpubr")


#===========
# Perojection des Y sur ces dimensions 5
library(FactoMineR)

# Les 6 dependantes 
Y = c("R_LIVABL","R_LDD","R_LEP","MMLA","MMLDDS","MMLEP")
data_afdm = base_selection[,!names(base_selection) %in% c("IDENT","POND_TRANS",Y)]

# Ajout des 6 y
data_afdm_proj = cbind(data_afdm, base_selection[,Y])

sup <- which(names(data_afdm_proj) %in% Y)

res_afmd = FAMD(data_afdm_proj,row.w = base_selection$POND_TRANS,ncp=5,sup.var = sup, graph = FALSE)

res_afdm$quanti.sup

#=====================  Broulllon MFA ==========================================
# Indicateurs synthétiques construits
# decile_patrimoine

# ANALYSE FACTOIRIELLE DES DONNEES MIXTES
# Installation des packages 
install.packages("FactoMineR")
library("FactoMineR")

# Organisation des variables 
data_mfa= base_selection %>% 
  select(
    # Groupe 1 : Description du ménage
    NENFANTS, NPERS, NACTIFS, TYPMEN5,
    
    # Groupe 2 : État civil / Situation familiale de la personne de référence
    MATRIPR, AGE_RELATIF, CATAGE_RELATIF,
    
    # Groupe 3 : Activité professionnelle personne de référence 
    SITUAPR, STATUTPR, CSPR8, 
    
    # Groupe 4 : TCM - Ressources culturelles personne de référence
    NIVEDUC,
    
    # Groupe 5 : Revenus 
    ZREVDISP, decile_revenu, RFIN, RLOG, RTRA, RRMI,
    
    # Groupe 6 : Habitudes de consommation
    CNDUR, CONSEP, DIEXP, DIF, LOYER,
    
    # Detention 
    ## Groupe 7 : Assurance vie
    MAVI, R_AVI, R_AVIMULT, R_AVIEURO,
    
    ## Groupe 8 : Épargne logement
    MCEL, MPEL, R_CEL, R_PEL, 
    
    ## Groupe 9 : Épargne retraite
    MEPRETRAITE, R_COMP, R_PERP, R_RETRAIT, R_PEP, 
    
    ## Groupe 10 : Épargne salariale
    MEPSALAR, MLEE, R_EPSALA, 
    
    ## Groupe 11 : Valeurs mobilières
    MVMOB, R_VMOB, R_ACTION, R_CAPI, R_PEATRE, 
    
    ## Groupe 12 : Comptes à terme
    MCOMPTERM, R_TERM,
    
    ## Groupe 13 : Livrets 
    R_LIVDF, R_LIVF, R_LIVJEUN,
    
    ## Groupe 14 : Agrégat financier
    R_PATFINANC, PATFISOM,
    
    # Actifs immobiliers 
    ## Groupe 15 : Résidence Principale 
    MRESPRIN, R_RESPRIN, 
    
    ## Groupe 16 : Autres logements
    NBLOG, R_LOGAUT, R_LOGSEC, 
    
    ## Groupe 17 : Agrégat immobilier
    PATRIRES, R_PATIMMO, PATIMM, 
    
    # Groupe 18 : Actifs professionnels
    NE, PATRIPRO, DETPRO,
    
    # Groupe 19 : Endettement
    MTDETTES, DETIMM, R_DETDI, R_DETEQ, R_DETRES, R_DETRE, 
    
    # Groupe 20 : Donations, héritages et aides
    Heritage, Donation_R, HERTABD, HERTABH,
    
    # Groupe 21 : Comportement financier
    RISKFI,
    
    # Groupe 22 : Événements de vie / Parcours
    RMODIF_CHO, RMODIF_MAL, RMODIF_RET,
    
    # Groupe 23 : Indicateurs synthétiques construits
    decile_patrimoine)

# =================== Contributions des variables 
# 4. Contributions des variables
contrib <- as.data.frame(res_afdm$var$contrib)

contrib$Variable <- rownames(contrib)

dims <- paste0("Dim.", 1:60)

contrib$score60 <- rowSums(contrib[, dims])

# 5. Classement des variables
contrib <- contrib[order(-contrib$score60), ]

# 6. Contribution relative
contrib$part <- contrib$score60 / sum(contrib$score60)

contrib$cumul <- cumsum(contrib$part)


# ******************* Modele Forward pour la selection des variables : Trop lourd ********************************


#===================== Je veux faire le Forward
cibles_binaires <- c("R_LEP", "R_LDD", "R_LIVABL")
cibles_quantitatives <- c("MMLA", "MMLDDS", "MMLEP")

n <- nrow(base_selection)
resultats <- list()

# Variables binaires
for(y in cibles_binaires){
  
  vars <- setdiff(names(base_selection), c(y, "POND"))
  
  f0 <- as.formula(paste(y, "~ 1"))
  ffull <- as.formula(paste(y, "~", paste(vars, collapse = " + ")))
  
  mod0 <- glm(f0,
              family = binomial(),
              weights = POND,
              data = base_selection)
  
  mod_full <- glm(ffull,
                  family = binomial(),
                  weights = POND,
                  data = base_selection)
  
  resultats[[y]] <- step(
    mod0,
    scope = list(lower = mod0, upper = mod_full),
    direction = "forward",
    k = log(n),
    trace = FALSE
  )
}

# Variables quantitatives
for(y in cibles_quantitatives){
  
  vars <- setdiff(names(base_selection), c(y, "POND"))
  
  f0 <- as.formula(paste(y, "~ 1"))
  ffull <- as.formula(paste(y, "~", paste(vars, collapse = " + ")))
  
  mod0 <- lm(f0,
             weights = POND,
             data = base_selection)
  
  mod_full <- lm(ffull,
                 weights = POND,
                 data = base_selection)
  
  resultats[[y]] <- step(
    mod0,
    scope = list(lower = mod0, upper = mod_full),
    direction = "forward",
    k = log(n),
    trace = FALSE
  )
}

# Variables retenues pour chaque cible
lapply(resultats, formula)

#========================================
summary(base_selection$POND)
all(base_selection$POND == round(base_selection$POND))
base_selection$POND

#=============  Sans la boucle 
base_selection$R_LEP_bin    <- ifelse(base_selection$R_LEP    == 1, 1, 0)
base_selection$R_LDD_bin    <- ifelse(base_selection$R_LDD    == 1, 1, 0)
base_selection$R_LIVABL_bin <- ifelse(base_selection$R_LIVABL == 1, 1, 0)

vars_candidats <- setdiff(
  names(base_selection),
  c("R_LEP","R_LDD","R_LIVABL",
    "MMLA","MMLDDS","MMLEP",
    "POND",
    "R_LEP_bin"))

n <- nrow(base_selection)

mod0 <- glm(
  R_LEP_bin ~ 1,
  family = quasibinomial(),
  weights = POND,
  data = base_selection)

summary(mod0)

mod_full <- glm(
  as.formula(
    paste("R_LEP_bin ~", paste(vars_candidats, collapse = " + "))),
  family = quasibinomial(),
  weights = POND,
  data = base_selection)

mod_bic <- step(mod0,
                scope = list(lower = mod0, upper = mod_full),
                direction = "forward",k = log(n),trace = TRUE)

summary(mod_bic)

#*********************** Corriger les NA du poids des INdividus*********************************
mean(is.na(BASE_ETUDE_FINAL$POIDS_IND_LONG)) * 100 

BASE_ETUDE_FINAL %>%
  group_by(ANNEE_ENQUETE) %>%
  summarise(
    part_NA = mean(is.na(POIDS_IND_LONG)),
    nb_NA = sum(is.na(POIDS_IND_LONG)),
    n = n())

V=BASE_ETUDE_FINAL %>% 
  filter(is.na(ANNEE_ENQUETE))

View(V)

BASE_ETUDE_FINAL = BASE_ETUDE_FINAL %>% 
  filter(!is.na(ANNEE_ENQUETE))

BASE_ETUDE_FINAL %>%
  filter(ANNEE_ENQUETE %in% c(2014, 2017)) %>%
  group_by(IDENTINDL) %>%
  filter(n_distinct(ANNEE_ENQUETE) == 2) %>%
  ungroup() %>%
  summarise(
    taux_na = mean(is.na(POIDS_IND_LONG)),
    nb_na = sum(is.na(POIDS_IND_LONG)),
    n = n())

sum(is.na(BASE_ETUDE_FINAL$ANNEE_ENQUETE))

sum(is.na(individu$POND_LONG_IND_2017))
sum(is.na(individu$POND_LONG_IND_2014)) 

unique()

sum(is.na(individu_2017$pond_long_ind)) 

V=individu %>% 
  select(IDENTINDL,POND_LONG_IND_2014,POND_LONG_IND_2017)
View(V)

nrow

View(BASE_ETUDE_FINAL)

P=BASE_ETUDE_FINAL %>% 
  filter(is.na(POIDS_IND_LONG) & ANNEE_ENQUETE == 2014) %>% 
  select(IDENTINDL,ANNEE_ENQUETE,POIDS_IND_LONG)

View(P)

Id_2014 = individu %>% 
  select(IDENTINDL,POND_LONG_IND_2014)

View(Id_2014)

P = P %>% 
  left_join(Id_2014,by="IDENTINDL")

View(P) 



P=BASE_ETUDE_FINAL %>% 
  filter(is.na(POIDS_IND_LONG) & ANNEE_ENQUETE == 2014) %>% 
  select(IDENTINDL,ANNEE_ENQUETE,POIDS_IND_LONG)

View(P)

Id_2017 = individu_2017 %>% 
  select(IDENTINDL,pond_long_ind)

View(Id_2017)

BASE_ETUDE_FINAL = BASE_ETUDE_FINAL %>% 
  left_join(Id_2017,by="IDENTINDL")

View(P)

# ********* Finalisation base **********************************************
View(BASE_2014F)
View(BASE_2017F)


names(individu)
individu$POND_LONG_IND_2014
individu$POND_LONG_IND_2017

names(individu)

M=individu_2017 %>% 
  select(IDENTINDL,pond_long_ind)

View(M) 

N=individu %>% 
  select(IDENTINDL,POND_LONG_IND_2014,POND_LONG_IND_2017)

View(N)

# Verifcation 
vars = c("LIVABL","ZREVDEC_I","AGE_RELATIF_I","ETAMATRI","NIVEDUC_I","LEP","LDD","IDENTINDL")

str(pdata2[vars])

sum(is.na(pdata2$IDENTINDL))

sapply(pdata2[c("ZREVDEC_I","AGE_RELATIF_I","LEP","LDD")],function(x) c(min =min(x,na.rm = TRUE),
                                                                        max=max(x,na.rm =TRUE),sd =sd(x,na.rm = TRUE)))



# ********************* PANEL *********************************************************************************
?cholmod_factor_ldetA

V = pdata2 %>% 
  filter(POND_LONG_IND_2014 < 0) %>% 
  select(IDENTINDL,ZREVDEC_I, POND_LONG_IND_2014)

View(V)

glm(
  LIVABL ~ ZREVDEC_I + AGE_RELATIF_I + ETAMATRI +
    NIVEDUC_I + LEP + LDD,
  data = pdata2,
  family = binomial()
)

glmmTMB(
  LIVABL ~ ZREVDEC_I + AGE_RELATIF_I + ETAMATRI +
    NIVEDUC_I + LEP + LDD +
    (1|IDENTINDL),
  data = pdata2,
  family = binomial())

colSums(is.na(pdata2[, c(
  "LIVABL",
  "ZREVDEC_I",
  "AGE_RELATIF_I",
  "ETAMATRI",
  "NIVEDUC_I",
  "LEP",
  "LDD",
  "IDENTINDL"
)]))

str(pdata2[, c("ETAMATRI", "NIVEDUC_I", "LEP", "LDD")])

table(pdata2$ETAMATRI, useNA = "ifany")
table(pdata2$NIVEDUC_I, useNA = "ifany")

summary(pdata2$ZREVDEC_I)
summary(pdata2$AGE_RELATIF_I)

sum(is.infinite(pdata2$ZREVDEC_I))
sum(is.infinite(pdata2$AGE_RELATIF_I))

m1 <- glmmTMB(
  LIVABL ~ 1 + (1|IDENTINDL),
  data = pdata2,
  family = binomial()
)
`
class(BASE_ETUDE_FATTRI$LIVABL)

glmmTMB(
  LIVABL ~ 1 + (1|IDENTINDL),
  data = BASE_RESERVE_PANEL,
  family = binomial()
)

sum(duplicated(
  pdata2[, c("IDENTINDL", "ANNEE_ENQUETE")]
))

doublons <- pdata2[
  duplicated(pdata2[, c("IDENTINDL", "ANNEE_ENQUETE")]) |
    duplicated(pdata2[, c("IDENTINDL", "ANNEE_ENQUETE")], fromLast = TRUE),
]

View(doublons)
`
# ******* Modelisation PANEL 
logit_re <- glmer(
  LIVABL ~ ZREVDEC_I + AGE_RELATIF_I + ETAMATRI +
    NIVEDUC_I + LEP + LDD +
    (1 | IDENTINDL),
  data = pdata2,
  family = binomial(link = "logit"),
  weights = POND_LONG_IND_2014)

# Modele a effet aleatoire
pglm(
  LIVABL ~ ZREVDEC_I + AGE_RELATIF_I + ETAMATRI +
    NIVEDUC_I + LEP + LDD,
  data = as.data.frame(pdata2),
  family = binomial("logit"),
  model = "random")

# Modele logit a effet fixe
clogit(
  LIVABL ~ ZREVDEC_I + AGE_RELATIF_I + ETAMATRI +
    NIVEDUC_I + LEP + LDD +
    strata(IDENTINDL),
  data = as.data.frame(pdata2)
)


# Vérificatio sur la taille du Panel 
unique(individu$INDIV_REINTERRO)
unique(individu$INDIV_PANEL)

# 2014
# Nombre d'individus par statut de réinterrogation
table(individu_2014$INDIV_REINTERRO)

# Nombre d'individus par statut de panel
table(individu_2014$INDIV_PANEL)

# 2017
# Nombre d'individus par statut de réinterrogation
table(individu_2017_NEW$INDIV_REINTERRO)

# Nombre d'individus par statut de panel
table(individu_2017$INDIV_PANEL)

# 2020
# Nombre d'individus par statut de réinterrogation
table(individu_2020_NEW$INDIV_REINTERRO)

# Nombre d'individus par statut de panel

library("haven")
individu_2014_NEW = read_sas("C:/Users/gessessinou/Downloads/BDD/2014_2015/individu.sas7bdat")
individu_2017_NEW = read_sas("C:/Users/gessessinou/Downloads/BDD/2017_2018/individu.sas7bdat")
individu_2020_NEW = read_sas("C:/Users/gessessinou/Downloads/BDD/2020_2021/individu.sas7bdat")

table(individu_2014_NEW$INDIV_PANEL) 
table(individu_2017_NEW$INDIV_PANEL) # 
table(individu_2020_NEW$INDIV_PANEL) # 

table(individu_2017_NEW$INDIV_REINTERRO) # 
table(individu_2020_NEW$INDIV_REINTERRO) # 


nrow(individu_2017)
nrow(individu_2014)
nrow(individu_2017)
nrow(individu_db_2014)
nrow(individu_2020_NEW)

nrow(pdata)

# Combien d'individu sont dans les trois 
#************** Faire joindre la ponderation longitudinale 2014 pour exploiter tout le PANEL ************************ 
View(BASE_2014F)
View(BASE_2017F)
View(BASE_2020F)

#* Anticper les NA dans les diffrentes bases 
Id = individu %>% 
  select(IDENTINDL,POND_LONG_IND_2014)

View(Id)

# Fusion 
IdInd2014 = individu_2014_NEW %>% 
  select(IDENTINDL) %>% 
  distinct()

IdInd2017 = individu_2017_NEW %>% 
  select(IDENTINDL) %>% 
  distinct()

IdInd2020 = individu_2020_NEW %>% 
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

# Faire une phrase entre les 

# ***************   Repartition montant moyen Livret **************************************
v <- ggplot(livreta_age,
            aes(x = AGE_RELATIF, y = montant_moyen, group = 1)) +
  geom_line(color = "#1f77b4", linewidth = 1.2) +
  geom_point(color = "#1f77b4", size = 2) +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  labs(
    title = "Montant moyen détenu sur Livret A selon l'âge",
    x = "Âge",
    y = "Montant moyen (€)",
    caption = "Source : Enquête Patrimoine - Pondération appliquée"
  ) +
  scale_y_continuous(
    labels = scales::label_number(
      big.mark = " ",
      suffix = " €"
    )
  ) +
  coord_cartesian(xlim = c(0, 98)) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.minor = element_blank()
  )

ggplotly(v)

# **** Etsimation avec le package WeMix *******************************************
bpanel$poids_norm =bpanel$POND_LONG_IND_2014/mean(bpanel$POND_LONG_IND_2014, na.rm =TRUE)

mod_logit_re = mix(LIVABL~LDD+ (1 | IDENTINDL), 
                   data = bpanel,
                   family=binomial(link="logit"),
                   weights = c("poids_obs","poids_norm"), # Ordre 1 et ordre 2 
                   nQuad = 13 )# Nombre de points de quadrature (impair)

summary(mod_logit_re)

# ******** Estimer par le package WemIx pour tenir compte du poids d"enquête  *****************************************************
# NIveau 1 : Observation 
BASE_ETUDE_FATTRI$poids_obs = 1 

# NIveau 2 (individu) : poids de sondage : verifier si identique 
stopifnot(all(tapply(pdata$POND_LONG_IND_2014,pdata$IDENTINDL, function(w) length(unique(w)) == 1)))


# **** Poids positifs **********************
View(BASE_ETUDE_FATTRI)

tmp <- subset(BASE_ETUDE_FATTRI,
              POND_LONG_IND_2014 > 0)



mod0 <- mix(
  LIVABL ~ (1 | IDENTINDL),
  data = tmp,
  family = binomial(),
  weights = c("poids_obs","POND_LONG_IND_2014"),
  nQuad = 7)



class(BASE_ETUDE_FATTRI)

# ****** Corrections des Varianles Niveduc et CS8I ********************************
#************** Corriger les variables CS8_I et NIVEDUC_I ***************************************
#*************** Recupérer les personnes qui ont déclaré une seule fois *************************
id_non_declares <- BASE_ETUDE_FATTRI2 %>%
  filter(is.na(CS8_I)) %>%
  pull(IDENTINDL) %>%
  unique()

resultat <- BASE_ETUDE_FATTRI2 %>%
  filter(IDENTINDL %in% id_non_declares) %>%
  select(IDENTINDL, ANNEE_ENQUETE, CS8_I, AGE_RELATIF_I, NIVEDUC_I) %>%
  arrange(IDENTINDL, ANNEE_ENQUETE)

View(resultat)
nrow(resultat)
View(BASE_ETUDE_FATTRI1)
unique(BASE_ETUDE_FATTRI1$CS8_I)

View(BASE_2014)

# ******* Faire une imputation longitudinale ********************
BASE_ETUDE_FATTRI1 <- BASE_ETUDE_FATTRI %>%
  arrange(IDENTINDL, ANNEE_ENQUETE) %>%
  group_by(IDENTINDL) %>%
  mutate(
    CS8_I = ifelse(CS8_I == "Non dclaré",
                   NA,
                   as.character(CS8_I))
  ) %>%
  fill(CS8_I, .direction = "down") %>%
  ungroup()

BASE_ETUDE_FATTRI2 <- BASE_ETUDE_FATTRI1 %>%
  arrange(IDENTINDL, ANNEE_ENQUETE) %>%
  group_by(IDENTINDL) %>%
  fill(CS8_I, .direction = "up") %>%
  ungroup()

BASE_ETUDE_FATTRI2 %>%
  filter(IDENTINDL == "117170002650489403") %>%
  select(IDENTINDL, ANNEE_ENQUETE, CS8_I)

# ***** Remplacer les CS8 des moins de 15-16 **********************
BASE_ETUDE_FATTRI2 <- BASE_ETUDE_FATTRI2 %>%
  mutate(CS8_I = ifelse(
    AGE_RELATIF_I <= 16,
    "Moins de 15-16 ans",
    CS8_I))

mean(is.na(BASE_ETUDE_FATTRI2$CS8_I)) * 100

# ***** 35 personnes dans sans actvité professionnelle ************
BASE_ETUDE_FATTRI2 <- BASE_ETUDE_FATTRI2 %>%
  mutate(CS8_I = ifelse(is.na(CS8_I),"Autres personnes sans activité professionnelle",CS8_I))


#***** Corriger le niveau d'éducation ***************************
id_non_declares <- BASE_ETUDE_FATTRI4 %>%
  filter(is.na(NIVEDUC_I)) %>%
  pull(IDENTINDL) %>%
  unique()

resultat <- BASE_ETUDE_FATTRI4 %>%
  filter(IDENTINDL %in% id_non_declares) %>%
  select(IDENTINDL, ANNEE_ENQUETE, CS8_I, AGE_RELATIF_I, NIVEDUC_I) %>%
  arrange(IDENTINDL, ANNEE_ENQUETE)

View(resultat)
nrow(resultat)
View(BASE_ETUDE_FATTRI1)
unique(BASE_ETUDE_FATTRI1$CS8_I)

View(BASE_2014F_PANEL)
View(BASE_2017F_PANEL)

# ******** Imputation longitidinale ***********************
BASE_ETUDE_FATTRI3 <- BASE_ETUDE_FATTRI2 %>%
  arrange(IDENTINDL, ANNEE_ENQUETE) %>%
  group_by(IDENTINDL) %>%
  mutate(
    NIVEDUC_I = ifelse(NIVEDUC_I == "Non dclaré",
                       NA,
                       as.character(NIVEDUC_I))
  ) %>%
  fill(NIVEDUC_I, .direction = "down") %>%
  ungroup()

BASE_ETUDE_FATTRI4 <- BASE_ETUDE_FATTRI3 %>%
  arrange(IDENTINDL, ANNEE_ENQUETE) %>%
  group_by(IDENTINDL) %>%
  fill(NIVEDUC_I, .direction = "up") %>%
  ungroup()

# La le niveau d'e
BASE_ETUDE_FATTRI4 <- BASE_ETUDE_FATTRI4 %>%
  mutate(NIVEDUC_I = ifelse(
    AGE_RELATIF_I <= 16,
    "Moins de 15-16 ans",
    NIVEDUC_I))

sum(is.na(BASE_ETUDE_FATTRI4$NIVEDUC_I)) # 11 personnes 

#************* Remplacer par lemode de cheque age ****************************
# Mode par âge
mode_age <- BASE_ETUDE_FATTRI4 %>%
  filter(!is.na(NIVEDUC_I)) %>%
  group_by(AGE_RELATIF_I) %>%
  summarise(
    mode_niveduc = names(sort(table(NIVEDUC_I), decreasing = TRUE))[1],
    .groups = "drop")

View(mode_age)

# Imputation des NA
BASE_ETUDE_FATTRI4 <- BASE_ETUDE_FATTRI %>%
  left_join(mode_age, by = "AGE_RELATIF_I") %>%
  mutate(
    NIVEDUC_I = ifelse(is.na(NIVEDUC_I),
                       mode_niveduc,
                       as.character(NIVEDUC_I))
  ) %>%
  select(-mode_niveduc)

sum(is.na(BASE_ETUDE_FATTRI4$NIVEDUC_I)) 

# Nombre de menages par Nb personnes les constituants 
table_ind_men <- table(pdata$IDENT)
summary(table_ind_men)

table(table_ind_men)

V= pdata %>%
  group_by(IDENT) %>%
  summarise(nb_individus = n())

View(V)

pdata$poids_obs = NULL 

sum(BASE_ETUDE_FATTRI4$NIVEDUC_I=="Non dclaré")

# **** Modele de PANEL pour l'arbitrage *********************************************

########################################################################################################################
#                                                  MODELE D'ARBITRAGE
#             
#######################################################################################################################

# ****** RE logit pondere, avec effets temporels ***********************************************
install.packages(c("censReg","plm","dplyr","car","AER","mhurdle"), type="binary")

library(censReg)
library(plm)
library(dplyr)
library(car)

# ************ DIAGNOSTIC DE LA CENSURE 
summary(pdata$MMLA)
hist(pdata$MMLA, breaks = 50, main = "Distribution de MMLA")

# Part exacte a 0 (censure a gauche)
prop_zero <- mean(pdata$MMLA_I == 0, na.rm = TRUE)
cat("Part de MMLA = 0 :", round(prop_zero * 100, 1), "%\n")

# DECISION : Ici double censure 
# - prop_plafond negligeable (< 1-2%) :  Tobit simple (censure a gauche seulement)

#************ STRUCTURE DU PANEL (comme pour le logit)****************
pdim(pdata)

# *********** ETAPE 3 : TOBIT POOLE ***********************************
names(pdata)


library(survival)

tobit_pool <- censReg(
  MMLA_I ~ NIVEDUC_I + ETAMATRI + LEP + factor(ANNEE_ENQUETE),
  data  = pdata,
  left  = 0)

summary(tobit_pool)

tobit_pool <- survreg(
  Surv(MMLA_I, MMLA_I > 0, type = "left") ~
    NIVEDUC_I + ETAMATRI + LEP + factor(ANNEE_ENQUETE),
  data = pdata,
  weights = poids_norm,
  dist = "gaussian")

summary(tobit_pool)

#************ ETAPE 4 : TOBIT PANEL A EFFETS ALEATOIRES ****************
# censReg gere les effets aleatoires panel automatiquement si la donnee
# est un pdata.frame indexe (methode a la Butler & Moffitt, quadrature
# de Gauss-Hermite comme pour le RE logit/probit)

tobit_re <- censReg(
  MMLA_I ~ NIVEDUC_I + ETAMATRI + LEP + factor(ANNEE_ENQUETE),
  data   = pdata,
  left   = 0,
  right  = 22950,
  method = "BHHH"   # methode d'optimisation, ajuster si non-convergence
)
summary(tobit_re)

logLik(tobit_re)
logLik(tobit_pool)

# Test de la presence d'effets individuels (H0 : variance de alpha_i = 0)
# comparer log-vraisemblances pooled vs RE (test LR, frontiere du parametre)
lr_stat <- 2 * (logLik(tobit_re) - logLik(tobit_pool))
pchisq(as.numeric(lr_stat), df = 1, lower.tail = FALSE) / 2
# p < 0.05 -> effets individuels significatifs -> RE justifie face au pooled

# *** Modele de PANEL pooled ***********************************************
# Avec glm 
logit_pooled_ml <- glm(
  LIVABL ~ LDD + LEP + NPERS + ACTIF + ETAMATRI + CATAGE_RELATIF_I +
    NIVEDUC_I + log(PATRIRES) + CS8_I + log(ZREVDEC_I+1) +
    log(PATFISOM+1) + log(PATIMM) + ASDEC +
    log(PATRIPRO+1) + log(MTDETTES+1) + AVI +
    log(MAVI+1) + CEL + PEL + RETRAIT + EPSALA +
    VMOB + TERM + PEA + LIVJEUN + R_PATIMMO +
    R_RESPRIN + NBLOG + NE + DETPRO +
    HERTABH_I + HERTABD_I + CONSEP + RISKFI,
  data = pdata,
  weights = poids_norm,
  family = binomial(link = "logit")
)

# ****** Modèle pooled **************************************
cor(pdata$CEL, pdata$MCEL_I, use = "complete.obs")

logit_pooled <- svyglm(LIVABL~LDD+LEP+NPERS+ACTIF+ETAMATRI+CATAGE_RELATIF_I+NIVEDUC_I+
                         log(PATRIRES) +CS8_I+ log(ZREVDEC_I+1)  + log(PATFISOM+1) +log(PATIMM) + ASDEC+
                         log(PATRIPRO+1)+  log(MTDETTES +1) + AVI + log(MAVI+1) + CEL+PEL+RETRAIT+EPSALA
                       +VMOB+TERM+PEA+LIVJEUN+R_PATIMMO+R_RESPRIN+NBLOG+NE+DETPRO+HERTABH_I+
                         +HERTABD_I+CONSEP+RISKFI, 
                       design = design_pool,family = quasibinomial(link = "probit")) 

summary(logit_pooled)

#**** Estimation du modele RE ******
system.time({logit_re_essai <- glmmTMB(
  LIVABL~LDD+LEP+NPERS+ACTIF+ETAMATRI+CATAGE_RELATIF_I+NIVEDUC_I+
    #    log(PATRIRES) +CS8_I+ log(ZREVDEC_I+1)  + log(PATFISOM+1) +log(PATIMM) + ASDEC+
    #   log(PATRIPRO+1)+  log(MTDETTES +1) + AVI + log(MAVI+1) + CEL+PEL+RETRAIT+EPSALA
    #  +VMOB+TERM+PEA+LIVJEUN+R_PATIMMO+R_RESPRIN+NBLOG+NE+DETPRO+HERTABH_I+
    #   +HERTABD_I+CONSEP+RISKFI +
    (1 | IDENTINDL),
  data = pdata,
  weights = poids_norm,
  family = binomial(link = "logit"))})

summary(logit_re_essai)
        
# *** Multicolinéarité ************
vif(logit_pooled)

logit_pooled2 <- update(
  logit_pooled,
  . ~ . - R_PATIMMO)

vif(logit_pooled2)

# **** Choix entre le pooled et le FE *****
aic_manual <- function(model) {
  res <- residuals(model)
  n <- length(res)
  k <- length(coef(model)) + 1
  ll <- sum(dnorm(res, sd = sd(res), log = TRUE))
  -2 * ll + 2 * k
}

cat("AIC pooled :", aic_manual(pooled), "\n")
cat("AIC RE     :", aic_manual(re), "\n")

# ***************************************************************
glmmTMB:::summary.glmmTMB(logit_re_final_LEP)




























