library(dplyr)
library(tidyr)
library(ggplot2)
library(dplyr)
library(ggplot2)
library(plotly)
library(janitor)
library(Hmisc)

#************** Statistique descriptives *************************
#************** Reprendre les graphqiques en anglais *************
individu_t = as_tibble(individu)

#******** Age distibution  ***************************************
# Age relatif à la période de l'enquête  
individu_t$AGE_RELATIF=2021-individu_t$ANAIS

# Histogramme des âges 
hist(individu_t$AGE_RELATIF,xlab = "Age of Individuals in 2021",ylab="Frequency",main="Histogram of Age")


individu_t$CATAGE_RELATIF=NA

individu_t$CATAGE_RELATIF[individu_t$AGE_RELATIF <= 14] = 1                              #0 à 14 ans
individu_t$CATAGE_RELATIF[individu_t$AGE_RELATIF > 14 & individu_t$AGE_RELATIF <=29] = 2 #15 à 29 ans
individu_t$CATAGE_RELATIF[individu_t$AGE_RELATIF > 29 & individu_t$AGE_RELATIF <=44] = 3 #30 à 44 ans
individu_t$CATAGE_RELATIF[individu_t$AGE_RELATIF > 44 & individu_t$AGE_RELATIF <=59] = 4 #45 à 59 ans
individu_t$CATAGE_RELATIF[individu_t$AGE_RELATIF > 59 & individu_t$AGE_RELATIF <=74] = 5 #60 à 74 ans
individu_t$CATAGE_RELATIF[individu_t$AGE_RELATIF > 74] = 6                               #75 ans ou plus 

unique(individu_t$CATAGE_RELATIF)


# Calculate percentages
proportions <- prop.table(table(individu_t$CATAGE_RELATIF)) * 100

# Create the bar chart
bp <- barplot(
  proportions,
  names.arg = rep("", 6),
  col = "skyblue",
  xlab = "Age Group",
  ylab = "Percentage (%)",
  main = "Age Distribution (%)",
  ylim = c(0, max(proportions) * 1.15)
)

text(
  x = bp,
  y = par("usr")[3] - 1,
  labels = c("0-14 years", "15-29 years", "30-44 years",
             "45-59 years", "60-74 years", "75 years and +"),
  srt = 45,
  adj = 1,
  xpd = TRUE,
  cex = 0.6
)
# Add percentage labels above bars
text(
  x = bp,
  y = proportions + max(proportions) * 0.03,
  labels = paste0(round(proportions, 1), "%"),
  pos = 3,
  cex = 0.6,
  offset = 0.3)

# ******** Etat matrimoniale et Nombre d'enfant *******************************
individu_t$ETAMATRI <- as.numeric(individu_t$ETAMATRI)

individu_t$ETAMATRI <- factor(
  individu_t$ETAMATRI,
  levels = c(1, 2, 3, 4),
  labels = c(
    "Single",
    "Married/Remarried",
    "Widowed",
    "Divorced"))

#Avec le filtre age >=18
sit_mat=individu_t%>%
  filter(AGE_RELATIF>=18) %>% 
  count(ETAMATRI) %>% 
  mutate(proportion=round((n/sum(n))*100,1))

View(sit_mat)

# Quantile 

V <- with(
  menage,
  wtd.quantile(
    NENFANTS,
    weights = POND_TRANS,
    probs = c(0.01, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99)
  )
)

tab_quantiles <- data.frame(
  Quantile = c("P1", "P5", "P10", "Q1", "Median", "Q3", "P90", "P95", "P99"),
  Value = as.numeric(V)
)

View(tab_quantiles)

# ****************** Niveducation des personnes ******************************************
individu_t_18$NIVEDUC=NA

individu_t_18$NIVEDUC[individu_t_18$DIPDET %in% c(00,70,71,"")] = 1       # Sans diplome ou CEP
individu_t_18$NIVEDUC[individu_t_18$DIPDET %in% c(45,51,52,53,60)] = 2    # brevet
individu_t_18$NIVEDUC[individu_t_18$DIPDET %in% c(41,42,43,44)] = 3       # bac ou bac+1
individu_t_18$NIVEDUC[individu_t_18$DIPDET %in% c(31,32,33,34,35,36)] = 4 # bac+2
individu_t_18$NIVEDUC[individu_t_18$DIPDET %in% c(22,23,24)] = 5          # bac+3
individu_t_18$NIVEDUC[individu_t_18$DIPDET %in% c(21)] = 6                # bac+4 
individu_t_18$NIVEDUC[individu_t_18$DIPDET %in% c(13,14,15,16,17)] = 7    # bac+5
individu_t_18$NIVEDUC[individu_t_18$DIPDET %in% c(11,12)] = 8             # bac+8

unique(individu_t_18$NIVEDUC)

individu_t_18$NIVEDUC <- factor(
  individu_t_18$NIVEDUC,
  levels = c(1, 2, 3, 4, 5, 6, 7, 8),
  labels = c(
    "No diploma or CEP",
    "brevet",
    "bac or bac+1",
    "bac+2",
    "bac+3",
    "bac+4",
    "bac+5",
    "bac+8"))

# Camembert
Niveau_educ <- individu_t_18 %>%
  count(NIVEDUC) %>%  # occurrences par niveau
  mutate(
    pourcentage = n / sum(n) * 100,  #percent
    pourcentage = round(pourcentage, 1)) 

Niveau_educ

# Visualization
ggplot(Niveau_educ, aes(x = NIVEDUC, y = n)) +
  geom_text(
    aes(label = paste0(pourcentage, "%")),
    hjust = -0.1,
    size = 2.0
  ) +
  geom_bar(
    stat = "identity",
    fill = "skyblue"
  ) +
  labs(
    title = "Distribution of Individuals by Education Level",
    x = "Education Level",
    y = "Number of Individuals"
  ) +
  theme_minimal() +
  coord_flip() +  # Flip the bars
  ylim(0, max(Niveau_educ$n) * 1.1)

#
# Calculate percentages
proportions <- prop.table(table(individu_t_18$NIVEDUC)) * 100

# Calcul des pourcentages
proportions <- prop.table(table(individu_t_18$NIVEDUC)) * 100

# Augmenter la marge du bas pour les libellés longs
par(mar = c(8, 4, 3, 1))

# Création du graphique
bp <- barplot(
  proportions,
  col = "white",
  xlab = "Education Level",
  ylab = "Percentage (%)",
  main = "Distribution of Individuals by Education Level",
  ylim = c(0, max(proportions) * 1.15),
  cex.names = 0.45,  # taille des libellés des catégories
  cex.lab = 0.8,     # taille des titres des axes
  cex.main = 0.9,    # taille du titre principal
  las = 2            # libellés verticaux
)

# Ajout des pourcentages au-dessus des barres
text(
  x = bp,
  y = proportions + max(proportions) * 0.03,
  labels = paste0(round(proportions, 1), "%"),
  pos = 3,
  cex = 0.6,         # taille des pourcentages
  offset = 0.5
)

# *************  Catégorie socioprofessionnelle ***************************
# Catégorie socio professionnelle 
individu_t$CS8=NA

individu_t$CS8[individu_t$CS24 %in% c(10)] = 1 #Agriculteurs exploitants 
individu_t$CS8[individu_t$CS24 %in% c(21,22,23)] = 2 #Artisans, commerçants et chefs d'entreprise
individu_t$CS8[individu_t$CS24 %in% c(31,32,36)] = 3 #Cadres et professions intellectuelles supérieures
individu_t$CS8[individu_t$CS24 %in% c(41,46,47,48)] = 4 #Professions Intermédiaires
individu_t$CS8[individu_t$CS24 %in% c(51,54,55,56)] = 5 #Employés
individu_t$CS8[individu_t$CS24 %in% c(61,66,69)] = 6 #Ouvriers
individu_t$CS8[individu_t$CS24 %in% c(71,72,73,76)] = 7 #Retraités
individu_t$CS8[individu_t$CS24 %in% c("00",81,82,"")] = 8 #Autres personnes sans activité professionnelle

# Vérifier le taux de manquants
mean(is.na(individu_t$CS8))

# Label for the CS8 variable
individu_t$CS8L <- factor(
  individu_t$CS8,
  levels = c(1, 2, 3, 4, 5, 6, 7, 8),
  labels = c(
    "Farmers",
    "Self-employed, Traders and Business Owners",
    "Managers and Higher Intellectual Professions",
    "Intermediate Occupations",
    "Employees",
    "Manual Workers",
    "Retirees",
    "Other Economically Inactive Individuals"
  )
)

# Distribution by socio-professional category
Repart_Cat_Pro <- individu_t %>%
  filter(AGE_RELATIF >= 15 & AGE_RELATIF <= 64) %>%  # Working-age population
  count(CS8L) %>%                                   # Number of individuals by category
  mutate(
    pourcent = n / sum(n) * 100,                    # Percentage
    pourcent = round(pourcent, 1)
  )

View(Repart_Cat_Pro)

# Part dans le patrimoine 
prop_actuelle <- mean(menage$PATRI_BRUT > 0) * 100  # 99.94%
prop_actuelle


Part_pat_immo <- 100 * sum(menage$PATIMM * menage$POND_TRANS, na.rm = TRUE) /
  sum(menage$PATRI_BRUT * menage$POND_TRANS, na.rm = TRUE)
Part_pat_immo

Part_pat_fin <- 100 * sum(menage$PATFISOM * menage$POND_TRANS, na.rm = TRUE)/
  sum(menage$PATRI_BRUT * menage$POND_TRANS, na.rm = TRUE)
Part_pat_fin

Part_pat_pro <- 100*sum((menage$PATPROFENT+menage$PATPROFHENT)*menage$POND_TRANS, na.rm=TRUE)/
  sum(menage$PATRI_BRUT * menage$POND_TRANS, na.rm = TRUE)
Part_pat_pro

# voitures, autres véhicules, biens d’équipements / bijoux /oeuvres d’art et autres objets de valeur
Part_pat_res <- 100*sum(menage$MTRESTE*menage$POND_TRANS, na.rm=TRUE)/
  sum(menage$PATRI_BRUT * menage$POND_TRANS, na.rm = TRUE)
Part_pat_res


# VISUALIZATION
data <- data.frame(
  categorie = c(
    "Residual Wealth",
    "Professional Wealth",
    "Financial Wealth",
    "Real Estate Wealth"
  ),
  proportion = c(
    round(Part_pat_res, 1),    # Example: 45
    round(Part_pat_pro, 1),    # Example: 10
    round(Part_pat_fin, 1),    # Example: 25
    round(Part_pat_immo, 1)    # Example: 20
  )
)

# Check that the total equals 100%
sum(data$proportion)  # Should be equal to 100, which is the case

# Plot
ggplot(data, aes(x = "", y = proportion, fill = categorie)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(
    aes(label = paste0(proportion, "%")),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 4
  ) +
  labs(title = "Distribution of Gross Household Wealth") +
  theme_void() +  # Remove axes and background
  theme(legend.position = "right")  # Place legend on the right

# Plot
ggplot(data, aes(x = "", y = proportion, fill = categorie)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(
    aes(label = paste0(proportion, "%")),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 2.5
  ) +
  scale_fill_manual(values = c(
    "skyblue",
    "lightblue",
    "steelblue1",
    "grey80"
  )) +
  labs(title = "Distribution of Gross Household Wealth") +
  theme_void() +
  theme(legend.position = "right")

# ****************************************************************
ggplot(data, aes(x = "", y = proportion, fill = categorie)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(
    aes(label = paste0(proportion, "%")),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 2.5
  ) +
  scale_fill_manual(values = c(
    "#0072CE",
    "#5B9BD5",
    "#A7C7E7",
    "#D9EAF7"
  )) +
  labs(title = "Distribution of Gross Household Wealth") +
  theme_void() +
  theme(legend.position = "right")

# ****************************************************************

# Lorenz Curve 
# Prepare data for the Lorenz curve
lorenz_data <- menage %>%
  arrange(PATRI_BRUT) %>%
  mutate(
    cum_pop = cumsum(POND_TRANS) / sum(POND_TRANS),
    cum_wealth = cumsum(PATRI_BRUT * POND_TRANS) / sum(PATRI_BRUT * POND_TRANS)
  )

ggplot(lorenz_data, aes(x = cum_pop, y = cum_wealth)) +
  geom_line(color = "#1f77b4", linewidth = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  geom_vline(xintercept = 0.5, linetype = "dotted", color = "red") +
  geom_hline(
    yintercept = lorenz_data$cum_wealth[
      which.min(abs(lorenz_data$cum_pop - 0.5))
    ],
    linetype = "dotted",
    color = "red"
  ) +
  labs(
    title = "Lorenz Curve of Household Wealth",
    x = "Cumulative Share of the Population",
    y = "Cumulative Share of Wealth"
  ) +
  annotate(
    "text",
    x = 0.5,
    y = 0.1,
    label = paste0(
      "The bottom 50% own ",
      round(
        100 * lorenz_data$cum_wealth[
          which.min(abs(lorenz_data$cum_pop - 0.5))
        ],
        1
      ),
      "% of total wealth"
    ),
    color = "red"
  ) +
  theme_minimal()

# The right one 
# Lorenz curves by wealth component

lorenz_data <- menage %>%
  # Compute the Lorenz curve for each wealth component
  pivot_longer(
    cols = c(PATFISOM, PATIMM, MTRESTE,
             PATPROFHENT, PATPROFENT,
             MTDETPRIV, MTDETPROFHENT, MTDETPROFENT),
    names_to = "component",
    values_to = "value"
  ) %>%
  mutate(
    # Group professional assets and debts
    component = case_when(
      component %in% c("PATPROFHENT", "PATPROFENT") ~ "Professional Wealth",
      component %in% c("MTDETPRIV", "MTDETPROFHENT", "MTDETPROFENT") ~ "Debt",
      TRUE ~ component
    ),
    
    # Treat debt as negative values
    value = ifelse(component == "Debt", -value, value)
  ) %>%
  group_by(component) %>%
  
  # Compute weighted cumulative shares
  arrange(value) %>%
  mutate(
    cum_pop = cumsum(POND_TRANS) / sum(POND_TRANS),
    cum_value = cumsum(value * POND_TRANS) / sum(value * POND_TRANS)
  ) %>%
  ungroup() %>%
  distinct(component, cum_pop, cum_value) %>%
  mutate(
    # Rename components for display
    component = case_when(
      component == "PATFISOM" ~ "Financial Wealth",
      component == "PATIMM" ~ "Real Estate Wealth",
      component == "MTRESTE" ~ "Residual Wealth",
      component == "Professional Wealth" ~ "Professional Wealth",
      component == "Debt" ~ "Debt"
    )
  )

# Share owned by the bottom 50%
shares_50 <- lorenz_data %>%
  group_by(component) %>%
  filter(cum_pop <= 0.5) %>%
  summarise(
    share_50 = last(cum_value)
  ) %>%
  ungroup()

# Plot
l <- ggplot(lorenz_data,
            aes(x = cum_pop,
                y = cum_value,
                color = component)) +
  geom_line(size = 1) +
  geom_abline(intercept = 0,
              slope = 1,
              linetype = "dashed",
              color = "grey50") +
  geom_vline(xintercept = 0.5,
             linetype = "dotted",
             color = "grey60") +
  geom_hline(data = shares_50,
             aes(yintercept = share_50),
             linetype = "dotted",
             color = "grey60") +
  
  labs(
    title = "Lorenz Curves by Wealth Component (Weighted)",
    x = "Cumulative Share of the Population",
    y = "Cumulative Share of Wealth Component",
    color = "Component"
  ) +
  
  scale_color_manual(values = c(
    "Financial Wealth"   = "#4F81BD",
    "Real Estate Wealth" = "#1F4E79",
    "Residual Wealth"    = "#9DC3E6",
    "Professional Wealth"= "#6C8EBF",
    "Debt"               = "#B0B0B0"
  )) +
  
  theme_minimal() +
  theme(
    legend.position = "bottom"
  ) +
  
  # Labels for the bottom 50%
  geom_text(
    data = shares_50,
    aes(x = 0.5,
        y = share_50,
        label = paste0(round(100 * share_50, 1), "%")),
    vjust = -1,
    hjust = -0.1,
    color = "black",
    size = 3
  ) +
  
  annotate(
    "text",
    x = 0.5,
    y = -0.05,
    label = "50% of the population",
    angle = 90,
    vjust = 0,
    hjust = 0,
    color = "grey50"
  )

ggplotly(l)


# Another one (The Good one) 
# Lorenz curves by wealth component

lorenz_data <- menage %>%
  
  # Compute the Lorenz curve for each wealth component
  pivot_longer(
    cols = c(
      PATFISOM, PATIMM, MTRESTE,
      PATPROFHENT, PATPROFENT,
      MTDETPRIV, MTDETPROFHENT, MTDETPROFENT
    ),
    names_to = "component",
    values_to = "value"
  ) %>%
  
  mutate(
    
    # Group professional assets and debts
    component = case_when(
      component %in% c("PATPROFHENT", "PATPROFENT") ~ "Professional Wealth",
      component %in% c("MTDETPRIV", "MTDETPROFHENT", "MTDETPROFENT") ~ "Debt",
      TRUE ~ component
    ),
    
    # Treat debt as negative values
    value = ifelse(component == "Debt", -value, value)
  ) %>%
  
  group_by(component) %>%
  
  # Compute weighted cumulative shares
  arrange(value) %>%
  
  mutate(
    cum_pop = cumsum(POND_TRANS) / sum(POND_TRANS),
    cum_value = cumsum(value * POND_TRANS) /
      sum(value * POND_TRANS)
  ) %>%
  
  ungroup() %>%
  
  distinct(component, cum_pop, cum_value) %>%
  
  mutate(
    
    # Rename components for display
    component = case_when(
      component == "PATFISOM" ~ "Financial Wealth",
      component == "PATIMM" ~ "Real Estate Wealth",
      component == "MTRESTE" ~ "Residual Wealth",
      component == "Professional Wealth" ~ "Professional Wealth",
      component == "Debt" ~ "Debt"
    )
  )

# Share held by the bottom 50%

shares_50 <- lorenz_data %>%
  group_by(component) %>%
  filter(cum_pop <= 0.5) %>%
  summarise(
    share_50 = last(cum_value)
  ) %>%
  ungroup()

# Plot

l <- ggplot(
  lorenz_data,
  aes(
    x = cum_pop,
    y = cum_value,
    color = component
  )
) +
  
  geom_line(linewidth = 1.2) +
  
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dashed",
    color = "grey60"
  ) +
  
  geom_vline(
    xintercept = 0.5,
    linetype = "dotted",
    color = "grey60"
  ) +
  
  geom_hline(
    data = shares_50,
    aes(yintercept = share_50),
    linetype = "dotted",
    color = "grey60"
  ) +
  
  labs(
    title = "Weighted Lorenz Curves by Wealth Component",
    x = "Cumulative Share of the Population",
    y = "Cumulative Share of Wealth Component",
    color = "Component"
  ) +
  
  scale_color_manual(
    values = c(
      "Financial Wealth"    = "#0072CE",  # CDC blue
      "Real Estate Wealth"  = "#003B70",  # dark blue
      "Residual Wealth"     = "#7FB3D5",  # light blue
      "Professional Wealth" = "#E30613",  # CDC red
      "Debt"                = "#A6A6A6"   # grey
    )
  ) +
  
  theme_minimal() +
  
  theme(
    legend.position = "bottom",
    plot.title = element_text(
      hjust = 0.5,
      face = "bold"
    ),
    legend.title = element_text(face = "bold")
  ) +
  
  # Labels for the bottom 50%
  geom_text(
    data = shares_50,
    aes(
      x = 0.5,
      y = share_50,
      label = paste0(round(100 * share_50, 1), "%")
    ),
    vjust = -1,
    hjust = -0.1,
    color = "black",
    size = 3
  ) +
  
  annotate(
    "text",
    x = 0.5,
    y = -0.05,
    label = "Bottom 50% of the population",
    angle = 90,
    color = "grey50"
  )

ggplotly(l)


#* 2* Description du menage 
menage$ZREVDISP # Revenu disponible 
menage$ZNIVVIE # Niveau de vie 
menage$PATRI_NET # Patrimoine Net 
menage$PATFISOM # Patrimoine financier 
menage$PATIMM # Patrimoine Immobilier 
menage$MTRESTE # Patrimoine résiduel
menage$PATPROFHENT + menage$PATPROFENT # Patrimoine professionnel
menage$MTDETTES # Montant de la dette 

install.packages("moments")

library(dplyr)
library(survey)
library(Hmisc)
library(moments)

# Création de la variable patrimoine professionnel
menage <- menage %>%
  mutate(PATPROF = PATPROFHENT + PATPROFENT)

# Variables à résumer
vars <- c(
  "ZREVDISP",    # Revenu disponible
  "ZNIVVIE",     # Niveau de vie
  "PATRI_NET",   # Patrimoine net
  "PATFISOM",    # Patrimoine financier
  "PATIMM",      # Patrimoine immobilier
  "MTRESTE",     # Patrimoine résiduel
  "PATPROF",     # Patrimoine professionnel
  "MTDETTES"     # Dette
)

# Plan de sondage avec pondérations
des <- svydesign(ids = ~1, weights = ~POND_TRANS, data = menage)

# Fonction de calcul des statistiques pondérées
stats_pond <- function(x, w) {
  
  x <- x[!is.na(x) & !is.na(w)]
  w <- w[!is.na(x) & !is.na(w)]
  
  c(
    Moyenne = round(weighted.mean(x, w), 0),
    Mediane = round(wtd.quantile(x, weights = w, probs = 0.50), 0),
    Q1 = round(wtd.quantile(x, weights = w, probs = 0.25), 0),
    Q3 = round(wtd.quantile(x, weights = w, probs = 0.75), 0),
    D1 = round(wtd.quantile(x, weights = w, probs = 0.10), 0),
    D9 = round(wtd.quantile(x, weights = w, probs = 0.90), 0),
    STD = round(sqrt(weighted.mean((x - weighted.mean(x, w))^2, w)), 0),
    Skewness = round(skewness(x), 0),
    Kurtosis = round(kurtosis(x), 0)
  )
}

# Construction du tableau
tableau_stats <- do.call(
  rbind,
  lapply(vars, function(v) {
    stats_pond(menage[[v]], menage$POND_TRANS)
  })
)

tableau_stats <- data.frame(
  Variable = vars,
  tableau_stats,
  row.names = NULL
)

print(tableau_stats)

View(tableau_stats)

#**************  Inégalité ***********************************
# Décile (50% les plus detenteurs)
decile_patri_net=round(with(menage, wtd.quantile(menage$PATRI_NET, weights = menage$POND_TRANS, probs = (1:9)/10), 
                             type = 'quantile'),0) #Calcul du décile des revenus
decile_patri_net

seuil_D9 <- decile_patri_net[5]
top10_mask <- menage$PATRI_NET >= seuil_D9

part_top10 <- round(100*sum(menage$PATRI_NET[top10_mask] * menage$POND_TRANS[top10_mask],  # Multiplication des vecteurs filtrés
                            na.rm = TRUE) / sum(menage$PATRI_NET * menage$POND_TRANS,na.rm = TRUE),0)

part_top10

# Décile (50% les moins dotés en patrimoine)
seuil_D1 <- decile_patri_net[5]
Bottom10_mask <- menage$PATRI_NET <= seuil_D1

part_bottom10 <- round(100*sum(menage$PATRI_NET[Bottom10_mask] * menage$POND_TRANS[Bottom10_mask],  # Multiplication des vecteurs filtrés
                               na.rm = TRUE) / sum(menage$PATRI_NET * menage$POND_TRANS,na.rm = TRUE),2)

part_bottom10

View(BASE_ETUDE_FATTRI)

#********* Statistique sur la transmission *************************************************** 
View(menage)

menage$POND_TRANS
transmission_t$POND_TRANS

# Ramener les poids dans transmission 
transmission_t = transmission

transmission_t <- transmission_t %>%
  left_join(menage %>% select(IDENT, POND_TRANS), by = "IDENT")

mean(is.na(transmission_t$POND_TRANS)) * 100 

men_heritier = transmission %>%
  filter(!(MTHERC == 0 | is.na(MTHERC))) %>%  # Garde les lignes où MTHERC ≠ 0 et ≠ NA
  summarise(n_unique = n_distinct(IDENT))     # Compte les ID uniques

men_heritier <- transmission_t %>%
  filter(!is.na(MTHERC) & MTHERC != 0) %>%
  distinct(IDENT, POND_TRANS) %>%
  summarise(effectif_pondere = sum(POND_TRANS, na.rm = TRUE))

men_heritier/nrow(transmission_t)
#Prop 
Prop=round(men_heritier/sum(menage$POND_TRANS)*100,1)   
Prop

# Montant moyen hérité (Recalculé avec le poids des ménages)
transmission %>%
  filter(!(MTHERC == 0 | is.na(MTHERC))) %>%    
  reframe(Montant_moyen_herite = weighted.mean(MTHERC))

View(transmission)

# Distribution des montants hérité (Actualisé avec les ponderations)
transmission_tb %>%
  filter(!(MTHERC == 0 | is.na(MTHERC))) %>%    
  reframe(Quantile_percent = c(1, 5, 10, 25, 50, 75, 90, 95, 99),
          Quantile_heritage = quantile(MTHERC,
                                       probs = c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.90, 0.95, 0.99),na.rm = TRUE))


#Proportion de menage ayant reçu un heritage (Actualisé avec le poids)
men_recevant_don = transmission_tb %>%
  filter(!(MTDONVC == 0 | is.na(MTDONVC))) %>%  # Garde les lignes où MTDONVC ≠ 0 et ≠ NA
  summarise(n_unique = n_distinct(IDENT))     # Compte les ID uniques

#Prop 
Prop_don=(men_recevant_don/10253)*100  
Prop_don


# Moyen reçu en donation 
transmission_tb %>%
  filter(!(MTDONVC == 0 | is.na(MTDONVC))) %>%    
  reframe(Montant_moyen_herite = mean(MTDONVC))

# Raprtition du montant hérité
# Quantile 
transmission_tb %>%
  filter(!(MTDONVC == 0 | is.na(MTDONVC))) %>%    
  reframe(Quantile_percent = c(1, 5, 10, 25, 50, 75, 90, 95, 99),Quantile_don = quantile(MTDONVC,
                                                                                         probs = c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.90, 0.95, 0.99),na.rm = TRUE))

# Les taux de detentions des produits financiers 
# Compte Courant
# Assurance Vie 
# Plan d'Epargne Entreprise 
# Plan d'Epargne retraite 
# Les valeurs mobilières 

# Individu
Taux_det_CC <- individu_t %>%
  count(CCQ) %>%  # occurrences par niveau
  mutate(pourcent = n / sum(n) * 100,  #percent
         pourcent = round(pourcent, 1)) 

Taux_det_CC

# Avec la pondération des ménages
Taux_det_CC_Men_p <- menage_tb %>%
  group_by(R_CCQ) %>%
  summarise(n = sum(POND_TRANS),pourcent = sum(POND_TRANS)/sum(menage_tb$POND_TRANS) * 100,
            pourcent = round(pourcent,1))

Taux_det_CC_Men_p

#**************** Montant moyen compte chèque ****************************
# Sous-échantillon Compte courant
cc <- produit_t %>%
  filter(FINNA == "01")

# Tableau de synthèse
Table_CC <- tibble(
  Produit = "Compte courant",
  
  # Taux de détention ménage pondéré
  Taux_detention_menage_pondere =
    menage %>%
    summarise(
      taux = 100 * sum(POND_TRANS[R_CCQ == 1], na.rm = TRUE) /
        sum(POND_TRANS, na.rm = TRUE)
    ) %>%
    pull(taux),
  
  # Taux de détention individu
  Taux_detention_individu =
    individu %>%
    summarise(
      taux = 100 * mean(CCQ == 1, na.rm = TRUE)
    ) %>%
    pull(taux),
  
  # Statistiques sur les montants détenus
  Montant_moyen  = mean(cc$MONTCLA, na.rm = TRUE),
  Montant_median = median(cc$MONTCLA, na.rm = TRUE),
  
  Q1 = quantile(cc$MONTCLA, 0.25, na.rm = TRUE),
  Q3 = quantile(cc$MONTCLA, 0.75, na.rm = TRUE),
  
  D1 = quantile(cc$MONTCLA, 0.10, na.rm = TRUE),
  D9 = quantile(cc$MONTCLA, 0.90, na.rm = TRUE)
) %>%
  mutate(across(where(is.numeric), round, 1))

Table_CC

#**************** Assurance Vie ******************************************
# Sous-échantillon Assurance-vie
av <- produit_t %>%
  filter(FINNA == "20")

Table_AV <- tibble(
  Produit = "Assurance-vie",
  
  # Taux de détention ménage pondéré
  Taux_detention_menage_pondere =
    menage %>%
    summarise(
      taux = 100 * sum(POND_TRANS[R_AVI == 1], na.rm = TRUE) /
        sum(POND_TRANS, na.rm = TRUE)
    ) %>%
    pull(taux),
  
  # Taux de détention individu
  Taux_detention_individu =
    individu %>%
    summarise(
      taux = 100 * mean(AVI == "1", na.rm = TRUE)
    ) %>%
    pull(taux),
  
  # Montants
  Montant_moyen  = mean(av$MONTCLA, na.rm = TRUE),
  Montant_median = median(av$MONTCLA, na.rm = TRUE),
  
  # Quartiles
  Q1 = quantile(av$MONTCLA, 0.25, na.rm = TRUE),
  Q3 = quantile(av$MONTCLA, 0.75, na.rm = TRUE),
  
  # Déciles
  D1 = quantile(av$MONTCLA, 0.10, na.rm = TRUE),
  D9 = quantile(av$MONTCLA, 0.90, na.rm = TRUE)
) %>%
  mutate(across(where(is.numeric), ~ round(.x, 1)))

Table_AV

# ******** Plan d'Epargne Entreprise **************************************************
individu$AGE_RELATIF = 2021 - individu$ANAIS

# Sous-échantillon PEE
pee <- produit %>%
  filter(FINNA == "10")

Table_PEE <- tibble(
  Produit = "PEE / Épargne salariale",
  
  # Taux de détention ménage pondéré
  Taux_detention_menage_pondere =
    menage %>%
    summarise(
      taux = 100 * sum(POND_TRANS[R_EPSALA == "1"], na.rm = TRUE) /
        sum(POND_TRANS, na.rm = TRUE)
    ) %>%
    pull(taux),
  
  # Taux de détention individu (15-64 ans)
  Taux_detention_individu =
    individu %>%
    filter(AGE_RELATIF >= 15, AGE_RELATIF <= 64) %>%
    summarise(
      taux = 100 * mean(EPSALA == "1", na.rm = TRUE)
    ) %>%
    pull(taux),
  
  # Statistiques de montant
  Montant_moyen = mean(pee$MONTCLA, na.rm = TRUE),
  Montant_median = median(pee$MONTCLA, na.rm = TRUE),
  
  # Quartiles
  Q1 = quantile(pee$MONTCLA, 0.25, na.rm = TRUE),
  Q3 = quantile(pee$MONTCLA, 0.75, na.rm = TRUE),
  
  # Déciles
  D1 = quantile(pee$MONTCLA, 0.10, na.rm = TRUE),
  D9 = quantile(pee$MONTCLA, 0.90, na.rm = TRUE)
) %>%
  mutate(across(where(is.numeric), ~ round(.x, 1)))

Table_PEE

# ********** Plan d'Epargne retraite ******************************************
# CSP
individu$CS8=NA

individu$CS8[individu$CS24 %in% c(10)] = 1 #Agriculteurs exploitants 
individu$CS8[individu$CS24 %in% c(21,22,23)] = 2 #Artisans, commerçants et chefs d'entreprise
individu$CS8[individu$CS24 %in% c(31,32,36)] = 3 #Cadres et professions intellectuelles supérieures
individu$CS8[individu$CS24 %in% c(41,46,47,48)] = 4 #Professions Intermédiaires
individu$CS8[individu$CS24 %in% c(51,54,55,56)] = 5 #Employés
individu$CS8[individu$CS24 %in% c(61,66,69)] = 6 #Ouvriers
individu$CS8[individu$CS24 %in% c(71,72,73,76)] = 7 #Retraités
individu$CS8[individu$CS24 %in% c("00",81,82,"")] = 8 #Autres personnes sans activité professionnelle

# Vérifier le taux de manquants
mean(is.na(individu$CS8))

# Labél de la variable CS8 
individu$CS8L <- factor(
  individu$CS8,levels = c(1, 2, 3, 4,5,6,7,8),
  labels = c("Agriculteurs exploitants","Artisans, commerçants et chefs d'entreprise",
             "Cadres et professions intellectuelles supérieures","Professions Intermédiaires",
             "Employés","Ouvriers","Retraités","Autres personnes sans activité professionnelle"))


# Sous-échantillon Épargne retraite
retraite <- produit %>%
  filter(FINNA %in% c("16", "17", "18", "19"))

Table_Retraite <- tibble(
  Produit = "Épargne retraite",
  
  # Taux de détention ménage pondéré
  Taux_detention_menage_pondere =
    menage %>%
    summarise(
      taux = 100 * sum(POND_TRANS[R_PERP == "1"], na.rm = TRUE) /
        sum(POND_TRANS, na.rm = TRUE)
    ) %>%
    pull(taux),
  
  # Taux de détention individu
  Taux_detention_individu =
    individu %>%
    filter(AGE_RELATIF <= 64 | !(CS8L == "Retraités")) %>%
    summarise(
      taux = 100 * mean(PEPAVI == "1", na.rm = TRUE)
    ) %>%
    pull(taux),
  
  # Montants
  Montant_moyen = mean(retraite$MONTCLA, na.rm = TRUE),
  Montant_median = median(retraite$MONTCLA, na.rm = TRUE),
  
  # Quartiles
  Q1 = quantile(retraite$MONTCLA, 0.25, na.rm = TRUE),
  Q3 = quantile(retraite$MONTCLA, 0.75, na.rm = TRUE),
  
  # Déciles
  D1 = quantile(retraite$MONTCLA, 0.10, na.rm = TRUE),
  D9 = quantile(retraite$MONTCLA, 0.90, na.rm = TRUE)
) %>%
  mutate(across(where(is.numeric), ~ round(.x, 1)))

Table_Retraite

# ********** Les valeurs mobilières ********************************
# Sous-échantillon Valeurs mobilières
vm <- produit %>%
  filter(FINNA %in% c("11","12","13","14","22","23","24","25","26","27","28","29","30"))

Table_VM <- tibble(
  Produit = "Valeurs mobilières",
  
  # Taux de détention ménage pondéré
  Taux_detention_menage_pondere =
    menage %>%
    summarise(
      taux = 100 * sum(POND_TRANS[R_VMOB == "1"], na.rm = TRUE) /
        sum(POND_TRANS, na.rm = TRUE)
    ) %>%
    pull(taux),
  
  # Taux de détention individu
  Taux_detention_individu =
    individu %>%
    summarise(
      taux = 100 * mean(VMOB == "1", na.rm = TRUE)
    ) %>%
    pull(taux),
  
  # Montants
  Montant_moyen = mean(vm$MONTCLA, na.rm = TRUE),
  Montant_median = median(vm$MONTCLA, na.rm = TRUE),
  
  # Quartiles
  Q1 = quantile(vm$MONTCLA, 0.25, na.rm = TRUE),
  Q3 = quantile(vm$MONTCLA, 0.75, na.rm = TRUE),
  
  # Déciles
  D1 = quantile(vm$MONTCLA, 0.10, na.rm = TRUE),
  D9 = quantile(vm$MONTCLA, 0.90, na.rm = TRUE)
) %>%
  mutate(across(where(is.numeric), ~ round(.x, 1)))

Table_VM

# ********* Tableau recapitulatif ******************************************************************
Table_finale <- bind_rows(
  Table_CC,
  Table_AV,
  Table_PEE,
  Table_Retraite,
  Table_VM)

View(Table_finale)

# ********* Distribution des encours de livrets et nombres livrets **********************************
# Définition des seuils LA
produit_t = as_tibble(produit)

# Intervalle 
produit_t$SLA <- 0
produit_t$SLA[produit_t$FINNA %in% c("02","03") & produit_t$MONTCLA < 150] <- "<150€"
produit_t$SLA[produit_t$FINNA %in% c("02","03") & produit_t$MONTCLA >= 150 & produit_t$MONTCLA < 750] <- "150-750€"
produit_t$SLA[produit_t$FINNA %in% c("02","03") & produit_t$MONTCLA >= 750 & produit_t$MONTCLA < 1500] <- "750-1500€"
produit_t$SLA[produit_t$FINNA %in% c("02","03") & produit_t$MONTCLA >= 1500 & produit_t$MONTCLA < 3000] <- "1500-3000€"
produit_t$SLA[produit_t$FINNA %in% c("02","03") & produit_t$MONTCLA >= 3000 & produit_t$MONTCLA < 7500] <- "3000-7500€"
produit_t$SLA[produit_t$FINNA %in% c("02","03") & produit_t$MONTCLA >= 7500 & produit_t$MONTCLA < 15300] <- "7500-15300€"
produit_t$SLA[produit_t$FINNA %in% c("02","03") & produit_t$MONTCLA >= 15300 & produit_t$MONTCLA < 19125] <- "15300-19125€"
produit_t$SLA[produit_t$FINNA %in% c("02","03") & produit_t$MONTCLA >= 19125 & produit_t$MONTCLA < 22950] <- "19125-22950€"
produit_t$SLA[produit_t$FINNA %in% c("02","03") & produit_t$MONTCLA >= 22950] <- ">=22950€"

# Encours et nombre total de livret A
Total_LA = produit_t %>%
  filter(FINNA %in% c("02","03")) %>%     #LA LBLEU
  summarise(Nombre_de_livret = sum(POND_TRANS, na.rm = TRUE),Encours_total=sum(POND_TRANS*MONTCLA))

Total_LA

# Encours et nombre de livret par catégorie
Livret_A =produit_t %>% 
  filter(FINNA %in% c("02","03")) %>% 
  group_by(SLA) %>% 
  summarise(nb_livret=round(sum(POND_TRANS,na.rm = TRUE)/1000000,2),
            percent_livret = round(sum(POND_TRANS,na.rm = TRUE)/Total_LA$Nombre_de_livret * 100,1),
            encours=round(sum(POND_TRANS*MONTCLA)/1000000000,2),
            percent_encours=round(sum(POND_TRANS*MONTCLA)/Total_LA$Encours_total*100,1)) %>% adorn_totals("row")  

Livret_A

View(produit_t)
# Garder cet ordre d'affichage pour les intervalles 
niveaux_SLA <- c("<150€","150-750€","750-1500€","1500-3000€","3000-7500€","7500-15300€","15300-19125€",
  "19125-22950€",">=22950€")

#********** Nombre de Livret A ***********************************************
ggplot(
  data = Livret_A %>% filter(SLA %in% niveaux_SLA),
  aes(x = SLA, y = percent_livret)
) +
  geom_bar(stat = "identity", fill = "#2C7FB8", width = 0.7) +
  geom_text(
    aes(label = paste0(
      format(percent_livret, big.mark = " ", nsmall = 0, decimal.mark = "."),
      "%"
    )),
    vjust = -0.5,
    size = 3.5
  ) +
  labs(
    title = NULL,
    x = "Livret A Threshold (€)",
    y = "Share of Accounts (%)"
  ) +
  theme_minimal() +
  scale_x_discrete(limits = niveaux_SLA) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1))

# ************ Encours LA **********************************
ggplot(
  data = Livret_A %>% filter(SLA %in% niveaux_SLA),
  aes(x = SLA, y = percent_encours)
) +
  geom_bar(stat = "identity", fill = "#DA291C", width = 0.7) +
  geom_text(
    aes(label = paste0(
      format(percent_encours, big.mark = " ", nsmall = 0, decimal.mark = "."),
      "%"
    )),
    vjust = -0.5,
    size = 3.5
  ) +
  labs(
    title = NULL,
    x = "Livret A Threshold (€)",
    y = "Share of Total Balance (%)"
  ) +
  theme_minimal() +
  scale_x_discrete(limits = niveaux_SLA) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1))

# ************* Faire un seul graphique pour les deux distributions ***************************************
library(dplyr)
library(tidyr)
library(ggplot2)

graph_LA <- Livret_A %>%
  filter(SLA %in% niveaux_SLA) %>%
  select(SLA, percent_livret, percent_encours) %>%
  pivot_longer(
    cols = c(percent_livret, percent_encours),
    names_to = "Type",
    values_to = "Pourcentage"
  ) %>%
  mutate(
    Type = recode(
      Type,
      percent_livret = "Number of Accounts",
      percent_encours = "Total Balance"
    )
  )

ggplot(graph_LA,
       aes(x = SLA, y = Pourcentage, fill = Type)) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  geom_text(
    aes(label = paste0(round(Pourcentage, 1), "%")),
    position = position_dodge(width = 0.8),
    vjust = -0.3,
    size = 2
  ) +
  scale_fill_manual(
    values = c(
      "Number of Accounts" = "#2C7FB8",
      "Total Balance" = "#DA291C"
    )
  ) +
  labs(
    x = "Livret A Threshold (€)",
    y = "Percentage (%)",
    fill = NULL
  ) +
  scale_x_discrete(limits = niveaux_SLA) +
  theme_minimal() +
theme(
  panel.background = element_rect(fill = "#F2F2F2", colour = NA),
  plot.background = element_rect(fill = "#F2F2F2", colour = NA),
  panel.grid.major.x = element_blank(),
  legend.position = "top",
  axis.text.x = element_text(angle = 45, hjust = 1)
)
#******************** Nombre de Livret DDS ********************************
#* Définition des seuils LDDS
produit_t$SLDDS <- 0
produit_t$SLDDS[produit_t$FINNA %in% c("04") & produit_t$MONTCLA < 150] <- "<150€"
produit_t$SLDDS[produit_t$FINNA %in% c("04") & produit_t$MONTCLA >= 150 & produit_t$MONTCLA < 750] <- "150-750€"
produit_t$SLDDS[produit_t$FINNA %in% c("04") & produit_t$MONTCLA >= 750 & produit_t$MONTCLA < 1500] <- "750-1500€"
produit_t$SLDDS[produit_t$FINNA %in% c("04") & produit_t$MONTCLA >= 1500 & produit_t$MONTCLA < 3000] <- "1500-3000€"
produit_t$SLDDS[produit_t$FINNA %in% c("04") & produit_t$MONTCLA >= 3000 & produit_t$MONTCLA < 6000] <- "3000-6000€"
produit_t$SLDDS[produit_t$FINNA %in% c("04") & produit_t$MONTCLA >= 6000 & produit_t$MONTCLA < 9000] <- "6000-9000€"
produit_t$SLDDS[produit_t$FINNA %in% c("04") & produit_t$MONTCLA >= 9000 & produit_t$MONTCLA < 12000] <- "9000-12000€"
produit_t$SLDDS[produit_t$FINNA %in% c("04") & produit_t$MONTCLA >= 12000] <- ">=12000€"


# Encours total et nombre total de LDDS

Total_LDDS <- produit_t %>%
  filter(FINNA %in% c("04")) %>%   # LDDS
  summarise(
    Nombre_de_livret = sum(POND_TRANS, na.rm = TRUE),
    Encours_total = sum(POND_TRANS * MONTCLA, na.rm = TRUE)
  )

Total_LDDS

# Encours et nombre de livret par catégorie
LDDS =produit_t %>% 
  filter(FINNA %in% c("04")) %>% 
  group_by(SLDDS) %>% 
  summarise(nb_livret=round(sum(POND_TRANS,na.rm = TRUE)/1000000,2),
            percent_livret = round(sum(POND_TRANS,na.rm = TRUE)/Total_LDDS$Nombre_de_livret * 100,1),
            encours=round(sum(POND_TRANS*MONTCLA)/1000000000,2),
            percent_encours=round(sum(POND_TRANS*MONTCLA)/Total_LDDS$Encours_total*100,1)) %>% adorn_totals("row")  

LDDS

niveaux_SLDDS <- c("<150€", "150-750€", "750-1500€","1500-3000€", "3000-6000€", "6000-9000€",
  "9000-12000€",">=12000€")

#******* Number of LDDS Accounts
ggplot(data = LDDS %>% filter(SLDDS %in% niveaux_SLDDS),
  aes(x = SLDDS, y = percent_livret)) +
  geom_bar(stat = "identity", fill = "#2C7FB8", width = 0.7) +
  geom_text(aes(label = paste0(format(percent_livret, big.mark = " ", nsmall = 0, decimal.mark = "."),"%")),
    vjust = -0.5,
    size = 3.5) +
  labs(title = NULL,
    x = "LDDS Threshold (€)",
    y = "Share of Accounts (%)") +
  theme_minimal() +
  scale_x_discrete(limits = niveaux_SLDDS) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#****** LDDS Balance
ggplot(data = LDDS %>% filter(SLDDS %in% niveaux_SLDDS),
  aes(x = SLDDS, y = percent_encours)) +
  geom_bar(stat = "identity", fill = "#DA291C", width = 0.7) +
  geom_text(
    aes(label = paste0(
      format(percent_encours, big.mark = " ", nsmall = 0, decimal.mark = "."),"%")),
    vjust = -0.5,size = 3.5) + labs(
    title = NULL,
    x = "LDDS Threshold (€)",
    y = "Share of Total Balance (%)") +
  theme_minimal() +
  scale_x_discrete(limits = niveaux_SLDDS) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#******** Faire un seul graphique *********************************************
graph_LDDS <- LDDS %>%
  filter(SLDDS %in% niveaux_SLDDS) %>%
  select(SLDDS, percent_livret, percent_encours) %>%
  pivot_longer(
    cols = c(percent_livret, percent_encours),
    names_to = "Type",
    values_to = "Pourcentage"
  ) %>%
  mutate(
    Type = recode(
      Type,
      percent_livret = "Number of Accounts",
      percent_encours = "Total Balance"
    )
  )

ggplot(
  graph_LDDS,
  aes(x = SLDDS, y = Pourcentage, fill = Type)
) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  geom_text(
    aes(label = paste0(round(Pourcentage, 1), "%")),
    position = position_dodge(width = 0.8),
    vjust = -0.3,
    size = 2
  ) +
  scale_fill_manual(
    values = c(
      "Number of Accounts" = "#2C7FB8",
      "Total Balance" = "#DA291C"
    )
  ) +
  labs(
    x = "LDDS Threshold (€)",
    y = "Percentage (%)",
    fill = NULL
  ) +
  scale_x_discrete(limits = niveaux_SLDDS) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "#F2F2F2", colour = NA),
    plot.background = element_rect(fill = "#F2F2F2", colour = NA),
    panel.grid.major.x = element_blank(),
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


#************** Nombre de Livret EP *******************************************
# Seuil des livrets 
produit_t$SLEP <- 0
produit_t$SLEP[produit_t$FINNA %in% c("05") & produit_t$MONTCLA < 150] <- "<150€"
produit_t$SLEP[produit_t$FINNA %in% c("05") & produit_t$MONTCLA >= 150 & produit_t$MONTCLA < 750] <- "150-750€"
produit_t$SLEP[produit_t$FINNA %in% c("05") & produit_t$MONTCLA >= 750 & produit_t$MONTCLA < 1500] <- "750-1500€"
produit_t$SLEP[produit_t$FINNA %in% c("05") & produit_t$MONTCLA >= 1500 & produit_t$MONTCLA < 3000] <- "1500-3000€"
produit_t$SLEP[produit_t$FINNA %in% c("05") & produit_t$MONTCLA >= 3000 & produit_t$MONTCLA < 7700] <- "3000-7700€"
produit_t$SLEP[produit_t$FINNA %in% c("05") & produit_t$MONTCLA >= 7700] <- ">=7700€"


# Encours total et nombre total de LEP
Total_LEP <- produit_t %>%
  filter(FINNA %in% c("05")) %>%   # LEP
  summarise(
    Nombre_de_livret = sum(POND_TRANS, na.rm = TRUE),
    Encours_total = sum(POND_TRANS * MONTCLA, na.rm = TRUE))

Total_LEP

# Encours et nombre de livret par catégorie
LEP =produit_t %>% 
  filter(FINNA %in% c("05")) %>% 
  group_by(SLEP) %>% 
  summarise(nb_livret=round(sum(POND_TRANS,na.rm = TRUE)/1000000,2),
            percent_livret = round(sum(POND_TRANS,na.rm = TRUE)/Total_LEP$Nombre_de_livret * 100,1),
            encours=round(sum(POND_TRANS*MONTCLA)/1000000000,2),
            percent_encours=round(sum(POND_TRANS*MONTCLA)/Total_LEP$Encours_total*100,1)) %>% adorn_totals("row")  

LEP

niveaux_SLEP <- c("<150€","150-750€", "750-1500€","1500-3000€","3000-7700€",">=7700€")

# Number of LEP Accounts
ggplot(data = LEP %>% filter(SLEP %in% niveaux_SLEP),
  aes(x = SLEP, y = percent_livret)) +  
  geom_bar(stat = "identity", fill = "#2C7FB8", width = 0.7) +
  geom_text(aes(label = paste0(
    format(percent_livret, big.mark = " ", nsmall = 0, decimal.mark = "."), "%")),
    vjust = -0.5,size = 3.5) + labs(
    title = NULL,
    x = "LEP Threshold (€)",
    y = "Share of Accounts (%)") +
  theme_minimal() +
  scale_x_discrete(limits = niveaux_SLEP) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# LEP Balance
ggplot(data = LEP %>% filter(SLEP %in% niveaux_SLEP),
  aes(x = SLEP, y = percent_encours)) +
  geom_bar(stat = "identity", fill = "#DA291C", width = 0.7) +
  geom_text(aes(label = paste0(
      format(percent_encours, big.mark = " ", nsmall = 0, decimal.mark = "."),"%")),
    vjust = -0.5,size = 3.5) +
  labs(title = NULL,
    x = "LEP Threshold",
    y = "Share of Total Balance (%)") +
  theme_minimal() +
  scale_x_discrete(limits = niveaux_SLEP) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# *********** Faire sur le même graphique *********************************************************
graph_LEP <- LEP %>%
  filter(SLEP %in% niveaux_SLEP) %>%
  select(SLEP, percent_livret, percent_encours) %>%
  pivot_longer(
    cols = c(percent_livret, percent_encours),
    names_to = "Type",
    values_to = "Pourcentage"
  ) %>%
  mutate(
    Type = recode(
      Type,
      percent_livret = "Number of Accounts",
      percent_encours = "Total Balance"
    )
  )

ggplot(
  graph_LEP,
  aes(x = SLEP, y = Pourcentage, fill = Type)
) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  geom_text(
    aes(label = paste0(round(Pourcentage, 1), "%")),
    position = position_dodge(width = 0.8),
    vjust = -0.3,
    size = 2
  ) +
  scale_fill_manual(
    values = c(
      "Number of Accounts" = "#2C7FB8",
      "Total Balance" = "#DA291C"
    )
  ) +
  labs(
    x = "LEP Threshold (€)",
    y = "Percentage (%)",
    fill = NULL
  ) +
  scale_x_discrete(limits = niveaux_SLEP) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "#F2F2F2", colour = NA),
    plot.background = element_rect(fill = "#F2F2F2", colour = NA),
    panel.grid.major.x = element_blank(),
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# ************** Profil des detenteurs de livret ***************************************************
# ******** Age (LA, LDDS, LEP, Distribution age) ***************************************************
individu_t$AGE_RELATIF=2021-individu_t$ANAIS

# Histogramme des âges 
hist(individu_t$AGE_RELATIF,xlab = "Age of respondents at the time of the survey (2021)",ylab="Frequency",main="Histogram of Age")

individu_t$CATAGE_RELATIF=NA

individu_t$CATAGE_RELATIF[individu_t$AGE_RELATIF <= 14] = 1                              #0 à 14 ans
individu_t$CATAGE_RELATIF[individu_t$AGE_RELATIF > 14 & individu_t$AGE_RELATIF <=29] = 2 #15 à 29 ans
individu_t$CATAGE_RELATIF[individu_t$AGE_RELATIF > 29 & individu_t$AGE_RELATIF <=44] = 3 #30 à 44 ans
individu_t$CATAGE_RELATIF[individu_t$AGE_RELATIF > 44 & individu_t$AGE_RELATIF <=59] = 4 #45 à 59 ans
individu_t$CATAGE_RELATIF[individu_t$AGE_RELATIF > 59 & individu_t$AGE_RELATIF <=74] = 5 #60 à 74 ans
individu_t$CATAGE_RELATIF[individu_t$AGE_RELATIF > 74] = 6                               #75 ans ou plus 

individu_t$CATAGE_RELATIF <- 
  factor(individu_t$CATAGE_RELATIF,levels = c(1, 2, 3, 4, 5, 6), 
         labels = c("0-14","15-29","30-44","45-59","60-74","75+"))


# Structure de la population
POP_AGE <- individu_t %>%
  group_by(CATAGE_RELATIF) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(pourcent = 100 * n / sum(n),
         Produit = "Population")

POP_AGE$pourcent[which.max(POP_AGE$pourcent)] =POP_AGE$pourcent[which.max(POP_AGE$pourcent)] + (100 - sum(POP_AGE$pourcent))

# Livret A
LA_AGE <- individu_t %>%
  filter(LIVABL == 1) %>%
  group_by(CATAGE_RELATIF) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(pourcent = 100 * n / sum(n),
         Produit = "Livret A")

# Ajustement pour que la somme fasse exactement 100
LA_AGE$pourcent[which.max(LA_AGE$pourcent)] = LA_AGE$pourcent[which.max(LA_AGE$pourcent)] + (100 - sum(LA_AGE$pourcent))

# LDDS
LDDS_AGE <- individu_t %>%
  filter(LDD == 1) %>%
  group_by(CATAGE_RELATIF) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(pourcent = 100 * n / sum(n),
         Produit = "LDDS")

LDDS_AGE$pourcent[which.max(LDDS_AGE$pourcent)] = LDDS_AGE$pourcent[which.max(LDDS_AGE$pourcent)] + (100 - sum(LDDS_AGE$pourcent))

# LEP
LEP_AGE <- individu_t %>%
  filter(LEP == 1) %>%
  group_by(CATAGE_RELATIF) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(pourcent = 100 * n / sum(n),
         Produit = "LEP")

LEP_AGE$pourcent[which.max(LEP_AGE$pourcent)] = LEP_AGE$pourcent[which.max(LEP_AGE$pourcent)] + (100 - sum(LEP_AGE$pourcent))

# Vérification 
sum(LA_AGE$pourcent)
sum(LDDS_AGE$pourcent)
sum(LEP_AGE$pourcent)
sum(POP_AGE$pourcent)

# Fusion
graph_age <- bind_rows(
  LA_AGE,
  LDDS_AGE,
  LEP_AGE,
  POP_AGE)

View(graph_age)

# Visualisation 
graph_age$Produit <- factor(
  graph_age$Produit,
  levels = c("Livret A", "LDDS", "LEP", "Population"))

ggplot(graph_age,
       aes(x = Produit,
           y = pourcent,
           fill = CATAGE_RELATIF)) +
  
  geom_col(width = 0.65) +
  
  geom_text(
    aes(label = round(pourcent, 0)),
    position = position_stack(vjust = 0.5),
    colour = "white",
    size = 3
  ) +
  
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 10)
  ) +
  
  scale_fill_manual(
    values = c(
      "0-14" = "#1565C0",
      "15-29" = "#00838F",
      "30-44" = "#F28C00",
      "45-59" = "#C00030",
      "60-74" = "#8DBF00",
      "75+" = "#4E7D00"
    )
  ) +
  
  labs(
    x = NULL,
    y = "Percentage (%)",
    fill = NULL,
    title = NULL
  ) +
  
  theme_minimal() +
  theme(legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(size = 12)) 

# ******** Encours Livret par age (Niveau de Vie) **************************************************
View(produit_t)

# Calcul du montant des livrets par individus
Montant_produit_I = produit_t %>%
  group_by(IDENTIND20) %>% 
  summarise(MLA_I = round(sum(MONTCLA * (FINNA %in% c("02","03")), na.rm = TRUE), 0),        # LA ou LBLEU
            NBLA_I = round(sum(NBLA),0), # NBLA 
            MLDDS_I=round(sum(MONTCLA * (FINNA %in% c("04")),na.rm = TRUE), 0),              # LDDS
            NBLDDS_I = round(sum(NBLDD),0), # NBLDDS 
            MLEP_I=round(sum(MONTCLA * (FINNA %in% c("05")),na.rm = TRUE), 0),               # LEP
            NBLEP_I = round(sum(NBLEP),0))                                                   # NBLEP 

View(Montant_produit_I)

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

# Calcul du montant moyen par livret
Montant_produit_I <- Montant_produit_I %>%
  mutate(MMLA_I = ifelse(NBLA_I > 0, round(MLA_I/NBLA_I,0), 0),
    MMLDDS_I = ifelse(NBLDDS_I > 0, round(MLDDS_I/NBLDDS_I,0), 0),
    MMLEP_I = ifelse(NBLEP_I > 0,round(MLEP_I/NBLEP_I,0), 0))

View(Montant_produit_I)

Montant_produit_I$NBLA_I = NULL 
Montant_produit_I$MLA_I = NULL 
Montant_produit_I$NBLDDS_I = NULL 
Montant_produit_I$MLDDS_I = NULL
Montant_produit_I$NBLEP_I = NULL 
Montant_produit_I$MLEP_I = NULL 
individu_t$MMLA_I.x = NULL 
individu_t$MMLDDS_I.x = NULL
individu_t$MMLEP_I.x = NULL 
individu_t$MMLA_I.y = NULL 
individu_t$MMLDDS_I.y = NULL 
individu_t$MMLEP_I.y= NULL 


# Ramener ces infos dans la table individu
individu_t = individu_t %>% 
  left_join(Montant_produit_I,by="IDENTIND20")

View(individu_t)

individu_t <- individu_t %>%
  mutate(MMLA_I   = ifelse(is.na(MMLA_I), 0, MMLA_I),
    MMLDDS_I = ifelse(is.na(MMLDDS_I), 0, MMLDDS_I),
    MMLEP_I  = ifelse(is.na(MMLEP_I), 0, MMLEP_I))

mean(is.na(individu_t$AGE_RELATIF)) * 100

# Moyenne du montant de Livret A par âge
livreta_age <- individu_t %>%
  group_by(AGE_RELATIF) %>%
  summarise(
    montant_moyen = mean(MMLA_I, na.rm = TRUE),
    montant_median = median(MMLA_I, na.rm = TRUE),
    nb_individus = n()
  ) %>%
  filter(!is.na(AGE_RELATIF))

View(livreta_age)

livreta_age <- livreta_age %>%
  filter(AGE_RELATIF <= 82) # Esperance de Vie 

v <- ggplot(livreta_age, aes(x = AGE_RELATIF)) +
  
  # Number of individuals by age
  geom_col(
    aes(y = nb_individus * max(montant_moyen) / max(nb_individus)),
    fill = "lightgrey",
    alpha = 0.5
  ) +
  
  # Average Livret A balance
  geom_line(
    aes(y = montant_moyen),
    color = "#1f77b4",
    linewidth = 0.8
  ) +
  
  geom_point(
    aes(y = montant_moyen),
    color = "#1f77b4",
    size = 0.5
  ) +
  
  # # LOESS trend
  # geom_smooth(
  #   aes(y = montant_moyen),
  #   method = "loess",
  #   color = "red",
  #   linewidth = 0.8,
  #   se = FALSE
  # ) +
  
  labs(
    title = "Average Livret A Balance by Age",
    x = "Age",
    y = "Average Livret A Balance (€)",
    caption = "Grey bars represent the number of individuals"
  ) +
  
  scale_y_continuous(
    labels = scales::label_number(
      big.mark = " ",
      suffix = " €"
    ),
    sec.axis = sec_axis(
      ~ . * max(livreta_age$nb_individus) / max(livreta_age$montant_moyen),
      name = "Number of Individuals"
    )
  ) +
  
  coord_cartesian(xlim = c(0, 98)) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.minor = element_blank()
  )

ggplotly(v)


# *********** LA, LDDS et LEP ***********************************************
livreta_age <- individu_t %>%
  group_by(AGE_RELATIF) %>%
  summarise(
    montant_moyen = mean(MMLA_I, na.rm = TRUE),
    montant_moyen_ldds = mean(MMLDDS_I, na.rm = TRUE),
    montant_moyen_lep = mean(MMLEP_I, na.rm = TRUE),
    montant_median = median(MMLA_I, na.rm = TRUE),
    nb_individus = n(),
    .groups = "drop"
  ) %>%
  filter(
    !is.na(AGE_RELATIF),
    AGE_RELATIF <= 82
  )

v <- ggplot(livreta_age, aes(x = AGE_RELATIF)) +
  
  # Population
  geom_col(
    aes(
      y = nb_individus * max(montant_moyen) / max(nb_individus),
      fill = "Population"
    ),
    alpha = 0.5
  ) +
  
  # Livret A
  geom_line(
    aes(y = montant_moyen, color = "Livret A"),
    linewidth = 0.8
  ) +
  geom_point(
    aes(y = montant_moyen, color = "Livret A"),
    size = 0.5
  ) +
  
  # LDDS
  geom_line(
    aes(y = montant_moyen_ldds, color = "LDDS"),
    linewidth = 0.8
  ) +
  geom_point(
    aes(y = montant_moyen_ldds, color = "LDDS"),
    size = 0.5
  ) +
  
  # LEP
  geom_line(
    aes(y = montant_moyen_lep, color = "LEP"),
    linewidth = 0.8
  ) +
  geom_point(
    aes(y = montant_moyen_lep, color = "LEP"),
    size = 0.5
  ) +
  
  scale_color_manual(
    values = c(
      "Livret A" = "#1f77b4",
      "LDDS" = "forestgreen",
      "LEP" = "#d62728"
    ),
    name = NULL
  ) +
  
  scale_fill_manual(
    values = c(
      "Population" = "lightgrey"
    ),
    name = NULL
  ) +
  
  guides(
    color = guide_legend(order = 1),
    fill = guide_legend(order = 2)
  ) +
  
  labs(
    title = NULL,
    x = "Age",
    y = "Average Balance (€)",
    caption = "Grey bars represent the number of individuals"
  ) +
  
  scale_y_continuous(
    labels = scales::label_number(
      big.mark = " ",
      suffix = " €"
    ),
    sec.axis = sec_axis(
      ~ . * max(livreta_age$nb_individus) / max(livreta_age$montant_moyen),
      name = "Number of Individuals"
    )
  ) +
  
  coord_cartesian(xlim = c(0, 98)) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.minor = element_blank(),
    legend.position = c(0.15, 0.85),
    legend.background = element_rect(
      fill = "white",
      color = "grey80"
    )
  )

ggplotly(v) %>%
layout(legend = list(title = list(text = "")))

p <- ggplotly(v)

p$x$data[[1]]$name <- "Population"
p$x$data[[2]]$name <- "Livret A"
p$x$data[[3]]$name <- "LDDS"
p$x$data[[4]]$name <- "LEP"

print(v)

v <- ggplot(livreta_age, aes(x = AGE_RELATIF)) +
  
  # Population
  geom_col(
    aes(
      y = nb_individus * max(montant_moyen) / max(nb_individus),
      fill = "Population"
    ),
    alpha = 0.5
  ) +
  
  # Livret A
  geom_line(
    aes(y = montant_moyen, color = "Livret A"),
    linewidth = 0.8
  ) +
  geom_point(
    aes(y = montant_moyen, color = "Livret A"),
    size = 0.5
  ) +
  
  # LDDS
  geom_line(
    aes(y = montant_moyen_ldds, color = "LDDS"),
    linewidth = 0.8
  ) +
  geom_point(
    aes(y = montant_moyen_ldds, color = "LDDS"),
    size = 0.5
  ) +
  
  # LEP
  geom_line(
    aes(y = montant_moyen_lep, color = "LEP"),
    linewidth = 0.8
  ) +
  geom_point(
    aes(y = montant_moyen_lep, color = "LEP"),
    size = 0.5
  ) +
  
  scale_color_manual(
    values = c(
      "Livret A" = "#1f77b4",
      "LDDS" = "forestgreen",
      "LEP" = "#d62728"
    ),
    name = NULL
  ) +
  
  scale_fill_manual(
    values = c(
      "Population" = "lightgrey"
    ),
    name = NULL
  ) +
  
  guides(
    color = guide_legend(
      order = 1,
      override.aes = list(
        linewidth = 1.2,
        size = 2
      )
    ),
    fill = guide_legend(
      order = 2,
      override.aes = list(
        alpha = 0.5
      )
    )
  ) +
  
  labs(
    title = NULL,
    x = "Age",
    y = "Average Balance (€)",
    caption = "Grey bars represent the number of individuals"
  ) +
  
  scale_y_continuous(
    labels = scales::label_number(
      big.mark = " ",
      suffix = " €"
    ),
    sec.axis = sec_axis(
      ~ . * max(livreta_age$nb_individus) /
        max(livreta_age$montant_moyen),
      name = "Number of Individuals"
    )
  ) +
  
  coord_cartesian(xlim = c(0, 98)) +
  
  theme_minimal() +
  
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.minor = element_blank(),
    
    legend.position = "bottom",
    legend.direction = "horizontal",
    
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    legend.key = element_blank()
  )

# Affichage
print(v)

# Sauvegarde PDF (idéal pour Overleaf)
ggsave(
  "Encours_par_age.pdf",
  plot = v,
  width = 10,
  height = 6
)


# ******** Situation matrimoniale ******************************************************************
individu_t = as_tibble(individu)

individu_t_18 <- individu_t %>%
  filter(AGE_RELATIF >= 18)

View(individu_t)

individu_t_18$ETAMATRI <- factor(
  individu_t_18$ETAMATRI,
  levels = c(1, 2, 3, 4),
  labels = c(
    "Single",
    "Married/Remarried",
    "Widowed",
    "Divorced"))

# Population
POP_MATRI <- individu_t_18 %>%
  group_by(ETAMATRI) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(
    pourcent = 100 * n / sum(n),
    Produit = "Population"
  )

POP_MATRI$pourcent[which.max(POP_MATRI$pourcent)] <-
  POP_MATRI$pourcent[which.max(POP_MATRI$pourcent)] +
  (100 - sum(POP_MATRI$pourcent))

# Livret A
LA_MATRI <- individu_t_18 %>%
  filter(LIVABL == 1) %>%
  group_by(ETAMATRI) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(
    pourcent = 100 * n / sum(n),
    Produit = "Livret A"
  )

LA_MATRI$pourcent[which.max(LA_MATRI$pourcent)] <-
  LA_MATRI$pourcent[which.max(LA_MATRI$pourcent)] +
  (100 - sum(LA_MATRI$pourcent))

# LDDS
LDDS_MATRI <- individu_t_18 %>%
  filter(LDD == 1) %>%
  group_by(ETAMATRI) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(
    pourcent = 100 * n / sum(n),
    Produit = "LDDS"
  )

LDDS_MATRI$pourcent[which.max(LDDS_MATRI$pourcent)] <-
  LDDS_MATRI$pourcent[which.max(LDDS_MATRI$pourcent)] +
  (100 - sum(LDDS_MATRI$pourcent))

# LEP
LEP_MATRI <- individu_t_18 %>%
  filter(LEP == 1) %>%
  group_by(ETAMATRI) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(
    pourcent = 100 * n / sum(n),
    Produit = "LEP"
  )

LEP_MATRI$pourcent[which.max(LEP_MATRI$pourcent)] <-
  LEP_MATRI$pourcent[which.max(LEP_MATRI$pourcent)] +
  (100 - sum(LEP_MATRI$pourcent))

# Fusion
graph_matri <- bind_rows(
  LA_MATRI,
  LDDS_MATRI,
  LEP_MATRI,
  POP_MATRI
)

View(graph_matri)

graph_matri$Produit <- factor(
  graph_matri$Produit,
  levels = c("Livret A", "LDDS", "LEP", "Population"))

ggplot(
  graph_matri,
  aes(
    x = Produit,
    y = pourcent,
    fill = ETAMATRI
  )
) +
  geom_col(width = 0.65) +
  geom_text(
    aes(label = round(pourcent, 0)),
    position = position_stack(vjust = 0.5),
    colour = "white",
    size = 2.5
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 10)
  ) +
  labs(
    x = NULL,
    y = "Percentage (%)",
    fill = NULL,
    title = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 7))

# ******** Niveau d'étude **************************************************************************
mean(is.na(individu$DIPDET))

# Recodage de la variable
individu_t$NIVEDUC=NA

individu_t$NIVEDUC[individu_t$DIPDET %in% c(00,70,71,"")] = 1       # Sans diplome ou CEP
individu_t$NIVEDUC[individu_t$DIPDET %in% c(45,51,52,53,60)] = 2    # brevet
individu_t$NIVEDUC[individu_t$DIPDET %in% c(41,42,43,44)] = 3       # bac ou bac+1
individu_t$NIVEDUC[individu_t$DIPDET %in% c(31,32,33,34,35,36)] = 4 # bac+2
individu_t$NIVEDUC[individu_t$DIPDET %in% c(22,23,24)] = 5          # bac+3
individu_t$NIVEDUC[individu_t$DIPDET %in% c(21)] = 6                # bac+4 
individu_t$NIVEDUC[individu_t$DIPDET %in% c(13,14,15,16,17)] = 7    # bac+5
individu_t$NIVEDUC[individu_t$DIPDET %in% c(11,12)] = 8             # bac+8

unique(individu_t_18$NIVEDUC)

individu_t_18$NIVEDUC <- factor(
  individu_t_18$NIVEDUC,levels = c(1, 2, 3, 4,5,6,7,8),
  labels = c("No diploma or CEP","brevet","bac or bac+1","bac+2","bac+3","bac+4","bac+5","bac+8"))

# Population
POP_NIVEDUC <- individu_t_18 %>%
  group_by(NIVEDUC) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(
    pourcent = 100 * n / sum(n),
    Produit = "Population")

POP_NIVEDUC$pourcent[which.max(POP_NIVEDUC$pourcent)] <-
  POP_NIVEDUC$pourcent[which.max(POP_NIVEDUC$pourcent)] +
  (100 - sum(POP_NIVEDUC$pourcent))

# Livret A
LA_NIVEDUC <- individu_t_18 %>%
  filter(LIVABL == 1) %>%
  group_by(NIVEDUC) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(
    pourcent = 100 * n / sum(n),
    Produit = "Livret A")

LA_NIVEDUC$pourcent[which.max(LA_NIVEDUC$pourcent)] <-
  LA_NIVEDUC$pourcent[which.max(LA_NIVEDUC$pourcent)] +
  (100 - sum(LA_NIVEDUC$pourcent))

# LDDS
LDDS_NIVEDUC <- individu_t_18 %>%
  filter(LDD == 1) %>%
  group_by(NIVEDUC) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(
    pourcent = 100 * n / sum(n),
    Produit = "LDDS")

LDDS_NIVEDUC$pourcent[which.max(LDDS_NIVEDUC$pourcent)] <-
  LDDS_NIVEDUC$pourcent[which.max(LDDS_NIVEDUC$pourcent)] +
  (100 - sum(LDDS_NIVEDUC$pourcent))

# LEP
LEP_NIVEDUC <- individu_t_18 %>%
  filter(LEP == 1) %>%
  group_by(NIVEDUC) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(
    pourcent = 100 * n / sum(n),
    Produit = "LEP")

LEP_NIVEDUC$pourcent[which.max(LEP_NIVEDUC$pourcent)] <-
  LEP_NIVEDUC$pourcent[which.max(LEP_NIVEDUC$pourcent)] +
  (100 - sum(LEP_NIVEDUC$pourcent))

graph_niveduc <- bind_rows(
  LA_NIVEDUC,
  LDDS_NIVEDUC,
  LEP_NIVEDUC,
  POP_NIVEDUC
)

graph_niveduc$Produit <- factor(
  graph_niveduc$Produit,
  levels = c("Livret A", "LDDS", "LEP", "Population"))

ggplot(graph_niveduc,
       aes(x = Produit,
           y = pourcent,
           fill = NIVEDUC)) +
  
  geom_col(width = 0.65) +
  
  geom_text(
    aes(label = round(pourcent, 0)),
    position = position_stack(vjust = 0.5),
    colour = "white",
    size = 1.8
  ) +
  
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 10)
  ) +
  
  labs(
    x = NULL,
    y = "Percentage (%)",
    fill = NULL,
    title = "Distribution by Education Level (%)"
  ) +
  
  theme_minimal() +
  
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 7),
    legend.key.width = unit(0.25, "cm"),
    legend.key.height = unit(0.25, "cm"),
    plot.title = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(size = 8),
    axis.text = element_text(size = 8)
  ) +
  guides(fill = guide_legend(nrow = 2))

# ******** Catégorie Socioprofessionelle (CSP) *****************************************************
individu_t_18$CS8=NA

individu_t_18$CS8[individu_t_18$CS24 %in% c(10)] = 1 #Agriculteurs exploitants 
individu_t_18$CS8[individu_t_18$CS24 %in% c(21,22,23)] = 2 #Artisans, commerçants et chefs d'entreprise
individu_t_18$CS8[individu_t_18$CS24 %in% c(31,32,36)] = 3 #Cadres et professions intellectuelles supérieures
individu_t_18$CS8[individu_t_18$CS24 %in% c(41,46,47,48)] = 4 #Professions Intermédiaires
individu_t_18$CS8[individu_t_18$CS24 %in% c(51,54,55,56)] = 5 #Employés
individu_t_18$CS8[individu_t_18$CS24 %in% c(61,66,69)] = 6 #Ouvriers
individu_t_18$CS8[individu_t_18$CS24 %in% c(71,72,73,76)] = 7 #Retraités
individu_t_18$CS8[individu_t_18$CS24  %in% c(81)] = 8 # Chomeurs
individu_t_18$CS8[individu_t_18$CS24 %in% c("00",82,"")] = 9 # Inactive

View(individu_t_18)
# Vérifier le taux de manquants
mean(is.na(individu_t_18$CS8)) * 100 

individu_t_18$CS8L <- factor(
  individu_t_18$CS8,
  levels = c(1, 2, 3, 4, 5, 6, 7, 8,9),
  labels = c("Farmers", "Self-employed, Business Owners",
    "Managers and Higher Intellectual Professions",
    "Intermediate Occupations", "Employees","Manual Workers","Retirees",
    "Unemployed","Inactive"))

# Population
POP_CS <- individu_t_18 %>%
  group_by(CS8L) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(
    pourcent = 100 * n / sum(n),
    Produit = "Population"
  )

POP_CS$pourcent[which.max(POP_CS$pourcent)] <-
  POP_CS$pourcent[which.max(POP_CS$pourcent)] +
  (100 - sum(POP_CS$pourcent))

# Livret A
LA_CS <- individu_t_18 %>%
  filter(LIVABL == 1) %>%
  group_by(CS8L) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(
    pourcent = 100 * n / sum(n),
    Produit = "Livret A"
  )

LA_CS$pourcent[which.max(LA_CS$pourcent)] <-
  LA_CS$pourcent[which.max(LA_CS$pourcent)] +
  (100 - sum(LA_CS$pourcent))

# LDDS
LDDS_CS <- individu_t_18 %>%
  filter(LDD == 1) %>%
  group_by(CS8L) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(
    pourcent = 100 * n / sum(n),
    Produit = "LDDS"
  )

LDDS_CS$pourcent[which.max(LDDS_CS$pourcent)] <-
  LDDS_CS$pourcent[which.max(LDDS_CS$pourcent)] +
  (100 - sum(LDDS_CS$pourcent))

# LEP
LEP_CS <- individu_t_18 %>%
  filter(LEP == 1) %>%
  group_by(CS8L) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(
    pourcent = 100 * n / sum(n),
    Produit = "LEP"
  )

LEP_CS$pourcent[which.max(LEP_CS$pourcent)] <-
  LEP_CS$pourcent[which.max(LEP_CS$pourcent)] +
  (100 - sum(LEP_CS$pourcent))

# Fusion
graph_cs <- bind_rows(
  LA_CS,
  LDDS_CS,
  LEP_CS,
  POP_CS
)

graph_cs$Produit <- factor(
  graph_cs$Produit,
  levels = c("Livret A", "LDDS", "LEP", "Population")
)

ggplot(
  graph_cs,
  aes(
    x = Produit,
    y = pourcent,
    fill = CS8L
  )
) +
  
  geom_col(width = 0.65) +
  
  geom_text(
    aes(label = round(pourcent, 0)),
    position = position_stack(vjust = 0.5),
    colour = "white",
    size = 2.0
  )+
  
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 10)
  ) +
  
  labs(
    x = NULL,
    y = "Percentage (%)",
    fill = NULL,
    title = "Distribution by Socio-Professional Category (%)"
  ) +
  
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    panel.grid.major.x = element_blank(),
    
    axis.text.x = element_text(size = 9),      # étiquettes axe X
    axis.text.y = element_text(size = 9),      # étiquettes axe Y
    axis.title.y = element_text(size = 9),     # titre axe Y
    legend.text = element_text(size = 6),       # texte légende
    legend.title = element_text(size = 9),      # titre légende
    plot.title = element_text(size = 10)        # titre graphique
  ) + guides(
    fill = guide_legend(
      nrow = 3,
      keywidth = unit(0.2, "cm"),
      keyheight = unit(0.2, "cm")
    ))
View(individu_t_18)

# ******** Position dans la distribution du revenu individu ****************************************:
individu_t_18$ZREVDEC_I = ifelse(is.na(individu$ZSALAIRES_I), 0, individu$ZSALAIRES_I) +
  ifelse(is.na(individu$ZCHOMAGE_I), 0, individu$ZCHOMAGE_I) +
  ifelse(is.na(individu$ZRETRAITES_I), 0, individu$ZRETRAITES_I) +
  ifelse(is.na(individu$ZPENALIR_I), 0, individu$ZPENALIR_I) +
  ifelse(is.na(individu$ZRIC_I), 0, individu$ZRIC_I) +
  ifelse(is.na(individu$ZRNC_I), 0, individu$ZRNC_I) +
  ifelse(is.na(individu$ZRAG_I), 0, individu$ZRAG_I) +
  ifelse(is.na(individu$ZRENTES_I), 0, individu$ZRENTES_I) 

# Je vais degager les observations ayant un revenu négatif ZREVDEC_I
mean(is.na(individu$ZREVDEC_I)) * 100 

decile_revenu_ind <- with(individu,quantile(ZREVDEC_I,probs = (1:10)/10,type = 7,na.rm = TRUE))
decile_revenu_ind

decile_df_revenu_ind <- as.data.frame(decile_revenu_ind[4:9])
decile_df_revenu_ind

seuils <- c(-Inf, decile_df_revenu_ind$`decile_revenu_ind[4:9]`, Inf)
seuils

length(seuils) - 1 == 8

individu <- individu %>%
  mutate(
    decile_revenu_ind = cut(
      ZREVDEC_I,
      breaks = seuils,
      labels = c(
        "D1-D3: <8,472€",
        "D4: 8,472-15,900€",
        "D5: 15,900-21,000€",
        "D6: 21,000-26,600€",
        "D7: 26,600-35,400€",
        "D8: 35,400-52,400€",
        "D9-D10: >52,400€"
      ),
      include.lowest = TRUE,
      right = FALSE))

mean(is.na(individu$decile_revenu_ind)) * 100 
unique(individu$decile_revenu_ind)

# Population
POP_REV <- individu_t_18 %>%
  group_by(decile_revenu_ind) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(
    pourcent = 100 * n / sum(n),
    Produit = "Population"
  )

POP_REV$pourcent[which.max(POP_REV$pourcent)] <-
  POP_REV$pourcent[which.max(POP_REV$pourcent)] +
  (100 - sum(POP_REV$pourcent))

# Livret A
LA_REV <- individu_t_18 %>%
  filter(LIVABL == 1) %>%
  group_by(decile_revenu_ind) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(
    pourcent = 100 * n / sum(n),
    Produit = "Livret A"
  )

LA_REV$pourcent[which.max(LA_REV$pourcent)] <-
  LA_REV$pourcent[which.max(LA_REV$pourcent)] +
  (100 - sum(LA_REV$pourcent))

# LDDS
LDDS_REV <- individu_t_18 %>%
  filter(LDD == 1) %>%
  group_by(decile_revenu_ind) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(
    pourcent = 100 * n / sum(n),
    Produit = "LDDS"
  )

LDDS_REV$pourcent[which.max(LDDS_REV$pourcent)] <-
  LDDS_REV$pourcent[which.max(LDDS_REV$pourcent)] +
  (100 - sum(LDDS_REV$pourcent))


# LEP
LEP_REV <- individu_t_18 %>%
  filter(LEP == 1) %>%
  group_by(decile_revenu_ind) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(
    pourcent = 100 * n / sum(n),
    Produit = "LEP"
  )

LEP_REV$pourcent[which.max(LEP_REV$pourcent)] <-
  LEP_REV$pourcent[which.max(LEP_REV$pourcent)] +
  (100 - sum(LEP_REV$pourcent))

graph_rev <- bind_rows(
  LA_REV,
  LDDS_REV,
  LEP_REV,
  POP_REV
)

graph_rev$Produit <- factor(
  graph_rev$Produit,
  levels = c("Livret A", "LDDS", "LEP", "Population")
)
View(graph_rev)
ggplot(
  graph_rev,
  aes(
    x = Produit,
    y = pourcent,
    fill = decile_revenu_ind
  )
) +
  geom_col(width = 0.65) +
  geom_text(
    aes(label = round(pourcent, 1)),
    position = position_stack(vjust = 0.5),
    colour = "white",
    size = 2.5
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 10)
  ) +
  labs(
    x = NULL,
    y = "Percentage (%)",
    fill = "Decile",
    title = "Distribution by Income Decile (%)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.key.size = unit(0.2, "cm"),
    legend.text = element_text(size = 8),   # texte des modalités
    legend.title = element_text(size = 7),  # titre de la légende
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(size = 10)
  )


# ******** Les produits financiers (...) ***********************************************************

# LA 
LA_AVI_IND = individu_t %>% 
  filter(LIVABL==1) %>% 
  group_by(AVI) %>% 
  summarise(n = n()) %>% 
  mutate(AVI = case_when(
    AVI == 1 ~ "Oui", 
    AVI == 2 ~ "Non"),
    pourcent = round(n / sum(n) * 100, 0)
  ) %>% select(AVI, n, pourcent)  %>% 
  adorn_totals("row")

LA_AVI_IND

LA_LEP_IND = individu_t %>% 
  filter(LIVABL==1) %>% 
  group_by(LEP) %>% 
  summarise(n = n()) %>%
  mutate(LEP = case_when(
    LEP == 1 ~ "Oui", 
    LEP == 2 ~ "Non"),
    pourcent = round(n / sum(n) * 100, 0)
  ) %>% select(LEP, n, pourcent)  %>% 
  adorn_totals("row")

LA_LEP_IND

LA_LDDS_IND = individu_t %>% 
  filter(LIVABL==1) %>% 
  group_by(LDD) %>% 
  summarise(n = n()) %>% 
  mutate(LDD = case_when(
    LDD == 1 ~ "Oui", 
    LDD == 2 ~ "Non"), 
    pourcent = round(n/ sum(n) * 100, 0)
  ) %>% select(LDD, n, pourcent)  %>% 
  adorn_totals("row")

LA_LDDS_IND

LA_LEP_LDDS_IND = individu_t %>% 
  filter(LIVABL==1 & LEP==1) %>% 
  group_by(LDD) %>% 
  summarise(n = n()) %>% 
  mutate(R_LDD = case_when(
    LDD == 1 ~ "Oui", 
    LDD == 2 ~ "Non"),
    pourcent = round(n / sum(n) * 100, 0)
  ) %>% select(LDD, n, pourcent)  %>% 
  adorn_totals("row")

LA_LEP_LDDS_IND

n_triple_LA_LEP_LDDS <- LA_LEP_LDDS_IND$n[LA_LEP_LDDS_IND$LDD == 1]
n_triple_LA_LEP_LDDS
total_LA <- sum(individu_t$LIVABL == 1)
pct_triple_LA_LEP_LDDS = 100 * n_triple_LA_LEP_LDDS / total_LA 


LA_LDDS_ASSURANCE_VIE_IND <- individu %>%
  filter(LIVABL == 1 & LDD==1) %>%
  group_by(AVI) %>%
  summarise(n = n()) %>%
  mutate(AVI = case_when(
    AVI == 1 ~ "Oui",
    AVI == 2 ~ "Non"),
    pourcent = round(n / sum(n) * 100, 0)) %>%
  select(AVI, n, pourcent) %>%
  adorn_totals("row")   
# Ne pas considérer ce pourcentage acr totale pas correct
# Les n sont correct, diviser par le total de départ pour LA
LA_LDDS_ASSURANCE_VIE_IND

n_triple_LA_LDDS_AVI <- LA_LDDS_ASSURANCE_VIE_IND$n[LA_LDDS_ASSURANCE_VIE_IND$AVI == "Oui"]
total_LA <- sum(individu_t$LIVABL == 1)
pct_triple_LA_LDDS_AVI = 100 * n_triple_LA_LDDS_AVI / total_LA

LA_LIVJEUNE_IND = individu_t %>% 
  filter(LIVABL==1) %>% 
  group_by(LIVJEUN) %>% 
  summarise(n = n()) %>% 
  mutate(LIVJEUN=case_when(
    LIVJEUN == 1 ~ "Oui", 
    LIVJEUN == 2 ~ "Non"),
    pourcent = round(n / sum(n) * 100, 0)
  ) %>% select(LIVJEUN, n, pourcent)

LA_LIVJEUNE_IND

LA_LIVF_IND = individu_t %>% 
  filter(LIVABL==1) %>% 
  group_by(LIVF) %>% 
  summarise(n = n()) %>% 
  mutate(LIVF = case_when(
    LIVF == 1 ~ "Oui", 
    LIVF == 2 ~ "Non"),
    pourcent = round(n / sum(n) * 100, 0)
  ) %>% select(LIVF, n, pourcent)  %>% 
  adorn_totals("row")

LA_LIVF_IND

LA_VM_IND = individu_t %>% 
  filter(LIVABL==1) %>% 
  group_by(VMOB) %>% 
  summarise(n = n()) %>% 
  mutate(VMOB = case_when(
    VMOB == 1 ~ "Oui", 
    VMOB == 2 ~ "Non"),
    pourcent = round(n/ sum(n) * 100, 1)
  ) %>% select(VMOB, n, pourcent)  %>% 
  adorn_totals("row")

LA_VM_IND

LA_CT_IND = individu_t %>% 
  filter(LIVABL==1) %>% 
  group_by(TERM) %>% 
  summarise(n = n()) %>% 
  mutate(
    TERM = case_when(
      TERM == 1 ~ "Oui",
      TERM == 2 ~ "Non"),
    pourcent = round(n/ sum(n) * 100, 1)) %>% select(TERM, n, pourcent)  %>% 
  adorn_totals("row")

LA_CT_IND

LA_LEE_IND <- individu_t %>%
  filter(LIVABL == 1) %>%
  group_by(LEE) %>%
  summarise(n = n()) %>%
  mutate(
    LEE = case_when(LEE == 1 ~ "Oui", LEE == 2 ~ "Non"),  
    pourcent = round(n / sum(n) * 100, 1) 
  ) %>%
  select(LEE, n, pourcent) %>%
  adorn_totals("row")

LA_LEE_IND

LA_PEL_IND = individu_t %>% 
  filter(LIVABL==1) %>% 
  group_by(PEL) %>% 
  summarise(n = n())%>% 
  mutate(PEL = case_when(
    PEL == 1 ~ "Oui", 
    PEL == 2 ~ "Non"),
    pourcent = round(n/ sum(n) * 100, 0)
  ) %>% select(PEL, n, pourcent)  %>% 
  adorn_totals("row")

LA_PEL_IND

LA_CEL_IND = individu_t %>% 
  filter(LIVABL==1) %>% 
  group_by(CEL) %>% 
  summarise(n = n()) %>% 
  mutate(CEL = case_when(
    CEL == 1 ~ "Oui", 
    CEL == 2 ~ "Non"),
    pourcent = round(n / sum(n) * 100, 0)
  ) %>% select(CEL, n, pourcent)  %>% 
  adorn_totals("row")

LA_CEL_IND

tableau_LA <- tibble::tibble(
  Produit = c(
    "Assurance-vie",
    "LEP",
    "LDDS",
    "Triple détention : LA + LEP + LDDS",
    "Triple détention : LA + LDDS + Assurance-vie",
    "Livret Jeune",
    "Livret fiscalisé",
    "Valeurs mobilières",
    "Compte à terme",
    "LEE",
    "PEL",
    "CEL"
  ),
  Pourcentage = c(
    LA_AVI_IND$pourcent[LA_AVI_IND$AVI == "Oui"],
    LA_LEP_IND$pourcent[LA_LEP_IND$LEP == "Oui"],
    LA_LDDS_IND$pourcent[LA_LDDS_IND$LDD == "Oui"],
    round(pct_triple_LA_LEP_LDDS, 1),
    round(pct_triple_LA_LDDS_AVI, 1),
    LA_LIVJEUNE_IND$pourcent[LA_LIVJEUNE_IND$LIVJEUN == "Oui"],
    LA_LIVF_IND$pourcent[LA_LIVF_IND$LIVF == "Oui"],
    LA_VM_IND$pourcent[LA_VM_IND$VMOB == "Oui"],
    LA_CT_IND$pourcent[LA_CT_IND$TERM == "Oui"],
    LA_LEE_IND$pourcent[LA_LEE_IND$LEE == "Oui"],
    LA_PEL_IND$pourcent[LA_PEL_IND$PEL == "Oui"],
    LA_CEL_IND$pourcent[LA_CEL_IND$CEL == "Oui"]
  )
)

tableau_LA

# LDDS
LDDS_AVI_IND = individu_t %>% 
  filter(LDD==1) %>% 
  group_by(AVI) %>% 
  summarise(n = n()) %>% 
  mutate(AVI = case_when(
    AVI == 1 ~ "Oui", 
    AVI == 2 ~ "Non"),
    pourcent = round(n / sum(n) * 100, 0)
  ) %>% select(AVI, n, pourcent)  %>% 
  adorn_totals("row")

LDDS_AVI_IND

LDDS_LEP_IND = individu_t %>% 
  filter(LDD==1) %>% 
  group_by(LEP) %>% 
  summarise(n = n()) %>%
  mutate(LEP = case_when(
    LEP == 1 ~ "Oui", 
    LEP == 2 ~ "Non"),
    pourcent = round(n / sum(n) * 100, 0)
  ) %>% select(LEP, n, pourcent)  %>% 
  adorn_totals("row")

LDDS_LEP_IND

LDDS_LIVJEUNE_IND = individu_t %>% 
  filter(LDD==1) %>% 
  group_by(LIVJEUN) %>% 
  summarise(n = n()) %>% 
  mutate(LIVJEUN=case_when(
    LIVJEUN == 1 ~ "Oui", 
    LIVJEUN == 2 ~ "Non"),
    pourcent = round(n / sum(n) * 100, 0)
  ) %>% select(LIVJEUN, n, pourcent)

LDDS_LIVJEUNE_IND

LDDS_LIVF_IND = individu_t %>% 
  filter(LDD==1) %>% 
  group_by(LIVF) %>% 
  summarise(n = n()) %>% 
  mutate(LIVF = case_when(
    LIVF == 1 ~ "Oui", 
    LIVF == 2 ~ "Non"),
    pourcent = round(n / sum(n) * 100, 0)
  ) %>% select(LIVF, n, pourcent)  %>% 
  adorn_totals("row")

LDDS_LIVF_IND

LDDS_VM_IND = individu_t %>% 
  filter(LDD==1) %>% 
  group_by(VMOB) %>% 
  summarise(n = n()) %>% 
  mutate(VMOB = case_when(
    VMOB == 1 ~ "Oui", 
    VMOB == 2 ~ "Non"),
    pourcent = round(n/ sum(n) * 100, 1)
  ) %>% select(VMOB, n, pourcent)  %>% 
  adorn_totals("row")

LDDS_VM_IND

LDDS_CT_IND = individu_t %>% 
  filter(LDD==1) %>% 
  group_by(TERM) %>% 
  summarise(n = n()) %>% 
  mutate(TERM = case_when(
    TERM == 1 ~ "Oui", 
    TERM == 2 ~ "Non"),  
    pourcent = round(n / sum(n) * 100, 1)) %>% select(TERM, n, pourcent)  %>% 
  adorn_totals("row")

LDDS_CT_IND

LDDS_LEE_IND = individu_t %>% 
  filter(LDD==1) %>% 
  group_by(LEE) %>% 
  summarise(n = n()) %>% 
  mutate(LEE = case_when(
    LEE == 1 ~ "Oui", 
    LEE == 2 ~ "Non"),
    pourcent = round(n/ sum(n) * 100, 1)
  ) %>% select(LEE, n, pourcent)  %>% 
  adorn_totals("row")

LDDS_LEE_IND

LDDS_PEL_IND = individu_t %>% 
  filter(LDD==1) %>% 
  group_by(PEL) %>% 
  summarise(n = n())%>% 
  mutate(PEL = case_when(
    PEL == 1 ~ "Oui", 
    PEL == 2 ~ "Non"),
    pourcent = round(n/ sum(n) * 100, 0)
  ) %>% select(PEL, n, pourcent)  %>% 
  adorn_totals("row")

LDDS_PEL_IND

LDDS_CEL_IND = individu_t %>% 
  filter(LDD==1) %>% 
  group_by(CEL) %>% 
  summarise(n = n()) %>% 
  mutate(CEL = case_when(
    CEL == 1 ~ "Oui", 
    CEL == 2 ~ "Non"),
    pourcent = round(n / sum(n) * 100, 0)
  ) %>% select(CEL, n, pourcent)  %>% 
  adorn_totals("row")

LDDS_CEL_IND

  tableau_LDDS <- tibble::tibble(
    Produit = c(
      "Assurance-vie",
      "LEP",
      "Livret Jeune",
      "Livret fiscalisé",
      "Valeurs mobilières",
      "Compte à terme",
      "LEE",
      "PEL",
      "CEL"
    ),
    Pourcentage = c(
      LDDS_AVI_IND$pourcent[LDDS_AVI_IND$AVI == "Oui"],
      LDDS_LEP_IND$pourcent[LDDS_LEP_IND$LEP == "Oui"],
      LDDS_LIVJEUNE_IND$pourcent[LDDS_LIVJEUNE_IND$LIVJEUN == "Oui"],
      LDDS_LIVF_IND$pourcent[LDDS_LIVF_IND$LIVF == "Oui"],
      LDDS_VM_IND$pourcent[LDDS_VM_IND$VMOB == "Oui"],
      LDDS_CT_IND$pourcent[LDDS_CT_IND$TERM == "Oui"],
      LDDS_LEE_IND$pourcent[LDDS_LEE_IND$LEE == "Oui"],
      LDDS_PEL_IND$pourcent[LDDS_PEL_IND$PEL == "Oui"],
      LDDS_CEL_IND$pourcent[LDDS_CEL_IND$CEL == "Oui"]
    )
  )
  
  tableau_LDDS

# LEP
LEP_AVI_IND = individu_t %>% 
  filter(LEP==1) %>% 
  group_by(AVI) %>% 
  summarise(n = n()) %>% 
  mutate(AVI = case_when(
    AVI == 1 ~ "Oui", 
    AVI == 2 ~ "Non"),
    pourcent = round(n / sum(n) * 100, 0)
  ) %>% select(AVI, n, pourcent) %>% 
  adorn_totals("row")

LEP_AVI_IND

LEP_LIVJEUNE_IND = individu_t %>% 
  filter(LEP==1) %>% 
  group_by(LIVJEUN) %>% 
  summarise(n = n()) %>% 
  mutate(LIVJEUN=case_when(
    LIVJEUN == 1 ~ "Oui", 
    LIVJEUN == 2 ~ "Non"),
    pourcent = round(n / sum(n) * 100, 0)
  ) %>% select(LIVJEUN, n, pourcent) 

LEP_LIVJEUNE_IND

LEP_LIVF_IND = individu_t %>% 
  filter(LEP==1) %>% 
  group_by(LIVF) %>% 
  summarise(n = n()) %>% 
  mutate(LIVF = case_when(
    LIVF == 1 ~ "Oui", 
    LIVF == 2 ~ "Non"),
    pourcent = round(n / sum(n) * 100, 0)
  ) %>% select(LIVF, n, pourcent)  %>% 
  adorn_totals("row")

LEP_LIVF_IND

LEP_VM_IND = individu_t %>% 
  filter(LEP==1) %>% 
  group_by(VMOB) %>% 
  summarise(n = n()) %>% 
  mutate(VMOB = case_when(
    VMOB == 1 ~ "Oui", 
    VMOB == 2 ~ "Non"),
    pourcent = round(n/ sum(n) * 100, 1)
  ) %>% select(VMOB, n, pourcent)  %>% 
  adorn_totals("row")

LEP_VM_IND

LEP_CT_IND = individu_t %>% 
  filter(LEP==1) %>% 
  group_by(TERM) %>% 
  summarise(n = n()) %>% 
  mutate(TERM = case_when(
    TERM == 1 ~ "Oui", 
    TERM == 2 ~ "Non"),
    pourcent = round(n/ sum(n) * 100, 1)
  ) %>% select(TERM, n, pourcent)  %>% 
  adorn_totals("row")

LEP_CT_IND

LEP_LEE_IND = individu_t %>% 
  filter(LEP==1) %>% 
  group_by(LEE) %>% 
  summarise(n = n()) %>% 
  mutate(LEE = case_when(
    LEE == 1 ~ "Oui", 
    LEE == 2 ~ "Non"),
    pourcent = round(n/ sum(n) * 100, 1)
  ) %>% select(LEE, n, pourcent)  %>% 
  adorn_totals("row")

LEP_LEE_IND

LEP_PEL_IND = individu_t %>% 
  filter(LEP==1) %>% 
  group_by(PEL) %>% 
  summarise(n = n())%>% 
  mutate(PEL = case_when(
    PEL == 1 ~ "Oui", 
    PEL == 2 ~ "Non"),
    pourcent = round(n/ sum(n) * 100, 0)
  ) %>% select(PEL, n, pourcent)  %>% 
  adorn_totals("row")

LEP_PEL_IND

LEP_CEL_IND = individu_t %>% 
  filter(LEP==1) %>% 
  group_by(CEL) %>% 
  summarise(n = n()) %>% 
  mutate(CEL = case_when(
    CEL == 1 ~ "Oui", 
    CEL == 2 ~ "Non"),
    pourcent = round(n / sum(n) * 100, 0)
  ) %>% select(CEL, n, pourcent)  %>% 
  adorn_totals("row")

LEP_CEL_IND


tableau_LEP <- tibble::tibble(
  Produit = c(
    "Assurance-vie",
    "Livret Jeune",
    "Livret fiscalisé",
    "Valeurs mobilières",
    "Compte à terme",
    "LEE",
    "PEL",
    "CEL"
  ),
  Pourcentage = c(
    LEP_AVI_IND$pourcent[LEP_AVI_IND$AVI == "Oui"],
    LEP_LIVJEUNE_IND$pourcent[LEP_LIVJEUNE_IND$LIVJEUN == "Oui"],
    LEP_LIVF_IND$pourcent[LEP_LIVF_IND$LIVF == "Oui"],
    LEP_VM_IND$pourcent[LEP_VM_IND$VMOB == "Oui"],
    LEP_CT_IND$pourcent[LEP_CT_IND$TERM == "Oui"],
    LEP_LEE_IND$pourcent[LEP_LEE_IND$LEE == "Oui"],
    LEP_PEL_IND$pourcent[LEP_PEL_IND$PEL == "Oui"],
    LEP_CEL_IND$pourcent[LEP_CEL_IND$CEL == "Oui"]))

tableau_LEP

# ***** Disribution des detenteurs au plafond du Livret A *****************************************
View(produit_t)

# Livrets A au-dessus du plafond
LA_plafond <- produit_t %>%
  filter(
    FINNA %in% c("02", "03"),
    MONTCLA >= 22950)

View(LA_plafond)

# Nombre de livrets au-dessus du plafond
nrow(LA_plafond)

View(individu_t)

LA_plafond_rev <- LA_plafond %>%
  left_join(
    individu_t %>%
      select(IDENTIND20, decile_revenu_ind, POND_LONG_IND_2014),
    by = "IDENTIND20")

mean(is.na(LA_plafond_rev$decile_revenu_ind)) * 100

View(LA_plafond_rev)
produit_t$IDENTIND20
individu_t$IDENTIND20


# Les deux ont ça en commun 
LA_plafond_rev %>%
  group_by(decile_revenu_ind) %>%
  summarise(
    n = n()
  ) %>%
  mutate(
    pourcent = round(100 * n / sum(n), 1)
  ) %>% 
  adorn_totals()

sum(individu_t$LIVABL == 1, na.rm = TRUE)
 














