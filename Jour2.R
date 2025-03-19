# Mise en mémoire des packages
library(tidyverse)
library(palmerpenguins)  # Pour accéder au tableau penguins
library(skimr)           # Pour les résumés de données
library(rstatix)         # Tests avec les syntaxes du tidyverse
library(pwr)             # Pour les tests de puissance

# Comparaison de la moyenne d'une population à une valeur théorique ----

# Importation des données
Temperature <- read_csv("Data/Temperature.csv")
Temperature

# Exploration statistique
Temperature |> 
  skim()

# Transformation Fahrenheit -> Celsius
Temperature <- Temperature |> 
  mutate(temp = (temperature - 32) / 1.8)

Temperature |> 
  skim()

# Exploration graphique
Temperature |> 
  ggplot(aes(x = temp)) +
  geom_density(fill = "steelblue", alpha = 0.4)

## Test de normalité : Shapiro-Wilk ----
# H0 : les données suivent la loi normale dans la population générale
# H1 : les données ne suivent pas la loi normale dans la population générale
Temperature |> 
  shapiro_test(temp)

# Graphique quantile quantile (QQ-plot)
Temperature %>%
  ggplot(aes(sample = temp)) +
  geom_qq_line() +
  geom_qq() +
  labs(x = "Theoretical quantiles", y = "Sample quantiles")

# Les données suivent la loi Normale

## Test paramétrique de Student ----
# H0 : mu = 37
# H1 : mu ≠ 37
Temperature |> 
  t_test(temp ~ 1, mu = 37, detailed = TRUE)

# On ne rejette pas H0 : nos données sont compatibles avec l'hypothèse que la moyenne des températures vaut 37ºC dans la population générale.

# Comparaison de moyenne : test paramétrique de Student unilatéral 
# Attention : ça n'est pas pertinent ici !
# H0 : mu = 37
# H1 : mu < 37
Temperature |> 
  t_test(temp ~ 1, mu = 37, detailed = TRUE, alternative = "less")

## Si les conditions d'application ne sont pas vérifiées, on fait le 
## Test non paramétrique de Wilcoxon ----
# H0 : médiane = 37
# H1 : médiane ≠ 37
Temperature |> 
  wilcox_test(temp ~ 1, mu = 37, detailed = TRUE)


# Test de puissance ----
# Calcul du d de Cohen (différence de moyenne / écart-type)
d <- 0.2 / sd(Temperature$temp)
pwr.t.test(d = d, sig.level = 0.05, power = 0.8, type = "one.sample")
pwr.t.test(d = d, sig.level = 0.05, n = 25, type = "one.sample")
pwr.t.test(sig.level = 0.05, n = 5, power = 0.8, type = "one.sample")

1.682 * sd(Temperature$temp)


# Test de Student à 2 échantillons appariés ----
testo <- read_csv("Data/Testosterone.csv")

# H0 : la moyenne du groupe "before" est égale à celle du groupe "after"
# H0 : mu_(after - before) = 0
# H1 : mu_(after - before) ≠ 0
# Calcul de la différence entre les 2 mesures répetées
testo <- testo |> 
  mutate(diff = logAfterImplant - logBeforeImplant)

testo

# Test de normalité : la différence suit-elle une loi Normale ?
testo |> 
  shapiro_test(diff)

# Test de Student : équivalent au test sur un échantillon
testo |> 
  t_test(diff ~ 1, mu = 0, detailed = TRUE)


# Test de Student à deux échantillons indépendants ----

# Importation des données des lézards cornus
lezard <- read_csv("Data/HornedLizards.csv")
lezard

# On renomme une variable pour plus de facilité
lezard <- lezard |> 
  rename(Length = squamosalHornLength)

# Exploration statistique
lezard |> 
  group_by(Survival) |> 
  skim(Length)

# Vérification des conditions d'application
# Normalité des données : à vérifier pour chacun des 2 groupes
lezard |> 
  group_by(Survival) |> 
  shapiro_test(Length)
# La normalité n'est pas vérifiée pour le groupe des vivants, 
# donc on doit faire un test de Wilcoxon

# Graphique quantile-quantile
lezard |> 
  ggplot(aes(sample = Length)) +
  geom_qq_line() +
  geom_qq() +
  labs(x = "Theoretical quantiles", y = "Sample quantiles") +
  facet_wrap(~Survival)

# On fait comme si la normalité était respectée dans les 2 groupes
# Test d'homoscédasticité de Levene
# H0 : tous les groupes ont même variance
# H1 : au moins un groupe a une variance différente des autres
lezard |> 
  levene_test(Length ~ Survival)
# p >> alpha : on ne rejette pas H0, les 2 groupes ont même variance

# Test de Student (bilatéral)
# H0 : mu_killed = mu_living -> mu_killed - mu_living = 0
# H1 : mu_killed ≠ mu_living -> mu_killed - mu_living ≠ 0
lezard |> 
  t_test(Length ~ Survival, var.equal = TRUE, detailed = TRUE)
# On rejette H0. Les lézards morts ont en moyenne des cornes plus courtes que
# les vivants de 2,29 mm (IC95% : [1.25 ; 3.34], test t...)

# Test de Student de "Welch", moins puissant que le "vrai" test de Student
# C'est utile si les données sont normales, mais que les variances ne sont
# pas homogènes (et qu'il n'y a pas plus d'un facteur 10 entre les 2 variances)
lezard |> 
  t_test(Length ~ Survival, var.equal = FALSE, detailed = TRUE)

# Puisque les données ne suivent pas la loi Normale, on aurait dû faire le test
# de Wilcoxon :
lezard |> 
  wilcox_test(Length ~ Survival, detailed = TRUE)

# Analyse de variance à 1 facteur ----
library(readr)
lumiere <- read_csv("Data/PhaseShift.csv")
lumiere

# Réalisation de l'ANOVA
# H0 : toutes les moyennes sont égales 
# mu_control = mu_knees = mu_eyes
# H1 : au moins une moyenne est différente des autres
res <- aov(shift ~ treatment, data = lumiere)

# L'objet qui contient les résultats d'un modèle linéaire est une liste
# qui contient beaucoup de choses, dont les résidus
glimpse(res)
res$residuals

# Normalité des résidus 
# (inutile la plupart du temps, la vérification graphique suffit)
hist(res$residuals)
shapiro.test(res$residuals)

# Homogénéité des résidus
# (inutile la plupart du temps, la vérification graphique suffit)
library(car)
leveneTest(res$residuals ~ lumiere$treatment)

# Vérification des conditions d'application
# Analyse des résidus (suffisant la plupart du temps)
par(mfrow = c(2,2))  # Découpage de la fenêtre graphique
plot(res)
par(mfrow = c(1,1))

# 1e condition : normalité des résidus respectée (graph en haut à droite)
# 2e condition : homogénéité des résidus respectée (haut gauche)

# Examen des résultats de l'ANOVA
summary(res)
# p < alpha : on rejette H0, au moins un groupe a une moyenne différente des autres

## Quels sont les groupes dont les moyennes diffèrent ?
## Test post-hoc ou test à posteriori : test Tukey HSD ----
res |> 
  tukey_hsd()

# Visualisation des résultats du test
res |> 
  tukey_hsd() |> 
  ggplot(aes(x = estimate, y = paste0(group1, "-", group2))) +
  geom_point() +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high)) +
  geom_vline(xintercept = 0, linetype = 2)


## Alternative non paramétrique ----
## Test de Kruskal-Wallis pour comparer les moyennes
lumiere |> 
  kruskal_test(shift ~ treatment)

# Test de Dunn pour le test post-hoc
lumiere |> 
  dunn_test(shift ~ treatment, detailed = TRUE)

# ANOVA 2 facteurs mixtes ----

# Saisie des données
zoopk <- tibble(
  treatment = rep(c("control", "low", "high"), 5),
  diversity = c(4.1, 2.2, 1.3, 3.2, 2.4, 2, 3, 1.5,
                1, 2.3, 1.3, 1, 2.5, 2.6, 1.6),
  block = factor(rep(1:5, each = 3)))

zoopk  

# Réalisation du test
model <- aov(diversity ~ treatment + Error(block), data = zoopk)
summary(model)


