# Mise en mémoire des packages
library(tidyverse)
library(palmerpenguins)  # Pour accéder au tableau penguins
library(skimr)           # Pour les résumés de données
library(ggbeeswarm)      # Pour les graphiques "beeswarm"
library(ggridges)        # Pour les graphiques de densités

# Affichage du tableau de données
penguins

# 1. Statistiques descriptives ----

## Indices de position ----

# Moyenne
mean(penguins$body_mass_g, na.rm = TRUE)

# Médiane
median(penguins$body_mass_g, na.rm = TRUE)

# Quartiles
quantile(penguins$body_mass_g, na.rm = TRUE, probs = c(0.25, 0.5, 0.75))
quantile(penguins$body_mass_g, na.rm = TRUE, probs = c(0.90))

# Une autre façon de procéder avec la syntaxe du tidyverse
penguins |> 
  summarise(moyenne = mean(body_mass_g, na.rm = TRUE),
            mediane = median(body_mass_g, na.rm = TRUE),
            .by = c(species, sex))

# Pour ranger dans une table les résultats de la fonction quantile
enframe(quantile(penguins$body_mass_g, na.rm = TRUE))

# Calcul des quantiles des masses corporelles pour chaque espèce et chaque sexe
penguins |> 
  reframe(enframe(quantile(body_mass_g, na.rm = TRUE)),
          .by = c(species, sex))

# Pour les facteurs : comptages
penguins |> 
  summarise(n(), .by = species)

penguins |> 
  count(species, sex)


## Indices de dispersion ----

# Variance
penguins |> 
  summarise(variance = var(body_mass_g, na.rm = TRUE))

# Écart-type
penguins |> 
  summarise(ecart_type = sd(body_mass_g, na.rm = TRUE))

# Intervalle inter-quartile
penguins |> 
  summarise(iqr = IQR(body_mass_g, na.rm = TRUE), .by = species)

# Étendue
penguins |> 
  summarise(
    etendue = max(body_mass_g, na.rm = TRUE) - min(body_mass_g, na.rm = TRUE), 
    .by = species
    )

# Utilisation de la fonction summary() (fonction générique)
summary(penguins$body_mass_g)  # Sur une variable numérique
summary(penguins$species)      # Sur un facteur
summary(penguins)              # Sur un tableau de données

# Utilisation de la fonction skim pour produire un résumé statistique synthétique
penguins |> 
  group_by(species) |> 
  skim(body_mass_g)


## Indices d'incertitude ----

# Erreur standard (s.e.)
# Calcul à la main...
penguins |> 
  summarise(ecart_type = sd(body_mass_g, na.rm = TRUE),
            n_obs = n(),
            se = ecart_type / sqrt(n_obs))

# ... ou avec une fonction dédiée du package ggplot2
penguins |> 
  reframe(mean_se(body_mass_g))

# Intervalle de confiance (à 95%)
# Calcul manuel approximatif
penguins |> 
  reframe(mean_se(body_mass_g, mult = 1.96))

# Calcul exact à l'aide d'une fonction dédiée
penguins |> 
  reframe(mean_cl_normal(body_mass_g), .by = species)


# 2. Graphiques exploratoires ----

## Données numériques ----

# Histogrammes
penguins |> 
  ggplot(aes(x = body_mass_g)) +
  geom_histogram(bins = 20) +
  facet_wrap(~species, ncol = 1)

# Diagrammes de densités
penguins |> 
  ggplot(aes(x = body_mass_g)) +
  geom_density() +
  facet_wrap(~species, ncol = 1)

# Diagrammes de densités avec ggridges
penguins |> 
  ggplot(aes(x = body_mass_g, y = species)) +
  geom_density_ridges(alpha = 0.5)

# Pour comparer les sexes pour chaque espèce
penguins |> 
  filter(!is.na(sex)) |> 
  ggplot(aes(x = body_mass_g, y = species, fill = sex)) +
  geom_density_ridges(alpha = 0.5)

# Stripchart
penguins |> 
  ggplot(aes(x = species, y = body_mass_g)) +
  geom_jitter(height = 0, width = 0.2, alpha = 0.4)

# Boîtes à moustaches
penguins |> 
  ggplot(aes(x = species, y = body_mass_g)) +
  geom_boxplot(notch = TRUE)

# Violin plots
penguins |> 
  ggplot(aes(x = species, y = body_mass_g)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))

# Beeswarm plot
penguins |> 
  ggplot(aes(x = species, y = body_mass_g)) +
  geom_beeswarm()

## Données catégorielles (facteurs) ----
penguins |> 
  ggplot(aes(x = species)) +
  geom_bar()
