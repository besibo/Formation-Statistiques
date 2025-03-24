# Mise en mémoire des packages
library(tidyverse)
library(skimr)           # Pour les résumés de données
library(rstatix)         # Tests avec les syntaxes du tidyverse

# ANOVA 3 facteurs, effet mixtes ----

# Création du jeu de données
noise <- tibble(score = c(12, 9, 9 , 10, 9, 11, 18, 20, 22, 22, 17, 23,
                          20, 24, 16, 18, 18, 22, 8, 10, 9, 11, 10, 12),
                background = rep(c("low", "high"), each = 12),
                stimul = rep(c("low", "high"), each = 6, times = 2),
                indiv = rep(letters[1:6], 4))

View(noise)
noise

# Réalisation de l'ANOVA
# Le terme Error(indiv / (background * stimul) indique que chaque individu a
# fourni une mesure pour chaque combinaison de modalité des variables
# background et stimul
res <- aov(score ~ background * stimul + Error(indiv / (background * stimul)),
           data = noise)

# Affichage des résultats
summary(res)

# Calcul des moyennes pour le graphique des interactions
resum <- noise |> 
  group_by(background, stimul) |> 
  summarise(moyenne = mean(score))

# Graphique des interactions
noise |> 
  ggplot(aes(x = background, y = score, color = stimul)) +
  geom_jitter(width = 0.2, height = 0) +
  geom_point(data = resum, aes(y = moyenne), shape = 17, size = 3) +
  geom_line(data = resum, aes(y = moyenne, group = stimul)) +
  theme_bw()


# ANOVA facteurs imbriqués (nested ANOVA) ----
# Importation des données
yields <- read_table("Data/Yields.txt")
yields

# Spécification du modèle
# Le terme Error(...) indique que dans chaque champ (block est un facteur 
# aléatoire), les 2 niveaux du facteur irrigation sont représentés, que dans
# chaque niveau du facetru irrigation, tous les niveaux du facteur density
# sont représentés, et ainsi de suite.
model <- aov(yield ~ irrigation * density * fertilizer +
               Error(block / irrigation / density / fertilizer),
             data = yields)

# Affichage des résultats
summary(model)

# ---- Analyse en composantes principales ----
library(ade4)  # Pour les analyses multivariées
library(corrplot) # Pour la visualisation graphique des corrélations
library(ggrepel)  # Pour afficher des étiquettes sans chevauchement sur les graphique
library(patchwork) # Pour afficher plusieurs graphiques dans la même fenêtre

# Récupération des données brutes.
data(olympic)
dat <- olympic$tab

# 1. Affichage des données brutes
dat

# 2. Examen des corrélations
round(cor(dat), 2)

# plot(dat$`100`, dat$long)
corrplot(cor(dat), diag = FALSE, type = "lower")

# 3. Réalisation de l'ACP
k <- ncol(dat) # Nb de variables = nb d'axes factoriels
acp <- dudi.pca(dat, scannf = FALSE, nf = k)  # ACP
aide <- inertia.dudi(acp, col.inertia = TRUE) # Aide à l'interprétation

# 4. Examen des valeurs propres
acp$eig
round(aide$tot.inertia, 2)  # On retient F1 et F2

# 5. Valeur seuil
seuil <- 100 / k
seuil

# 6. Quelles variables sont portées par les axes ?
# Contributions absolues : se lit en colonnes
round(aide$col.abs, 2)
# F1 : 100, long, 400, 110, perc
# F2 : poid, 400, disq, jave, 1500

# Contributions relatives : se lit en lignes
abs(round(aide$col.rel, 2))
# La majeure partie de l'information de la variable "saut en hauteur" est portée
# par l'axe F3. C'est donc bien sur cet axe qu'on retiendra cette variable.
# F3 : haut

# 7. Cercle(s) des corrélations
s.corcircle(acp$co, xax = 1, yax = 2)
s.corcircle(acp$co, xax = 1, yax = 3)

# 8. signification des axes
# F1 : gradient de performance sur 5 disciplines (sprints et sauts)
# F2 : compromis entre lancers et demi-fond
# F3 : résultats au saut en hauteur

# 9 et 10 : graphique des individus et interprétation
s.label(acp$li, xax = 1, yax = 2)
s.label(acp$li, xax = 1, yax = 3)

# Fusion du tableau de résultats avec les scores obtenus par les athlètes à
# l'issue de décathlon
res <- cbind(acp$li, olympic$score)

# Visualisatio des résultats
p1 <- res |> 
  ggplot(aes(x = Axis1, y = Axis2, color = `olympic$score`)) +
  geom_point() +
  geom_text_repel(aes(label = 1:33)) +
  scale_color_viridis_c() +
  theme_bw() +
  labs(color = "Score")

p2 <- res |> 
  ggplot(aes(x = Axis1, y = Axis3, color = `olympic$score`)) +
  geom_point() +
  geom_text_repel(aes(label = 1:33)) +
  scale_color_viridis_c() +
  theme_bw() +
  labs(color = "Score")

# Affichage des 2 graphiques côte à côte avec patchwork
p1 + p2


# Analyse en Factorielle des Correspondances ----

# Récupération du jeu de données
data(doubs)
doubs
glimpse(doubs)
dat <- doubs$fish

# Nombre d'axes factoriels
k <- min(dim(dat)) - 1

# Réalisation de l'AFC
afc <- dudi.coa(dat, scannf = FALSE, nf = k)
afc

# Pourcentages d'information portée par les axes
100 * (afc$eig) / sum(afc$eig)

# Visualisation des résultats
scatter(afc) # Avec la fonction intégrée de ade4

# Avec ggolot2
ggplot() +
  geom_vline(xintercept = 0, color = "grey35", linetype = 2) +
  geom_hline(yintercept = 0, color = "grey35", linetype = 2) +
  geom_label(data = afc$li,
             aes(x = Axis1, y = Axis2,
                 label = rownames(afc$li))) +
  geom_point(data = afc$co,
             aes(x = Comp1, y = Comp2), color = "red") +
  geom_text_repel(data = afc$co,
                  aes(x = Comp1, y = Comp2,
                      label = rownames(afc$co)),
                  max.overlaps = 30) +
  theme_bw()
