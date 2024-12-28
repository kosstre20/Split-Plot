
# Chargement des packages
library(lme4)

# Paramètres de simulation
set.seed(123)
nb_blocs <- 2
nb_parcelles_principales <- nb_blocs * 2 # Deux fertilisants par bloc
nb_sous_parcelles <- 3 # Trois variétés par parcelle principale

# Effets fixes
effet_fertilisant <- c(0, 2) # Différence entre les fertilisants
effet_variete <- c(-1, 0, 1) # Différences entre variétés

# Effets aléatoires
sigma_bloc <- 1   # Variabilité entre blocs
sigma_resid <- 0.5 # Variabilité résiduelle

# Génération des données
data <- expand.grid(
  Bloc = factor(1:nb_blocs),
  Fertilisant = factor(1:2),
  Variete = factor(1:3)
)
data$Parcelle <- interaction(data$Bloc, data$Fertilisant)
data$Y <-
  rep(rnorm(nb_blocs, 0, sigma_bloc), each = nb_parcelles_principales / nb_blocs * nb_sous_parcelles) +
  effet_fertilisant[as.numeric(data$Fertilisant)] +
  effet_variete[as.numeric(data$Variete)] +
  rnorm(nrow(data), 0, sigma_resid)

head(data)

# Ajustement du modèle
mod <- lmer(Y ~ Fertilisant * Variete + (1|Bloc) + (1|Parcelle), data = data)
summary(mod)

# Tests des effets fixes
tableau_anova <- anova(mod)
print(tableau_anova)


# Moyennes marginales
library(emmeans)
emm <- emmeans(mod, ~ Fertilisant * Variete)
plot(emm)




#Extensions aux Plans Split-Split-Plot

# Simulation pour un plan split-split-plot
set.seed(456)
nb_irrigation <- 2
nb_parcelles <- nb_blocs * 2
nb_varietes <- 3

data_split_split <- expand.grid(
  Bloc = factor(1:nb_blocs),
  Fertilisant = factor(1:2),
  Irrigation = factor(1:nb_irrigation),
  Variete = factor(1:nb_varietes)
)
data_split_split$Parcelle <- interaction(data_split_split$Bloc, data_split_split$Fertilisant)
data_split_split$SousParcelle <- interaction(data_split_split$Parcelle, data_split_split$Irrigation)
data_split_split$Y <-
  rep(rnorm(nb_blocs, 0, sigma_bloc), each = nb_parcelles / nb_blocs * nb_irrigation * nb_varietes) +
  effet_fertilisant[as.numeric(data_split_split$Fertilisant)] +
  rnorm(nrow(data_split_split), 0, sigma_resid)

mod_split_split <- lmer(Y ~ Fertilisant * Irrigation * Variete +
                          (1|Bloc) + (1|Parcelle) + (1|SousParcelle),
                        data = data_split_split)
summary(mod_split_split)
