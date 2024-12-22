#Memoire Robin Schreiber
#Biologie des organismes et ecologie - UCLouvain
#Titre : Etude des barrirere de reproduction intraspecfique chez une espece de tomate sauvage Solanum Chilense
#Promoteur : Muriel Quinet - Encadrant : Pauline Moreels
#Earth Life Institut A - Groupe de recherche en physiologie vegetale 

#R Version 4.4.1
R.version
RStudio.Version()

#----Set Working Directory----

setwd("C:/Users/robin/OneDrive - UCL/Documents/UCL/MASTER/Mémoire/Expérience/R")

#----Installation des packages---- 

library(tidyverse) 
library(ggplot2) #Graphiques
library(car) #LevenTest
library(emmeans) #Comparaison mutliples
library(dunn.test) #Comparaison multiples
library(multcomp) #Fonction cld()
library(gt) #Tableaux
library(scales)  # col_numeric()
library(webshot2) #gtsave()
library(factoextra) #graphiques ACP
library(ggpattern) #graphique avec pattern 
library(corrplot) #graphique de correlation
library(patchwork) #présentation de plusieurs graphique en meme temps
library(rlang) #Fonction graphique de correlation

#Pour avoir la police "Aptos"
library(sysfonts) #font
library(showtext) #font
font_add("Aptos", "Aptos.ttf")


#Palette de couleurs 
colors <- c("PIS10" = "#003049",
            "PIS14" = "#362E41",
            "PIS19" = "#6B2C39",
            "PIS31" = "#D62828",
            "PIS33" = "#E75414",
            "PIS34" = "#F77F00",
            "PIS36" = "#FA9F25",
            "PIS38" = "#FCBF49",
            "PIS40" = "#F3D180", 
            "SC2" = "#EAE2B7",
            "Controle" = "#A1A09D")

##----Partie 1 - Description morphologique et exertion du stigmate----
#Importation des donnees et modification
Exertion <- read.csv("R_Exertion.csv", sep = ";", dec = ",") 

Exertion$Nb_Feuille <- as.factor(Exertion$Nb_Feuille)
Exertion$Ind <- as.factor(Exertion$Ind)
Exertion$Organe <- as.factor(Exertion$Organe)
Exertion$Place <- as.factor(Exertion$Place)


#Suppression des lignes inutiles
Exertion <- Exertion %>% 
  filter(Ind != "Rien") %>% 
  filter(!(Ind == "PIS14" & Nb_ech <= 5)) #Les 5 premiers mesures de PIS14 sont de trop

#Verification des donnees
Verif <- Exertion %>% 
  group_by(Ind,Organe,Nb_ech) %>% 
  reframe(Nb = n())

#Ajout des lignes pour l'aire des petale (corolle/5)
petale_data <- Exertion[Exertion$Organe == "corolle", ]
petale_data$Area <- petale_data$Area / 5 #Divison de l'aire de la corolle par 5
petale_data$Organe <- "petale" #Changement de la colonne Organe de "corolle" à "petale"
Exertion <- rbind(Exertion, petale_data) #Ajout des nouvelles lignes avec les pétales au tableau original
#Analyse statistique - ANOVA 1 - Aire des organes floraux----
#Aire petale----
Exertion_aire_petale <- Exertion %>% 
  filter(Organe == "petale") %>% 
  reframe(Ind,
          Organe,
          Area)

#ANOVA 1 : Aire ~ Ind
mod_aire_petale <- lm(Area ~ Ind , data = Exertion_aire_petale)

#Hypothèse d'applications

#(1) Données indépendantes -> Oui
#(2) chantillonage aléatoire ~ Erreurs Indépendants -> Oui
#(3) Homoscédasitcité (Variance égale) 
plot(mod_aire_petale,1)

leveneTest(Area ~ Ind, data = Exertion_aire_petale) #Ok 
bartlett.test(Area ~ Ind, data = Exertion_aire_petale) #Ok

#(4) Normalité des résidus 
plot(mod_aire_petale,2)

hist(residuals(mod_aire_petale)) #Gaussien Ok

shapiro.test(residuals(mod_aire_petale)) #Ok

#Test - ANOVA1
mod_anova_aire_petale <- aov(Area ~ Ind, data = Exertion_aire_petale)
summary(mod_anova_aire_petale)

TukeyHSD(mod_anova_aire_petale, conf.level = 0.95)


#Comparaisons multiples

mod_aire_petale <- lm(Area ~ Ind, data=Exertion_aire_petale)
summary(mod_aire_petale)

?emmeans
emm <- emmeans(mod_aire_petale, "Ind")
cld(emm, adjust="tukey")
tukey_aire_petale <- cld(emm, adjust="tukey")

group_aire_petale <- tibble(Ind= tukey_aire_petale$Ind, Groupe =  tukey_aire_petale$.group ) %>%
  mutate(Groupe = str_replace_all(Groupe, c('1' = 'a', '2' = 'b', '3' = 'c', '4' = 'd', '5' = 'e', '6' = 'f')),
         Organe = "petale")


#Aire sepale----
Exertion_aire_sepale <- Exertion %>% 
  filter(Organe == "sepale")

#ANOVA 1 : Aire ~ Ind
mod_aire_sepale <- lm(Area ~ Ind , data = Exertion_aire_sepale)

#Hypothèse d'applications

#(1) Données indépendantes -> Oui
#(2) chantillonage aléatoire ~ Erreurs Indépendants -> Oui
#(3) Homoscédasitcité (Variance égale) 
plot(mod_aire_sepale,1) #OK

leveneTest(Area ~ Ind, data = Exertion_aire_sepale) #Pas ok ?? mais graphique OK


#(4) Normalité des résidus 
plot(mod_aire_sepale,2) #Ok

hist(residuals(mod_aire_sepale)) #Gaussien Ok

shapiro.test(residuals(mod_aire_sepale)) #Ok

#Test - ANOVA1
mod_anova_aire_sepale <- aov(Area ~ Ind, data = Exertion_aire_sepale)
summary(mod_anova_aire_sepale)

TukeyHSD(mod_anova_aire_sepale, conf.level = 0.95)

#Comparaisons multiples

mod_aire_sepale <- lm(Area ~ Ind, data=Exertion_aire_sepale)
summary(mod_aire_sepale)

emm <- emmeans(mod_aire_sepale, "Ind")
cld(emm, adjust="tukey")
tukey_aire_sepale <- cld(emm, adjust="tukey")

group_aire_sepale <- tibble(Ind= tukey_aire_sepale$Ind, Groupe =  tukey_aire_sepale$.group ) %>%
  mutate(Groupe = str_replace_all(Groupe, c('1' = 'a', '2' = 'b', '3' = 'c', '4' = 'd', '5' = 'e', '6' = 'f')),
         Organe = "sepale")

#Aire etamine----
Exertion_aire_etamine <- Exertion %>% 
  filter(Organe == "etamine") %>% 
  reframe(Ind,
          Organe,
          Area)

#ANOVA 1 : Aire ~ Ind
mod_aire_etamine <- lm(Area ~ Ind , data = Exertion_aire_etamine)

#Hypothèse d'applications

#(1) Données indépendantes -> Oui
#(2) chantillonage aléatoire ~ Erreurs Indépendants -> Oui
#(3) Homoscédasitcité (Variance égale) 
plot(mod_aire_etamine,1) #Ok

leveneTest(Area ~ Ind, data = Exertion_aire_etamine) #Pas ok mais graphique ok


#(4) Normalité des résidus 
plot(mod_aire_etamine,2)

hist(residuals(mod_aire_etamine)) #Gaussien Ok

shapiro.test(residuals(mod_aire_etamine)) #Ok

#Test - ANOVA1
mod_anova_aire_etamine <- aov(Area ~ Ind, data = Exertion_aire_etamine)
summary(mod_anova_aire_etamine)

TukeyHSD(mod_anova_aire_etamine, conf.level = 0.95)

#Comparaisons multiples

mod_aire_etamine <- lm(Area ~ Ind, data=Exertion_aire_etamine)
summary(mod_aire_etamine)

emm <- emmeans(mod_aire_etamine, "Ind")
cld(emm, adjust="tukey")
tukey_aire_etamine <- cld(emm, adjust="tukey")

group_aire_etamine <- tibble(Ind= tukey_aire_etamine$Ind, Groupe =  tukey_aire_etamine$.group ) %>%
  mutate(Groupe = str_replace_all(Groupe, c('1' = 'a', '2' = 'b', '3' = 'c', '4' = 'd', '5' = 'e', '6' = 'f')),
         Organe = "etamine")

#Graphiques - Aire des organes floraux -----
Exertion_tout <- Exertion %>%
  filter(Organe != "corolle") %>% 
  reframe(Ind,
          Organe,
          Area)

Exertion_tout_resum <- Exertion_tout %>% 
  group_by(Ind,Organe) %>% 
  summarise(mean = mean(Area),
            var = var(Area),
            sd = sd(Area),
            N = n()) %>% 
  mutate(SD_S = mean + sd,
         SD_I = mean - sd,
         CI_S = mean + qt(0.975, N-1)*sqrt(var/N),
         CI_I = mean - qt(0.975, N-1)*sqrt(var/N))

group_aire_tout <- bind_rows(group_aire_petale,group_aire_sepale,group_aire_etamine) %>%
  mutate(Groupe = str_trim(Groupe, side = "both"))


Exertion_tout_resum <- Exertion_tout_resum %>%
  left_join(group_aire_tout, by = c("Ind", "Organe"))

#Graphiques
plot_names <- c('corolle' = "Corolle",
                'etamine' = "Étamine",
                'petale' = "Pétale",
                'pistile' = "Pistil",
                'sepale' = "Sépale")

Exertion_tout_resum <- within(Exertion_tout_resum, Organe <- factor(Organe, levels=c('petale', 'sepale', 'etamine','pistile')))
Exertion_tout <- within(Exertion_tout, Organe <- factor(Organe, levels=c('petale', 'sepale', 'etamine','pistile')))

#Figure 18 - Aire des organes floraux des différents individus de S. chilense
Exertion_tout_graph <-  Exertion_tout_resum %>% 
  filter(Organe != "pistile")

ggplot(Exertion_tout_graph, aes(x = Ind, y = mean))+
  geom_bar(stat = "identity", aes(fill = Ind))+
  geom_errorbar(aes(ymin = SD_I, ymax = SD_S), color = "black", width=.3 ,size = 0.75)+
  labs(title = "Aire des organes floraux",
       x = "Indiviudu",
       y = expression(paste("Aire ", "(",cm^{2},")")),
       fill = "Individu")+
  geom_text(mapping = aes(x = Ind, y = SD_S *1.1, label = Groupe), size = 4)+
  scale_fill_manual(values = colors)+
  facet_wrap(vars(Organe), scales = "free",labeller = as_labeller(plot_names))+
  theme(text = element_text(size = 15, family = "Aptos"),
        strip.text = element_text(size = 12, family = "Aptos"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 11, family = "Aptos"),
        axis.text.y = element_text(family = "Aptos"))

#Donnée d'air minmal et maximal des individus de S. chilense avec leur nom
Exertion_tout_graph %>% 
  group_by(Organe) %>% 
  summarize(ind_min = Ind[which.min(mean)],
            min = min(mean),
            ind_max = Ind[which.max(mean)],
            max = max(mean))

#Analyse statistique - ANOVA 1 - Longueur des étamines et des pistils----
#Longueur etamine----
Exertion_lg_etamine <- Exertion %>% 
  filter(Organe == "etamine") %>% 
  reframe(Ind,
          Organe,
          Length)

#ANOVA 1 : Lentgh ~ Ind
mod_lg_etamine <- lm(Length ~ Ind , data = Exertion_lg_etamine)

#Hypothèse d'applications

#(1) Données indépendantes -> Oui
#(2) Echantillonage aléatoire ~ Erreurs Indépendants -> Oui
#(3) Homoscédasitcité (Variance égale) 
plot(mod_lg_etamine,1)

leveneTest(Length ~ Ind, data = Exertion_lg_etamine) #Pas ok mais graphique ok


#(4) Normalité des résidus 
plot(mod_lg_etamine,2)

hist(residuals(mod_lg_etamine)) #Gaussien Ok

shapiro.test(residuals(mod_lg_etamine)) #Pas Ok mais graphique ok
 
#Test - ANOVA1
mod_anova_lg_etamine <- aov(Length ~ Ind, data = Exertion_lg_etamine)
summary(mod_anova_lg_etamine)

TukeyHSD(mod_anova_lg_etamine, conf.level = 0.95)

#Comparaisons multiples

mod_lg_etamine <- lm(Length ~ Ind, data=Exertion_lg_etamine)
summary(mod_lg_etamine)

emm <- emmeans(mod_lg_etamine, "Ind")
cld(emm, adjust="tukey")
tukey_lg_etamine <- cld(emm, adjust="tukey")

group_lg_etamine <- tibble(Ind= tukey_lg_etamine$Ind, Groupe =  tukey_lg_etamine$.group ) %>%
  mutate(Groupe = str_replace_all(Groupe, c('1' = 'a', '2' = 'b', '3' = 'c', '4' = 'd', '5' = 'e', '6' = 'f')),
         Organe = "etamine")

#Longueur Pistil----
Exertion_lg_pistil <- Exertion %>% 
  filter(Organe == "pistile") %>% 
  reframe(Ind,
          Organe,
          Length)

#ANOVA 1 : Length ~ Ind
mod_lg_pistil <- lm(Length ~ Ind , data = Exertion_lg_pistil)

#Hypothèse d'applications

#(1) Données indépendantes -> Oui
#(2) chantillonage aléatoire ~ Erreurs Indépendants -> Oui
#(3) Homoscédasitcité (Variance égale) 
plot(mod_lg_pistil,1)

leveneTest(Length ~ Ind, data = Exertion_lg_pistil) #Ok

#(4) Normalité des résidus 
plot(mod_lg_pistil,2)

hist(residuals(mod_lg_pistil)) #Gaussien Ok

shapiro.test(residuals(mod_lg_pistil)) #Ok

#Test - ANOVA1
mod_anova_lg_pistil <- aov(Length ~ Ind, data = Exertion_lg_pistil)
summary(mod_anova_lg_pistil)

TukeyHSD(mod_anova_lg_pistil, conf.level = 0.95)

#Comparaisons multiples

mod_lg_pistil <- lm(Length ~ Ind, data=Exertion_lg_pistil)
summary(mod_lg_pistil)

emm <- emmeans(mod_lg_pistil, "Ind")
cld(emm, adjust="tukey")
tukey_lg_pistil <- cld(emm, adjust="tukey")

group_lg_pistil <- tibble(Ind= tukey_lg_pistil$Ind, Groupe =  tukey_lg_pistil$.group ) %>%
  mutate(Groupe = str_replace_all(Groupe, c('1' = 'a', '2' = 'b', '3' = 'c', '4' = 'd', '5' = 'e', '6' = 'f')),
         Organe = "pistile")

#Graphiques - Longueur des étamines et des pistils----
Exertion_long <- Exertion %>%
  filter(Organe == "etamine" | Organe == "pistile") %>% 
  reframe(Ind,
          Organe,
          Length)

Exertion_long_resum <- Exertion_long %>% 
  group_by(Ind,Organe) %>% 
  summarise(mean = mean(Length),
            var = var(Length),
            sd = sd(Length),
            N = n()) %>% 
  mutate(SD_S = mean + sd,
         SD_I = mean - sd,
         CI_S = mean + qt(0.975, N-1)*sqrt(var/N),
         CI_I = mean - qt(0.975, N-1)*sqrt(var/N))


group_lg_tout <- bind_rows(group_lg_etamine,group_lg_pistil)%>%
  mutate(Groupe = str_trim(Groupe, side = "both"))

Exertion_long_resum <- Exertion_long_resum %>%
  left_join(group_lg_tout, by = c("Ind", "Organe"))

#Graphiques de longueurs des étamines et pistils
plot_names <- c('corolle' = "Corolle",
                'etamine' = "Étamine",
                'petale' = "Pétale",
                'pistile' = "Style",
                'sepale' = "Sépale")

Exertion_long_resum <- within(Exertion_long_resum, Organe <- factor(Organe, levels=c('petale', 'sepale', 'etamine','pistile')))
Exertion_long <- within(Exertion_long, Organe <- factor(Organe, levels=c('petale', 'sepale', 'etamine','pistile')))


#Figure 19 - Longeur moyenne des organes floraux des différents individus de S. chilense
ggplot(Exertion_long_resum, aes(x = Ind, y = mean))+
  geom_bar(stat = "identity", aes(fill = Ind))+
  geom_errorbar(aes(ymin = SD_I, ymax = SD_S), color = "black", width=.3 ,size = 0.75)+
  labs(title = "Longueur des organes floraux",
       x = "Indiviudu",
       y = expression(paste("Longueur ", "(",cm,")")),
       fill = "Individu")+
  geom_text(mapping = aes(x = Ind, y = SD_S *1.1, label = Groupe))+
  scale_fill_manual(values = colors)+
  facet_wrap(vars(Organe), scales = "free",labeller = as_labeller(plot_names))+
  theme(text = element_text(size = 15, family = "Aptos"),
        strip.text = element_text(size = 12, family = "Aptos"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 11, family = "Aptos"),
        axis.text.y = element_text(family = "Aptos"))

#Données texte
#Min et max Lonngueur
Exertion_long_resum %>% 
  group_by(Organe) %>% 
  summarize(ind_min = Ind[which.min(mean)],
            min = min(mean),
            ind_max = Ind[which.max(mean)],
            max = max(mean),
            rap = (max-min)/min*100)



#Analyse statistique - ANOVA 1 - Rapport Taille pistil / taille etamine ----
Exertion_stigmate <- Exertion %>%
  filter(Organe == "etamine" | Organe == "pistile") %>% 
  group_by(Ind,Nb_ech,Organe) %>% 
  summarise(mean = mean(Length)) %>% 
  spread(key = Organe, value = mean) %>% 
  summarise(rapport = pistile/etamine)

mod_exertion_stigmate <- lm(rapport ~ Ind , data = Exertion_stigmate)

#Hypothèse d'applications

#(1) Données indépendantes -> Oui
#(2) chantillonage aléatoire ~ Erreurs Indépendants -> Oui
#(3) Homoscédasitcité (Variance égale) 
plot(mod_exertion_stigmate,1)

leveneTest(rapport ~ Ind, data = Exertion_stigmate) #Ok

#(4) Normalité des résidus 
plot(mod_exertion_stigmate,2)

hist(residuals(mod_exertion_stigmate)) #Gaussien Ok

shapiro.test(residuals(mod_exertion_stigmate)) #Ok

#Test - ANOVA1
mod_anova_exertion_stigmate <- aov(rapport ~ Ind, data = Exertion_stigmate)
summary(mod_anova_exertion_stigmate)

TukeyHSD(mod_anova_lg_pistil, conf.level = 0.95)

#Comparaisons multiples

mod_exertion_stigmate <- lm(rapport ~ Ind , data = Exertion_stigmate)
summary(mod_exertion_stigmate)

emm <- emmeans(mod_exertion_stigmate, "Ind")
cld(emm, adjust="tukey")
tukey_exertion_stigmate <- cld(emm, adjust="tukey")

group_exertion_stigmate <- tibble(Ind= tukey_exertion_stigmate$Ind, Groupe = tukey_exertion_stigmate$.group ) %>%
  mutate(Groupe = str_replace_all(Groupe, c('1' = 'a', '2' = 'b', '3' = 'c', '4' = 'd', '5' = 'e', '6' = 'f')),
         Organe = "rapport")%>%
  mutate(Groupe = str_trim(Groupe, side = "both"))

#Graphiques - Exertion du stigmate (Rapport Taille pistil / taille etamine)----
Exertion_stigmate_resum <- Exertion_stigmate %>% 
  group_by(Ind) %>% 
  summarise(mean = mean(rapport),
            var = var(rapport),
            sd = sd(rapport),
            N = n()) %>% 
  mutate(SD_S = mean + sd,
         SD_I = mean - sd,
         CI_S = mean + qt(0.975, N-1)*sqrt(var/N),
         CI_I = mean - qt(0.975, N-1)*sqrt(var/N))

Exertion_stigmate_resum <- Exertion_stigmate_resum %>%
  left_join(group_exertion_stigmate, by = c("Ind"))

#Figure 20 - Exsertion moyenne des stigmates des différents individus de S. chilense
ggplot(Exertion_stigmate_resum, aes(x = Ind, y = mean))+
  geom_bar(stat = "identity", aes(fill = Ind))+
  geom_errorbar(aes(ymin = SD_I, ymax = SD_S), color = "black", width=.3, size = 0.75)+
  labs(title = "Exertion stimagtique",
       x = "Indiviudu",
       y = expression(italic(Lg)[italic(style)] / italic(Moy)(italic(Lg)[italic(étamines)])),
       fill = "Individu")+
  geom_text(mapping = aes(x = Ind, y = SD_S *1.1, label = Groupe))+
  scale_fill_manual(values = colors)+
  theme(text = element_text(size = 12, family = "Aptos"),
        strip.text = element_text(size = 12, family = "Aptos"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10, family = "Aptos"),
        axis.text.y = element_text(family = "Aptos"))

#Analyse à composante principale (ACP)----
#Réarrangement des données
#On utilise que les donnés d'aire de sépale et pétale, et longeur d'étamine et de style
Exertion_ACP <- Exertion %>% 
  group_by(Ind, Organe,Nb_ech) %>% 
  summarise(Mean_Area = mean(Area),
            Mean_Length = mean(Length)) %>% 
  pivot_wider(names_from = Organe,
              values_from = c(Mean_Area,Mean_Length)) %>% 
  dplyr::select(-c(Mean_Length_petale, 
                   Mean_Length_sepale, 
                   Mean_Area_corolle,
                   Mean_Area_pistile,
                   Mean_Length_corolle)) %>% 
  rename(Length_Style = Mean_Length_pistile)



#Première méthode - avec des données normalisée----
Exertion_ACP

Exertion_ACP_norm <- scale(Exertion_ACP[,3:7])
rownames(Exertion_ACP_norm) <- paste0(Exertion_ACP$Ind,"_",Exertion_ACP$Nb_ech)

apply(Exertion_ACP_norm, 2, sd) #Ok

ACP_Norm <- prcomp(Exertion_ACP_norm) #Création de l'ACP_Norm
?prcomp
?princomp
#Deuxième méthode - avec la matrice de correlation, des donnée standardisée----
#Normalement utilisation de la matrice variance-covariance si les données sont sur la même échelle.
#Donc 
#Utilisation de la matrice de corrélation comme input de la PCA
#Plusieurs variables sont mesurées et sont sur  différentes échelles


#Calcul de la matrice de correlation
Exertion_ACP_std <- cor(Exertion_ACP[,3:7])

ACP_cor <-  princomp(Exertion_ACP[,3:7], cor = TRUE)

sum(ACP_cor$sdev^2)

#Particularité lorsqu'on utilise une matrice de corrélation pour la PCA
#la somme des valeurs propres (eigenvalues, ??) = nombre total de variables (ici = 4)

#Coefficients (loadings)
ACP_cor$loadings
#L'axe 1 augmente avec toutes les varaibles
#L'axe 2 augmente uniquement avec les varaible d'air de pétale et de sépale et diminue avec les longeurs d'étamine et de pistils

#Combien d'axes choisir ?----- 
#Regardons les différentes stratégies explorées dans
#Jackson 1993. Ecology 74:2204-2214; Peres-Neto et al. 2005. Computational Statistics and Data Analysis 49:974-997)

#(1) Fixer une valeur de varience à expliquer (par exemple 75%) et déterminer le nombre d'axes nécesaire pour y arriver

ACP_cor$sdev[1]^2/5 + ACP_cor$sdev[2]^2/5
ACP_Norm$sdev[1]^2/5 + ACP_Norm$sdev[2]^2/5
#On retiendrais l'axe 1 et l'axe 2, la somme de leur variance est de 77%


#(2) Retenir uniquement les axes qui ont une eingvalues > 1 (pour une matrice de correlation)
ACP_cor$sdev^2
ACP_Norm$sdev^2


#On ne retiendrais que l'axe 1

#(3) Regarder le scree plot
#On utilise les points qui se démarquent de la ligne presqu'horizontale de la portion droite du graphique (les débris)
par(mar = c(5, 4.5, 4, 2))
plot(x = 1:5, y = ACP_cor$sdev^2, xlab = "PCA axis",
     ylab = expression(paste("Eigenvalue (", italic(lambda), " )")),
     type = "b", main = "Scree plot",
     xaxt = "n",
     cex = 1.2, cex.axis = 1.2, cex.lab = 1.2)+
axis(side = 1,
     at = 1:5, cex.axis = 1.2, cex.lab = 1.2)

plot(x = 1:5, y = ACP_Norm$sdev^2, xlab = "PCA axis",
     ylab = expression(paste("Eigenvalue (", italic(lambda), " )")),
     type = "b", main = "Scree plot",
     xaxt = "n",
     cex = 1.2, cex.axis = 1.2, cex.lab = 1.2)+
  axis(side = 1,
       at = 1:5, cex.axis = 1.2, cex.lab = 1.2)


#Ou 
screeplot(ACP_cor)
screeplot(ACP_Norm)
#On retiendrais l'axe 1 et 2

#(4) La méthode du Broken stick

#Broken stick model yields the expected proportion of variance - once a trivial (or unimportant) axis is uncovered, all other axes
#are considered non significant (check Jackson 1993, or Peres-Neto et al. 2003, 2004)

#single argument p, corresponds to number of PCA axes considered
#broken_stick(p=5)

#set up values to be summed
broken_stick<-function(p=2) {
  b_stick<-data.frame(k=1:p, prop=0)
  for(i in 1:p) {
    b_stick$prop[i]<-1/b_stick$k[i]
  }
  #sum values
  b_stick$csum<-1
  for (j in 1:p) {
    b_stick$csum[j]<-sum(b_stick$prop[j:p])
  }
  #generate expected values
  b_stick$expected<-b_stick$csum/p
  return(b_stick[,c("k", "expected")])
}

broken_stick_4 <- broken_stick(p = 5)

#Regarder si ces valeurs sont < aux variance
ACP_cor$sdev[1]^2/5 > broken_stick_4[1,2]
ACP_cor$sdev[2]^2/5 > broken_stick_4[2,2]
ACP_cor$sdev[3]^2/5 > broken_stick_4[3,2]
ACP_cor$sdev[4]^2/5 > broken_stick_4[4,2]
ACP_cor$sdev[5]^2/5 > broken_stick_4[5,2]

ACP_Norm$sdev[1]^2/5 > broken_stick_4[1,2]
ACP_Norm$sdev[2]^2/5 > broken_stick_4[2,2]
ACP_Norm$sdev[3]^2/5 > broken_stick_4[3,2]
ACP_Norm$sdev[4]^2/5 > broken_stick_4[4,2]
ACP_Norm$sdev[5]^2/5 > broken_stick_4[5,2]

#On choisirai l'axe 1 et 4

#(5) Randomisation des observation et calcul des eigenvalues


#Résultat des ACP----


summary(ACP_Norm)
summary(ACP_cor)

#Résultat identique


ACP_Norm$rotation
#On va utiliser les deux premiers composants, ou axe pour le graphique

#Figure 21 : Analyse en composante principale (ACP) de la surface et de la taille des organes floraux des individus de S. chilense. 
#Graphique des individus pour ACP_Norm (prcomp)
fviz_pca_ind(ACP_Norm, 
             geom.ind = "point",                   # Utiliser des points pour les individus
             habillage = Exertion_ACP$Ind,    # Colorer selon la colonne "Ind"
             palette = colors,                  # Palette de couleurs
             addEllipses = TRUE,               # Ajouter des ellipses pour chaque groupe
             ellipse.level = 0.85,             # Niveau de confiance des ellipses
             repel = FALSE,
             pointsize = 1.5,
             mean.point = FALSE) + 
  scale_shape_manual(values = rep(16, length(unique(Exertion_ACP$Ind)))) +
  theme(text = element_text(size = 12, family = "Aptos"),
        strip.text = element_text(size = 12, family = "Aptos"),
        axis.text.x = element_text(hjust = 1, size = 11, family = "Aptos"),
        axis.text.y = element_text(family = "Aptos"),
        panel.background = element_rect(fill = "grey70"),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_blank()
        )+
  labs(x = "Dim1 (52.1%)",
       y = "Dim2 (25.9%)")

# Graphique des variables pour ACP_Norm
fviz_pca_var(ACP_Norm, col.var = "contrib", gradient.cols = c("#135B67", "#B698D7", "#9A099A"), repel = TRUE)+
  theme(text = element_text(size = 12, family = "Aptos"),
        strip.text = element_text(size = 12, family = "Aptos"),
        axis.text.x = element_text(hjust = 1, size = 11, family = "Aptos"),
        axis.text.y = element_text(family = "Aptos"))+
  labs(x = "Dim1 (52.1%)",
       y = "Dim2 (25.9%)",
       col = "Contribution (%)")

ACP_Norm$x


##----Partie 2.1 - Taux de mise à fruit ----
#Importation des donnees
Croisement <- read.csv("R_Croisements.csv", sep = ";", dec = ",") 

Croisement$Parent_Femelle <- as.factor(Croisement$Parent_Femelle )
Croisement$Parent_Male <- as.factor(Croisement$Parent_Male)
Croisement$Qui <- as.factor(Croisement$Qui)


#Selection des individu utilisé
Croisement <- Croisement %>% 
  filter(Parent_Femelle %in% c("PIS10", "PIS14", "PIS19", "PIS31", "PIS33", "PIS34", "PIS36","PIS38","PIS40","SC2")) %>% 
  filter(Parent_Male %in% c("PIS10", "PIS14", "PIS19", "PIS31", "PIS33", "PIS34", "PIS36", "PIS38", "PIS40","SC2")) %>% 
  filter(N_Fruit_tot != "NA")



#Graphique - Tableaux résumés des croisements----
Croisement_resmu <- Croisement %>% 
  group_by(Parent_Femelle,Parent_Male) %>% 
  summarise(Sum_Flower = sum(N_Fleurs_Polli),
            Sum_Fruit = sum(N_Fruit_tot),
            Fruit_set = (Sum_Fruit/Sum_Flower)*100) %>% 
  mutate(Category = case_when(
    Fruit_set == 0 ~ "0%",
    Fruit_set <= 10 ~ "10% ou moins",
    Fruit_set > 10 & Fruit_set <= 50 ~ "Entre 10 et 50%",
    Fruit_set > 50 & Fruit_set <= 75 ~ "Entre 50 et 75%",
    Fruit_set > 75 ~ "Plus de 75%",
  ))

#Nombre de croisements totaux
sum(Croisement_resmu$Sum_Flower)


#Tableau 10 - Synthèse des croisements réalisés entre dix individus de S. chilense et taux de mise à fruit. 
ggplot(Croisement_resmu, aes(y = Parent_Femelle, x = Parent_Male, fill = Fruit_set)) +    
  scale_x_discrete(position = "top")+
  scale_y_discrete(limits = rev(levels(factor(Croisement_resmu$Parent_Male))))+
  geom_tile(color = "white",lwd = 1.5,linetype = 1)+
  geom_text(aes(label = Sum_Flower), size = 5, family = "Aptos")+
  coord_fixed() +
  guides(fill = guide_colourbar(barwidth = 0.5, 
                                barheight = 10, 
                                ticks = FALSE, 
                                nbin = 100))+
  scale_fill_gradientn(
    colors = c("#4c4c4c", "#691515", "white", "darkgreen"),
    values = scales::rescale(c(0, 0.5,50,100)), 
    breaks = c(0.5, 20, 40, 60, 76),  # Fixer les breaks pour une meilleure graduation
    labels = c("0.5%", "20%", "40%", "60%", "76%"),
    na.value = "black", 
    guide = "colorbar") +
  
  labs(title = "Taux de mise à fruit des différents croisements",
       y = "Parent femelle",
       x = "Parent mâle",
       fill = "Taux de mise 
       à fruit (%)")+
  
  theme(text = element_text(size = 12, family = "Aptos"),
        strip.text = element_text(size = 12, family = "Aptos"),
        axis.text.x = element_text(size = 12, family = "Aptos"),
        axis.text.y = element_text(size = 12, family = "Aptos"),
        axis.title = element_text(size = 14, family = "Aptos"))

##----Partie 2.2 - Nombre moyen de graine par fruit ----
#Importation des donnees
Graine <- read.csv("R_Graine.csv", sep = ";", dec = ",") 

Graine$Parent_Femelle <- as.factor(Graine$Parent_Femelle )
Graine$Parent_Male <- as.factor(Graine$Parent_Male)

#Graphique - Tableaux résumés des nombre moyen de graines----
#Tableau 11 - Synthèse du nombre fruits disséqués issus des entre dix individus de S. chilense. 
ggplot(Graine, aes(y = Parent_Femelle, x = Parent_Male, fill = N_Moy_Graine)) +    
  scale_x_discrete(position = "top")+
  scale_y_discrete(limits = rev(levels(factor(Graine$Parent_Male))))+
  geom_tile(color = "white",lwd = 1.5,linetype = 1)+
  geom_text(aes(label = N_Fruit_Mesur),size = 5, family = "Aptos")+
  coord_fixed() +
  guides(fill = guide_colourbar(barwidth = 0.5,barheight = 10))+
  scale_fill_gradientn(
    colors = c("#D7771E", "white", "#4774C3"),
    values = scales::rescale(c(0,15,30)), 
    breaks = c(3, 10, 20, 30),  # Fixer les breaks pour une meilleure graduation
    labels = c("3","10", "20", "30"),
    na.value = "black", 
    guide = "colorbar") +
  
  labs(title = "Nombre moyen de graine par fruit",
       y = "Parent femelle",
       x = "Parent mâle",
       fill = "Nombre moyen de 
       graine par fruit")+

  theme(text = element_text(size = 12, family = "Aptos"),
        strip.text = element_text(size = 12, family = "Aptos"),
        axis.text.x = element_text(size = 12, family = "Aptos"),
        axis.text.y = element_text(size = 12, family = "Aptos"),
        axis.title = element_text(size = 14, family = "Aptos"))


#Tableau - Donnée de taux de mise à fruit et nombre de graine (PARTIE 2.1/2.2)----
data_table_croisement_1 <-  Croisement_resmu %>% 
  mutate(Croisement = paste(Parent_Femelle,"x",Parent_Male),
         Nb_Croisement = Sum_Flower,
         Nb_Fruit = Sum_Fruit,
         Taux_mise_a_fruit = round(Fruit_set,2)) %>% 
  ungroup() %>% 
  dplyr::select(Croisement, Nb_Croisement, Nb_Fruit, Taux_mise_a_fruit)

data_table_croisement_2 <- Graine %>% 
  mutate(Croisement = paste(Parent_Femelle,"x",Parent_Male),
         N_Moy_Graine = round(N_Moy_Graine,2)) %>% 
  dplyr::select(Croisement, N_Fruit_Mesur, N_Graine_tot, N_Moy_Graine) 

data_table_croisement <- left_join(data_table_croisement_1, data_table_croisement_2, by = "Croisement")

table_croisement <- data_table_croisement %>% 
  gt() %>%
  tab_header(
    title = "Donnée des croisements interspécifique de S.chilense",
    subtitle = ""
  ) %>%
  cols_label(
    Croisement = html("Croisement<br>??? x ???"),
    Nb_Croisement = html("Nombre de<br>croisement réalisé"),
    Nb_Fruit = html("Nombre de<br>fruit"),
    Taux_mise_a_fruit = html("Taux de<br>mise à fruit"),
    N_Fruit_Mesur = html("Nombre de<br>fruit dissequé"),
    N_Graine_tot = html("Nombre de<br>graine"),
    N_Moy_Graine = html("Nombre moyen de<br>graine par fruit")
  ) %>%
  fmt_number(
    columns = vars(Taux_mise_a_fruit, N_Moy_Graine),
    decimals = 2
  ) %>%
  
  #Ajouter un groupement de colonnes (deuxième ligne d'en-tête)
  tab_spanner(
    label = "Tableau 9",
    columns = vars(Nb_Croisement,Nb_Fruit, Taux_mise_a_fruit)
  ) %>%
  tab_spanner(
    label = "Tableau 10",
    columns = vars(N_Fruit_Mesur, N_Graine_tot, N_Moy_Graine)
  ) %>% 
  
  #Couleurs
  data_color(
    columns = vars(Taux_mise_a_fruit),
    colors = col_numeric(
      palette = c("#691515", "white", "darkgreen"),  # Palette de couleurs pour le gradient
      domain = c(0.5, 78),
      na.color = "grey20" # Les points spécifiques pour le dégradé
    )
  ) %>% 
  data_color(
    columns = vars(N_Moy_Graine),
    colors = col_numeric(
      palette = c("#D7771E", "white", "#4774C3"),  # Palette de couleurs pour le gradient
      domain = c(0.5, 31),
      na.color = "grey20")
  ) %>%
  tab_options(
    table.font.size = 11,
    data_row.padding =px(3),
  )  

#Annexe 4 - Données des croisements de S. chilense
table_croisement

gtsave(table_croisement, filename = "table_croisements.png")



##----Partie 2.3 - Croissance tube pollinique----
Tube_polli <- read.csv("R_Tube_polli.csv", sep = ";", dec = ",") 


Tube_polli_prop <- Tube_polli %>% 
  mutate(prop0 = Germe/Pollen,
         prop1 = (Prolongation1)/Germe,
         prop2 = (Prologation2)/(Prolongation1),
         prop3 = (Prolongation3)/(Prologation2)) %>% 
  pivot_longer(cols = prop0:prop3,
               names_to = "Tier",
               values_to = "Proportion") 

Tube_polli_resume <- Tube_polli_prop %>% 
  na.omit() %>% 
  mutate(Croisement = paste(Fleurs_femelle, "x",Pollen_male)) %>% 
  group_by(Croisement,Tier) %>% 
  summarise(mean = mean(Proportion),
            var = var(Proportion),
            sd = sd(Proportion),
            N = n()) %>% 
  mutate(SD_S = if_else(mean + sd > 1, 1, mean + sd),
         SD_I = if_else(mean - sd < 0, 0, mean - sd))


#Analyse statistique - GLM Binomiale -----
#Annexe 5 - Tableaux des p-valeurs issus des modèles de régression logistique binomiale pour l'analyse statistiques des taux germinations du pollen et des croissances des tubes polliniques dans les différents tiers du styles, chez six croisements de S. chilense.
#GLM Binomiale
Tube_polli_stat <- Tube_polli %>% 
  mutate(Croisement = paste(Fleurs_femelle, "x",Pollen_male))

Tube_polli_stat$Croisement <- factor(Tube_polli_stat$Croisement)


Tube_polli_stat$Croisement <- factor(Tube_polli_stat$Croisement, levels = c("PIS10 x PIS19", 
                                                                            "PIS36 x PIS19",
                                                                            "PIS10 x PIS14",
                                                                            "PIS14 x PIS40",
                                                                            "PIS14 x SC2",
                                                                            "PIS40 x PIS14"))
#Taux de germination des grains de pollen
#Hypothèse d'application

#(1) Données indépendantes -> Oui
#(2) La relation entre les prédicteurs et la probabilité de l'événement suit une transformation logit.
model_1 <- glm(cbind(Germe, Pollen - Germe) ~ Croisement, family = binomial(link = "logit"), data = Tube_polli_stat)

#plot(model_1) #OK

#Test GLM
summary(model_1)

#Analyse en changent à chaque fois le niveaux de référence 
# Fonction pour créer un tableau des résultats pairwise
analyze_crossings_1 <- function(data, response, predictor, levels_order) {
  results <- list()  # Initialiser une liste pour stocker les résultats
  
  # Boucler sur chaque niveau à définir comme référence
  for (ref_level in levels_order) {
    # Changer le niveau de référence
    data[[predictor]] <- factor(data[[predictor]], levels = c(ref_level, setdiff(levels_order, ref_level)))
    
    # Créer et ajuster le modèle GLM
    model <- glm(cbind(Germe, Pollen - Germe) ~ Croisement, 
                 family = binomial(link = "logit"), 
                 data = data)
    
    # Sauvegarder le résumé du modèle
    results[[ref_level]] <- summary(model)
  }
  
  return(results)  # Retourner tous les résultats
}


# Ordre des croisements à utiliser comme niveaux de référence
levels_order <- c("PIS10 x PIS19", "PIS36 x PIS19", "PIS10 x PIS14", 
                  "PIS14 x PIS40", "PIS14 x SC2", "PIS40 x PIS14")

# Appeler la fonction
results <- analyze_crossings_1(data = Tube_polli_stat, 
                               response = c("Germe", "Pollen"), 
                               predictor = "Croisement", 
                               levels_order = levels_order)

# Afficher les résultats pour chaque niveau de référence
results[["PIS10 x PIS19"]]  # Par exemple, voir les résultats pour ce niveau de référence

#Fonction pour extraire la Z values et p-value
extract_results <- function(results, levels_order) {
  # Initialiser une matrice carrée pour tous les croisements
  z_matrix <- matrix(NA, nrow = length(levels_order), ncol = length(levels_order),
                     dimnames = list(levels_order, levels_order))
  p_matrix <- matrix(NA, nrow = length(levels_order), ncol = length(levels_order),
                     dimnames = list(levels_order, levels_order))
  
  # Parcourir les résultats par niveau de référence
  for (ref_level in levels_order) {
    model_summary <- results[[ref_level]]
    
    # Extraire les coefficients (sauf l'intercept)
    coeffs <- coef(model_summary)
    coeffs <- coeffs[-1, , drop = FALSE]  # Retirer l'intercept
    
    # Adapter les noms des coefficients
    rownames(coeffs) <- gsub("Croisement", "", rownames(coeffs))
    
    # Obtenir les Z-values et p-values
    z_values <- coeffs[, "z value", drop = TRUE]
    p_values <- coeffs[, "Pr(>|z|)", drop = TRUE]
    
    # Correspondance des colonnes dans la matrice
    for (crossing in rownames(coeffs)) {
      if (crossing %in% levels_order) {
        z_matrix[ref_level, crossing] <- z_values[crossing]
        p_matrix[ref_level, crossing] <- p_values[crossing]
      }
    }
  }
  
  return(list(z_values = z_matrix, p_values = p_matrix))
}


# Appliquer la fonction pour extraire les Z-values et p-values
results_summary <- extract_results(results, levels_order)
results_summary


#Fonction pour crée un tableau
create_gt_table_pvalues <- function(results_summary) {
  # Extraire uniquement les p-values
  p_values <- results_summary$p_values
  
  # Remplacer NA par des chaînes vides et ajouter '*' pour les p-values significatives
  p_values_cleaned <- matrix(
    ifelse(is.na(p_values), "", 
           ifelse(p_values < 0.05, 
                  sprintf("%.3e*", p_values),  # Notation scientifique pour les p-values significatives
                  sprintf("%.3e", p_values))),  # Notation scientifique pour les autres p-values
    nrow = nrow(p_values), 
    dimnames = dimnames(p_values)
  )
  
  # Convertir la matrice en data frame pour `gt`
  p_values_df <- as.data.frame(p_values_cleaned, row.names = rownames(p_values))
  p_values_df <- tibble::rownames_to_column(p_values_df, "Référence")
  
  # Créer le tableau gt avec seulement les p-values
  gt_table <- p_values_df %>%
    gt() %>%
    tab_header(
      title = "P-values des Comparaisons Statistiques",
      subtitle = "P-values pour chaque croisement"
    ) %>%
    fmt_missing(
      columns = everything(),
      missing_text = ""  # Afficher une chaîne vide pour les NA
    ) %>%
    cols_align(
      align = "center",
      columns = everything()
    ) %>%
    tab_style(
      style = list(
        cell_text(weight = "bold")
      ),
      locations = cells_body(columns = "Référence")
    ) %>%
    tab_style(
      style = list(
        cell_text(weight = "bold")
      ),
      locations = cells_column_labels(columns = everything())  # Mettre en gras les en-têtes de colonnes
    ) %>%
    tab_options(
      table.font.size = "small",
      table.border.top.width = px(2),
      table.border.bottom.width = px(2),
      table.align = "center"
    )
  
  return(gt_table)
}

#Appliquer la fonction
gt_table_1 <- create_gt_table_pvalues(results_summary)
gt_table_1 <- gt_table_1 %>%
  tab_header(
    title = "P-values issus des GLM binomiales, en changeant le niveau de référence",  # Titre principal
    subtitle = "Formule : cbind(Germe, Pollen - Germe) ~ Croisement"  # Sous-titre optionnel
  )
print(gt_table_1)


#Nombre de TP traversant le 1er tier / Nombre pollen germé
#Hypothèse d'application

#(1) Données indépendantes -> Oui
#(2) La relation entre les prédicteurs et la probabilité de l'événement suit une transformation logit.
model_2 <- glm(cbind(Prolongation1, Germe - Prolongation1) ~ Croisement, family = binomial(link = "logit"), data = Tube_polli_stat)

#plot(model_2) #OK

#Test GLM
summary(model_2)

#Analyse en changent à chaque fois le niveaux de référence 
# Fonction pour créer un tableau des résultats pairwise
analyze_crossings_2 <- function(data, response, predictor, levels_order) {
  results <- list()  # Initialiser une liste pour stocker les résultats
  
  # Boucler sur chaque niveau à définir comme référence
  for (ref_level in levels_order) {
    # Changer le niveau de référence
    data[[predictor]] <- factor(data[[predictor]], levels = c(ref_level, setdiff(levels_order, ref_level)))
    
    # Créer et ajuster le modèle GLM
    model <- glm(cbind(Prolongation1, Germe - Prolongation1) ~ Croisement, 
                 family = binomial(link = "logit"), 
                 data = data)
    
    # Sauvegarder le résumé du modèle
    results[[ref_level]] <- summary(model)
  }
  
  return(results)  # Retourner tous les résultats
}


# Ordre des croisements à utiliser comme niveaux de référence
levels_order <- c("PIS10 x PIS19", "PIS36 x PIS19", "PIS10 x PIS14", 
                  "PIS14 x PIS40", "PIS14 x SC2", "PIS40 x PIS14")

# Appeler la fonction
results <- analyze_crossings_2(data = Tube_polli_stat, 
                               response = c("Prolongation1", "Germe"), 
                               predictor = "Croisement", 
                               levels_order = levels_order)

# Afficher les résultats pour chaque niveau de référence
results[["PIS10 x PIS19"]]  # Par exemple, voir les résultats pour ce niveau de référence

# Appliquer la fonction pour extraire les Z-values et p-values
results_summary <- extract_results(results, levels_order)
results_summary

#Appliquer la fonction
gt_table_2 <- create_gt_table_pvalues(results_summary)
gt_table_2 <- gt_table_2 %>%
  tab_header(
    title = "P-values issus des GLM binomiales, en changeant le niveau de référence",  # Titre principal
    subtitle = "Formule : cbind(Prolongation1, Germe - Prolongation1) ~ Croisement"  # Sous-titre optionnel
  )

#Nombre de TP traversant le 2e tier / Nombre de TP traversant le 1er tier
#Hypothèse d'application

#(1) Données indépendantes -> Oui
#(2) La relation entre les prédicteurs et la probabilité de l'événement suit une transformation logit.
model_3 <- glm(cbind(Prologation2, Prolongation1 - Prologation2) ~ Croisement, family = binomial(link = "logit"), data = Tube_polli_stat)

#plot(mode_3) #OK

#Test GLM
summary(model_3)

#Analyse en changent à chaque fois le niveaux de référence 
# Fonction pour créer un tableau des résultats pairwise
analyze_crossings_3 <- function(data, response, predictor, levels_order) {
  results <- list()  # Initialiser une liste pour stocker les résultats
  
  # Boucler sur chaque niveau à définir comme référence
  for (ref_level in levels_order) {
    # Changer le niveau de référence
    data[[predictor]] <- factor(data[[predictor]], levels = c(ref_level, setdiff(levels_order, ref_level)))
    
    # Créer et ajuster le modèle GLM
    model <- glm(cbind(Prologation2, Prolongation1 - Prologation2) ~ Croisement, 
                 family = binomial(link = "logit"), 
                 data = data)
    
    # Sauvegarder le résumé du modèle
    results[[ref_level]] <- summary(model)
  }
  
  return(results)  # Retourner tous les résultats
}


# Ordre des croisements à utiliser comme niveaux de référence
levels_order <- c("PIS10 x PIS19", "PIS36 x PIS19", "PIS10 x PIS14", 
                  "PIS14 x PIS40", "PIS14 x SC2", "PIS40 x PIS14")

# Appeler la fonction
results <- analyze_crossings_3(data = Tube_polli_stat, 
                               response = c("Prologation2", "Prolongation1"), 
                               predictor = "Croisement", 
                               levels_order = levels_order)

# Afficher les résultats pour chaque niveau de référence
results[["PIS10 x PIS19"]]  # Par exemple, voir les résultats pour ce niveau de référence

# Appliquer la fonction pour extraire les Z-values et p-values
results_summary <- extract_results(results, levels_order)
results_summary

#Appliquer la fonction
gt_table_3 <- create_gt_table_pvalues(results_summary)
gt_table_3 <- gt_table_3 %>%
  tab_header(
    title = "P-values issus des GLM binomiales, en changeant le niveau de référence",  # Titre principal
    subtitle = "Formule : cbind(Prolongation2, Prolongation1 - Prolongation2) ~ Croisement"  # Sous-titre optionnel
  )


print(gt_table_3)



#Nombre de TP traversant le 3e tier / Nombre de TP traversant le 2e tier
#Hypothèse d'application

#(1) Données indépendantes -> Oui
#(2) La relation entre les prédicteurs et la probabilité de l'événement suit une transformation logit.
model_4 <- glm(cbind(Prolongation3, Prologation2 - Prolongation3) ~ Croisement, family = binomial(link = "logit"), data = Tube_polli_stat)

#plot(model) #OK

#Test GLM
summary(model_4)

#Analyse en changent à chaque fois le niveaux de référence 
# Fonction pour créer un tableau des résultats pairwise
analyze_crossings_4 <- function(data, response, predictor, levels_order) {
  results <- list()  # Initialiser une liste pour stocker les résultats
  
  # Boucler sur chaque niveau à définir comme référence
  for (ref_level in levels_order) {
    # Changer le niveau de référence
    data[[predictor]] <- factor(data[[predictor]], levels = c(ref_level, setdiff(levels_order, ref_level)))
    
    # Créer et ajuster le modèle GLM
    model <- glm(cbind(Prolongation3, Prologation2 - Prolongation3) ~ Croisement, 
                 family = binomial(link = "logit"), 
                 data = data)
    
    # Sauvegarder le résumé du modèle
    results[[ref_level]] <- summary(model)
  }
  
  return(results)  # Retourner tous les résultats
}


# Ordre des croisements à utiliser comme niveaux de référence
levels_order <- c("PIS10 x PIS19", "PIS36 x PIS19", "PIS10 x PIS14", 
                  "PIS14 x PIS40", "PIS14 x SC2", "PIS40 x PIS14")

# Appeler la fonction
results <- analyze_crossings_4(data = Tube_polli_stat, 
                               response = c("Prolongation3", "Prologation2"), 
                               predictor = "Croisement", 
                               levels_order = levels_order)

# Afficher les résultats pour chaque niveau de référence
results[["PIS10 x PIS19"]]  # Par exemple, voir les résultats pour ce niveau de référence

# Appliquer la fonction pour extraire les Z-values et p-values
results_summary <- extract_results(results, levels_order)
results_summary

#Appliquer la fonction
gt_table_4 <- create_gt_table_pvalues(results_summary)
gt_table_4 <- gt_table_4 %>%
  tab_header(
    title = "P-values issus des GLM binomiales, en changeant le niveau de référence",  # Titre principal
    subtitle = "Formule : cbind(Prolongation3, Prolongation2 - Prolongation3) ~ Croisement"  # Sous-titre optionnel
  )


print(gt_table_4)



gtsave(gt_table_1, filename = "table_pva_1.png")
gtsave(gt_table_2, filename = "table_pva_2.png")
gtsave(gt_table_3, filename = "table_pva_3.png")
gtsave(gt_table_4, filename = "table_pva_4.png")

#Graphiques -----
Tube_polli_nb_mean <- Tube_polli %>%  
  mutate(Croisement = paste(Fleurs_femelle, "x", Pollen_male)) %>%
  group_by(Croisement) %>% 
  summarize(
    prop0 = mean(Pollen), # Nombre moyen de pollen germé
    prop1 = mean(Germe), # Nombre moyen de TP traversant le 1er tier
    prop2 = mean(Prolongation1), # Nombre moyen de TP traversant le 2e tier
    prop3 = mean(Prologation2) # Nombre moyen de TP traversant le 3e tier
  ) %>%
  pivot_longer(
    cols = prop0:prop3,
    names_to = "Tier",
    values_to = "Denominateur"
  )

# Transformation de l'écart-type
Tube_polli_nb_sd <- Tube_polli %>%  
  mutate(Croisement = paste(Fleurs_femelle, "x", Pollen_male)) %>%
  group_by(Croisement) %>% 
  summarize(
    prop0 = sd(Pollen),
    prop1 = sd(Germe),
    prop2 = sd(Prolongation1),
    prop3 = sd(Prologation2)
  ) %>%
  pivot_longer(
    cols = prop0:prop3,
    names_to = "Tier",
    values_to = "SD_Denom"
  )

# Fusion des deux dataframes sur "Croisement" et "Tier"
Tube_polli_nb <- left_join(Tube_polli_nb_mean, Tube_polli_nb_sd, by = c("Croisement", "Tier")) %>% 
  mutate(SD_S_Denom = Denominateur + SD_Denom,
         SD_I_Denom = if_else(Denominateur - SD_Denom < 0, 0, Denominateur - SD_Denom))


Tube_polli_resume$Croisement <-  as.factor(Tube_polli_resume$Croisement)
levels(Tube_polli_resume$Croisement)


#Ordre de taux de mise à fruit
Tube_polli_resume$Croisement <- factor(Tube_polli_resume$Croisement, levels = c("PIS10 x PIS19", 
                                                                                "PIS36 x PIS19",
                                                                                "PIS10 x PIS14",
                                                                                "PIS14 x PIS40",
                                                                                "PIS14 x SC2",
                                                                                "PIS40 x PIS14"))
levels(Tube_polli_resume$Croisement)

Tube_polli_resume <- Tube_polli_resume %>% 
  mutate(Groupe.Prop = case_when(
    Croisement == "PIS10 x PIS14" & Tier == "prop0" ~ "a",
    Croisement == "PIS10 x PIS19" & Tier == "prop0" ~ "a",
    Croisement == "PIS36 x PIS19" & Tier == "prop0" ~ "b",
    Croisement == "PIS14 x PIS40" & Tier == "prop0" ~ "a",
    Croisement == "PIS14 x SC2" & Tier == "prop0" ~ "b",
    Croisement == "PIS40 x PIS14" & Tier == "prop0" ~ "c",
    Croisement == "PIS10 x PIS14" & Tier == "prop1" ~ "a",
    Croisement == "PIS10 x PIS19" & Tier == "prop1" ~ "c",
    Croisement == "PIS36 x PIS19" & Tier == "prop1" ~ "c",
    Croisement == "PIS14 x PIS40" & Tier == "prop1" ~ "c",
    Croisement == "PIS14 x SC2" & Tier == "prop1" ~ "b",
    Croisement == "PIS40 x PIS14" & Tier == "prop1" ~ "ab",
    Croisement == "PIS10 x PIS14" & Tier == "prop2" ~ "a",
    Croisement == "PIS10 x PIS19" & Tier == "prop2" ~ "ab",
    Croisement == "PIS36 x PIS19" & Tier == "prop2" ~ "a",
    Croisement == "PIS14 x PIS40" & Tier == "prop2" ~ "ab",
    Croisement == "PIS14 x SC2" & Tier == "prop2" ~ "ab",
    Croisement == "PIS40 x PIS14" & Tier == "prop2" ~ "b",
    Croisement == "PIS10 x PIS14" & Tier == "prop3" ~ "a",
    Croisement == "PIS10 x PIS19" & Tier == "prop3" ~ "a",
    Croisement == "PIS36 x PIS19" & Tier == "prop3" ~ "a",
    Croisement == "PIS14 x PIS40" & Tier == "prop3" ~ "a",
    Croisement == "PIS14 x SC2" & Tier == "prop3" ~ "a",
    Croisement == "PIS40 x PIS14" & Tier == "prop3" ~ "a",
    ))


plot_names_2 <- c('prop0' = "Pollen germé / Pollen total",
                  'prop1' = "Tub. pollin. au 1er tier / Pollen germé",
                  'prop2' = "Tub. pollin. au 2e tier / Tub. pollin. 1er tier",
                  'prop3' = "Tub. pollin. au 3e tier / Tub. pollin. 2e tier")

plot_names_3 <- c('prop0' = "Pollen total",
                  'prop1' = "Pollen germé",
                  'prop2' = "Tub. pollin. 1er tier",
                  'prop3' = "Tub. pollin. 2e tier")

Tube_polli_graph <- left_join(Tube_polli_resume,Tube_polli_nb, by = c("Croisement","Tier"))

Tube_polli_graph$Croisement <- factor(Tube_polli_graph$Croisement, levels = c("PIS10 x PIS19", 
                                                                              "PIS36 x PIS19",
                                                                              "PIS10 x PIS14",
                                                                              "PIS14 x PIS40",
                                                                              "PIS14 x SC2",
                                                                              "PIS40 x PIS14"))




#Figure 30-----
#Figure 30 - Germination du pollen et croissance des tubes polliniques à travers le style pour six types de croisements entre individus de S. chilense. 
#Graphique de base
plot_mean <- 
  ggplot(Tube_polli_graph, aes(x = Croisement, y = mean*100)) +
  geom_bar_pattern(
    stat = "identity",
    aes(fill = Croisement, pattern_fill = Croisement),  # Couleurs de fond et de motif basées sur Croisement
    pattern = "stripe",  # Choisissez le motif souhaité (par exemple "stripe" pour des rayures)
    pattern_angle = 45,  # Angle des rayures
    pattern_density = 0.3,  # Espacement des rayures
    pattern_spacing = 0.2,  # Épaisseur des rayures
    color = "black",
    show.legend =  FALSE)+
  
  geom_errorbar(aes(ymin = SD_I*100, ymax = SD_S*100), color = "black", width = .3, linewidth = 0.75) +
  labs(y = "Pourcentage") +
  
  geom_text(mapping = aes(x = Croisement, y = SD_S*100 + 10, label = Groupe.Prop),family = "Aptos")+
  scale_y_continuous(breaks = c(0,25,50,75,100))+
  
  scale_fill_manual(values = c("#003049", "#FA9F25", "#003049","#362E41", "#362E41", "#F3D180")) +  # Définissez vos couleurs ici
  scale_pattern_fill_manual(values = c("#6B2C39", "#6B2C39", "#362E41","#F3D180", "#EAE2B7", "#362E41")) +  # Couleurs des motifs
  scale_pattern_manual(values = c("stripe", "stripe", "stripe", "stripe", "stripe", "stripe")) +  # Définir le motif pour chaque croisement
  
  facet_wrap(vars(Tier), nrow = 1, labeller = as_labeller(plot_names_2)) +
  theme(text = element_text(size = 15, family = "Aptos"),
        strip.text = element_text(size = 12, family = "Aptos"),
        axis.text.x = element_blank(),
        axis.text.y = element_text(family = "Aptos"),
        axis.title.x = element_blank(),
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0))



#Graphique inférieur (Denominateur) avec axe y inversé
plot_denominateur <- 
  ggplot(Tube_polli_graph, aes(x = Croisement, y = -Denominateur)) +
  geom_bar(
    stat = "identity",
    alpha = 0.5,
    color = "black",
    fill = "darkgreen"
  ) +
  
  geom_errorbar(
    aes(y = -Denominateur, ymin = -SD_I_Denom, ymax = -SD_S_Denom),
    color = "black", width = 0.3, linewidth = 0.75,
  ) +
  
  scale_y_continuous(labels = abs)+
  labs(x = "Croisement",
       y = "N") +
  
  facet_wrap(vars(Tier), nrow = 1,strip.position = "bottom", labeller = as_labeller(plot_names_3)) +
  theme(text = element_text(size = 15, family = "Aptos"),
        #strip.text =  element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 11, family = "Aptos"),
        axis.text.y = element_text(family = "Aptos"),
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0))

# Combiner les deux graphiques verticalement
plot_mean / plot_denominateur + plot_layout(heights = c(1,1), guides = "collect")



#Pourcentage réelle de tube polliniques attéigant l'ovaire----
Tab_TP <- Tube_polli %>% 
  mutate(Prop_Att_Ovaire = Prolongation3/Germe) %>% 
  na.omit() %>%
  group_by(Fleurs_femelle, Pollen_male) %>% 
  summarise(Mean_prop = mean(Prop_Att_Ovaire)*100,
           sd = sd(Prop_Att_Ovaire)) %>% 
  mutate(Prop_SD = paste0(round(Mean_prop,2)," (±",round(sd,2),")"),
         Croisement = paste(Fleurs_femelle,"x",Pollen_male)) %>% 
  rename("Parent_Femelle" = Fleurs_femelle,
         "Parent_Male" = Pollen_male)




##----Partie 2.4 - Test de Correlation----
#Test de Correlation entre le taux de mise à fruit et le nombre moyen de graine par fruit et ----
head(data_table_croisement)

data_croisement_correlation_18_4 <- data_table_croisement %>% 
  filter(N_Fruit_Mesur >= 4,
         Nb_Croisement >= 18) %>% 
  dplyr::select(Taux_mise_a_fruit, N_Moy_Graine)

data_croisement_correlation_18_1 <- data_table_croisement %>% 
  filter(N_Fruit_Mesur >= 1,
         Nb_Croisement >= 18) %>% 
  dplyr::select(Taux_mise_a_fruit, N_Moy_Graine)



# Calcul des corrélations
cor_18_4 <- cor(data_croisement_correlation_18_4)["Taux_mise_a_fruit", "N_Moy_Graine"]
cor_18_1 <- cor(data_croisement_correlation_18_1)["Taux_mise_a_fruit", "N_Moy_Graine"]

# Calcul des tests de corrélation
cor_test_18_4 <- cor.mtest(data_croisement_correlation_18_4, conf.level = .95)
cor_test_18_1 <- cor.mtest(data_croisement_correlation_18_1, conf.level = .95)

# Extraction des p-valeurs
p_val_18_4 <- cor_test_18_4$p["Taux_mise_a_fruit", "N_Moy_Graine"]
p_val_18_1 <- cor_test_18_1$p["Taux_mise_a_fruit", "N_Moy_Graine"]

# Extraction des intervalles de confiance
lowCI_18_4 <- cor_test_18_4$lowCI["Taux_mise_a_fruit", "N_Moy_Graine"]
uppCI_18_4 <- cor_test_18_4$uppCI["Taux_mise_a_fruit", "N_Moy_Graine"]

lowCI_18_1 <- cor_test_18_1$lowCI["Taux_mise_a_fruit", "N_Moy_Graine"]
uppCI_18_1 <- cor_test_18_1$uppCI["Taux_mise_a_fruit", "N_Moy_Graine"]

# Création du tableau des résultats
data_table_correlation <- data.frame(
  N_Replica_Croisement = c(18, 18),
  N_Fruit_Disseq = c(4, 1),
  N_Croisement_diff = c(nrow(data_croisement_correlation_18_4), nrow(data_croisement_correlation_18_1)),
  Correlation = round(c(cor_18_4, cor_18_1), 3),
  IC_inf = round(c(lowCI_18_4, lowCI_18_1), 3),
  IC_sup = round(c(uppCI_18_4, uppCI_18_1), 3),
  p_val = round(c(p_val_18_4, p_val_18_1), 4))

table_correlation <- data_table_correlation %>% 
  gt() %>%
  tab_header(
    title = "Test de corrélation entre le taux de mise a fruit et nombre moyen de graine par fruit",
    subtitle = ""
  ) %>%
  cols_label(
    N_Replica_Croisement = html("Nombre de replica<br> par croisement"),
    N_Fruit_Disseq = html("Nombre de<br>fruit disséqué"),
    N_Croisement_diff = html("Nombre de<br>croisements<br>différent"),
    Correlation = html("Coefficient de<br>corrélation (R)"),
    IC_inf = html("IC inf"),
    IC_sup = html("IC sup"),
    p_val = html("P-valeur")
  ) %>%
  
  #Ajouter un groupement de colonnes (deuxième ligne d'en-tête)
  tab_spanner(
    label = "Critère de sélection",
    columns = vars(N_Replica_Croisement, N_Fruit_Disseq)
  ) %>%
  tab_spanner(
    label = "Test de corrélation",
    columns = vars(Correlation, IC_inf, IC_sup,p_val),
  ) %>% 
  
  tab_options(
    table.font.size = 11,
    data_row.padding =px(3),
  )  

#Tableau 12 : Test de corrélation entre le taux de mise à fruit et le nombre moyen de graines par fruit
table_correlation

gtsave(table_correlation, filename = "table_correlation.png")


#Test de correlélation entre---- 
# (1) Le taux de mise à fruit
Fruit_set_cor <- Croisement_resmu %>% 
  mutate(Croisement = paste(Parent_Femelle,"x",Parent_Male)) %>%
  filter(Croisement %in%  c("PIS10 x PIS19", 
                            "PIS36 x PIS19",
                            "PIS10 x PIS14",
                            "PIS14 x PIS40",
                            "PIS14 x SC2",
                            "PIS40 x PIS14")) %>% 
  ungroup()%>% 
  dplyr::select(Croisement, Fruit_set)
# (2) Nombre moyen de graine par fruit
Seed_cor <- Graine %>% 
  mutate(Croisement = paste(Parent_Femelle,"x",Parent_Male)) %>%
  filter(Croisement %in%  c("PIS10 x PIS19", 
                            "PIS36 x PIS19",
                            "PIS10 x PIS14",
                            "PIS14 x PIS40",
                            "PIS14 x SC2",
                            "PIS40 x PIS14")) %>% 
  ungroup()%>% 
  dplyr::select(Croisement, N_Moy_Graine)
# (3) La coninuité dans la TP Tier 1
# (4) La coninuité dans la TP Tier 2
# (5) La coninuité dans la TP Tier 3
Tube_polli_cor <- Tube_polli_resume %>%
  filter(Croisement %in%  c("PIS10 x PIS19", 
                            "PIS36 x PIS19",
                            "PIS10 x PIS14",
                            "PIS14 x PIS40",
                            "PIS14 x SC2",
                            "PIS40 x PIS14")) %>%
  dplyr::select(Croisement, Tier, mean) %>% 
  pivot_wider(names_from = Tier,
              values_from = mean)

#Rassemblemant des donnée,
Data_cor_large <- left_join(Fruit_set_cor, Seed_cor, by = "Croisement")
Data_cor_large <- left_join(Data_cor_large, Tube_polli_cor, by = "Croisement") %>% 
  dplyr::select(-Croisement)

Data_cor_large <- Data_cor_large %>% 
  rename(Fruit = Fruit_set,
         Graine = N_Moy_Graine,
         Germ. = prop0,
         TP.1 = prop1,
         TP.2 = prop2,
         TP.3 = prop3) %>% 
  dplyr::select(-TP.3) #Car pas d'effet précédement



cor(Data_cor_large)

cor_test <- cor.mtest(Data_cor_large, conf.level = .95) # Test de corrélation


desired_order <- c("Fruit", "Graine", "Germ.", "TP.1", "TP.2")
color_palette <- c("darkgreen", "darkgreen", "red", "red", "red")
# Réordonnez la matrice de corrélation et la matrice de p-valeurs selon cet ordre
cor_matrix <- cor(Data_cor_large)[desired_order, desired_order]
p_matrix <- cor_test$p[desired_order, desired_order]


#Figure 31 : Graphique de corrélation 
corrplot.mixed(cor_matrix, order = "original", p.mat = p_matrix,
               tl.col = color_palette)




##----Partie 3 - S-allele----
S_allele <- read.csv("R_S_allele.csv", sep = ";", dec = ",")
S_allele_theory <- read.csv("R_S_allele_theory.csv", sep = ";", dec = ",") 
Ind_S <- read.csv("R_Ind_S.csv", sep = ";", dec = ",")

names(S_allele)

S_allele <- left_join(S_allele,S_allele_theory, c("S_allele_F_S10", 
                                                  "S_allele_F_S35",
                                                  "S_allele_F_S36", 
                                                  "S_allele_M_S10",
                                                  "S_allele_M_S35",
                                                  "S_allele_M_S36"))
S_allele <- S_allele %>% 
  left_join(Ind_S, by = c("Parent_Femelle" = "Individu")) %>% 
  rename("S_allele_PF" = S_allele) %>% 
  left_join(Ind_S, by = c("Parent_Male" = "Individu")) %>% 
  rename("S_allele_PM" = S_allele)

S_allele <- S_allele %>% 
  mutate("P_F_S_allele" = paste0(Parent_Femelle," ","(",S_allele_PF,")"),
         "P_M_S_allele" = paste0(Parent_Male," ","(",S_allele_PM,")"))

#Compatibilité TP
S_allele <-  S_allele %>% 
  mutate(N_S_allele_commun = if_else(Parent_Femelle == Parent_Male, "2", N_S_allele_commun),
         Compatibilite_TP = if_else(Parent_Femelle == Parent_Male, "0", Compatibilite_TP))

S_allele$Compatibilite_TP <- factor(S_allele$Compatibilite_TP, levels = c("0","0 ou 50","50","50 ou 100","100",NA))
S_allele$N_S_allele_commun <- factor(S_allele$N_S_allele_commun, levels = c("0","max 1","1","min 1", "2", NA))
levels(S_allele$Compatibilite_TP)

#Compatibilité_FruitSet
S_allele <-  S_allele %>% 
  mutate(N_S_allele_commun = if_else(Parent_Femelle == Parent_Male, "2", N_S_allele_commun),
         Compatibilite_FruitSet = if_else(Parent_Femelle == Parent_Male, "0", Compatibilite_FruitSet))

S_allele$Compatibilite_FruitSet <- factor(S_allele$Compatibilite_FruitSet, levels = c("0",">50","100",NA))
levels(S_allele$Compatibilite_FruitSet)




#TP
ggplot(S_allele, aes(y = P_F_S_allele, x = P_M_S_allele, fill = Compatibilite_TP)) +    
  scale_x_discrete(position = "top")+
  scale_y_discrete(limits = rev(levels(factor(S_allele$P_M_S_allele))))+
  geom_tile(color = "white",lwd = 1.5,linetype = 1)+
  geom_text(aes(label = N_S_allele_commun), size = 4, family = "Aptos")+
  coord_fixed() +
  
  scale_fill_manual(values = c(
    "0"= "#691515",
    "0 ou 50" = "#B48A8A", 
    "50" = "#D8D0D0", 
    "50 ou 100" = "#80B280", 
    "100" = "darkgreen")) +
  
  labs(title = "Compatibilité théorique au niveau des",
       y = "Parent femelle",
       x = "Parent mâle",
       fill = 
"Pourcentage théorique 
des tubes polliniques 
attégnant l'ovaire (%)")+
  
  theme(text = element_text(size = 12, family = "Aptos"),
        strip.text = element_text(size = 12, family = "Aptos"),
        axis.text.x = element_text(angle = 45, hjust = -0.001,size = 12, family = "Aptos"),
        axis.text.y = element_text(size = 12, family = "Aptos"),
        axis.title = element_text(size = 14, family = "Aptos"))

#Fruit
ggplot(S_allele, aes(y = P_F_S_allele, x = P_M_S_allele, fill = Compatibilite_FruitSet)) +    
  scale_x_discrete(position = "top")+
  scale_y_discrete(limits = rev(levels(factor(S_allele$P_M_S_allele))))+
  geom_tile(color = "white",lwd = 1.5,linetype = 1)+
  geom_text(aes(label = N_S_allele_commun), size = 4, family = "Aptos")+
  coord_fixed() +
  
  scale_fill_manual(values = c(
    "0"= "#691515",
    ">50" = "#80B280", 
    "100" = "darkgreen")) +
  
  labs(title = "Compatibilité théorique au niveau des",
       y = "Parent femelle",
       x = "Parent mâle",
       fill = 
         "Taux de mise à 
fruit théorique (%)")+
  
  theme(text = element_text(size = 12, family = "Aptos"),
        strip.text = element_text(size = 12, family = "Aptos"),
        axis.text.x = element_text(angle = 45, hjust = -0.001,size = 12, family = "Aptos"),
        axis.text.y = element_text(size = 12, family = "Aptos"),
        axis.title = element_text(size = 14, family = "Aptos"))


#Différence théorique et réelle dans le taux de mise à fruit
S_allele_fruit_th <- S_allele %>% 
  dplyr::select(Parent_Femelle,Parent_Male, Compatibilite_FruitSet,P_F_S_allele,P_M_S_allele) %>% 
  mutate(FruitSetTh = case_when(
    Compatibilite_FruitSet == "0" ~ 0,
    Compatibilite_FruitSet == ">50" ~ 100,
    Compatibilite_FruitSet == "100" ~ 100
  ))

Croisement_th_reel <- Croisement_resmu %>% 
  left_join(S_allele_fruit_th, join_by(Parent_Femelle,Parent_Male)) %>% 
  mutate(Diff_FruitSet =  FruitSetTh - Fruit_set)


#Tableau 15 : Synthèse des croisements réalisés entre dix individus de S. chilense et différence entre le taux de mise à fruit théorique (0% ou 100%) établi sur base de leur S-allèles et le taux de mise à fruit réel.  

ggplot(Croisement_th_reel, aes(y = P_F_S_allele, x = P_M_S_allele, fill = Diff_FruitSet)) +    
  scale_x_discrete(position = "top")+
  scale_y_discrete(limits = rev(levels(factor(Croisement_th_reel$P_M_S_allele))))+
  geom_tile(color = "white",lwd = 1.5,linetype = 1)+
  geom_text(aes(label = Sum_Flower), size = 5, family = "Aptos")+
  coord_fixed() +
  guides(fill = guide_colourbar(barwidth = 0.5, 
                                barheight = 10, 
                                ticks = FALSE, 
                                nbin = 100))+
  scale_fill_gradientn(
    colors = c("#76560F", "white", "#4C2849", "#760F0F"),
    values = scales::rescale(c(-23, 0 ,80, 100)), 
    breaks = c(-20, 0, 20, 40, 60, 80, 100),  # Fixer les breaks pour une meilleure graduation
    labels = c("-20%","0%", "20%", "40%", "60%", "80%","100%"),
    na.value = "black", 
    guide = "colorbar") +
  
  labs(title = "Taux de mise à fruit théorique - taux de mise à fruit réel",
       y = "Parent femelle",
       x = "Parent mâle",
       fill = "Taux de mise à fruit théorique - taux 
       de mise à fruit réel (%)")+
  
  theme(text = element_text(size = 12, family = "Aptos"),
        strip.text = element_text(size = 12, family = "Aptos"),
        axis.text.x = element_text(angle = 45, hjust = -0.001,size = 12, family = "Aptos"),
        axis.text.y = element_text(size = 12, family = "Aptos"),
        axis.title = element_text(size = 14, family = "Aptos"))


##----Partie 4 - qPCR / Analyse d'expression de gènes----
#Importation des données
qPCR <- read.csv("R_qPCR.csv", sep = ";", dec = ",") 

qPCR$Ind <- as.factor(qPCR$Ind)

qPCR_pivot <- qPCR %>% 
  rename(HT_A = HT.A.HK.PIS10,
         HT_B = HT.B.HK.PIS10,
         S10 = S10.HK.PIS10,
         S16 = S16.HK.PIS10) %>% 
  pivot_longer(cols = HT_A:S16,
               names_to = "gene",
               values_to = "expression_relative")

#Analyse statistique - qPCR ----
#HT-A----
qPCR_HTA <- qPCR %>% 
  filter(!Ind %in% c('PIS31','PIS34')) %>% 
  dplyr::select(Ind,HT.A.HK.PIS10)

mod_qPCR_HTA <- lm(HT.A.HK.PIS10 ~ Ind , data = qPCR_HTA)
#Application d'une transformation de log sur les données

#Hypothèse d'applications

#(1) Données indépendantes -> Oui
#(2) chantillonage aléatoire ~ Erreurs Indépendants -> Oui
#(3) Homoscédasitcité (Variance égale) 
plot(mod_qPCR_HTA,1)

leveneTest(HT.A.HK.PIS10 ~ Ind , data = qPCR_HTA) #Ok


#(4) Normalité des résidus 
plot(mod_qPCR_HTA,2) #Non

hist(residuals(mod_qPCR_HTA)) #Gaussien

shapiro.test(residuals(mod_qPCR_HTA)) #Pas Ok
#Mais l'anova reste robutse aux donnée non-normale

#ANOVA 1
mod_anova_qPCR_HTA <- aov(HT.A.HK.PIS10 ~ Ind , data = qPCR_HTA)
summary(mod_anova_qPCR_HTA)

#Comparaisons multiples

mod_qPCR_HTA <- lm(HT.A.HK.PIS10 ~ Ind , data = qPCR_HTA)

emm <- emmeans(mod_qPCR_HTA, "Ind")
cld(emm, adjust="tukey")
tukey_qPCR_HTA <- cld(emm, adjust="tukey")

group_HTA <- tibble(Ind= tukey_qPCR_HTA$Ind, Groupe =  tukey_qPCR_HTA$.group ) %>%
  mutate(Groupe = str_replace_all(Groupe, c('1' = 'a', '2' = 'b', '3' = 'c', '4' = 'd', '5' = 'e', '6' = 'f')),
         gene = "HT_A")


#HT-B----
qPCR_HTB <- qPCR %>% 
  filter(!Ind %in% c('PIS31','PIS34')) %>% 
  dplyr::select(Ind,HT.B.HK.PIS10)

mod_qPCR_HTB <- lm(HT.B.HK.PIS10 ~ Ind , data = qPCR_HTB)

#Hypothèse d'applications

#(1) Données indépendantes -> Oui
#(2) chantillonage aléatoire ~ Erreurs Indépendants -> Oui
#(3) Homoscédasitcité (Variance égale) 
plot(mod_qPCR_HTB,1) #OK

leveneTest(log(HT.B.HK.PIS10)  ~ Ind , data = qPCR_HTB) #Ok


#(4) Normalité des résidus #(4) Normalité dqPCR_HTBes résidus 
plot(mod_qPCR_HTB,2) #Ok

hist(residuals(mod_qPCR_HTB)) #Gaussien

shapiro.test(residuals(mod_qPCR_HTB)) #Pas ok mais on garde

#Test - ANOVA1
mod_anova_qPCR_HTB <- aov(HT.B.HK.PIS10 ~ Ind , data = qPCR_HTB)
summary(mod_anova_qPCR_HTB)

TukeyHSD(mod_anova_qPCR_HTB, conf.level = 0.95)

#Comparaisons multiples

emm <- emmeans(mod_qPCR_HTB, "Ind")
cld(emm, adjust="tukey")
tukey_qPCR_HTB <- cld(emm, adjust="tukey")

group_HTB <- tibble(Ind= tukey_qPCR_HTB$Ind, Groupe =  tukey_qPCR_HTB$.group ) %>%
  mutate(Groupe = str_replace_all(Groupe, c('1' = 'a', '2' = 'b', '3' = 'c', '4' = 'd', '5' = 'e', '6' = 'f')),
         gene = "HT_B")

#S10----
qPCR_S10 <- qPCR %>% 
  filter(Ind %in% c('PIS10','PIS33','PIS36','PIS38','PIS40','SC2')) %>% 
  dplyr::select(Ind,S10.HK.PIS10)

mod_qPCR_S10 <- lm(S10.HK.PIS10 ~ Ind , data = qPCR_S10)

#Hypothèse d'applications

#(1) Données indépendantes -> Oui
#(2) chantillonage aléatoire ~ Erreurs Indépendants -> Oui
#(3) Homoscédasitcité (Variance égale) 
plot(mod_qPCR_S10,1)

leveneTest(log(S10.HK.PIS10)  ~ Ind , data = qPCR_S10) #Ok


#(4) Normalité des résidus #(4) Normalité dqPCR_HTBes résidus 
plot(mod_qPCR_S10,2) #mouais

hist(residuals(mod_qPCR_S10)) #Gaussien

shapiro.test(residuals(mod_qPCR_S10)) #Ok


#Test - ANOVA1
mod_anova_qPCR_S10 <- aov(S10.HK.PIS10  ~ Ind , data = qPCR_S10)
summary(mod_anova_qPCR_S10)

TukeyHSD(mod_anova_qPCR_S10, conf.level = 0.95)

#Comparaisons multiples
summary(mod_qPCR_S10)

emm <- emmeans(mod_qPCR_S10, "Ind")
cld(emm, adjust="tukey")
tukey_qPCR_S10 <- cld(emm, adjust="tukey")

group_S10 <- tibble(Ind= tukey_qPCR_S10$Ind, Groupe =  tukey_qPCR_S10$.group ) %>%
  mutate(Groupe = str_replace_all(Groupe, c('1' = 'a', '2' = 'b', '3' = 'c', '4' = 'd', '5' = 'e', '6' = 'f')),
         gene = "S10")


#S16 (S35)----
qPCR_S16 <- qPCR %>% 
  filter(Ind %in% c('PIS10','PIS14','PIS33','PIS19','PIS40','SC2')) %>% 
  dplyr::select(Ind,S16.HK.PIS10)

mod_qPCR_S16 <- lm(S16.HK.PIS10 ~ Ind , data = qPCR_S16)

#Hypothèse d'applications

#(1) Données indépendantes -> Oui
#(2) chantillonage aléatoire ~ Erreurs Indépendants -> Oui
#(3) Homoscédasitcité (Variance égale) 
plot(mod_qPCR_S16,1)

leveneTest(S16.HK.PIS10 ~ Ind , data = qPCR_S16) #Ok


#(4) Normalité des résidus #(4) Normalité dqPCR_HTBes résidus 
plot(mod_qPCR_S16,2) #mouais

hist(residuals(mod_qPCR_S16)) # mouais Gaussien

shapiro.test(residuals(mod_qPCR_S16)) #Pas ok mais on garde


#Test - ANOVA1
mod_anova_qPCR_S16 <- aov(S16.HK.PIS10 ~ Ind , data = qPCR_S16)
summary(mod_anova_qPCR_S16)

TukeyHSD(mod_anova_qPCR_S16, conf.level = 0.95)

#Comparaisons multiples

emm <- emmeans(mod_qPCR_S16, "Ind")
cld(emm, adjust="tukey")
tukey_qPCR_S16 <- cld(emm, adjust="tukey")

group_S16 <- tibble(Ind= tukey_qPCR_S16$Ind, Groupe =  tukey_qPCR_S16$.group ) %>%
  mutate(Groupe = str_replace_all(Groupe, c('1' = 'a', '2' = 'b', '3' = 'c', '4' = 'd', '5' = 'e', '6' = 'f')),
         gene = "S16")

#Graphiques - qPCR ----
qPCR_resmue <- qPCR_pivot %>% 
  group_by(Ind,gene) %>% 
  summarise(mean = mean(na.omit(expression_relative)),
            var = var(na.omit(expression_relative)),
            sd = sd(na.omit(expression_relative)),
            N = n()) %>% 
  mutate(SD_S = mean + sd,
         SD_I = mean - sd,
         CI_S = mean + qt(0.975, N-1)*sqrt(var/N),
         CI_I = mean - qt(0.975, N-1)*sqrt(var/N))

qPCR_resmue_filtre <- qPCR_pivot %>%
  filter(!Ind %in% c('PIS31', 'PIS34')) %>% 
  group_by(Ind,gene) %>% 
  summarise(mean = mean(na.omit(expression_relative)),
            var = var(na.omit(expression_relative)),
            sd = sd(na.omit(expression_relative)),
            N = n()) %>% 
  mutate(SD_S = mean + sd,
         SD_I = mean - sd,
         CI_S = mean + qt(0.975, N-1)*sqrt(var/N),
         CI_I = mean - qt(0.975, N-1)*sqrt(var/N))

#Modifié en fonction des comparaison multiples
group_qPCR_tout <- bind_rows(group_HTA,group_HTB,group_S10,group_S16) %>%
  mutate(Groupe = str_trim(Groupe, side = "both"))


qPCR_resmue_filtre <- qPCR_resmue_filtre %>%
  left_join(group_qPCR_tout, by = c("Ind", "gene"))

#Graphiques
gene_names <- c('HT_A' = "HT-A",
                'HT_B' = "HT-B",
                'S10' = "S10",
                'S16' = "S35")

qPCR_resmue_filtre$gene <- factor(qPCR_resmue_filtre$gene, levels = c("HT_A","S10","HT_B","S16")) 
                                                                                
                                

#Figure 32 : Expression de HT-A et HT-B normalisée aux pistils de PIS10 avec LeEF1?? et TIP41 comme gènes de référence dans les pistils de 8 individus de S. chilense
ggplot(qPCR_resmue_filtre[qPCR_resmue_filtre$gene  %in% c("HT_A", "HT_B"), ], aes(x = Ind, y = mean))+
  geom_bar(stat = "identity", aes(fill = Ind))+
  geom_errorbar(aes(ymin = SD_I, ymax = SD_S), color = "black", width=.3, size = 0.75)+
  labs(title = "Expression relative",
       x = "Indiviudu",
       y = "Expression relative",
       fill = "Individu")+
  geom_text(mapping = aes(x = Ind, y = 0.4 + SD_S*1.4, label = Groupe,))+
  scale_fill_manual(values = colors)+
  facet_wrap(vars(gene), scales = ,labeller = as_labeller(gene_names))+
  scale_y_continuous(trans = "pseudo_log",
                     breaks = c(0,2,4,10, 50, 100,200))+
  
  theme(text = element_text(size = 15, family = "Aptos"),
        strip.text = element_text(size = 12, family = "Aptos"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 11, family = "Aptos"),
        axis.text.y = element_text(family = "Aptos"))

#Figure 33 : Expression des allèles S10 et S35 normalisée aux pistils de PIS10 avec LeEF1?? et TIP41 comme gènes de référence dans les pistils de 8 individus de S. chilense
ggplot(qPCR_resmue_filtre[qPCR_resmue_filtre$gene  %in% c("S10", "S16"), ], aes(x = Ind, y = mean))+
  geom_bar(stat = "identity", aes(fill = Ind))+
  geom_errorbar(aes(ymin = SD_I, ymax = SD_S), color = "black", width=.3, size = 0.75)+
  labs(title = "Expression relative",
       x = "Indiviudu",
       y = "Expression relative",
       fill = "Individu")+
  geom_text(mapping = aes(x = Ind, y = 0.4 + SD_S*1.4, label = Groupe,))+
  scale_fill_manual(values = colors)+
  facet_wrap(vars(gene), scales = ,labeller = as_labeller(gene_names),)+
  scale_y_continuous(trans = "pseudo_log",
                     breaks = c(0,2,4,10, 25,50, 100,200))+

  geom_text(data = subset(qPCR_resmue_filtre, gene == "S10"),
            aes(x = "PIS14", y = 0.2, label = "NS", family = "Aptos")) +
  geom_text(data = subset(qPCR_resmue_filtre, gene == "S10"),
            aes(x = "PIS19", y = 0.2, label = "NS", family = "Aptos")) +
  geom_text(data = subset(qPCR_resmue_filtre, gene == "S16"),
            aes(x = "PIS38", y = 0.2, label = "NA", family = "Aptos")) +
  geom_text(data = subset(qPCR_resmue_filtre, gene == "S16"),
            aes(x = "PIS36", y = 0.2, label = "NS", family = "Aptos")) +
  
  theme(text = element_text(size = 15, family = "Aptos"),
        strip.text = element_text(size = 12, family = "Aptos"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 11, family = "Aptos"),
        axis.text.y = element_text(family = "Aptos"))


##----Partie 5.1 - Boutures Racine----
Racine <- read.csv("R_Boutures_Racine.csv", sep = ";", dec = ",") 

Racine$Individu <- as.factor(Racine$Individu)
Racine$Traitement <- as.factor(Racine$Traitement)
Racine$Lettre_Bouture <- as.factor(Racine$Lettre_Bouture)
Racine$S0_Racine <- as.factor(Racine$S0_Racine)
Racine$S1_1_Racine <- as.factor(Racine$S1_1_Racine)
Racine$S1_2_Racine <- as.factor(Racine$S1_2_Racine)
Racine$S2_1_Racine <- as.factor(Racine$S2_1_Racine)
Racine$S2_2_Racine <- as.factor(Racine$S2_2_Racine)

#Graphique - Evolution du nombre de racine---- 
Evol_Nb_Racine <-  Racine %>% 
  group_by(Individu,Traitement) %>% 
  summarise(Sum_S0 = sum(S0_Nb_Racine),
            Sum_S1_1 = sum(S1_1_Nb_Racine),
            Sum_S1_2 = sum(S1_2_Nb_Racine),
            Sum_S2_1 = sum(S2_1_Nb_Racine),
            Sum_S2_2 = sum(S2_2_Nb_Racine))

Evol_Nb_Racine_longer_2 <- Racine %>% 
  mutate(S0_Nb_Racine = 0) %>% 
  dplyr::select(Individu,Traitement,Lettre_Bouture,S0_Nb_Racine,S1_1_Nb_Racine,S1_2_Nb_Racine,S2_1_Nb_Racine,S2_2_Nb_Racine) %>% 
  rename("0" = S0_Nb_Racine,
         "7" = S1_1_Nb_Racine,
         "10" = S1_2_Nb_Racine,
         "13" = S2_1_Nb_Racine,
         "17" = S2_2_Nb_Racine) %>% 
  pivot_longer(cols = 4:8,
               names_to = "Temps",
               values_to = "Nb_Racine")

Evol_Nb_Racine_longer_2$Temps <- as.numeric(Evol_Nb_Racine_longer_2$Temps)
  

#Annexe 6 - Ecolution du nombre de racine en fonction du temps
ggplot(Evol_Nb_Racine_longer_2, aes(x = Temps, y = Nb_Racine, group = Lettre_Bouture, color = Individu))+
  geom_line(size = 1, alpha = 0.4)+
  geom_point(alpha = 0.4)+
  labs(title = "Evolution du nombre racine sur les différentes boutures",
       x = "Temps depuis le début de l'expérience (jour)",
       y = "Nombre de racine",
       color = "Individu")+
  scale_color_manual(values = colors)+
  facet_grid(cols = vars(Traitement), rows = vars(Individu)) +
  scale_x_continuous(breaks = c(0,7,10,13,17))+
  
  theme(text = element_text(size = 12, family = "Aptos"),
        strip.text = element_text(size = 12, family = "Aptos"),
        axis.text.x = element_text(size = 11, family = "Aptos"),
        axis.text.y = element_text(family = "Aptos"))

#Graphique - Evolution du nombre de bouture morte----
#Pour les 4 solutions de bouturage
Racine_mort <- Racine %>% 
  mutate(S1_1_Statut = ifelse(S1_1_Obs == "mort", "mort", "vivant"),
         S1_2_Statut = ifelse(S1_2_Obs == "mort", "mort", "vivant"),
         S2_1_Statut = ifelse(S2_2_Obs == "mort", "mort", "vivant"),
         S2_2_Statut = ifelse(S2_2_Obs == "mort", "mort", "vivant")) %>% 
  group_by(Traitement) %>% 
  summarise(S0_Statut_Mort = 0,
            S1_1_Statut_Mort = sum(S1_1_Statut == "mort"),
            #S1_1_Statut_Vivant = sum(S1_1_Statut == "vivant"),
            S1_2_Statut_Mort = sum(S1_2_Statut == "mort"),
            #S1_2_Statut_Vivant = sum(S1_2_Statut == "vivant"),
            S2_1_Statut_Mort = sum(S2_1_Statut == "mort"),
            #S2_1_Statut_Vivant = sum(S2_1_Statut == "vivant"),
            S2_2_Statut_Mort = sum(S2_2_Statut == "mort"),
            #S2_2_Statut_Vivant = sum(S2_2_Statut == "vivant")
            ) 


#Pour les différents individu
Racine_mort_ind <- Racine %>% 
  mutate(S1_1_Statut = ifelse(S1_1_Obs == "mort", "mort", "vivant"),
         S1_2_Statut = ifelse(S1_2_Obs == "mort", "mort", "vivant"),
         S2_1_Statut = ifelse(S2_2_Obs == "mort", "mort", "vivant"),
         S2_2_Statut = ifelse(S2_2_Obs == "mort", "mort", "vivant")) %>% 
  group_by(Individu) %>% 
  summarise(S0_Statut_Mort = 0,
            S1_1_Statut_Mort = sum(S1_1_Statut == "mort"),
            #S1_1_Statut_Vivant = sum(S1_1_Statut == "vivant"),
            S1_2_Statut_Mort = sum(S1_2_Statut == "mort"),
            #S1_2_Statut_Vivant = sum(S1_2_Statut == "vivant"),
            S2_1_Statut_Mort = sum(S2_1_Statut == "mort"),
            #S2_1_Statut_Vivant = sum(S2_1_Statut == "vivant"),
            S2_2_Statut_Mort = sum(S2_2_Statut == "mort"),
            #S2_2_Statut_Vivant = sum(S2_2_Statut == "vivant")
  )

#Pour les différentes traitement et les différents individu
Racine_mort_ind_trait <- Racine %>% 
  mutate(S1_1_Statut = ifelse(S1_1_Obs == "mort", "mort", "vivant"),
         S1_2_Statut = ifelse(S1_2_Obs == "mort", "mort", "vivant"),
         S2_1_Statut = ifelse(S2_2_Obs == "mort", "mort", "vivant"),
         S2_2_Statut = ifelse(S2_2_Obs == "mort", "mort", "vivant")) %>% 
  group_by(Individu, Traitement) %>% 
  summarise(S0_Statut_Mort = 0,
            S1_1_Statut_Mort = sum(S1_1_Statut == "mort"),
            #S1_1_Statut_Vivant = sum(S1_1_Statut == "vivant"),
            S1_2_Statut_Mort = sum(S1_2_Statut == "mort"),
            #S1_2_Statut_Vivant = sum(S1_2_Statut == "vivant"),
            S2_1_Statut_Mort = sum(S2_1_Statut == "mort"),
            #S2_1_Statut_Vivant = sum(S2_1_Statut == "vivant"),
            S2_2_Statut_Mort = sum(S2_2_Statut == "mort"),
            #S2_2_Statut_Vivant = sum(S2_2_Statut == "vivant")
  )

Racine_mort_longer_ind_trait <- Racine_mort_ind_trait %>% 
  group_by(Individu, Traitement) %>% 
  summarise("0" = S0_Statut_Mort,
            "7" = S1_1_Statut_Mort,
            "10" = S1_2_Statut_Mort,
            "13" = S2_1_Statut_Mort,
            "17" = S2_2_Statut_Mort) %>% 
  pivot_longer(cols = "0":"17",
               names_to = "Temps",
               values_to = "Nb_mort") %>% 
  mutate(death_rate = Nb_mort*20) #Comme on a 5 réplica

Racine_mort_longer_ind_trait$Temps <- as.numeric(Racine_mort_longer_ind_trait$Temps)

ggplot(Racine_mort_longer_ind_trait, aes(x = Temps, y = death_rate, group = Individu, color = Individu))+
  geom_line(size = 1)+
  geom_point()+
  labs(title = "Evolution du pourcentage de boutures mortes",
       x = "Temps depuis le début de l'expérience (jour)",
       y = "Pourcentage de bouture mortes (%)",
       color = "Individu")+
  scale_color_manual(values = colors)+
  facet_grid(cols = vars(Traitement), rows = vars(Individu)) +
  scale_x_continuous(breaks = c(0,7,10,13,17))+
  
  theme(text = element_text(size = 12, family = "Aptos"),
        strip.text = element_text(size = 12, family = "Aptos"),
        axis.text.x = element_text(size = 11, family = "Aptos"),
        axis.text.y = element_text(family = "Aptos"))



#Analyse statistique Chi2 / Test Exact----
#Mortalité des boutures ~ Traitement
Racine_mort_Chi2 <- Racine_mort %>% 
  group_by(Traitement) %>% 
  summarise(Morte = S2_2_Statut_Mort,
            Vivante = 25-S2_2_Statut_Mort)

table_contingence <- as.table(cbind(
  Racine_mort_Chi2$Morte,
  Racine_mort_Chi2$Vivante))


colnames(table_contingence) <- c("Morte", "Vivante")
rownames(table_contingence) <- c("Eau","Hormones","NaCl","Hormones + NaCl")
print(table_contingence)

#Test du Chi²
chi2 <- chisq.test(table_contingence)

#Vérifier les conditions
chi2$expected  # Affiche les effectifs attendus

#Test chi2
chi2

#Mortalité des boutures ~ Ind
Racine_mort_Chi2 <- Racine_mort_ind %>% 
  group_by(Individu) %>% 
  summarise(Morte = S2_2_Statut_Mort,
            Vivante = 25-S2_2_Statut_Mort)

table_contingence <- as.table(cbind(
  Racine_mort_Chi2$Morte,
  Racine_mort_Chi2$Vivante))


colnames(table_contingence) <- c("Morte", "Vivante")
rownames(table_contingence) <- c("PIS10","PIS19","PIS33","PIS38","PIS40")
print(table_contingence)

#Test du Chi²
chi2 <- chisq.test(table_contingence)

#Vérifier les conditions
chi2$expected  # Affiche les effectifs attendus

#Test chi2
chi2


#Présence ou non de racine ~ Traitement
Racine_Chi2 <- Racine %>% 
  group_by(Traitement) %>% 
  summarise(Racine = sum(S2_2_Racine == "Oui"),
            Pas_Racine = sum(S2_2_Racine == "Non"))

table_contingence <- as.table(cbind(
  Racine_Chi2$Racine,
  Racine_Chi2$Pas_Racine))


colnames(table_contingence) <- c("Racine", "Pas_Racine")
rownames(table_contingence) <- c("Eau","Hormones","NaCl","Hormones + NaCl")
print(table_contingence)

#Test du Chi²
chi2 <- chisq.test(table_contingence)

#Vérifier les conditions
chi2$expected  # Affiche les effectifs attendus
#Certains effectif attendu sont > 5 donc 

#Test exact de fisher
fisher <- fisher.test(table_contingence)

fisher

#Présence ou non de racine ~ Individu
Racine_Chi2 <- Racine %>% 
  group_by(Individu) %>% 
  summarise(Racine = sum(S2_2_Racine == "Oui"),
            Pas_Racine = sum(S2_2_Racine == "Non"))

table_contingence <- as.table(cbind(
  Racine_Chi2$Racine,
  Racine_Chi2$Pas_Racine))


colnames(table_contingence) <- c("Racine", "Pas_Racine")
rownames(table_contingence) <- c("PIS10","PIS19","PIS33","PIS38","PIS40")
print(table_contingence)

#Test du Chi²
chi2 <- chisq.test(table_contingence)

#Vérifier les conditions
chi2$expected  # Affiche les effectifs attendus
#Certains effectif attendu sont > 5 donc 

#Test exact de fisher
fisher <- fisher.test(table_contingence)

fisher


##----Partie 5.2 - Boutures Conductivité----
Conductance <- read.csv("R_Boutures_Conductance.csv", sep = ";", dec = ",") 

Conductance$Individu <- as.factor(Conductance$Individu)
Conductance$Traitement <- as.factor(Conductance$Traitement)

#Graphique - Conductivité électrique----
Conductance_traitement_resmue <- Conductance %>% 
  filter(Individu != "Controle") %>% 
  group_by(Traitement) %>% 
  summarise(mean = mean(Conductance..mS.),
            var = var(Conductance..mS.),
            sd = sd(Conductance..mS.),
            N = n()) %>% 
  mutate(SD_S = mean + sd,
         SD_I = mean - sd,
         CI_S = mean + qt(0.975, N-1)*sqrt(var/N),
         CI_I = mean - qt(0.975, N-1)*sqrt(var/N))




#Compilation des données (data_all) -> Go to "Chargement des données compilées"----
# (1) Exertion Stigmatique Moyenne
Exertion_sti_cor_2 <- Exertion_stigmate_resum %>% 
  mutate(Exertion_stigmate = mean) %>% 
  dplyr::select(Ind, Exertion_stigmate)

# (2) Aire organe florale  moyenne
Morph_air_cor_2 <- Exertion_tout_graph %>% 
  dplyr::select(Ind,Organe,mean) %>% 
  pivot_wider(names_from = Organe,
              values_from = mean) %>% 
  rename(Area_petale = petale,
         Area_sepale = sepale,
         Area_etamine = etamine)

# (3) Long organe florale  moyenne
Morph_long_cor_2 <- Exertion_long_resum %>% 
  dplyr::select(Ind,Organe,mean) %>% 
  pivot_wider(names_from = Organe,
              values_from = mean) %>% 
  rename(Lg_etamine = etamine,
         Lg_pistil = pistile)

# (4) Fruit set
Fruit_set_cor_2 <- Croisement_resmu %>% 
  mutate(Croisement = paste(Parent_Femelle,"x",Parent_Male)) %>%
  dplyr::select(Parent_Femelle, Croisement, Fruit_set,Sum_Flower)


# (3) Nombre moyen de graine par fruit
Seed_cor_2 <- Graine %>% 
  mutate(Croisement = paste(Parent_Femelle,"x",Parent_Male)) %>%
  ungroup()%>% 
  dplyr::select(Parent_Femelle, Croisement, N_Moy_Graine)

# (4) Expression HT-A, HT-B, S10, S35
qPCR_cor_2 <- qPCR_resmue_filtre %>% 
  mutate(Expression = mean) %>% 
  dplyr::select(Ind, gene, Expression) %>% 
  pivot_wider(names_from = gene,
              values_from = Expression)

# (5) Tube polllinique
Tube_polli_cor_2 <- Tube_polli_resume %>%
  filter(Croisement %in%  c("PIS10 x PIS19", 
                            "PIS36 x PIS19",
                            "PIS10 x PIS14",
                            "PIS14 x PIS40",
                            "PIS14 x SC2",
                            "PIS40 x PIS14")) %>%
  dplyr::select(Croisement, Tier, mean) %>% 
  pivot_wider(names_from = Tier,
              values_from = mean)

#Rassemblemant des donnée,
rm(Data_all)
Data_all <- left_join(Fruit_set_cor_2, Seed_cor_2, by = c("Croisement","Parent_Femelle"))
Data_all <- left_join(Data_all, Exertion_sti_cor_2, by = c("Parent_Femelle" = "Ind")) 
Data_all <- left_join(Data_all, Morph_air_cor_2, by = c("Parent_Femelle" = "Ind")) 
Data_all <- left_join(Data_all, Morph_long_cor_2, by = c("Parent_Femelle" = "Ind")) 
Data_all <- left_join(Data_all, qPCR_cor_2, by = c("Parent_Femelle" = "Ind")) 
Data_all <- left_join(Data_all, Tube_polli_cor_2, by = c("Croisement"))

Data_all <- Data_all %>% 
  rename(Fruit = Fruit_set,
         Graine = N_Moy_Graine,
         Germ. = prop0,
         TP.1 = prop1,
         TP.2 = prop2,
         TP.3 = prop3)

S_allele_data_all <- S_allele %>% 
  dplyr::select(Parent_Femelle,S_allele_F_S10,S_allele_F_S35,S_allele_F_S36) %>% 
  group_by(Parent_Femelle,S_allele_F_S10,S_allele_F_S35,S_allele_F_S36) %>% 
  summarise(n=n()) %>% 
  dplyr::select(-n)

Data_all <- Data_all %>% 
  left_join(S_allele_data_all, join_by(Parent_Femelle))

Tab_TP.1 <- Tab_TP %>%
  dplyr::select(Croisement, Mean_prop)

Data_all <- Data_all %>% 
  left_join(Tab_TP.1, join_by(Croisement, Parent_Femelle))


save(Data_all, file = "R_Data_all.rda")

#Chargement des données compilées----
load("R_Data_all.rda")

#ACP - Discussion - Objectif 1----
ACP_1 <- Data_all %>%
  group_by(Parent_Femelle, Exertion_stigmate,Area_etamine,Area_petale,Area_sepale,HT_A,HT_B,S10,S16) %>% 
  summarise(N = n()) %>% 
  mutate(S_allele = mean(c(S10, S16), na.rm = TRUE)) %>% 
  ungroup() %>% 
  dplyr::select(Parent_Femelle, Exertion_stigmate,Area_etamine,Area_petale,Area_sepale,HT_A,HT_B,S_allele) %>% 
  na.omit()


#ACP
analyze_pca <- function(data, start_col = 3, font_family = "Aptos") {
  # Mise à l'échelle des données
  ACP_norm <- scale(data[, start_col:ncol(data)])
  rownames(ACP_norm) <- paste0(data$Parent_Femelle)
  
  # Vérification de la normalisation
  if (any(apply(ACP_norm, 2, sd) == 0)) {
    stop("Certaines variables ont une variance nulle après mise à l'échelle.")
  }
  
  # Calcul de l'ACP
  ACP_Norm <- prcomp(ACP_norm)
  
  
  # Graphique des individus (par croisement)
  pca_ind <- fviz_pca_ind(ACP_Norm, 
               geom.ind = "point", 
               habillage = data$Parent_Femelle, 
               palette = colors,
               repel = FALSE,
               pointsize = 3.5,
               mean.point = FALSE) +
    scale_shape_manual(values = rep(16, length(unique(data$Parent_Femelle)))) +
    theme(text = element_text(size = 12, family = font_family),
          strip.text = element_text(size = 12, family = font_family),
          axis.text.x = element_text(hjust = 1, size = 11, family = font_family),
          axis.text.y = element_text(family = font_family),
          panel.background = element_rect(fill = "grey70"),
          panel.grid.major = element_line(color = "gray90"),
          panel.grid.minor = element_blank())
  
  # Graphique des variables (contributions)
 pca_var <-  fviz_pca_var(ACP_Norm, 
               col.var = "contrib", 
               gradient.cols = c("#135B67", "#B698D7", "#9A099A"), 
               repel = TRUE) +
    theme(text = element_text(size = 12, family = font_family),
          strip.text = element_text(size = 12, family = font_family),
          axis.text.x = element_text(hjust = 1, size = 11, family = font_family),
          axis.text.y = element_text(family = font_family))
 
 pca_ind|pca_var
}

#Figure 37 : Analyse en composante principale (ACP) de la surface des organes floraux et des expressions de gène et allèle d'AI de 7 individus de S. chilense
analyze_pca(ACP_1, start_col = 2)


#Graphique - Corrélation entre expression de gène du parent femelle et compatibilité----
#Table corrélarion 
#Graphique
Exp_Comp_HT_A <- Data_all %>% 
  filter(Sum_Flower > 10) %>% 
  dplyr::select(Parent_Femelle,Croisement,Fruit,Graine,Mean_prop,HT_A) %>% 
  pivot_longer(cols = 3:5,
               names_to = "Compat",
               values_to = "Valeur")

compat_names <- c("Fruit" = "Taux de mise à fruit",
                  "Graine" = "Nombre moyen de graine par fruit",
                  "Mean_prop" = "Pourcentage moyen de tubes 
polliniques atteignant l'ovaire")

# Calcul des corrélations
Exp_Comp_HT_A_cor_fruit <- Data_all %>% 
  filter(Sum_Flower > 10) %>% 
  ungroup() %>% 
  dplyr::select(Fruit,HT_A) %>% 
  na.omit()

corHT_A_fruit <- cor(Exp_Comp_HT_A_cor_fruit$Fruit,Exp_Comp_HT_A_cor_fruit$HT_A)
pvalHT_A_fruit <- cor.mtest(Exp_Comp_HT_A_cor_fruit, conf.level = .95)$p["Fruit", "HT_A"]

# Calcul des corrélations
Exp_Comp_HT_A_cor_graine <- Data_all %>% 
  filter(Sum_Flower > 10) %>% 
  ungroup() %>% 
  dplyr::select(Graine,HT_A) %>% 
  na.omit()

corHT_A_graine <- cor(Exp_Comp_HT_A_cor_graine$Graine,Exp_Comp_HT_A_cor_graine$HT_A)
pvalHT_A_graine <- cor.mtest(Exp_Comp_HT_A_cor_graine, conf.level = .95)$p["Graine", "HT_A"]

# Calcul des corrélations
Exp_Comp_HT_A_cor_TP <- Data_all %>% 
  filter(Sum_Flower > 10) %>% 
  ungroup() %>% 
  dplyr::select(Mean_prop,HT_A) %>% 
  na.omit()

corHT_A_TP <- cor(Exp_Comp_HT_A_cor_TP$Mean_prop,Exp_Comp_HT_A_cor_TP$HT_A)
pvalHT_A_TP <- cor.mtest(Exp_Comp_HT_A_cor_TP, conf.level = .95)$p["Mean_prop", "HT_A"]


cor_table_HT_A <- tibble(
  Compat = c("Fruit", "Graine", "Mean_prop"),
  Cor = round(c(corHT_A_fruit,corHT_A_graine,corHT_A_TP),2),
  Pval = round(c(pvalHT_A_fruit,pvalHT_A_graine,pvalHT_A_TP),3),
)
  

cor_table_HT_A
Exp_Comp_HT_A <- merge(Exp_Comp_HT_A, cor_table_HT_A, by = "Compat")

plot_HT_A <- ggplot(Exp_Comp_HT_A, aes(x = HT_A, y = Valeur))+
  geom_point(aes(color = Parent_Femelle))+
  geom_smooth(method = "lm", se = F, col = "lightblue")+
  geom_text(aes(x = 10, y = 5, label = paste0("r = ", round(Cor, 2), " | p = ", signif(Pval, 3))), 
            inherit.aes = FALSE, size = 4) +
  labs(title = "",
       x = "Expression relative de HT-A",
       y = "",
       color = "Parent femelle impliqué
dans le croisement")+
  scale_color_manual(values = colors)+
  facet_wrap(vars(Compat), scales = "free_y",labeller = as_labeller(compat_names))+
  scale_x_continuous(trans = "pseudo_log",
                     breaks = c(0,2,4,10, 50, 100,150))+
  
  theme(text = element_text(size = 12, family = "Aptos"),
        strip.text = element_text(size = 12, family = "Aptos"),
        axis.text.x = element_text(size = 11, family = "Aptos"),
        axis.text.y = element_text(family = "Aptos"))

#HT-B

Exp_Comp_HT_B <- Data_all %>% 
  filter(Sum_Flower > 10) %>% 
  dplyr::select(Parent_Femelle,Croisement,Fruit,Graine,Mean_prop,HT_B) %>% 
  pivot_longer(cols = 3:5,
               names_to = "Compat",
               values_to = "Valeur")

compat_names <- c("Fruit" = "Taux de mise à fruit",
                  "Graine" = "Nombre moyen de graine par fruit",
                  "Mean_prop" = "Pourcentage moyen de tubes 
polliniques atteignant l'ovaire")

# Calcul des corrélations
Exp_Comp_HT_B_cor_fruit <- Data_all %>% 
  filter(Sum_Flower > 10) %>% 
  ungroup() %>% 
  dplyr::select(Fruit,HT_B) %>% 
  na.omit()

corHT_B_fruit <- cor(Exp_Comp_HT_B_cor_fruit$Fruit,Exp_Comp_HT_B_cor_fruit$HT_B)
pvalHT_B_fruit <- cor.mtest(Exp_Comp_HT_B_cor_fruit, conf.level = .95)$p["Fruit", "HT_B"]

# Calcul des corrélations
Exp_Comp_HT_B_cor_graine <- Data_all %>% 
  filter(Sum_Flower > 10) %>% 
  ungroup() %>% 
  dplyr::select(Graine,HT_B) %>% 
  na.omit()

corHT_B_graine <- cor(Exp_Comp_HT_B_cor_graine$Graine,Exp_Comp_HT_B_cor_graine$HT_B)
pvalHT_B_graine <- cor.mtest(Exp_Comp_HT_B_cor_graine, conf.level = .95)$p["Graine", "HT_B"]

# Calcul des corrélations
Exp_Comp_HT_B_cor_TP <- Data_all %>% 
  filter(Sum_Flower > 10) %>% 
  ungroup() %>% 
  dplyr::select(Mean_prop,HT_B) %>% 
  na.omit()

corHT_B_TP <- cor(Exp_Comp_HT_B_cor_TP$Mean_prop,Exp_Comp_HT_B_cor_TP$HT_B)
pvalHT_B_TP <- cor.mtest(Exp_Comp_HT_B_cor_TP, conf.level = .95)$p["Mean_prop", "HT_B"]


cor_table_HT_B <- tibble(
  Compat = c("Fruit", "Graine", "Mean_prop"),
  Cor = round(c(corHT_B_fruit,corHT_B_graine,corHT_B_TP),2),
  Pval = round(c(pvalHT_B_fruit,pvalHT_B_graine,pvalHT_B_TP),3),
)


cor_table_HT_B
Exp_Comp_HT_B <- merge(Exp_Comp_HT_B, cor_table_HT_B, by = "Compat")

plot_HT_B <- ggplot(Exp_Comp_HT_B, aes(x = HT_B, y = Valeur))+
  geom_point(aes(color = Parent_Femelle))+
  geom_smooth(method = "lm", se = F, col = "lightblue")+
  geom_text(aes(x = 10, y = 5, label = paste0("r = ", round(Cor, 2), " | p = ", signif(Pval, 3))), 
            inherit.aes = FALSE, size = 4) +
  labs(title = "",
       x = "Expression relative de HT-B",
       y = "",
       color = "Parent femelle impliqué
dans le croisement")+
  scale_color_manual(values = colors)+
  facet_wrap(vars(Compat), scales = "free_y",labeller = as_labeller(compat_names))+
  scale_x_continuous(trans = "pseudo_log",
                     breaks = c(0,2,4,10, 50, 80,150))+
  
  theme(text = element_text(size = 12, family = "Aptos"),
        strip.text = element_blank(),
        axis.text.x = element_text(size = 11, family = "Aptos"),
        axis.text.y = element_text(family = "Aptos"))


#S_allele

Exp_Comp_S_allele <- Data_all %>% 
  filter(Sum_Flower > 10) %>% 
  mutate(S_allele = mean(c(S10, S16), na.rm = TRUE)) %>% 
  dplyr::select(Parent_Femelle,Croisement,Fruit,Graine,Mean_prop,S_allele) %>% 
  pivot_longer(cols = 3:5,
               names_to = "Compat",
               values_to = "Valeur")

compat_names <- c("Fruit" = "Taux de mise à fruit",
                  "Graine" = "Nombre moyen de graine par fruit",
                  "Mean_prop" = "Pourcentage moyen de tubes 
polliniques atteignant l'ovaire")


# Calcul des corrélations
Exp_Comp_S_allele_cor_fruit <- Data_all %>% 
  filter(Sum_Flower > 10) %>% 
  mutate(S_allele = mean(c(S10, S16), na.rm = TRUE)) %>% 
  ungroup() %>% 
  dplyr::select(Fruit,S_allele) %>% 
  na.omit()

corS_allele_fruit <- cor(Exp_Comp_S_allele_cor_fruit$Fruit,Exp_Comp_S_allele_cor_fruit$S_allele)
pvalS_allele_fruit <- cor.mtest(Exp_Comp_S_allele_cor_fruit, conf.level = .95)$p["Fruit", "S_allele"]

# Calcul des corrélations
Exp_Comp_S_allele_cor_graine <- Data_all %>% 
  filter(Sum_Flower > 10) %>% 
  mutate(S_allele = mean(c(S10, S16), na.rm = TRUE)) %>% 
  ungroup() %>% 
  dplyr::select(Graine,S_allele) %>% 
  na.omit()

corS_allele_graine <- cor(Exp_Comp_S_allele_cor_graine$Graine,Exp_Comp_S_allele_cor_graine$S_allele)
pvalS_allele_graine <- cor.mtest(Exp_Comp_S_allele_cor_graine, conf.level = .95)$p["Graine", "S_allele"]

# Calcul des corrélations
Exp_Comp_S_allele_cor_TP <- Data_all %>% 
  filter(Sum_Flower > 10) %>% 
  mutate(S_allele = mean(c(S10, S16), na.rm = TRUE)) %>% 
  ungroup() %>% 
  dplyr::select(Mean_prop,S_allele) %>% 
  na.omit()

corS_allele_TP <- cor(Exp_Comp_S_allele_cor_TP$Mean_prop,Exp_Comp_S_allele_cor_TP$S_allele)
pvalS_allele_TP <- cor.mtest(Exp_Comp_S_allele_cor_TP, conf.level = .95)$p["Mean_prop", "S_allele"]


cor_table_S_allele <- tibble(
  Compat = c("Fruit", "Graine", "Mean_prop"),
  Cor = round(c(corS_allele_fruit,corS_allele_graine,corS_allele_TP),2),
  Pval = round(c(pvalS_allele_fruit,pvalS_allele_graine,pvalS_allele_TP),3),
)


cor_table_S_allele
Exp_Comp_S_allele <- merge(Exp_Comp_S_allele, cor_table_S_allele, by = "Compat")

plot_S_allele <-ggplot(Exp_Comp_S_allele, aes(x = S_allele, y = Valeur))+
  geom_point(aes(color = Parent_Femelle))+
  geom_smooth(method = "lm", se = F, col = "lightblue")+
  geom_text(aes(x = 10, y = 5, label = paste0("r = ", round(Cor, 2), " | p = ", signif(Pval, 3))), 
            inherit.aes = FALSE, size = 4) +
  labs(title = "",
       x = "Expression relative des ou du S-allele(s)",
       y = "",
       color = "Parent femelle impliqué
dans le croisement")+
  scale_color_manual(values = colors)+
  facet_wrap(vars(Compat), scales = "free_y",labeller = as_labeller(compat_names))+
  scale_x_continuous(trans = "pseudo_log",
                     breaks = c(0,2,4,10, 50, 80,150))+
  
  theme(text = element_text(size = 12, family = "Aptos"),
        strip.text = element_blank(),
        axis.text.x = element_text(size = 11, family = "Aptos"),
        axis.text.y = element_text(family = "Aptos"),)

plot_HT_A/plot_HT_B/plot_S_allele


Exp_Comp_HT_A_lg <- Exp_Comp_HT_A %>% 
  pivot_longer(cols = 4,
               names_to = "gene",
               values_to = "gene_value")

Exp_Comp_HT_B_lg <- Exp_Comp_HT_B %>% 
  pivot_longer(cols = 4,
               names_to = "gene",
               values_to = "gene_value")

Exp_Comp_S_allele_lg <- Exp_Comp_S_allele %>% 
  pivot_longer(cols = 4,
               names_to = "gene",
               values_to = "gene_value")

Exp_Comp_all <- rbind(Exp_Comp_HT_A_lg,Exp_Comp_HT_B_lg,Exp_Comp_S_allele_lg)

compat_names <- c("Fruit" = "Taux de mise à fruit",
                  "Graine" = "Nombre moyen 
de graines par fruit",
                  "Mean_prop" = "% moyen de TP 
atteignant l'ovaire",
                  "HT_A" = "HT-A",
                  "HT_B" = "HT-B",
                  "S_allele" = "S-allèle")


#Figure 34 : Compatibilité réelle des croisements en fonction de l'expression relative des gènes HT-A, HT-B et de l'expression relative de S-allèle(s) des parents femelle impliquée dans ces croisements
ggplot(Exp_Comp_all, aes(x = gene_value, y = Valeur))+
  geom_point(size = 2.5,aes(color = Parent_Femelle))+
  geom_smooth(method = "lm", se = F, col = "lightblue")+
  geom_text(aes(x = 8, y = 5, label = paste0("r = ", round(Cor, 2), " | p = ", signif(Pval, 3))), 
            inherit.aes = FALSE, size = 5, alpha =0.025,family = "Aptos") +
  labs(title = "",
       x = "Expression relative",
       y = "",
       color = "Parent femelle impliqué
dans le croisement")+
  scale_color_manual(values = colors)+
  facet_grid(Compat ~ gene, scales = "free",labeller = as_labeller(compat_names))+
  scale_x_continuous(trans = "pseudo_log",
                     breaks = c(0,2,4,10,20,50, 80,150))+
  
  theme(text = element_text(size = 14, family = "Aptos"),
        strip.text = element_text(size = 14, family = "Aptos"),
        axis.text.x = element_text(size = 11, family = "Aptos"),
        axis.text.y = element_text(family = "Aptos"),)
