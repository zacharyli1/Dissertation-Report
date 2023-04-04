# Dissertation Statistical Analysis 
# Script Purpose: Analyze metal and metalloid concentrations in Kinghorn Loch
# Zachary Li - s1996567@ed.ac.uk
# Created 03/30/23

# Install packages----
install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyr")
install.packages("hablar")
install.packages("tidyverse")

# Libraries----
library(ggplot2)
library(dplyr)
library(tidyr)
library(hablar)
library(tidyverse)

# Load data----
kl_icp <- read.csv(file = 'data/KL_analysis_clean.csv')

# Remove NA values
kl_icp <- kl_icp %>%
  drop_na()

# Visualize the data set
head(kl_icp)
str(kl_icp)

# Analyze for As----
# Create new data set with only As PLANT samples
dfAs <- kl_icp[kl_icp$Element=="As",]
dfAs <- dfAs[dfAs$Measurement_type=="P",]

# Change "Site" to character
dfAs <- dfAs %>%
  convert(chr(Site))

# Boxplot of As for each species by site
(As_site_boxplot <- ggplot(dfAs, aes(Species, Solid_conc, fill = Site)) +
    geom_boxplot() +
    scale_fill_manual(values = c("#e74c3c", "#2ecc71", "#0099f8")) +
    scale_x_discrete(labels=c(expression(italic('Chara spp')), expression(italic('M. spicatum')), expression(italic('P. pectinatus')))) +
    labs(title = "As", 
          y = "Concentration (μg/g)\n") + 
    theme_classic() + 
    theme() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title.x = element_blank(),
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.box.background = element_rect(color = "grey", size = 0.3)))

# Boxplot of As for each species by site
(As_site_boxplot <- ggplot(dfAs, aes(Species, Solid_conc, fill = Tissue_type)) +
    geom_boxplot() +
    scale_fill_manual(values = c("#2ecc71", "#996633")) +
    scale_x_discrete(labels=c(expression(italic('Chara spp')), expression(italic('M. spicatum')), expression(italic('P. pectinatus')))) +
    labs(title = "As", 
         y = "Concentration (μg/g)\n") + 
    theme_classic() + 
    theme() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title.x = element_blank(),
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.box.background = element_rect(color = "grey", size = 0.3)))


# Create new data set with only As SEDIMENT samples
dfAs3 <- kl_icp[kl_icp$Element=="As",]
dfAs3 <- dfAs2[dfAs2$Measurement_type=="S",]

# Change "Site" to character
dfAs3 <- dfAs3 %>%
  convert(chr(Site))

# Boxplot of As in sediment for each site
(As_sediment_boxplot <- ggplot(dfAs3, aes(Site, Solid_conc, fill = Site)) +
    geom_boxplot() +
    scale_fill_manual(values = c("#FFFF00", "#9933CC")) +
    labs(title = "As", 
         y = "Concentration (μg/g)\n") + 
    theme_classic() + 
    theme() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.box.background = element_rect(color = "grey", size = 0.3)))

# ANOVA for SITE regardless of species
# As
dfAs <- kl_icp[kl_icp$Element=="As",]
dfAs <- dfAs[dfAs$Measurement_type=="P",]

# Change "Site" to character
dfAs <- dfAs %>%
  convert(chr(Site))

# Build model
as_site_all <- lm(Solid_conc ~ Site, data = dfAs)

# ANOVA
anova(as_site_all)

# Basic visualization
(As_site_boxplot_basic <- ggplot(dfAs, aes(Site, Solid_conc, fill = Site)) +
    geom_boxplot() +
    scale_fill_manual(values = c("#e74c3c", "#2ecc71", "#0099f8")) +
    labs(title = "As", 
         y = "Concentration (μg/g)\n", x = "Site") + 
    theme_classic() + 
    theme() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.box.background = element_rect(color = "grey", size = 0.3)))

# Al
dfAl <- kl_icp[kl_icp$Element=="Al",]
dfAl <- dfAl[dfAl$Measurement_type=="P",]

# Change "Site" to character
dfAl <- dfAl %>%
  convert(chr(Site))

# Build model
al_site_all <- lm(Solid_conc ~ Site, data = dfAl)

# ANOVA
anova(al_site_all)

# Basic visualization
(Al_site_boxplot_basic <- ggplot(dfAl, aes(Site, Solid_conc, fill = Site)) +
    geom_boxplot() +
    scale_fill_manual(values = c("#e74c3c", "#2ecc71", "#0099f8")) +
    labs(title = "As", 
         y = "Concentration (μg/g)\n", x = "Site") + 
    theme_classic() + 
    theme() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.box.background = element_rect(color = "grey", size = 0.3)))


# V
dfV <- kl_icp[kl_icp$Element=="V",]
dfV <- dfV[dfV$Measurement_type=="P",]

# Change "Site" to character
dfV <- dfV %>%
  convert(chr(Site))

# Build model
V_site_all <- lm(Solid_conc ~ Site, data = dfV)

# ANOVA
anova(V_site_all)

# Basic visualization
(V_site_boxplot_basic <- ggplot(dfV, aes(Site, Solid_conc, fill = Site)) +
    geom_boxplot() +
    scale_fill_manual(values = c("#e74c3c", "#2ecc71", "#0099f8")) +
    labs(title = "As", 
         y = "Concentration (μg/g)\n", x = "Site") + 
    theme_classic() + 
    theme() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.box.background = element_rect(color = "grey", size = 0.3)))

# Cr
dfCr <- kl_icp[kl_icp$Element=="Cr",]
dfCr <- dfCr[dfCr$Measurement_type=="P",]

# Change "Site" to character
dfCr <- dfCr %>%
  convert(chr(Site))

# Build model
Cr_site_all <- lm(Solid_conc ~ Site, data = dfCr)

# ANOVA
anova(Cr_site_all)

# Basic visualization
(Cr_site_boxplot_basic <- ggplot(dfCr, aes(Site, Solid_conc, fill = Site)) +
    geom_boxplot() +
    scale_fill_manual(values = c("#e74c3c", "#2ecc71", "#0099f8")) +
    labs(title = "As", 
         y = "Concentration (μg/g)\n", x = "Site") + 
    theme_classic() + 
    theme() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.box.background = element_rect(color = "grey", size = 0.3)))

# Statistical analysis of As----
# Effect of site for Chara
dfAs <- kl_icp[kl_icp$Element=="As",]
dfAs <- dfAs[dfAs$Measurement_type=="P",]
dfAs <- dfAs[dfAs$Species=="CH",]

# T-test model
(t_test_chara <- t.test(Solid_conc~Site, data = dfAs))

# Summary table
summary(t_test_chara)

# Visualize results
(chara_lm_boxplot <- ggplot(dfAs, aes(Site, Solid_conc, fill = Site)) +
    geom_boxplot() +
    scale_fill_manual(values = c("#FF9933", "#339999")) +
    labs(title = "As", 
         y = "Concentration (μg/g)\n") + 
    theme_classic() + 
    theme() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.box.background = element_rect(color = "grey", size = 0.3)))

# Effect of site for Myriophyllum
dfAs <- kl_icp[kl_icp$Element=="As",]
dfAs <- dfAs[dfAs$Measurement_type=="P",]
dfAs <- dfAs[dfAs$Species=="MY",]

# Model establishment
(t_test_my <- t.test(Solid_conc~Site, data = dfAs))

# Visualize results
(my_lm_boxplot <- ggplot(dfAs, aes(Site, Solid_conc, fill = Site)) +
    geom_boxplot() +
    scale_fill_manual(values = c("#FF9933", "#339999")) +
    labs(title = "As", 
         y = "Concentration (μg/g)\n") + 
    theme_classic() + 
    theme() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.box.background = element_rect(color = "grey", size = 0.3)))


# Leaves vs stems for Chara
dfAs <- kl_icp[kl_icp$Element=="As",]
dfAs <- dfAs[dfAs$Measurement_type=="P",]
dfAs <- dfAs[dfAs$Species=="CH",]

# Model establishment
chara_lm2 <- lm(Solid_conc~Tissue_type, data = dfAs)

# Visualize results
(chara_lm2_boxplot <- ggplot(dfAs, aes(Tissue_type, Solid_conc, fill = Tissue_type)) +
    geom_boxplot() +
    scale_fill_manual(values = c("#FF9933", "#339999")) +
    labs(title = "As", 
         y = "Concentration (μg/g)\n") + 
    theme_classic() + 
    theme() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.box.background = element_rect(color = "grey", size = 0.3)))

# Calculate mean and sd
aggregate(dfAs$Solid_conc, by=list(dfAs$Tissue_type), FUN = mean)
aggregate(dfAs$Solid_conc, by=list(dfAs$Tissue_type), FUN = sd)

# Species differences in metal and metalloid concentrations----
# As
dfAs <- kl_icp[kl_icp$Element=="As",]
dfAs <- dfAs[dfAs$Measurement_type=="P",]

# Change "Site" to character
dfAs <- dfAs %>%
  convert(chr(Site))

# Build model
As_species <- lm(Solid_conc ~ Species, data = dfAs)

# ANOVA
anova(As_species)

# Boxplot visualization
(As_species_boxplot <- ggplot(dfAs, aes(Species, Solid_conc, fill = Species)) +
    geom_boxplot() +
    scale_fill_manual(values = c("#e74c3c", "#2ecc71", "#0099f8")) +
    scale_x_discrete(labels=c(expression(italic('Chara spp')), expression(italic('M. spicatum')), expression(italic('P. pectinatus')))) +
    labs(title = "As", 
         y = "Concentration (μg/g)\n", x = "Species") + 
    theme_classic() + 
    theme() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title.x = element_blank(),
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.position = "none"))

# Mean and sd
aggregate(dfAs$Solid_conc, by=list(dfAs$Species), FUN = mean)
aggregate(dfAs$Solid_conc, by=list(dfAs$Species), FUN = sd)


# 2nd option
# Boxplot of As for each species by site
(As_site_boxplot <- ggplot(dfAs, aes(Species, Solid_conc, fill = Site)) +
    geom_boxplot() +
    scale_fill_manual(values = c("#e74c3c", "#2ecc71", "#0099f8")) +
    scale_x_discrete(labels=c(expression(italic('Chara spp')), expression(italic('M. spicatum')), expression(italic('P. pectinatus')))) +
    labs(title = "As", 
         y = "Concentration (μg/g)\n") + 
    theme_classic() + 
    theme() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title.x = element_blank(),
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.box.background = element_rect(color = "grey", size = 0.3)))

# Al
dfAl <- kl_icp[kl_icp$Element=="Al",]
dfAl <- dfAl[dfAl$Measurement_type=="P",]

# Change "Site" to character
dfAl <- dfAl %>%
  convert(chr(Site))

# Build model
al_species <- lm(Solid_conc ~ Species, data = dfAl)

# ANOVA
anova(al_species)

# Basic visualization
(Al_species_boxplot <- ggplot(dfAl, aes(Species, Solid_conc, fill = Species)) +
    geom_boxplot() +
    scale_fill_manual(values = c("#e74c3c", "#2ecc71", "#0099f8")) +
    scale_x_discrete(labels=c(expression(italic('Chara spp')), expression(italic('M. spicatum')), expression(italic('P. pectinatus')))) +
    labs(title = "Al", 
         y = "Concentration (μg/g)\n", x = "Species") + 
    theme_classic() + 
    theme() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title.x = element_blank(),
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.position = "none"))

# V 
dfV <- kl_icp[kl_icp$Element=="V",]
dfV <- dfV[dfV$Measurement_type=="P",]

# Change "Site" to character
dfV <- dfV %>%
  convert(chr(Site))

# Build model
V_species <- lm(Solid_conc ~ Species, data = dfV)

# ANOVA
anova(V_species)

# Basic visualization
(V_species_boxplot <- ggplot(dfV, aes(Species, Solid_conc, fill = Species)) +
    geom_boxplot() +
    scale_fill_manual(values = c("#e74c3c", "#2ecc71", "#0099f8")) +
    scale_x_discrete(labels=c(expression(italic('Chara spp')), expression(italic('M. spicatum')), expression(italic('P. pectinatus')))) +
    labs(title = "V", 
         y = "Concentration (μg/g)\n", x = "Species") + 
    theme_classic() + 
    theme() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title.x = element_blank(),
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.position = "none"))

# Mean and sd
aggregate(dfV$Solid_conc, by=list(dfV$Species), FUN = mean)
aggregate(dfV$Solid_conc, by=list(dfV$Species), FUN = sd)


# Cr
dfCr <- kl_icp[kl_icp$Element=="Cr",]
dfCr <- dfCr[dfCr$Measurement_type=="P",]

# Change "Site" to character
dfCr <- dfCr %>%
  convert(chr(Site))

# Build model
Cr_species <- lm(Solid_conc ~ Species, data = dfCr)

# ANOVA
anova(Cr_species)

# Basic visualization
(Cr_species_boxplot <- ggplot(dfCr, aes(Species, Solid_conc, fill = Species)) +
    geom_boxplot() +
    scale_fill_manual(values = c("#e74c3c", "#2ecc71", "#0099f8")) +
    scale_x_discrete(labels=c(expression(italic('Chara spp')), expression(italic('M. spicatum')), expression(italic('P. pectinatus')))) +
    labs(title = "Cr", 
         y = "Concentration (μg/g)\n", x = "Species") + 
    theme_classic() + 
    theme() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title.x = element_blank(),
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.position = "none"))

# Mean and sd
aggregate(dfCr$Solid_conc, by=list(dfCr$Species), FUN = mean)
aggregate(dfCr$Solid_conc, by=list(dfCr$Species), FUN = sd)

# Pb206
dfPb206 <- kl_icp[kl_icp$Element=="Pb206",]
dfPb206 <- dfPb206[dfPb206$Measurement_type=="P",]

# Change "Site" to character
dfPb206 <- dfPb206 %>%
  convert(chr(Site))

# Build model
Pb206_species <- lm(Solid_conc ~ Species, data = dfPb206)

# ANOVA
anova(Pb206_species)

# Basic visualization
(Pb206_species_boxplot <- ggplot(dfPb206, aes(Species, Solid_conc, fill = Species)) +
    geom_boxplot() +
    scale_fill_manual(values = c("#e74c3c", "#2ecc71", "#0099f8")) +
    scale_x_discrete(labels=c(expression(italic('Chara spp')), expression(italic('M. spicatum')), expression(italic('P. pectinatus')))) +
    labs(title = expression(text = ""^206*"Pb"), 
         y = "Concentration (μg/g)\n", x = "Species") + 
    theme_classic() + 
    theme() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title.x = element_blank(),
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.position = "none"))

# Pb207
dfPb207 <- kl_icp[kl_icp$Element=="Pb207",]
dfPb207 <- dfPb207[dfPb207$Measurement_type=="P",]

# Change "Site" to character
dfPb207 <- dfPb207 %>%
  convert(chr(Site))

# Build model
Pb207_species <- lm(Solid_conc ~ Species, data = dfPb207)

# ANOVA
anova(Pb207_species)

# Basic visualization
(Pb207_species_boxplot <- ggplot(dfPb207, aes(Species, Solid_conc, fill = Species)) +
    geom_boxplot() +
    scale_fill_manual(values = c("#e74c3c", "#2ecc71", "#0099f8")) +
    scale_x_discrete(labels=c(expression(italic('Chara spp')), expression(italic('M. spicatum')), expression(italic('P. pectinatus')))) +
    labs(title = expression(text = ""^207*"Pb"), 
         y = "Concentration (μg/g)\n", x = "Species") + 
    theme_classic() + 
    theme() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title.x = element_blank(),
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.position = "none"))

# Pb208
dfPb208 <- kl_icp[kl_icp$Element=="Pb208",]
dfPb208 <- dfPb208[dfPb208$Measurement_type=="P",]

# Change "Site" to character
dfPb208 <- dfPb208 %>%
  convert(chr(Site))

# Build model
Pb208_species <- lm(Solid_conc ~ Species, data = dfPb208)

# ANOVA
anova(Pb208_species)

# Basic visualization
(Pb208_species_boxplot <- ggplot(dfPb208, aes(Species, Solid_conc, fill = Species)) +
    geom_boxplot() +
    scale_fill_manual(values = c("#e74c3c", "#2ecc71", "#0099f8")) +
    scale_x_discrete(labels=c(expression(italic('Chara spp')), expression(italic('M. spicatum')), expression(italic('P. pectinatus')))) +
    labs(title = expression(text = ""^208*"Pb"), 
         y = "Concentration (μg/g)\n", x = "Species") + 
    theme_classic() + 
    theme() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title.x = element_blank(),
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.position = "none"))








