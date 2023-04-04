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

# Build model
as_site_all <- lm(Solid_conc ~ Site, data = dfAs)

# ANOVA
anova(as_site_all)

# Al
dfAl <- kl_icp[kl_icp$Element=="Al",]
dfAl <- dfAl[dfAl$Measurement_type=="P",]

# Build model
al_site_all <- lm(Solid_conc ~ Site, data = dfAl)

# ANOVA
anova(al_site_all)


# Build model
as_site_all <- lm(Solid_conc ~ Site, data = dfAs)

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


