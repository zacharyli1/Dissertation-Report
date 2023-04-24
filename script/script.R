# Dissertation Statistical Analysis 
# Script Purpose: Analyze metal and metalloid concentrationtrations in Kinghorn Loch
# Zachary Li - s1996567@ed.ac.uk
# Created 03/30/23

# Install packages----
install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyr")
install.packages("hablar")
install.packages("tidyverse")
install.packages("ggpubr")
install.packages("cowplot")

# Libraries----
library(ggplot2)
library(dplyr)
library(tidyr)
library(hablar)
library(tidyverse)
library(ggpubr)
library(cowplot)

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

# SITE ANOVA regardless of species----
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

# Check assumptions
# Shapiro test
site_resids <- resid(as_site_all)
shapiro.test(site_resids)

# Bartlett's test
bartlett.test(Solid_conc~Site, data = dfAs)

# View plots
plot(as_site_all)

# Log Solid_conc
dfAs$log_Solid_conc <- log(dfAs$Solid_conc)

# Build model with log-transformed data
as_site_all2 <- lm(log_Solid_conc ~ Site, data = dfAs)

# Check assumptions again
site_resids <- resid(as_site_all2)
shapiro.test(site_resids)

# Summary table
summary(as_site_all)

# Basic visualization
(As_site_boxplot <- ggplot(dfAs, aes(Site, Solid_conc, fill = Site)) +
    geom_boxplot() +
    scale_fill_manual(values = c("#FFCC66", "#FF6666", "#660000")) +
    labs(title = "As", 
         y = expression(Concentration~(μg~g^-1)), x = "Site") + 
    theme_classic() + 
    theme() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.position = "none"))

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

# Check assumptions
site_resids2 <- resid(as_site_all)
shapiro.test(site_resids2)
bartlett.test(Solid_conc~Site, data = dfAs)

plot(as_site_all)


# Summary table
summary(al_site_all)

# Basic visualization
(Al_site_boxplot <- ggplot(dfAl, aes(Site, Solid_conc, fill = Site)) +
    geom_boxplot() +
    scale_fill_manual(values = c("#FFCC66", "#FF6666", "#660000")) +
    labs(title = "Al", 
         y = expression(Concentration~(μg~g^-1)), x = "Site") + 
    theme_classic() + 
    theme() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
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
V_site_all <- lm(Solid_conc ~ Site, data = dfV)

# ANOVA
anova(V_site_all)

# Summary table
summary(V_site_all)

# Basic visualization
(V_site_boxplot <- ggplot(dfV, aes(Site, Solid_conc, fill = Site)) +
    geom_boxplot() +
    scale_fill_manual(values = c("#FFCC66", "#FF6666", "#660000")) +
    labs(title = "V", 
         y = expression(Concentration~(μg~g^-1)), x = "Site") + 
    theme_classic() + 
    theme() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.position = "none"))


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

# Summary table
summary(Cr_site_all)

# Basic visualization
(Cr_site_boxplot <- ggplot(dfCr, aes(Site, Solid_conc, fill = Site)) +
    geom_boxplot() +
    scale_fill_manual(values = c("#FFCC66", "#FF6666", "#660000")) +
    labs(title = "Cr", 
         y = expression(Concentration~(μg~g^-1)), x = "Site") + 
    theme_classic() + 
    theme() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.position = "none"))

# Pb206
dfPb206 <- kl_icp[kl_icp$Element=="Pb206",]
dfPb206 <- dfPb206[dfPb206$Measurement_type=="P",]

# Change "Site" to character
dfPb206 <- dfPb206 %>%
  convert(chr(Site))

# Build model
Pb206_site_all <- lm(Solid_conc ~ Site, data = dfPb206)

# ANOVA
anova(Pb206_site_all)

# Summary table
summary(Pb206_site_all)

# Basic visualization
(Pb206_site_boxplot <- ggplot(dfPb206, aes(Site, Solid_conc, fill = Site)) +
    geom_boxplot() +
    scale_fill_manual(values = c("#FFCC66", "#FF6666", "#660000")) +
    labs(title = expression(bold(text = ""^206*"Pb")), 
         y = expression(Concentration~(μg~g^-1)), x = "Site") + 
    theme_classic() + 
    theme() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.position = "none"))

# Combine all site plots 
combined_site_plots <- ggarrange(As_site_boxplot, Al_site_boxplot, V_site_boxplot, Cr_site_boxplot, Pb206_site_boxplot,
                              ncol = 3, nrow = 2)
combined_site_plots


# Check sediment differences between sites for Chara----
dfAs <- kl_icp[kl_icp$Element=="As",]
dfAs <- dfAs[dfAs$Measurement_type=="PW",]

# Convert site to character
dfAs <- dfAs %>%
  convert(chr(Site))

# Build model
as_site_chara_test <- lm(Solid_conc ~ Site, data = dfAs)

# ANOVA
anova(as_site_all)

# Check assumptions
# Shapiro test
site_resids <- resid(as_site_all)
shapiro.test(site_resids)

# Bartlett's test
bartlett.test(Solid_conc~Site, data = dfAs)

# Statistical analysis of As----
# Effect of site for Chara
dfAs <- kl_icp[kl_icp$Element=="As",]
dfAs <- dfAs[dfAs$Measurement_type=="P",]

# Filter data sets by Chara
dfAs <- dfAs[dfAs$Species=="CH",]
dfAl <- dfAl[dfAl$Species=="CH",]
dfV <- dfV[dfV$Species=="CH",]
dfCr <- dfCr[dfCr$Species=="CH",]
dfPb206 <- dfPb206[dfPb206$Species=="CH",]

# Change "Site" to character
dfAs <- dfAs %>%
  convert(chr(Site))

dfAl <- dfAl %>%
  convert(chr(Site))

dfV <- dfV %>%
  convert(chr(Site))

dfCr <- dfCr %>%
  convert(chr(Site))

dfPb206 <- dfPb206 %>%
  convert(chr(Site))


# T-test model
(t_test_chara_As <- t.test(Solid_conc~Site, data = dfAs)) # Yes
(t_test_chara_Al <- t.test(Solid_conc~Site, data = dfAl)) # No
(t_test_chara_V <- t.test(Solid_conc~Site, data = dfV)) # Yes
(t_test_chara_Cr <- t.test(Solid_conc~Site, data = dfCr)) # No
(t_test_chara_Pb206 <- t.test(Solid_conc~Site, data = dfPb206)) # No


# Mean and sd testing
dfAs <- dfAs[dfAs$Data_collector=="L",]

aggregate(dfAs$Solid_conc, by=list(dfAs$Site), FUN = sd)
aggregate(dfAl$Solid_conc, by=list(dfAl$Site), FUN = sd)
aggregate(dfV$Solid_conc, by=list(dfV$Site), FUN = sd)
aggregate(dfCr$Solid_conc, by=list(dfCr$Site), FUN = sd)
aggregate(dfPb206$Solid_conc, by=list(dfPb206$Site), FUN = sd)

# Summary table
summary(t_test_chara)

# Mean and sd
aggregate(dfAs$Solid_conc, by=list(dfAs$Site), FUN = mean)
aggregate(dfAs$Solid_conc, by=list(dfAs$Site), FUN = sd)


# Visualize results 
# As
(chara_site_As_boxplot <- ggplot(dfAs, aes(Site, Solid_conc, fill = Site)) +
    geom_boxplot() +
    scale_fill_manual(values = c("#FF9933", "#339999")) +
    labs(title = expression(paste(bold("As")~"("*italic("Chara")~paste("spp.)"))), 
         y = expression(Concentration~(μg~g^-1))) + 
    theme_classic() + 
    theme() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.position = "none"))

# Al
(chara_site_Al_boxplot <- ggplot(dfAl, aes(Site, Solid_conc, fill = Site)) +
    geom_boxplot() +
    scale_fill_manual(values = c("#FF9933", "#339999")) +
    labs(title = expression(paste(bold("Al")~"("*italic("Chara")~paste("spp.)"))), 
         y = expression(Concentration~(μg~g^-1))) + 
    theme_classic() + 
    theme() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.position = "none"))

# V 
(chara_site_V_boxplot <- ggplot(dfV, aes(Site, Solid_conc, fill = Site)) +
    geom_boxplot() +
    scale_fill_manual(values = c("#FF9933", "#339999")) +
    labs(title = expression(paste(bold("V")~"("*italic("Chara")~paste("spp.)"))), 
         y = expression(Concentration~(μg~g^-1))) + 
    theme_classic() + 
    theme() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.position = "none"))

# Cr
(chara_site_Cr_boxplot <- ggplot(dfCr, aes(Site, Solid_conc, fill = Site)) +
    geom_boxplot() +
    scale_fill_manual(values = c("#FF9933", "#339999")) +
    labs(title = expression(paste(bold("Cr")~"("*italic("Chara")~paste("spp.)"))), 
         y = expression(Concentration~(μg~g^-1))) + 
    theme_classic() + 
    theme() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.position = "none"))

# Pb206
(chara_site_Pb206_boxplot <- ggplot(dfPb206, aes(Site, Solid_conc, fill = Site)) +
    geom_boxplot() +
    scale_fill_manual(values = c("#FF9933", "#339999")) +
    labs(title = expression(paste(bold(text = ""^206*"Pb")~"("*italic("Chara")~paste("spp.)"))), 
         y = expression(Concentration~(μg~g^-1))) + 
    theme_classic() + 
    theme() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.position = "none"))


# Effect of site for Myriophyllum
dfAs <- kl_icp[kl_icp$Element=="As",]
dfAs <- dfAs[dfAs$Measurement_type=="P",]

# All elements
dfAs <- dfAs[dfAs$Species=="MY",]
dfAl <- dfAl[dfAl$Species=="MY",]
dfV <- dfV[dfV$Species=="MY",]
dfCr <- dfCr[dfCr$Species=="MY",]
dfPb206 <- dfPb206[dfPb206$Species=="MY",]

# Change "Site" to character
dfAs <- dfAs %>%
  convert(chr(Site))

dfAl <- dfAl %>%
  convert(chr(Site))

dfV <- dfV %>%
  convert(chr(Site))

dfCr <- dfCr %>%
  convert(chr(Site))

dfPb206 <- dfPb206 %>%
  convert(chr(Site))


# Model establishment
(t_test_my_As <- t.test(Solid_conc~Site, data = dfAs))
(t_test_my_Al <- t.test(Solid_conc~Site, data = dfAl))
(t_test_my_V <- t.test(Solid_conc~Site, data = dfV))
(t_test_my_Cr <- t.test(Solid_conc~Site, data = dfCr))
(t_test_my_Pb206 <- t.test(Solid_conc~Site, data = dfPb206))

# Mean and sd testing
aggregate(dfAs$Solid_conc, by=list(dfAs$Site), FUN = sd)
aggregate(dfAl$Solid_conc, by=list(dfAl$Site), FUN = sd)
aggregate(dfV$Solid_conc, by=list(dfV$Site), FUN = sd)
aggregate(dfCr$Solid_conc, by=list(dfCr$Site), FUN = sd)
aggregate(dfPb206$Solid_conc, by=list(dfPb206$Site), FUN = sd)

# As
(my_site_As_boxplot <- ggplot(dfAs, aes(Site, Solid_conc, fill = Site)) +
    geom_boxplot() +
    scale_fill_manual(values = c("#FFFF66", "#339999")) +
    labs(title = expression(paste(bold("As")~"("*italic("M. spicatum)"))), 
         y = expression(Concentration~(μg~g^-1))) + 
    theme_classic() + 
    theme() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.position = "none"))

# Al
(my_site_Al_boxplot <- ggplot(dfAl, aes(Site, Solid_conc, fill = Site)) +
    geom_boxplot() +
    scale_fill_manual(values = c("#FFFF66", "#339999")) +
    labs(title = expression(paste(bold("Al")~"("*italic("M. spicatum)"))),  
         y = expression(Concentration~(μg~g^-1))) + 
    theme_classic() + 
    theme() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.position = "none"))

# V 
(my_site_V_boxplot <- ggplot(dfV, aes(Site, Solid_conc, fill = Site)) +
    geom_boxplot() +
    scale_fill_manual(values = c("#FFFF66", "#339999")) +
    labs(title = expression(paste(bold("V")~"("*italic("M. spicatum)"))), 
         y = expression(Concentration~(μg~g^-1))) + 
    theme_classic() + 
    theme() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.position = "none"))

# Cr
(my_site_Cr_boxplot <- ggplot(dfCr, aes(Site, Solid_conc, fill = Site)) +
    geom_boxplot() +
    scale_fill_manual(values = c("#FFFF66", "#339999")) +
    labs(title = expression(paste(bold("Cr")~"("*italic("M. spicatum)"))), 
         y = expression(Concentration~(μg~g^-1))) + 
    theme_classic() + 
    theme() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.position = "none"))

# Pb206
(my_site_Pb206_boxplot <- ggplot(dfPb206, aes(Site, Solid_conc, fill = Site)) +
    geom_boxplot() +
    scale_fill_manual(values = c("#FFFF66", "#339999")) +
    labs(title = expression(paste(bold(text = ""^206*"Pb")~"("*italic("M. spicatum)"))), 
         y = expression(Concentration~(μg~g^-1))) + 
    theme_classic() + 
    theme() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.position = "none"))

# Combine Chara and M. spicatum plots
combined_site_species <- ggarrange(chara_site_As_boxplot, chara_site_Al_boxplot, chara_site_V_boxplot, chara_site_Cr_boxplot, chara_site_Pb206_boxplot, my_site_As_boxplot, my_site_Al_boxplot, my_site_V_boxplot, my_site_Cr_boxplot, my_site_Pb206_boxplot,
                              ncol = 5, nrow = 2,
                              align = "v")
combined_site_species

# Leaves vs stems----
# Chara
# Filter data sets by Chara
dfAs <- dfAs[dfAs$Species=="CH",]
dfAl <- dfAl[dfAl$Species=="CH",]
dfV <- dfV[dfV$Species=="CH",]
dfCr <- dfCr[dfCr$Species=="CH",]
dfPb206 <- dfPb206[dfPb206$Species=="CH",]

# Change "Site" to character
dfAs <- dfAs %>%
  convert(chr(Site))

dfAl <- dfAl %>%
  convert(chr(Site))

dfV <- dfV %>%
  convert(chr(Site))

dfCr <- dfCr %>%
  convert(chr(Site))

dfPb206 <- dfPb206 %>%
  convert(chr(Site))


# T-test model
(t_test_chara_As_tissue <- t.test(Solid_conc~Tissue_type, data = dfAs)) 
(t_test_chara_Al_tissue <- t.test(Solid_conc~Tissue_type, data = dfAl)) 
(t_test_chara_V_tissue <- t.test(Solid_conc~Tissue_type, data = dfV)) 
(t_test_chara_Cr_tissue <- t.test(Solid_conc~Tissue_type, data = dfCr)) 
(t_test_chara_Pb206_tissue <- t.test(Solid_conc~Tissue_type, data = dfPb206)) 


# Mean and sd testing
aggregate(dfAs$Solid_conc, by=list(dfAs$Tissue_type), FUN = sd)
aggregate(dfAl$Solid_conc, by=list(dfAl$Tissue_type), FUN = sd)
aggregate(dfV$Solid_conc, by=list(dfV$Tissue_type), FUN = sd)
aggregate(dfCr$Solid_conc, by=list(dfCr$Tissue_type), FUN = sd)
aggregate(dfPb206$Solid_conc, by=list(dfPb206$Tissue_type), FUN = sd)

# Summary table
summary(t_test_chara)

# Visualize results 
# As
(chara_tissue_As_boxplot <- ggplot(dfAs, aes(Tissue_type, Solid_conc, fill = Tissue_type)) +
    geom_boxplot() +
    scale_fill_manual(values = c("#2ecc71", "#996633")) +
    labs(title = expression(paste(bold("As")~"("*italic("Chara")~paste("spp.)"))), 
         x = "",
         y = expression(Concentration~(μg~g^-1))) + 
    theme_classic() + 
    theme() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.position = "none"))

# Al
(chara_tissue_Al_boxplot <- ggplot(dfAl, aes(Tissue_type, Solid_conc, fill = Tissue_type)) +
    geom_boxplot() +
    scale_fill_manual(values = c("#2ecc71", "#996633")) +
    labs(title = expression(paste(bold("Al")~"("*italic("Chara")~paste("spp.)"))),
         x = "",
         y = expression(Concentration~(μg~g^-1))) + 
    theme_classic() + 
    theme() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.position = "none"))

# V 
(chara_tissue_V_boxplot <- ggplot(dfV, aes(Tissue_type, Solid_conc, fill = Tissue_type)) +
    geom_boxplot() +
    scale_fill_manual(values = c("#2ecc71", "#996633")) +
    labs(title = expression(paste(bold("V")~"("*italic("Chara")~paste("spp.)"))), 
         x = "",
         y = expression(Concentration~(μg~g^-1))) + 
    theme_classic() + 
    theme() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.position = "none"))

# Cr
(chara_tissue_Cr_boxplot <- ggplot(dfCr, aes(Tissue_type, Solid_conc, fill = Tissue_type)) +
    geom_boxplot() +
    scale_fill_manual(values = c("#2ecc71", "#996633")) +
    labs(title = expression(paste(bold("Cr")~"("*italic("Chara")~paste("spp.)"))), 
         x = "",
         y = expression(Concentration~(μg~g^-1))) + 
    theme_classic() + 
    theme() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.position = "none"))

# Pb206
(chara_tissue_Pb206_boxplot <- ggplot(dfPb206, aes(Tissue_type, Solid_conc, fill = Tissue_type)) +
    geom_boxplot() +
    scale_fill_manual(values = c("#2ecc71", "#996633")) +
    labs(title = expression(paste(bold(text = ""^206*"Pb")~"("*italic("Chara")~paste("spp.)"))), 
         x = "",
         y = expression(Concentration~(μg~g^-1))) + 
    theme_classic() + 
    theme() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.position = "none"))

# Combine graphs
combined_tissue_type <- ggarrange(chara_tissue_As_boxplot, chara_tissue_Al_boxplot, chara_tissue_V_boxplot, chara_tissue_Cr_boxplot, chara_tissue_Pb206_boxplot,
                                  ncol = 5, nrow = 1,
                                  align = "v")
combined_tissue_type

# M. spicatum
# Filter data sets by MY
dfAs <- dfAs[dfAs$Species=="MY",]
dfAl <- dfAl[dfAl$Species=="MY",]
dfV <- dfV[dfV$Species=="MY",]
dfCr <- dfCr[dfCr$Species=="MY",]
dfPb206 <- dfPb206[dfPb206$Species=="MY",]

# Change "Site" to character
dfAs <- dfAs %>%
  convert(chr(Site))

dfAl <- dfAl %>%
  convert(chr(Site))

dfV <- dfV %>%
  convert(chr(Site))

dfCr <- dfCr %>%
  convert(chr(Site))

dfPb206 <- dfPb206 %>%
  convert(chr(Site))


# T-test model
(t_test_chara_As_tissue2 <- t.test(Solid_conc~Tissue_type, data = dfAs)) 
(t_test_chara_Al_tissue2 <- t.test(Solid_conc~Tissue_type, data = dfAl)) 
(t_test_chara_V_tissue2 <- t.test(Solid_conc~Tissue_type, data = dfV)) 
(t_test_chara_Cr_tissue2 <- t.test(Solid_conc~Tissue_type, data = dfCr)) 
(t_test_chara_Pb206_tissue2 <- t.test(Solid_conc~Tissue_type, data = dfPb206)) 


# Mean and sd testing
aggregate(dfAs$Solid_conc, by=list(dfAs$Tissue_type), FUN = sd)
aggregate(dfAl$Solid_conc, by=list(dfAl$Tissue_type), FUN = sd)
aggregate(dfV$Solid_conc, by=list(dfV$Tissue_type), FUN = sd)
aggregate(dfCr$Solid_conc, by=list(dfCr$Tissue_type), FUN = sd)
aggregate(dfPb206$Solid_conc, by=list(dfPb206$Tissue_type), FUN = sd)

# Summary table
summary(t_test_chara)

# Visualize results 
# As
(chara_tissue_As_boxplot2 <- ggplot(dfAs, aes(Tissue_type, Solid_conc, fill = Tissue_type)) +
    geom_boxplot() +
    scale_fill_manual(values = c("#2ecc71", "#996633")) +
    labs(title = expression(paste(bold("As")~"("*italic("M. spicatum)"))), 
         x = "",
         y = expression(Concentration~(μg~g^-1))) + 
    theme_classic() + 
    theme() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.position = "none"))

# Al
(chara_tissue_Al_boxplot2 <- ggplot(dfAl, aes(Tissue_type, Solid_conc, fill = Tissue_type)) +
    geom_boxplot() +
    scale_fill_manual(values = c("#2ecc71", "#996633")) +
    labs(title = expression(paste(bold("Al")~"("*italic("M. spicatum)"))),
         x = "",
         y = expression(Concentration~(μg~g^-1))) + 
    theme_classic() + 
    theme() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.position = "none"))

# V 
(chara_tissue_V_boxplot2 <- ggplot(dfV, aes(Tissue_type, Solid_conc, fill = Tissue_type)) +
    geom_boxplot() +
    scale_fill_manual(values = c("#2ecc71", "#996633")) +
    labs(title = expression(paste(bold("V")~"("*italic("M. spicatum)"))), 
         x = "",
         y = expression(Concentration~(μg~g^-1))) + 
    theme_classic() + 
    theme() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.position = "none"))

# Cr
(chara_tissue_Cr_boxplot2 <- ggplot(dfCr, aes(Tissue_type, Solid_conc, fill = Tissue_type)) +
    geom_boxplot() +
    scale_fill_manual(values = c("#2ecc71", "#996633")) +
    labs(title = expression(paste(bold("Cr")~"("*italic("M. spicatum)"))), 
         x = "",
         y = expression(Concentration~(μg~g^-1))) + 
    theme_classic() + 
    theme() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.position = "none"))

# Pb206
(chara_tissue_Pb206_boxplot2 <- ggplot(dfPb206, aes(Tissue_type, Solid_conc, fill = Tissue_type)) +
    geom_boxplot() +
    scale_fill_manual(values = c("#2ecc71", "#996633")) +
    labs(title = expression(paste(bold(text = ""^206*"Pb")~"("*italic("M. spicatum)"))), 
         x = "",
         y = expression(Concentration~(μg~g^-1))) + 
    theme_classic() + 
    theme() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.position = "none"))

# Combine graphs
combined_tissue_type <- ggarrange(chara_tissue_As_boxplot, chara_tissue_Al_boxplot, chara_tissue_V_boxplot, chara_tissue_Cr_boxplot, chara_tissue_Pb206_boxplot,
                                  ncol = 5, nrow = 1,
                                  align = "v")
combined_tissue_type

# P. pectinatus
# Filter data sets by PO
dfAs <- dfAs[dfAs$Species=="PO",]
dfAl <- dfAl[dfAl$Species=="PO",]
dfV <- dfV[dfV$Species=="PO",]
dfCr <- dfCr[dfCr$Species=="PO",]
dfPb206 <- dfPb206[dfPb206$Species=="PO",]

# Change "Site" to character
dfAs <- dfAs %>%
  convert(chr(Site))

dfAl <- dfAl %>%
  convert(chr(Site))

dfV <- dfV %>%
  convert(chr(Site))

dfCr <- dfCr %>%
  convert(chr(Site))

dfPb206 <- dfPb206 %>%
  convert(chr(Site))


# T-test model
(t_test_chara_As_tissue3 <- t.test(Solid_conc~Tissue_type, data = dfAs)) 
(t_test_chara_Al_tissue3 <- t.test(Solid_conc~Tissue_type, data = dfAl)) 
(t_test_chara_V_tissue3 <- t.test(Solid_conc~Tissue_type, data = dfV)) 
(t_test_chara_Cr_tissue3 <- t.test(Solid_conc~Tissue_type, data = dfCr)) 
(t_test_chara_Pb206_tissue3 <- t.test(Solid_conc~Tissue_type, data = dfPb206)) 


# Mean and sd testing
aggregate(dfAs$Solid_conc, by=list(dfAs$Tissue_type), FUN = sd)
aggregate(dfAl$Solid_conc, by=list(dfAl$Tissue_type), FUN = sd)
aggregate(dfV$Solid_conc, by=list(dfV$Tissue_type), FUN = sd)
aggregate(dfCr$Solid_conc, by=list(dfCr$Tissue_type), FUN = sd)
aggregate(dfPb206$Solid_conc, by=list(dfPb206$Tissue_type), FUN = sd)

# Summary table
summary(t_test_chara)

# Visualize results 
# As
(chara_tissue_As_boxplot3 <- ggplot(dfAs, aes(Tissue_type, Solid_conc, fill = Tissue_type)) +
    geom_boxplot() +
    scale_fill_manual(values = c("#2ecc71", "#996633")) +
    labs(title = expression(paste(bold("As")~"("*italic("P. pectinatus)"))), 
         x = "",
         y = expression(Concentration~(μg~g^-1))) + 
    theme_classic() + 
    theme() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.position = "none"))

# Al
(chara_tissue_Al_boxplot3 <- ggplot(dfAl, aes(Tissue_type, Solid_conc, fill = Tissue_type)) +
    geom_boxplot() +
    scale_fill_manual(values = c("#2ecc71", "#996633")) +
    labs(title = expression(paste(bold("Al")~"("*italic("P. pectinatus)"))),
         x = "",
         y = expression(Concentration~(μg~g^-1))) + 
    theme_classic() + 
    theme() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.position = "none"))

# V 
(chara_tissue_V_boxplot3 <- ggplot(dfV, aes(Tissue_type, Solid_conc, fill = Tissue_type)) +
    geom_boxplot() +
    scale_fill_manual(values = c("#2ecc71", "#996633")) +
    labs(title = expression(paste(bold("V")~"("*italic("P. pectinatus)"))), 
         x = "",
         y = expression(Concentration~(μg~g^-1))) + 
    theme_classic() + 
    theme() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.position = "none"))

# Cr
(chara_tissue_Cr_boxplot3 <- ggplot(dfCr, aes(Tissue_type, Solid_conc, fill = Tissue_type)) +
    geom_boxplot() +
    scale_fill_manual(values = c("#2ecc71", "#996633")) +
    labs(title = expression(paste(bold("Cr")~"("*italic("P. pectinatus)"))), 
         x = "",
         y = expression(Concentration~(μg~g^-1))) + 
    theme_classic() + 
    theme() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.position = "none"))

# Pb206
(chara_tissue_Pb206_boxplot3 <- ggplot(dfPb206, aes(Tissue_type, Solid_conc, fill = Tissue_type)) +
    geom_boxplot() +
    scale_fill_manual(values = c("#2ecc71", "#996633")) +
    labs(title = expression(paste(bold(text = ""^206*"Pb")~"("*italic("P. pectinatus)"))), 
         x = "",
         y = expression(Concentration~(μg~g^-1))) + 
    theme_classic() + 
    theme() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.position = "none"))

# Combine graphs
combined_tissue_type <- ggarrange(chara_tissue_As_boxplot, chara_tissue_Al_boxplot, chara_tissue_V_boxplot, chara_tissue_Cr_boxplot, chara_tissue_Pb206_boxplot, chara_tissue_As_boxplot2, chara_tissue_Al_boxplot2, chara_tissue_V_boxplot2, chara_tissue_Cr_boxplot2, chara_tissue_Pb206_boxplot2, chara_tissue_As_boxplot3, chara_tissue_Al_boxplot3, chara_tissue_V_boxplot3, chara_tissue_Cr_boxplot3, chara_tissue_Pb206_boxplot3,
                                  ncol = 5, nrow = 3,
                                  align = "v")
combined_tissue_type


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

# Summary table
summary(As_species)

# Tukey test
tukey_as <- aov(Solid_conc ~ Species, data = dfAs)
TukeyHSD(tukey_as)

# Boxplot visualization
(As_species_boxplot <- ggplot(dfAs, aes(Species, Solid_conc, fill = Species)) +
    geom_boxplot() +
    scale_fill_manual(values = c("#e74c3c", "#2ecc71", "#0099f8")) +
    scale_x_discrete(labels=c(expression(italic('Chara spp')), expression(italic('M. spicatum')), expression(italic('P. pectinatus')))) +
    labs(title = "As", 
         y = expression(Concentration~(μg~g^-1)), x = "Species") + 
    theme_classic() + 
    theme() + 
    theme(panel.grid = element_blank(), 
          axis.text.x = element_text(size = 10), 
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

# Summary table
summary(al_species)

# Basic visualization
(Al_species_boxplot <- ggplot(dfAl, aes(Species, Solid_conc, fill = Species)) +
    geom_boxplot() +
    scale_fill_manual(values = c("#e74c3c", "#2ecc71", "#0099f8")) +
    scale_x_discrete(labels=c(expression(italic('Chara spp')), expression(italic('M. spicatum')), expression(italic('P. pectinatus')))) +
    labs(title = "Al", 
         y = expression(Concentration~(μg~g^-1)), x = "Species") + 
    theme_classic() + 
    theme() + 
    theme(panel.grid = element_blank(), 
          axis.text.x = element_text(size = 10), 
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

# Summary table
summary(V_species)

# Basic visualization
(V_species_boxplot <- ggplot(dfV, aes(Species, Solid_conc, fill = Species)) +
    geom_boxplot() +
    scale_fill_manual(values = c("#e74c3c", "#2ecc71", "#0099f8")) +
    scale_x_discrete(labels=c(expression(italic('Chara spp')), expression(italic('M. spicatum')), expression(italic('P. pectinatus')))) +
    labs(title = "V", 
         y = expression(Concentration~(μg~g^-1)), x = "Species") + 
    theme_classic() + 
    theme() + 
    theme(panel.grid = element_blank(), 
          axis.text.x = element_text(size = 10), 
          axis.title.x = element_blank(),
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.position = "none"))

# Mean and sd
aggregate(dfV$Solid_conc, by=list(dfV$Species), FUN = mean)
aggregate(dfV$Solid_conc, by=list(dfV$Species), FUN = sd)

# Tukey test
tukey_V <- aov(Solid_conc ~ Species, data = dfV)
TukeyHSD(tukey_V)

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

# Summary table
summary(Cr_species)

# Basic visualization
(Cr_species_boxplot <- ggplot(dfCr, aes(Species, Solid_conc, fill = Species)) +
    geom_boxplot() +
    scale_fill_manual(values = c("#e74c3c", "#2ecc71", "#0099f8")) +
    scale_x_discrete(labels=c(expression(italic('Chara spp')), expression(italic('M. spicatum')), expression(italic('P. pectinatus')))) +
    labs(title = "Cr", 
         y = expression(Concentration~(μg~g^-1)), x = "Species") + 
    theme_classic() + 
    theme() + 
    theme(panel.grid = element_blank(), 
          axis.text.x = element_text(size = 10), 
          axis.title.x = element_blank(),
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.position = "none"))

# Tukey test
tukey_Cr <- aov(Solid_conc ~ Species, data = dfCr)
TukeyHSD(tukey_Cr)

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

# Summary table
summary(Pb206_species)

# Basic visualization
(Pb206_species_boxplot <- ggplot(dfPb206, aes(Species, Solid_conc, fill = Species)) +
    geom_boxplot() +
    scale_fill_manual(values = c("#e74c3c", "#2ecc71", "#0099f8")) +
    scale_x_discrete(labels=c(expression(italic('Chara spp')), expression(italic('M. spicatum')), expression(italic('P. pectinatus')))) +
    labs(title = expression(text = ""^206*"Pb"), 
         y = expression(Concentration~(μg~g^-1)), x = "Species") + 
    theme_classic() + 
    theme() + 
    theme(panel.grid = element_blank(), 
          axis.text.x = element_text(size = 10), 
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

# Summary table
summary(Pb207_species)

# Basic visualization
(Pb207_species_boxplot <- ggplot(dfPb207, aes(Species, Solid_conc, fill = Species)) +
    geom_boxplot() +
    scale_fill_manual(values = c("#e74c3c", "#2ecc71", "#0099f8")) +
    scale_x_discrete(labels=c(expression(italic('Chara spp')), expression(italic('M. spicatum')), expression(italic('P. pectinatus')))) +
    labs(title = expression(text = ""^207*"Pb"), 
         y = expression(Concentration~(μg~g^-1)), x = "Species") + 
    theme_classic() + 
    theme() + 
    theme(panel.grid = element_blank(), 
          axis.text.x = element_text(size = 10), 
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

# Summary table 
summary(Pb208_species)

# Basic visualization
(Pb208_species_boxplot <- ggplot(dfPb208, aes(Species, Solid_conc, fill = Species)) +
    geom_boxplot() +
    scale_fill_manual(values = c("#e74c3c", "#2ecc71", "#0099f8")) +
    scale_x_discrete(labels=c(expression(italic('Chara spp')), expression(italic('M. spicatum')), expression(italic('P. pectinatus')))) +
    labs(title = expression(text = ""^208*"Pb"), 
         y = expression(Concentration~(μg~g^-1)), x = "Species") + 
    theme_classic() + 
    theme() + 
    theme(panel.grid = element_blank(), 
          axis.text.x = element_text(size = 10), 
          axis.title.x = element_blank(),
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.position = "none"))

# Combine species plots
combined_species <- ggarrange(As_species_boxplot, Al_species_boxplot, V_species_boxplot, Cr_species_boxplot, Pb206_species_boxplot, Pb207_species_boxplot, Pb208_species_boxplot,
                               ncol = 4, nrow = 2,
                              align = "v")
combined_species


# Porewater analysis----
# Create new data set with only As Porewater samples
# Dont drop NA for kl_icp
dfAs <- kl_icp[kl_icp$Element=="As",]
dfAs <- dfAs[dfAs$Measurement_type=="PW",]
dfAs <- dfAs[dfAs$Site=="2",]

# Round Solid_conc to 2 decimal places
dfAs$Conc <- round(dfAs$Conc, digits = 2)

# Create barplot
(As_PW_barplot_S2 <- ggplot(data=dfAs, aes(x=Depth, y=Conc)) +
  geom_bar(stat="identity", fill = "steelblue", color = "black") +
    scale_x_reverse(breaks=c(0,2,4,6,8,10,12,14), expand = c(0,0), labels = c("0 - 2", "2 - 4", "4 - 6", "6 - 8", "8 - 10", "10 - 12", "12 - 14", "14 - 16")) +
    scale_y_continuous(expand = c(0,0)) +
    geom_text(aes(label=Conc), color = "white", position=position_stack(vjust=0.5), size=4, fontface = "bold") +
    coord_flip() +
    labs(title = "Site 2 (As)", x = "Depth (cm)", y = expression(Concentration~(μg~L^-1))) +
    theme_classic() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 13),
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
))

# Other elements
# Al
dfAl <- kl_icp[kl_icp$Element=="Al",]
dfAl <- dfAl[dfAl$Measurement_type=="PW",]
dfAl <- dfAl[dfAl$Site=="2",]

# Round Solid_conc to 2 decimal places
dfAl$Conc <- round(dfAl$Conc, digits = 2)

# Create barplot Al
(Al_PW_barplot_S2 <- ggplot(data=dfAl, aes(x=Depth, y=Conc)) +
    geom_bar(stat="identity", fill = "steelblue", color = "black") +
    scale_x_reverse(breaks=c(0,2,4,6,8,10,12,14), expand = c(0,0), labels = c("0 - 2", "2 - 4", "4 - 6", "6 - 8", "8 - 10", "10 - 12", "12 - 14", "14 - 16")) +
    scale_y_continuous(expand = c(0,0)) +
    geom_text(aes(label=Conc), color = "white", position=position_stack(vjust=0.5), size=4, fontface = "bold") +
    coord_flip() +
    labs(title = "Site 2 (Al)", x = "Depth (cm)", y = expression(Concentration~(μg~L^-1))) +
    theme_classic() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 13),
          plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    ))

# V 
dfV <- kl_icp[kl_icp$Element=="V",]
dfV <- dfV[dfV$Measurement_type=="PW",]
dfV <- dfV[dfV$Site=="2",]

# Round Solid_conc to 2 decimal places
dfV$Conc <- round(dfV$Conc, digits = 2)

# Create barplot
(V_PW_barplot_S2 <- ggplot(data=dfV, aes(x=Depth, y=Conc)) +
    geom_bar(stat="identity", fill = "steelblue", color = "black") +
    scale_x_reverse(breaks=c(0,2,4,6,8,10,12,14), expand = c(0,0), labels = c("0 - 2", "2 - 4", "4 - 6", "6 - 8", "8 - 10", "10 - 12", "12 - 14", "14 - 16")) +
    scale_y_continuous(expand = c(0,0)) +
    coord_flip() +
    labs(title = "Site 2 (V)", x = "Depth (cm)", y = expression(Concentration~(μg~L^-1))) +
    theme_classic() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 13),
          plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    ))

# Cr
dfCr <- kl_icp[kl_icp$Element=="Cr",]
dfCr <- dfCr[dfCr$Measurement_type=="PW",]
dfCr <- dfCr[dfCr$Site=="2",]

# Round Solid_conc to 2 decimal places
dfCr$Conc <- round(dfCr$Conc, digits = 2)

# Create barplot
(Cr_PW_barplot_S2 <- ggplot(data=dfCr, aes(x=Depth, y=Conc)) +
    geom_bar(stat="identity", fill = "steelblue", color = "black") +
    scale_x_reverse(breaks=c(0,2,4,6,8,10,12,14), expand = c(0,0), labels = c("0 - 2", "2 - 4", "4 - 6", "6 - 8", "8 - 10", "10 - 12", "12 - 14", "14 - 16")) +
    scale_y_continuous(expand = c(0,0)) +
    geom_text(aes(label=Conc), color = "white", position=position_stack(vjust=0.5), size=4, fontface = "bold") +
    coord_flip() +
    labs(title = "Site 2 (Cr)", x = "Depth (cm)", y = expression(Concentration~(μg~L^-1))) +
    theme_classic() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 13),
          plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    ))

# Pb206
dfPb206 <- kl_icp[kl_icp$Element=="Pb206",]
dfPb206 <- dfPb206[dfPb206$Measurement_type=="PW",]
dfPb206 <- dfPb206[dfPb206$Site=="2",]

# Round Solid_conc to 2 decimal places
dfPb206$Conc <- round(dfPb206$Conc, digits = 2)

# Create barplot
(Pb206_PW_barplot_S2 <- ggplot(data=dfPb206, aes(x=Depth, y=Conc)) +
    geom_bar(stat="identity", fill = "steelblue", color = "black") +
    scale_x_reverse(breaks=c(0,2,4,6,8,10,12,14), expand = c(0,0), labels = c("0 - 2", "2 - 4", "4 - 6", "6 - 8", "8 - 10", "10 - 12", "12 - 14", "14 - 16")) +
    scale_y_continuous(expand = c(0,0)) +
    geom_text(aes(label=Conc), color = "white", position=position_stack(vjust=0.5), size=4, fontface = "bold") +
    coord_flip() +
    labs(title = expression(bold(text = "Site 2"~" ("^206*"Pb)")), x = "Depth (cm)", y = expression(Concentration~(μg~L^-1))) +
    theme_classic() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 13),
          plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    ))

# Site 3
# As
dfAs <- kl_icp[kl_icp$Element=="As",]
dfAs <- dfAs[dfAs$Measurement_type=="PW",]
dfAs <- dfAs[dfAs$Site=="3",]

# Round Solid_conc to 2 decimal places
dfAs$Conc <- round(dfAs$Conc, digits = 2)

# Create barplot
(As_PW_barplot_S3 <- ggplot(data=dfAs, aes(x=Depth, y=Conc)) +
    geom_bar(stat="identity", fill = "steelblue", color = "black") +
    scale_x_reverse(breaks=c(0,2,4,6,8), expand = c(0,0), labels = c("0 - 2", "2 - 4", "4 - 6", "6 - 8", "8 - 10")) +
    scale_y_continuous(expand = c(0,0)) +
    geom_text(aes(label=Conc), color = "white", position=position_stack(vjust=0.5), size=5, fontface = "bold") +
    coord_flip() +
    labs(title = "Site 3 (As)", x = "Depth (cm)", y = expression(Concentration~(μg~L^-1))) +
    theme_classic() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 13),
          plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    ))

# Al
dfAl <- kl_icp[kl_icp$Element=="Al",]
dfAl <- dfAl[dfAl$Measurement_type=="PW",]
dfAl <- dfAl[dfAl$Site=="3",]

# Round Solid_conc to 2 decimal places
dfAl$Conc <- round(dfAl$Conc, digits = 2)

# Create barplot
(Al_PW_barplot_S3 <- ggplot(data=dfAl, aes(x=Depth, y=Conc)) +
    geom_bar(stat="identity", fill = "steelblue", color = "black") +
    scale_x_reverse(breaks=c(0,2,4,6,8), expand = c(0,0), labels = c("0 - 2", "2 - 4", "4 - 6", "6 - 8", "8 - 10")) +
    scale_y_continuous(expand = c(0,0)) +
    geom_text(aes(label=Conc), color = "white", position=position_stack(vjust=0.5), size=5, fontface = "bold") +
    coord_flip() +
    labs(title = "Site 3 (Al)", x = "Depth (cm)", y = expression(Concentration~(μg~L^-1))) +
    theme_classic() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 13),
          plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    ))

# V
dfV <- kl_icp[kl_icp$Element=="V",]
dfV <- dfV[dfV$Measurement_type=="PW",]
dfV <- dfV[dfV$Site=="3",]

# Round Solid_conc to 2 decimal places
dfV$Conc <- round(dfV$Conc, digits = 2)

# Create barplot
(V_PW_barplot_S3 <- ggplot(data=dfV, aes(x=Depth, y=Conc)) +
    geom_bar(stat="identity", fill = "steelblue", color = "black") +
    scale_x_reverse(breaks=c(0,2,4,6,8), expand = c(0,0), labels = c("0 - 2", "2 - 4", "4 - 6", "6 - 8", "8 - 10")) +
    scale_y_continuous(expand = c(0,0)) +
    geom_text(aes(label=Conc), color = "white", position=position_stack(vjust=0.5), size=5, fontface = "bold") +
    coord_flip() +
    labs(title = "Site 3 (V)", x = "Depth (cm)", y = expression(Concentration~(μg~L^-1))) +
    theme_classic() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 13),
          plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    ))

# Cr
dfCr <- kl_icp[kl_icp$Element=="Cr",]
dfCr <- dfCr[dfCr$Measurement_type=="PW",]
dfCr <- dfCr[dfCr$Site=="3",]

# Round Solid_conc to 2 decimal places
dfCr$Conc <- round(dfCr$Conc, digits = 2)

# Create barplot
(Cr_PW_barplot_S3 <- ggplot(data=dfCr, aes(x=Depth, y=Conc)) +
    geom_bar(stat="identity", fill = "steelblue", color = "black") +
    scale_x_reverse(breaks=c(0,2,4,6,8), expand = c(0,0), labels = c("0 - 2", "2 - 4", "4 - 6", "6 - 8", "8 - 10")) +
    scale_y_continuous(expand = c(0,0)) +
    geom_text(aes(label=Conc), color = "white", position=position_stack(vjust=0.5), size=5, fontface = "bold") +
    coord_flip() +
    labs(title = "Site 3 (Cr)", x = "Depth (cm)", y = expression(Concentration~(μg~L^-1))) +
    theme_classic() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 13),
          plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    ))

# Pb206
dfPb206 <- kl_icp[kl_icp$Element=="Pb206",]
dfPb206 <- dfPb206[dfPb206$Measurement_type=="PW",]
dfPb206 <- dfPb206[dfPb206$Site=="3",]

# Round Solid_conc to 2 decimal places
dfPb206$Conc <- round(dfPb206$Conc, digits = 2)

# Create barplot
(Pb206_PW_barplot_S3 <- ggplot(data=dfPb206, aes(x=Depth, y=Conc)) +
    geom_bar(stat="identity", fill = "steelblue", color = "black") +
    scale_x_reverse(breaks=c(0,2,4,6,8), expand = c(0,0), labels = c("0 - 2", "2 - 4", "4 - 6", "6 - 8", "8 - 10")) +
    scale_y_continuous(expand = c(0,0)) +
    geom_text(aes(label=Conc), color = "white", position=position_stack(vjust=0.5), size=5, fontface = "bold") +
    coord_flip() +
    labs(title = expression(bold(text = "Site 2"~" ("^206*"Pb)")), y = expression(Concentration~(μg~L^-1))) +
    theme_classic() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 13),
          plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    ))

# Combine all barplots from Site 2
combined_barplots_S2_PW <- ggarrange(As_PW_barplot_S2, Al_PW_barplot_S2, V_PW_barplot_S2, Cr_PW_barplot_S2, Pb206_PW_barplot_S2, 
                                     ncol = 3, nrow = 2,
                                  align = "v")
combined_barplots_S2_PW

# Combine all barplots from Site 3
combined_barplots_S3_PW <- ggarrange(As_PW_barplot_S3, Al_PW_barplot_S3, V_PW_barplot_S3, Cr_PW_barplot_S3, Pb206_PW_barplot_S3,
                                     ncol = 3, nrow = 2,
                                     align = "v")

combined_barplots_S3_PW


# Surface water analysis----
# As
dfAs <- kl_icp[kl_icp$Element=="As",]
dfAs <- dfAs[dfAs$Measurement_type=="SW",]


# Justyna's research comparison----
# Compare Chara 
# As
dfAs <- kl_icp[kl_icp$Element=="As",]
dfAs <- dfAs[dfAs$Measurement_type=="P",]
dfAs <- dfAs[which(dfAs$Species=="CH"),]

# Run t-test to compare change over time
(t_test_chara_As_time <- t.test(Solid_conc~Data_collector, data = dfAs)) 

# SD 
aggregate(dfAs$Solid_conc, by=list(dfAs$Data_collector), FUN = sd)

# V
dfV <- kl_icp[kl_icp$Element=="V",]
dfV <- dfV[dfV$Measurement_type=="P",]
dfV <- dfV[which(dfV$Species=="CH"),]

# Run t-test to compare change over time
(t_test_chara_V_time <- t.test(Solid_conc~Data_collector, data = dfV)) 

# SD 
aggregate(dfV$Solid_conc, by=list(dfV$Data_collector), FUN = sd)

# Cr
dfCr <- kl_icp[kl_icp$Element=="Cr",]
dfCr <- dfCr[dfCr$Measurement_type=="P",]
dfCr <- dfCr[which(dfCr$Species=="CH"),]

# Run t-test to compare change over time
(t_test_chara_Cr_time <- t.test(Solid_conc~Data_collector, data = dfCr)) 

# SD 
aggregate(dfCr$Solid_conc, by=list(dfCr$Data_collector), FUN = sd)


# Compare MY
dfAs <- kl_icp[kl_icp$Element=="As",]
dfAs <- dfAs[dfAs$Measurement_type=="P",]
dfAs <- dfAs[which(dfAs$Species=="MY"),]

# Run t-test to compare change over time
(t_test_chara_As_time <- t.test(Solid_conc~Data_collector, data = dfAs)) 

# SD 
aggregate(dfAs$Solid_conc, by=list(dfAs$Data_collector), FUN = sd)

# V
dfV <- kl_icp[kl_icp$Element=="V",]
dfV <- dfV[dfV$Measurement_type=="P",]
dfV <- dfV[which(dfV$Species=="MY"),]

# Run t-test to compare change over time
(t_test_chara_V_time <- t.test(Solid_conc~Data_collector, data = dfV)) 

# SD 
aggregate(dfV$Solid_conc, by=list(dfV$Data_collector), FUN = sd)

# Cr
dfCr <- kl_icp[kl_icp$Element=="Cr",]
dfCr <- dfCr[dfCr$Measurement_type=="P",]
dfCr <- dfCr[which(dfCr$Species=="MY"),]

# Run t-test to compare change over time
(t_test_chara_Cr_time <- t.test(Solid_conc~Data_collector, data = dfCr)) 

# SD 
aggregate(dfCr$Solid_conc, by=list(dfCr$Data_collector), FUN = sd)


# Compare PO
# As
dfAs <- kl_icp[kl_icp$Element=="As",]
dfAs <- dfAs[dfAs$Measurement_type=="P",]
dfAs <- dfAs[which(dfAs$Species=="PO"),]

# Run t-test to compare change over time
(t_test_chara_As_time <- t.test(Solid_conc~Data_collector, data = dfAs)) 

# SD 
aggregate(dfAs$Solid_conc, by=list(dfAs$Data_collector), FUN = sd)

# V
dfV <- kl_icp[kl_icp$Element=="V",]
dfV <- dfV[dfV$Measurement_type=="P",]
dfV <- dfV[which(dfV$Species=="PO"),]

# Run t-test to compare change over time
(t_test_chara_V_time <- t.test(Solid_conc~Data_collector, data = dfV)) 

# SD 
aggregate(dfV$Solid_conc, by=list(dfV$Data_collector), FUN = sd)

# Cr
dfCr <- kl_icp[kl_icp$Element=="Cr",]
dfCr <- dfCr[dfCr$Measurement_type=="P",]
dfCr <- dfCr[which(dfCr$Species=="PO"),]

# Run t-test to compare change over time
(t_test_chara_Cr_time <- t.test(Solid_conc~Data_collector, data = dfCr)) 

# SD 
aggregate(dfCr$Solid_conc, by=list(dfCr$Data_collector), FUN = sd)


# Sediment analysis----
# Site 2
# As
dfAs <- kl_icp[kl_icp$Element=="As",]
dfAs <- dfAs %>% filter (Measurement_type %in% c("S","PW"))
dfAs <- dfAs[dfAs$Site=="2",]
dfAs <- dfAs %>%
  pivot_wider(names_from = Measurement_type, values_from = Solid_conc)
dfAs <- dfAs %>%
  pivot_wider(names_from = Depth, values_from = PW)


# Round Solid_conc to 2 decimal places
dfAs$Solid_conc <- round(dfAs$Solid_conc, digits = 2)

# Create barplot
(As_S_barplot_S2 <- ggplot(data=dfAs, aes(x=Depth, y=S)) +
    geom_bar(stat="identity", fill = "#8B4513", color = "black") +
    geom_line(aes(x = Depth, y = PW)) +
    scale_x_reverse(breaks=c(0,2,4,6,8,10,12,14), expand = c(0,0), labels = c("0 - 2", "2 - 4", "4 - 6", "6 - 8", "8 - 10", "10 - 12", "12 - 14", "14 - 16")) +
    coord_flip() +
    labs(title = "Site 2 (As)", x = "Depth (cm)", y = expression(Concentration~(μg~g^-1))) +
    theme_classic() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 13),
          plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    ))

# Al
dfAl <- kl_icp[kl_icp$Element=="Al",]
dfAl <- dfAl[dfAl$Measurement_type=="S",]
dfAl <- dfAl[dfAl$Site=="2",]

# Round Solid_conc to 2 decimal places
dfAl$Solid_conc <- round(dfAl$Solid_conc, digits = 2)

# Create barplot
(Al_S_barplot_S2 <- ggplot(data=dfAl, aes(x=Depth, y=Solid_conc)) +
    geom_bar(stat="identity", fill = "#8B4513", color = "black") +
    scale_x_reverse(breaks=c(0,2,4,6,8,10,12,14), expand = c(0,0), labels = c("0 - 2", "2 - 4", "4 - 6", "6 - 8", "8 - 10", "10 - 12", "12 - 14", "14 - 16")) +
    scale_y_continuous(expand = c(0,0)) +
    geom_text(aes(label=Solid_conc), color = "white", position=position_stack(vjust=0.5), size=4, fontface = "bold") +
    coord_flip() +
    labs(title = "Site 2 (Al)", x = "Depth (cm)", y = expression(Concentration~(μg~g^-1))) +
    theme_classic() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 13),
          plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    ))

# V
dfV <- kl_icp[kl_icp$Element=="V",]
dfV <- dfV[dfV$Measurement_type=="S",]
dfV <- dfV[dfV$Site=="2",]

# Round Solid_conc to 2 decimal places
dfV$Solid_conc <- round(dfV$Solid_conc, digits = 2)

# Create barplot
(V_S_barplot_S2 <- ggplot(data=dfV, aes(x=Depth, y=Solid_conc)) +
    geom_bar(stat="identity", fill = "#8B4513", color = "black") +
    scale_x_reverse(breaks=c(0,2,4,6,8,10,12,14), expand = c(0,0), labels = c("0 - 2", "2 - 4", "4 - 6", "6 - 8", "8 - 10", "10 - 12", "12 - 14", "14 - 16")) +
    scale_y_continuous(expand = c(0,0)) +
    geom_text(aes(label=Solid_conc), color = "white", position=position_stack(vjust=0.5), size=4, fontface = "bold") +
    coord_flip() +
    labs(title = "Site 2 (V)", x = "Depth (cm)", y = expression(Concentration~(μg~g^-1))) +
    theme_classic() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 13),
          plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    ))

# Cr
dfCr <- kl_icp[kl_icp$Element=="Cr",]
dfCr <- dfCr[dfCr$Measurement_type=="S",]
dfCr <- dfCr[dfCr$Site=="2",]

# Round Solid_conc to 2 decimal places
dfCr$Solid_conc <- round(dfCr$Solid_conc, digits = 2)

# Create barplot
(Cr_S_barplot_S2 <- ggplot(data=dfCr, aes(x=Depth, y=Solid_conc)) +
    geom_bar(stat="identity", fill = "#8B4513", color = "black") +
    scale_x_reverse(breaks=c(0,2,4,6,8,10,12,14), expand = c(0,0), labels = c("0 - 2", "2 - 4", "4 - 6", "6 - 8", "8 - 10", "10 - 12", "12 - 14", "14 - 16")) +
    scale_y_continuous(expand = c(0,0)) +
    geom_text(aes(label=Solid_conc), color = "white", position=position_stack(vjust=0.5), size=4, fontface = "bold") +
    coord_flip() +
    labs(title = "Site 2 (Cr)", x = "Depth (cm)", y = expression(Concentration~(μg~g^-1))) +
    theme_classic() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 13),
          plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    ))

# Pb
dfPb <- kl_icp[kl_icp$Element=="Pb",]
dfPb <- dfPb[dfPb$Measurement_type=="S",]
dfPb <- dfPb[dfPb$Site=="2",]

# Round Solid_conc to 2 decimal places
dfPb$Solid_conc <- round(dfPb$Solid_conc, digits = 2)

# Create barplot
(Pb_S_barplot_S2 <- ggplot(data=dfPb, aes(x=Depth, y=Solid_conc)) +
    geom_bar(stat="identity", fill = "#8B4513", color = "black") +
    scale_x_reverse(breaks=c(0,2,4,6,8,10,12,14), expand = c(0,0), labels = c("0 - 2", "2 - 4", "4 - 6", "6 - 8", "8 - 10", "10 - 12", "12 - 14", "14 - 16")) +
    scale_y_continuous(expand = c(0,0)) +
    geom_text(aes(label=Solid_conc), color = "white", position=position_stack(vjust=0.5), size=4, fontface = "bold") +
    coord_flip() +
    labs(title = "Site 2 (Pb)", x = "Depth (cm)", y = expression(Concentration~(μg~g^-1))) +
    theme_classic() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 13),
          plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    ))

# Combine site 2 plots
combined_sediment_s2_plot <- ggarrange(As_S_barplot_S2, Al_S_barplot_S2, V_S_barplot_S2, Cr_S_barplot_S2, Pb_S_barplot_S2,
                                       ncol = 3, nrow = 2,
                                       align = "v")
combined_sediment_s2_plot


# Site 3
# As
dfAs <- kl_icp[kl_icp$Element=="As",]
dfAs <- dfAs[dfAs$Measurement_type=="S",]
dfAs <- dfAs[dfAs$Site=="3",]

# Round Solid_conc to 2 decimal places
dfAs$Solid_conc <- round(dfAs$Solid_conc, digits = 2)

# Create barplot
(As_S_barplot_S3 <- ggplot(data=dfAs, aes(x=Depth, y=Solid_conc)) +
    geom_bar(stat="identity", fill = "#8B4513", color = "black") +
    scale_x_reverse(breaks=c(0,2,4,6,8), expand = c(0,0), labels = c("0 - 2", "2 - 4", "4 - 6", "6 - 8", "8 - 10")) +
    scale_y_continuous(expand = c(0,0)) +
    geom_text(aes(label=Solid_conc), color = "white", position=position_stack(vjust=0.5), size=5, fontface = "bold") +
    coord_flip() +
    labs(title = "Site 3 (As)", x = "Depth (cm)", y = expression(Concentration~(μg~g^-1))) +
    theme_classic() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 13),
          plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    ))

# Al
dfAl <- kl_icp[kl_icp$Element=="Al",]
dfAl <- dfAl[dfAl$Measurement_type=="S",]
dfAl <- dfAl[dfAl$Site=="3",]

# Round Solid_conc to 2 decimal places
dfAl$Solid_conc <- round(dfAl$Solid_conc, digits = 2)

# Create barplot
(Al_S_barplot_S3 <- ggplot(data=dfAl, aes(x=Depth, y=Solid_conc)) +
    geom_bar(stat="identity", fill = "#8B4513", color = "black") +
    scale_x_reverse(breaks=c(0,2,4,6,8), expand = c(0,0), labels = c("0 - 2", "2 - 4", "4 - 6", "6 - 8", "8 - 10")) +
    scale_y_continuous(expand = c(0,0)) +
    geom_text(aes(label=Solid_conc), color = "white", position=position_stack(vjust=0.5), size=5, fontface = "bold") +
    coord_flip() +
    labs(title = "Site 3 (Al)", x = "Depth (cm)", y = expression(Concentration~(μg~g^-1))) +
    theme_classic() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 13),
          plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    ))

# V 
dfV <- kl_icp[kl_icp$Element=="V",]
dfV <- dfV[dfV$Measurement_type=="S",]
dfV <- dfV[dfV$Site=="3",]

# Round Solid_conc to 2 decimal places
dfV$Solid_conc <- round(dfV$Solid_conc, digits = 2)

# Create barplot
(V_S_barplot_S3 <- ggplot(data=dfV, aes(x=Depth, y=Solid_conc)) +
    geom_bar(stat="identity", fill = "#8B4513", color = "black") +
    scale_x_reverse(breaks=c(0,2,4,6,8), expand = c(0,0), labels = c("0 - 2", "2 - 4", "4 - 6", "6 - 8", "8 - 10")) +
    scale_y_continuous(expand = c(0,0)) +
    geom_text(aes(label=Solid_conc), color = "white", position=position_stack(vjust=0.5), size=5, fontface = "bold") +
    coord_flip() +
    labs(title = "Site 3 (V)", x = "Depth (cm)", y = expression(Concentration~(μg~g^-1))) +
    theme_classic() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 13),
          plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    ))

# Cr
dfCr <- kl_icp[kl_icp$Element=="Cr",]
dfCr <- dfCr[dfCr$Measurement_type=="S",]
dfCr <- dfCr[dfCr$Site=="3",]

# Round Solid_conc to 2 decimal places
dfCr$Solid_conc <- round(dfCr$Solid_conc, digits = 2)

# Create barplot
(Cr_S_barplot_S3 <- ggplot(data=dfCr, aes(x=Depth, y=Solid_conc)) +
    geom_bar(stat="identity", fill = "#8B4513", color = "black") +
    scale_x_reverse(breaks=c(0,2,4,6,8), expand = c(0,0), labels = c("0 - 2", "2 - 4", "4 - 6", "6 - 8", "8 - 10")) +
    scale_y_continuous(expand = c(0,0)) +
    geom_text(aes(label=Solid_conc), color = "white", position=position_stack(vjust=0.5), size=5, fontface = "bold") +
    coord_flip() +
    labs(title = "Site 3 (Cr)", x = "Depth (cm)", y = expression(Concentration~(μg~g^-1))) +
    theme_classic() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 13),
          plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    ))

# Pb
dfPb <- kl_icp[kl_icp$Element=="Pb",]
dfPb <- dfPb[dfPb$Measurement_type=="S",]
dfPb <- dfPb[dfPb$Site=="3",]

# Round Solid_conc to 2 decimal places
dfPb$Solid_conc <- round(dfPb$Solid_conc, digits = 2)

# Create barplot
(Pb_S_barplot_S3 <- ggplot(data=dfPb, aes(x=Depth, y=Solid_conc)) +
    geom_bar(stat="identity", fill = "#8B4513", color = "black") +
    scale_x_reverse(breaks=c(0,2,4,6,8), expand = c(0,0), labels = c("0 - 2", "2 - 4", "4 - 6", "6 - 8", "8 - 10")) +
    scale_y_continuous(expand = c(0,0)) +
    geom_text(aes(label=Solid_conc), color = "white", position=position_stack(vjust=0.5), size=5, fontface = "bold") +
    coord_flip() +
    labs(title = "Site 3 (Pb)", x = "Depth (cm)", y = expression(Concentration~(μg~g^-1))) +
    theme_classic() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 13),
          plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    ))

# Combine site 3 plots
combined_sediment_s3_plot <- ggarrange(As_S_barplot_S3, Al_S_barplot_S3, V_S_barplot_S3, Cr_S_barplot_S3, Pb_S_barplot_S3,
                                       ncol = 3, nrow = 2,
                                       align = "v")
combined_sediment_s3_plot


# CRM----
# Plant acid digestion
dfPCRM <- kl_icp[kl_icp$Sample_name=="p21",]
dfPCRM2 <- kl_icp[kl_icp$Sample_name=="p22",]

# As
(0.2090497 + 0.2236307)/2
(0.2163402/0.042)*100

# Cr
(18.4772060 + 18.7913379)/2
(18.63427/24.8)*100

# Pb
(2.5620493 + 2.0747134) /2
(2.318381/1.67)*100


# Aggregate plant As concentrations
dfAs <- kl_icp[kl_icp$Element=="As",]
dfAs <- dfAs[dfAs$Measurement_type=="P",]
dfAs <- dfAs[dfAs$Data_collector=="L",]


aggregate(dfAs$Solid_conc, by=list(dfAs$Site), FUN = mean)


# Titanium----
# Plants

dfTi <- kl_icp[which(kl_icp$Element=="Ti"),]
dfTi<- dfTi[!(dfTi$Solid_conc=="NA"),]

dfTi <- dfTi %>% drop_na(Solid_conc)
           
# Calculate mean concentration
aggregate(dfTi$Solid_conc, by=list(dfTi$Measurement_type), FUN = mean)
aggregate(dfTi$Solid_conc, by=list(dfTi$Measurement_type), FUN = sd)





