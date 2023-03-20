# TITLE:          Effect size plots
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Phoebe Zarnetske, Pat Bills
# DATA INPUT:     L2 manipulated data from the L2 folder
# DATA OUTPUT:    Effect size plots
# DATE:           Sep 2022; Jan 2023

# clear environment
rm(list = ls())

# Load packages
library(tidyverse)
library(metafor)
library(plotrix)

# set working directory
MA_dir<-Sys.getenv("MADIR")

# read in data
otc_data <- read.csv(file.path(MA_dir,"L2/otc_data_cleaned_L2.csv"))


# calculating effect sizes
# https://rfunctions.blogspot.com/2016/10/meta-analysis-in-r.html
esmd <- escalc(measure="SMD", m1i=Warmed_Mean, m2i=Ambient_Mean, # SMD = Hedge's g
               sd1i=Warmed_SD, sd2i=Ambient_SD,
               n1i=Warmed_N, n2i=Ambient_N,
               data=otc_data)

# remove rows with incomplete data
esmd_clean <- esmd %>%
  filter(!is.na(vi))

# uploading the effect size data
write.csv(esmd_clean, file.path(MA_dir,"L2/otc_effect_sizes_L2.csv"), row.names=F)

# https://ecologyforacrowdedplanet.wordpress.com/2013/05/10/using-metafor-and-ggplot-togetherpart-1/
# example stats: update var_type based on what variable to look at
unique(esmd_clean$Var_type_broad)
unique(esmd_clean$Pub_number)
esmd_clean %>% 
  count(Var_type_broad)

esmd_var_type <- esmd_clean %>%
  filter(Var_type_broad == "Biomass_above" | Var_type_broad == "Biomass_below"
         | Var_type_broad == "Percent_cover" | Var_type_broad == "Height"
         | Var_type_broad == "Flower_num" | Var_type_broad == "Nitrogen_above")

esmd_var_phen <- esmd_clean %>%
  filter(Var_type_broad == "Phen_early" | Var_type_broad == "Phen_late")

# basic plot
forest.rma(SMD.ma)

# all data sorted by variable type
esmd_var_type_sum <- esmd_clean %>%
  group_by(Var_type_broad) %>%
  summarize(avg = mean(yi, na.rm = TRUE),
            se = std.error(yi, na.rm = TRUE))
png("effect.png", units="in", width=8, height=6, res=300)
ggplot(esmd_var_type_sum, aes(y = reorder(Var_type_broad, -avg, FUN=mean), x = avg)) +
  #facet_wrap(.~Var_type) +
  geom_point(shape = 18, size = 4) +  
  geom_errorbarh(aes(xmin = avg - se, xmax = avg + se), height = 0.25) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", cex = 1, alpha = 0.5) +
  #scale_y_continuous(name = "", breaks=1:4, labels = dat$label, trans = "reverse") +
  xlab("Mean effect size (Hedges' g) +/- SE") + 
  ylab(" ") + 
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 12, colour = "black"),
        axis.text.x.bottom = element_text(size = 12, colour = "black"),
        axis.title.x = element_text(size = 12, colour = "black"))
dev.off()

# selected variables sorted by functional group
esmd_var_type_sum2 <- esmd_var_type %>%
  group_by(Var_type_broad, Func_group_broad) %>%
  summarize(avg = mean(yi, na.rm = TRUE),
            se = std.error(yi, na.rm = TRUE))
png("effect_func.png", units="in", width=8, height=6, res=300)
ggplot(esmd_var_type_sum2, aes(y = Func_group_broad, x = avg)) +
  facet_wrap(.~Var_type_broad) +
  geom_point(shape = 18, size = 4) +  
  geom_errorbarh(aes(xmin = avg - se, xmax = avg + se), height = 0.25) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", cex = 1, alpha = 0.5) +
  #scale_y_continuous(name = "", breaks=1:4, labels = dat$label, trans = "reverse") +
  xlab("Mean effect size (Hedges' g) +/- SE") + 
  ylab(" ") + 
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 12, colour = "black"),
        axis.text.x.bottom = element_text(size = 12, colour = "black"),
        axis.title.x = element_text(size = 12, colour = "black"))
dev.off()

# regression between latitude and effect size
ggplot(esmd_var_type, aes(x = Latitude, y = yi)) +
  facet_wrap(.~Var_type_broad) +
  geom_point(size = 2) +
  geom_smooth(method = 'lm') +
  xlab("Latitude") +
  ylab("Effect size") +
  #ylim(0,80) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

# funnel plot
# https://www.metafor-project.org/doku.php/plots:funnel_plot_variations
### fit equal-effects model
res <- rma(yi, vi, data=esmd_clean, measure="SMD", method="EE")
### draw funnel plots
funnel(res, main="Standard Error")
funnel(res, yaxis="vi", main="Sampling Variance")
funnel(res, yaxis="seinv", main="Inverse Standard Error")
funnel(res, yaxis="vinv", main="Inverse Sampling Variance")
