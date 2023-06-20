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
esmd_clean <- read.csv(file.path(MA_dir,"L2/otc_effect_sizes_L2.csv"))

# clean labels for plotting
var_labels <- c("Phen_flwr_lifespan" = "Flower lifespan",
                            "Phen_early" = "Spring phenophases",
                            "Nitrogen_above" = "Aboveground N",
                            "Phen_late" = "Fall phenophases",
                            "Nitrogen_below" = "Belowground N",
                            "Flower_num" = "Number of flowers",
                            "Percent_cover" = "Percent cover",
                            "Leaf_Growth" = "Leaf growth",
                            "Fruit_num" = "Number of fruits",
                            "Biomass_above" = "Aboveground biomass",
                            "Growth" = "Plant growth",
                            "Fruit_weight" = "Fruit weight",
                            "Biomass_below" = "Belowground biomass")


### removing variables that have <10 effect sizes
esmd_clean %>%
  group_by(Var_type_broad) %>%
  summarize(count = n())
esmd_clean <- esmd_clean %>%
  filter(!(Var_type_broad == "Biomass_total" |
             Var_type_broad == "Flower_weight" |
             Var_type_broad == "Phen_leaf_lifespan" |
             Var_type_broad == "Phen_preflwr_length"))


### overall effect size for all variable types ###
# https://ecologyforacrowdedplanet.wordpress.com/2013/05/10/using-metafor-and-ggplot-togetherpart-1/
esmd_var_type_sum <- esmd_clean %>%
  group_by(Var_type_broad) %>%
  summarize(count = n(),
            avg = mean(yi, na.rm = TRUE),
            se = std.error(yi, na.rm = TRUE))
png("effect.png", units="in", width=8, height=6, res=300)
ggplot(esmd_var_type_sum, aes(y = reorder(Var_type_broad, -avg, FUN=mean), x = avg)) +
  #facet_wrap(.~Var_type) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", cex = 1, alpha = 0.5) +
  geom_point(shape = 18, size = 4) +  
  geom_errorbarh(aes(xmin = avg - se, xmax = avg + se), height = 0.25) +
  scale_y_discrete(labels=c("Phen_flwr_lifespan" = "Flower lifespan (13)",
                            "Phen_early" = "Spring phenophases (118)",
                            "Nitrogen_above" = "Aboveground N (127)",
                            "Phen_late" = "Fall phenophases (54)",
                            "Nitrogen_below" = "Belowground N (12)",
                            "Flower_num" = "Number of flowers (79)",
                            "Percent_cover" = "Percent cover (177)",
                            "Leaf_Growth" = "Leaf growth (134)",
                            "Fruit_num" = "Number of fruits (34)",
                            "Biomass_above" = "Aboveground biomass (153)",
                            "Growth" = "Plant growth (135)",
                            "Fruit_weight" = "Fruit weight (27)",
                            "Biomass_below" = "Belowground biomass (44)")) +
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


### effect size for variables sorted by functional groups ###
esmd_func <- esmd_clean %>%
  group_by(Var_type_broad, Func_group_broad) %>%
  summarize(count = n(),
            avg = mean(yi, na.rm = TRUE),
            se = std.error(yi, na.rm = TRUE)) %>%
  filter(!(Func_group_broad == "" | Func_group_broad == "Pteridophyte" | Func_group_broad == "Total"))
png("effect_func.png", units="in", width=8, height=6, res=300)
ggplot(esmd_func, aes(y = Func_group_broad, x = avg)) +
  facet_wrap(.~Var_type_broad, labeller = as_labeller(var_labels)) +
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


### effect size for variables sorted by native/non-native ###
esmd_native <- esmd_clean %>%
  filter(!(Native_Status == "")) %>%
  filter(!(Var_type_broad == "Nitrogen_below" | Var_type_broad == "Phen_flwr_lifespan")) %>%
  group_by(Var_type_broad, Native_Status) %>%
  summarize(count = n(),
            avg = mean(yi, na.rm = TRUE),
            se = std.error(yi, na.rm = TRUE))
png("effect_native.png", units="in", width=8, height=6, res=300)
ggplot(esmd_native, aes(y = Native_Status, x = avg)) +
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


### effect size based on year round warming (yes or no) ###
esmd_yearround <- esmd_clean %>%
  filter(!(Var_type_broad == "Phen_flwr_lifespan")) %>% # remove vars that don't contain both categories
  group_by(Var_type_broad, Year_round_warm) %>%
  summarize(count = n(),
            avg = mean(yi, na.rm = TRUE),
            se = std.error(yi, na.rm = TRUE))
# making text labels for sample sizes
# year round warming
ann_text_abovebio <- data.frame(avg=1.8,Year_round_warm="Yes",lab = "Text",
                       Var_type_broad = factor("Biomass_above"))
ann_text_belowbio <- data.frame(avg=1.1,Year_round_warm="Yes",lab = "Text",
                                Var_type_broad = factor("Biomass_below"))
ann_text_flwrnum <- data.frame(avg=0.6,Year_round_warm="Yes",lab = "Text",
                                Var_type_broad = factor("Flower_num"))
ann_text_fruitnum <- data.frame(avg=0.8,Year_round_warm="Yes",lab = "Text",
                               Var_type_broad = factor("Fruit_num"))
ann_text_fruitweight <- data.frame(avg=2.2,Year_round_warm="Yes",lab = "Text",
                               Var_type_broad = factor("Fruit_weight"))
ann_text_plantgrwth <- data.frame(avg=1.4,Year_round_warm="Yes",lab = "Text",
                               Var_type_broad = factor("Growth"))
ann_text_leafgrwth <- data.frame(avg=1,Year_round_warm="Yes",lab = "Text",
                               Var_type_broad = factor("Leaf_Growth"))
ann_text_aboven <- data.frame(avg=0.2,Year_round_warm="Yes",lab = "Text",
                               Var_type_broad = factor("Nitrogen_above"))
ann_text_belown <- data.frame(avg=1,Year_round_warm="Yes",lab = "Text",
                               Var_type_broad = factor("Nitrogen_below"))
ann_text_perc <- data.frame(avg=0.7,Year_round_warm="Yes",lab = "Text",
                               Var_type_broad = factor("Percent_cover"))
ann_text_spring <- data.frame(avg=0,Year_round_warm="Yes",lab = "Text",
                               Var_type_broad = factor("Phen_early"))
ann_text_fall <- data.frame(avg=1.2,Year_round_warm="Yes",lab = "Text",
                               Var_type_broad = factor("Phen_late"))
# seasonal wawrming
ann_text_abovebio2 <- data.frame(avg=0.9,Year_round_warm="No",lab = "Text",
                                Var_type_broad = factor("Biomass_above"))
ann_text_belowbio2 <- data.frame(avg=0.7,Year_round_warm="No",lab = "Text",
                                Var_type_broad = factor("Biomass_below"))
ann_text_flwrnum2 <- data.frame(avg=0.7,Year_round_warm="No",lab = "Text",
                               Var_type_broad = factor("Flower_num"))
ann_text_fruitnum2 <- data.frame(avg=2.2,Year_round_warm="No",lab = "Text",
                                Var_type_broad = factor("Fruit_num"))
ann_text_fruitweight2 <- data.frame(avg=1,Year_round_warm="No",lab = "Text",
                                   Var_type_broad = factor("Fruit_weight"))
ann_text_plantgrwth2 <- data.frame(avg=1.3,Year_round_warm="No",lab = "Text",
                                  Var_type_broad = factor("Growth"))
ann_text_leafgrwth2 <- data.frame(avg=1.2,Year_round_warm="No",lab = "Text",
                                 Var_type_broad = factor("Leaf_Growth"))
ann_text_aboven2 <- data.frame(avg=0.7,Year_round_warm="No",lab = "Text",
                              Var_type_broad = factor("Nitrogen_above"))
ann_text_belown2 <- data.frame(avg=-0.1,Year_round_warm="No",lab = "Text",
                              Var_type_broad = factor("Nitrogen_below"))
ann_text_perc2 <- data.frame(avg=0.7,Year_round_warm="No",lab = "Text",
                            Var_type_broad = factor("Percent_cover"))
ann_text_spring2 <- data.frame(avg=0.1,Year_round_warm="No",lab = "Text",
                              Var_type_broad = factor("Phen_early"))
ann_text_fall2 <- data.frame(avg=0.1,Year_round_warm="No",lab = "Text",
                            Var_type_broad = factor("Phen_late"))
png("effect_yearround.png", units="in", width=8, height=6, res=300)
ggplot(esmd_yearround, aes(y = Year_round_warm, x = avg)) +
  facet_wrap(.~Var_type_broad, labeller = as_labeller(var_labels)) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", cex = 1, alpha = 0.5) +
  geom_point(shape = 18, size = 4) +  
  geom_errorbarh(aes(xmin = avg - se, xmax = avg + se), height = 0.25) +
  scale_y_discrete(labels=c("Yes" = "Year-round warming",
                            "No" = "Seasonal warming")) +
  xlab("Mean effect size (Hedges' g) +/- SE") + 
  ylab(" ") + 
  geom_text(data = ann_text_abovebio,label = "(104)",size=3.5) +
  geom_text(data = ann_text_belowbio,label = "(42)",size=3.5) +
  geom_text(data = ann_text_aboven,label = "(101)",size=3.5) +
  geom_text(data = ann_text_belown,label = "(11)",size=3.5) +
  geom_text(data = ann_text_fall,label = "(23)",size=3.5) +
  geom_text(data = ann_text_flwrnum,label = "(49)",size=3.5) +
  geom_text(data = ann_text_fruitnum,label = "(25)",size=3.5) +
  geom_text(data = ann_text_fruitweight,label = "(24)",size=3.5) +
  geom_text(data = ann_text_leafgrwth,label = "(102)",size=3.5) +
  geom_text(data = ann_text_perc,label = "(86)",size=3.5) +
  geom_text(data = ann_text_plantgrwth,label = "(77)",size=3.5) +
  geom_text(data = ann_text_spring,label = "(36)",size=3.5) +
  geom_text(data = ann_text_abovebio2,label = "(49)",size=3.5) +
  geom_text(data = ann_text_belowbio2,label = "(2)",size=3.5) +
  geom_text(data = ann_text_aboven2,label = "(26)",size=3.5) +
  geom_text(data = ann_text_belown2,label = "(1)",size=3.5) +
  geom_text(data = ann_text_fall2,label = "(31)",size=3.5) +
  geom_text(data = ann_text_flwrnum2,label = "(30)",size=3.5) +
  geom_text(data = ann_text_fruitnum2,label = "(9)",size=3.5) +
  geom_text(data = ann_text_fruitweight2,label = "(3)",size=3.5) +
  geom_text(data = ann_text_leafgrwth2,label = "(32)",size=3.5) +
  geom_text(data = ann_text_perc2,label = "(91)",size=3.5) +
  geom_text(data = ann_text_plantgrwth2,label = "(58)",size=3.5) +
  geom_text(data = ann_text_spring2,label = "(82)",size=3.5) +
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


### effect size based on latitude of study ###
ggplot(esmd_clean, aes(x = Latitude, y = yi)) +
  facet_wrap(.~Var_type_broad) +
  geom_point(size = 2) +
  geom_smooth(method = 'lm') +
  xlab("Latitude") +
  ylab("Effect size") +
  #ylim(0,80) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


### effect size based on distance from range edge ###
#esmd_clean$Lat_difference[esmd_clean$Lat_difference > 30] <- NA
ggplot(esmd_clean, aes(x = Lat_difference, y = yi)) +
  facet_wrap(.~Var_type_broad) +
  geom_point(size = 2) +
  geom_smooth(method = 'lm') +
  xlab("Latitude difference") +
  ylab("Effect size") +
  ylim(-10,10) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
# categorical? 
esmd_range <- esmd_clean
esmd_range$Lat_category <- NA
esmd_range$Lat_category[esmd_range$Lat_difference < 30] <- "Near_edge"
esmd_range$Lat_category[esmd_range$Lat_difference >= 30] <- "Far_from_edge"
esmd_range <- esmd_range %>%
  na.omit(Lat_category) %>%
  group_by(Var_type_broad, Lat_category) %>%
  summarize(count = n(),
            avg = mean(yi, na.rm = TRUE),
            se = std.error(yi, na.rm = TRUE))
png("effect_range.png", units="in", width=8, height=6, res=300)
ggplot(esmd_range, aes(y = Lat_category, x = avg)) +
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

### effect size based on amount warmed ###
esmd_amount <- esmd_clean %>%
  filter(!(Amount_warmed_type == "" |
             Amount_warmed_type == "Entire_average" |
             Amount_warmed_type == "Annual_average" |
             Amount_warmed_type == "Monthly_average"))
ggplot(esmd_amount, aes(x = Amount_warmed_C, y = yi)) +
  facet_wrap(.~Var_type_broad) +
  geom_point(size = 2) +
  geom_smooth(method = 'lm') +
  xlab("Amount warmed (°C)") +
  ylab("Effect size") +
  #ylim(-10,10) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


### effect size based on # months warmed ###
ggplot(esmd_clean, aes(x = Years_warmed, y = yi)) +
  facet_wrap(.~Var_type_broad) +
  geom_point(size = 2) +
  geom_smooth(method = 'lm') +
  xlab("Amount warmed (°C)") +
  ylab("Effect size") +
  #ylim(-10,10) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))



### funnel plot ###
# https://www.metafor-project.org/doku.php/plots:funnel_plot_variations
### fit equal-effects model
res <- rma(yi, vi, data=esmd_clean, measure="SMD", method="EE")
### draw funnel plots
funnel(res, main="Standard Error")
funnel(res, yaxis="vi", main="Sampling Variance")
funnel(res, yaxis="seinv", main="Inverse Standard Error")
funnel(res, yaxis="vinv", main="Inverse Sampling Variance")

# correlation test between effect size and sample size
res <- cor.test(esmd_clean$Warmed_N+esmd_clean$Ambient_N, esmd_clean$yi, 
                method = "pearson")
res
ggplot(esmd_clean, aes(x = Ambient_N, y = yi)) +
  geom_point() +
  geom_smooth(method = 'lm')
