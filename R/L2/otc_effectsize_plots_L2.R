# TITLE:          Effect size plots
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Phoebe Zarnetske
# DATA INPUT:     L2 manipulated data from the L2 folder
# DATA OUTPUT:    Effect size plots
# DATE:           Sep 2022; Jan 2023; July 2023

# clear environment
rm(list = ls())

# Load packages
library(tidyverse)
library(metafor)
library(plotrix)
library(maps)
library(ggpubr)
library(ggpattern)

# set working directory
MA_dir<-Sys.getenv("MADIR")

# read in data
esmd_clean <- read.csv(file.path(MA_dir,"L2/otc_effect_sizes_L2.csv")) # year-limited (main dataset)
esmd_clean_allyears <- read.csv(file.path(MA_dir,"L2/otc_data_cleaned_allyears_L2.csv")) # data for all years
world <- map_data("world")

# clean labels for plotting
var_labels <- c("Phen_flwr_lifespan" = "Flower lifespan",
                            "Phen_early" = "Spring phenophases",
                            "Nitrogen_above" = "Aboveground N",
                            "Phen_late" = "Fall phenophases",
                            "Nitrogen_below" = "Belowground N",
                            "Flower_num" = "Number of flowers",
                            "Percent_cover" = "Percent cover",
                            "Leaf_growth" = "Leaf growth",
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
             Var_type_broad == "Phen_preflwr_length" |
             Var_type == "LMA")) # also removing LMA
esmd_clean_allyears <- esmd_clean_allyears %>%
  filter(!(Var_type_broad == "Biomass_total" |
             Var_type_broad == "Flower_weight" |
             Var_type_broad == "Phen_leaf_lifespan" |
             Var_type_broad == "Phen_preflwr_length" |
             Var_type == "LMA")) # also removing LMA


### overall effect size for all variable types ###
# https://ecologyforacrowdedplanet.wordpress.com/2013/05/10/using-metafor-and-ggplot-togetherpart-1/
# if species is blank, then input func type, if not keep species listed
# fixing species names for measurements that don't have a species
esmd_clean$Genus_Species[esmd_clean$Genus_Species == "_"] <- ""
esmd_clean2 <- esmd_clean
esmd_clean2$Genus_Species <- ifelse(esmd_clean2$Genus_Species == "",
                                    esmd_clean2$Func_group,
                                    esmd_clean2$Genus_Species)
esmd_clean_allyears$Genus_Species[esmd_clean_allyears$Genus_Species == "_"] <- ""
esmd_clean_allyears2 <- esmd_clean_allyears
esmd_clean_allyears2$Genus_Species <- ifelse(esmd_clean_allyears2$Genus_Species == "",
                                    esmd_clean_allyears2$Func_group,
                                    esmd_clean_allyears2$Genus_Species)
# getting avgs
esmd_var_type_sum <- esmd_clean2 %>% # year-limited
  group_by(Var_type_broad) %>%
  summarize(count = n(),
            avg = mean(yi, na.rm = TRUE),
            se = std.error(yi, na.rm = TRUE),
            CI_lower = avg - (1.96 * se),
            CI_upper = avg + (1.96 * se))
esmd_var_type_sum2 <- esmd_clean_allyears2 %>% # all years
  group_by(Var_type_broad) %>%
  summarize(count = n(),
            avg = mean(yi, na.rm = TRUE),
            se = std.error(yi, na.rm = TRUE),
            CI_lower = avg - (1.96 * se),
            CI_upper = avg + (1.96 * se))
# making dataframe for mean estimataed effect sizes from model output (in otc_effectsize_analyses_L2.R)
esmd_est_mean <- data.frame(Var_type_broad = c("Biomass_above","Biomass_below","Flower_num","Fruit_num","Fruit_weight",
                                               "Growth","Leaf_growth","Nitrogen_above","Nitrogen_below","Percent_cover",
                                               "Phen_early","Phen_flwr_lifespan","Phen_late"),
                            avg = c(0.2528,0.6033,-0.0763,-0.0982,0.5776,0.6488,0.5369,-0.4093,-0.1082,0.1044,-0.1223,0.1364,-0.0373),
                            CI_upper = c(0.3889,0.9203,0.0866,0.0865,0.8578,0.7757,0.6646,-0.2450,0.4367,0.2495,0.0140,0.3091,0.1190),
                            CI_lower = c(0.1167,0.2862,-0.2391,-0.2829,0.2974,0.5220,0.4091,-0.5735,-0.6531,-0.0406,-0.2586,-0.0363,-0.1936),
                            sig = c("yes","yes","no","no","yes","yes","yes","yes","no","no","yes","no","no"))

png("effect.png", units="in", width=8, height=6, res=300)
ggplot(esmd_est_mean, aes(y = reorder(Var_type_broad, -avg, FUN=mean), x = avg)) +
  #facet_wrap(.~Var_type) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", cex = 1, alpha = 0.7) +
  geom_point(aes(shape = sig), size = 4) +  
  scale_shape_manual(values=c(1,16)) +
  geom_errorbarh(aes(xmin = CI_lower, xmax =CI_upper), height = 0.25) +
  scale_y_discrete(labels=c("Phen_flwr_lifespan" = "Flower lifespan (37)",
                            "Phen_early" = "Spring phenophases (186)",
                            "Nitrogen_above" = "Aboveground N (131)",
                            "Phen_late" = "Fall phenophases (77)",
                            "Nitrogen_below" = "Belowground N (12)",
                            "Flower_num" = "Number of flowers (79)",
                            "Percent_cover" = "Percent cover (193)",
                            "Leaf_growth" = "Leaf growth (140)",
                            "Fruit_num" = "Number of fruits (34)",
                            "Biomass_above" = "Aboveground biomass (169)",
                            "Growth" = "Plant growth (135)",
                            "Fruit_weight" = "Fruit weight (27)",
                            "Biomass_below" = "Belowground biomass (44)")) +
  xlab("Mean effect size (Hedges' g)") + 
  ylab(" ") + 
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 13, colour = "black"),
        axis.text.x.bottom = element_text(size = 12, colour = "black"),
        legend.position="none",
        axis.title.x = element_text(size = 13, colour = "black"))
dev.off()



### effect size for variables sorted by functional groups ###
esmd_func <- esmd_clean2 %>%
  group_by(Var_type_broad, Func_group_broad) %>%
  summarize(count = n(),
            avg = mean(yi, na.rm = TRUE),
            se = std.error(yi, na.rm = TRUE),
            CI_lower = avg - (1.96 * se),
            CI_upper = avg + (1.96 * se)) %>%
  filter(!(Func_group_broad == "")) # remove blank functional groups
  #filter(!(count < 8)) %>% # remove categories w/ sample size < 8
 # filter(!(Var_type_broad == "Fruit_num" | Var_type_broad == "Nitrogen_below"))
# making dataframe for mean estimated effect sizes from model output (in otc_effectsize_analyses_L2.R)
esmd_func2 <- data.frame(Var_type_broad = c("Biomass_above","Biomass_above","Biomass_above","Biomass_above","Biomass_above","Biomass_above","Biomass_above",
                                            "Biomass_below","Biomass_below","Biomass_below","Biomass_below","Biomass_below","Biomass_below","Biomass_below",
                                            "Flower_num", "Flower_num","Flower_num", "Flower_num","Flower_num", "Flower_num","Flower_num",
                                            "Fruit_num","Fruit_num","Fruit_num","Fruit_num","Fruit_num","Fruit_num","Fruit_num",
                                            "Fruit_weight","Fruit_weight","Fruit_weight","Fruit_weight","Fruit_weight","Fruit_weight","Fruit_weight",
                                            "Growth","Growth","Growth","Growth","Growth","Growth","Growth",
                                            "Leaf_growth","Leaf_growth","Leaf_growth","Leaf_growth","Leaf_growth","Leaf_growth","Leaf_growth",
                                            "Percent_cover","Percent_cover","Percent_cover","Percent_cover","Percent_cover","Percent_cover","Percent_cover",
                                            "Nitrogen_above","Nitrogen_above","Nitrogen_above","Nitrogen_above","Nitrogen_above","Nitrogen_above","Nitrogen_above",
                                            "Nitrogen_below","Nitrogen_below","Nitrogen_below","Nitrogen_below","Nitrogen_below","Nitrogen_below","Nitrogen_below",
                                            "Phen_early","Phen_early","Phen_early","Phen_early","Phen_early","Phen_early","Phen_early",
                                            "Phen_late","Phen_late","Phen_late","Phen_late","Phen_late","Phen_late","Phen_late",
                                            "Phen_flwr_lifespan","Phen_flwr_lifespan","Phen_flwr_lifespan","Phen_flwr_lifespan","Phen_flwr_lifespan","Phen_flwr_lifespan","Phen_flwr_lifespan"),
                             Func_group_broad = c("Bryophyte","Forb","Graminoid","Lichen","Shrub","Total","Tree",
                                                  "Bryophyte","Forb","Graminoid","Lichen","Shrub","Total","Tree",
                                                  "Bryophyte","Forb","Graminoid","Lichen","Shrub","Total","Tree",
                                                  "Bryophyte","Forb","Graminoid","Lichen","Shrub","Total","Tree",
                                                  "Bryophyte","Forb","Graminoid","Lichen","Shrub","Total","Tree",
                                                  "Bryophyte","Forb","Graminoid","Lichen","Shrub","Total","Tree",
                                                  "Bryophyte","Forb","Graminoid","Lichen","Shrub","Total","Tree",
                                                  "Bryophyte","Forb","Graminoid","Lichen","Shrub","Total","Tree",
                                                  "Bryophyte","Forb","Graminoid","Lichen","Shrub","Total","Tree",
                                                  "Bryophyte","Forb","Graminoid","Lichen","Shrub","Total","Tree",
                                                  "Bryophyte","Forb","Graminoid","Lichen","Shrub","Total","Tree",
                                                  "Bryophyte","Forb","Graminoid","Lichen","Shrub","Total","Tree",
                                                  "Bryophyte","Forb","Graminoid","Lichen","Shrub","Total","Tree"),
                         sig = c("no","no","yes","no","no","yes","yes",
                                 "no","no","no","no","no","no","no",
                                 "no","no","no","no","no","no","no",
                                 "no","no","yes","no","no","no","no",
                                 "no","yes","yes","no","no","no","no",
                                 "no","yes","yes","no","yes","yes","no",
                                 "no","yes","yes","no","yes","no","no",
                                 "yes","no","yes","yes","yes","no","no",
                                 "no","no","yes","no","yes","no","yes",
                                 "no","no","no","no","no","no","no",
                                 "no","yes","yes","no","yes","no","no",
                                 "no","no","no","no","no","no","no",
                                 "no","yes","yes","no","no","no","no"),
                         avg = c(-0.015,0.1589,0.454,-0.2938,0.2817,0.7380,1.0902,
                                      NA,NA,1.4871,NA,1.0431,0.6535,1.4969,
                                      NA,-0.0236,-0.0107,NA,0.1258,NA,NA,
                                      0.571,0.0445,1.3665,NA,0.4327,NA,NA,
                                      NA,0.6859,1.8999,NA,0.4854,NA,NA,
                                      -0.1576,0.7488,0.6011,NA,0.655,1.0637,0.4919,
                                      NA,0.2353,0.3797,NA,0.3802,-0.0586,0.1148,
                                      -0.461,-0.1072,0.2241,-0.389,0.3956,-0.0646,NA,
                                      0.4963,-0.3122,-0.583,0.0831,-0.472,0.3571,-1.018,
                                      NA,NA,0.4537,NA,0.1525,-0.6438,-0.5678,
                                      NA,-0.4575,-0.5069,NA,-0.6483,NA,-1.3683,
                                      NA,-0.8473,-0.6985,NA,-1.0726,NA,1.0533,
                                      NA,-0.8822,-0.7976,NA,0.8026,NA,NA),
                         CI_upper = c(0.6948,0.6680,0.9104,0.5979,0.7234,1.2185,2.2649,
                                           NA,NA,3.2778,NA,3.2785,1.6451,4.179,
                                           NA,0.2719,0.3159,NA,0.4281,NA,NA,
                                           1.7673,0.4837,2.6020,NA,1.1992,NA,NA,
                                           NA,1.3923,3.2099,NA,1.4488,NA,NA,
                                           0.9766,1.1556,0.9919,NA,1.0392,1.6632,1.0839,
                                           NA,0.4913,0.6137,NA,0.6282,0.7407,1.1026,
                                           -0.1901,0.1311,0.4119,-0.1089,0.6205,0.2981,NA,
                                           1.6966,0.1194,-0.2237,0.682,-0.1789,1.1432,-0.0645,
                                           NA,NA,3.6141,NA,4.4186,3.8236,3.76,
                                           NA,-0.0863,-0.1198,NA,-0.2344,NA,0.404,
                                           NA,0.4419,0.6012,NA,0.2793,NA,5.3189,
                                           NA,0.0329,0.1003,NA,2.1287,NA,NA),
                         CI_lower = c(-0.7247,-0.3501,-0.0024,-1.1856,-0.1599,0.2576,-0.0845,
                                           NA,NA,-0.3037,NA,-1.1922,-0.3382,-1.1853,
                                           NA,-0.3192,-0.3374,NA,-0.1764,NA,NA,
                                           -0.6252,-0.3948,0.1309,NA,-0.3338,NA,NA,
                                           NA,-0.0206,0.5899,NA,-0.478,NA,NA,
                                           -1.2917,0.3419,0.2103,NA,0.2707,0.4641,-0.1001,
                                           NA,-0.0207,0.1458,NA,0.1322,-0.8579,-0.873,
                                           -0.7329,-0.3455,0.0362,-0.6692,0.1708,-0.4273,NA,
                                           -0.7041,-0.7437,-0.9424,-0.5159,-0.7651,-0.4289,-1.9715,
                                           NA,NA,-2.7067,NA,-4.1137,-5.1112,-4.8956,
                                           NA,-0.8287,-0.894,NA,-1.0622,NA,-3.1405,
                                           NA,-2.1365,-1.9981,NA,-2.4245,NA,-3.2123,
                                           NA,-1.7973,-1.6954,NA,-0.5236,NA,NA))

# making text labels for sample sizes
# Tree
ann_text_abovebio <- data.frame(avg=3,Func_group_broad="Tree",lab = "Text",
                                Var_type_broad = factor("Biomass_above"))
ann_text_belowbio <- data.frame(avg=2.6,Func_group_broad="Tree",lab = "Text",
                                Var_type_broad = factor("Biomass_below"))
ann_text_plantgrwth <- data.frame(avg=1.6,Func_group_broad="Tree",lab = "Text",
                                  Var_type_broad = factor("Growth"))
ann_text_leafgrwth <- data.frame(avg=2,Func_group_broad="Tree",lab = "Text",
                                 Var_type_broad = factor("Leaf_Growth"))
ann_text_aboven <- data.frame(avg=-0.6,Func_group_broad="Tree",lab = "Text",
                              Var_type_broad = factor("Nitrogen_above"))
ann_text_belown <- data.frame(avg=1.2,Func_group_broad="Tree",lab = "Text",
                              Var_type_broad = factor("Nitrogen_below"))
ann_text_spring <- data.frame(avg=0,Func_group_broad="Tree",lab = "Text",
                              Var_type_broad = factor("Phen_early"))
ann_text_fall <- data.frame(avg=3,Func_group_broad="Tree",lab = "Text",
                            Var_type_broad = factor("Phen_late"))
# Shrub
ann_text_abovebio2 <- data.frame(avg=1.2,Func_group_broad="Shrub",lab = "Text",
                                Var_type_broad = factor("Biomass_above"))
ann_text_belowbio2 <- data.frame(avg=0.4,Func_group_broad="Shrub",lab = "Text",
                                Var_type_broad = factor("Biomass_below"))
ann_text_flwrnum2 <- data.frame(avg=1.2,Func_group_broad="Shrub",lab = "Text",
                                  Var_type_broad = factor("Flower_num"))
ann_text_fruitnum2 <- data.frame(avg=1.8,Func_group_broad="Shrub",lab = "Text",
                               Var_type_broad = factor("Fruit_num"))
ann_text_fruitweight2 <- data.frame(avg=1.8,Func_group_broad="Shrub",lab = "Text",
                               Var_type_broad = factor("Fruit_weight"))
ann_text_plantgrwth2 <- data.frame(avg=1.8,Func_group_broad="Shrub",lab = "Text",
                                  Var_type_broad = factor("Growth"))
ann_text_leafgrwth2 <- data.frame(avg=1.3,Func_group_broad="Shrub",lab = "Text",
                                 Var_type_broad = factor("Leaf_Growth"))
ann_text_aboven2 <- data.frame(avg=1.1,Func_group_broad="Shrub",lab = "Text",
                              Var_type_broad = factor("Nitrogen_above"))
ann_text_belown2 <- data.frame(avg=0.8,Func_group_broad="Shrub",lab = "Text",
                              Var_type_broad = factor("Nitrogen_below"))
ann_text_perc2 <- data.frame(avg=1.5,Func_group_broad="Shrub",lab = "Text",
                                  Var_type_broad = factor("Percent_cover"))
ann_text_spring2 <- data.frame(avg=0.4,Func_group_broad="Shrub",lab = "Text",
                              Var_type_broad = factor("Phen_early"))
ann_text_fall2 <- data.frame(avg=0.4,Func_group_broad="Shrub",lab = "Text",
                            Var_type_broad = factor("Phen_late"))
ann_text_flwrlife2 <- data.frame(avg=1.4,Func_group_broad="Shrub",lab = "Text",
                                  Var_type_broad = factor("Phen_flwr_lifespan"))
# Lichen
ann_text_abovebio3 <- data.frame(avg=0.4,Func_group_broad="Lichen",lab = "Text",
                                 Var_type_broad = factor("Biomass_above"))
ann_text_aboven3 <- data.frame(avg=0.8,Func_group_broad="Lichen",lab = "Text",
                               Var_type_broad = factor("Nitrogen_above"))
ann_text_perc3 <- data.frame(avg=0.6,Func_group_broad="Lichen",lab = "Text",
                             Var_type_broad = factor("Percent_cover"))
# Graminoid
ann_text_abovebio4 <- data.frame(avg=1.2,Func_group_broad="Graminoid",lab = "Text",
                                 Var_type_broad = factor("Biomass_above"))
ann_text_belowbio4 <- data.frame(avg=4.1,Func_group_broad="Graminoid",lab = "Text",
                                 Var_type_broad = factor("Biomass_below"))
ann_text_flwrnum4 <- data.frame(avg=1.1,Func_group_broad="Graminoid",lab = "Text",
                                Var_type_broad = factor("Flower_num"))
ann_text_fruitnum4 <- data.frame(avg=4.4,Func_group_broad="Graminoid",lab = "Text",
                                 Var_type_broad = factor("Fruit_num"))
ann_text_fruitweight4 <- data.frame(avg=3.6,Func_group_broad="Graminoid",lab = "Text",
                                    Var_type_broad = factor("Fruit_weight"))
ann_text_plantgrwth4 <- data.frame(avg=1.4,Func_group_broad="Graminoid",lab = "Text",
                                   Var_type_broad = factor("Growth"))
ann_text_leafgrwth4 <- data.frame(avg=1.5,Func_group_broad="Graminoid",lab = "Text",
                                  Var_type_broad = factor("Leaf_Growth"))
ann_text_aboven4 <- data.frame(avg=0.2,Func_group_broad="Graminoid",lab = "Text",
                               Var_type_broad = factor("Nitrogen_above"))
ann_text_belown4 <- data.frame(avg=1.4,Func_group_broad="Graminoid",lab = "Text",
                               Var_type_broad = factor("Nitrogen_below"))
ann_text_perc4 <- data.frame(avg=1.2,Func_group_broad="Graminoid",lab = "Text",
                             Var_type_broad = factor("Percent_cover"))
ann_text_spring4 <- data.frame(avg=0.4,Func_group_broad="Graminoid",lab = "Text",
                               Var_type_broad = factor("Phen_early"))
ann_text_fall4 <- data.frame(avg=1.4,Func_group_broad="Graminoid",lab = "Text",
                             Var_type_broad = factor("Phen_late"))
ann_text_flwrlife4 <- data.frame(avg=-0.6,Func_group_broad="Graminoid",lab = "Text",
                                 Var_type_broad = factor("Phen_flwr_lifespan"))
# Forb
ann_text_abovebio5 <- data.frame(avg=1.2,Func_group_broad="Forb",lab = "Text",
                                 Var_type_broad = factor("Biomass_above"))
ann_text_flwrnum5 <- data.frame(avg=1,Func_group_broad="Forb",lab = "Text",
                                Var_type_broad = factor("Flower_num"))
ann_text_fruitnum5 <- data.frame(avg=1.4,Func_group_broad="Forb",lab = "Text",
                                 Var_type_broad = factor("Fruit_num"))
ann_text_fruitweight5 <- data.frame(avg=2.5,Func_group_broad="Forb",lab = "Text",
                                    Var_type_broad = factor("Fruit_weight"))
ann_text_plantgrwth5 <- data.frame(avg=2.2,Func_group_broad="Forb",lab = "Text",
                                   Var_type_broad = factor("Growth"))
ann_text_leafgrwth5 <- data.frame(avg=1.4,Func_group_broad="Forb",lab = "Text",
                                  Var_type_broad = factor("Leaf_Growth"))
ann_text_aboven5 <- data.frame(avg=0.6,Func_group_broad="Forb",lab = "Text",
                               Var_type_broad = factor("Nitrogen_above"))
ann_text_perc5 <- data.frame(avg=1,Func_group_broad="Forb",lab = "Text",
                             Var_type_broad = factor("Percent_cover"))
ann_text_spring5 <- data.frame(avg=0.6,Func_group_broad="Forb",lab = "Text",
                               Var_type_broad = factor("Phen_early"))
ann_text_fall5 <- data.frame(avg=0.6,Func_group_broad="Forb",lab = "Text",
                             Var_type_broad = factor("Phen_late"))
ann_text_flwrlife5 <- data.frame(avg=-0.6,Func_group_broad="Forb",lab = "Text",
                                 Var_type_broad = factor("Phen_flwr_lifespan"))
# Bryophyte
ann_text_abovebio6 <- data.frame(avg=3.6,Func_group_broad="Bryophyte",lab = "Text",
                                 Var_type_broad = factor("Biomass_above"))
ann_text_fruitnum6 <- data.frame(avg=1.4,Func_group_broad="Bryophyte",lab = "Text",
                                 Var_type_broad = factor("Fruit_num"))
ann_text_plantgrwth6 <- data.frame(avg=1.2,Func_group_broad="Bryophyte",lab = "Text",
                                   Var_type_broad = factor("Growth"))
ann_text_aboven6 <- data.frame(avg=1.1,Func_group_broad="Bryophyte",lab = "Text",
                               Var_type_broad = factor("Nitrogen_above"))
ann_text_perc6 <- data.frame(avg=0.6,Func_group_broad="Bryophyte",lab = "Text",
                             Var_type_broad = factor("Percent_cover"))
# Total community
ann_text_abovebio7 <- data.frame(avg=3,Func_group_broad="Total",lab = "Text",
                                 Var_type_broad = factor("Biomass_above"))
ann_text_perc7 <- data.frame(avg=1,Func_group_broad="Total",lab = "Text",
                             Var_type_broad = factor("Percent_cover"))
# changing order of facets
esmd_func$Var_type_broad <- factor(esmd_func$Var_type_broad,
                                    levels=c("Biomass_above", "Biomass_below", "Percent_cover",
                                             "Flower_num","Fruit_weight","Growth",
                                             "Leaf_growth","Nitrogen_above","Phen_early","Phen_late",
                                             "Phen_flwr_lifespan"))

png("effect_func.png", units="in", width=8, height=8, res=300)
ggplot(esmd_func2, aes(y = Func_group_broad, x = avg)) +
  facet_wrap(.~Var_type_broad, scales="free_x", labeller = as_labeller(var_labels)) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", cex = 1, alpha = 0.7) +
  geom_point(aes(shape = sig), size = 4) +  
  scale_shape_manual(values = c(1,16)) +
  geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper), height = 0.25) +
  #scale_y_continuous(name = "", breaks=1:4, labels = dat$label, trans = "reverse") +
  xlab("Mean effect size (Hedges' g)") + 
  ylab(" ") + 
  #geom_text(data = ann_text_abovebio,label = "(18)",size=3.3) +
  #geom_text(data = ann_text_aboven,label = "(8)",size=3.3) +
  #geom_text(data = ann_text_plantgrwth,label = "(29)",size=3.3) +
  #geom_text(data = ann_text_abovebio2,label = "(55)",size=3.3) +
  #geom_text(data = ann_text_flwrnum2,label = "(20)",size=3.3) +
  #geom_text(data = ann_text_fruitweight2,label = "(10)",size=3.3) +
  #geom_text(data = ann_text_plantgrwth2,label = "(38)",size=3.3) +
  #geom_text(data = ann_text_leafgrwth2,label = "(48)",size=3.3) +
  #geom_text(data = ann_text_aboven2,label = "(59)",size=3.3) +
  #geom_text(data = ann_text_perc2,label = "(32)",size=3.3) +
  #geom_text(data = ann_text_spring2,label = "(35)",size=3.3) +
  #geom_text(data = ann_text_fall2,label = "(8)",size=3.3) +
  #geom_text(data = ann_text_aboven3,label = "(11)",size=3.3) +
  #geom_text(data = ann_text_perc3,label = "(25)",size=3.3) +
  #geom_text(data = ann_text_abovebio4,label = "(30)",size=3.3) +
  #geom_text(data = ann_text_flwrnum4,label = "(32)",size=3.3) +
  #geom_text(data = ann_text_plantgrwth4,label = "(32)",size=3.3) +
  #geom_text(data = ann_text_leafgrwth4,label = "(58)",size=3.3) +
  #geom_text(data = ann_text_aboven4,label = "(29)",size=3.3) +
  #geom_text(data = ann_text_perc4,label = "(55)",size=3.3) +
  #geom_text(data = ann_text_spring4,label = "(43)",size=3.3) +
  #geom_text(data = ann_text_fall4,label = "(21)",size=3.3) +
  #geom_text(data = ann_text_abovebio5,label = "(19)",size=3.3) +
  #geom_text(data = ann_text_flwrnum5,label = "(27)",size=3.3) +
  #geom_text(data = ann_text_fruitweight5,label = "(14)",size=3.3) +
  #geom_text(data = ann_text_plantgrwth5,label = "(19)",size=3.3) +
  #geom_text(data = ann_text_leafgrwth5,label = "(32)",size=3.3) +
  #geom_text(data = ann_text_aboven5,label = "(15)",size=3.3) +
  #geom_text(data = ann_text_perc5,label = "(31)",size=3.3) +
  #geom_text(data = ann_text_spring5,label = "(89)",size=3.3) +
  #geom_text(data = ann_text_fall5,label = "(42)",size=3.3) +
  #geom_text(data = ann_text_abovebio6,label = "(9)",size=3.3) +
  #geom_text(data = ann_text_perc6,label = "(27)",size=3.3) +
  #geom_text(data = ann_text_abovebio7,label = "(22)",size=3.3) +
  #geom_text(data = ann_text_perc7,label = "(14)",size=3.3) +
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_line(),
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 12, colour = "black"),
        axis.text.x.bottom = element_text(size = 12, colour = "black"),
        axis.title.x = element_text(size = 12, colour = "black"),
        legend.position = "none",
        strip.text = element_text(face = "bold"))
dev.off()



### effect size for variables sorted by native/non-native ###
esmd_native <- esmd_clean2 %>%
  filter(!(Native_Status == "")) %>%
  #filter(!(Var_type_broad == "Nitrogen_above" | Var_type_broad == "Nitrogen_below" |
  #           Var_type_broad == "Fruit_num" | Var_type_broad == "Biomass_below" |
  #           Var_type_broad == "Fruit_weight" | Var_type_broad == "Growth" |
  #           Var_type_broad == "Percent_cover")) %>%
  group_by(Var_type_broad, Native_Status) %>%
  summarize(count = n(),
            avg = mean(yi, na.rm = TRUE),
            se = std.error(yi, na.rm = TRUE),
            CI_lower = avg - (1.96 * se),
            CI_upper = avg + (1.96 * se))

esmd_native2 <- data.frame(Var_type_broad = c("Biomass_above","Biomass_above","Flower_num","Flower_num",
                                              "Leaf_growth","Leaf_growth","Phen_early","Phen_early",
                                              "Phen_flwr_lifespan","Phen_flwr_lifespan","Phen_late","Phen_late"),
                           Native_Status = c("Native", "Non-native","Native", "Non-native",
                                             "Native", "Non-native","Native", "Non-native",
                                             "Native", "Non-native","Native", "Non-native"),
                           sig = c("yes","no","no","no","yes","yes","yes","yes","no","no","no","no"),
                           avg = c(0.3864,0.0657,
                                   0.0531,-0.0742,
                                   0.3309,0.3483,
                                   -0.5621,-0.5321,
                                   -0.4361,-0.2833,
                                   -0.7009,-0.8636),
                           CI_upper = c(0.7545,0.5713,
                                        0.2515,0.3496,
                                        0.5007,0.6435,
                                        -0.2115,-0.1227,
                                        0.5172,0.757,
                                        0.5326,0.424),
                           CI_lower = c(0.0183,-0.4398,
                                        -0.1453,-0.4981,
                                        0.1611,0.0531,
                                        -0.9126,-0.9414,
                                        -1.3894,-1.3236,
                                        -1.9344,-2.1512))
png("effect_native.png", units="in", width=8, height=6, res=300)
ggplot(esmd_native2, aes(y = Native_Status, x = avg)) +
  facet_wrap(.~Var_type_broad, labeller = as_labeller(var_labels)) +
  geom_point(aes(shape = sig), size = 4) +  
  scale_shape_manual(values=c(1,16)) + 
  geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper), height = 0.25) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", cex = 1, alpha = 0.5) +
  #scale_y_continuous(name = "", breaks=1:4, labels = dat$label, trans = "reverse") +
  xlab("Mean effect size (Hedges' g) +/- SE") + 
  ylab(" ") + 
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.title = element_text(size=15),
        axis.text.y = element_text(size=14),
        axis.text.x = element_text(size=12),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=15),
        legend.position="none",
        strip.text = element_text(face = "bold", size=9))
dev.off()



### effect size based on year round warming (yes or no) ###
esmd_yearround <- esmd_clean2 %>%
  filter(!(Var_type_broad == "Phen_flwr_lifespan")) %>% # remove vars that don't contain both categories
  group_by(Var_type_broad, Year_round_warm) %>%
  summarize(count = n(),
            avg = mean(yi, na.rm = TRUE),
            se = std.error(yi, na.rm = TRUE),
            CI_lower = avg - (1.96 * se),
            CI_upper = avg + (1.96 * se))
# making dataframe for mean estimated effect sizes from model output (in otc_effectsize_analyses_L2.R)
# only traits w/ differences between catergories
esmd_yearround2 <- data.frame(Var_type_broad = c("Biomass_above","Biomass_above","Biomass_below","Biomass_below",
                                                 "Fruit_num","Fruit_num","Fruit_weight","Fruit_weight",
                                                 "Nitrogen_above","Nitrogen_above","Phen_late","Phen_late"),
                              Year_round_warm = c("No","Yes","No","Yes","No","Yes","No","Yes","No","Yes","No","Yes"),
                              sig = c("no","yes","no","yes","yes","no","no","yes","no","yes","yes","no"),
                              avg = c(-0.0072,0.6537,
                                      -0.2839,1.0570,
                                      0.6338,0.1273,
                                      0.1372,0.9447,
                                      -0.0867,-0.5187,
                                      -1.7047,0.334),
                              CI_upper = c(0.6259,1.0433,
                                           1.7913,1.8104,
                                           1.3335,0.5634,
                                           1.901,1.7215,
                                           0.3493,-0.2498,
                                           -0.1473,1.8501),
                              CI_lower = c(-0.6403,0.2641,
                                           -2.359,0.3036,
                                           -0.0659,-0.3087,
                                           -1.6266,0.1678,
                                           -0.5227,-0.7876,
                                           -3.2621,-1.182))
# all traits
esmd_yearround3 <- data.frame(Var_type_broad = c("Biomass_above","Biomass_above","Biomass_below","Biomass_below","Flower_num","Flower_num",
                                                 "Fruit_num","Fruit_num","Fruit_weight","Fruit_weight","Growth","Growth",
                                                "Leaf_growth","Leaf_growth","Nitrogen_above","Nitrogen_above","Nitrogen_below","Nitrogen_below",
                                                "Percent_cover","Percent_cover","Phen_early","Phen_early","Phen_late","Phen_late"),
                              Year_round_warm = c("No","Yes","No","Yes","No","Yes","No","Yes","No","Yes","No","Yes",
                                                  "No","Yes","No","Yes","No","Yes","No","Yes","No","Yes","No","Yes"),
                              sig = c("no","yes","no","yes","no","no",
                                      "yes","no","no","yes","yes","yes",
                                      "yes","yes","no","yes","no","no",
                                      "no","no","yes","yes","yes","no"),
                              avg = c(-0.0072,0.6537,
                                      -0.2839,1.0570,
                                      0.1012,-0.0315,
                                      0.6338,0.1273,
                                      0.1372,0.9447,
                                      0.7502,0.6375,
                                      0.3742,0.3066,
                                      -0.0867,-0.5187,
                                      -0.6438,0.0654,
                                      0.0227,0.002,
                                      -0.4216,-0.6886,
                                      -1.7047,0.334),
                              CI_upper = c(0.6259,1.0433,
                                           1.7913,1.8104,
                                           0.361,0.2064,
                                           1.3335,0.5634,
                                           1.901,1.7215,
                                           1.1668,0.9419,
                                           0.7356,0.4813,
                                           0.3493,-0.2498,
                                           1.9124,1.2494,
                                           0.1696,0.165,
                                           0.0654,-0.208,
                                           -0.1473,1.8501),
                              CI_lower = c(-0.6403,0.2641,
                                           -2.359,0.3036,
                                           -0.1586,-0.2694,
                                           -0.0659,-0.3087,
                                           -1.6266,0.1678,
                                           0.3335,0.333,
                                           0.0127,0.1319,
                                           -0.5227,-0.7876,
                                           -3.2,-1.1187,
                                           -0.1242,-0.1609,
                                           -0.9085,-1.1693,
                                           -3.2621,-1.182))
# making text labels for sample sizes
# year round warming
ann_text_abovebio <- data.frame(avg=1.7,Year_round_warm="Yes",lab = "Text",
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
                               Var_type_broad = factor("Leaf_growth"))
ann_text_aboven <- data.frame(avg=0.2,Year_round_warm="Yes",lab = "Text",
                               Var_type_broad = factor("Nitrogen_above"))
ann_text_belown <- data.frame(avg=1,Year_round_warm="Yes",lab = "Text",
                               Var_type_broad = factor("Nitrogen_below"))
ann_text_perc <- data.frame(avg=0.8,Year_round_warm="Yes",lab = "Text",
                               Var_type_broad = factor("Percent_cover"))
ann_text_spring <- data.frame(avg=0.4,Year_round_warm="Yes",lab = "Text",
                               Var_type_broad = factor("Phen_early"))
ann_text_fall <- data.frame(avg=1,Year_round_warm="Yes",lab = "Text",
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
                                 Var_type_broad = factor("Leaf_growth"))
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
# changing order of facets
esmd_yearround$Var_type_broad <- factor(esmd_yearround$Var_type_broad,
                                   levels=c("Biomass_above","Biomass_below","Percent_cover",
                                            "Flower_num","Fruit_num","Fruit_weight",
                                            "Growth","Leaf_growth","Nitrogen_above",
                                            "Nitrogen_below","Phen_early","Phen_late"))
esmd_yearround2$Var_type_broad <- factor(esmd_yearround2$Var_type_broad,
                                        levels=c("Biomass_above","Biomass_below",
                                                 "Fruit_weight",
                                                 "Nitrogen_above",
                                                 "Fruit_num", "Phen_late"))

png("effect_yearround_all.png", units="in", width=8, height=6, res=300)
ggplot(esmd_yearround3, aes(y = Year_round_warm, x = avg)) +
  facet_wrap(.~Var_type_broad, labeller = as_labeller(var_labels)) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", cex = 1, alpha = 0.7) +
  geom_point(aes(shape = sig), size = 4) +  
  scale_shape_manual(values=c(1,16)) +
  geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper), height = 0.25) +
  scale_y_discrete(labels=c("Yes" = "Year-round warming",
                            "No" = "Seasonal warming")) +
  xlab("Mean effect size (Hedges' g)") + 
  ylab(" ") + 
  #geom_text(data = ann_text_abovebio,label = "(120)",size=3.5) +
  #geom_text(data = ann_text_belowbio,label = "(42)",size=3.5) +
  #geom_text(data = ann_text_aboven,label = "(105)",size=3.5) +
  #geom_text(data = ann_text_belown,label = "(11)",size=3.5) +
  #geom_text(data = ann_text_fall,label = "(43)",size=3.5) +
  #geom_text(data = ann_text_flwrnum,label = "(49)",size=3.5) +
  #geom_text(data = ann_text_fruitnum,label = "(25)",size=3.5) +
  #geom_text(data = ann_text_fruitweight,label = "(24)",size=3.5) +
  #geom_text(data = ann_text_leafgrwth,label = "(112)",size=3.5) +
  #geom_text(data = ann_text_perc,label = "(102)",size=3.5) +
  #geom_text(data = ann_text_plantgrwth,label = "(77)",size=3.5) +
  #geom_text(data = ann_text_spring,label = "(90)",size=3.5) +
  #geom_text(data = ann_text_abovebio2,label = "(49)",size=3.5) +
  #geom_text(data = ann_text_belowbio2,label = "(2)",size=3.5) +
  #geom_text(data = ann_text_aboven2,label = "(26)",size=3.5) +
  #geom_text(data = ann_text_belown2,label = "(1)",size=3.5) +
  #geom_text(data = ann_text_fall2,label = "(31)",size=3.5) +
  #geom_text(data = ann_text_flwrnum2,label = "(30)",size=3.5) +
  #geom_text(data = ann_text_fruitnum2,label = "(9)",size=3.5) +
  #geom_text(data = ann_text_fruitweight2,label = "(3)",size=3.5) +
  #geom_text(data = ann_text_leafgrwth2,label = "(32)",size=3.5) +
  #geom_text(data = ann_text_perc2,label = "(91)",size=3.5) +
  #geom_text(data = ann_text_plantgrwth2,label = "(58)",size=3.5) +
  #geom_text(data = ann_text_spring2,label = "(82)",size=3.5) +
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 12, colour = "black"),
        axis.text.x.bottom = element_text(size = 12, colour = "black"),
        axis.title.x = element_text(size = 12, colour = "black"),
        legend.position = "none",
        strip.text = element_text(face = "bold", size=9))
dev.off()



### effect size based on latitude of study ###
esmd_lat_trim <- esmd_clean2 %>% # selecting traits/properties that had an effect
  filter(Var_type_broad == "Flower_num" |
           Var_type_broad == "Fruit_num" |
           Var_type_broad == "Fruit_weight" |
           Var_type_broad == "Phen_late" |
           Var_type_broad == "Nitrogen_below" |
           Var_type_broad == "Phen_early")
png("effect_lat.png", units="in", width=8, height=6, res=300)
lat_plot <- ggplot(esmd_lat_trim, aes(x = Abs_Latitude, y = yi)) +
  facet_wrap(.~Var_type_broad, scales="free_y",labeller = as_labeller(var_labels), ncol=2) +
  geom_point(size = 1) +
  geom_smooth(method = 'lm',color="darkred") +
  labs(x = NULL, y = NULL, title="A") +
  theme_bw() +
  #scale_color_manual(values=c("Rep" = "#663333",
  #                            "Phen" = "#222255",
  #                            "N" = "#666633")) +
  #ylim(-5,5) +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.title = element_text(size=15),
        axis.text = element_text(size=11),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15),
        legend.position="none",
        strip.text = element_text(face = "bold", size=9))
dev.off()



### effect size based on elevation ###
esmd_elev_trim <- esmd_clean2 %>% # selecting traits/properties that had an effect
  filter(Var_type_broad == "Flower_num" |
           Var_type_broad == "Fruit_num" |
           Var_type_broad == "Fruit_weight" |
           Var_type_broad == "Phen_early" |
           Var_type_broad == "Nitrogen_below")
png("effect_elev.png", units="in", width=8, height=6, res=300)
elev_plot <- ggplot(esmd_elev_trim, aes(x = Elevation_m, y = yi)) +
  facet_wrap(.~Var_type_broad, scales="free_y",labeller = as_labeller(var_labels), ncol=2) +
  geom_point(size = 1) +
  geom_smooth(method = 'lm',color="darkred") +
  labs(x = NULL, y = NULL, title="B") +
  theme_bw() +
  #scale_color_manual(values=c("Rep" = "#663333",
  #                            "Phen" = "#222255",
  #                            "N" = "#666633")) +
  #ylim(-5,5) +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.title = element_text(size=15),
        axis.text = element_text(size=11),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15),
        legend.position="none",
        strip.text = element_text(face = "bold", size=9))
dev.off()

### combining lat and elev ###
lat_elev_merge <- ggpubr::ggarrange(lat_plot,elev_plot,
                                    ncol = 2, common.legend=T,legend="none")
png("effect_lat_elev.png", units="in", width=10.25, height=7, res=300)
annotate_figure(lat_elev_merge,
                left = text_grob("Effect size", color = "black", rot = 90, size=15),
                bottom = text_grob("Latitude (°)                                                        Elevation (m)", color = "black", size=15))
dev.off()



### effect size based on mean annual temperature ###
png("effect_temp.png", units="in", width=8, height=6, res=300)
ggplot(esmd_clean2, aes(x = Mean_annual_temp, y = yi)) +
  facet_wrap(.~Var_type_broad, scales="free",labeller = as_labeller(var_labels)) +
  geom_point(size = 1) +
  geom_smooth(method = 'lm',color="darkred") +
  xlab("Mean annual temperature (°C)") +
  ylab("Effect size") +
  #scale_color_manual(values=c("Rep" = "#663333",
  #                            "Phen" = "#222255",
  #                            "N" = "#666633")) +
  #ylim(-5,5) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15),
        legend.position="none",
        strip.text = element_text(face = "bold", size=10))
dev.off()



### effect size based on mean annual precipitation ###
png("effect_precip.png", units="in", width=8, height=6, res=300)
ggplot(esmd_clean2, aes(x = Mean_annual_precip, y = yi)) +
  facet_wrap(.~Var_type_broad, scales="free",labeller = as_labeller(var_labels)) +
  geom_point(size = 1) +
  geom_smooth(method = 'lm',color="darkred") +
  xlab("Mean annual precipitation (mm)") +
  ylab("Effect size") +
  #scale_color_manual(values=c("Rep" = "#663333",
  #                            "Phen" = "#222255",
  #                            "N" = "#666633")) +
  #ylim(-5,5) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15),
        legend.position="none",
        strip.text = element_text(face = "bold", size=10))
dev.off()



### effect size based on distance from range edge ###
esmd_lat_diff_trim <- esmd_clean2 %>% # testing if we restrict latitude difference to remove large outlier values
  filter(!(Lat_difference > 60))
png("effect_latdiff.png", units="in", width=8, height=6, res=300)
ggplot(esmd_clean2, aes(x = Lat_difference, y = yi)) +
  facet_wrap(.~Var_type_broad, labeller = as_labeller(var_labels), scales="free") +
  geom_point(size = 1) +
  geom_smooth(method = 'lm', color = "darkred") +
  xlab("Distance from range edge (°)") +
  ylab("Effect size") +
  #ylim(-5,5) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15),
        strip.text = element_text(face = "bold", size=10))
dev.off()



### effect size based on amount warmed ###
esmd_amount <- esmd_clean2 %>%
  filter(!(Amount_warmed_type == "" |
             Amount_warmed_type == "Entire_average" |
             Amount_warmed_type == "Annual_average" |
             Amount_warmed_type == "Monthly_average"))
png("effect_amountw.png", units="in", width=8, height=6, res=300)
ggplot(esmd_amount, aes(x = Amount_warmed_C, y = yi)) +
  facet_wrap(.~Var_type_broad, labeller = as_labeller(var_labels), scales="free") +
  geom_point(size = 1) +
  geom_smooth(method = 'lm', color = "darkred") +
  xlab("Amount warmed (°C)") +
  ylab("Effect size") +
  #ylim(-10,10) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15),
        strip.text = element_text(face = "bold", size=10))
dev.off()



### effect size based on # months warmed ###
esmd_months_warm <- esmd_clean2 %>%
  filter(!(Years_warmed >10))
esmd_months_poster <- esmd_clean2 %>%
  filter(Var_type_broad == "Fruit_num" |
           Var_type_broad == "Leaf_Growth" |
           Var_type_broad == "Nitrogen_above" |
           Var_type_broad == "Phen_early")

png("effect_yearsw.png", units="in", width=8, height=6, res=300)
ggplot(esmd_clean2, aes(x = Years_warmed, y = yi)) +
  facet_wrap(.~Var_type_broad, labeller = as_labeller(var_labels), scales="free") +
  geom_point(size = 1) +
  geom_smooth(method = 'lm', color="darkred") +
  xlab("Number of years warmed") +
  ylab("Effect size") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15),
        legend.position="none",
        strip.text = element_text(face = "bold", size=10))
dev.off()



### funnel plot ###
# https://www.metafor-project.org/doku.php/plots:funnel_plot_variations
### fit equal-effects model
res <- rma(yi, vi, data=esmd_clean, measure="SMD", method="EE")
### draw funnel plots
png("funnels.png", units="in", width=7, height=6, res=300)
par(mfrow=c(2,2))
funnel(res, main="Standard Error")
funnel(res, yaxis="vi", main="Sampling Variance")
funnel(res, yaxis="seinv", main="Inverse Standard Error")
funnel(res, yaxis="vinv", main="Inverse Sampling Variance")
dev.off()

# correlation test between effect size and sample size
res <- cor.test(esmd_clean$Warmed_N+esmd_clean$Ambient_N, esmd_clean$yi, 
                method = "pearson")
res
ggplot(esmd_clean, aes(x = Ambient_N, y = yi)) +
  geom_point() +
  geom_smooth(method = 'lm')



###### total # of each growth form measured ###########
esmd_total_growth <- esmd_clean2 %>%
  group_by(Func_group_broad) %>%
  summarize(count = n()) %>%
  filter(!(Func_group_broad == ""))
png("effect_total_growth.png", units="in", width=7, height=6, res=300)
ggplot(esmd_total_growth, aes(y = reorder(Func_group_broad, -count), x = count)) +
  geom_bar(stat = "identity") +
  xlab("Total sample size") + 
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



############ comparing variables that were grouped ###############
### effect sizes for evergreen vs. deciduous shrubs
esmd_func_ever_decid <- esmd_clean2 %>%
  group_by(Var_type_broad, Func_group) %>%
  summarize(count = n(),
            avg = mean(yi, na.rm = TRUE),
            se = std.error(yi, na.rm = TRUE),
            CI_lower = avg - (1.96 * se),
            CI_upper = avg + (1.96 * se)) %>%
  filter(Func_group == "Ever_Shrub" | Func_group == "Decid_Shrub")
png("effect_decid_ever_shrub.png", units="in", width=8, height=6, res=300)
ggplot(esmd_func_ever_decid, aes(y = Func_group, x = avg)) +
  facet_wrap(.~Var_type_broad) +
  geom_point(shape = 18, size = 4) +  
  geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper), height = 0.25) +
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



### effect sizes for evergreen vs. deciduous trees
esmd_func_ever_decid_tree <- esmd_clean2 %>%
  group_by(Var_type_broad, Func_group) %>%
  summarize(count = n(),
            avg = mean(yi, na.rm = TRUE),
            se = std.error(yi, na.rm = TRUE),
            CI_lower = avg - (1.96 * se),
            CI_upper = avg + (1.96 * se)) %>%
  filter(Func_group == "Ever_Tree" | Func_group == "Decid_Tree")
png("effect_decid_ever_tree.png", units="in", width=8, height=6, res=300)
ggplot(esmd_func_ever_decid_tree, aes(y = Func_group, x = avg)) +
  facet_wrap(.~Var_type_broad) +
  geom_point(shape = 18, size = 4) +  
  geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper), height = 0.25) +
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



### effect sizes for forbs vs. leguminous forbs
esmd_func_forbs <- esmd_clean2 %>%
  group_by(Var_type_broad, Func_group) %>%
  summarize(count = n(),
            avg = mean(yi, na.rm = TRUE),
            se = std.error(yi, na.rm = TRUE),
            CI_lower = avg - (1.96 * se),
            CI_upper = avg + (1.96 * se)) %>%
  filter(Func_group == "Forb" | Func_group == "Legume_Forb")
png("effect_forbs.png", units="in", width=8, height=6, res=300)
ggplot(esmd_func_forbs, aes(y = Func_group, x = avg)) +
  facet_wrap(.~Var_type_broad) +
  geom_point(shape = 18, size = 4) +  
  geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper), height = 0.25) +
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



### effect sizes for spring phenophases
esmd_spring_pheno <- esmd_clean2 %>%
  group_by(Var_type) %>%
  summarize(count = n(),
            avg = mean(yi, na.rm = TRUE),
            se = std.error(yi, na.rm = TRUE),
            CI_lower = avg - (1.96 * se),
            CI_upper = avg + (1.96 * se)) %>%
  filter(Var_type == "Phen_leaf_appear" |
           Var_type == "Phen_leaf_expand" |
           Var_type == "Phen_emergence" |
           Var_type == "Phen_bud_break" |
           Var_type == "Phen_flwr" |
           Var_type == "Phen_stem_elong")
png("effect_spring.png", units="in", width=8, height=6, res=300)
ggplot(esmd_spring_pheno, aes(y = Var_type, x = avg)) +
  geom_point(shape = 18, size = 4) +  
  geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper), height = 0.25) +
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



### effect sizes for fall phenophases
esmd_fall_pheno <- esmd_clean2 %>%
  group_by(Var_type) %>%
  summarize(count = n(),
            avg = mean(yi, na.rm = TRUE),
            se = std.error(yi, na.rm = TRUE),
            CI_lower = avg - (1.96 * se),
            CI_upper = avg + (1.96 * se)) %>%
  filter(Var_type == "Phen_senes" |
           Var_type == "Phen_abscission" |
           Var_type == "Phen_seed_set")
png("effect_fall.png", units="in", width=8, height=6, res=300)
ggplot(esmd_fall_pheno, aes(y = Var_type, x = avg)) +
  geom_point(shape = 18, size = 4) +  
  geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper), height = 0.25) +
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



### effect sizes for growth
esmd_growth <- esmd_clean2 %>%
  group_by(Var_type) %>%
  summarize(count = n(),
            avg = mean(yi, na.rm = TRUE),
            se = std.error(yi, na.rm = TRUE),
            CI_lower = avg - (1.96 * se),
            CI_upper = avg + (1.96 * se)) %>%
  filter(Var_type == "Height" |
           Var_type == "Shoot_length")
png("effect_growth.png", units="in", width=8, height=6, res=300)
ggplot(esmd_growth, aes(y = Var_type, x = avg)) +
  geom_point(shape = 18, size = 4) +  
  geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper), height = 0.25) +
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



### effect sizes for leaf growth
esmd_leaf_growth <- esmd_clean2 %>%
  group_by(Var_type) %>%
  summarize(count = n(),
            avg = mean(yi, na.rm = TRUE),
            se = std.error(yi, na.rm = TRUE),
            CI_lower = avg - (1.96 * se),
            CI_upper = avg + (1.96 * se)) %>%
  filter(Var_type == "SLA" |
           Var_type == "Leaf_area" |
           Var_type == "Leaf_width" |
           Var_type == "Leaf_length")
png("effect_leaf_growth.png", units="in", width=8, height=6, res=300)
ggplot(esmd_leaf_growth, aes(y = Var_type, x = avg)) +
  geom_point(shape = 18, size = 4) +  
  geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper), height = 0.25) +
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



### seasonally vs. year-warmed study locations ###
# checking to see if there is a latitudinal split between year-round vs. seasonally warmed studies
png("map_yearroundwarm.png", units="in", width=10, height=6, res=300)
ggplot() +
  geom_map(
    data = world, map = world,
    aes(long,lat,map_id = region),
    color = "lightgrey", fill = "darkgrey", size = 0.1
  ) +
  geom_point(
    data = esmd_clean2,
    aes(Longitude, Latitude, color=Year_round_warm),
    alpha = 0.7,
    size=2.5
  ) +
  scale_color_manual(values = c("steelblue","orange")) +
  theme_classic() +
  labs(x = "Longitude",y = "Latitude") + 
  theme(axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14))
dev.off()
# no, they both seem to have a good range of latitudes
# slightly more seasonally warmed studies at higher latitudes, though
