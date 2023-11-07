# TITLE:          Subsetting Collins et al. 2021 data
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Phoebe Zarnetske
# DATA INPUT:     Data from Collins et al.
# DATA OUTPUT:    Means, SD, and n (to input into my file)
# DATE:           Jan 2023


### Reading in RData files from Collins et al. paper (from their github)
load(file.path("R/L0/flowerdata.RData"))
load(file.path("R/L0/fruitdata.RData"))
load(file.path("R/L0/greenupdata.RData"))

### Subsetting out data for white mountains and niwot
# focused on these sites because we have some data from the other sites and don't want to pseudoreplicate
# flowering data
flower_whitemtn <- flower %>%
  filter(site_name == "WHITEMTNS") %>%
  filter(treatment == "CTL" | treatment == "OTC") %>%
  group_by(treatment, subsite, spp, year) %>%
  summarize(avg_flwr = mean(doy),
            sd = sd(doy),
            count = n())
flower_niwot <- flower %>%
  filter(site_name == "NIWOT") %>%
  filter(treatment == "CTL" | treatment == "OTC") %>%
  filter(phen_stage == "R2") %>% # flower open
  group_by(treatment, subsite, spp, year) %>%
  summarize(avg_flwr = mean(doy),
            sd = sd(doy),
            count = n()) %>%
  filter(year == 2019) # selecting latest year
# senescence data
sen_niwot <- sen %>%
  filter(site_name == "NIWOT") %>%
  filter(treatment == "CTL" | treatment == "OTC") %>%
  filter(phen_stage == "V4") %>% # 50% senescence
  group_by(treatment, subsite, spp, year) %>%
  summarize(avg_sen = mean(doy),
            sd = sd(doy),
            count = n())
# green-up
green_niwot <- green %>%
  filter(site_name == "NIWOT") %>%
  filter(treatment == "CTL" | treatment == "OTC") %>% # start of vegetative growth
  group_by(treatment, subsite, spp, year) %>%
  summarize(avg_green = mean(doy),
            sd = sd(doy),
            count = n()) %>%
  filter(year == 2019)

