########################
# data management

library(here)
library(tidyverse)

##################
## SEDA data
##################

seda <- read.csv(here("data/seda_geodist_poolsub_cs_4.1.csv"))
cov <- read.csv(here("data/seda_cov_geodist_pool_4.1.csv"))

seda <- select(seda, c(sedalea, sedaleaname, fips, stateabb, cs_mn_avg_mth_ol, cs_mn_avg_rla_ol)) %>%
  drop_na()

cov <- select(cov, c(sedalea, sedaleaname, fips, urban, suburb, town, rural, pernam, perasn, perhsp, perblk, perwht, perfl, perecd, perell, perspeced,
                     totenrl, rsflnfl, sesavgall, lninc50avgall, baplusavgall, unempavgall, snapavgall, single_momavgall)) %>%
  drop_na()

seda2 <- left_join(seda, cov, by = c("sedalea")) %>% drop_na()

seda2 <- seda2 %>% rename(sedaleaname = sedaleaname.x,
                          fips = fips.x) %>%
  select(-c(sedaleaname.y, fips.y))

seda2 <- seda2 %>% group_by(sedalea, sedaleaname, fips, stateabb) %>% 
  summarise(across(everything(), mean))

seda2 <- seda2 %>% rename(math = cs_mn_avg_mth_ol,
                          read = cs_mn_avg_rla_ol)

seda2 <- seda2 %>% mutate(urban2 = ifelse(urban>0.5, 1, 0))
seda2 <- seda2 %>% mutate(town2 = ifelse(town>0.5, 1, 0))
seda2 <- seda2 %>% mutate(suburb2 = ifelse(suburb>0., 1, 0))
seda2 <- seda2 %>% mutate(rural2 = ifelse(rural>0.5, 1, 0))

seda2 <- seda2 %>% mutate(locale = case_when(urban2==1 ~ "Urban", 
                                            suburb2==1 ~ "Suburb", 
                                            town2==1 ~ "Town", 
                                            rural2==1 ~ "Rural", 
                                            TRUE ~ NA_character_))

seda2 <- seda2 %>% mutate(locale = case_when(!is.na(locale) ~ locale,
                                             sedalea==408850 ~ "Rural",
                                             sedalea==1735400 ~ "Town",
                                             sedalea==4012240 ~ "Town",
                                             sedalea==4203840 ~ "Urban",
                                             sedalea==4501110 ~ "Town",
                                             sedalea==4832370 ~ "Urban"))

seda2 <- select(seda2, -c(urban, suburb, town, rural, urban2, suburb2, town2, rural2))

data.table::fwrite(seda2, "data/seda.csv")

###################
### NERDS data
###################

nerds <- readxl::read_excel(here("data/nerds_report_2019.xlsx"))

nerds <- nerds %>% mutate(ppe = case_when(pp_total_raw=="NRD" ~ NA_character_,
                                          TRUE ~ pp_total_raw))

nerds <- nerds %>% mutate(enroll = case_when(enroll_raw == "NRD" ~ NA_character_,
                                            TRUE ~ enroll_raw))

nerds$ppe <- as.numeric(as.character(nerds$ppe))
nerds$enroll <- as.numeric(as.character(nerds$enroll))

nerds <- nerds %>% mutate(ppe = case_when(ppe > 50000 ~ NA_real_,
                                          ppe < 2000 ~ NA_real_,
                                          TRUE ~ ppe))

sum(is.na(nerds$ppe))

nerds <- filter(nerds, !is.na(ppe) & enroll>=20 )

nerds <- select(nerds, -c(ncesdistid_admin, pp_fed_raw, pp_stloc_raw, pp_total_raw, pp_total_norm_NERDS, flag_nerds, flag_f33, schooltot_raw, ncesenroll, enroll_raw, enrollmetric_raw, gradespan))

nerds$level <- factor(nerds$level,
                      levels = c("0", "1", "2", "3", "4"), labels = c("Other", "Early Ed", "Elementary", "Middle", "High"))

nerds$ncesdistid_geo <- as.integer(as.character(nerds$ncesdistid_geo))

nerds <- left_join(nerds, cov, by = c("ncesdistid_geo" = "sedalea"))



nerds <- nerds %>% mutate(urban2 = ifelse(urban>0.5, 1, 0))
nerds <- nerds %>% mutate(town2 = ifelse(town>0.5, 1, 0))
nerds <- nerds %>% mutate(suburb2 = ifelse(suburb>0., 1, 0))
nerds <- nerds %>% mutate(rural2 = ifelse(rural>0.5, 1, 0))

nerds <- nerds %>% mutate(locale = case_when(urban2==1 ~ "Urban", 
                                             suburb2==1 ~ "Suburb", 
                                             town2==1 ~ "Town", 
                                             rural2==1 ~ "Rural", 
                                             TRUE ~ NA_character_))

nerds <- select(nerds, -c(sedaleaname, fips, urban, suburb, town, rural, urban2, town2, suburb2, rural2, 
                          pernam, perasn, perhsp, perblk, perwht, perecd, perell, perspeced, totenrl, rsflnfl, single_momavgall))

schools <- read.csv(here("data/or_schools.csv")) %>% select(-c(District.ID, School.ID))

nerds <- left_join(nerds, schools, by = c("schoolname" = "School.Name", "distname" = "District.Name"))

nerds$rand1 <- runif(1209, 94, 100)
nerds$rand2 <- runif(1209, 0, 6)

nerds <- nerds %>% mutate(frpl = case_when(Free.Reduced.Priced.Lunch==">95%" ~ rand1,
                                           Free.Reduced.Priced.Lunch=="<5%" ~ rand2,
                                           Free.Reduced.Priced.Lunch=="*" ~ NA_real_,
                                           TRUE ~ NA_real_))

nerds$frpl2 <- gsub("%$", "", nerds$Free.Reduced.Priced.Lunch)

nerds <- nerds %>% mutate(frpl = ifelse(is.na(frpl), frpl2, frpl))

nerds$frpl <- as.numeric(as.character(nerds$frpl))

nerds <- nerds %>% mutate(frpl = ifelse(is.na(frpl), perfl, frpl))
nerds <- nerds %>% mutate(frpl = frpl/100)

nerds <- select(nerds, -c(Free.Reduced.Priced.Lunch, perfl, rand1, rand2, frpl2, census_id))

nerds <- nerds %>% drop_na()

data.table::fwrite(nerds, "data/nerds.csv")

