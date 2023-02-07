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


#############################
### DIBELS
#############################

mean      <- readxl::read_excel("data/dibels8_educ_data_deid.xlsx",
                           sheet=1) %>%
              rename(y1_boy_mean = y1_boy,
                    y1_moy_mean = y1_moy,
                    y2_boy_mean = y2_boy,
                    y2_moy_mean = y2_moy)
intensive <- readxl::read_excel("data/dibels8_educ_data_deid.xlsx",
                                sheet=2) %>%
              select(sch_deid, grade, y1_boy, y1_moy, y2_boy, y2_moy) %>%
              rename(y1_boy_intense_prop = y1_boy,
                     y1_moy_intense_prior = y1_moy,
                     y2_boy_intense_prop = y2_boy,
                     y2_moy_intense_prop = y2_moy)
strategic <- readxl::read_excel("data/dibels8_educ_data_deid.xlsx",
                                sheet=3) %>%
              select(sch_deid, grade, y1_boy, y1_moy, y2_boy, y2_moy) %>%
              rename(y1_boy_strat_prop = y1_boy,
                     y1_moy_strat_prop = y1_moy,
                     y2_boy_strat_prop = y2_boy,
                     y2_moy_strat_prop = y2_moy)

dibels <- left_join(mean, intensive, by = c("sch_deid", "grade"))
dibels <- left_join(dibels, strategic, by=c("sch_deid", "grade"))

dibels <- dibels %>% mutate(across(c(a_ts, b_ts, h_ts ,w_ts),
                                   .fns = ~.x / tr_ts,
                                   .names = "{paste0({col},sept='_','prop')}"
            ))

dibels <- dibels %>% group_by(sch_deid) %>% mutate(school_enroll = sum(tr_ts))
dibels <- dibels %>% mutate(frpl_prop = (school_lunch_free + school_lunch_reduced)/school_enroll)

dibels <- dibels %>% select(-c(tab, subtest, summary, nces_year, school_virtual, lea_idea_count, lea_lep_count, school_lunch_free, school_lunch_reduced, school_lunch_missing, school_lunch_nocode, 
                               school_lunch_na, aian_f:tr_us))

dibels <- filter(dibels, !is.na(y1_boy_mean) & !is.na(y1_moy_mean) & !is.na(y2_boy_mean) & !is.na(y2_moy_mean) & !is.na(school_titlei))

dibels <- filter(dibels, grade<6)

# Check how many missing
sapply(dibels, function(x) sum(is.na(x)))

# Create pre/post
dibels$pre <- rowMeans(dibels[ ,c("y1_boy_mean", "y1_moy_mean")], na.rm=T)
dibels$post <- rowMeans(dibels[ ,c("y2_boy_mean", "y2_moy_mean")], na.rm=T)

# Assign mean w/ noise
dibels <- ungroup(dibels)

# Asian
dibels <- dibels %>% mutate(asian_temp = rnorm(nrow(dibels), mean= mean(a_ts_prop, na.rm=T), sd= sd(a_ts_prop, na.rm=T)))
dibels <- dibels %>% mutate(asian_prop = case_when(!is.na(a_ts_prop) ~ a_ts_prop,
                                                   is.na(a_ts_prop) ~ asian_temp)) %>%
                     mutate(asian_prop = case_when(asian_prop<0 ~ 0,
                                                   TRUE ~ asian_prop))
# Black
dibels <- dibels %>% mutate(black_temp = rnorm(nrow(dibels), mean= mean(b_ts_prop, na.rm=T), sd= sd(b_ts_prop, na.rm=T)))
dibels <- dibels %>% mutate(black_prop = case_when(!is.na(b_ts_prop) ~ b_ts_prop,
                                                   is.na(b_ts_prop) ~ black_temp)) %>%
                      mutate(black_prop = case_when(black_prop<0 ~ 0,
                                                    black_prop>1 ~ 1,
                                                    TRUE ~ black_prop))
# Hispanic
dibels <- dibels %>% mutate(hisp_temp = rnorm(nrow(dibels), mean= mean(h_ts_prop, na.rm=T), sd= sd(h_ts_prop, na.rm=T)))
dibels <- dibels %>% mutate(hisp_prop = case_when(!is.na(h_ts_prop) ~ h_ts_prop,
                                                   is.na(h_ts_prop) ~ hisp_temp)) %>%
                      mutate(hisp_prop = case_when(hisp_prop<0 ~ 0,
                                                   hisp_prop>1 ~ 1,
                                                   TRUE ~ hisp_prop))
# White
dibels <- dibels %>% mutate(white_temp = rnorm(nrow(dibels), mean= mean(w_ts_prop, na.rm=T), sd= sd(w_ts_prop, na.rm=T)))
dibels <- dibels %>% mutate(white_prop = case_when(!is.na(w_ts_prop) ~ w_ts_prop,
                                                   is.na(w_ts_prop) ~ white_temp)) %>%
                      mutate(white_prop = case_when(white_prop<0 ~ 0,
                                                    white_prop>1 ~ 1,
                                                    TRUE ~ white_prop))
# FRPL
dibels <- dibels %>% mutate(frpl_temp = rnorm(nrow(dibels), mean= mean(frpl_prop, na.rm=T), sd= sd(frpl_prop, na.rm=T)))
dibels <- dibels %>% mutate(frpl_prop = case_when(!is.na(frpl_prop) ~ frpl_prop,
                                                   is.na(frpl_prop) ~ frpl_temp)) %>%
                      mutate(frpl_prop = case_when(frpl_prop<0 ~ 0,
                                                    TRUE ~ frpl_prop))
# Enroll
dibels <- dibels %>% mutate(enroll_temp = round(rnorm(nrow(dibels), mean= mean(school_enroll, na.rm=T), sd= sd(school_enroll, na.rm=T)),0))
dibels <- dibels %>% mutate(school_enroll = case_when(!is.na(school_enroll) ~ school_enroll,
                                                  is.na(school_enroll) ~ enroll_temp)) %>%
                      mutate(school_enroll = case_when(school_enroll<26 ~ 25,
                                                    TRUE ~ school_enroll))

dibels <- select(dibels, -c(y1_boy_intense_prop:y2_moy_strat_prop, a_ts_prop:w_ts_prop, asian_temp, black_temp, hisp_temp, white_temp, frpl_temp, enroll_temp))

# Check how many missing  -- NONE!
sapply(dibels, function(x) sum(is.na(x)))

# Check average values
sapply(dibels, function(x) summary(x))


data.table::fwrite(dibels, "data/dibels.csv")

dibels_long <- dibels %>% 
                  pivot_longer(
                    cols = c("y1_boy_mean", "y1_moy_mean", "y2_boy_mean", "y2_moy_mean"),
                    names_to = "period",
                    names_pattern = "(.*)_mean",
                    values_to = "mean_orf")
dibels_long$period <- factor(dibels_long$period)

dibels_long <- dibels_long %>% select(-c(pre, post))

dibels_long <- dibels_long %>% mutate(post = ifelse(period=="y1_boy" | period=="y1_moy", 0, 1))

data.table::fwrite(dibels_long, "data/dibels_long.csv")

