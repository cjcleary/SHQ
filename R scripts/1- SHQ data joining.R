library(tidyverse)

mvic_dat <- read_csv("../Data Collection/SHQ_MVIC_results_2026-04-20.csv")
mvic_dat_wide <- mvic_dat %>% 
  pivot_wider(names_from = c("Leg", "Action", "Rep"),
              values_from = "MVIC_Nm")
mvic_dat_wide

descrips_dat <- read_csv("../Data Collection/SHQ_Participant_Characteristics.csv") %>% 
  mutate(Dom = ifelse(`Dominant Leg` == "L", "Left", "Right"))
descrips_dat

sl_drop_dat <- read_csv("../Data Collection/SHQ_SLDropLanding_results_2026-04-20.xlsx") %>% 
  separate(Trial, into = c("Leg", "Action", "Rep"),
           sep = "_") %>% 
  mutate(Leg = ifelse(Leg == "L", "Left", "Right")) %>% 
  select(-Action)

sl_drop_dat_wide <- sl_drop_dat %>% 
  pivot_wider(names_from = c("Leg", "Rep"),
              values_from = 4:6)

full_dat <- reduce(list(descrips_dat,
                        mvic_dat_wide,
                        sl_drop_dat_wide),
                   full_join, by = "ID")
full_dat

mvic_dat_mean <- mvic_dat %>% 
  group_by(ID, Leg, Action) %>% 
  summarize(MVIC_mean = mean(MVIC_Nm),
            .groups = 'drop') %>% 
  pivot_wider(names_from = c("Leg", "Action"),
              values_from = MVIC_mean)

sl_drop_mean <- sl_drop_dat %>% 
  group_by(ID, Leg) %>% 
  summarize(across(where(is.numeric), mean),
            .groups = 'drop') %>% 
  pivot_wider(names_from = Leg,
              values_from = 3:5,
              names_glue = c('{Leg}_{.value}'))

            
full_dat_average <- reduce(list(descrips_dat,
                                mvic_dat_mean,
                                sl_drop_mean),
                           full_join, by = "ID")
full_dat_average %>% 
  write.csv('../Data Collection/SHQ_AverageSLandMVICData_2026-04-20.csv',
            row.names = F, na = "")


full_dat_average

hq_dat <- full_dat_average %>% 
  select(ID, Sex, Dom, contains(c("KE", "KF"))) %>% 
  pivot_longer(cols = 4:7,
               names_to = c("Tested_Leg", "Action"),
               names_sep = "_",
               values_to = "MVIC") %>% 
  mutate(Leg = ifelse(Dom == Tested_Leg, "D", "ND"),
         .before = MVIC) %>% 
  pivot_wider(names_from = Action,
              values_from = MVIC) %>% 
  select(ID, Sex, Leg, KE:KF) %>% 
  mutate(HQ = KF/KE)
hq_dat

library(rstatix)
library(afex)
hq_sex_model <- aov_4(KE ~ Sex * (Leg | ID),
                      data = hq_dat,
                      anova_table = list(correction = 'none',
                                         es = 'pes'))
hq_sex_model

sl_contacts <- full_dat_average %>% 
  select(ID, Sex, Dom, contains("Flexion")) %>% 
  pivot_longer(cols = 4:7,
               names_to = c("Tested_Leg", "FL", "Timepoint", "deg"),
               values_to = "Degrees", 
               names_sep = "_") %>% 
  mutate(Leg = ifelse(Dom == Tested_Leg, "D", "ND")) %>% 
  select(ID, Sex, Leg, Timepoint, Degrees) %>% 
  pivot_wider(names_from = Timepoint,
              values_from = Degrees)
hq_dat

hq_sl_contacts <- left_join(hq_dat, sl_contacts)
hq_sl_contacts

dom_contact_model <- lm(Contact ~ Sex * HQ, data = hq_sl_contacts %>% 
                          filter(Leg == "D"))
summary(dom_contact_model)

nd_contact_model <- lm(Contact ~ Sex * HQ, data = hq_sl_contacts %>% 
                         filter(Leg == "ND"))

summary(nd_contact_model)

dom_peak_model <- lm(Peak ~ Sex * HQ, data = hq_sl_contacts %>% 
                       filter(Leg == "D"))
summary(dom_peak_model)

nd_contact_model <- lm(Peak ~ Sex * HQ, data = hq_sl_contacts %>% 
                       filter(Leg == "ND"))
nd_contact_model %>% summary()
