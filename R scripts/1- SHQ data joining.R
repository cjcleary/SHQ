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

full_dat_average

full_dat_average %>% 
  write.csv('../Data Collection/SHQ_AverageSLandMVICData_2026-04-20.csv',
            row.names = F, na = "")


full_dat_dom <- full_dat_average %>% pivot_longer(cols = starts_with(c("Left", "Right")),
                                  names_to = c("Tested_Leg", "Action"),
                                  names_sep = 5) %>% 
  mutate(Action = str_replace_all(Action, "_K", "K" ),
         Tested_Leg = str_remove(Tested_Leg, "_"),
         Leg = ifelse(Dom == Tested_Leg, "Dom", "NDom"),
         .before = Action) %>% 
  select(1:7, 12:14) %>% 
  pivot_wider(names_from = c("Leg", "Action"),
              names_glue = c("{Leg}_{Action}"))  %>% 
  mutate(Dom_HQ = Dom_KF/Dom_KE,
         NDom_HQ = NDom_KF/NDom_KE)
  
full_dat_dom %>% 
  write.csv('../Data Collection/SHQ_AverageSLandMVICData_2026-04-21.csv',
            row.names = F, na = "")
full_dat_dom  



