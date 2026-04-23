library(tidyverse)
library(afex)
library(rstatix)

full_dat <- read_csv('../Data Collection/SHQ_AverageSLandMVICData_2026-04-21.csv')

mvic_dat <- full_dat %>% 
  select(ID, Sex, ends_with(c("KE", "KF", "HQ"))) %>% 
  pivot_longer(cols = 3:ncol(.),
               names_to = c("Leg", "Action"),
               names_sep = "_") %>% 
  pivot_wider(names_from = Action) %>% 
  mutate(Sex = fct(Sex, levels = c("M", "F")),
         Leg = fct(Leg, levels = c("Dom", "NDom")))
mvic_dat

full_dat %>% 
  group_by(Sex) %>% 
  count(Position)

#### Descriptive stats
mvic_dat %>% 
  group_by(Sex, Leg) %>% 
  summarize(KE_mean = mean(KE),
            KE_sd = sd(KE),
            KF_mean = mean(KF),
            KF_sd = sd(KF),
            HQ_mean = mean(HQ))

theme_set(theme_classic())

landing_dat <- full_dat %>% 
  select(ID, Sex, ends_with('Excursion')) %>% 
  pivot_longer(cols = 3:4,
               names_to = c("Leg", 'Var'),
               names_sep = "_") %>% 
  pivot_wider(names_from = Var)
landing_dat


mvic_dat %>% 
  ggplot(aes(x = HQ,
             y = KneeExcursion,
             color = Sex)) +
  geom_point() 

hq_landing_dat <- left_join(mvic_dat, landing_dat)


hq_landing_dat %>% 
  ggplot(aes(x = HQ,
             y = KneeExcursion,
             color = Sex)) +
  geom_point() +
  geom_smooth(method ='lm') +
  facet_wrap(~Leg)

dom_hq_model <- lm(KneeExcursion ~ Sex * HQ,
                   data = hq_landing_dat %>% 
                     filter(Leg == 'Dom'))

summary(dom_hq_model)

ndom_hq_model <- lm(KneeExcursion ~ Sex * HQ,
                    data = hq_landing_dat %>% 
                      filter(Leg == "NDom"))
summary(ndom_hq_model)



ke_model <- aov_4(KE ~ Sex * (Leg | ID),
                  data = mvic_dat)
ke_model

emmeans(ke_model, pairwise ~ Sex)

kf_model <- aov_4(KF ~ Sex * (Leg | ID),
                  data = mvic_dat)
kf_model

hq_dat <- aov_4(HQ ~ Sex * (Leg | ID),
                data = mvic_dat)
hq_dat

library(emmeans)
emmeans(hq_dat, pairwise ~ Sex)
