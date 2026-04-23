library(tidyverse)
sf <- 2000
dt <- 1/sf


mvic_dat <- read_csv('../Data Collection/SHQ001/Full Data/LEFT_KE1.csv')

n_samples_mvic_dat <- nrow(mvic_dat)
time_mvic_dat <- n_samples_mvic_dat/sf
time_s <- seq(from = 0, to = time_mvic_dat, by = dt)
time_s <- time_s[-length(time_s)]
mvic_dat_w_time <- mvic_dat %>% 
  mutate(time_s = time_s)

time_s
mvic_dat %>% 
  ggplot(aes(x = time_s,
             y = Torque)) +
  geom_line(color = 'purple')

opencap_sf <- 120
contact_frame <- 164
contact_time <- contact_frame / opencap_sf

 

drop_dat <- read_csv('../Data Collection/SHQ001/Full Data/L_drop_1.csv') %>% 
  filter(time_s <2.2)

peak_time_of_knee_angle <- drop_dat %>% 
  filter(left_knee_angle_deg == max(left_knee_angle_deg)) %>% 
  pull(time_s)


drop_dat %>% 
  ggplot(aes(x = time_s,
             y = left_knee_angle_deg)) +
  geom_line() +
  geom_vline(xintercept = contact_time,
             color = 'red') +
  geom_vline(xintercept = peak_time_of_knee_angle,
             color = 'blue') +
  labs(x = 'Time (s)',
       y = 'Knee Angle (deg)')
