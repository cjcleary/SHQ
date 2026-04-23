### Load in the packages required...
# if these do not load, remove the # from the next line and run that line,
# only has to be done once
# install.packages(c('tidyverse','afex','rstatix'))

library(tidyverse) # general data manipulation
library(afex) # anova models
library(rstatix) # t tests etc

# Look up how to make a project in R and store the csv file in that same directory
# much easier than setwd() or other things you may have been taught in the past
full_dat <- read_csv('../Data Collection/SHQ_AverageSLandMVICData_2026-04-21.csv')

# now wrangle the data into long format
# long format = each row is a single observation....
# so a subject will have multiple rows (E.g, SHQ001 will have a row each variable for the DOM leg and NDom leg)
full_data_long <- full_dat %>% 
  # filter out subject 018
  filter(!ID == "SHQ018") %>% 
  # select the variables of interest
  select(ID, Sex, starts_with(c("Dom", "NDom"))) %>% 
  # pivot to long format
  pivot_longer(cols = 3:ncol(.),
               names_to = c("Leg", "Action"),
               names_sep = "_") %>% 
  # now we pivot back to a clean long format 
  # run these lines separately to see what's going on, just highlight the full data %>% 
  # to whatever line you wwant to run
  pivot_wider(names_from = Action) %>% 
  mutate(Sex = fct(Sex, levels = c("M", "F")),
         Leg = fct(Leg, levels = c("Dom", "NDom")))
full_data_long

# split full data long into dominant and nondominat only dataframes
# just for ease
dom_dat <- full_data_long %>% 
  filter(Leg == "Dom") %>% 
  # keep things clean, unselect the leg column
  select(-Leg)

ndom_dat <- full_data_long %>% 
  filter(Leg == 'NDom') %>% 
  select(-Leg)

colnames(dom_dat)

##### Analysis example 
# model_name <- lm(DV ~ Sex * HQ, data = data_frame)
# From my example, you can repeat for the non-dominant (NDom) leg and change the
# Dependent variable (DV) if you want
dom_contact_model <- lm(KneeFlexionContact ~ Sex * HQ, data = dom_dat)

# view the model output table
summary(dom_contact_model)

# insert the ndom model here or adjust the DVs, 100% your call

#### plot example
# first i like to assign colors outside of the plotting call 
# we'lll just use skidmore colors, although orange does go well w/ the green
skidmore_green <- '#006A52'
skidmore_yellow <- '#FFD100'

# I apply a custom theme using this code. look into it 
theme_set(theme_classic(base_family = 'serif'))
theme_update(legend.text = element_text(face = 'bold',
                                        color = 'black'),
             legend.position = 'inside',
             legend.position.inside = c(0.1, 1),
             axis.text = element_text(family = "serif", 
                                      face = "bold",
                                      color = "black",
                                      size = 10),
             axis.title = element_text(family = "serif",
                                       face = "bold",
                                       color = "black",
                                       size = 10),
             panel.spacing.x = unit(0.25, "in"),
             axis.line = element_line(linewidth = 1.25,
                                      color = "black"),
             axis.ticks = element_line(linewidth = 1.25),
             panel.background = element_rect(fill = "transparent"),
             plot.background = element_rect(fill = "transparent"),
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank())

# main plot code
dom_contact_plot <- dom_dat %>% 
  # call ggplot to initialize a plot
  ggplot(aes(x = HQ,
             y = KneeFlexionContact,
             color = Sex)) +
  # apply the layers aka geoms, first is a point (scatterplot)
  geom_point(aes(shape = Sex),
             size = 3) +
  # next is to apply a trendline on the figure 
  geom_smooth(method = 'lm',
              se = F,
              show.legend = F) + 
  # you can adjust limits in this block and the next one to 
  # center the data better
  scale_y_continuous(
    breaks = scales::breaks_pretty(n = 5),
    expand = expansion(mult = c(0, 0.25)),
    limits = c(0, 25)) +
  scale_x_continuous(
    breaks = scales::breaks_pretty(n = 5),
    expand = expansion(mult = c(0, 0)),
    limits = c(0, 0.8)) +
  scale_color_manual(values = c(skidmore_green, skidmore_yellow)) +
  labs(x = "Hamstring:Quadricep Ratio (AU)",
       y = "Knee Flexion at Ground Contact (degrees)")
dom_contact_plot

# we'll go over saving plots later. 