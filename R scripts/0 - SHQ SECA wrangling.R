library(tidyverse)

full_seca <- read_csv('../Data Collection/SECA_Export_Full04162026.csv')

glimpse(full_seca)

# Filter to just only include the SHQ subjects

shq_seca <- full_seca %>% 
  filter(grepl('SHQ', ExternalPatientId)) %>% 
  # remove SHQ000
  filter(!ExternalPatientId == "SHQ000") %>% 
  mutate(ExternalPatientId = str_remove(ExternalPatientId, "_"))
shq_seca

# Print the number of subjects, sanity check
n_ids <- length(unique(shq_seca$ExternalPatientId))
cat('There are', n_ids, 'participants.')

# Define the columns we want to select
columns_to_keep <- c('ExternalPatientId', 'Gender', 'Age', 'Height', 'Weight',
                     'Adult_BMI', 'Adult_FM', 'Adult_FMP', 'Adult_FFM', 'Adult_SMM',
                     'Adult_SSMMRightArm', 'Adult_SSMMLeftArm', 'Adult_SSMMRightLeg', 
                     'Adult_SSMMLeftLeg', 'Adult_SSMMTorso', 'Adult_PhA', 
                     'Adult_PhARightArm', 'Adult_PhALeftArm', 'Adult_PhARightLeg', 'Adult_PhALeftLeg',
                     'Adult_PhATorso')

# Now define the new column names we want to use
new_colnames <- c('ID', 'sex', 'age', 'height', 'bodymass', 'BMI', 'fatmass', 'bodyfatpercent',
                  'fatfreemass', 'skeletalmusclemass', 'rightarm_skeletalmusclemass', 'leftarm_skeletalmusclemass',
                  'rightleg_skeeltalmusclemass', 'leftleg_skeletalmusclemass', 'torso_skeletalmusclemass',  
                  'phaseangle', 'rightarm_phaseangle', 'leftarm_phaseangle', 'rightleg_phaseangle', 
                  'leftleg_phaseangle', 'torso_phaseangle')

# Now clean the SHQ SECA data frame
shq_seca_cleaned <- shq_seca %>% 
  select(all_of(columns_to_keep)) %>% 
  set_names(new_colnames)

# Write new data frame to a .csv file and arrange that data frame to be ascending IDs
# also round to 3 decimal points
shq_seca_cleaned %>% 
  arrange(ID) %>% 
  mutate(across(where(is.numeric), ~round(., 3))) %>% 
  write.csv('Data Collection/SHQ_SECA04162026.csv',
            row.names = F)
