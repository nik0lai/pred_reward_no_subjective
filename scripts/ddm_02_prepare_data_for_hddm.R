# Packages
if (!require('pacman', quietly = TRUE)) install.packages('pacman'); library('pacman', quietly = TRUE)
p_load(readr, ggplot2, magrittr, here, dplyr)

source(here('functions/functions.R'))

# Read data ---------------------------------------------------------------

data_exp1 <- read_csv(here('data/processed/ddm_all_data_filtered_experiment_1.csv'))
data_exp2 <- read_csv(here('data/processed/ddm_all_data_filtered_experiment_2.csv'))
data_exp3 <- read_csv(here('data/processed/ddm_all_data_filtered_experiment_3.csv'))
data_arrows <- read_csv(here('data/processed/ddm_all_data_filtered_arrowheads.csv'))

## Filter fast and slow RTs ------------------------

# Fast RTs are those below 200 ms, slow 
# are those outside four standard deviation 
# from the mean

rt_filtered_data_exp1 <- filter_fast_slow_responses(data = data_exp1, sd_threshold = 4)
rt_filtered_data_exp2 <- filter_fast_slow_responses(data = data_exp2, sd_threshold = 4)
rt_filtered_data_exp3 <- filter_fast_slow_responses(data = data_exp3, sd_threshold = 4)
rt_filtered_data_arrows <- filter_fast_slow_responses(data = data_arrows, sd_threshold = 4)

# Select and rename columns for HDDM --------------------------------------

# Format data set so only columns needed are kept but
# also rename participant, line_type and answer columns.
# Also recode line_type and answer to 0 and 1.

ddm_exp1 <- format_columns_for_hddm(rt_filtered_data_exp1)
ddm_exp2 <- format_columns_for_hddm(rt_filtered_data_exp2)
ddm_exp3 <- format_columns_for_hddm(rt_filtered_data_exp3)

# The arrows dataset needs has slightly difference columns so
# it's format manually

ddm_arrows <- 
  rt_filtered_data_arrows %>% 
  ungroup() %>% 
  select(participant, bias_source, bias_direction, add_len, line_type, answer, rt) %>% 
  rename(subj_idx = participant,
         stim = line_type,
         response = answer,
         addition_length = add_len) %>% 
  mutate(across(c(stim, response), ~ recode(.x, 'long' = 1, 'short' = 0)))
  
# Save data ---------------------------------------------------------------

write_csv(ddm_exp1 %>% arrange(subj_idx), here('data/processed/ddm_exp1_200sd4.csv'))
write_csv(ddm_exp2 %>% arrange(subj_idx), here('data/processed/ddm_exp2_200sd4.csv'))
write_csv(ddm_exp3 %>% arrange(subj_idx), here('data/processed/ddm_exp3_200sd4.csv'))
write_csv(ddm_arrows %>% arrange(subj_idx), here('data/processed/ddm_arrows_200sd4.csv'))
