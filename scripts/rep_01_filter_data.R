# Packages
if (!require('pacman', quietly = TRUE)) install.packages('pacman'); library('pacman', quietly = TRUE)
p_load(readr, dplyr, here, purrr, tidyr)

# Functions ---------------------------------------------------------------
source(here('functions/functions.R'))

# Read data ---------------------------------------------------------------
dat <- read_csv(here('data/rep_all_data.csv'))

# Count subjects per condition --------------------------------------------

# Before doing any kind of filtering there are:
# bias_source count
# <chr>       <int>
# 1 baserate       64
# 2 mullerlyer     31
# 3 payoff         54

dat %>% 
  select(participant, bias_source) %>% 
  distinct() %>% 
  group_by(bias_source) %>% 
  summarise(count = n())

# Outlier filtering -------------------------------------------------------

# For each of the measures to which filtering is applied the cutoff
# is calculated across all conditions, so orthogonally to the 
# manipulations of interest

# Number of standard deviations to use as cutoff threshold
sd_threshold <- 4

## Staircase --------------

staircase_data <- 
  dat %>% 
  select(participant, bias_source, staircase_threshold) %>% 
  distinct() %>% 
  rename(value = staircase_threshold)

# Get staircase outliers
staircase_outliers <- outlier_filtering(staircase_data, sd_threshold)

# Remove staircase outliers from data
dat <-
  dat %>% 
  anti_join(staircase_outliers %>% select(participant, bias_source))


## SDT measures ---------

sdt_data <-
  dat %>% 
  filter(trial_type == 'decision') %>% 
  mutate(answer = recode(answer, `1`='long', `0`='short')) %>% 
  mutate(conf_mat = paste0(tolower(accuracy), '_', answer)) %>%
  group_by(participant, bias_source, bias_direction, conf_mat) %>%
  summarise(count = n(), 
            .groups = "keep") %>% 
  pivot_wider(data = ., names_from = conf_mat, 
              values_from = count, values_fill = list(count = 0)) %>% 
  do(sdt_calc(hits = .$true_long, misses = .$false_short, 
              crrej = .$true_short, far = .$false_long)) %>%
  pivot_longer(data = ., 
               cols = c(hr, fr, lambda, d, ccrit, beta), names_to = "key", 
               values_to = "value") %>% 
  filter(key %in% c('d', 'ccrit')) %>% 
  pivot_wider(names_from = key, values_from = value) %>% 
  ungroup()

### d' below equal or below zero --------------

# Get low d' subjects
low_d_subs <- 
  sdt_data %>% 
  filter(d <= 0)

# Remove low d' subs
dat <- dat %>% 
  anti_join(low_d_subs %>% ungroup() %>% select(participant, bias_source))
sdt_data <- sdt_data %>% 
  anti_join(low_d_subs %>% ungroup() %>% select(participant, bias_source))

## d' outliers --------------

d_data <-
  sdt_data %>% 
  select(participant, bias_source, bias_direction, d) %>% 
  distinct() %>% 
  rename(value = d) %>% 
  ungroup()

# Get staircase outliers
d_outliers <- outlier_filtering(d_data, sd_threshold)

# Remove staircase outliers from data
dat <-
  dat %>% 
  anti_join(d_outliers %>% ungroup() %>% select(participant, bias_source))
sdt_data <-
  sdt_data %>% 
  anti_join(d_outliers %>% ungroup() %>% select(participant, bias_source))

## c outliers --------------

c_data <-
  sdt_data %>% 
  select(participant, bias_source, bias_direction, ccrit) %>% 
  distinct() %>% 
  rename(value = ccrit) %>% 
  ungroup()

# Get staircase outliers
c_outliers <- outlier_filtering(c_data, sd_threshold)

# Remove staircase outliers from data
dat <-
  dat %>% 
  anti_join(c_outliers %>% ungroup() %>% select(participant, bias_source))
sdt_data <-
  sdt_data %>% 
  anti_join(c_outliers %>% ungroup() %>% select(participant, bias_source))
  
# Reproduction outliers ---------------------------------------------------

rep_data <-
  dat %>%
  filter(trial_type == 'reproduction') %>% 
  mutate(value = abs(as.integer(answer)) - as.integer(target_length)) %>% 
  group_by(participant, bias_source, bias_direction) %>% 
  summarise(value = mean(value)) %>% 
  ungroup()

# Get staircase outliers
rep_outliers <- outlier_filtering(rep_data, sd_threshold)

dat <-
  dat %>% 
  anti_join(rep_outliers %>% ungroup() %>% select(participant, bias_source))
sdt_data <-
  sdt_data %>% 
  anti_join(rep_outliers %>% ungroup() %>% select(participant, bias_source))
rep_data <-
  rep_data %>% 
  anti_join(rep_outliers %>% ungroup() %>% select(participant, bias_source))

# Filtering summary -------------------------------------------------------

staircase_outliers
low_d_subs
d_outliers
c_outliers
rep_outliers

# Check how many participants per condition was removed

bind_rows(staircase_outliers, low_d_subs, d_outliers, c_outliers, rep_outliers) %>% 
  select(participant, bias_source) %>% 
  unique() %>% 
  group_by(bias_source) %>% 
  summarise(count = n())
  
# Count filtered subjects -------------------------------------------------

# After filtering there are:
# bias_source count
# <chr>       <int>
# 1 baserate       62
# 2 mullerlyer     28
# 3 payoff         47

dat %>% 
  select(participant, bias_source) %>% 
  distinct() %>% 
  group_by(bias_source) %>% 
  summarise(count = n())

# Demographics ---------------------------------------------------------------------

dat %>% 
  select(bias_source, participant, dem_age, dem_gen) %>% 
  distinct() %>% 
  mutate(dem_gen = recode(dem_gen, 'other'=2, 'female'=1, 'male'=0)) %>% 
  summarise(mean_age = mean(dem_age),
            sd = sd(dem_age),
            fem = sum(dem_gen == 1),
            other = sum(dem_gen == 2))
  
# Save all data ---------------------------------------------------------------

write_csv(dat, here('data/processed/rep_all_data_filtered.csv'))

# Save SDT reproduction data ----------------------------------------------

write_csv(rep_data %>% rename(rep = value) %>% full_join(sdt_data), 
          here('data/processed/rep_bias_and_reproduction.csv'))
