# Packages
if (!require('pacman', quietly = TRUE)) install.packages('pacman'); library('pacman', quietly = TRUE)
p_load(readr, dplyr, here, purrr, tidyr)

source(here('functions/functions.R'))

# Experiment 1 ------------------------------------------------------------

## Read data
data_exp1 <- read_csv(file = here('data/ddm_all_data_experiment_1.csv'))

## Count subjects per condition --------------------------------------------

# Before doing any kind of filtering there are:
# bias_source count
# <chr>       <int>
# 1 mullerlyer     50
# 2 payoff         50

data_exp1 %>% 
  select(participant, bias_source) %>% 
  distinct() %>% 
  group_by(bias_source) %>% 
  summarise(count = n())

# Run filtering
filtering_exp1 <- filter_data_ddm(data = data_exp1, sd_threshold = 4)
# Get filtered data
filtered_data_exp1 <- filtering_exp1$data

# Get text with filtering info
filtering_text_exp1 <-get_filtering_text(filtering_exp1)
# Get table with filtering info
filtered_table_exp1 <- get_filtering_table(filtering_exp1)
  
## Count filtered subjects -------------------------------------------------

# Before doing any kind of filtering there are:
# bias_source count
# <chr>       <int>
# 1 mullerlyer     48
# 2 payoff         50

filtered_data_exp1 %>% 
  select(participant, bias_source) %>% 
  distinct() %>% 
  group_by(bias_source) %>% 
  summarise(count = n())

# Experiment 2 ------------------------------------------------------------

## Read data
data_exp2 <- read_csv(here('data/ddm_all_data_experiment_2.csv'))

## Count subjects per condition --------------------------------------------

# Before doing any kind of filtering there are:
# bias_source count
# <chr>       <int>
# 1 mullerlyer     50
# 2 payoff         40

data_exp2 %>% 
  select(participant, bias_source) %>% 
  distinct() %>% 
  group_by(bias_source) %>% 
  summarise(count = n())

# Run filtering
filtering_exp2 <- filter_data_ddm(data = data_exp2, sd_threshold = 4)
# Get filtered data
filtered_data_exp2 <- filtering_exp2$data

# Get text with filtering info
filtering_text_exp2 <-get_filtering_text(filtering_exp2)
# Get table with filtering info
filtered_table_exp2 <- get_filtering_table(filtering_exp2)

## Count filtered subjects -------------------------------------------------

# Before doing any kind of filtering there are:
# bias_source count
# <chr>       <int>
# 1 mullerlyer     48
# 2 payoff         38

filtered_data_exp2 %>% 
  select(participant, bias_source) %>% 
  distinct() %>% 
  group_by(bias_source) %>% 
  summarise(count = n())

# Experiment 3 ------------------------------------------------------------

## Read data
data_exp3 <- read_csv(here('data/ddm_all_data_experiment_3.csv'))

## Count subjects per condition --------------------------------------------

# Before doing any kind of filtering there are:
# bias_source count
# <chr>       <int>
# 1 baserate       36
# 2 mullerlyer     50
# 3 payoff         50

data_exp3 %>% 
  select(participant, bias_source) %>% 
  distinct() %>% 
  group_by(bias_source) %>% 
  summarise(count = n())

# Run filtering
filtering_exp3 <- filter_data_ddm(data = data_exp3, sd_threshold = 4)
# Get filtered data
filtered_data_exp3 <- filtering_exp3$data

# Get text with filtering info
filtering_text_exp3 <-get_filtering_text(filtering_exp3)
# Get table with filtering info
filtered_table_exp3 <- get_filtering_table(filtering_exp3)

## Count filtered subjects -------------------------------------------------

# Before doing any kind of filtering there are:
# bias_source count
# <chr>       <int>
# 1 baserate       37
# 2 mullerlyer     47
# 3 payoff         50

filtered_data_exp3 %>% 
  select(participant, bias_source) %>% 
  distinct() %>% 
  group_by(bias_source) %>% 
  summarise(count = n())

# Experiment addition length ----------------------------------------------

## Read data
data_arrows <- read_csv(here('data/ddm_all_data_arrowheads.csv'))

## Count subjects per condition --------------------------------------------

# Before doing any kind of filtering there are:
# bias_source count
# <chr>       <int>
# 1 mullerlyer     50

data_arrows %>% 
  select(participant, bias_source) %>% 
  distinct() %>% 
  group_by(bias_source) %>% 
  summarise(count = n())

# Run filtering
filtering_arrows <- filter_data_ddm(data = data_arrows, sd_threshold = 4)
# Get filtered data
filtered_data_arrows <- filtering_arrows$data

# Get text with filtering info
filtering_text_arrows <-get_filtering_text(filtering_arrows)
# Get table with filtering info
filtered_table_arrows <- get_filtering_table(filtering_arrows)

## Count filtered subjects -------------------------------------------------

# Before doing any kind of filtering there are:
# bias_source count
# <chr>       <int>
# 1 mullerlyer     48

filtered_data_arrows %>% 
  select(participant, bias_source) %>% 
  distinct() %>% 
  group_by(bias_source) %>% 
  summarise(count = n())

# Count filtered subjects per condition -----------------------------------

bind_rows(filtered_table_exp1 %>% mutate(exp='1'),
          filtered_table_exp2 %>% mutate(exp='2'),
          filtered_table_exp3 %>% mutate(exp='3'),
          filtered_table_arrows %>% mutate(exp='A')) %>% 
  select(exp, participant, bias_source) %>% 
  unique() %>% 
  group_by(exp, bias_source) %>% 
  summarise(count = n())


# Count participants per experiment ---------------------------------------

filtered_data_exp1 %>% select(participant, bias_source) %>% distinct() %>% group_by(bias_source) %>% summarise(count = n())
filtered_data_exp2 %>% select(participant, bias_source) %>% distinct() %>% group_by(bias_source) %>% summarise(count = n())
filtered_data_exp3 %>% select(participant, bias_source) %>% distinct() %>% group_by(bias_source) %>% summarise(count = n())


# Save data ---------------------------------------------------------------

# Save filtered data
write_csv(filtered_data_exp1, here('data/processed/ddm_all_data_filtered_experiment_1.csv'))
write_csv(filtered_data_exp2, here('data/processed/ddm_all_data_filtered_experiment_2.csv'))
write_csv(filtered_data_exp3, here('data/processed/ddm_all_data_filtered_experiment_3.csv'))
write_csv(filtered_data_arrows, here('data/processed/ddm_all_data_filtered_arrowheads.csv'))

