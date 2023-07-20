# Packages
if (!require('pacman', quietly = TRUE)) install.packages('pacman'); library('pacman', quietly = TRUE)
p_load(magrittr, purrr, dplyr, here, readr, tidyr, stringr, ggplot2, ggtext, patchwork, quantreg, colorspace)

source(here('functions/functions.R'))


# DDM experiments (1, 2 and 3) --------------------------------------------

# Data --------------------------------------------------------------------

# Get files that went into HDDM
files <-
  dir(here('data/processed/'), pattern = 'ddm_exp', full.names = TRUE)

# Read data
dat <- 
  map(files, ~ read_csv(.x) %>% mutate(experiment = str_extract(string = .x, 'exp[123]'))) %>% 
  bind_rows()

# Simulated data
files <-
  dir(here('data/'), pattern = 'ppc_rt_exp', full.names = TRUE)

# Read data
sim_data <- 
  map(files, ~ read_csv(.x) %>% mutate(experiment = str_extract(string = .x, 'exp[123]'))) %>% 
  bind_rows()

# Format data -------------------------------------------------------------

# Create condition code column and convert some columns to factor
# so the conditions are in the right order in the plot

dat <- 
  dat %>% 
  mutate(cond_code = paste0(bias_source, '_', bias_direction),
         bias_source = factor(bias_source, levels = c('mullerlyer', 'payoff', 'baserate')),
         bias_direction = factor(bias_direction, levels = c('long', 'short')),
         experiment = factor(experiment, levels = c('exp1', 'exp2', 'exp3'))) %>% 
  mutate(cond_code = factor(cond_code, 
                            levels = c('mullerlyer_long', 'mullerlyer_short', 
                                       'payoff_long', 'payoff_short', 
                                       'baserate_long', 'baserate_short')))

sim_data <- 
  sim_data %>% 
  mutate(cond_code = paste0(bias_source, '_', bias_direction),
         bias_source = factor(bias_source, levels = c('mullerlyer', 'payoff', 'baserate')),
         bias_direction = factor(bias_direction, levels = c('long', 'short')),
         experiment = factor(experiment, levels = c('exp1', 'exp2', 'exp3'))) %>% 
  mutate(cond_code = factor(cond_code, 
                            levels = c('mullerlyer_long', 'mullerlyer_short', 
                                       'payoff_long', 'payoff_short', 
                                       'baserate_long', 'baserate_short')))

# Labels ------------------------------------------------------------------

bias_source_labels = c('mullerlyer' = 'Müller-Lyer',
                       'baserate' = 'Base-rate',
                       'payoff' = 'Payoff')

cond_code_labels = c('mullerlyer_long' = 'Müller-Lyer-Long', 
                     'mullerlyer_short' = 'Müller-Lyer-Short',
                     'payoff_long' = 'Payoff-Long', 
                     'payoff_short' = 'Payoff-Short',
                     'baserate_long' = 'Base-rate-Long', 
                     'baserate_short' = 'Base-rate-Short')

experiment_labels <- c('exp1' = 'Experiment 1',
                       'exp2' = 'Experiment 2',
                       'exp3' = 'Experiment 3')

# Functions ---------------------------------------------------------------

# Calculate RT quantiles for lower and upper boundary
data_quantiles <- function(data, quantiles = c(0.1, 0.3, 0.5, 0.7, 0.9)) {
  # compute the quantiles of 2AFC data
  
  # Flip the sign of the rt values when the response is 0.
  # Also gets rid of the other columns but 
  if ("data.frame" %in% class(data)) {
    if ("response" %in% colnames(data)) {
      data <- flip_errors(data)$rt
    } else {
      data <- data$rt
    }
  }
  
  # Quantiles
  q_lower <- as.numeric(quantile(-data[data < 0], probs = quantiles))
  q_upper <- as.numeric(quantile(data[data > 0], probs = quantiles))
  # probability of answering 1 (upper boundary)
  p_upper <- mean(data>0)
  # this is prob of answering 0 (just 1-p_upper)
  p_lower <- mean(data<0)
  
  return(tibble(q_lower, q_upper, quantiles, p_upper, p_lower))
}

# Flip sign for lower boundary ("short") responses
flip_errors <- function(data) {
  
  # Check if data is already flipped
  if (any(data$rt < 0)) {
    return(data)
  }
  
  # Convert into df
  data <- as.data.frame(data)
  # Index of response 0 
  idx <- data$response != 1
  data$rt[idx] <- -data$rt[idx]
  
  return(data)
}

# Empirical data ----------------------------------------------------------

# Group the data frame and nest the data
quantile_data <-
  dat %>%
  group_by(experiment, subj_idx, bias_source, bias_direction, cond_code) %>%
  nest() %>% 
  mutate(quantiles = map(data, data_quantiles)) %>% 
  unnest(quantiles) %>% 
  select(-data)

# Reformat output quantile df
quantile_data <- 
  quantile_data %>% 
  pivot_longer(names_to = 'boundary', values_to = 'value', cols = c(q_lower, q_upper)) %>% 
  mutate(p = case_when(boundary == 'q_lower' ~ p_lower,
                       boundary == 'q_upper' ~ p_upper)) %>% 
  select(-c(p_upper, p_lower))


# Get mean for each condition
summary_quantile_data <- 
  quantile_data %>% 
  group_by(experiment, bias_source, bias_direction, cond_code, quantiles, boundary) %>% 
  summarise(p = mean(p),
            mean = mean(value),
            sd = sd(value),
            sem = sd/sqrt(n())) %>% 
  mutate(quantiles = factor(quantiles, levels = seq(1,9)/10))

# Plot empirical RT quantiles
p <-
  summary_quantile_data %>% 
  ggplot(aes(x=quantiles, y=mean, color=cond_code)) +
  # One panel for each bias source and bias direction condition of each experiment
  facet_wrap(. ~ experiment + cond_code, ncol = 4, scales = 'free_y',
             labeller = labeller(experiment=experiment_labels, 
                                 cond_code=cond_code_labels, .multi_line = TRUE)) +
  # Plot average and error
  geom_point(aes(group=interaction(bias_direction, boundary), 
                 shape=boundary), 
             size=1) +
  # geom_line(aes(group=quantiles)) + 
  geom_line(aes(group=interaction(experiment, cond_code, boundary), linetype=boundary), alpha=.4) +
  geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem, x=quantiles), 
                width=.3, alpha=.8) +
  # Format plot
  theme(legend.position = 'bottom') +
  ylab('Reaction time (seconds)') +
  xlab('Quantiles') +
  ggtitle('Reaction time values by quantile')

# Simulated data
summary_sim_data <- 
  sim_data %>% 
  group_by(experiment, bias_source, bias_direction, cond_code, quantiles, boundary) %>% 
  summarise(p = mean(p),
            mean = mean(value),
            sd = sd(value),
            sem = sd/sqrt(n())) %>% 
  mutate(color=paste0('simulated_', bias_direction)) %>% 
  mutate(quantiles = factor(quantiles, levels = seq(1,9)/10))

# Plot simulated data over empirical data
p +
  # plot averages and errorbars
  geom_point(data=summary_sim_data, aes(shape=boundary, color=color), alpha=.5) + 
  geom_errorbar(data=summary_sim_data, 
                aes(group=interaction(bias_direction, boundary, quantiles), 
                    ymin=mean-sem, ymax=mean+sem, x=quantiles,
                    color=color), alpha=.5, width=.3) +
  # Format legend ------------------------------------------------------------
guides(
  color = guide_legend(title = 'Bias direction', 
                       override.aes = list(linetype=0, shape=15, size=5, color=c(lighten('black',.6), 'black', 
                                                                                 get_condition_colors()['simulated_short'], 
                                                                                 get_condition_colors()['simulated_long'])), nrow = 2),
  shape = guide_legend(title = 'Response', override.aes = list(color='black'), nrow = 1),
  linetype = guide_legend(title = 'Response', nrow = 1)
) + 
  scale_color_manual(values = get_condition_colors(), 
                     breaks = c('mullerlyer_short', 'mullerlyer_long', 'simulated_short', 'simulated_long'), 
                     labels = c('mullerlyer_short' = 'Short (empirical)', 
                                'mullerlyer_long' = 'Long (empirical)', 
                                'simulated_long' = 'Long (simulated)', 
                                'simulated_short' = 'Short (simulated)'))  +
  scale_shape_manual(values = c(16,17), labels=c('q_lower'='Short', 'q_upper'='Long')) + 
  scale_linetype_manual(values = c(1,2), labels=c('q_lower'='Short', 'q_upper'='Long')) +
  theme(strip.text.x = element_text(size=8),
        legend.direction = 'vertical',
        legend.key.width = unit(.7, "cm"),
        legend.position = c(0.75, 0.12),
        legend.spacing.y = unit(.01, 'cm'))

# Save final plot
ggsave('plots/ddm_rt_quantile_predicted.png', height = 8, width = 6, scale = 1)

# -------------------------------------------------------------------------------
# -------------------------------------------------------------------------------
# -------------------------------------------------------------------------------

# DDM arrowhead length experiment

# Data --------------------------------------------------------------------

# Get files that went into HDDM
dat <- 
  read_csv('data/processed/ddm_arrows_200sd4.csv')


# Read data
sim_data <- 
  read_csv('data/ppc_rt_add_len_dczBDAL_sV.csv')


# Format data -------------------------------------------------------------

# Create condition code column and convert some columns to factor
# so the conditions are in the right order in the plot

dat <- 
  dat %>% 
  mutate(cond_code = paste0(bias_source, '_', bias_direction),
         bias_direction = factor(bias_direction, levels = c('long', 'short'))) %>% 
  mutate(cond_code = factor(cond_code, levels = c('mullerlyer_long', 'mullerlyer_short')))

sim_data <- 
  sim_data %>%
  mutate(cond_code = paste0(bias_source, '_', bias_direction),
         bias_source = factor(bias_source, levels = c('mullerlyer', 'baserate', 'payoff')),
         bias_direction = factor(bias_direction, levels = c('long', 'short'))) %>% 
  mutate(cond_code = factor(cond_code, levels = c('mullerlyer_long', 'mullerlyer_short')))

# Empirical data ----------------------------------------------------------

# Group the data frame and nest the data
quantile_data <-
  dat %>%
  group_by(subj_idx, bias_source, bias_direction, cond_code, addition_length) %>%
  nest() %>% 
  mutate(quantiles = map(data, data_quantiles)) %>% 
  unnest(quantiles) %>% 
  select(-data)

# Reformat output quantile df
quantile_data <- 
  quantile_data %>% 
  pivot_longer(names_to = 'boundary', values_to = 'value', cols = c(q_lower, q_upper)) %>% 
  mutate(p = case_when(boundary == 'q_lower' ~ p_lower,
                       boundary == 'q_upper' ~ p_upper)) %>% 
  select(-c(p_upper, p_lower))


# Get mean for each condition
summary_quantile_data <- 
  quantile_data %>% 
  group_by(bias_source, bias_direction, cond_code, quantiles, boundary, addition_length) %>% 
  summarise(p_mean = mean(p),
            p_sd = sd(p),
            p_sem = p_sd/sqrt(n()),
            mean = mean(value),
            sd = sd(value),
            sem = sd/sqrt(n())) %>% 
  mutate(quantiles = factor(quantiles, levels = seq(1,9)/10))

# Plot empirical RT quantiles
p <-
  summary_quantile_data %>% 
  ggplot(aes(x=quantiles, y=mean, color=cond_code)) +
  # One panel for each bias source and bias direction condition of each experiment
  facet_grid(cols = vars(addition_length), rows = vars(bias_direction ), 
             labeller = labeller(addition_length = function(x) paste0('Arrowhead length\n', x, ' pixels'),
                                 bias_direction = function(x) paste0('Biased-to-', x))) +
  # Plot average and error
  geom_point(aes(group=interaction(bias_direction, boundary), 
                 shape=boundary), 
             size=1) +
  # geom_line(aes(group=quantiles)) + 
  geom_line(aes(group=interaction(cond_code, boundary), linetype=boundary), alpha=.4) +
  geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem, x=quantiles), 
                width=.3, alpha=.8) +
  # Format plot
  theme(legend.position = 'bottom') +
  ylab('Reaction time (seconds)') +
  xlab('Quantiles') +
  ggtitle('Reaction time values by quantile')

# Simulated data
summary_sim_data <- 
  sim_data %>% 
  group_by(bias_source, bias_direction, cond_code, quantiles, boundary) %>% 
  summarise(p_mean = mean(p),
            p_sd = sd(p),
            p_sem = p_sd/sqrt(n()),
            mean = mean(value),
            sd = sd(value),
            sem = sd/sqrt(n())) %>% 
  mutate(color=paste0('simulated_', bias_direction)) %>% 
  mutate(quantiles = factor(quantiles, levels = seq(1,9)/10))

# Plot simulated data over empirical data
p + 
  geom_point(data=summary_sim_data, aes(shape=boundary, color=color), 
             alpha=.5) +
  geom_errorbar(data=summary_sim_data, 
                aes(group=interaction(bias_direction, boundary), 
                    ymin=mean-sem, ymax=mean+sem, x=quantiles, color=color), 
                alpha=.5, width=.3) +
  # Format legend
  guides(color = guide_legend(title = 'Bias direction', 
                              override.aes = list(linetype=0, shape=15, 
                                                  size=5, 
                                                  color=c(lighten('black',.6), 'black', 
                                                          get_condition_colors()['simulated_short'], 
                                                          get_condition_colors()['simulated_long'])), nrow = 2),
         shape = guide_legend(title = 'Response', override.aes = list(color='black'), nrow = 2),
         linetype = guide_legend(title = 'Response')) + 
  scale_color_manual(values = get_condition_colors(), 
                     breaks = c('mullerlyer_short', 'mullerlyer_long', 'simulated_short', 'simulated_long'), 
                     labels = c('mullerlyer_short' = 'Short (empirical)', 
                                'mullerlyer_long' = 'Long (empirical)', 
                                'simulated_long' = 'Long (simulated)', 
                                'simulated_short' = 'Short (simulated)')) +
  scale_shape_manual(values = c(16,17), labels=c('q_lower'='Short', 'q_upper'='Long')) + 
  scale_linetype_manual(values = c(1,2), labels=c('q_lower'='Short', 'q_upper'='Long')) +
  theme(legend.key.width = unit(1, "cm"), legend.direction = 'vertical')  

# Save final plot
ggsave('plots/ddm_arrows_rt_quantile_predicted.png', height = 8, width = 6, scale = .9)

