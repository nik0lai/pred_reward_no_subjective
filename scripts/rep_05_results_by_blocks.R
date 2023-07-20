# Packages
if (!require('pacman', quietly = TRUE)) install.packages('pacman'); library('pacman', quietly = TRUE)
p_load(magrittr, purrr, dplyr, here, readr, tidyr, stringr, ggplot2, ggtext, patchwork, BayesFactor, bayestestR)

source('functions/functions.R')

# Data --------------------------------------------------------------------

dat <- read_csv('data/processed/rep_all_data_filtered.csv')

# Labels ------------------------------------------------------------------

# Bias source labels
labels_bias_source = c('baserate'='Base-rate', 'mullerlyer'='Müller-Lyer', 'payoff'='Payoff')

# Block data --------------------------------------------------------------

# Both decision and reproduction data is split the data into 
# three blocks of 50 trials each
block_size = 50
number_of_groups = 150/block_size

# Decision data -----------------------------------------------------------

dec_data <- 
  dat %>% 
  filter(trial_type == 'decision')

# Create block number column
dec_data <- 
  dec_data %>% 
  group_by(participant, bias_direction) %>% 
  arrange(participant, bias_direction, trial_number) %>% 
  mutate(new_ix = seq(n())) %>% 
  mutate(block = cut(new_ix, breaks = c(seq(1, max(new_ix) + block_size, block_size)), 
                     include.lowest = TRUE, 
                     labels = seq(number_of_groups), 
                     right = FALSE))

# Check that all blocks have 50 trials
dec_data %>% 
  group_by(participant, bias_source, bias_direction, trial_type, block) %>% 
  summarise(count = n()) %>% 
  filter(count != 50)

# Get SDT measures
sdt_data <-
  dec_data %>% 
  # make confusion matrix and count each instance
  mutate(conf_mat = paste0(tolower(accuracy), '_', answer)) %>%
  group_by(participant, bias_source, bias_direction, block, conf_mat) %>%
  summarise(count = n(), 
            .groups = "keep") %>% 
  pivot_wider(data = ., names_from = conf_mat, 
              values_from = count, values_fill = list(count = 0)) %>% 
  # calculate sdt measures
  do(sdt_calc(hits = .$true_long, misses = .$false_short, 
              crrej = .$true_short, far = .$false_long)) %>%
  # reshape data
  pivot_longer(data = ., 
               cols = c(hr, fr, lambda, d, ccrit, beta), names_to = "key", 
               values_to = "value") %>% 
  filter(key == 'ccrit') %>% 
  pivot_wider(names_from = key, values_from = value) %>% 
  ungroup()

# Test for effect of block
bf_bias_block <- 
  sdt_data %>% 
  mutate(participant = factor(participant),
         block = factor(block)) %>% 
  group_by(bias_source) %>% 
  nest() %>% 
  mutate(lmbf_bias_dir = map(data, ~lmBF(ccrit ~ bias_direction + participant, whichRandom = 'participant', data=as.data.frame(.x))),
         lmbf_bias_dir_block = map(data, ~lmBF(ccrit ~ bias_direction * block + participant, whichRandom = 'participant', data=as.data.frame(.x))))

# Get bf and bf of simple against interaction model
bf_bias_block <- 
  bf_bias_block %>%
  mutate(bf_bias_dir = unlist(map(lmbf_bias_dir, ~unique(describe_posterior(.x)$BF))),
         bf_bias_dir_block = unlist(map(lmbf_bias_dir_block, ~unique(describe_posterior(.x)$BF)))) %>% 
  mutate(bf_over_block = bf_bias_dir/bf_bias_dir_block)


# Calculate effect size and bf 
bf_bias <- 
  get_bf_d(data = sdt_data %>% 
             rename(value = ccrit) %>% 
             pivot_wider(names_from = bias_direction, values_from = value) %>% 
             group_by(bias_source, block) %>% 
             nest(),
           col_names = c('long', 'short')) %>% 
  mutate(d = round(d, 2),
         bf_format = shorter_bf_value(bf)) %>% 
  mutate(label_bf = paste0('bold(BF[10]) == "', bf_format, '"'),
         label_d = paste0('italic(d) == ', d)
  )

# Plot categorization bias by block
dec_plot <- 
  sdt_data %>% 
  mutate(bias_direction = factor(str_to_title(bias_direction), levels = str_to_title(unique(sdt_data$bias_direction)))) %>% 
  ggplot(aes(x=bias_direction, y=ccrit, color=bias_source)) +
  facet_wrap(. ~ bias_source + block, nrow = 3, 
             labeller = labeller(block = function(x) paste0('Block ', x), 
                                 bias_source = labels_bias_source,
                                 .multi_line = TRUE)) +
  geom_point() +
  geom_line(aes(group=participant)) +
  stat_summary(fun=mean, geom='point', color='black') +
  stat_summary(fun=mean, 
               fun.min = function(x) mean(x) - sd(x)/sqrt(length(x)),
               fun.max = function(x) mean(x) + sd(x)/sqrt(length(x)),
               geom = 'errorbar', color='black', width=.2) +
  ggtitle('Categorization bias') +
  geom_text(data = bf_bias,
            # aes(x = 1.45, y = 1.6, label = paste0('atop(', label_bf, ', ', label_d,')')),
            aes(x = 1.45, y = 1.6, label = label_d),
            color = "black", size = 3, parse = TRUE) +
  ylab('SDT criterion<br><span style=\'font-size:9pt\'><- bias to "short" - bias to "long" -></span>') +
  xlab('Bias direction') +
  theme(legend.position = 'none',
        axis.title.y = element_textbox_simple(
          halign = .56,
          size=15,
          orientation = "left-rotated",
          margin = margin(0, 0, 0, 0)
        ))

# Show and save plot
dec_plot
ggsave('plots/reproduction_experiment_categorization_bias_by_block.png', height = 6, width = 4.5, scale=1.1)

# Reproduction data -------------------------------------------------------

rep_data <- 
  dat %>% 
  filter(trial_type == 'reproduction')

# Get reproduction error
rep_data <- 
  rep_data %>% 
  group_by(participant, bias_direction) %>% 
  arrange(participant, bias_direction, trial_number) %>% 
  mutate(new_ix = seq(n())) %>% 
  mutate(block = cut(new_ix, breaks = c(seq(1, max(new_ix) + block_size, block_size)), 
                     include.lowest = TRUE, labels = seq(number_of_groups), 
                     right = FALSE))

# Count block
rep_data %>% 
  group_by(participant, bias_source, bias_direction, trial_type, block) %>% 
  summarise(count = n()) %>% 
  filter(count != 50)

# Get reproduction error
rep_data <- 
  rep_data %>% 
  mutate(value = abs(as.integer(answer)) - as.integer(target_length)) %>% 
  group_by(participant, bias_source, bias_direction, block) %>% 
  summarise(value = mean(value)) %>% 
  ungroup()


# Test for effect of block
bf_rep_block <- 
  rep_data %>% 
  mutate(participant = factor(participant),
         block = factor(block)) %>% 
  group_by(bias_source) %>% 
  nest() %>% 
  mutate(lmbf_bias_dir = map(data, ~lmBF(value ~ bias_direction + participant, whichRandom = 'participant', data=as.data.frame(.x))),
         lmbf_bias_dir_block = map(data, ~lmBF(value ~ bias_direction * block + participant, whichRandom = 'participant', data=as.data.frame(.x))))

bf_rep_block <- 
  bf_rep_block %>%
  mutate(bf_bias_dir = unlist(map(lmbf_bias_dir, ~unique(describe_posterior(.x)$BF))),
         bf_bias_dir_block = unlist(map(lmbf_bias_dir_block, ~unique(describe_posterior(.x)$BF)))) %>% 
  mutate(bf_over_block = bf_bias_dir/bf_bias_dir_block)

# Calculate effect size and bf 
bf_rep <- 
  get_bf_d(data = rep_data %>%
             pivot_wider(names_from = bias_direction, values_from = value) %>% 
             group_by(bias_source, block) %>% 
             nest(),
           col_names = c('long', 'short')) %>% 
  mutate(d = round(d, 2),
         bf_format = shorter_bf_value(bf)) %>% 
  mutate(label_bf = paste0('bold(BF[10]) == "', bf_format, '"'),
         label_d = paste0('italic(d) == ', d)
  )

# Plot reproduction error
rep_plot <- 
  rep_data %>% 
  mutate(bias_direction = factor(str_to_title(bias_direction), levels = str_to_title(unique(sdt_data$bias_direction)))) %>% 
  ggplot(aes(x=bias_direction, y=value, color=bias_source)) +
  facet_wrap(. ~ bias_source + block, nrow = 3, 
             labeller = labeller(block = function(x) paste0('Block ', x), 
                                 bias_source = labels_bias_source,
                                 .multi_line = TRUE)) +
  geom_point() +
  geom_line(aes(group=participant)) +
  stat_summary(fun=mean, geom='point', color='black') +
  stat_summary(fun=mean, 
               fun.min = function(x) mean(x) - sd(x)/sqrt(length(x)),
               fun.max = function(x) mean(x) + sd(x)/sqrt(length(x)),
               geom = 'errorbar', color='black', width=.2) +
  ggtitle('Reproduction error') +
  geom_text(data = bf_rep,
            aes(x = 1.45, y = 56, 
                label = label_d
                ),
            color = "black", size = 3, parse = TRUE) +
  ylab('Reproduction error (pixels)<br><span style=\'font-size:9pt\'><- Underestimation - Overestimation  -></span>') +
  xlab('Bias direction') +
  theme(legend.position = 'none',
        axis.title.y = element_textbox_simple(
          halign = .56,
          size=15,
          orientation = "left-rotated",
          margin = margin(0, 0, 0, 0)
        ))

# Show and save plot
rep_plot
ggsave('plots/reproduction_experiment_reproduction_error_by_block.png', height = 6, width = 4.5, scale=1.1)

# Combine plots -----------------------------------------------------------

dec_plot + rep_plot
ggsave('plots/reproduction_experiment_by_block.png', height = 7, width = 8, scale=1)


