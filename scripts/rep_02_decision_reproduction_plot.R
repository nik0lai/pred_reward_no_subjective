# Packages
if (!require('pacman', quietly = TRUE)) install.packages('pacman'); library('pacman', quietly = TRUE)
p_load(dplyr, readr, ggplot2, colorspace, tibble, stringr, ggtext, here, tidyr, purrr, patchwork)

source(here('functions/functions.R'))

# Read data ---------------------------------------------------------------

dat <- read_csv(here('data/processed/rep_bias_and_reproduction.csv'))

# Create a condition code (bias source + bias direction) column and set
# order by converting columns to factor
dat <- 
  dat %>% 
  mutate(
    bias_source = factor(bias_source, levels = c('mullerlyer', 'baserate', 'payoff')),
    bias_direction = factor(bias_direction, levels = c('long', 'short')),
    cond_code = paste(bias_source, bias_direction, sep = '_'))

## Get SDT data ------------------------------------------------------------

sdt_data <-
  dat %>% 
  select(-rep)

## Get reproduction data ---------------------------------------------------

rep_data <-
  dat %>%
  select(-c(d, ccrit)) %>% 
  rename(value = rep) 

# Labels ------------------------------------------------------------------

# Bias source labels
labels_bias_source = c('baserate'='Base-rate', 'mullerlyer'='Müller-Lyer', 'payoff'='Payoff')

# All labels
labels_data <-
  dat %>%
  select(bias_source, participant) %>% 
  distinct() %>% 
  group_by(bias_source) %>% 
  summarise(count = n()) %>% 
  mutate(bias_source_label = recode(bias_source, !!!labels_bias_source),
         sample_label = paste0(bias_source_label, '\nN = ', count))


# Condition labels with sample size
labels_bias_source_sample <- 
  labels_data %>% 
  select(-c(count, bias_source_label)) %>% 
  deframe()

# Plot bias ---------------------------------------------------------------

bf_bias <- 
  get_bf_d(data = sdt_data %>% 
             select(-d, -cond_code) %>% 
             rename(value = ccrit) %>% 
             pivot_wider(names_from = bias_direction, values_from = value) %>% 
             group_by(bias_source) %>% 
             nest(),
           col_names = c('long', 'short')) %>% 
  
  mutate(d = round(d, 2),
         bf_format = case_when(bf > 100 ~ formatC(bf, format = 'e', digits = 1),
                               bf > 10 ~ as.character(round(bf)),
                               bf > 1 ~ as.character(round(bf, 1)),
                               bf < 1 ~ as.character(round(bf, 2)))) %>% 
  mutate(label_bf = paste0('bold(BF[10]) == "', bf_format, '"'),
         label_d = paste0('italic(d) == ', d)
  )

# Bias per bias source and bias direction
p_bias <-
  sdt_data %>% 
  select(participant, cond_code, bias_source, bias_direction, ccrit) %>% 
  mutate(bias_direction = factor(str_to_title(bias_direction), levels = str_to_title(unique(sdt_data$bias_direction)))) %>% 
  rename(value = ccrit) %>%
  # Plot
  rep_base_plot() +
  # Customize plot
  theme(panel.spacing.x=unit(-1, "lines"),
        axis.title.y = element_textbox_simple(
          halign = .56,
          size=15,
          orientation = "left-rotated",
          margin = margin(0, 0, 0, 0)
        )) +
  scale_color_manual(values = get_condition_colors()) +
  ylab('SDT criterion<br><span style=\'font-size:9pt\'><- bias to "short" - bias to "long" -></span>') +
  xlab('Bias direction') +
  ylim(-2.27, 1.95) + 
  scale_x_discrete(expand=c(0.45, 0.1))

# Add effect sizes and save plot
p_bias <-
  p_bias +
  ggtitle('Categorization bias') +
  geom_text(data = bf_bias,
            aes(x = 1.45, y = 1.88, label = label_d),
            color = "black", size = 3.6, parse = TRUE)

# Increase axis text size
p_bias <-
  p_bias +
  theme(axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        strip.text = element_text(size=12, hjust = .5))

# Save plot 
p_bias

# Plot est error ----------------------------------------------------------

bf_rep <-
  get_bf_d(data = rep_data %>% 
             select(-cond_code) %>% 
             pivot_wider(names_from = bias_direction, values_from = value) %>% 
             group_by(bias_source) %>% 
             nest(),
           col_names = c('long', 'short')) %>% 
  
  mutate(d = round(d, 2),
         bf_format = case_when(bf > 100 ~ formatC(bf, format = 'e', digits = 1),
                               bf > 10 ~ as.character(round(bf)),
                               bf > 1 ~ as.character(round(bf, 1)),
                               bf < 1 ~ as.character(round(bf, 2)))) %>% 
  mutate(label_bf = paste0('bold(BF[10]) == "', bf_format, '"'),
         label_d = paste0('italic(d) == ', d)
  )

# Reproduction error per bias source and bias direction
p_rep <-
  rep_data %>% 
  mutate(bias_direction = factor(str_to_title(bias_direction), levels = str_to_title(unique(sdt_data$bias_direction)))) %>% 
  # Plot
  rep_base_plot() +
  # Customize plot
  theme(panel.spacing.x=unit(-1, "lines"),
        axis.title.y = element_textbox_simple(
          halign = .56,
          size=15,
          orientation = "left-rotated",
          margin = margin(0, 0, 0, 0))) +
  scale_color_manual(values = get_condition_colors()) +
  ylab('Reproduction error (pixels)<br><span style=\'font-size:9pt\'><- Underestimation - Overestimation  -></span>') +
  xlab('Bias direction') +
  ylim(-67.88, 60) +
  scale_x_discrete(expand=c(0.45, 0.1))

# Add effect sizes and save plot
p_rep <-
  p_rep +
  ggtitle('Reproduction error') + 
  geom_text(data = bf_rep,
            aes(x = 1.45, y = 57.88, label = label_d),
            color = "black", size = 3.6, parse = TRUE)


# Increase axis text size
p_rep <-
  p_rep +
  theme(axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        strip.text = element_text(size=12, hjust = .5))

# Save plot 
p_rep

# Plot bias delta ---------------------------------------------------------

# Get deltas
bias_delta <- 
  sdt_data %>% 
  select(-c(d, cond_code)) %>% 
  rename(value = ccrit) %>% 
  get_delta()

# Plot
p_delta_bias <-
  bias_delta %>%
  rep_delta_plot() +
  ylab('SDT criterion \u0394\n(bias to long - bias to short)') + 
  theme(axis.title.y = element_text(size = 9)) + 
  # Separate bias sources
  facet_wrap(. ~ bias_source, labeller = labeller(bias_source = labels_bias_source), scales = 'free_x', strip.position = 'bottom') +
  theme(panel.spacing.x=unit(-1, "lines"),
        axis.title.x=element_blank(),
        axis.title.y = element_text(size=15),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  scale_color_manual(values = get_condition_colors()) +
  scale_fill_manual(values = get_condition_colors()) +
  ylim(-1.6, 2.9555) +
  scale_x_discrete(expand=c(0.45, 0.2))

p_delta_bias <-
  p_delta_bias +
  geom_text(data = bf_bias, aes(x = 1, y = 2.89, label = label_bf),
            color = 'black', size = 2.9, parse = TRUE) +
  ggtitle('Categorization bias \u0394\ ')


# Increase axis text size
p_delta_bias <- p_delta_bias +
  theme(axis.text = element_text(size = 12),
        strip.text.x = element_text(size = 12)) 

p_delta_bias

# Plot reproduction delta -------------------------------------------------

# Get delta
rep_delta <- 
  rep_data %>% 
  get_delta()

# Plot
p_delta_rep <-
  rep_delta %>%
  rep_delta_plot() +
  ylab('Reproduction error \u0394 \n(bias to long - bias to short)') + 
  facet_wrap(. ~ bias_source, labeller = labeller(bias_source = labels_bias_source), scales = 'free_x', strip.position = 'bottom') +
  theme(
    axis.title.y = element_text(size = 15),
    panel.spacing.x=unit(-1, "lines"),
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank()) +
  scale_color_manual(values = c('mullerlyer' = gg_color_hue(3)[2], 'baserate' = gg_color_hue(3)[1], 'payoff' = gg_color_hue(3)[3])) +
  scale_fill_manual(values = c('mullerlyer' = gg_color_hue(3)[2], 'baserate' = gg_color_hue(3)[1], 'payoff' = gg_color_hue(3)[3])) +
  scale_x_discrete(expand=c(0.45, 0.4))

p_delta_rep <-
  p_delta_rep +
  geom_text(data = bf_rep, aes(x = 1, y = 63.4, label = label_bf),
            color = 'black', size = 2.9, parse = TRUE) +
  ggtitle('Reproduction error \u0394\ ')

# Increase axis text size
p_delta_rep <- p_delta_rep +
  theme(axis.text = element_text(size = 12),
        strip.text.x = element_text(size = 12))

p_delta_rep

# Combine plots -----------------------------------------------------------

(p_bias + p_rep) / (p_delta_bias + p_delta_rep) 

ggsave(here('plots/reproduction_experiment_main_results.png'),
       dpi = 1200, height = 9, width = 11, scale = .8)


# Sensitivity -------------------------------------------------------------

bf_sensitivity <- 
  get_bf_d(data = sdt_data %>% 
             select(-ccrit, -cond_code) %>% 
             rename(value = d) %>% 
             pivot_wider(names_from = bias_direction, values_from = value) %>% 
             group_by(bias_source) %>% 
             nest(),
           col_names = c('long', 'short')) %>% 
  mutate(d = round(d, 2),
         bf_format = case_when(bf > 100 ~ formatC(bf, format = 'e', digits = 1),
                               bf > 10 ~ as.character(round(bf)),
                               bf > 1 ~ as.character(round(bf, 1)),
                               bf < 1 ~ as.character(round(bf, 2)))) %>% 
  mutate(label_bf = paste0('bold(BF[10]) == "', bf_format, '"'),
         label_d = paste0('italic(d) == ', d)
  )

# Bias per bias source and bias direction
p_sensitivity <-
  sdt_data %>% 
  select(participant, cond_code, bias_source, bias_direction, d) %>% 
  mutate(bias_direction = factor(str_to_title(bias_direction), levels = str_to_title(unique(sdt_data$bias_direction)))) %>% 
  
  rename(value = d) %>% 
  # Plot
  rep_base_plot() +
  # Customize plot
  theme(panel.spacing.x=unit(-1, "lines"),
        axis.title.y = element_textbox_simple(
          halign = .56,
          size=15,
          orientation = "left-rotated",
          margin = margin(0, 0, 0, 0)
        )) +
  scale_color_manual(values = get_condition_colors()) +
  ylab('SDT d\'') +
  xlab('Bias direction') 

# Add effect sizes and save plot
p_sensitivity <-
  p_sensitivity +
  geom_text(data = bf_sensitivity,
            aes(x = 1.5, y = 4.5, label = label_bf),
            color = "black", size = 2.5, parse=TRUE) +
  geom_text(data = bf_sensitivity,
            aes(x = 1.5, y = 4.3, label = label_d),
            color = "black", size = 2.5, parse=TRUE)

p_sensitivity
ggsave(here('plots/reproduction_experiment_sensitivity.png'), width = 6, height = 4, scale = .8)

# Reproduction correlation ------------------------------------------------

## Read data ----

corr_rep_data <- read_csv(here('data/processed/rep_all_data_filtered.csv'))

# Get target lengths and reproductions
corr_rep_data <- 
  corr_rep_data %>% 
  filter(trial_type == 'reproduction') %>% 
  select(participant, bias_source, bias_direction, target_length, answer) %>% 
  mutate(answer = as.integer(abs(as.double(answer))),
         target_length = as.integer(target_length)) %>% 
  mutate(
    bias_source = factor(bias_source, levels = c('mullerlyer', 'baserate', 'payoff')),
    bias_direction = factor(bias_direction, levels = c('long', 'short')),
    cond_code = paste(bias_source, bias_direction, sep = '_'))

# Summarize length reproductions by target length
corr_rep_data <- corr_rep_data %>% 
  group_by(participant, cond_code, bias_source, bias_direction, target_length) %>% 
  summarise(answer = mean(answer))

## Get correlation -----

# Calculate BF and sample posterior
bf_corr_rep <-
  corr_rep_data %>% 
  group_by(bias_source, bias_direction, cond_code) %>% 
  nest() %>% 
  mutate(
    cor = map(data, ~correlationBF(x = .x$target_length, y=.x$answer)),
    cor_posterior = map(data, ~correlationBF(x = .x$target_length, y=.x$answer, posterior = TRUE, iterations = 1000))
  )

# Extract BF and corr coefficient, make labels
bf_corr_rep <- 
  bf_corr_rep %>% 
  mutate(
    bf = unlist(map(cor, ~describe_posterior(.x)$BF)),
    rho = mean(unlist(map(cor_posterior, ~.x[, 'rho'])))
  ) %>% 
  mutate(rho_format = str_remove(round(rho, 2), '0(?=\\.)'),
         bf_format = case_when(bf > 100 ~ formatC(bf, format = 'e', digits = 1),
                             bf > 10 ~ as.character(round(bf)),
                             bf > 1 ~ as.character(round(bf, 1)),
                             bf < 1 ~ as.character(round(bf, 2)))) %>%
  mutate(label_bf = paste0('bold(BF[10]) == "', bf_format, '"'),
         label_rho = paste('italic(r) == "', rho_format, '"')
  )


## Plot correlation ------

corr_rep_data %>% 
  ggplot(., aes(x=target_length, y=answer, color=cond_code)) +
  facet_grid(rows = vars(bias_direction), cols = vars(bias_source), 
             labeller = labeller(bias_source = labels_bias_source_sample, bias_direction = c('long'='Long', 'short'='Short'))) +
  geom_point(show.legend = FALSE, size=.4) +
  geom_hline(yintercept = 175*2, linetype=2) + 
  geom_vline(xintercept = 175*2, linetype=2) +
  geom_smooth(method=lm, linetype=1, color='black', fill='gray', size=.2) +
  scale_color_manual(values = get_condition_colors()) +
  geom_text(data=bf_corr_rep, aes(x=384, y=250, label=label_bf), color='black', size=2, parse = TRUE) +
  geom_text(data=bf_corr_rep, aes(x=385, y=225, label=label_rho), color='black', size=2.3, parse = TRUE) +
  ylab('Length reproduction') + xlab('Target length')

ggsave(here('plots/reproduction_experiment_correlation.png'), height = 4, width = 6, scale = .8)

