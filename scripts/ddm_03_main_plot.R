# Packages
if (!require('pacman', quietly = TRUE)) install.packages('pacman'); library('pacman', quietly = TRUE)
p_load(magrittr, purrr, dplyr, here, readr, tidyr, stringr, ggplot2, ggtext, patchwork)

source(here('functions/functions.R'))

# Data --------------------------------------------------------------------

# Get files that went into HDDM
files <-
  dir(here('data/processed/'), pattern = 'ddm_exp', full.names = TRUE)

# Read data
dat <- 
  map(files, ~ read_csv(.x) %>% mutate(experiment = str_extract(string = .x, 'exp[123]'))) %>% 
  bind_rows()

# Format data -------------------------------------------------------------

# Create condition code column and convert some columns to factor
# so the conditions are in the right order in the plot
dat <- 
  dat %>% 
  mutate(cond_code = paste0(bias_source, '_', bias_direction),
         bias_source = factor(bias_source, levels = c('mullerlyer', 'baserate', 'payoff')),
         bias_direction = factor(bias_direction, levels = c('long', 'short')),
         experiment = factor(experiment, levels = c('exp1', 'exp2', 'exp3'))) %>% 
  mutate(cond_code = factor(cond_code, levels = c('mullerlyer_long', 'mullerlyer_short', 'baserate_long', 'baserate_short', 'payoff_long', 'payoff_short')))

# Labels ------------------------------------------------------------------

bias_source_labels = c('mullerlyer' = 'Müller-Lyer',
                       'baserate' = 'Base-rate',
                       'payoff' = 'Payoff')

cond_code_labels = c('mullerlyer_long' = 'Müller-Lyer\nLong', 
                     'mullerlyer_short' = 'Müller-Lyer\nShort', 
                     'baserate_long' = 'Base-rate\nLong', 
                     'baserate_short' = 'Base-rate\nShort', 
                     'payoff_long' = 'Payoff\nLong`', 
                     'payoff_short' = 'Payoff\nShort')

param_labels = c(z = 'Starting point (z)',
                 dc = 'Drift-criterion (dc)',
                 a = 'Boundary sep. (a)',
                 t = 'Non-dec. time (t)',
                 v = 'Drift rate (v)')

experiment_labels <- c('exp1' = '1',
                       'exp2' = '2',
                       'exp3' = '3')

# RT quantiles ------------------------------------------------------------

# Get RT quantiles by subject and conditions
rt_quantile_data <- 
  dat %>%
  group_by(., experiment, subj_idx, bias_source, bias_direction, cond_code) %>% 
  mutate(quantile = findInterval(rt, quantile(rt, probs=c(.1, .3, .5, .7))))

# Get SDT values for each participant, quantile and condition
rt_quantile_data <- 
  rt_quantile_data %>% 
  ungroup() %>%
  mutate(accuracy = stim == response) %>% 
  mutate(across(c(stim, response), ~recode(.x, `1`='long', `0`='short'))) %>% 
  # Make confusion matrix columns
  mutate(conf_mat = paste0(tolower(accuracy), '_', response)) %>%
  group_by(experiment, subj_idx, cond_code, bias_source, bias_direction, quantile, conf_mat) %>%
  summarise(count = n(), 
            .groups = "keep") %>% 
  pivot_wider(data = ., names_from = conf_mat, 
              values_from = count, values_fill = list(count = 0)) %>% 
  # Get SDT values
  do(sdt_calc(hits = .$true_long, misses = .$false_short, 
              crrej = .$true_short, far = .$false_long)) %>% 
  # Transform to long data
  pivot_longer(names_to = 'key', values_to = 'value', cols = -c(experiment, subj_idx, bias_source, bias_direction, cond_code, quantile))

# Keep only bias and sensitivity measures
rt_quantile_data <- 
  rt_quantile_data %>% 
  filter(key == 'ccrit') %>% 
  pivot_wider(names_from = key, values_from = value) %>% 
  rename(crit = ccrit) %>% 
  mutate(quantile = as.integer(quantile) + 1)

# Summarize bias by rt data
rt_quantile_data_summary <- 
  rt_quantile_data %>% 
  group_by(experiment, cond_code, bias_source, bias_direction, quantile) %>% 
  summarise(mean = mean(crit),
            se = se(crit)) %>% 
  ungroup()

## RT plot --------------------------

# The points are plot separately baserate and then for other two conditions
# because the size/width is relative to the number of groups that are being 
# dodged. Instead each condition size is set manually by plotting them on 
# different geom_point() calls.

dodge = position_dodge(width = .7)

p_rt_empirical <-
  rt_quantile_data_summary %>% 
  ggplot(., aes(x = quantile, y = mean, shape = experiment, color = cond_code)) + 
  facet_wrap(. ~ bias_source, ncol = 1,
             labeller = labeller(bias_source = bias_source_labels)) +
  # Plot points (first baserate then muller-lyer and payoff)
  geom_point(data=rt_quantile_data_summary %>% filter(bias_source=='baserate'), size=1) +
  geom_point(data=rt_quantile_data_summary %>% filter(bias_source!='baserate'), size=1, position = dodge) +
  # Plot lines (first baserate then muller-lyer and payoff)
  geom_line(data=rt_quantile_data_summary %>% filter(bias_source=='baserate'), aes(linetype=experiment), size=.4, show.legend = TRUE) +
  geom_line(data=rt_quantile_data_summary %>% filter(bias_source!='baserate'), aes(linetype=experiment), size=.4, position = dodge, show.legend = TRUE) +
  # Plot error bars (first baserate then muller-lyer and payoff)
  geom_errorbar(data=rt_quantile_data_summary %>% filter(bias_source=='baserate'), aes(ymin=mean-se, ymax=mean+se), size = .5, width = .33) +
  geom_errorbar(data=rt_quantile_data_summary %>% filter(bias_source!='baserate'), aes(ymin=mean-se, ymax=mean+se), size = .5, width = 1, position = dodge) +
  # Set colors
  scale_color_manual(values = get_condition_colors()) +
  # Remove legends
  guides(colour = 'none',
         shape='none',
         linetype = 'none') +
  # Axis and plot title
  ylab('SDT criterion<br><span style=\'font-size:9pt\'><- bias to "short" - bias to "long" -></span>') +
  xlab('RT quantiles') + ggtitle('Bias by RT quantile') +
  # Extra formatting for combining with other plots
  theme(plot.title = element_text(hjust=0.8),
        plot.title.position = 'plot',
        axis.title.x = element_text(margin = margin(-11, 0, 0, 0)),
        axis.title.y = element_textbox_simple(
          halign = .5,
          minwidth = 10,
          orientation = "left-rotated",
          margin = margin(0, 10, 0, -10))
  ) 

# Traces ------------------------------------------------------------------

# Read DDM traces_data
traces_data <- 
  read_csv(here('data/ddm_full_model_traces.csv'))

# Format data
traces_data <- 
  traces_data %>% 
  mutate(cond_code = case_when(is.na(bias_direction) ~ bias_source,
                               TRUE ~ paste0(bias_source, '_', bias_direction)),
         bias_source = factor(bias_source, levels = c('mullerlyer', 'baserate', 'payoff')),
         bias_direction = factor(bias_direction, levels = c('long', 'short')),
         experiment = factor(experiment, levels = c('exp1', 'exp2', 'exp3')),
         parameter = factor(parameter, levels = c('a', 't', 'v', 'dc', 'z')))

# traces_data
traces_data_summary <-
  traces_data %>% 
  filter(parameter %in% c('z', 'dc')) %>%
  group_by(experiment, parameter, cond_code, bias_source, bias_direction) %>%
  summarise(mean = mean(value),
            se = se(value)
  )

## Plot ----------------------------

# Dummy data to use with geom blank to manually change
# y limits of each panel
dummy_data_traces_plot <-
  tibble(bias_source = 'mullerlyer', 
         bias_direction = 'short',
         experiment = 'exp1',
         parameter = c('dc', 'dc','z','z'), 
         mean = c(-.5, 1.1,-.5, .5))

p_traces <-
  traces_data_summary %>% 
  # Base
  ggplot(., aes(x=bias_direction, y = mean, fill=bias_direction)) +
  # Separate parameters
  facet_wrap(. ~ parameter, scales = 'free_y', labeller = labeller(parameter = param_labels)) +
  # Plot lines, points and errorbars
  geom_line(aes(color=bias_source, group=interaction(experiment, bias_source), linetype=experiment),
            position = position_dodge(width = .5), show.legend = TRUE) +
  geom_point(aes(shape=experiment, color=cond_code,
                 group=interaction(experiment, bias_source, bias_direction)),
             position = position_dodge(width = .5),
             show.legend = TRUE) +
  geom_errorbar(aes(ymin=mean-se,ymax=mean+se,color=cond_code,
                    group=interaction(experiment, bias_source, bias_direction)),
                position = position_dodge(width = .5), 
                width=1, show.legend = FALSE) +
  # Plot and axis titles
  ylab('Parameter mean<br><span style=\'font-size:9pt\'><- bias to "short" - bias to "long" -></span>') +
  xlab('Bias direction') + ggtitle('DDM parameter estimation') +
  # Set colors and fix legend
  scale_color_manual(name = 'Bias source', 
                     values = get_condition_colors(), 
                     labels = bias_source_labels, 
                     breaks = c('mullerlyer', 'baserate', 'payoff')) + 
  scale_fill_manual(name = 'Bias direction',
                    values = c('long'= 'black', 'short'=lighten('black',.6)),
                    labels = c('long'='Long', 'short'='Short')) +
  scale_linetype_manual(values=c('exp1'=1, 'exp2'=2, 'exp3'=3)) +
  scale_x_discrete(labels = c('Long', 'Short')) +
  scale_shape_discrete(labels=experiment_labels) +
  # Format legends 
  guides(fill = guide_legend(title.position = "top", order=3, ncol=1, nrow = 2, byrow = TRUE, override.aes = list(linetype=c(0,0), color=c('black', lighten('black',.6)))),
         shape = guide_legend(title.position = "top", order=1, ncol=2, nrow = 2, byrow = TRUE, override.aes = list(linetype=c(1,2,3))),
         color = guide_legend(title.position = "top", order=2, ncol=2, nrow = 2, byrow = TRUE, override.aes = list(linetype=c(0,0,0))),
         linetype = 'none') +
  labs(shape = 'Experiment') +
  # Further format legend for combining with other plots later
  theme(legend.position='bottom',
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 7),
        legend.key.width = unit(.6,"cm"), 
        legend.key.height = unit(.2,"cm"), 
        legend.direction = "horizontal",
        legend.spacing.x = unit(.1, 'cm'),
        legend.spacing.y = unit(.2, 'cm'),
        legend.box = "horizontal",
        legend.box.margin=margin(-10,-20,-10,-20),
        axis.title.y = element_textbox_simple(
          halign = .5,
          minwidth = 10,
          orientation = "left-rotated",
          margin = margin(0, 10, 0, -10)
        )
  )  + 
  # Change each panel limits
  geom_blank(data = dummy_data_traces_plot) 

# Traces delta ------------------------------------------------------------

# Get delta
traces_delta_data <-
  traces_data %>% 
  filter(parameter %in% c('dc', 'z')) %>%
  select(-c(node, condition, cond_code)) %>%
  group_by(experiment, parameter, bias_source, bias_direction) %>% 
  mutate(index = seq(1:n())) %>% 
  pivot_wider(names_from = bias_direction, values_from = value) %>% 
  unnest(c(short, long)) %>%
  mutate(delta = long - short) %>%
  select(parameter, experiment, bias_source, delta) %>% 
  mutate(bias_source = factor(bias_source, levels = c('mullerlyer', 'baserate', 'payoff')))

# Traces delta summary
traces_delta_data_summary <- 
  traces_delta_data %>% 
  group_by(parameter, experiment, bias_source) %>% 
  summarise(
    se = se(delta),
    delta = mean(delta)
  )

## Plot  -----------------------------

# Conditions are plot separately, baserate and then for other two conditions
# because the size/width is relative to the number of groups that are being 
# dodged. Instead each condition size is set manually by plotting them on 
# different geom_point() calls.

# Dummy data to use with geom blank to manually change
# y limits of each panel
dc_y_min <- 0
dc_y_max <- 1.7
z_y_min <- -.5
z_y_max <- .65

dummy_data_traces_delta_plot <-
  tibble(bias_source = 'mullerlyer', 
         bias_direction = 'short',
         experiment = c(rep('exp1', 4), rep('exp2', 4), rep('exp3', 4)),
         parameter = c('dc', 'dc','z','z', 
                       'dc', 'dc','z','z',
                       'dc', 'dc','z','z'),
         delta = c(dc_y_min, dc_y_max,z_y_min, z_y_max, 
                   dc_y_min, dc_y_max,z_y_min, z_y_max,
                   dc_y_min, dc_y_max,z_y_min, z_y_max)
  )

# Dodge setting for overlaping violins
pd <- position_dodge(width = .6)

p_traces_delta <-
  traces_delta_data %>% 
  ggplot(., aes(x=bias_source, y=delta)) +
  facet_wrap(. ~ parameter, 
             labeller = labeller(experiment = experiment_labels, 
                                 parameter = param_labels,
                                 .multi_line = FALSE),
             ncol = 2, scales = 'free_y') +
  # Violin plot: first muller-lyer and payoff, then base rate
  geom_violin(data=traces_delta_data %>% filter(bias_source!='baserate'), 
              aes(fill=bias_source, 
                  group=interaction(experiment, bias_source)),
              position = pd, scale='count', lwd=.2, width=.9) +
  geom_violin(data=traces_delta_data %>% filter(bias_source=='baserate'), 
              aes(fill=bias_source, 
                  group=interaction(experiment, bias_source)),
              position = pd, lwd=.2, width=.3) +
  # Error bras plot: first muller-lyer and payoff, then base rate
  geom_errorbar(data=traces_delta_data_summary %>% filter(bias_source!='baserate'),
                aes(group=interaction(experiment, bias_source),
                    ymax=delta+se,ymin=delta-se,x=bias_source),
                position=pd, width=.5, size=.5) +
  geom_errorbar(data=traces_delta_data_summary %>% filter(bias_source=='baserate'),
                aes(group=interaction(experiment, bias_source),
                    ymax=delta+se,ymin=delta-se,x=bias_source),
                position=pd, width=.2, size=.5) +
  # All conditions means 
  geom_point(data=traces_delta_data_summary,
             aes(shape=experiment, 
                 group=interaction(experiment, bias_source)), 
             position = pd) +
  # Set colors
  scale_fill_manual(breaks = c('mullerlyer', 'baserate', 'payoff'), 
                    values = get_condition_colors(), labels = bias_source_labels) +
  # Set axis titles and values
  scale_x_discrete(labels = bias_source_labels, limits=c('mullerlyer', 'baserate', 'payoff')) + 
  
  # Blank layer to adjust y axis limits
  geom_blank(data = dummy_data_traces_delta_plot, show.legend = FALSE) +
  # Remove legend
  guides(
    fill = 'none',
    shape = 'none',
    color = 'none') +
  theme(
    panel.spacing.x=unit(.1, "lines"),
    strip.text = element_text(size=9),
    axis.text.x = element_text(size=10, angle=40),
    axis.title.x = element_text(margin = margin(-10, 0, 0, 0)),
    axis.title.y = element_textbox_simple(
      halign = .5,
      minwidth = 10,
      orientation = "left-rotated",
      margin = margin(0, 10, 0, -10)
    )
  ) +
  # Axis and plot titles
  ylab('DDM Parameter \u0394\ <br><span style=\'font-size:9pt\'>(bias to long - bias to short)</span>') +
  xlab('Bias source') + ggtitle('DDM parameter \u0394')

# Combine plots -----------------------------------------------------------

((p_rt_empirical) + (p_traces/p_traces_delta)) + plot_layout(widths = c(.4, 1))
ggsave(here('plots/ddm_main_figure.png'), width = 9, height = 9, scale = .7, dpi=1200)                                                                                                          

# Supplementary figures ---------------------------------------------------

## Predicted RT quantile data ----------------------------------------------

### Data ----------------------------

# Get simulated data SDT by quantile
ppc_data_rt <- 
  dir(here('data/'), pattern = 'ppc_sdt_sirt', full.names = TRUE) %>% 
  map(~read_csv(.x)) %>% 
  bind_rows()

# Summary
ppc_data_rt_summary <- 
  ppc_data_rt %>%
  group_by(experiment, bias_source, bias_direction, quantile) %>% 
  summarise(mean = mean(crit), se = sd(crit)/sqrt(length(crit))) %>% 
  ungroup()

# Format columns
ppc_data_rt_summary <- 
  ppc_data_rt_summary %>% 
  mutate(bias_source = factor(bias_source, levels = c('mullerlyer', 'payoff',  'baserate')),
         experiment = factor(experiment, levels = c('exp1', 'exp2', 'exp3')),
         cond_code = paste0(bias_source, '_', bias_direction))

### Plot --------------------------

# Same dodge as RT plot
dodge = position_dodge(width = .7)

p_rt_empirical +
  facet_wrap(. ~ bias_source, nrow = 1,
             labeller = labeller(bias_source = bias_source_labels)) +
  # Plot base rate data
  geom_errorbar(
    data = ppc_data_rt_summary %>% filter(bias_source=='baserate'), 
    aes(ymax=mean+se, ymin=mean-se, 
        group=interaction(experiment, bias_direction)),
    size = .3, width = .4, color="darkviolet", alpha=.6) +
  geom_point(
    data = ppc_data_rt_summary %>% filter(bias_source=='baserate'),
    aes(shape=experiment), color='darkviolet', size=1.5, alpha=.6) +
  # Plot Muller-Lyer and payoff
  geom_errorbar(
    data = ppc_data_rt_summary %>% filter(bias_source!='baserate'), 
    aes(ymax=mean+se, ymin=mean-se, x=quantile, y=mean),
    color="darkviolet", alpha=.6, linetype=1, position=dodge, width=1) +
  geom_point(
    data = ppc_data_rt_summary %>% filter(bias_source!='baserate'),
    aes(shape=experiment, x=quantile, y=mean),
    color='darkviolet', alpha=.6, position=dodge) +
  
  # Recover legend
  guides(shape=guide_legend(title = "Experiment",
                            override.aes = list(color = 'black')),
         linetype=guide_legend(title = 'Experiment'),
         colour = guide_legend(ncol = 2, nrow = 1, byrow = TRUE, 
                               override.aes = list(colour=c('black', 
                                                            lighten('black', .6)), 
                                                   linetype=c(0,0)))) +
  # Relabel experiment
  scale_shape_discrete(labels=c('exp1'='1', 'exp2'='2', 'exp3'='3')) +
  scale_linetype_discrete(labels=c('exp1'='1', 'exp2'='2', 'exp3'='3')) +
  # Manually modify color legend to indicate bias direction
  scale_color_manual(name = 'Bias direction', 
                     values = get_condition_colors(), 
                     breaks = c('mullerlyer_long', 'mullerlyer_short'),
                     labels = c('Long', 'Short')) + 
  # Reposition legend
  theme(legend.position = 'bottom',
        axis.title.x = element_text(margin = margin(2, 0, 0, 0)),
        plot.title = element_text(hjust=0),
        plot.title.position = 'panel') 

ggsave(here('plots/ddm_rt_predicted_data.png'), width = 6, height = 4, scale = .95)

## Bias and sensitivity all experiments ----------------

### Data -----------------------

# Get simulated data SDT by quantile
ppc_data_nort <- 
  dir(here('data/'), pattern = 'ppc_sdt_nort', full.names = TRUE) %>% 
  map(~read_csv(.x)) %>% 
  bind_rows() %>% 
  mutate(bias_source = factor(bias_source, levels = c('mullerlyer', 'payoff',  'baserate')),
         experiment = factor(experiment, levels = c('exp1', 'exp2', 'exp3')),
         cond_code = paste0(bias_source, '_', bias_direction)) %>%
  mutate(cond_code = factor(cond_code, levels = c('mullerlyer_long', 'mullerlyer_short', 'baserate_long', 'baserate_short', 'payoff_long', 'payoff_short'))) %>% 
  # For getting color from get_condition_colors()
  mutate(color = paste0('simulated_', bias_direction))

# Get SDT from empirical data
sdt_data <- 
  dat %>% 
  mutate(accuracy = stim == response) %>% 
  mutate(across(c(stim, response), ~recode(.x, `1`='long', `0`='short'))) %>% 
  # Make confusion matrix columns
  mutate(conf_mat = paste0(tolower(accuracy), '_', response)) %>%
  group_by(experiment, subj_idx, cond_code, bias_source, bias_direction, conf_mat) %>%
  summarise(count = n(), 
            .groups = "keep") %>% 
  pivot_wider(data = ., names_from = conf_mat, 
              values_from = count, values_fill = list(count = 0)) %>% 
  # Get SDT values
  do(sdt_calc(hits = .$true_long, misses = .$false_short, 
              crrej = .$true_short, far = .$false_long)) %>% 
  # Transform to long data
  pivot_longer(names_to = 'key', values_to = 'value', cols = -c(experiment, subj_idx, bias_source, bias_direction, cond_code)) %>% 
  ungroup()

# Get BF and effect size
bf_sdt_data <- 
  sdt_data %>% 
  filter(key %in% c('ccrit', 'd')) %>% 
  select(-cond_code) %>% 
  pivot_wider(names_from = bias_direction, values_from = value) %>% 
  group_by(experiment, bias_source, key) %>% 
  nest() %>% 
  get_bf_d(col_names = c('short', 'long')) %>% 
  mutate(d = round(d, 2),
         bf_format = case_when(bf > 100 ~ formatC(bf, format = 'e', digits = 1),
                               bf > 10 ~ as.character(round(bf)),
                               bf > 1 ~ as.character(round(bf, 1)),
                               bf < 1 ~ as.character(round(bf, 2)))) %>% 
  mutate(label_bf = paste0('bold(BF[10]) == "', bf_format, '"'),
         label_d = paste0('italic(d) == ', d),
         full_label = paste0('atop(', label_bf, ', ', label_d,')')
  )

# Format data to long
sdt_data <- 
  sdt_data %>% 
  filter(key %in% c('ccrit', 'd')) %>% 
  pivot_wider(names_from = key, values_from = value)

### Summarize data --------------------------

sdt_data_summary <-
  sdt_data %>% 
  group_by(experiment, cond_code, bias_source, bias_direction) %>% 
  summarise(mean_crit = mean(ccrit), se_crit = se(ccrit),
            mean_dprime = mean(d), se_dprime = se(d))

ppc_data_nort_summary <- 
  ppc_data_nort %>% 
  group_by(experiment, cond_code, bias_source, bias_direction, color) %>% 
  summarise(mean_crit = mean(crit), se_crit = se(crit),
            mean_dprime = mean(dprime), se_dprime = se(dprime))

### Slightly different labels -----------------

experiment_labels <- c('exp1' = 'Experiment 1',
                       'exp2' = 'Experiment 2',
                       'exp3' = 'Experiment 3')

### Plot criterion ----------------------------

p_crit <-
  sdt_data %>%
  ggplot(aes(x=cond_code, y = ccrit)) +
  facet_wrap(. ~ experiment, ncol = 1, labeller = labeller(experiment=experiment_labels), scales='free_x') + 
  # Plot empirical subject averages
  geom_line(aes(group=interaction(bias_source, subj_idx), color=bias_source), show.legend = FALSE) +
  geom_point(aes(color=cond_code), show.legend = FALSE) +
  # Plot empirical condition summary
  geom_point(data=sdt_data_summary, aes(x=cond_code, y=mean_crit)) +
  geom_errorbar(data=sdt_data_summary, aes(x=cond_code, ymax=mean_crit + se_crit, ymin=mean_crit - se_crit, y=mean_crit), width=.2) +
  # Plot simulated condition summary
  geom_point(data=ppc_data_nort_summary %>% filter(bias_direction=='long'), aes(x=cond_code, y=mean_crit, color=color), position = position_nudge(x=-.23)) +
  geom_point(data=ppc_data_nort_summary %>% filter(bias_direction=='short'), aes(x=cond_code, y=mean_crit, color=color), position = position_nudge(x=.23)) +
  # Error-bars (simulated data) are plotted separately for experiment with 
  # base rate because the width of error bars is relative to the number of
  # levels on the x asis
  geom_errorbar(data=ppc_data_nort_summary %>% filter(bias_direction=='long', experiment!='exp3'), aes(x=cond_code, ymax=mean_crit+se_crit, ymin=mean_crit-se_crit, y=mean_crit, color=color), width=.15, position = position_nudge(x=-.23)) +
  geom_errorbar(data=ppc_data_nort_summary %>% filter(bias_direction=='short', experiment!='exp3'), aes(x=cond_code, ymax=mean_crit+se_crit, ymin=mean_crit-se_crit, y=mean_crit, color=color), width=.15, position = position_nudge(x=.23)) +
  geom_errorbar(data=ppc_data_nort_summary %>% filter(bias_direction=='long', experiment=='exp3'), aes(x=cond_code, ymax=mean_crit+se_crit, ymin=mean_crit-se_crit, y=mean_crit, color=color), width=.2, position = position_nudge(x=-.23)) +
  geom_errorbar(data=ppc_data_nort_summary %>% filter(bias_direction=='short', experiment=='exp3'), aes(x=cond_code, ymax=mean_crit+se_crit, ymin=mean_crit-se_crit, y=mean_crit, color=color), width=.2, position = position_nudge(x=.23)) +
  # Plot bf and effect sizes
  geom_text(data=bf_sdt_data %>% filter(key=='ccrit', experiment!='exp3'), aes(label=full_label, x=as.integer(bias_source)+.5, y=1.8), parse=TRUE) +
  geom_text(data=bf_sdt_data %>% filter(key=='ccrit', experiment=='exp3'), aes(label=full_label, x=(as.integer(bias_source)*2)-.5, y=1.8), parse=TRUE) +
  # Formatting colors and legend
  scale_color_manual(name = 'Bias direction', 
                     values = get_condition_colors(), 
                     breaks = c('mullerlyer_long', 'mullerlyer_short'),
                     labels = c('Long', 'Short')) +
  # Force black and grey on legend
  guides(colour = guide_legend(ncol = 2, nrow = 1, byrow = TRUE, override.aes = list(colour=c('black', lighten('black', .6)), linetype=c(0,0)))) +
  # Move legend
  theme(legend.position = 'bottom', legend.direction = 'horizontal')  +
  # Apply condition labels
  scale_x_discrete(labels=cond_code_labels) + xlab('Condition') + ylab('Bias (SDT criterion)') + ggtitle('Categorization bias') + ylim(-1.5, 2.5)

p_crit

### Plot d' ----------------------------

p_dprime <-
  sdt_data %>%
  ggplot(aes(x=cond_code, y = d)) +
  facet_wrap(. ~ experiment, ncol = 1, labeller = labeller(experiment=experiment_labels), scales='free_x') + 
  # Plot empirical subject averages
  geom_line(aes(group=interaction(bias_source, subj_idx), color=bias_source), show.legend = FALSE) +
  geom_point(aes(color=cond_code), show.legend = FALSE) +
  # Plot empirical condition summary
  geom_point(data=sdt_data_summary, aes(x=cond_code, y=mean_dprime)) +
  geom_errorbar(data=sdt_data_summary, aes(x=cond_code, ymax=mean_dprime + se_dprime, ymin=mean_dprime - se_dprime, y=mean_dprime), width=.2) +
  # Plot simulated condition summary
  geom_point(data=ppc_data_nort_summary %>% filter(bias_direction=='long'), aes(x=cond_code, y=mean_dprime, color=color), position = position_nudge(x=-.23)) +
  geom_point(data=ppc_data_nort_summary %>% filter(bias_direction=='short'), aes(x=cond_code, y=mean_dprime, color=color), position = position_nudge(x=.23)) +
  # Error-bars (simulated data) are plotted separately for experiment with 
  # base rate because the width of error bars is relative to the number of
  # levels on the x asis
  geom_errorbar(data=ppc_data_nort_summary %>% filter(bias_direction=='long', experiment!='exp3'), aes(x=cond_code, ymax=mean_dprime+se_dprime, ymin=mean_dprime-se_dprime, y=mean_dprime, color=color), width=.15, position = position_nudge(x=-.23)) +
  geom_errorbar(data=ppc_data_nort_summary %>% filter(bias_direction=='short', experiment!='exp3'), aes(x=cond_code, ymax=mean_dprime+se_dprime, ymin=mean_dprime-se_dprime, y=mean_dprime, color=color), width=.15, position = position_nudge(x=.23)) +
  geom_errorbar(data=ppc_data_nort_summary %>% filter(bias_direction=='long', experiment=='exp3'), aes(x=cond_code, ymax=mean_dprime+se_dprime, ymin=mean_dprime-se_dprime, y=mean_dprime, color=color), width=.2, position = position_nudge(x=-.23)) +
  geom_errorbar(data=ppc_data_nort_summary %>% filter(bias_direction=='short', experiment=='exp3'), aes(x=cond_code, ymax=mean_dprime+se_dprime, ymin=mean_dprime-se_dprime, y=mean_dprime, color=color), width=.2, position = position_nudge(x=.23)) +
  # Plot bf and effect sizes
  geom_text(data=bf_sdt_data %>% filter(key=='d', experiment!='exp3'), aes(label=full_label, x=as.integer(bias_source)+.5, y=4), parse=TRUE) +
  geom_text(data=bf_sdt_data %>% filter(key=='d', experiment=='exp3'), aes(label=full_label, x=(as.integer(bias_source)*2)-.5, y=4), parse=TRUE) +
  # Formatting colors and legend
  scale_color_manual(name = 'Bias direction', 
                     values = get_condition_colors(), 
                     breaks = c('mullerlyer_long', 'mullerlyer_short'),
                     labels = c('Long', 'Short')) +
  # Force black and grey on legend
  guides(colour = guide_legend(ncol = 2, nrow = 1, byrow = TRUE, override.aes = list(colour=c('black', lighten('black', .6)), linetype=c(0,0)))) +
  # Move legend
  theme(legend.position = 'bottom', legend.direction = 'horizontal')  +
  # Apply condition labels
  scale_x_discrete(labels=cond_code_labels) + xlab('Condition') + 
  ylab('Sensitivity (SDT d\')') + ggtitle('Categorization sensitivity') +
  ylim(0, 5)

p_dprime

### Combine plots ------------------

(p_crit + p_dprime) +
  plot_layout(guides = 'collect') & theme(legend.position = 'bottom')

ggsave(here('plots/ddm_supplementary_sdt.png'), width = 9, height = 7, scale = .95)


# All DDM parameters full posterior distributions -------------------------

traces_data %>% 
  ggplot(aes(y=value)) +
  facet_wrap(. ~ experiment + parameter, scales = 'free', nrow = 3, labeller = labeller(experiment = experiment_labels, parameter = param_labels, .multi_line = TRUE)) +
  geom_density(aes(group = cond_code, color = cond_code, fill = cond_code), alpha=.3) +
  scale_color_manual(name = 'Bias source', 
                     values = get_condition_colors(), 
                     breaks = c('mullerlyer', 'baserate', 'payoff'),
                     labels = bias_source_labels) +
  scale_fill_manual(name = 'Bias direction', 
                    values = get_condition_colors(), 
                    breaks = c('mullerlyer_long', 'mullerlyer_short'),
                    labels = c('Long', 'Short')) +
  guides(fill = guide_legend(ncol = 2, nrow = 1, byrow = TRUE, override.aes = list(fill=c('black', lighten('black', .6)), color=c('black', lighten('black', .6)))),
         color = guide_legend(override.aes = list(fill = get_condition_colors()[c('mullerlyer', 'baserate', 'payoff')], color=get_condition_colors()[c('mullerlyer', 'baserate', 'payoff')]))) +
  theme(legend.position = 'bottom') + 
  ylab('Parameter value') + xlab('Density')

ggsave(here('plots/ddm_main_full_posterior_distributions.png'), width = 9, height = 6)


