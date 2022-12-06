# Packages
if (!require('pacman', quietly = TRUE)) install.packages('pacman'); library('pacman', quietly = TRUE)
p_load(magrittr, purrr, dplyr, here, readr, tidyr, stringr, ggplot2, ggtext, patchwork)

source(here('functions/functions.R'))

# Data --------------------------------------------------------------------

# Read empirical data
empirical_data <- 
  read_csv(here('data/processed/ddm_arrows_200sd4.csv')) %>% 
  mutate(bias_direction = factor(bias_direction, levels = c('long', 'short')),
         addition_length = factor(addition_length, levels = c(30, 40, 50, 60)),
         cond_code = factor(paste0(bias_source, '_', bias_direction), levels = c('mullerlyer_long', 'mullerlyer_short'))) 

# Read ppc simulated data
simulated_data <-
  read_csv(here('data/ppc_sdt_arrowheads.csv')) %>% 
  mutate(bias_direction = factor(bias_direction, levels = c('long', 'short')),
         addition_length = factor(addition_length, levels = c(30, 40, 50, 60)),
         cond_code = factor(paste0(bias_source, '_', bias_direction), levels = c('mullerlyer_long', 'mullerlyer_short'))) 

# Traces
traces_data <- 
  read_csv(here('data/ddm_arrowheads_traces.csv')) %>% 
  mutate(bias_direction = factor(bias_direction, levels = c('long', 'short')),
         addition_length = factor(addition_length, levels = c(30, 40, 50, 60)),
         cond_code = factor(paste0(bias_source, '_', bias_direction), levels = c('mullerlyer_long', 'mullerlyer_short'))) 

# Get SDT -----------------------------------------------------------------

empirical_data_sdt <- 
  empirical_data %>% 
  mutate(accuracy = stim == response) %>% 
  mutate(across(c(stim, response), ~recode(.x, `1`='long', `0`='short'))) %>% 
  mutate(conf_mat = paste0(tolower(accuracy), '_', response)) %>%
  group_by(subj_idx, cond_code, bias_source, bias_direction, addition_length, conf_mat) %>%
  summarise(count = n(), 
            .groups = "keep") %>% 
  pivot_wider(data = ., names_from = conf_mat, 
              values_from = count, values_fill = list(count = 0)) %>% 
  # Get SDT values
  do(sdt_calc(hits = .$true_long, misses = .$false_short, 
              crrej = .$true_short, far = .$false_long)) %>% 
  # Transform to long data
  pivot_longer(names_to = 'key', values_to = 'value', cols = -c(subj_idx, cond_code, bias_source, bias_direction, addition_length))

# Summarize data ----------------------------------------------------------

# Behav data
summary_empirical_sdt <-
  empirical_data_sdt %>% 
  filter(key == 'ccrit') %>% 
  select(-key) %>% 
  rename(ccrit = value) %>% 
  group_by(cond_code, bias_source, bias_direction, addition_length) %>% 
  summarise(mean = mean(ccrit),
            se = se(ccrit))

# Simulated data
summary_simulated <- 
  simulated_data %>%
  group_by(cond_code, bias_source, bias_direction, addition_length) %>%
  summarise(crit_mean = mean(crit),
            crit_se = se(crit)) %>% 
  # For getting color from get_condition_colors()
  mutate(color = paste0('simulated_', bias_direction))

# Traces
summary_traces_dcz <-
  traces_data %>%
  filter(parameter %in% c('z', 'dc')) %>%
  group_by(parameter, cond_code, bias_source, bias_direction, addition_length) %>%
  summarise(mean = mean(value),
            se = se(value))

# Labels ------------------------------------------------------------------

bias_source_labels = c('mullerlyer' = 'Müller-Lyer')

param_labels = c(z = 'Starting point (z)', 
                 dc = 'Drift-criterion (dc)', 
                 a = 'Boundary sep. (a)',
                 t = 'Non-dec. time (t)',
                 v = 'Drift rate (v)')

# Plot: bias behavior ----------------------------------------------------

## Legend margins ---------------------

top_margin <- 3
bottom_margin <- -15
plot_margins <- c(0,.1,-10,.1)

# Plot
p_bias_empirical <-
  summary_empirical_sdt %>% 
  ggplot(., aes(x=addition_length, y=mean, color=cond_code)) +
  facet_wrap(. ~ bias_source, labeller = labeller(bias_source = bias_source_labels)) +
  geom_point(size=2, show.legend = FALSE) +
  geom_line(aes(group=bias_direction), show.legend = FALSE) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.3, show.legend = FALSE) +
  theme(
    plot.title.position = 'plot',
    plot.title = element_text(hjust = 1),
    axis.title.y = element_textbox_simple(
      size = 14,
      halign = .5,
      minwidth = 10,
      orientation = "left-rotated",
      margin = margin(0, 10, 0, -10)
    )) +
  guides(color='none') + 
  scale_color_manual(values=get_condition_colors(), limits=c('mullerlyer_long', 'mullerlyer_short'), labels=c('mullerlyer_short'='Short', 'mullerlyer_long'='Long')) + 
  ylab('SDT criterion<br><span style=\'font-size:11pt\'><- bias to "short" - bias to "long" -></span>') + 
  xlab('Arrowhead length (pixels)') +
  guides(color = guide_legend('Bias direction', title.hjust = -1), nrow = 1) +
  ggtitle('Decision bias by arrowhead\nlength and bias direction') + 
  ylim(-.5, 1.16)

p_bias_empirical

# Traces plot -------------------------------------------------------------

# Dummy data to adjust plot axis
dummy_data <-
  tibble( bias_direction = 'short', 
          addition_length = factor('30'), 
          parameter = c('dc', 'dc',
                        'z','z'), 
          mean = c(-.5, 1.15,
                   -.3,.3)
  )

# Plot
pd = position_dodge(width = .2)

# Plot traces
p_traces <-
  summary_traces_dcz %>% 
  ggplot(., aes(x = addition_length, y = mean, group = bias_direction, color = cond_code)) +
  facet_wrap(. ~ parameter, labeller = labeller(parameter = param_labels), scale = 'free') +
  geom_point(size=2, show.legend = TRUE)  +
  geom_line(show.legend = FALSE) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.35, show.legend = FALSE) +
  xlab('Arrowhead length (pixels)') +
  ylab('Parameter value') +
  geom_blank(data = dummy_data, aes(x=addition_length, y=mean),
             inherit.aes = FALSE) +
  scale_color_manual(name = 'Bias direction', values = get_condition_colors(), 
                     labels = c('long'='Long', 'short'='Short'), breaks = c('short', 'long')) +
  theme(
    panel.spacing.x=unit(.2, "lines"),
    axis.title.y = element_text(size=14),
  ) + 
  ggtitle('DDM parameter estimation\nby arrowhead length and bias direction') +
  guides(color = guide_legend(ncol = 2, nrow = 1, byrow = TRUE)) 


# Make behavioral bias and DDM traces plot --------------------------

(p_bias_empirical + p_traces) +
  plot_layout(widths = c(.5, 1)) +
  plot_layout(guides = 'collect') & 
  theme(legend.position = 'bottom',
        plot.title = element_text(size=15),
        strip.text = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.text.x = element_text(size=12),
        axis.title.x = element_text(margin = margin(9, 0, -7 , 0), size=14),
        plot.margin = unit(c(.1, .1, .2, .1), 'cm')
  )

ggsave(here('plots/ddm_arrowheads_main_figure.png'), height =5, width = 12, scale = .7, dpi=300)

# Simulated arrowhead bias data ---------------------------------------------

# Add simulated data to arrowhead 
p_bias_empirical +
  geom_errorbar(data=summary_simulated,
                aes(x=addition_length, ymin=crit_mean-crit_se, ymax=crit_mean+crit_se, y=crit_mean, color=color), 
                width=.3, position = position_nudge(x=.1), alpha = .8, show.legend = FALSE) +
  geom_point(data=summary_simulated, 
             aes(x=addition_length, y=crit_mean, color=color), 
             alpha = .5, size=2, position = position_nudge(x=.1), show.legend = FALSE) +
  scale_color_manual(values=get_condition_colors(),
                     limits=c('mullerlyer_long', 'mullerlyer_short', 'simulated_long', 'simulated_short'), labels=c('mullerlyer_short'='Short', 'mullerlyer_long'='Long')) +
  theme(legend.position = 'bottom',
        plot.title = element_text(size=15),
        strip.text = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.text.x = element_text(size=12),
        axis.title.x = element_text(margin = margin(9, 0, -7 , 0), size=14),
        plot.margin = unit(c(.1, .1, .2, .1), 'cm')
  )

# Save empirical+predicted data plot
ggsave(here('plots/ddm_arrowheads_bias_predicted.png'), height = 4, width = 4, scale = .8)


  