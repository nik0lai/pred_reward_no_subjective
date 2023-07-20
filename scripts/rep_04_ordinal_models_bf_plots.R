# Packages
if (!require('pacman', quietly = TRUE)) install.packages('pacman'); library('pacman', quietly = TRUE)
p_load(magrittr, purrr, dplyr, here, readr, tidyr, ggplot2, patchwork)


# Simple models -----------------------------

# All models are compared against the null model

## Data decision -------

bfs_decision <- read_csv(here('data/processed/rep_ordinal_models_bf_decision.csv'))

# Compare all simple models against null
bfs_decision_simple_models <- 
  bfs_decision %>% 
  filter(nchar(model) == 1 | model == 'UN') %>% 
  mutate(baseline = bfs_decision$bf[bfs_decision$model == 'UN']) %>% 
  mutate(bf_over_null_log = bf - baseline,
         bf_over_null_raw = exp(bf_over_null_log)) %>%
  arrange(desc(bf_over_null_raw)) 

bfs_decision_simple_models %>% 
  mutate(bf_over_null_raw = format(bf_over_null_raw, scientific = FALSE))

bfs_decision_simple_models %>% 
  mutate(bf_over_null_raw = formatC(bf_over_null_raw, digits = 1, format = 'e'))

## Data reproduction ----------------------------

bfs_reproduction <- read_csv(here('data/processed/rep_ordinal_models_bf_reproduction.csv'))

# Compare all simple models against null
bfs_reproduction_simple_models <- 
  bfs_reproduction %>% 
  filter(nchar(model) == 1 | model == 'UN') %>% 
  mutate(baseline = bfs_decision$bf[bfs_decision$model == 'UN']) %>% 
  mutate(bf_over_null_log = bf - baseline,
         bf_over_null_raw = exp(bf_over_null_log)) %>%
  arrange(desc(bf_over_null_raw)) 

bfs_reproduction_simple_models %>% 
  mutate(bf_over_null_raw = format(bf_over_null_raw, scientific = FALSE))

bfs_reproduction_simple_models %>% 
  mutate(bf_over_null_raw = formatC(bf_over_null_raw, digits = 1, format = 'e'))


# More constrained models -------------------------------------------------

# All models are compared against model A

## Data decision --------------------------------

# Compare all simple models against null
bfs_decision_more_constrained <-
  bfs_decision %>% 
  mutate(baseline = bfs_decision$bf[bfs_decision$model == 'a']) %>% 
  mutate(bf_over_null = exp(bf - baseline)) %>% 
  arrange(desc(bf_over_null)) 

bfs_decision_more_constrained %>% 
  mutate(bf_over_null = format(bf_over_null, scientific = FALSE))

## Data reproduction ----------------------------

# Compare all simple models against null
bfs_reproduction_more_constrained <- 
  bfs_reproduction %>% 
  mutate(baseline = bfs_reproduction$bf[bfs_reproduction$model == 'a']) %>% 
  mutate(bf_over_null = exp(bf - baseline)) %>% 
  arrange(desc(bf_over_null)) 

bfs_reproduction_more_constrained %>% 
  mutate(bf_over_null = format(bf_over_null, scientific = FALSE))

# Plots -------------------------------------------------------------------

## Decision plot ---------------

bfs_decision_simple_models

# Base plot
p_bias <-
  bfs_decision_simple_models %>%
  mutate(model = toupper(model)) %>% 
  # filter(nchar(model) == 1 | model %in% c('BM'), model != 'a') %>%
  mutate(model = recode(model, 'UN'='null'))  %>% 
  mutate(model = factor(model, levels = c('null', sort(LETTERS[1:13], decreasing = TRUE)))) %>%
  ggplot(., aes(y=model, x=bf_over_null_raw)) + 
  geom_point() +
  ylab('Model') + xlab(expression(BF[10])) +
  ggtitle('Categorization bias') 

# Best performing models annotation
p_bias  +
  annotate("text", x = Inf, y = -Inf, 
           hjust = 1.4, vjust = -10,
           label = "Best performing models", size = 3) +
  geom_segment(aes(x = 25e+14, xend = 3.5e+15, 
                   y = 3.8, yend = 3.8),
               arrow = arrow(length = unit (0.3, "cm")))

# Save plot
ggsave(here('plots/reproduction_experiment_ordinal_simple_models_decision.png'), 
       height = 4, width = 5, scale = .6, dpi=1200)


# More constrained models
all_models_order <- c(
  sort(simplify(map(LETTERS[1:13], ~ paste(rep(.x, 2), collapse = ''))), decreasing = TRUE),
  sort(LETTERS[1:7], decreasing = TRUE)
)

# All models
bfs_decision_more_constrained %>%
  mutate(model = toupper(model)) %>% 
  mutate(model = recode(model, 'UN'='null'))  %>%
  mutate(model = factor(model, levels = c('null', all_models_order))) %>%
  ggplot(., aes(y=model, x=bf_over_null)) + 
  geom_point() +
  ylab('Model') + xlab(expression(BF[10]~over~model~A)) +
  ggtitle('Categorization bias') +
  # Best performing models annotation
  annotate("text", x = 8, y = 11,
           label = "Best performing models", size = 3) +
  geom_segment(aes(x = 7, xend = 10, 
                   y = 10, 
                   yend = 10),
               arrow = arrow(length = unit (0.3, "cm"))) +
  geom_vline(xintercept = 1, linetype=2) 

# Save plot
ggsave(here('plots/reproduction_experiment_ordinal_all_models_decision.png'),
       height = 6, width = 4, scale = .75, dpi=1200)

# Reproduction plot -------------------------------------------------------

# Base plot
p_rep <-
  bfs_reproduction_simple_models %>%
  mutate(model = toupper(model)) %>% 
  # filter(nchar(model) == 1 | model %in% c('BM'), model != 'a') %>%
  mutate(model = recode(model, 'UN'='null'))  %>% 
  mutate(model = factor(model, levels = c('null', sort(LETTERS[1:13], decreasing = TRUE)))) %>%
  ggplot(., aes(y=model, x=bf_over_null_raw)) + 
  geom_point() +
  ylab('Model') + xlab(expression(BF[10])) +
  ggtitle('Reproduction bias') 

# Best performing models annotation
p_rep  +
  annotate("text", x = Inf, y = -Inf, 
           hjust = 1.4, vjust = -10,
           label = "Best performing models", size = 3) +
  geom_segment(aes(x = 6.02e+32, xend = 8.45e+32, 
                   y = 3.8, yend = 3.8),
               arrow = arrow(length = unit (0.3, "cm")))

# Save plot
ggsave(here('plots/reproduction_experiment_ordinal_simple_models_reproduction.png'), 
       height = 4, width = 5, scale = .6, dpi=1200)

# All models
bfs_reproduction_more_constrained %>%
  mutate(model = toupper(model)) %>% 
  mutate(model = recode(model, 'UN'='null'))  %>%
  mutate(model = factor(model, levels = c('null', all_models_order))) %>%
  ggplot(., aes(y=model, x=bf_over_null)) + 
  geom_point() +
  ylab('Model') + xlab(expression(BF[10]~over~model~A)) +
  ggtitle('Reproduction bias') +
  # Best performing models annotation
  annotate("text", x = 8, y = 11,
           label = "Best performing models", size = 3) +
  geom_segment(aes(x = 7, xend = 10, 
                   y = 10, 
                   yend = 10),
               arrow = arrow(length = unit (0.3, "cm"))) +
  geom_vline(xintercept = 1, linetype=2) + 
  xlim(0, 13)

# Save plot
ggsave(here('plots/reproduction_experiment_ordinal_all_models_reproduction.png'),
       height = 6, width = 4, scale = .75, dpi=1200)
