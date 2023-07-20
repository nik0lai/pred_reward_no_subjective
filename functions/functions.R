if (!require('pacman', quietly = TRUE)) install.packages('pacman'); library('pacman', quietly = TRUE)

se <- function(x) {
  sd(x)/sqrt(length(x))
}

# This function takes a dataframe with at least one column
# named value and a standard deviation threshold. It will 
# output the rows that lay outside the mean + the number of
# standard deviations input
outlier_filtering <- function(data, sd_threshold) {
  
  # Calculate thresholds and mark outliers
  outliers <- 
    data %>%
    mutate(mean = mean(value),
           upper_boundary = mean + (sd(value) * sd_threshold),
           lower_boundary = mean - (sd(value) * sd_threshold)) %>% 
    mutate(outlier = value > upper_boundary | value < lower_boundary) %>% 
    filter(outlier)
  
  return(outliers)
  
}

# Calculate signal detection theory criterion and d'
sdt_calc <- 
  function (hits, misses, crrej, far) 
  {
    if (0 %in% c(hits, misses, crrej, far)) {
      hits = hits + 0.5
      misses = misses + 0.5
      crrej = crrej + 0.5
      far = far + 0.5
    }
    
    # Get hit and false alarm rate 
    hr <- hits/(hits + misses)
    fr <- far/(far + crrej)
    
    # Get SDT measures
    lambda <- qnorm(fr)
    d <- qnorm(hr) - lambda
    ccrit <- (qnorm(hr) + qnorm(fr))/2
    beta <- d * (-lambda - d * 1/2)
    return(tibble(hr, fr, lambda, d, ccrit, beta))
  }

# To get default ggplot's colors
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

# Get colors for each condition
get_condition_colors = function(sep='_') {
  p_load(colorspace)
  
  # Get base color
  base_colors <- gg_color_hue(3)
  
  # Assign colors and brightness factr
  color_ml <- 2
  color_br <- 1
  color_po <- 3
  dark_factor <- .4
  light_factor <- .2
  
  # Dictionary with colors
  condition_colors <- c(
    'mullerlyer' = base_colors[color_ml],
    'baserate' = base_colors[color_br],
    'payoff' = base_colors[color_po],
    
    'mullerlyer.long' = darken(base_colors[color_ml], dark_factor),
    'mullerlyer.short' = lighten(base_colors[color_ml], light_factor),
    'baserate.long' = darken(base_colors[color_br], dark_factor),
    'baserate.short' = lighten(base_colors[color_br], light_factor),
    'payoff.long' = darken(base_colors[color_po], dark_factor),
    'payoff.short' = lighten(base_colors[color_po], light_factor),
    'simulated.long' = darken('violet', dark_factor),
    'simulated.short' = lighten('violet', light_factor)
    
  )
  
  # Apply separator
  return(set_names(condition_colors, str_replace(names(condition_colors), '\\.', sep)))
  
}

# Basic plot for decision and reproduction data
rep_base_plot <- function(dat) {
  
  # Get summary data by condition
  summ_data <-
    dat %>%
    group_by(cond_code, bias_source, bias_direction) %>%
    summarise(
      se = sd(value)/sqrt(length(value)),
      value = mean(value)
    )
  
  # Make plot
  dat %>%
    # Plot
    ggplot(., aes(x=bias_direction, y=value, color = cond_code)) +
    facet_wrap(. ~ bias_source, labeller = labeller(bias_source = labels_bias_source_sample)) +
    # Subject data
    geom_line(aes(group=interaction(participant, bias_source), color = bias_source), alpha= .8, show.legend = FALSE) +
    geom_point(size=2, show.legend = FALSE) +
    # Summary data
    geom_errorbar(data=summ_data, aes(x=bias_direction, ymin=value-se, ymax=value+se), color='black', width=.2) +
    geom_point(data=summ_data, aes(x=bias_direction, y=value), color='black')
}

# Basic delta plot for decision and reproduction data
rep_delta_plot <- function(dat) {
  
  summ_dat <- 
    dat %>% 
    group_by(bias_source) %>% 
    summarise(mean = mean(delta),
              se = sd(delta)/sqrt(n()))
  
  dat %>% 
    ggplot(., aes(x = bias_source, y = delta, color = bias_source)) +
    geom_hline(yintercept = 0, size=.8,linetype=3) +
    geom_dotplot(aes(fill = bias_source), binaxis='y', stackdir='center', dotsize = .8, show.legend = FALSE) +
    geom_point(data=summ_dat, aes(x=bias_source, y=mean), color='black') +
    geom_errorbar(data=summ_dat, width=.15,
                  aes(x=bias_source, y=mean, ymin=mean-se, ymax=mean+se), color='black') +
    xlab('Bias source') +  
    guides(color=guide_legend(title="Bias source")) 
  
}


# Get Bayesian T test and effect size for two conditions
get_bf_d <- function(data, col_names, paired = TRUE) {
  # Load packages
  p_load(BayesFactor, effectsize, dplyr, bayestestR)
  # Get bf and cohens d
  data %>% 
    mutate(
      ttest = map(data, ~ ttestBF(x = pull(.x[col_names[1]]), y = pull(.x[col_names[2]]), paired = paired, rscale = sqrt(2)/2)),
      cohensd = map(data, ~ tibble(cohens_d(x = pull(.x[col_names[1]]), y = pull(.x[col_names[2]]), data = .x, paired = paired))) 
    ) %>% 
    mutate(
      bf = unlist(map(ttest, ~describe_posterior(.x)$BF)),
      d = unlist(map(cohensd, 'Cohens_d'))
    ) 
  
}

# Gets delta between bias direction conditions
get_delta <- function(dat) {
  dat %>%
    select(participant, bias_source, bias_direction, value) %>% 
    pivot_wider(names_from = bias_direction, values_from = value) %>% 
    mutate(delta = long - short)
  
}

filter_data_ddm <- function(data, sd_threshold) {
  
  ### Staircase ------------------
  
  staircase_data <-
    data %>% 
    select(participant, bias_source, staircase_threshold) %>% 
    distinct() %>% 
    rename(value = staircase_threshold)
  
  # Get staircase outliers
  staircase_outliers <- outlier_filtering(data = staircase_data, sd_threshold = sd_threshold)
  
  # Remove staircase outliers from data
  data <-
    data %>% 
    anti_join(staircase_outliers %>% select(participant, bias_source))
  
  ## Low d' --------------
  
  sdt_data <-
    data %>%
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
    pivot_wider(names_from = key, values_from = value)
  
  # Get low d' subjects
  low_d_subs <- 
    sdt_data %>% 
    filter(d <= 0)
  
  # Remove low d' subs
  data <- data %>% 
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
  if (nrow(d_outliers)) {
    data <-
      data %>% 
      anti_join(d_outliers %>% ungroup() %>% select(participant, bias_source))
    sdt_data <-
      sdt_data %>% 
      anti_join(d_outliers %>% ungroup() %>% select(participant, bias_source))
  }
  
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
  if (nrow(c_outliers)) {
    data <-
      data %>% 
      anti_join(c_outliers %>% ungroup() %>% select(participant, bias_source))
    sdt_data <-
      sdt_data %>% 
      anti_join(c_outliers %>% ungroup() %>% select(participant, bias_source))
    
  }
  
  # Filtering summary -------------------------------------------------------
  print('Staircase outliers:')
  print(staircase_outliers)
  print('Low d\':')
  print(low_d_subs)
  print('d\'outliers:')
  print(d_outliers)
  print('Low d\'outliers:')
  print(c_outliers)
  
  return(list('data' = data, 
              'staircase_outliers' = staircase_outliers,
              'low_d' = low_d_subs,
              'd_outliers' = d_outliers,
              'c_outliers' = c_outliers))
  
}

get_filtering_text <- function(x) {
  paste(sprintf('Staircase outliers: %s ', nrow(x$staircase_outliers)),
        sprintf('Low d\': %s ', nrow(x$low_d)),
        sprintf('d\' outliers: %s ', nrow(x$d_outliers)),
        sprintf('c\' outliers: %s ', nrow(x$c_outliers)), sep = '\n')
  
}

get_filtering_table <- function(x) {
  
  x[names(x) != 'data'] %>%
    names() %>%
    map(., ~x[[.x]] %>% mutate(key = .x)) %>%
    bind_rows() %>%
    select(key, names(.))
  
}

filter_fast_slow_responses <- function(data, sd_threshold) {
  library('magrittr')
  library('dplyr')
  
  # Mark all RTs faster than 200ms
  data <- 
    data %>% 
    mutate(fast_trial = rt < .2)
  
  # Get percentage of fast trials
  prop_fast_trials <-
    data %>% 
    group_by(participant, bias_source, bias_direction) %>% 
    summarise(prop_fast = mean(fast_trial)) %>% 
    arrange(desc(prop_fast))
  
  # Mark slow trials given SD
  data <- 
    data %>% 
    filter(!fast_trial) %>%
    mutate(slow_trials = rt > mean(rt) + sd(rt) * sd_threshold)
  
  # Get percentage of slow trials
  prop_slow_trials <- 
    data %>% 
    group_by(participant, bias_source, bias_direction) %>% 
    summarise(prop_slow = mean(slow_trials)) %>% 
    arrange(desc(prop_slow))
  
  # Filter slow/fast
  data <- 
    data %>% 
    filter(!fast_trial, !slow_trials)
  
  return(data)
}

format_columns_for_hddm <- function(data) {
  
  data %>% 
    ungroup() %>% 
    select(participant, bias_source, bias_direction, line_type, answer, rt) %>% 
    rename(subj_idx = participant,
           stim = line_type,
           response = answer) %>% 
    mutate(across(c(stim, response), ~ recode(.x, 'long' = 1, 'short' = 0)))
  
}


shorter_bf_value <- function(bf) {
  if (bf < 1) {
    bf <- round(bf, 2)
  } else if (bf > 1) {
    if (nchar(round(bf)) > 3) {
      bf <- formatC(bf, format = "e", digits = 1)
    } else if (round(bf) > 1) {
      bf <- round(bf)
    } else {
      bf <- round(bf, 2)
    }
  }
  return(as.character(bf))
}
