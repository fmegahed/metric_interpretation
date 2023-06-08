# Code written to examine the sensitivity and specificity in multiclass problems



# * Needed packages -------------------------------------------------------
if(require(pacman)==FALSE) install.packages("pacman")
pacman::p_load(tidyverse, caret, broom, ggtext)

set.seed(2022)
num_sim = 10^4


# * Balanced die ----------------------------------------------------------

results_bal_die = tibble()

for (i in 1:num_sim) {
  sim_data = data.frame(
    obs = sample( x = c( paste0('c', 1:4)), # four classes
                  size = 3108, # size of sample = number of continguous counties
                  replace = T) %>%  # sample with replacement
      as.factor(),
    pred = sample( x = c( paste0('c', 1:4)), # four classes
                   3108, # size of sample = number of continguous counties
                   replace = T) %>%  # sample with replacement
      as.factor()
  )
  
  results_bal_die = bind_rows(
    results_bal_die, 
    # compute confusion matrix based on the caret package
    confusionMatrix(
      data = sim_data$pred, reference = sim_data$obs) %>% 
      # tidy the results and focus on the byClass results
      broom::tidy(byClass = T) %>% 
      # select the five measures of interest (where probs were not computed)
      filter(term %in% c('accuracy', 'sensitivity',
                         'specificity', 'f1')) %>% 
      # adding a column titled scenario
      mutate(sim_num = i,
             scenario = 'die_balanced_obs') %>%
      # keeping only 5 columns since conf.intervals were only computed for acc
      select(sim_num, scenario, term, class, estimate)
  )
}



# * Imabalanced Die -------------------------------------------------------

results_imbal_die = tibble()

for (i in 1:num_sim) {
  sim_data = data.frame(
    obs = sample( x = c( rep('c1', 1261),
                         rep('c2', 226),
                         rep('c3', 827),
                         rep('c4', 794)), # four classes
                  size = 3108,  # size = number of continguous counties
                  replace = T) %>%  # sample with replacement
      as.factor(),
    pred = sample( x = c( paste0('c', 1:4)), # four classes
                   size = 3108, # size = number of continguous counties
                   replace = T) %>%  # sample with replacement
      as.factor()
  )
  
  results_imbal_die = bind_rows(
    results_imbal_die, 
    # compute confusion matrix based on the caret package
    confusionMatrix(
      data = sim_data$pred, reference = sim_data$obs) %>% 
      # tidy the results and focus on the byClass results
      broom::tidy(byClass = T) %>% 
      # select the five measures of interest (where probs were not computed)
      filter(term %in% c('accuracy', 'sensitivity',
                         'specificity', 'f1')) %>% 
      # adding a column titled scenario
      mutate(sim_num = i,
             scenario = 'die_imbalanced_obs') %>%
      # keeping only 5 columns since conf.intervals were only computed for acc
      select(sim_num, scenario, term, class, estimate)
  )
}



# * Proportional Prediction -----------------------------------------------

results_prop_pred = tibble()

for (i in 1:num_sim) {
  sim_data = data.frame(
    obs = sample( x = c( rep('c1', 1261),
                         rep('c2', 226),
                         rep('c3', 827),
                         rep('c4', 794)), # four classes
                  size = 3108, # size = number of continguous counties
                  replace = T) %>%  # sample with replacement
      as.factor(),
    pred = sample( x = c( rep('c1', 1261),
                          rep('c2', 226),
                          rep('c3', 827),
                          rep('c4', 794)), # four classes
                   size = 3108, # size = number of continguous counties
                   replace = T) %>%  # sample with replacement
      as.factor()
  )
  
  results_prop_pred = bind_rows(
    results_prop_pred, 
    # compute confusion matrix based on the caret package
    confusionMatrix(
      data = sim_data$pred, reference = sim_data$obs) %>% 
      # tidy the results and focus on the byClass results
      broom::tidy(byClass = T) %>% 
      # select the five measures of interest (where probs were not computed)
      filter(term %in% c('accuracy', 'sensitivity',
                         'specificity', 'f1')) %>% 
      # adding a column titled scenario
      mutate(sim_num = i,
             scenario = 'proportional_pred') %>%
      # keeping only 5 columns since conf.intervals were only computed for acc
      select(sim_num, scenario, term, class, estimate)
  )
}

results = bind_rows(results_bal_die,
                    results_imbal_die,
                    results_prop_pred)

write_csv(x = results, file = 'sim_results.csv')



# * Plots -----------------------------------------------------------------


# * * Sensitivity Plot ----------------------------------------------------
results %>% filter(term == 'sensitivity') %>% 
  mutate(scenario = recode(
    scenario,
    die_balanced_obs = '4-sided die (balanced data)',
    die_imbalanced_obs = '4-sided die (imbalanced data)',
    proportional_pred = 'proportional guessing'),
    reported_values = case_when(
      class == 'c1' ~ 0.7105,
      class == 'c2' ~ 0.4204,
      class == 'c3' ~ 0.3906,
      class == 'c4' ~ 0.7427
    )) %>% 
  ggplot(aes(x = estimate, group = class)) +
  facet_grid(cols = vars(scenario), rows = vars(class), scales = 'fixed') +
  geom_histogram(aes(fill = as.factor(class)), binwidth = 0.01, color = 'black') +
  scale_x_continuous(breaks = scales::pretty_breaks(5), limits = c(0,1)) +
  scale_y_continuous(breaks = scales::pretty_breaks(5), labels = scales::comma) +
  scale_fill_brewer(palette = "Paired") +
  scale_color_brewer(palette = "Paired") +
  geom_vline(aes(xintercept = reported_values), size = 1, color = 'black') +
  geom_curve(aes(x = reported_values + 0.15, y = 2750, xend = reported_values, yend = 3250), 
             colour = "black", 
             size=1.5, 
             curvature = 0.25,
             arrow = arrow(length = unit(0.055, "npc"))) +
  geom_text(aes(x = reported_values + 0.15, y = 2000, 
                label = paste('reported\nsensitivity\nin paper \n for', class)),
            color = 'black', size = 3) +
  theme_bw() +
  theme(legend.position = 'none') +
  labs(x = 'Sensitivity Values', y = 'Count', 
       title = paste('A histogram of sensitivity values based on',
                     scales::comma(num_sim), 'simulation runs'))

