# Code written to examine the sensitivity and specificity in multiclass problems



# * Needed packages -------------------------------------------------------
if(require(pacman)==FALSE) install.packages("pacman")
pacman::p_load(tidyverse, caret, broom)


# * 4-sided die -----------------------------------------------------------
set.seed(2022)

sim_size = 1000000

four_sided_die = data.frame(
  obs = sample( x = c( paste0('c', 1:4)), # four classes
                size = sim_size, # size of sample = sim_size
                replace = T) %>%  # sample with replacement
    as.factor(),
  pred = sample( x = c( paste0('c', 1:4)), # four classes
                size = sim_size, # size of sample = sim_size
                replace = T) %>%  # sample with replacement
    as.factor()
)

four_sided_results = 
  # compute confusion matrix based on the caret package
  confusionMatrix(
  data = four_sided_die$pred, reference = four_sided_die$obs) %>% 
  # tidy the results and focus on the byClass results
  broom::tidy(byClass = T) %>% 
  # select the five measures of interest (where probs were not computed)
  filter(term %in% c('accuracy', 'balanced_accuracy', 'sensitivity',
                     'specificity', 'f1')) %>% 
  # adding a column titled scenario
  mutate(scenario = 'four_sided_die') %>%
  # keeping only four columns since conf.intervals were only computed for acc
  select(scenario, term, class, estimate)



# * Ratios similar to our covid_deaths_paper ------------------------------
covid_ratios = data.frame(
  obs = sample( x = c( rep('c1', 1261),
                       rep('c2', 226),
                       rep('c3', 827),
                       rep('c4', 794)), # four classes
                size = sim_size, # size of sample = sim_size
                replace = T) %>%  # sample with replacement
    as.factor(),
  pred = sample( x = c( paste0('c', 1:4)), # four classes
                 size = sim_size, # size of sample = sim_size
                 replace = T) %>%  # sample with replacement
    as.factor()
)

covid_ratios_results = 
  # compute confusion matrix based on the caret package
  confusionMatrix(
    data = covid_ratios$pred, reference = covid_ratios$obs) %>% 
  # tidy the results and focus on the byClass results
  broom::tidy(byClass = T) %>% 
  # select the five measures of interest (where probs were not computed)
  filter(term %in% c('accuracy', 'balanced_accuracy', 'sensitivity',
                     'specificity', 'f1')) %>% 
  # adding a column titled scenario
  mutate(scenario = 'covid_death_clusters') %>%
  # keeping only four columns since conf.intervals were only computed for acc
  select(scenario, term, class, estimate)

# putting them in one scenario
results = bind_rows(four_sided_results,
                    covid_ratios_results)

write_csv(x = results, file = '2_num_sim_results.csv')
