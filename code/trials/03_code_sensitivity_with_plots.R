# Code written to examine the sensitivity and specificity in multiclass problems



# * Needed packages -------------------------------------------------------
if(require(pacman)==FALSE) install.packages("pacman")
pacman::p_load(tidyverse, caret, broom, ggtext)



# * Experimental setup ----------------------------------------------------

set.seed(2022) # setting the seed to 2022

# The five experimental conditions:
# _________________________________

sim_num = 1:10^4 # setting the number of simulations per exp run to 10,000

num_classes = c(2, 3, 5, 10) # number of classes

num_observations = c(10^3, 10^4, 10^5) # number of observations

first_class_perc = c(0.01, 0.02, 0.05, 0.1, 0.15, 0.2, 0.25, 0.5) # imbalance

pred_approach = c('random', 'proportional')


# Setting up the experiment:
# __________________________

sim_setup = 
  # creating a tibble of all combinations of the five experimental conditions
  expand_grid( sim_num, num_classes, num_observations, first_class_perc,
               pred_approach) %>% 
  # using group_by to create ids which are NOT dependent on the sim_num
  group_by(num_classes, num_observations, first_class_perc, pred_approach) %>% 
  # using the dplyr::cur_group_id() to create the IDs
  mutate( exp_num = cur_group_id() ) %>% 
  # ungrouping since it was only performed to create unique ids
  ungroup() %>% 
  # moving the exp_num to the beginning
  relocate( exp_num ) %>% 
  # creating list columns for all the columns with except of ID-type columns
  nest(data = -c(exp_num, sim_num) )

write_rds(x = sim_setup, file = 'results/sim_setup.rds')


# * Running the experiment ------------------------------------------------

# custom vectorized functions for generating obervations and the predictions
# based on the experimental conditions of num_classes, num_observations,
# first_class_perc and pred_approach
source('custom_functions.R') 

experiment = 
  sim_setup %>% 
  # creating list columns of reference data, predictions (based on approach),
  # the confusion matrix, and a tidied version of the confusion matrix 
  # (which can be easily unnested to create columns of interest)
  mutate(
    ref = map(.x = data, .f = generate_ref_data, base_class_name = 'class_'),
    pred = map(.x = data, .f = generate_pred_data, base_class_name = 'class_'),
    conf_matrix = map2(.x = pred, .y = ref, .f = confusionMatrix),
    tidied_conf_matrix = map(.x = conf_matrix, .f =  broom::tidy, byClass = T)
  )

write_rds(x = experiment, file = 'results/experiment.rds')


# a longer version of the results where each combination of experimental conditions
# will have one row for the five accuracy metrics below 
results = experiment %>% 
  unnest(data) %>% 
  select(-c(ref, pred, conf_matrix)) %>% 
unnest(tidied_conf_matrix) %>% 
  filter(term %in% c('accuracy', 'balanced_accuracy' , 'sensitivity',
                     'specificity', 'f1') ) %>% 
  select( -c(conf.low, conf.high, p.value) ) 

write_rds(x = results, file = 'results/tabular_results.rds')
