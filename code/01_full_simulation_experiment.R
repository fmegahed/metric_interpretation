
# * Needed packages -------------------------------------------------------
if(require(pacman)==FALSE) install.packages("pacman")
pacman::p_load(tidyverse, yardstick)



# * Experimental setup ----------------------------------------------------

set.seed(2022) # setting the seed to 2022

# The five experimental conditions:
# _________________________________

sim_num = 1:10^4 # setting the number of simulations per exp run to 10,000

num_classes = c(2, 3, 5, 10) # number of classes

num_observations = c(10^3, 10^4) # number of observations

first_class_perc = c(0.01, 0.1, 0.2, (1/3), 0.5) # imbalance

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
  nest(data = -c(exp_num, sim_num) ) %>% 
  # sort the experimental data by exp_num
  arrange(exp_num)

write_rds(
  x = sim_setup, file = 'results/sim_setup.rds'
)


# * Running the experiment ------------------------------------------------

# a custom vectorized function that generates: 
# (a) observations, 
# (b) predictions
# based on the experimental conditions of num_classes, num_observations,
# first_class_perc and pred_approach
# The function returns five accuracy metrics of interest:
# (1) acc, (2) sens, (3) spec, (4) f_measure, and (5) g_mean
source('code/custom_functions_final.R')

experimental_results = 
  sim_setup %>% 
  mutate(
    acc_metrics = map(.x = data, .f = sim_function, base_class_name = 'class_')
  )


write_rds(
  x = experimental_results, file = 'results/experimental_results.rds'
)


summary_results = 
  experimental_results %>% 
  # unnesting the acc_metrics (i.e., generating columns for each of the metrics)
  unnest( acc_metrics ) %>% 
  # grouping by exp_num 
  # (and data which is redundant but to keep it in the summary table)
  group_by(exp_num, data) %>% 
  # creating summaries of means and sds for each of the metrics by exp_num
  summarise(
    across(accuracy:g_mean,  
           list(mean = ~ mean(.x, na.rm = TRUE),
                sd = ~ sd(.x, na.rm = TRUE)), .names = "{.fn}_{.col}" )
  ) %>% 
  # expanding the data into the different experimental conditions for interpretation
  unnest(data)


write_rds(
  x = summary_results, file = 'results/summary_results.rds'
)

write_csv(
  x = summary_results, file = 'results/summary_results.csv'
)
