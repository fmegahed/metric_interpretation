

# * Computing the geometric mean of sens and spec -------------------------

g_mean = function(x, y){
  
  result = sqrt(x*y)
  
  return(result)
}


# * Big simulation function -----------------------------------------------

# a custom vectorized function that generates: 
# (a) observations, 
# (b) predictions
# based on the experimental conditions of num_classes, num_observations,
# first_class_perc and pred_approach
# The function returns five accuracy metrics of interest:
# (1) acc, (2) sens, (3) spec, (4) f_measure, and (5) g_mean

sim_function = function(data = NULL, base_class_name = 'class_'){
  
  # extracting the relevant features from the data 
  number_classes = data$num_classes
  number_observations = data$num_observations
  first_class_percentage = data$first_class_perc
  prediction_approach = data$pred_approach
  
  
  # generating the reference data
  ref = sample(x = c(paste0(base_class_name, 1:number_classes)),
               size = number_observations,
               replace = T,
               prob = c(first_class_percentage, 
                        rep( (1-first_class_percentage)/(number_classes - 1),
                             (number_classes -1 ) )
               )
  ) %>% factor(levels = paste0(base_class_name, 1:number_classes) )
  
  
  # generating the predictions
  if(prediction_approach == 'proportional'){
    predictions = sample(
      x = c(paste0(base_class_name, 1:number_classes)),
      size = number_observations,
      replace = T,
      prob = c(first_class_percentage, 
               rep( (1-first_class_percentage)/(number_classes - 1),
                    (number_classes -1 ) )
      )
    ) %>% factor(levels = paste0(base_class_name, 1:number_classes) )
  } else{
    predictions = sample(
      x = c(paste0(base_class_name, 1:number_classes)),
      size = number_observations,
      replace = T,
      prob = rep(1/number_classes, number_classes) 
    ) %>% factor(levels = paste0(base_class_name, 1:number_classes) )
  }
  
  
  # computing the acc metrics of interest
  acc_metrics = tibble(
    accuracy = yardstick::accuracy_vec(truth = ref, estimate = predictions),
    sensitivity = yardstick::sens_vec(truth = ref, estimate = predictions, event_level = 'first'),
    specificity = yardstick::spec_vec(truth = ref, estimate = predictions, event_level = 'first'),
    f_measure = yardstick::f_meas_vec(truth = ref, estimate = predictions, event_level = 'first'),
    g_mean = g_mean(x = sensitivity, y = specificity)
  )
  
  return(acc_metrics)
}
