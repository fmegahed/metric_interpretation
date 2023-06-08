

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
# The function returns seven accuracy metrics of interest:
# (1) acc, (2) sens, (3) spec, (4) g_mean, (5) precision, (6) recall, and (7) f_measure.


sim_function = function(data = NULL, base_class_name = 'class_'){
  
  # extracting the relevant features from the data 
  number_classes = data$num_classes
  number_observations = data$num_observations
  first_class_percentage = data$first_class_perc
  prediction_approach = data$pred_approach
  
  
  # generating the reference data
  x = c(paste0(base_class_name, 1:number_classes))
  ref = c(rep(x[1], number_observations*first_class_percentage),
          rep(x[-1], each = number_observations*(1-first_class_percentage)/(number_classes-1)))
  
  if (length(ref)!=number_classes) ref = c(ref, rep(x[number_classes], (number_observations-length(ref))))
  
  ref = sample(ref, number_observations, replace=F) |> factor(levels = paste0(base_class_name, 1:number_classes) )
  
  # generating the predictions
  if(prediction_approach == 'uniform'){
    predictions = sample(
      x = c(paste0(base_class_name, 1:number_classes)),
      size = number_observations,
      replace = T,
      prob = rep(1/number_classes, number_classes) 
    ) |> factor(levels = paste0(base_class_name, 1:number_classes))
  }else if(prediction_approach == 'proportional'){
    predictions = sample(
      x = c(paste0(base_class_name, 1:number_classes)),
      size = number_observations,
      replace = T,
      prob = (table(ref)/length(ref))
    ) |> factor(levels = paste0(base_class_name, 1:number_classes))
  } 
  
  conf_matrix = caret::confusionMatrix(data = predictions, reference = ref)
  
  # computing the acc metrics of interest
  if (number_classes == 2){
    acc_metrics = tibble(
      accuracy = conf_matrix$overall['Accuracy'],
      sensitivity = conf_matrix$byClass["Sensitivity"],
      specificity = conf_matrix$byClass["Specificity"],
      g_mean = g_mean(x = sensitivity, y = specificity),
      precision = conf_matrix$byClass["Precision"],
      recall = conf_matrix$byClass["Recall"],
      f_measure = conf_matrix$byClass["F1"]
    )
  }else{
    acc_metrics = tibble(
      accuracy = conf_matrix$overall['Accuracy'],
      sensitivity = conf_matrix$byClass[paste0("Class: ", base_class_name, 1), "Sensitivity"],
      specificity = conf_matrix$byClass[paste0("Class: ", base_class_name, 1), "Specificity"],
      g_mean = g_mean(x = sensitivity, y = specificity),
      precision = conf_matrix$byClass[paste0("Class: ", base_class_name, 1), "Precision"],
      recall = conf_matrix$byClass[paste0("Class: ", base_class_name, 1), "Recall"],
      f_measure = conf_matrix$byClass[paste0("Class: ", base_class_name, 1), "F1"]
    )
  }
  return(acc_metrics)
}
