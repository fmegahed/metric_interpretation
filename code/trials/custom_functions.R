

# * Generating the reference data -----------------------------------------

generate_ref_data = function(data = NULL, base_class_name = 'class_'){
  
  # extracting the relevant columns from the data
  number_classes = data$num_classes
  number_observations = data$num_observations
  first_class_percentage = data$first_class_perc
  
  # generating the sample
  reference = sample(x = c(paste0(base_class_name, 1:number_classes)),
                     size = number_observations,
                     replace = T,
                     prob = c(first_class_percentage, 
                              rep( (1-first_class_percentage)/(number_classes - 1),
                                   (number_classes -1 ) )
                     )
  ) %>% as.factor()
  
  return(reference)
}



# * Generating the predictions based on one of two approaches -------------


generate_pred_data = function(data = NULL, base_class_name = 'class_'){
  
  # extracting the relevant columns from the data
  number_classes = data$num_classes
  number_observations = data$num_observations
  first_class_percentage = data$first_class_perc
  
  prediction_approach = data$pred_approach
  
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
    ) %>% as.factor()
  } else{
    predictions = sample(
      x = c(paste0(base_class_name, 1:number_classes)),
      size = number_observations,
      replace = T,
      prob = rep(1/number_classes, number_classes) 
      ) %>% as.factor()
  }
  
  return(predictions)
}


