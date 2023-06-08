predict_stratified <- function(X, n_classes_, classes_, class_prior_, random_state) {
  n_samples <- nrow(X)
  
  # Set the random seed
  set.seed(random_state)
  
  # Compute probabilities only once for the stratified strategy
  proba <- list()
  for (k in seq_along(n_classes_)) {
    out <- matrix(0, n_samples, n_classes_[[k]])
    selected <- apply(matrix(runif(n_samples), nrow = n_samples), 1, function(x) findInterval(x, cumsum(class_prior_[[k]])))
    out[cbind(1:n_samples, selected)] <- 1
    proba[[k]] <- out
  }
  
  # Get the predicted classes based on the probabilities
  y <- t(sapply(1:length(proba), function(k) {
    classes_[[k]][apply(proba[[k]], 1, which.max)]
  }))
  
  y <- as.factor(as.vector(y))
  y <- factor(y, levels = c(levels(y), 
                            levels(unlist(classes_))[!levels(unlist(classes_)) %in% levels(y)]))
  return(y)
}

# Load the iris dataset
data(iris)

# Split the data into training and testing sets
index <- 1 + c(13, 68, 22, 104, 148, 27, 9, 97, 137, 85, 87, 
               149, 40, 34, 86, 140, 62, 41, 0, 111, 147, 96,
               17, 131, 141, 93, 57, 138, 115, 5, 11, 134, 18,
               16, 101, 2, 37, 55, 73, 61, 33, 60, 107, 4, 98,
               39, 43, 66, 110, 26, 77, 81, 67, 72, 125, 1, 50,
               82, 65, 112, 139, 145, 105, 118, 76, 133, 102, 53,
               47, 91, 106, 23, 10, 84, 109, 21, 42, 103, 70, 
               124, 143, 58, 49, 128, 114, 24, 119, 92, 95, 78, 
               126, 123, 129, 14, 25, 136, 32, 46, 3, 121, 117, 
               31, 71, 52, 56, 54, 30, 63, 69, 44, 28, 64)
train_data <- iris[index, ]
test_data <- iris[-index, ]

# Calculate class_prior_ for the training set
class_counts <- table(train_data$Species)
class_prior_ <- list(as.numeric(class_counts) / sum(class_counts))

# Define the number of classes and unique classes for each output
n_classes_ <- list(length(unique(train_data$Species)))
classes_ <- list(unique(train_data$Species))

# Make predictions using the predict_stratified function
predictions <- predict_stratified(test_data[, 1:4], n_classes_, classes_, class_prior_, random_state = 42)

# Calculate the confusion matrix and accuracy
library(caret)
conf_matrix <- confusionMatrix(predictions, test_data$Species)
print(conf_matrix)



dummy_classifier <- function(strategy = "stratified", constant = NULL) {
  if (!strategy %in% c("stratified", "most_frequent", "prior", "uniform", "constant")) {
    stop("Invalid strategy. Choose from 'stratified', 'most_frequent', 'prior', 'uniform', or 'constant'.")
  }
  
  if (strategy == "constant" & is.null(constant)) {
    stop("For constant strategy, you need to provide a constant value.")
  }
  
  function(X, y) {
    model <- list()
    model$strategy <- strategy
    model$constant <- constant
    model$unique_classes <- unique(y)
    model$y <- y
    
    if (strategy == "most_frequent") {
      model$most_frequent <- levels(y)[which.max(table(y))]
    }
    
    return(model)
  }
}


predict_dummy_classifier <- function(model, X, random_state=NULL) {
  y <- model$y
  n <- nrow(X)
  if (model$strategy == "stratified") {
    # Calculate class_prior_ for the training set
    class_prior_ <- list(table(y) / length(y))
    # Define the number of classes and unique classes for each output
    n_classes_ <- list(length(unique(y)))
    classes_ <- list(unique(y))
    # Make predictions using the predict_stratified function
    predict_stratified(X, n_classes_, classes_, class_prior_, random_state)
  } else if (model$strategy == "most_frequent") {
    rep(model$most_frequent, n)
  } else if (model$strategy == "prior") {
    sample(model$unique_classes, n, replace = TRUE, prob = table(y) / length(y))
  } else if (model$strategy == "uniform") {
    sample(model$unique_classes, n, replace = TRUE, prob = rep(1 / length(model$unique_classes), length(model$unique_classes)))
  } else if (model$strategy == "constant") {
    rep(model$constant, n)
  } else {
    stop("Invalid strategy.")
  }
}



dummy_classifier_caret <- list(
  label = "Dummy Classifier",
  library = NULL,
  type = "Classification",
  parameters = data.frame(parameter = "parameter",
                          class = "character",
                          label = "parameter"),
  grid = function(x, y, len = NULL, search = "grid") {
    data.frame(parameter = "none")
  },
  loop = NULL,
  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
    theDots <- list(...)
    if (!any(names(theDots) == "strategy")){
      theDots$strategy <- "prior"
    }
    if (!any(names(theDots) == "constant")){
      theDots$constant <- NULL
    }
    model <- dummy_classifier(theDots$strategy, theDots$constant)(x, y)
    model$levels <- lev
    model
  },
  predict = function(modelFit, newdata, submodels = NULL) {
    factor(predict_dummy_classifier(modelFit, newdata), levels = modelFit$levels)
  },
  prob = NULL,
  predictors = function(x, ...) {
    NULL
  },
  tags = c("Dummy Classifier"),
  levels = NULL,
  sort = function(x) x
)

dummy_regressor_caret <- list(
  label = "Dummy Regressor",
  library = NULL,
  type = "Regression",
  parameters = data.frame(parameter = "strategy",
                          class = "character",
                          label = "Strategy"),
  grid = function(x, y, len = NULL, search = "grid") {
    data.frame(strategy = c("mean", "median", "quantile", "constant"))
  },
  loop = NULL,
  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
    dummy_regressor(param$strategy)(x, y)
  },
  predict = function(modelFit, newdata, submodels = NULL) {
    predict_dummy_regressor(modelFit, newdata)
  },
  prob = NULL,
  predictors = function(x, ...) {
    NULL
  },
  tags = c("Dummy Regressor"),
  levels = NULL,
  sort = function(x) x
)



# Load the iris dataset

data(iris)



# Split the data into training and testing sets

set.seed(2023)

index <- sample(1:nrow(iris), nrow(iris) * 0.8)

train_data <- iris[index,]

test_data <- iris[-index,]


ctrl1 <- trainControl(method = "cv", number = 5)
# Train a dummy classifier with caret

classifier_model_caret <- train(Species ~ ., data = train_data,
                                method = dummy_classifier_caret, 
                                strategy = "stratified", 
                                trControl = ctrl1)



# Make predictions using the trained dummy classifier

classifier_predictions_caret <- predict(classifier_model_caret, test_data)



# Evaluate the performance of the dummy classifier

classifier_confusion_matrix <- confusionMatrix(classifier_predictions_caret, test_data$Species)

print(classifier_confusion_matrix)
