dummy_classifier <- function(strategy = "proportional", constant = NULL, random_state = NULL) {
  if (!strategy %in% c("stratified", "most_frequent", "proportional", "uniform", "constant")) {
    stop("Invalid strategy. Choose from 'stratified', 'most_frequent', 'proportional', 'uniform', or 'constant'.")
  }
  
  if (strategy == "constant" & is.null(constant)) {
    stop("For constant strategy, you need to provide a constant value.")
  }
  
  function(X, y) {
    model <- list()
    model$strategy <- strategy
    model$constant <- constant
    model$y <- as.factor(y)
    model$classes <- levels(y)
    model$n_classes <- length(model$classes)
    model$class_prior <- table(y) / length(y)
    model$random_state <- random_state
    
    if (strategy == "most_frequent") {
      model$most_frequent <- levels(y)[which.max(table(y))]
    }
    
    return(model)
  }
}


predict_proba <- function(model, X) {
  y <- model$y
  # Extract necessary parameters from the model object
  n_classes <- model$n_classes
  class_prior <- model$class_prior
  classes <- model$classes
  constant <- model$constant
  strategy <- model$strategy
  set.seed(model$random_state)
  
  # Compute the number of samples and create a random state
  n_samples <- nrow(X)
  
  # Initialize an empty list to store the probability estimates
  P <- matrix(NA, n_samples, n_classes)
  # Loop over each output and compute the probability estimates
    if (strategy == "most_frequent") {
      temp <- unname(sort(class_prior))
      if (temp[1] == temp[2]) warning(paste0("At least two classes had equal and highest frequency. The reported results use the first majority class, ", classes[which.max(class_prior)], "."))
      ind <- which.max(class_prior)
      out <- matrix(0, n_samples, n_classes)
      out[, ind] <- 1.0
    } else if (strategy == "proportional") {
      out <- matrix(class_prior, n_samples, n_classes, byrow=T)
    } else if (strategy == "stratified") {
      out <- matrix(0, n_samples, n_classes)
      for (i in seq_len(n_samples)) {
        out[i, ] <- rmultinom(1, 1, class_prior)
      }
      out <- as.matrix(out, dtype = "numeric")
    } else if (strategy == "uniform") {
      out <- matrix(1/n_classes_, n_samples, n_classes)
    } else if (strategy == "constant") {
      ind <- which(classes == constant)
      out <- matrix(0, n_samples, n_classes)
      out[, ind] <- 1.0
    }
    P <- out

  return(P)
}


predict_dummy_classifier <- function(object, X) {
  
  n_samples <- nrow(X)
  set.seed(object$random_state)
  n_classes <- object$n_classes
  classes <- object$classes
  class_prior <- object$class_prior
  constant <- object$constant
  strategy <- object$strategy
  
  if (strategy == "proportional") {
    y <- sample(classes, n_samples, replace = TRUE, prob = class_prior)
  } else if (strategy == c("most_frequent")) {
      temp <- unname(sort(class_prior))
      if (temp[1] == temp[2]) warning(paste0("At least two classes had equal and highest frequency. The reported results use the first majority class, ", classes[which.max(class_prior)], "."))
      y <- rep(classes[which.max(class_prior)], each = n_samples)
  } else if (strategy == "stratified") {
      proba <- predict_proba(object, X)
      y <- classes[apply(proba, 1, which.max)]
    } else if (strategy == "constant") {
      y <- rep(constant, n_samples)
    } else if (strategy == "uniform") {
      y <- sample(classes, n_samples, replace = TRUE, prob = rep(1/n_classes, n_classes))
    } else {
      stop("Invalid strategy specified.")
    }
  y <- factor(y, levels = classes)
  return(y)
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
      theDots$strategy <- "proportional"
    }
    if (!any(names(theDots) == "constant")){
      theDots$constant <- NULL
    }
    if (!any(names(theDots) == "random_state")){
      theDots$random_state <- NULL
    }
    model <- dummy_classifier(theDots$strategy, theDots$constant, theDots$random_state)(x, y)
    model$levels <- levels(model$y)
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

dummy_regressor <- function(strategy = "mean", quantile = NULL, constant = NULL) {
  if (!strategy %in% c("mean", "median", "quantile", "constant")) {
    stop("Invalid strategy. Choose from 'mean', 'median', 'quantile', or 'constant'.")
  }
  if (strategy == "quantile" & is.null(quantile)) {
    stop("For quantile strategy, you have to specify the desired quantile in the range [0, 1].")
  }
  if (strategy == "constant" & is.null(constant)) {
    stop("For constant strategy, you need to provide a constant value.")
  }
  
  function(X, y) {
    model <- list()
    model$strategy <- strategy
    model$quantile <- quantile
    model$constant <- constant
    model$y <- y
    return(model)
  }
}

predict_dummy_regressor <- function(object, X) {
  
  n_samples <- nrow(X)
  strategy <- object$strategy
  percentile <- object$quantile
  constant <- object$constant
  y <- object$y
  if (!(is.numeric(y)|is.integer(y))) stop("The response variable is not numerical.")
  if (strategy == "mean") {
    y.hat <- rep(mean(y, na.rm=T), n_samples)
  } else if (strategy == c("median")) {
    y.hat <- rep(median(y, na.rm=T), n_samples)
  } else if (strategy == "quantile") {
    if (percentile < 0 | percentile > 1){
      stop("quantile must be a value in the range [0, 1].")
    } 
    y.hat <- rep(unname(quantile(y, percentile, na.rm=T)), n_samples)
  } else if (strategy == "constant") {
    y.hat <- rep(constant, n_samples)
  } else {
    stop("Invalid strategy specified.")
  }
  
  return(y.hat)
}

dummy_regressor_caret <- list(
  label = "Dummy Regressor",
  library = NULL,
  type = "Regression",
  parameters = data.frame(parameter = "parameter",
                          class = "character",
                          label = "Strategy"),
  grid = function(x, y, len = NULL, search = "grid") {
    data.frame(parameter = "none")
  },
  loop = NULL,
  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
    theDots <- list(...)
    if (!any(names(theDots) == "strategy")){
      theDots$strategy <- "mean"
    }
    if (!any(names(theDots) == "quantile")){
      theDots$quantile <- NULL
    }
    if (!any(names(theDots) == "constant")){
      theDots$constant <- NULL
    }
    model <- dummy_regressor(theDots$strategy, theDots$quantile, theDots$constant)(x, y)
    model$levels <- lev
    model
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



library(caret)
data(iris)


# Split the data into training and testing sets

set.seed(2023)

index <- sample(1:nrow(iris), nrow(iris) * 0.8)

train_data <- iris[index,]

test_data <- iris[-index,]


ctrl1 <- trainControl(method = "none")
# Train a dummy classifier with caret

classifier_model_caret <- train(Species ~ ., data = train_data,
                                method = dummy_classifier_caret, 
                                strategy = "most_frequent", 
                                trControl = ctrl1)


# Make predictions using the trained dummy classifier

classifier_predictions_caret <- predict(classifier_model_caret, test_data)



# Evaluate the performance of the dummy classifier

classifier_confusion_matrix <- caret::confusionMatrix(classifier_predictions_caret, test_data$Species)

print(classifier_confusion_matrix)


regression_model_caret <- train(Sepal.Length~., data = train_data,
                                method = dummy_regressor_caret, 
                                strategy = "most_frequent",
                                trControl = ctrl1)

y.hat <- predict(regression_model_caret, test_data)

library(ModelMetrics)
mse(test_data[,1], y.hat)
