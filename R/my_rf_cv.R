#' Random Forest Cross Validation
#'
#' Predicting a variable using covariates with random forest cross validation
#'
#' @param k number of folds
#' @keywords prediction
#'
#' @return a numeric with the cross validation error
#'
#' @examples
#' my_rf_cv(5)
#' my_rf_cv(1)
#'
#' @export
my_rf_cv <- function(k) {
  fold <- sample(rep(1:k, length = nrow(mypenguins)))
  fold_mse <- vector(mode = "numeric", length = k)

  for(i in 1:k) {
    train <- which(fold != i)
    test <- which(fold == i)
    train_data <- mypenguins[train,]
    test_data <- mypenguins[test,]
    model <- randomForest::randomForest(mypenguins$body_mass_g ~ mypenguins$bill_length_mm + mypenguins$bill_depth_mm + mypenguins$flipper_length_mm, data = train_data, ntree = 100)
    predictions <- predict(model, train_data)
    fold_mse[i] <- mean((predictions - train_data$body_mass_g)^2)

  }
    return(mean(fold_mse))
}
