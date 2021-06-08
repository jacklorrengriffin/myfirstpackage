#' K Nearest Neighbours
#'
#' Predicts output class species using covariates bill_length_mm, bill_depth_mm,
#' flipper_length_mm, and body_mass_g.
#'
#' @param train input data frame
#' @param cl true class value of your training data
#' @param k_nn integer representing the number of neighbors
#' @param k_cv integer representing the number of folds
#' @keywords prediction
#'
#' @importFrom stats model.matrix model.response model.frame pt na.omit predict sd
#'
#' @return list containing a vector of the predicted class for all observations,
#' and a numberic with the cross-validation and misclassification error
#'
#' @examples
#' my_penguins <- na.omit(my_penguins)
#' my_knn_cv(my_penguins[,3:6], my_penguins$species, 1, 2)
#'
#' @export
my_knn_cv <- function(train, cl, k_nn, k_cv) {
  # randomly split data into k_cv parts
  fold <- sample(rep(1:k_cv, length = nrow(train)))
  mc_rate <- vector(mode = "numeric", length = k_cv)

  # Iterate through segments, predicting the class of the fold and counting mistakes
  missclass_rate <- rep(NA, k_cv)
  for (i in 1:k_cv) {
    x_train <- train[which(fold != i),]
    x_test <- train[which(fold == i),]
    y_train <- cl[which(fold != i)]
    y_test <- cl[which(fold == i)]
    class <- class::knn(x_train, x_test, cl = y_train, k = k_nn)
    missclass_rate[i] <- mean(class != y_test)
  }
  cv_err <- mean(missclass_rate)
  predictions <- class::knn(train, train, cl = cl, k = k_nn)
  result <- list(predictions, cv_err)
  return (result)
}
