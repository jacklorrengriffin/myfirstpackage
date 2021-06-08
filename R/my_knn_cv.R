#' K Nearest Neighbours
#'
#' Predicts output class species using covariates bill_length_mm, bill_depth_mm,
#' flipper_length_mm, and body_mass_g.
#'
#' @param train: input data frame
#' @param cl: true class value of your training data
#' @param k_nn: integer representing the number of neighbors
#' @param k_cv: integer representing the number of folds
#' @keywords prediction
#'
#' @return list containing a vector of the predicted class for all observations,
#' and a numberic with the cross-validation and misclassification error
#'
#' @examples
#' my_knn_cvv(mypenguins, mypenguins$species)
#' my_knn_cvv(data, daya$x)
#'
#' @export
my_knn_cv <- function(train, cl, k_nn, k_cv) {
  # randomly split data into k_cv parts
  fold <- sample(rep(1:k_cv, length = nrow(train)))
  mc_rate <- vector(mode = "numeric", length = k_cv)

  # Iterate through segments, predicting the class of the fold and counting mistakes
  class <- rep(NA, length(cl))
  for (i in 1:k_cv) {
    x_train <- train[which(fold != i),]
    x_test <- train[which(fold == i),]
    y_train <- cl[which(fold != i)]
    y_test <- cl[which(fold == i)]
    cl_train <- cl[x_train]
    cl_test <- cl[x_test]
    class <- class::knn(y_train, y_test, cl = cl_train, k = k_nn)
    missclass_rate[i] <- mean(class != cl_test)
  }
  cv_err <- mean(missclass_rate)
  predictions <- class::knn(y_train, y_test, cl = cl_train, k = k_nn)
  result <- list(predictions, cv_err)
  return (result)
}
