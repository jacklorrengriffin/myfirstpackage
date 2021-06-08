#' Linear Model
#'
#' This function fits a linear model in R
#'
#' @param formula a formula class object
#' @param data: input data frame
#' @keywords inference
#'
#' @return table with rows for each coefficient and columns for the estimate,
#' standard error, t value, and Pr(>|t|)
#'
#' @examples
#' # my_lm(formula, x)
#' # my_lm(formula, y)
#' x <- 1 : 50
#' y <- rnorm(50, mean = 25, sd = 10)
#' z <- rnorm(50, mean = 25, sd = 5)
#' my_data <- data.frame("X" = x, "Y" = y, "Z" = z)
#'
#' @export
my_lm <- function(data, formula) {
  x <- model.matrix(object = formula, data = data)
  y <- model.response(model.frame(formula = formula, data = data))
  B <- solve(t(x) %*% x) %*% t(x) %*% y
  df <- dim(data)[1] - length(B)
  sigma_sq <- sum((y - x %*% B) * (y - x %*% B) / df)
  B_error <- diag(sqrt(sigma_sq * solve(t(x) %*% x)))
  t <- B / B_error
  p_values <- 2 * pt(abs(t), df, lower.tail = FALSE)
  result <- data.frame("coefficients" = B,
                       "err" = B_error,
                       "t" = t,
                       "Pr(>|t|)" = p_values
  )
  result.table <- as.table(as.matrix(result))
  return(result.table)
}
