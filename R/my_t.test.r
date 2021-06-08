#' T-test
#'
#' This function performs a one sample t-test in R.
#'
#' @param x a numeric vector of data
#' @param alternative a character string specifying the alternative hypothesis.
#' @param mu a number indicating the null hypothesis value of the mean.
#' @keywords inference
#'
#' @return list containing the test statistic, degrees of freedom, value of the
#' parameter alternative, and the p-value
#'
#' @examples
#' x <- 1:10
#' my_t.test(x, "less", 5)
#'
#' @export
my_t.test <- function(x, alternative, mu) {
  mu.hat <- mean(x)
  test_stat <- (mu.hat - mu)/(sd(x)/sqrt(length(x)))
  if (alternative == "greater") {
    my_pval <- pt(test_stat, df = length(x) - 1, lower.tail = FALSE)
  }
  else if (alternative == "two.sided") {
    my_pval <- 2 * (pt(-abs(test_stat), df = length(x) - 1, lower.tail = FALSE))
  }
  else if (alternative == "less") {
    my_pval <- pt(test_stat, df = length(x) - 1, lower.tail = TRUE)
  }
  else {
    stop("Alternative must be greater, two.sided, or less ")
  }
  result <- list()
  result[["test_stat"]] <- test_stat
  result[["df"]] <- length(x) - 1
  result[["alternative"]] <- alternative
  result[["p_val"]] <- my_pval
  return(result)
}
