library(dplyr)
#' Error handling for the fdp_eval function output
#'
#' This function takes as input the output of fdp_eval function
#' and checks if it inherits from 'try-error'. If so, it
#' returns a data frame with NA values, otherwise, it returns the original input.
#'
#' @param x Output from fdp_eval function
#' @return A data frame with columns `rjs`, `pow`, `FDP`, `FWER`. If the input has 'try-error' class, the data frame will be filled with NA values.
#' @export
#' @examples
#' save.seed <- .Random.seed
#' set.seed(1)
#' X <- runif(20000, min = 0, max = 2.5) # covariate
#' H <- rbinom(20000, 1, 0.1) # hypothesis true or false
#' Z <- rnorm(20000, H * X) # Z-score
#' .Random.seed <- save.seed
#' pvalue <- 1 - pnorm(Z) # pvalue
#' rjs <- p.adjust(pvalue, method = "BH") <= 0.1
#' error_fdp_table(fdp_eval(H, rjs)) # Expected output: data frame with fdp_eval output
error_fdp_table <- function(x) {
  if (inherits(x, "try-error")) {
    x <- data.frame(rjs = NA, pow = NA, FDP = NA, FWER = NA)
  }
  x
}


# Evaluate Multiple Testing Procedure
#
# This function calculates power, false discovery proportion, and family-wise error rate for the given hypotheses
# and rejections. If no hypotheses are provided, it only returns the total count of rejections.
#
# @param Hs A numeric vector indicating the true (1) and false (0) hypotheses. Defaults to NULL.
# @param rjs A numeric vector indicating the rejected hypotheses.
# @return A data frame with columns 'rjs' (total rejections), 'pow' (Power), 'FDP' (False Discovery Proportion), 'FWER' (Family-wise Error Rate). If no hypotheses are provided, only 'rjs' is returned.
# @export
#
# Examples:
# fdp_eval(rjs = c(0,1,1,1,0)) # only 'rjs' returned as no hypotheses are provided
# fdp_eval(Hs = c(1,1,0,0,1), rjs = c(0,1,1,1,0)) # 'rjs', 'pow', 'FDP', 'FWER' returned as hypotheses are provided
fdp_eval <- function(Hs = NULL, rjs) {
  rjs_total <- sum(rjs)
  if (is.null(Hs)) {
    data.frame(rjs = rjs_total)
  } else {
    pow <- sum(rjs * Hs) / max(1, sum(Hs))
    FDP <- sum(rjs * (1 - Hs)) / max(1, rjs_total)
    FWER <- sum((1 - Hs) * rjs) > 0
    data.frame(rjs = rjs_total, pow = pow, FDP = FDP, FWER = FWER)
  }
}

# Evaluate Multiple Testing Procedure with Error Handling
#
# This is a wrapper function for fdp_eval that includes error handling. The function attempts to execute the fdp_eval function
# and then applies the error_fdp_table function on the result, which could include an error.
#
# @param Hs A numeric vector indicating the true (1) and false (0) hypotheses. Defaults to NULL.
# @param rjs A numeric vector indicating the rejected hypotheses.
# @return The result of applying the error_fdp_table function on the fdp_eval result or the thrown error.
#
# Examples:
# fdp_eval_error_wrapper(rjs = c(0,1,1,1,0)) # Execute with only rejections vector
# fdp_eval_error_wrapper(Hs = c(1,1,0,0,1), rjs = c(0,1,1,1,0)) # Execute with both hypotheses and rejections vectors
fdp_eval_error_wrapper <- function(Hs = NULL, rjs) {
  error_fdp_table(try(fdp_eval(Hs, rjs)))
}