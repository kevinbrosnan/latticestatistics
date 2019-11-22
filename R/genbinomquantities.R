#' The Generalised Binomial Quantities
#'
#' @name GenBinomialQuantities
#'
#' @description Expectation and variance for the generalised binomial
#'     distribution with parameters \code{pi}, \code{theta} and \code{size}.
#'
#' @param pi probability of success of the bernoulli.
#' @param theta probability of success on each binomial trial.
#' @param size number of trials.
#'
#' @details This function derives the expectation and variance for
#'     the generalised binomial distribution with \code{size = n},
#'     \code{pi = pi} and \code{theta = theta}
#'
#' @aliases genbinom_expectation genbinom_variance

## GenBinom Expectation ##
#' @export
genbinom_expectation <- function(pi, theta, size) {

  if (missing(pi)) {
    stop('pi is missing and is a required input')
  } else if (!is.numeric(pi)) {
    stop('pi must be a numeric argument')
  } else if (pi < 0 | pi > 1) {
    stop('pi must be a probability')
  }

  if (missing(theta)) {
    stop('theta is missing and is a required input')
  } else if (!is.numeric(theta)) {
    stop('theta must be a numeric argument')
  } else if (theta < 0 | theta > 1) {
    stop('theta must be a probability')
  }

  if (missing(size)) {
    stop('size is missing and is a required input')
  } else if (!is.numeric(size)) {
    stop('size must be an integer valued input')
  } else if (size < 1 | floor(size) != size) {
    stop('size must be greater than 1')
  }

  exp_val <- pi * theta * size

  return(exp_val)
}

## GenBinom Variance ##
#' @export
genbinom_variance <- function(pi, theta, size) {

  if (missing(pi)) {
    stop('pi is missing and is a required input')
  } else if (!is.numeric(pi)) {
    stop('pi must be a numeric argument')
  } else if (pi < 0 | pi > 1) {
    stop('pi must be a probability')
  }

  if (missing(theta)) {
    stop('theta is missing and is a required input')
  } else if (!is.numeric(theta)) {
    stop('theta must be a numeric argument')
  } else if (theta < 0 | theta > 1) {
    stop('theta must be a probability')
  }

  if (missing(size)) {
    stop('size is missing and is a required input')
  } else if (!is.numeric(size)) {
    stop('size must be an integer valued input')
  } else if (size < 1 | floor(size) != size) {
    stop('size must be greater than 1')
  }

  variance <- pi * size * theta * (1 - theta + size * theta * (1 - pi))

  return(variance)
}
