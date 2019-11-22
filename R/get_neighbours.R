#' Get Neighbours of a Node
#'
#' @name GenBinomial
#'
#' @description Density, distribution function, quantile function and random
#'     generation for the generalised binomial distribution with parameters
#'     \code{pi}, \code{theta} and \code{size}.
#'
#' @param x x coordinate of node.
#' @param y y coordinate of node.
#' @param size vector of dimensions of the lattice in the form \code{c(x,y)}.
#'
#' @details For a given node on a lattice of dimensions specified by \code{size}
#'     calculate the neighbouring nodes according to the von Neumann method.
#'
#' @export

get_neighbours <- function(x, y, size) {

  if (missing(x)) {
    stop('x is missing and is a required input')
  } else if (!is.numeric(x)) {
    stop('x must be a numeric argument')
  } else if (floor(x) != x | x < 1) {
    stop('x must be an integer greater than 1')
  }

  if (missing(y)) {
    stop('y is missing and is a required input')
  } else if (!is.numeric(y)) {
    stop('y must be a numeric argument')
  } else if (floor(y) != y | y < 1) {
    stop('y must be an integer greater than 1')
  }

  if (missing(size)) {
    stop('size is missing and is a required input')
  } else if (!is.numeric(size)) {
    stop('size must be a numeric vector')
  } else if (any(size < 1) | any(floor(size) != size) | length(size) != 2) {
    stop('size must be a vector of length 2 of integers greater than 1')
  }

  north <- c(x - 1, y)
  south <- c(x + 1, y)
  east <- c(x, y + 1)
  west <- c(x, y - 1)

  if (any(north < 1) | north[1] > size[1] | north[2] > size[2]) {
    north <- c(NA, NA)
  }

  if (any(south < 1) | south[1] > size[1] | south[2] > size[2]) {
    south <- c(NA, NA)
  }

  if (any(west < 1) | west[1] > size[1] | west[2] > size[2]) {
    west <- c(NA, NA)
  }

  if (any(east < 1) | east[1] > size[1] | east[2] > size[2]) {
    east <- c(NA, NA)
  }

  neighbours <- matrix(c(north, south, east, west), ncol = 4, nrow = 2)
  out <- list()
  out$x <- neighbours[1, !is.na(neighbours[1, ])]
  out$y <- neighbours[2, !is.na(neighbours[2, ])]

  return(out)
}
