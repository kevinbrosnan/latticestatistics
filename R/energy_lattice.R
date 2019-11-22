#' Energy across a binary lattice
#'
#' @description Calculate the energy across a binary lattice of 0s and 1s.
#'
#' @param lattice lattice for which energy is required.
#'
#' @export
energy_lattice <- function(lattice) {

  if (missing(lattice)) {
    stop('lattice is missing and is a required input')
  } else if (!is.matrix(lattice)) {
    stop('lattice must be a matrix')
  } else if (!all(lattice %in% c(0, 1))) {
    stop('lattice matrix must include include values 0 or 1')
  }

  lattice.size <- dim(lattice)
  energy <- 0

  for (n in 1:lattice.size[1]) {
    for (m in 1:lattice.size[2]) {
      neighbours <- get_neighbours(x = n, y = m, size = lattice.size)
      neighbours <- neighbours$x + (neighbours$y - 1) * lattice.size[1]
      energy.node <- (2 * lattice[n, m] - 1) * sum(2 * lattice[neighbours] - 1)
      energy <- energy + energy.node
    }
  }

  energy <- - energy
  return(energy)
}
