test_that("Calculating lattice energy", {
####---- Input Tests: lattice ----------------------------------------------####
  # Return error when lattice is missing
  expect_error(
    object = energy_lattice(),
    regexp = 'lattice is missing and is a required input'
  )
  # Return error when lattice is a character rather than a matrix
  expect_error(
    object = energy_lattice(lattice = 'test'),
    regexp = 'lattice must be a matrix'
  )
  # Return error when lattice is a vector rather than a matrix
  expect_error(
    object = energy_lattice(lattice = c(1, 2, 3, 4)),
    regexp = 'lattice must be a matrix'
  )
  # Return error when lattice is a numeric rather than a matrix
  expect_error(
    object = energy_lattice(lattice = 1),
    regexp = 'lattice must be a matrix'
  )
  # Return error when lattice is not a numeric matrix of 0 and 1
  expect_error(
    object = energy_lattice(
      lattice = matrix(
        c(1, 0, 0, 1, 0, 0, 2, 'test', 2.4),
        nrow = 3,
        ncol = 3
      )
    ),
    regexp = 'lattice matrix must include include values 0 or 1'
  )
####---- General Usage -----------------------------------------------------####
  # 2 x 2 example: All 0's
  expect_equal(
    object = energy_lattice(
      lattice = matrix(
        c(0, 0, 0, 0),
        nrow = 2,
        ncol = 2
      )
    ),
    expected = -8
  )
  # 3 x 3 example: Mixed
  expect_equal(
    object = energy_lattice(
      lattice = matrix(
        c(0, 1, 0, 1, 0, 0, 0, 0, 0),
        nrow = 3,
        ncol = 3
      )
    ),
    expected = 0
  )
  # 2 x 5 example: Mixed (Non-Square)
  expect_equal(
    object = energy_lattice(
      lattice = matrix(
        c(0, 0, 1, 0, 0, 1, 1, 0, 1, 1),
        nrow = 5,
        ncol = 2
      )
    ),
    expected = 10
  )
})
