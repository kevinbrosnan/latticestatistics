test_that("Get Neigbours", {

####---- Input Tests: x ---------------------------------------------------####
  # Return error when x is missing
  expect_error(
    object = get_neighbours(y = 1, size = c(5, 5)),
    regexp = 'x is missing and is a required input'
  )
  # Return error when x is a double rather than an integer
  expect_error(
    object = get_neighbours(x = 1.2, y = 1, size = c(5, 5)),
    regexp = 'x must be an integer greater than 1'
  )
  # Return error when x is not strictly positive
  expect_error(
    object = get_neighbours(x = -1, y = 1, size = c(5, 5)),
    regexp = 'x must be an integer greater than 1'
  )
  # Return error when x is a character rather than an integer
  expect_error(
    object = get_neighbours(x = '1.2', y = 1, size = c(5, 5)),
    regexp = 'x must be a numeric argument'
  )

####---- Input Tests: y ---------------------------------------------------####
  # Return error when y is missing
  expect_error(
    object = get_neighbours(x = 1, size = c(5, 5)),
    regexp = 'y is missing and is a required input'
  )
  # Return error when y is a double rather than an integer
  expect_error(
    object = get_neighbours(x = 1, y = 2.8, size = c(5, 5)),
    regexp = 'y must be an integer greater than 1'
  )
  # Return error when y is not strictly positive
  expect_error(
    object = get_neighbours(x = 1, y = -10, size = c(5, 5)),
    regexp = 'y must be an integer greater than 1'
  )
  # Return error when y is a character rather than an integer
  expect_error(
    object = get_neighbours(x = 1, y = 'test', size = c(5, 5)),
    regexp = 'y must be a numeric argument'
  )

####---- Input Tests: size -------------------------------------------------####
  # Return error when size is missing
  expect_error(
    object = get_neighbours(x = 1, y = 2),
    regexp = 'size is missing and is a required input'
  )
  # Return error when y is a double rather than an integer
  expect_error(
    object = get_neighbours(x = 1, y = 2, size = c(5, 5.2)),
    regexp = 'size must be a vector of length 2 of integers greater than 1'
  )
  # Return error when y is not strictly positive
  expect_error(
    object = get_neighbours(x = 1, y = 2, size = c(5, -1)),
    regexp = 'size must be a vector of length 2 of integers greater than 1'
  )
  # Return error when y is not strictly positive
  expect_error(
    object = get_neighbours(x = 1, y = 2, size = c(5, 1, 8)),
    regexp = 'size must be a vector of length 2 of integers greater than 1'
  )
  # Return error when y is a character rather than an integer
  expect_error(
    object = get_neighbours(x = 1, y = 1, size = c('test', 5)),
    regexp = 'size must be a numeric vector'
  )

####---- General Usage -----------------------------------------------------####
  # 2 x 2 matrix: Corner Node
  expect_equal(
    object = get_neighbours(x = 1, y = 1, size = c(2, 2)),
    expected = list(
      x = c(2, 1),
      y = c(1, 2)
    )
  )
  # 3 x 3 matrix: Corner Node
  expect_equal(
    object = get_neighbours(x = 3, y = 3, size = c(3, 3)),
    expected = list(
      x = c(2, 3),
      y = c(3, 2)
    )
  )
  # 3 x 3 matrix: Edge Node
  expect_equal(
    object = get_neighbours(x = 3, y = 2, size = c(3, 3)),
    expected = list(
      x = c(2, 3, 3),
      y = c(2, 3, 1)
    )
  )
  # 3 x 3 matrix: Inner Node
  expect_equal(
    object = get_neighbours(x = 2, y = 2, size = c(3, 3)),
    expected = list(
      x = c(1, 3, 2, 2),
      y = c(2, 2, 3, 1)
    )
  )
  # 4 x 2 matrix: Edge Node
  expect_equal(
    object = get_neighbours(x = 3, y = 1, size = c(4, 2)),
    expected = list(
      x = c(2, 4, 3),
      y = c(1, 1, 2)
    )
  )
})
