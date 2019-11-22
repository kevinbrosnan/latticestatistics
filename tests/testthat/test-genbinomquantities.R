test_that("Expectation of Generalised Binomial", {
####---- Input Tests: pi ---------------------------------------------------####
  # Return error when pi is missing
  expect_error(
    object = genbinom_expectation(theta = 0, size = 4),
    regexp = 'pi is missing and is a required input'
  )
  # Return error when pi is less than 0
  expect_error(
    object = genbinom_expectation(pi = -0.1, theta = 0, size = 4),
    regexp = 'pi must be a probability'
  )
  # Return error when pi is greater than 1
  expect_error(
    object = genbinom_expectation(pi = 1.1, theta = 0, size = 4),
    regexp = 'pi must be a probability'
  )
  # Return error when pi is a character rather than a numeric
  expect_error(
    object = genbinom_expectation(pi = '123445', theta = 0, size = 4),
    regexp = 'pi must be a numeric argument'
  )

####---- Input Tests: theta ---------------------------------------------------####
  # Return error when theta is missing
  expect_error(
    object = genbinom_expectation(pi = 0, size = 4),
    regexp = 'theta is missing and is a required input'
  )
  # Return error when theta is less than 0
  expect_error(
    object = genbinom_expectation(pi = 0, theta = -1.5, size = 4),
    regexp = 'theta must be a probability'
  )
  # Return error when theta is greater than 1
  expect_error(
    object = genbinom_expectation(pi = 0, theta = 10.5, size = 4),
    regexp = 'theta must be a probability'
  )
  # Return error when theta is a character rather than an integer
  expect_error(
    object = genbinom_expectation(pi = 0, theta = '1234456', size = 4),
    regexp = 'theta must be a numeric argument'
  )

####---- Input Tests: size ---------------------------------------------------####
  # Return error when size is missing
  expect_error(
    object = genbinom_expectation(pi = 0, theta = 0),
    regexp = 'size is missing and is a required input'
  )
  # Return error when size is less than 1
  expect_error(
    object = genbinom_expectation(pi = 0, theta = 0.8, size = -6),
    regexp = 'size must be greater than 1'
  )
  # Return error when size is not an integer
  expect_error(
    object = genbinom_expectation(pi = 0, theta = 0.8, size = 4.1234),
    regexp = 'size must be greater than 1'
  )
  # Return error when size is a character rather than an integer
  expect_error(
    object = genbinom_expectation(pi = 0, theta = 0.8, size = '4545454'),
    regexp = 'size must be an integer valued input'
  )

####---- General Usage -----------------------------------------------------####
  # Return correct value for generalised case
  expect_equal(
    object = genbinom_expectation(pi = 0.25, theta = 0.5, size = 6),
    expected = 6 / 8
  )
  # Return correct value for symmetric generalised case
  expect_equal(
    object = genbinom_expectation(pi = 0.25, theta = 0.25, size = 6),
    expected = 6 / 16
  )
  # Return correct value for symmetric Bernoulli's
  expect_equal(
    object = genbinom_expectation(pi = 0.25, theta = 0.25, size = 1),
    expected = 1 / 16
  )
  # Return correct value for non-symmetric Bernoulli's
  expect_equal(
    object = genbinom_expectation(pi = 0.5, theta = 2 / 3, size = 1),
    expected = 1 / 3
  )
})

test_that("Variance of Generalised Binomial", {
####---- Input Tests: pi ---------------------------------------------------####
  # Return error when pi is missing
  expect_error(
    object = genbinom_variance(theta = 0, size = 4),
    regexp = 'pi is missing and is a required input'
  )
  # Return error when pi is less than 0
  expect_error(
    object = genbinom_variance(pi = -0.1, theta = 0, size = 4),
    regexp = 'pi must be a probability'
  )
  # Return error when pi is greater than 1
  expect_error(
    object = genbinom_variance(pi = 1.1, theta = 0, size = 4),
    regexp = 'pi must be a probability'
  )
  # Return error when pi is a character rather than a numeric
  expect_error(
    object = genbinom_variance(pi = '123445', theta = 0, size = 4),
    regexp = 'pi must be a numeric argument'
  )

####---- Input Tests: theta ---------------------------------------------------####
  # Return error when theta is missing
  expect_error(
    object = genbinom_variance(pi = 0, size = 4),
    regexp = 'theta is missing and is a required input'
  )
  # Return error when theta is less than 0
  expect_error(
    object = genbinom_variance(pi = 0, theta = -1.5, size = 4),
    regexp = 'theta must be a probability'
  )
  # Return error when theta is greater than 1
  expect_error(
    object = genbinom_variance(pi = 0, theta = 10.5, size = 4),
    regexp = 'theta must be a probability'
  )
  # Return error when theta is a character rather than an integer
  expect_error(
    object = genbinom_variance(pi = 0, theta = '1234456', size = 4),
    regexp = 'theta must be a numeric argument'
  )

####---- Input Tests: size ---------------------------------------------------####
  # Return error when size is missing
  expect_error(
    object = genbinom_variance(pi = 0, theta = 0),
    regexp = 'size is missing and is a required input'
  )
  # Return error when size is less than 1
  expect_error(
    object = genbinom_variance(pi = 0, theta = 0.8, size = -6),
    regexp = 'size must be greater than 1'
  )
  # Return error when size is not an integer
  expect_error(
    object = genbinom_variance(pi = 0, theta = 0.8, size = 4.1234),
    regexp = 'size must be greater than 1'
  )
  # Return error when size is a character rather than an integer
  expect_error(
    object = genbinom_variance(pi = 0, theta = 0.8, size = '4545454'),
    regexp = 'size must be an integer valued input'
  )

####---- General Usage -----------------------------------------------------####
  # Return correct value for generalised case
  expect_equal(
    object = genbinom_variance(pi = 0.25, theta = 0.5, size = 6),
    expected = 66 / 32
  )
  # Return correct value for symmetric generalised case
  expect_equal(
    object = genbinom_variance(pi = 0.25, theta = 0.25, size = 6),
    expected = 180 / 256
  )
  # Return correct value for symmetric Bernoulli's
  expect_equal(
    object = genbinom_variance(pi = 0.25, theta = 0.25, size = 1),
    expected = 15 / 256
  )
  # Return correct value for non-symmetric Bernoulli's
  expect_equal(
    object = genbinom_variance(pi = 0.5, theta = 2 / 3, size = 1),
    expected = 2 / 9
  )
})
