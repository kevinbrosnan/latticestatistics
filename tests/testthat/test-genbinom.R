test_that("Density of Generalised Binomial", {
####---- Input Tests: g ---------------------------------------------------####
  # Return error when g is missing
  expect_equal(
    object = dgenbinom(pi = 0, theta = 0, size = 4),
    expected = list()
  )
  # Return error when g is a double rather than an integer
  expect_error(
    object = dgenbinom(g = 1.1, pi = 0, theta = 0, size = 4),
    regexp = 'g must be an integer valued input'
  )
  # Return error when g is a character rather than an integer
  expect_error(
    object = dgenbinom(g = '123456', pi = 0, theta = 0, size = 4),
    regexp = 'g must be an integer valued input'
  )

####---- Input Tests: pi ---------------------------------------------------####
  # Return error when pi is missing
  expect_error(
    object = dgenbinom(g = 0, theta = 0, size = 4),
    regexp = 'pi is missing and is a required input'
  )
  # Return error when pi is less than 0
  expect_error(
    object = dgenbinom(g = 0, pi = -0.1, theta = 0, size = 4),
    regexp = 'pi must be a probability'
  )
  # Return error when pi is greater than 1
  expect_error(
    object = dgenbinom(g = 0, pi = 1.1, theta = 0, size = 4),
    regexp = 'pi must be a probability'
  )
  # Return error when pi is a character rather than a numeric
  expect_error(
    object = dgenbinom(g = 0, pi = '123445', theta = 0, size = 4),
    regexp = 'pi must be a numeric argument'
  )

####---- Input Tests: theta ---------------------------------------------------####
  # Return error when theta is missing
  expect_error(
    object = dgenbinom(g = 0, pi = 0, size = 4),
    regexp = 'theta is missing and is a required input'
  )
  # Return error when theta is less than 0
  expect_error(
    object = dgenbinom(g = 0, pi = 0, theta = -1.5, size = 4),
    regexp = 'theta must be a probability'
  )
  # Return error when theta is greater than 1
  expect_error(
    object = dgenbinom(g = 0, pi = 0, theta = 10.5, size = 4),
    regexp = 'theta must be a probability'
  )
  # Return error when theta is a character rather than an integer
  expect_error(
    object = dgenbinom(g = 0, pi = 0, theta = '1234456', size = 4),
    regexp = 'theta must be a numeric argument'
  )

####---- Input Tests: size ---------------------------------------------------####
  # Return error when size is missing
  expect_error(
    object = dgenbinom(g = 0, pi = 0, theta = 0),
    regexp = 'size is missing and is a required input'
  )
  # Return error when size is less than 1
  expect_error(
    object = dgenbinom(g = 0, pi = 0, theta = 0.8, size = -6),
    regexp = 'size must be greater than 1'
  )
  # Return error when size is not an integer
  expect_error(
    object = dgenbinom(g = 0, pi = 0, theta = 0.8, size = 4.1234),
    regexp = 'size must be greater than 1'
  )
  # Return error when size is a character rather than an integer
  expect_error(
    object = dgenbinom(g = 0, pi = 0, theta = 0.8, size = '4545454'),
    regexp = 'size must be an integer valued input'
  )

####---- General Usage -----------------------------------------------------####
  # Return PMF of zero when g < 0
  expect_equal(
    object = dgenbinom(g = -1, pi = 0, theta = 0, size = 4),
    expected = 0
  )
  # Return PMF of zero when g > size
  expect_equal(
    object = dgenbinom(g = 5,  pi = 0, theta = 0, size = 4),
    expected = 0
  )
  # Return PMF of one when g = pi = theta = 0
  expect_equal(
    object = dgenbinom(g = 0,  pi = 0, theta = 0, size = 4),
    expected = 1
  )
  # Return corect PMF when g = 1, pi = theta = 0.25 & size = 4
  expect_equal(
    object = dgenbinom(g = 1,  pi = 0.25, theta = 0.25, size = 4),
    expected = 27 / 256
  )
})

test_that("Distribution of Generalised Binomial", {
####---- Input Tests: g ---------------------------------------------------####
  # Return error when g is missing
  expect_equal(
    object = pgenbinom(pi = 0, theta = 0, size = 4),
    expected = list()
  )
  # Return error when g is a double rather than an integer
  expect_error(
    object = pgenbinom(g = 1.1, pi = 0, theta = 0, size = 4),
    regexp = 'g must be an integer valued input'
  )
  # Return error when g is a character rather than an integer
  expect_error(
    object = pgenbinom(g = '123456', pi = 0, theta = 0, size = 4),
    regexp = 'g must be an integer valued input'
  )

####---- Input Tests: pi ---------------------------------------------------####
  # Return error when pi is missing
  expect_error(
    object = pgenbinom(g = 0, theta = 0, size = 4),
    regexp = 'pi is missing and is a required input'
  )
  # Return error when pi is less than 0
  expect_error(
    object = pgenbinom(g = 0, pi = -0.1, theta = 0, size = 4),
    regexp = 'pi must be a probability'
  )
  # Return error when pi is greater than 1
  expect_error(
    object = pgenbinom(g = 0, pi = 1.1, theta = 0, size = 4),
    regexp = 'pi must be a probability'
  )
  # Return error when pi is a character rather than a numeric
  expect_error(
    object = pgenbinom(g = 0, pi = '123445', theta = 0, size = 4),
    regexp = 'pi must be a numeric argument'
  )

####---- Input Tests: theta ---------------------------------------------------####
  # Return error when theta is missing
  expect_error(
    object = pgenbinom(g = 0, pi = 0, size = 4),
    regexp = 'theta is missing and is a required input'
  )
  # Return error when theta is less than 0
  expect_error(
    object = pgenbinom(g = 0, pi = 0, theta = -1.5, size = 4),
    regexp = 'theta must be a probability'
  )
  # Return error when theta is greater than 1
  expect_error(
    object = pgenbinom(g = 0, pi = 0, theta = 10.5, size = 4),
    regexp = 'theta must be a probability'
  )
  # Return error when theta is a character rather than an integer
  expect_error(
    object = pgenbinom(g = 0, pi = 0, theta = '1234456', size = 4),
    regexp = 'theta must be a numeric argument'
  )

####---- Input Tests: size ---------------------------------------------------####
  # Return error when size is missing
  expect_error(
    object = pgenbinom(g = 0, pi = 0, theta = 0),
    regexp = 'size is missing and is a required input'
  )
  # Return error when size is less than 1
  expect_error(
    object = pgenbinom(g = 0, pi = 0, theta = 0.8, size = -6),
    regexp = 'size must be greater than 1'
  )
  # Return error when size is not an integer
  expect_error(
    object = pgenbinom(g = 0, pi = 0, theta = 0.8, size = 4.1234),
    regexp = 'size must be greater than 1'
  )
  # Return error when size is a character rather than an integer
  expect_error(
    object = pgenbinom(g = 0, pi = 0, theta = 0.8, size = '4545454'),
    regexp = 'size must be an integer valued input'
  )
####---- Input Tests: lower.tail -------------------------------------------####
  # Return error when lower.tail is not logical
  expect_error(
    object = pgenbinom(g = 0, pi = 0, theta = 0, size = 4, lower.tail = '123'),
    regexp = 'lower.tail must be either TRUE or FALSE'
  )
####---- General Usage -----------------------------------------------------####
  # Return CDF of zero when g < 0
  expect_equal(
    object = pgenbinom(g = -1, pi = 0, theta = 0, size = 4),
    expected = 0
  )
  # Return CDF of one when g > size
  expect_equal(
    object = pgenbinom(g = 5,  pi = 0, theta = 0, size = 4),
    expected = 1
  )
  # Return correct CDF when g = 2, pi = theta = 0.25 & size = 4
  expect_equal(
    object = pgenbinom(g = 2,  pi = 0.25, theta = 0.25, size = 4),
    expected = 849 / 1024 + 27 / 256 + 54 / 1024
  )
  # Return correct CDF when g = 2, pi = theta = 0.25, size = 4 & lower.tail = F
  expect_equal(
    object = pgenbinom(g = 2,  pi = 0.25, theta = 0.25, size = 4,
                       lower.tail = FALSE),
    expected = 1 - (849 / 1024 + 27 / 256 + 54 / 1024)
  )
})

test_that("Quantiles of Generalised Binomial", {
####---- Input Tests: p ---------------------------------------------------####
  # Return error when p is missing
  expect_equal(
    object = qgenbinom(pi = 0, theta = 0, size = 4),
    expected = list()
  )
  # Return error when p is greater than 1
  expect_error(
    object = qgenbinom(p = 1.1, pi = 0, theta = 0, size = 4),
    regexp = 'p must be a probability'
  )
  # Return error when p is less than 0
  expect_error(
    object = qgenbinom(p = -0.4, pi = 0, theta = 0, size = 4),
    regexp = 'p must be a probability'
  )
  # Return error when p is a character rather than numeric
  expect_error(
    object = qgenbinom(p = '123456', pi = 0, theta = 0, size = 4),
    regexp = 'p must be a numeric argument'
  )

####---- Input Tests: pi ---------------------------------------------------####
  # Return error when pi is missing
  expect_error(
    object = qgenbinom(p = 0, theta = 0, size = 4),
    regexp = 'pi is missing and is a required input'
  )
  # Return error when pi is less than 0
  expect_error(
    object = qgenbinom(p = 0, pi = -0.1, theta = 0, size = 4),
    regexp = 'pi must be a probability'
  )
  # Return error when pi is greater than 1
  expect_error(
    object = qgenbinom(p = 0, pi = 1.1, theta = 0, size = 4),
    regexp = 'pi must be a probability'
  )
  # Return error when pi is a character rather than a numeric
  expect_error(
    object = qgenbinom(p = 0, pi = '123445', theta = 0, size = 4),
    regexp = 'pi must be a numeric argument'
  )

####---- Input Tests: theta ---------------------------------------------------####
  # Return error when theta is missing
  expect_error(
    object = qgenbinom(p = 0, pi = 0, size = 4),
    regexp = 'theta is missing and is a required input'
  )
  # Return error when theta is less than 0
  expect_error(
    object = qgenbinom(p = 0, pi = 0, theta = -1.5, size = 4),
    regexp = 'theta must be a probability'
  )
  # Return error when theta is greater than 1
  expect_error(
    object = qgenbinom(p = 0, pi = 0, theta = 10.5, size = 4),
    regexp = 'theta must be a probability'
  )
  # Return error when theta is a character rather than an integer
  expect_error(
    object = qgenbinom(p = 0, pi = 0, theta = '1234456', size = 4),
    regexp = 'theta must be a numeric argument'
  )

####---- Input Tests: size ---------------------------------------------------####
  # Return error when size is missing
  expect_error(
    object = qgenbinom(p = 0, pi = 0, theta = 0),
    regexp = 'size is missing and is a required input'
  )
  # Return error when size is less than 1
  expect_error(
    object = qgenbinom(p = 0, pi = 0, theta = 0.8, size = -6),
    regexp = 'size must be greater than 1'
  )
  # Return error when size is not an integer
  expect_error(
    object = qgenbinom(p = 0, pi = 0, theta = 0.8, size = 4.1234),
    regexp = 'size must be greater than 1'
  )
  # Return error when size is a character rather than an integer
  expect_error(
    object = qgenbinom(p = 0, pi = 0, theta = 0.8, size = '4545454'),
    regexp = 'size must be an integer valued input'
  )

####---- Input Tests: lower.tail -------------------------------------------####
  # Return error when lower.tail is not logical
  expect_error(
    object = qgenbinom(p = 0, pi = 0, theta = 0, size = 4, lower.tail = '123'),
    regexp = 'lower.tail must be either TRUE or FALSE'
  )

####---- General Usage -----------------------------------------------------####
  # Return 0 as quantile cut off
  expect_equal(
    object = qgenbinom(p = 0, pi = 0, theta = 0, size = 4),
    expected = 0
  )
  # Return size as quantile cut off
  expect_equal(
    object = qgenbinom(p = 0, pi = 0, theta = 0, size = 4, lower.tail = FALSE),
    expected = 4
  )
  # Return size as quantile cut off
  expect_equal(
    object = qgenbinom(p = 1, pi = 0, theta = 0, size = 4),
    expected = 4
  )
  # Return 0 as quantile cut off
  expect_equal(
    object = qgenbinom(p = 1, pi = 0, theta = 0, size = 4, lower.tail = FALSE),
    expected = 0
  )
  # Return correct CDF when g = 2, pi = theta = 0.25 & size = 4
  expect_equal(
    object = qgenbinom(p = 0.98,  pi = 0.25, theta = 0.25, size = 4),
    expected = 2
  )
  # Return correct CDF when g = 2, pi = theta = 0.25, size = 4 & lower.tail = F
  expect_equal(
    object = qgenbinom(p = 0.98,  pi = 0.25, theta = 0.25, size = 4,
                       lower.tail = FALSE),
    expected = 0
  )
})

test_that("Random generation of Generalised Binomial", {
####---- Input Tests: n ---------------------------------------------------####
  # Return error when n is missing
  expect_error(
    object = rgenbinom(pi = 0, theta = 0, size = 4),
    regexp = 'n is missing and is a required input'
  )
  # Return error when n is a double rather than an integer
  expect_error(
    object = rgenbinom(n = 1.1, pi = 0, theta = 0, size = 4),
    regexp = 'n must be an integer valued input greater than 0'
  )
  # Return error when n is less than 1
  expect_error(
    object = rgenbinom(n = 0, pi = 0, theta = 0, size = 4),
    regexp = 'n must be an integer valued input greater than 0'
  )
  # Return error when n is a character rather than an integer
  expect_error(
    object = rgenbinom(n = '123456', pi = 0, theta = 0, size = 4),
    regexp = 'n must be an integer valued input'
  )

####---- Input Tests: pi ---------------------------------------------------####
  # Return error when pi is missing
  expect_error(
    object = rgenbinom(n = 1, theta = 0, size = 4),
    regexp = 'pi is missing and is a required input'
  )
  # Return error when pi is less than 0
  expect_error(
    object = rgenbinom(n = 1, pi = -0.1, theta = 0, size = 4),
    regexp = 'pi must be a probability'
  )
  # Return error when pi is greater than 1
  expect_error(
    object = rgenbinom(n = 1, pi = 1.1, theta = 0, size = 4),
    regexp = 'pi must be a probability'
  )
  # Return error when pi is a character rather than a numeric
  expect_error(
    object = rgenbinom(n = 1, pi = '123445', theta = 0, size = 4),
    regexp = 'pi must be a numeric argument'
  )

####---- Input Tests: theta ---------------------------------------------------####
  # Return error when theta is missing
  expect_error(
    object = rgenbinom(n = 1, pi = 0, size = 4),
    regexp = 'theta is missing and is a required input'
  )
  # Return error when theta is less than 0
  expect_error(
    object = rgenbinom(n = 1, pi = 0, theta = -1.5, size = 4),
    regexp = 'theta must be a probability'
  )
  # Return error when theta is greater than 1
  expect_error(
    object = rgenbinom(n = 1, pi = 0, theta = 10.5, size = 4),
    regexp = 'theta must be a probability'
  )
  # Return error when theta is a character rather than an integer
  expect_error(
    object = rgenbinom(n = 1, pi = 0, theta = '1234456', size = 4),
    regexp = 'theta must be a numeric argument'
  )

####---- Input Tests: size ---------------------------------------------------####
  # Return error when size is missing
  expect_error(
    object = rgenbinom(n = 1, pi = 0, theta = 0),
    regexp = 'size is missing and is a required input'
  )
  # Return error when size is less than 1
  expect_error(
    object = rgenbinom(n = 1, pi = 0, theta = 0.8, size = -6),
    regexp = 'size must be greater than 1'
  )
  # Return error when size is not an integer
  expect_error(
    object = rgenbinom(n = 1, pi = 0, theta = 0.8, size = 4.1234),
    regexp = 'size must be greater than 1'
  )
  # Return error when size is a character rather than an integer
  expect_error(
    object = rgenbinom(n = 1, pi = 0, theta = 0.8, size = '4545454'),
    regexp = 'size must be an integer valued input'
  )

####---- General Usage -----------------------------------------------------####
  # Return that random generation is less than 1
  expect_lte(
    object = max(rgenbinom(n = 100, pi = 0, theta = 0, size = 4)),
    expected = 1
  )
  # Return 0 as quantile cut off
  expect_gte(
    object = min(rgenbinom(n = 100, pi = 0, theta = 0, size = 4)),
    expected = 0
  )
})
