context("check binomials")

test_that("bin_choose is working", {
  expect_error(bin_choose(n = 1, k = 2))
  expect_error(bin_choose(n = "ten", k = 2))
  expect_true(length(bin_choose(n = 3:5, k = 2)) == 3)
})

test_that("bin_probability is working", {
  expect_error(bin_probability(success = -2, trials = 3, prob = 0.5))
  expect_error(bin_probability(success = "two", trials = 3, prob = 0.5))
  expect_true(bin_probability(success = 2:4, trials = 5, prob = 0.5))
})

test_that("bin_distribution is working", {
  expect_error(bin_distribution(trials = -5, prob = 0.5))
  expect_error(bin_distribution(trials = "five", prob = 0.5))
  expect_true(nrow(bin_distribution(trials = 5, prob = 0.5)) == 5)
})

test_that("bin_cumulative is working", {
  expect_error(bin_cumulative(trials = -5, prob = 0.5))
  expect_error(bin_cumulative(trials = "five", prob = 0.5))
  expect_true(nrow(bin_cumulative(trials = 5, prob = 0.5)) == 6)
})
