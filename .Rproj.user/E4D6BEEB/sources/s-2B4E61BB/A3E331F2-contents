context("check summary measures")

test_that("aux_mean is working", {
  expect_true(aux_mean(trials = 10, prob = 0.3) ==3)
  expect_error(aux_mean(trials = "ten", prob = 0.3))
  expect_error(length(aux_mean(trials = 1:5, prob = 0.3)) == 5)
})

test_that("aux_variance is working", {
  expect_true(aux_variance(trials = 10, prob = 0.3) == 2.1)
  expect_error(aux_variance(trials = "ten", prob = 0.3))
  expect_error(length(aux_variance(trials = 1:5, prob = 0.3)) == 5)
})

test_that("aux_mode is working", {
  expect_true(aux_mode(trials = 10, prob = 0.3) == 3)
  expect_error(aux_mode(trials = "ten", prob = 0.3))
  expect_error(length(aux_mode(trials = 1:5, prob = 0.3)) == 5)
})

test_that("aux_skewness is working", {
  expect_true(aux_skewness(trials = 10, prob = 0.3) == 0.2760262)
  expect_error(aux_skewness(trials = "ten", prob = 0.3))
  expect_error(length(aux_skewness(trials = 1:5, prob = 0.3)) == 5)
})

test_that("aux_kurtosis is working", {
  expect_true(aux_kurtosis(trials = 10, prob = 0.3) == -0.1238095)
  expect_error(aux_kurtosis(trials = "ten", prob = 0.3))
  expect_error(length(aux_kurtosis(trials = 1:5, prob = 0.3)) == 5)
})
