context("check the checkers")

test_that("check_prob is working", {
  expect_error(check_prob(prob = -1))
  expect_error(check_prob(prob = 3))
  expect_true(check_prob(prob = 0:1))
})

test_that("check_trials is working", {
  expect_error(check_trials(trials = -1))
  expect_true(check_trials(trials = 3))
  expect_error(check_trials(trials = "hello"))
})

test_that("check_success is working", {
  expect_error(check_success(success = -1,trials= -1))
  expect_error(check_success(success = 3,trials= 2))
  expect_error(check_success(success = 3,trials= -2))
})
