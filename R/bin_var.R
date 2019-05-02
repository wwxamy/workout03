check_prob <- function(prob){
  if (prob < 0) {
    stop("p has to be a number betwen 0 and 1")
  }
  if (prob > 1) {
    stop("p has to be a number betwen 0 and 1")
  }
}

check_trials <- function(trials){
  if (trials < 0) {
    stop("invalid trials value")
  }
}

check_success <- function(success, trials){
  if (success < 0) {
    stop("invalid success value")
  }
  if (success > trials) {
    stop("success need to be equal or less than trials")
  }
}

#' @title binomial random variable object
#' @description return binomial random variable object
#' @param trials number of trials
#' @param prob probability of success
#' @return binomial random variable object
#' @export
bin_variable <- function(trials, prob){
  check_prob(prob = prob)
  check_trials(trials = trials)
  list <- list("trials" = trials, "prob" = prob)
  class(list) <- "binvar"
  return(list)
}

#' @export
print.binvar <- function(trials, prob){
  cat('"Binomial variable"\n')
  cat('Parameters\n')
  cat(sprintf('trials: "%s"', trials), "\n")
  cat(sprintf('prob: "%s"', prob),"\n")
  invisible(x)
}

aux_mean <- function(trials, prob){
  return(trials*prob)
}

aux_variance <- function(trials, prob){
  trials*prob*(1-prob)
}

aux_mode <- function(trials, prob){
  if (is.integer(((trials*prob)+prob)) == FALSE) {
    return(floor(((trials*prob)+prob)))
  }
  if (is.integer(((trials*prob)+prob)) == TRUE) {
    return((floor(trials*prob)+prob-1), floor((trials*prob)+prob))
  }
}

aux_skewness <- function(trials, prob){
  (1-2*prob)/sqrt(trials*prob*(1-prob))
}

aux_kurtosis <- function(trials, prob){
  (1-6*prob*(1-prob))/(trials*prob*(1-prob))
}

#' @export
summary.binvar <- function(trials, prob){
  cat('"Binomial variable"\n', "\n")
  cat('Parameters\n')
  cat(sprintf('trials: "%s"', trials), "\n")
  cat(sprintf('prob: "%s"', prob),"\n", "\n")
  cat('Measures\n')
  cat(sprintf('mean: "%s"', aux_mean(trials = trials, prob = prob)), "\n")
  cat(sprintf('variance: "%s"', aux_variance(trials = trials, prob = prob)),"\n")
  cat(sprintf('mode: "%s"', aux_mode(trials = trials, prob = prob)),"\n")
  cat(sprintf('skewness: "%s"', aux_skewness(trials = trials, prob = prob)),"\n")
  cat(sprintf('kurtosis: "%s"', aux_kurtosis(trials = trials, prob = prob)),"\n")
  invisible(x)
}


