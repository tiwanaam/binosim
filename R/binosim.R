#' @title Simulate binary data from logistic regression.
#' @param n Sample size.
#' @param px Probability of x.
#' @param baselineprob Baseline probability of Y.
#' @param or True odds ratio.
#' @return A data frame with binary data
#' @author Amrit Tiwana
#' @examples
#' sim_data <- binosim(n = 10, px = 0.6, baselineprob = 1, or = 1)
#' sim_data
#' @export

# Simulate binary data from simple logistic regression
binosim <- function(n, px, baselineprob, or) {
  # Generate predictor variable X from a binomial distribution
  x <- rbinom(n, size = 1, prob = px)
  # Calculate the log-odds using baseline probability as the intercept
  log_odds <- baselineprob + log(or) * x
  # Calculate the probability of success for Y given the log-odds
  py <- exp(log_odds) / (1 + exp(log_odds))
  # Generate outcome variable Y from a binomial distribution
  y <- rbinom(n, size = 1, prob = py)
  # Combine X and Y variables into a data frame
  dat <- data.frame(y = y, x = x)
  # Return the simulated data
  return(dat)
}
