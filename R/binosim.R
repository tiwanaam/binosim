#' @title Simulate binary data from logistic regression.
#' @param n Sample size.
#' @param px Probability of x.
#' @param baseprob Baseline probability of Y.
#' @param or True odds ratio.
#' @return A data frame with binary data
#' @author Amrit Tiwana
#' @examples
#' sim_data <- binosim(n = 100000, px = 0.5, baseprob = 0.3, or = 2)
#' sim_data
#' @export

# Simulate binary data from simple logistic regression
binosim <- function(n, px, baseprob, or) {
  # Generate predictor variable X from a binomial distribution
  x <- rbinom(n, size = 1, prob = px)
  # Convert baseline probability to log-odds using the logit function
  baseprob <- log((baseprob) / (1 - baseprob))
  # Calculate the log-odds using baseline probability as the intercept
  logodds <- baseprob + log(or) * x
  # Calculate the probability of success for Y given the log-odds
  py <- exp(logodds) / (1 + exp(logodds))
  # Generate outcome variable Y from a binomial distribution
  y <- rbinom(n, size = 1, prob = py)
  # Combine X and Y variables into a data frame
  dat <- data.frame(y = y, x = x)
  # Return the simulated data
  return(dat)
}

