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

# Example data
set.seed(123)
sim_data <- binosim(n = 100000, px = 0.5, baseprob = 0.3, or = 2)

# Check 1: Compute the proportion of Y = 1 for X = 0
prop <- mean(sim_data$y[sim_data$x == 0])
prop

#Check 2: Fit a logistic regression model and check the exponent of the coefficient for x
model <- glm(y ~ x, data = sim_data, family = binomial)
or <- exp(coef(model)[2])
or


for(s in 1:simnum){
  sim_data <- binosim(n = 100000, px = 0.5, baseprob = 0.3, or = 2)
  ## fit unadjusted logistic regression model
  model <- glm(y ~ x, data = sim_data, family = binomial)
  or <- exp(summary(model)$coef['x', 'Estimate'])
  ## save p-values for coffee from both models in a vector
  unadj.p[s] <- or[2,4]
  ## show simulation progress
  print(s)
}

# Initialize an empty vector to store odds ratios
or_vector <- numeric(simnum)

for (s in 1:simnum) {
  sim_data <- binosim(n = 100000, px = 0.5, baseprob = 0.3, or = 2)
  ## fit unadjusted logistic regression model
  model <- glm(y ~ x, data = sim_data, family = binomial)
  or <- exp(summary(model)$coef['x', 'Estimate'])
  ## save odds ratio in the vector
  or_vector[s] <- or
  ## show simulation progress
  print(s)
}

# Calculate the mean of the odds ratios
mean_or <- mean(or_vector)

# Print or use the mean_or as needed
print(mean_or)

