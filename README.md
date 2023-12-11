# binosim

This is an R package that returns a data frame of X and Y binary data based on given logistic regression parameters.

### R Installation Instructions

Before you can download the 'binosim' package, install the 'devtools' package if you haven't already.

```{r}
install.packages("devtools") 
library(devtools)
devtools::install_github("tiwanaam/binosim")
library(binosim)
```

### Example

In this example, we use the 'binosim' package as part of a simulation study. The goal is to simulate binary data, fit a logistic regression model using the simulated data, and assess the performance of the simulation in capturing the true odds ratios within confidence intervals.

```{r}
# Set seed for reproducibility
set.seed(123)
# True odds ratio to be simulated
true_or <- 2
# Number of simulations
simnum <- 1000
# Create vectors to store results
est <- se <- p_value <- or <- lower_ci <- upper_ci <- in_ci <- rep(NA, simnum)
# Loop through simulations
for (s in 1:simnum) {
  # Simulate binary data using binosim
  sim_data <- binosim(1000, 0.5, 0.1, true_or)
  # Fit logistic regression model
  mod <- glm(y ~ x, data = sim_data, family = "binomial")
  # Extract coefficients and confidence intervals
  coef <- summary(mod)$coef
  est[s] <- coef[2, 1]
  se[s] <- coef[2, 2]
  p_value[s] <- coef[2, 4]
  or[s] <- exp(coef[2, 1])
  lower_ci[s] <- exp(confint(mod)[2, 1])
  upper_ci[s] <- exp(confint(mod)[2, 2])
  # Check if the true odds ratio is within the confidence interval
  in_ci[s] <- true_or >= lower_ci[s] & true_or <= upper_ci[s]
  # Print simulation number for tracking progress
  print(s)
}
```

Calculate the empirical coverage.

```{r}
# Calculate empirical coverage
coverage <- mean(in_ci)
coverage
```

Plot the distribution of the estimated odds ratios.

```{r}
# Plot the distribution of the estimated odds ratios
hist(or, main = "Distribution of Odds Ratios", xlab = "Odds Ratio", col = "lightblue", border = "black")
# Add a vertical line for the true odds ratio
abline(v = true_or, col = "red", lty = 2, lwd = 3)
```
