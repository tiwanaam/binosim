test_that("function works", {
  baseprob <- 0.1
  # Generate data using binosim
  sim_data <- binosim(100000, 0.5, baseprob, 2)
  # Compute the proportion of Y = 1 for X = 0
  prop <- mean(sim_data$y[sim_data$x == 0])
  # Set the expected proportion based on simulation parameters
  expected_prop <- exp(log(baseprob) / (1 - baseprob)) / (1 + exp(log(baseprob) / (1 - baseprob)))
  # Check if the computed proportion is as expected within a tolerance
  expect_equal(prop, expected_prop, tolerance = 0.05)
})
