# Check 1: Compute the proportion of Y = 1 for X = 0

test_that("function works", {
  # Set the expected proportion
  baseprob <- 0.1
  # Generate data using binosim
  sim_data <- binosim(100000, 0.3, baseprob, 1)
  # Compute the proportion of Y = 1 for X = 0
  observed_prop <- mean(sim_data$y[sim_data$x == 0])
  # Set the expected proportion based on simulation parameter
  expected_prop <- baseprob
  # Check if the computed proportion is as expected within a tolerance of 0.05
  expect_equal(observed_prop, expected_prop, tolerance = 0.05)
})
