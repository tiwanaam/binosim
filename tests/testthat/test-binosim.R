test_that("binosim generates correct proportion for X = 0", {
  # Generate test data
  sim_data <- binosim(100000, 0.5, 0.3, 2)
  # Compute the proportion of Y = 1 for X = 0
  prop <- mean(sim_data$y[sim_data$x == 0])
  # Set the expected proportion based on simulation parameters
  expected_prop <- exp(log(0.3) / (1 - 0.3)) / (1 + exp(log(0.3) / (1 - 0.3)))
  # Check if the computed proportion is as expected within a tolerance
  expect_equal(prop, expected_prop, tolerance = 0.30)
})
