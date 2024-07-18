## Here we test the simulation function.

# Test the simulation function
test_that("Simulation works", {
  
  # Simulate flat lines with and without noise
  data <- simulate_data(0, 0, 0, 0, n = 10L, sd_y = 0)
  data_noise <- simulate_data(0, 0, 0, 0, n = 10L)
  
  # All values should be zero, or not
  expect_true(all(data$y == 0))
  expect_false(all(data_noise$y == 0))
  
})

# Simulate transformed data
test_that("Simulation with transformation", {
  
  # Simulate with dummy transformation
  data <- simulate_data(0, 0, 0, 0, n = 10L, sd_y = 0, fun = function(x) x + 1)
  
  # All values should be one
  expect_true(all(data$y == 1))
  
})



