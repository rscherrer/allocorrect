## Here we test the plotting function, but not too much because we have a
## vignette covering all use cases in a more visual way.

# Test that the plotting function produces a plot
test_that("Plotting works", {
  
  set.seed(55)
  
  # Simulate some data
  data <- simulate_data(
    intercept_f = 0.1,
    intercept_m = 10,
    slope_f = 0.5,
    slope_m = 0.5,
    n = 20L
  )  
  
  # Make a plot
  plot <- plot_data(data)
  
  # Check that it is a plot
  expect_true("ggplot" %in% class(plot))
  
})
