## Here we test the plotting function, but not too much because we have a
## vignette covering all use cases in a more visual way.

set.seed(55)

# Simulate some data
data <- simulate_data(
  intercept_f = 0.1,
  intercept_m = 10,
  slope_f = 0.5,
  slope_m = 0.5,
  n = 20L
)  

# Test that the plotting function produces a plot
test_that("Plotting works", {
  
  # Make plots covering all options
  plots <- list(
    p1 = plot_data(data),
    p2 = plot_data(data, lines = FALSE),
    p3 = plot_data(data, midline = TRUE),
    p4 = plot_data(data, midline = TRUE, transform = log, inverse = exp),
    p5 = plot_data(data, midline = TRUE, transform = log, inverse = exp, refit = TRUE),
    p6 = plot_data(data, midline = TRUE, correct = TRUE),
    p7 = plot_data(data, midline = TRUE, correct = TRUE, transform = log)
  )
  
  # Are they plots?
  is_plot <- unlist(lapply(plots, function(x) "ggplot" %in% class(x)))
  
  # Check that they all are
  expect_equal(length(is_plot), length(plots)) # in case of NULL
  expect_true(all(is_plot))
  
})
