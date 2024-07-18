## ---- message = FALSE---------------------------------------------------------
# But first we setup R
rm(list = ls())

library(allocorrect)

ggplot2::theme_set(ggplot2::theme_classic())

set.seed(42) # for reproducibility

## -----------------------------------------------------------------------------
# Let us first simulate some data
data <- simulate_data(intercept_f = 0.1, intercept_m = 10, slope_f = 0.5, slope_m = 0.5, sd_y = 1)

# Check them out
data

## -----------------------------------------------------------------------------
# Plot
plot_data(data)

## -----------------------------------------------------------------------------
# Here is the global straight line
plot_data(data, midline = TRUE)

## -----------------------------------------------------------------------------
# Plot the corrected values
plot_data(data, midline = TRUE, correct = TRUE)

## -----------------------------------------------------------------------------
# Simulate non-linear relationship this time
data_nl <- simulate_data(intercept_f = 0.1, intercept_m = 10, slope_f = 0.5, slope_m = 0.5, sd_y = 1, fun = function(x) exp(x / 7))

# Note: the `fun` argument allows us to transform otherwise linear data.

## -----------------------------------------------------------------------------
# Plot the allometric data
plot_data(data_nl, midline = TRUE, linear = FALSE)

# Note: setting `linear` to FALSE makes sure we fit an allometric model and not a linear one

## -----------------------------------------------------------------------------
# Plot the corrected allometric data
plot_data(data_nl, midline = TRUE, correct = TRUE, linear = FALSE)

## -----------------------------------------------------------------------------
# Plot on a log-scale
plot_data(data_nl, midline = TRUE, linear = FALSE, transform = log, inverse = exp)

# Note: we have to provide the `inverse` of the re-scaling function.

## -----------------------------------------------------------------------------
# Correct the transformed data
plot_data(data_nl, midline = TRUE, linear = FALSE, correct = TRUE, transform = log, inverse = exp)

## -----------------------------------------------------------------------------
# Re-fit after transforming
plot_data(data_nl, midline = TRUE, linear = FALSE, transform = log, inverse = exp, refit = TRUE)

## -----------------------------------------------------------------------------
# Correct
plot_data(data_nl, midline = TRUE, linear = FALSE, transform = log, inverse = exp, refit = TRUE, correct = TRUE)

## -----------------------------------------------------------------------------
# Correct
plot_data(data_nl, midline = TRUE, linear = FALSE, transform = log, inverse = exp, refit = TRUE, linear_refit = TRUE, correct = TRUE)

# Note: this was done with `linear_refit`.

## -----------------------------------------------------------------------------
# Simulate heterogeneity along the horizontal axis
data_hg <- simulate_data(intercept_f = 0.1, intercept_m = 10, slope_f = 0.2, slope_m = 0.2, sd_y = 1, homogeneity = 0.4)

# Plot heterogeneous data
plot_data(data_hg)

# Note: `homogeneity` shrinks one sex to the left and the other to the right
# when smaller than one

## -----------------------------------------------------------------------------
# Fit a global regression
plot_data(data_hg, midline = TRUE)

## -----------------------------------------------------------------------------
# Plot the corrected data
plot_data(data_hg, midline = TRUE, correct = TRUE)

## -----------------------------------------------------------------------------
# With middle ground
plot_data(data_hg, midline = TRUE, separate = TRUE)

## -----------------------------------------------------------------------------
# Correct
plot_data(data_hg, midline = TRUE, separate = TRUE, correct = TRUE)

## -----------------------------------------------------------------------------
# Simulate problematic data
data_pb <- simulate_data(intercept_f = 0.1, intercept_m = 0.5, slope_f = 0.5, slope_m = 2, fun = function(x) exp(x / 30), homogeneity = 0.7)

# Plot
plot_data(data_pb, linear = FALSE)

## -----------------------------------------------------------------------------
# Plot with mid-line
plot_data(data_pb, linear = FALSE, midline = TRUE, separate = TRUE)

## -----------------------------------------------------------------------------
# Correct
plot_data(data_pb, linear = FALSE, midline = TRUE, separate = TRUE, correct = TRUE)

## -----------------------------------------------------------------------------
# Log-transform
plot_data(data_pb, linear = FALSE, midline = TRUE, separate = TRUE, transform = log, inverse = exp)

## -----------------------------------------------------------------------------
# Correct
plot_data(data_pb, linear = FALSE, midline = TRUE, separate = TRUE, transform = log, inverse = exp, correct = TRUE)

## -----------------------------------------------------------------------------
# New transformation
plot_data(data_pb, linear = FALSE, midline = TRUE, separate = TRUE, transform = function(x) log(x) / (1 + log(x)), inverse = function(y) exp(y / (1 - y)))

# Note: exp(y / (1 - y)) is the inverse of log(x) / (1 + log(x)).

## -----------------------------------------------------------------------------
# Correct
plot_data(data_pb, linear = FALSE, midline = TRUE, separate = TRUE, transform = function(x) log(x) / (1 + log(x)), inverse = function(y) exp(y / (1 - y)), correct = TRUE)

## -----------------------------------------------------------------------------
# Re-fit after transforming
plot_data(data_pb, linear = FALSE, midline = TRUE, separate = TRUE, transform = function(x) log(x) / (1 + log(x)), inverse = function(y) exp(y / (1 - y)), refit = TRUE)

## -----------------------------------------------------------------------------
# Linear re--fitting
plot_data(data_pb, linear = FALSE, midline = TRUE, separate = TRUE, transform = function(x) log(x) / (1 + log(x)), inverse = function(y) exp(y / (1 - y)), refit = TRUE, linear_refit = TRUE)

## -----------------------------------------------------------------------------
# Correct
plot_data(data_pb, linear = FALSE, midline = TRUE, separate = TRUE, transform = function(x) log(x) / (1 + log(x)), inverse = function(y) exp(y / (1 - y)), refit = TRUE, linear_refit = TRUE, correct = TRUE)

## -----------------------------------------------------------------------------
# Show session information
sessionInfo()

