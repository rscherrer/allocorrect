## Here we test the allometric correction function.

# Create some dummy data
data <- data.frame(
  
  x = rep(1:10, each = 2L),
  y = 1:10 * 1:2,
  sex = rep(c("female", "male"), 10L)
  
)

# Function to get the mid-line
get_midline <- function(data, fit) {
  
  # data: the data set
  # fit: the fitted model
  
  # Pretend that everybody is of the same sex for a minute
  data_female <- data_male <- data
  data_female$sex <- "female"
  data_male$sex <- "male"
  
  # Extract predictions for each sex
  preds_female <- predict(fit, data_female)
  preds_male <- predict(fit, data_male)
  
  # Compute the mid-line
  return((preds_male + preds_female) / 2)
  
}

# Test that allometric correction works with straight lines
test_that("Correction w. r. t. mid-line works", {
  
  # Correct the data
  new_data <- correct_data(data)
  
  # Fit a separate model in both sexes
  fit <- lm(y ~ x * sex, data)
  
  # Compute the mid-line
  preds_midline <- get_midline(data, fit)
  
  # Check that corrected values are deviations from the mid-line
  expect_true(all(data$y - preds_midline == new_data$y))
  
})

# Test that allometric correction works
test_that("Allometric correction works", {
  
  # Correct the data
  new_data <- correct_data(data, linear = FALSE)
  
  # Fit a separate model in both sexes
  fit <- lm(log10(y) ~ log10(x) * sex, data)
  
  # Get the mid-line
  preds_midline <- get_midline(data, fit)
  
  # Make it allometric
  preds_midline <- exp(log(10) * preds_midline)
  
  # Check that corrected values are deviations from the mid-line
  expect_true(all(data$y - preds_midline == new_data$y))
  
})

# Global regression case
test_that("Correction w. r. t. global regression works", {
  
  # Correct the data
  new_data <- correct_data(data, separate = FALSE)
  
  # Fit a global regression
  fit <- lm(y ~ x, data)
  
  # Compute the mid-line
  preds_midline <- predict(fit, data)
  
  # Check that corrected values are deviations from the mid-line
  expect_true(all(data$y - preds_midline == new_data$y))
  
})

# Transformed data
test_that("Correction works on transformed data", {
  
  # Correct the data with dummy transformation (halving here)
  new_data <- correct_data(data, transform = function(x) x / 2)
  
  # Fit a separate model in both sexes
  fit <- lm(y ~ x * sex, data)
  
  # Compute the mid-line
  preds_midline <- get_midline(data, fit)
  
  # Check that corrected values are half of the deviations from the mid-line
  expect_true(all(data$y - preds_midline == 2 * new_data$y))
  
})
