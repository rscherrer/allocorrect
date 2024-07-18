## Here we test the prediction function.

set.seed(55)

# Simulate some data
data <- simulate_data(
  intercept_f = 0.1,
  intercept_m = 10,
  slope_f = 0.5,
  slope_m = 0.5,
  n = 20L
)  

# Test that predictions work
test_that("Predictions work", {
  
  # Fit a regression
  fit <- lm(y ~ x, data)

  # Extract predictions
  preds <- get_predictions(fit, data, linear = TRUE)
  
  # Check that the predictions are those of a global regression
  expect_true(all(preds == predict(fit, data)))
  
  # If we pretend that the model was in fact allometric...
  preds_allo <- get_predictions(fit, data, linear = FALSE)
  
  # Then predictions should have been transformed appropriately
  expect_true(all(preds_allo == exp(log(10) * preds)))
  
})

# Test that predictions work with multiple groups
test_that("Predictions with multiple groups", {
  
  # If we now fit a model with separate regression lines...
  fit <- lm(y ~ x * sex, data)
  
  # Then the predictions...
  preds <- get_predictions(fit, data, linear = TRUE)
  
  # Should be equal to the model predictions
  expect_true(all(preds == predict(fit, data)))
  
  # But if the predictions are for a mid-line...
  preds_midline <- get_predictions(fit, data, linear = TRUE, separate = TRUE)
  
  # Create dummy data sets where we pretend that everybody is of the same sex
  data_female <- data_male <- data
  data_female$sex <- "female"
  data_male$sex <- "male"
  
  # Extract predictions for males and females
  preds_female <- predict(fit, data_female)
  preds_male <- predict(fit, data_male)
  
  # Check that the mid-line is the average of both
  expect_true(all(preds_midline == (preds_male + preds_female) / 2))
  
})
