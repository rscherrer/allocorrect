# Function to simulate sexually dimorphic allometric data
simulate_data <- function(
  
  intercept_f,
  intercept_m,
  slope_f,
  slope_m,
  n = 1000L,
  mean_x = 25,
  sd_x = 5,
  sd_y = 5,
  fun = function(x) x,
  homogeneity = 1
  
) {
  
  # intercept_f, intercept_m, slope_f, slope_m: parameters
  # n: number of data points per group
  # mean_x: mean value along the horizontal axis
  # sd_x: standard deviation along the horizontal axis
  # sd_y: noise to add to the simulated variable (normally distributed)
  # fun: optional function to convert the relationship into non-linear allometry
  # homogeneity: degree of range cover of both groups along the horizontal axis (0 to 1)
  
  # Note: homogeneity below one shrinks one group towards the minimum of the range
  # and the other towards the maximum.
  
  # Sample values along the horizontal axis from a normal distribution
  xvalues <- rnorm(n, mean_x, sd_x)
  
  # What is the range?
  xmin <- min(xvalues)
  xmax <- max(xvalues)
  
  # Make a table
  tibble(female = xvalues, male = xvalues) %>%
    
    # Add heterogeneity if needed...
    mutate(
      
      # ... by shrinking one group to the left and the other to the right
      female = xmin + homogeneity * (female - xmin),
      male = xmax + homogeneity * (male - xmax)
      
    ) %>%
    pivot_longer(female:male, names_to = "sex", values_to = "x") %>%
    
    # For both sexes...
    mutate(
      
      # ... simulate the vertical axis with two different linear models
      y_female = x * slope_f + intercept_f,
      y_male = x * slope_m + intercept_m,
      
      # Pick the correct one
      y = if_else(sex == "female", y_female, y_male)
      
    ) %>%
    select(-y_female, -y_male) %>%
    
    # Add noise
    mutate(y = rnorm(y, y, sd_y)) %>%
    
    # Non-linear transformation if needed
    mutate(y = fun(y))
    
}

# Function to extract predictions
get_predictions <- function(fit, data, linear, separate = FALSE) {
  
  # fit: the fitted model
  # data: a dataset to make predictions from
  # linear: whether the model is linear (otherwise allometric)
  # separate: whether to compute the average of the sex-specific predictions
  
  # Extract expected values from the fitted model
  preds <- predict(fit, data)
  
  # If needed...
  if (separate) {
    
    # Extract the lines for both sexes  
    preds_female <- predict(fit, data %>% mutate(sex = "female"))
    preds_male <- predict(fit, data %>% mutate(sex = "male"))
    
    # Compute the middle ground
    preds <- (preds_male + preds_female) / 2
    
  }
  
  # Back-transform if needed
  if (!linear) preds <- exp(log(10) * preds)
  
  return(preds)
  
}

# Function to plot the data
plot_data <- function(
    
  data, lines = TRUE, linear = TRUE, midline = FALSE, separate = FALSE,
  correct = FALSE, transform = NULL, inverse = NULL, refit = FALSE, 
  linear_refit = NA, alpha = 0.3
  
) {
  
  # data: the data as produced by simulate_data()
  # lines: whether to display allometric lines for each sex
  # linear: whether the lines should be straight or allometric
  # midline: whether to draw a global regression line
  # separate: whether the mid-line should be an aggregate of both sexes (otherwise a global regression)
  # correct: whether to apply the correction
  # transform: transformation function to re-scale the vertical axis
  # inverse: the inverse of the transformation function
  # refit: whether the lines must be re-drawn after transformation
  # linear_refit: whether the re-fitted model should be linear or allometric
  # alpha: transparency of the points
  
  # Note: by default the re-fitted model will be of the same type (linear or
  # allometric) as the first one.
  
  # Plot
  plot <- data %>%
    ggplot(aes(x = x, y = y)) +
    geom_point(aes(color = sex), alpha = alpha) +
    xlab("Variable x") +
    ylab("Variable y") +
    labs(color = NULL)
  
  # Early exit if needed
  if (!lines) return(plot)
  
  # Function to control the shape of the relationship
  fun <- function(x) if (linear) x else log10(x)
  
  # Find the right relationship for each sex
  fit <- lm(fun(y) ~ fun(x) * sex, data)
  
  # Add expected values to the data
  plot$data <- plot$data %>% mutate(y_pred = get_predictions(fit, data, linear))
  
  # Add the lines to the plot 
  plot <- plot + geom_line(aes(y = y_pred, color = sex))
  
  # Exit here if needed
  if (!midline) return(plot)
  
  # Find the mid-line
  mid_fit <- lm(fun(y) ~ fun(x), data)
  
  # Pick the relevant model to extract predictions from 
  this_fit <- if (separate) fit else mid_fit
  
  # Add predictions to the data
  plot$data <- plot$data %>% 
    mutate(y_mid = get_predictions(this_fit, data, linear, separate))
  
  # Re-scale the axis if needed
  if (!is.null(transform) & !correct) plot <- plot + 
    scale_y_continuous(transform = scales::new_transform("trans", transform, inverse))
  
  # Save it for later
  plot0 <- plot
  
  # Plot the mid-line
  plot <- plot + geom_line(aes(y = y_mid), linetype = 2L)
  
  # Exit if needed
  if (!correct & !refit) return(plot)
  
  # If we must re-fit the model...
  if (refit) {
    
    # Same regression as the original one by default
    if (is.na(linear_refit)) linear_refit <- linear
    
    # Pick a type of regression for the re-fitted model
    new_fun <- function(x) if (linear_refit) x else log10(x)
    
    # Fit new models for expectations
    new_fit <- lm(new_fun(transform(y)) ~ new_fun(x) * sex, plot$data)
    new_mid_fit <- lm(new_fun(transform(y)) ~ new_fun(x), plot$data)
    
    # Pick the relevant model to extract predictions from 
    this_new_fit <- if (separate) new_fit else new_mid_fit
    
    # Update predictions
    plot$data <- plot$data %>% 
      mutate(
        y_pred = inverse(get_predictions(new_fit, plot$data, linear_refit)),
        y_mid = inverse(get_predictions(this_new_fit, plot$data, linear_refit, separate))
      )
    
  }
  
  # Exit if needed
  if (!correct) return(plot)
  
  # Import the data we have transformed
  plot0$data <- plot$data
  
  # Go back to the old plot without a mid-line
  plot <- plot0
  
  # Transform the actual data if needed
  if (!is.null(transform)) plot$data <- plot$data %>%
    mutate(across(c(y, y_pred, y_mid), transform))
  
  # Correct the data
  plot$data <- plot$data %>% mutate(across(c(y, y_pred), \(y) y - y_mid))
  
  # The mid-line is now the horizontal axis
  plot + geom_hline(yintercept = 0, linetype = 2L) + ylab("Corrected y")
  
}