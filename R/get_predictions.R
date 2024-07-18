#' Extract predictions from a fitted model
#'
#' This function extracts predicted values for a given data set from a fitted
#' model, either fitted separately through multiple groups (i.e. with an
#' interaction term) or a global regression through the whole data set. 
#'
#' @param fit The fitted model (by \code{lm()})
#' @param data The data frame to \code{predict} values for
#' @param linear Whether the model is linear (if not, an allometric relationship
#'  is assumed and the inverse of \code{log10} will be applied to the predictions)
#' @param separate Whether to compute the average of separate group-specific 
#'  predictions or not (i.e. global regression)
#'
#' @details This function is basically a wrapper around \code{predict} and can
#'  work on a model, fitted with \code{lm} and regressing a certain variable
#'  against another, either linearly (\code{y ~ x}) or allometrically (
#'  \code{log10(y) ~ log10(x)}), and on a group-specific basis or not (i.e.
#'  with one regression line per group if \code{separate} is \code{TRUE}).
#'  See \code{?predict} for more details.
#'
#' @return A vector of predicted values for the variable that the fitted model
#'  \code{fit} is supposed to model.
#'
#' @seealso \code{correct_data}, \code{plot_data}
#'
#' @examples
#'
#' # Simulate some data
#' data <- simulate_data(
#'   intercept_f = 0.1, 
#'   intercept_m = 10, 
#'   slope_f = 0.5, 
#'   slope_m = 0.5,
#'   n = 20L
#' )
#'
#' # Fit a linear model
#' fit <- lm(y ~ x * sex, data)
#' 
#' # Extract predictions
#' get_predictions(fit, data, linear = TRUE, separate = TRUE)
#'
#' @export

# Function to extract predictions
get_predictions <- function(fit, data, linear, separate = FALSE) {
  
  # Extract expected values from the fitted model
  preds <- if (!separate) stats::predict(fit, data) else {
    
    # Pretend that all data points are of the same sex
    data_male <- data_female <- data
    data_female$sex <- "female"
    data_male$sex <- "male"
    
    # Extract the lines for both sexes  
    preds_female <- stats::predict(fit, data_female)
    preds_male <- stats::predict(fit, data_male)
    
    # Compute the middle ground
    preds <- (preds_male + preds_female) / 2
    
  }
  
  # Back-transform if needed
  if (!linear) preds <- exp(log(10) * preds)
  
  return(preds)
  
}
