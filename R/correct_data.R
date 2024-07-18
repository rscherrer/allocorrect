#' Correct for varying allometries among groups
#'
#' This function performs allometry correction on some data when the allometric
#' relationship may differ among groups.
#'
#' @inheritParams plot_data
#'
#' @details The function first applies the requested transformation, fits a
#'  regression model (either linear, \code{y ~ x}, or allometric, \code{log10(y)
#'  ~ log10(x)}) to come up with a mid-line (either through global regression
#'  or averaging group-specific regressions --- which is the recommended default), 
#'  and computes the corrected values of the variable of interest as deviations 
#'  from that mid-line.
#'
#' @return An updated tibble where the variable of interest has been corrected.
#'
#' @seealso \code{plot_data}
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
#' # Correct for allometry
#' correct_data(data)
#'
#' @export

# Function to actually perform the correction on a data set
correct_data <- function(
    
  data, transform = NULL, linear = TRUE, separate = TRUE
  
) {
  
  # Transform the data if needed
  if (!is.null(transform)) data$y <- transform(data$y)
  
  # Function to control the shape of the relationship
  fun <- function(x) if (linear) x else log10(x)
  
  # Prepare model formula
  formula <- "fun(y) ~ fun(x)"
  if (separate) formula <- paste(formula, "* sex")
    
  # Fit the appropriate regression (per group or global)
  fit <- stats::lm(stats::as.formula(formula), data)
  
  # Generate the mid-line
  mid_preds <- get_predictions(fit, data, linear, separate)
  
  # Correct the data
  data$y <- data$y - mid_preds
  
}