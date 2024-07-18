#' Simulate allometric data
#'
#' This function can be used to simulate a dummy data set with sexually dimorphic
#' allometry, on which to test the correction tools of the package.
#' 
#' @param intercept_f,intercept_m,slope_f,slope_m Linear model parameters for both sexes
#' @param n Number of data points per sex
#' @param mean_x Mean value along the horizontal axis
#' @param sd_x Standard deviation along the horizontal axis
#' @param sd_y Noise to add to the (normally distributed) data
#' @param fun Optional function to convert the relationship into a non-linear allometry
#' @param homogeneity Degree of range cover of both groups along the horizontal axis (0 to 1). 
#'  A value of 1 means that the full range will be covered by both sexes on the
#'  horizontal axis, and the closer to 0, the more each sex will be restricted
#'  to one side of the range (by default, females to the left and males to the 
#'  right).
#'
#' @details The function simulates a variable \code{x} that depends on some
#' underlying variable \code{y} according to some linear relationship that can differ
#' between males and females, and then transforms those data so the relationship
#' is no longer linear (if requested). Noise is added to the simulated data before 
#' transformation.
#'
#' @return A tibble containing the columns \code{x}, \code{y} and \code{sex}.
#'
#' @seealso \code{plot_data}, \code{correct_data}
#'
#' @examples
#'
#' # Simulate some data
#' simulate_data(
#'   intercept_f = 0.1, 
#'   intercept_m = 10, 
#'   slope_f = 0.5, 
#'   slope_m = 0.5,
#'   n = 20L
#' )
#'
#' @export

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
  
  # Sample values along the horizontal axis from a normal distribution
  xvalues <- stats::rnorm(n, mean_x, sd_x)
  
  # What is the range?
  xmin <- min(xvalues)
  xmax <- max(xvalues)
  
  # Restrict the range if needed
  x_females <- xmin + homogeneity * (xvalues - xmin)
  x_males <- xmax + homogeneity * (xvalues - xmax)
  
  # Simulate the vertical axis
  y_females <- x_females * slope_f + intercept_f + stats::rnorm(n, 0, sd_y)
  y_males = x_males * slope_m + intercept_m + stats::rnorm(n, 0, sd_y)
  
  # Make a table
  data <- data.frame(
    
    # ... a table
    x = c(x_females, x_males),
    y = c(y_females, y_males),
    sex = rep(c("female", "male"), each = n)
    
  )
  
  # Transform if needed
  if (!is.null(fun)) data$y <- fun(data$y)
  
  return(data)
  
}