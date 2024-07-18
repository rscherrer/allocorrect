#' Plot allometric data
#'
#' This function produces a scatter plot of the variable of interest against a
#' variable to correct for, with multiple regression, transformation and 
#' correction options that the user can use to diagnose how to correct for 
#' allometric scaling across groups.
#'
#' @param data The data set to use, as a data frame or tibble
#' @param lines Whether to display regression lines for each sex
#' @param linear Whether the lines should be straight or allometric
#' @param midline Whether to draw a global tendency (mid-)line
#' @param separate Whether the mid-line should be an average of separate, group-specific
#'  lines or the result of a global regression through the whole data set
#' @param correct Whether to apply the allometric correction
#' @param transform Optional transformation function used to re-scale the 
#'  vertical axis
#' @param inverse The inverse of the transformation function, if 
#'  \code{transform} is provided
#' @param refit Whether the lines must be re-fitted after transformation (if
#'  not the original lines are simply re-scaled)
#' @param linear_refit Whether the re-fitted curves should be linear or allometric
#' @param alpha Transparency of the points
#'
#' @details The function can produce a number of plots depending on the option
#'  specified. It can (1) plot the raw data, (2) add group-specific
#'  regression lines, (3) add a mid-line between groups, (4) transform the scale
#'  of the vertical axis if needed, (5) re-fit group-specific regressions and
#'  mid-line on transformed data and (6) correct the alleomtric data by taking
#'  their deviation from the mid-line. Those outputs are generated in order, so
#'  that \code{lines} and \code{midline} must be \code{TRUE}, for example, for 
#'  the code to proceed all the way to axis transformation and data correction
#'  (otherwise the function exits before). Each regression step is either
#'  linear (\code{y ~ x}) or allometric (\code{log10(y) ~ log10(x)}). By default 
#'  if new regression lines must be fitted after the data has
#'  been transformed, they will be fitted with the same type of regression as
#'  specified by \code{linear}, unless \code{linear_refit} is specified.
#'
#' @return A `ggplot` object.
#'
#' @seealso \code{simulate_data}, \code{correct_data}
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
#' # Plot them
#' plot_data(data)
#' 
#' @importFrom rlang .data 
#'
#' @export

# Function to plot the data
plot_data <- function(
    
  data, lines = TRUE, linear = TRUE, midline = FALSE, separate = FALSE,
  correct = FALSE, transform = NULL, inverse = NULL, refit = FALSE, 
  linear_refit = NA, alpha = 0.3
  
) {
  
  # Plot
  plot <- ggplot2::ggplot(data, ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::geom_point(ggplot2::aes(color = .data$sex), alpha = alpha) +
    ggplot2::xlab("Variable x") +
    ggplot2::ylab("Variable y") +
    ggplot2::labs(color = NULL)
  
  # Early exit if needed
  if (!lines) return(plot)
  
  # Function to control the shape of the relationship
  fun <- function(x) if (linear) x else log10(x)
  
  # Find the right relationship for each sex
  fit <- stats::lm(fun(y) ~ fun(x) * sex, data)
  
  # Add expected values to the data
  plot$data$y_pred <- get_predictions(fit, data, linear)
  
  # Add the lines to the plot 
  plot <- plot + 
    ggplot2::geom_line(ggplot2::aes(y = .data$y_pred, color = .data$sex))
  
  # Exit here if needed
  if (!midline) return(plot)
  
  # Find the mid-line
  mid_fit <- stats::lm(fun(y) ~ fun(x), data)
  
  # Pick the relevant model to extract predictions from 
  this_fit <- if (separate) fit else mid_fit
  
  # Add predictions to the data
  plot$data$y_mid <- get_predictions(this_fit, data, linear, separate)
  
  # Re-scale the axis if needed
  if (!is.null(transform) & !correct) plot <- plot + 
    ggplot2::scale_y_continuous(
      transform = scales::new_transform("trans", transform, inverse)
    )
  
  # Save it for later
  plot0 <- plot
  
  # Plot the mid-line
  plot <- plot + 
    ggplot2::geom_line(ggplot2::aes(y = .data$y_mid), linetype = 2L)
  
  # Exit if needed
  if (!correct & !refit) return(plot)
  
  # If we must re-fit the model...
  if (refit) {
    
    # Same regression as the original one by default
    if (is.na(linear_refit)) linear_refit <- linear
    
    # Pick a type of regression for the re-fitted model
    new_fun <- function(x) if (linear_refit) x else log10(x)
    
    # Fit new models for expectations
    new_fit <- stats::lm(new_fun(transform(y)) ~ new_fun(x) * sex, plot$data)
    new_mid_fit <- stats::lm(new_fun(transform(y)) ~ new_fun(x), plot$data)
    
    # Pick the relevant model to extract predictions from 
    this_new_fit <- if (separate) new_fit else new_mid_fit
    
    # Update predictions
    plot$data$y_pred <- inverse(get_predictions(new_fit, plot$data, linear_refit))
    plot$data$y_mid <- inverse(get_predictions(this_new_fit, plot$data, linear_refit, separate))
    
  }
  
  # Exit if needed
  if (!correct) return(plot)
  
  # Import the data we have transformed
  plot0$data <- plot$data
  
  # Go back to the old plot without a mid-line
  plot <- plot0
  
  # If needed...
  if (!is.null(transform)) {
    
    # Transform the actual data 
    plot$data$y <- transform(plot$data$y)
    plot$data$y_pred <- transform(plot$data$y_pred)
    plot$data$y_mid <- transform(plot$data$y_mid)
    
  }
    
  # Correct the data
  plot$data$y <- with(plot$data, y - y_mid)
  plot$data$y_pred <- with(plot$data, y_pred - y_mid)
  
  # The mid-line is now the horizontal axis
  plot + 
    ggplot2::geom_hline(yintercept = 0, linetype = 2L) + 
    ggplot2::ylab("Corrected y")
  
}