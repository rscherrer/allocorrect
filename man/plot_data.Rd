% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_data.R
\name{plot_data}
\alias{plot_data}
\title{Plot allometric data}
\usage{
plot_data(
  data,
  lines = TRUE,
  linear = TRUE,
  midline = FALSE,
  separate = FALSE,
  correct = FALSE,
  transform = NULL,
  inverse = NULL,
  refit = FALSE,
  linear_refit = NA,
  alpha = 0.3
)
}
\arguments{
\item{data}{The data set to use, as a data frame or tibble}

\item{lines}{Whether to display regression lines for each sex}

\item{linear}{Whether the lines should be straight or allometric}

\item{midline}{Whether to draw a global tendency (mid-)line}

\item{separate}{Whether the mid-line should be an average of separate, group-specific
lines or the result of a global regression through the whole data set}

\item{correct}{Whether to apply the allometric correction}

\item{transform}{Optional transformation function used to re-scale the
vertical axis}

\item{inverse}{The inverse of the transformation function, if
\code{transform} is provided}

\item{refit}{Whether the lines must be re-fitted after transformation (if
not the original lines are simply re-scaled)}

\item{linear_refit}{Whether the re-fitted curves should be linear or allometric}

\item{alpha}{Transparency of the points}
}
\value{
A \code{ggplot} object.
}
\description{
This function produces a scatter plot of the variable of interest against a
variable to correct for, with multiple regression, transformation and
correction options that the user can use to diagnose how to correct for
allometric scaling across groups.
}
\details{
The function can produce a number of plots depending on the option
specified. It can (1) plot the raw data, (2) add group-specific
regression lines, (3) add a mid-line between groups, (4) transform the scale
of the vertical axis if needed, (5) re-fit group-specific regressions and
mid-line on transformed data and (6) correct the alleomtric data by taking
their deviation from the mid-line. Those outputs are generated in order, so
that \code{lines} and \code{midline} must be \code{TRUE}, for example, for
the code to proceed all the way to axis transformation and data correction
(otherwise the function exits before). Each regression step is either
linear (\code{y ~ x}) or allometric (\code{log10(y) ~ log10(x)}). By default
if new regression lines must be fitted after the data has
been transformed, they will be fitted with the same type of regression as
specified by \code{linear}, unless \code{linear_refit} is specified.
}
\examples{

# Simulate some data
data <- simulate_data(
  intercept_f = 0.1, 
  intercept_m = 10, 
  slope_f = 0.5, 
  slope_m = 0.5,
  n = 20L
)

# Plot them
plot_data(data)

}
\seealso{
\code{simulate_data}, \code{correct_data}
}
