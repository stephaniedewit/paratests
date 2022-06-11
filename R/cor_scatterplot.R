#' Scatterplot for correlation test results
#'
#' @param data A tidy tibble with grouping and two value columns
#' @param column_x The measurement values of a variable, plotted on the x axis, which are to correlate to...
#' @param column_y ...the measurement values of another variable, plotted on the y axis
#' @param group Alternatively: The groups into which the values are divided
#' @param reposition_r Numerical argument to adjust the "Pearson's r = #" label position on the x axis
#'
#' @return A scatterplot to visualize the correlation between two variables, with a Pearson's correlation coefficient
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 annotate
#' @importFrom stats cor.test
#' @importFrom dplyr summarise
#' @importFrom dplyr group_by
#' @export
#'
#' @examples
#' library("palmerpenguins")
#' library("ggplot2")
#' cor_scatterplot(penguins, penguins$body_mass_g, penguins$flipper_length_mm, penguins$species, reposition_r = 1000)
cor_scatterplot <- function(data, column_x, column_y, group = NULL, reposition_r = 10) {

cor_pvalue <- round(cor.test(column_x, column_y, method = "pearson")$p.value, 2)
cor_coefficient <- round(cor.test(column_x, column_y, method = "pearson")$estimate, 2)

ggplot(data = data, aes(x = column_x, y = column_y)) +
  geom_point(aes(color = group), size = 1, alpha = 0.8) +
  annotate("text", x = (min(column_x, na.rm = TRUE) + reposition_r), y = max(column_y, na.rm = TRUE), size = 4, label = paste0("Pearson's r: ", cor_coefficient, ", P-value of the correlation: ", cor_pvalue), colour = "red") +
  theme_bw()
}

