#' Title
#'
#' @param data A tidy tibble with grouping and two value columns.
#' @param kolom_x The measurement values of a variable which are to correlate to...
#' @param kolom_y ...the measurement values of another variable
#' @param groep The groups into which we can divide the values
#'
#' @return A scatterplot to visualize the correlation between twe two variables, with a Pearson's correlation coefficient
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
#' cor_scatterplot(penguins, penguins$body_mass_g, penguins$flipper_length_mm, penguins$species)
cor_scatterplot <- function(data, kolom_x, kolom_y, groep, reposition_r = 10) {

cor_pvalue <- round(cor.test(kolom_x, kolom_y, method = "pearson")$p.value, 2)

cor_coefficient <- round(cor.test(kolom_x, kolom_y, method = "pearson")$estimate, 2)

ggplot(data = data, aes(x = kolom_x, y = kolom_y)) +
  geom_point(aes(color = groep), size = 1, alpha = 0.8) +
  annotate("text", x = (min(kolom_x, na.rm = TRUE) + reposition_r), y = max(kolom_y, na.rm = TRUE), size = 4, label = paste("Pearson's r = ", cor_coefficient), colour = "red")

paste("P-value of the correlation:", cor_pvalue)
}

