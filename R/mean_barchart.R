#' Barchart for means with errorbars
#'
#' @param data A tidy tibble with group and values column
#' @param group The column with the measured groups
#' @param value The column with measurement values
#'
#' @return A barchart with for each param groep the mean param kolom with error bars of standard deviation 1.
#' @importFrom magrittr %>%
#' @importFrom stats sd
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_col
#' @importFrom ggplot2 geom_errorbar
#' @importFrom ggplot2 aes
#' @importFrom dplyr summarise
#' @importFrom dplyr group_by
#' @export
#'
#' @examples
#' library("ggplot2")
#' rabbit_ears <- data.frame(rabbitnr = seq(1,20),colour = c(rep("beige",10),rep("brown",10)), earlength = c(17.5,13.0,18.6,19.4,20.9,18.9,22.8,20.3,22.1,20.4,23.0,24.1,26.1,25.2,27.8,28.5,26.5,21.5,20.2,18.8))
#' mean_barchart(rabbit_ears, colour, earlength)
mean_barchart <- function(data, group, value) {

  summary <- data %>% group_by({{ group }}) %>% summarise(mean = mean({{ value }}), stdev = sd({{ value }}))

  ggplot(summary, aes(x = {{ group }}, y = mean, fill = {{ group }})) +
    geom_col() +
    geom_errorbar(aes(ymin = mean - stdev, ymax = mean + stdev), width = 0.2) +
    theme_bw()
}
