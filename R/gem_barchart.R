#' Title
#'
#'
#' @param data A tidy tibble with group and values column
#' @param groep The groups measured
#' @param kolom The measurement values
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
#' rabbit_ears <- data.frame(rabbitnr = seq(1,20),colour = c(rep("beige",10),rep("brown",10)), earlength = c(17.5,13.0,18.6,19.4,20.9,18.9,22.8,20.3,22.1,20.4,23.0,24.1,26.1,25.2,27.8,28.5,26.5,21.5,20.2,18.8))
#' gem_barchart(rabbit_ears, colour, earlength)
gem_barchart <- function(data, groep, kolom) {
  summary <- data %>% group_by({{ groep }}) %>% summarise(gem = mean({{ kolom }}), stdev = sd({{ kolom }}))
  ggplot(summary, aes(x = {{ groep }}, y = gem, fill = {{ groep }})) +
     geom_col() +
     geom_errorbar(aes(ymin = gem - stdev, ymax = gem + stdev), width = 0.2)
}
