#' Shapiro Wilk normality test for tidy data
#'
#' @param data A tidy tibble with group and values column
#' @param group The column with the measured groups
#' @param value The column with measurement values
#'
#' @return A P-value
#' @importFrom stats shapiro.test
#' @importFrom magrittr %>%
#' @importFrom dplyr summarise
#' @importFrom dplyr group_by
#' @export
#'
#' @examples
#' rabbit_ears <- data.frame(rabbitnr = seq(1,20),colour = c(rep("beige",10),rep("brown",10)), earlength = c(17.5,13.0,18.6,19.4,20.9,18.9,22.8,20.3,22.1,20.4,23.0,24.1,26.1,25.2,27.8,28.5,26.5,21.5,20.2,18.8))
#' sw_tidy(rabbit_ears, colour, earlength)
sw_tidy <- function(data, group, value) {
  data %>% group_by({{ group }}) %>% summarise(p.value.sw = shapiro.test({{ value }})$p.value)
}
