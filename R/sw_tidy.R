#' Title
#'
#' @param data A tidy tibble with group and values column
#' @param groep The column with the measured groups
#' @param waarde The column with measurement values
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
sw_tidy <- function(data, groep, waarde) {
  data %>% group_by({{ groep }}) %>% summarise(p.value.sw = shapiro.test({{ waarde }})$p.value)
}
