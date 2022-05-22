#' Title
#'
#' @param waarde The column with measurement values inside a tidy tible
#' @param groep The column with the measured groups inside a tidy tibble
#'
#' @return A P-value
#' @importFrom stats aov
#' @importFrom stats summary.aov
#' @importFrom readr read_delim
#' @export
#'
#' @examples
#' group1 <- "group1"
#' group2 <- "group2"
#' group3 <- "group3"
#' group4 <- "group4"
#' potato <- read_delim(here::here("data_raw", "02_potato", "potato"),delim=":",locale=locale(decimal_mark=","),col_types=cols(group1=col_number(),group2=col_number(),group3=col_number(),group4=col_number()))
#' potato_tidy <- pivot_longer(data = read, cols = c(group1, group2, group3, group4),names_to="Groep",values_to="Natrium")
#' anova(tidy$Natrium, tidy$Groep)
anova <- function(waarde, groep) {
aov <- summary.aov(aov(formula = waarde ~ groep))
aov[[1]]$'Pr(>F)'[1]
}
