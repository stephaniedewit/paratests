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
#' potato <- data.frame(Groep = c(rep("group1", 11), rep("group2", 11), rep("group3", 11), rep("group4", 11)), Natrium = c(0.312, 0.392, 0.413, 0.419, 0.320, 0.430, 0.379, 0.305, 0.479, 0.382, 0.425, 0.416, 0.339, 0.320, 0.302, 0.302, 0.463, 0.483, 0.460, 0.350, 0.352, 0.488, 0.434, 0.381, 0.374, 0.414, 0.331, 0.466, 0.420, 0.304, 0.358, 0.336, 0.383, 0.474, 0.415, 0.440, 0.330, 0.331, 0.356, 0.493, 0.463, 0.308, 0.309, 0.334))
#' anova(potato$Natrium, potato$Groep)
anova <- function(waarde, groep) {
aov <- summary.aov(aov(formula = waarde ~ groep))
aov[[1]]$'Pr(>F)'[1]
}
