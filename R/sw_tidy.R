sw_tidy <- function(tidy_tibble, group, column) {
  tidy_tibble %>% group_by(group) %>% summarise(p.value.sw = shapiro.test(column))$p.value
}
