# settings_list <- list(
#   edish = generateSettings(chart="edish"),
#   safetyHistogram = generateSettings(chart="safetyHistogram")
# )


combineSettings <- function(settings_list){

  s <- map(settings_list, ~ tibble(setting = names(.x), value = .x)) %>% 
    reduce(full_join, by="setting") %>% 
    mutate(value = map2(value.x, value.y, ~ if (is.null(.x)) {.y} else {.x})) %>%   ## eventually we need to generalize this to more..
    select(setting, value)
  
  l <- as.list(s$value)
  names(l) <- s$setting
  
  return(l)
}

 