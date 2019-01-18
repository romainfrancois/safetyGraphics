getChartSettings <- function(settings_list, chart){
  settings_names <- names(generateSettings("None",chart=chart))
  return(settings_list[settings_names])
}
