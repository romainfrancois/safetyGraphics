renderChart <- function(input, output, session, data, settings, status){
  
  ns <- session$ns
  
  # module to render eDish chart
  callModule(renderEDishChart, "chartEDish",
             data = reactive(data()),
             settings = reactive(settings()$eDish),
             valid = reactive(status()$eDish$valid))
  
  # module to render safety Histogram chart
  callModule(renderSafetyHistogramChart, "chartSafetyHistogram",
             data = reactive(data()),
             settings = reactive(settings()$safetyHistogram),
             valid = reactive(status()$safetyHistogram$valid))

}