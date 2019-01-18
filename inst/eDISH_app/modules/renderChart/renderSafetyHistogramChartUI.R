renderSafetyHistogramChartUI <- function(id){
  
  ns <- NS(id)
  
  tagList(
     safetyHistogramOutput(ns("chart")) 
  )
}