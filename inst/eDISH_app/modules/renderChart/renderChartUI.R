renderChartUI <- function(id){
  
  ns <- NS(id)
  
  list(
    tabPanel(title = "eDISH", 
            # title = htmlOutput("eDISH_title", inline=TRUE),
             renderEDishChartUI(ns("chartEDish"))),
    tabPanel("safetyHistogram",
             renderSafetyHistogramChartUI(ns("chartSafetyHistogram")))
  )
}