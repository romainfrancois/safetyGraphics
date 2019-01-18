renderSafetyHistogramChart <- function(input, output, session, data, settings, valid){
  
  ns <- session$ns
  
  
  # render eDISH chart if settings pass validation
  output$chart <- renderSafetyHistogram({
    req(data())
    req(settings())
    
    if (valid()==TRUE){
      safetyHistogram(data = data(), settings = settings())
    } else{
      return()
    }
  }) 
  
}