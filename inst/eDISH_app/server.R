function(input, output, session){


  # run dataUpload module
  #
  #  returns selected dataset, settings, and validation status
  dataUpload_out <- callModule(dataUpload, "datatab")

  # add status to data panel nav bar
  #   always OK for now, since example data is loaded by default
  output$data_tab_title = renderUI({
    HTML(paste("Data", icon("check", class="ok")))
  })
  
  # based on selected data set & generated/selected settings obj, generate settings page.
  #
  #  NOTE:  module is being triggered when selected dataset changes OR when settings list changes
  #   this could cause the module to trigger twice unecessarily in some cases because the settings are generated
  #   AFTER the data is changed.
  #
  # reutrns updated settings and validation status
    settings_new <-   callModule(renderSettings, "settingsUI",
                                 data = isolate(reactive(dataUpload_out$data_selected())),
                                 settings = reactive(dataUpload_out$settings()),
                                 status = reactive(dataUpload_out$status()))


    
    num_fail <- reactive({
      fail <- settings_new$status() %>% flatten %>% keep(., names(.)=="valid") %>% unlist %>% {sum(.==FALSE)} 
      return(fail)
    })
    
  # update settings navbar
    output$settings_tab_title = renderUI({
      
      if (num_fail()==0){
        HTML(paste("Settings", icon("check", class="ok")))
      } else {
        HTML(paste("Settings", icon("times", class="notok")))
      }
    })
    
    # update charts navbar
    output$chart_tab_title = renderUI({
    #  if (settings_new$status()$valid==TRUE){
      if (num_fail()==0){
        HTML(paste("Charts", icon("check", class="ok")))
      } else {
        HTML(paste("Charts", icon("times", class="notok")))
      }
    })
    
    # update eDISH navbar title
    output$chart_tab_title = renderUI({
      #  if (settings_new$status()$valid==TRUE){
      if (num_fail()==0){
        HTML(paste("Charts", icon("check", class="ok")))
      } else {
        HTML(paste("Charts", icon("times", class="notok")))
      }
    })    


  callModule(renderChart, "charts", 
             data = reactive(dataUpload_out$data_selected()),
             settings = reactive(settings_new$settings()),
             status = reactive(settings_new$status()))

  # # module to render eDish chart
  # callModule(renderEDishChart, "chartEDish",
  #            data = reactive(dataUpload_out$data_selected()),
  #            settings = reactive(settings_new$settings()$eDish),
  #            valid = reactive(settings_new$status()$eDish$valid))
  # 
  # # module to render safety Histogram chart
  # callModule(renderSafetyHistogramChart, "chartSafetyHistogram",
  #            data = reactive(dataUpload_out$data_selected()),
  #            settings = reactive(settings_new$settings()$safetyHistogram),
  #            valid = reactive(settings_new$status()$safetyHistogram$valid))
  

  
  session$onSessionEnded(stopApp)

}
