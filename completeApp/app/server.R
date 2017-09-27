
server <- function(input, output, session){
  
  # Tab one ---
  
  # Inputs
  output$daterange_period1_ui <- renderUI({
    dateRangeInput(
      inputId = 'daterange_period1',
      label = 'Select a period',
      start = as.Date('2017-08-01'),
      end = as.Date('2017-09-30'),
      min = as.Date('2017-01-01'),
      max = as.Date('2017-12-31'),
      weekstart = 1,
      language = "en",
      separator = "to",
      format = "mm-dd-yyyy"
    )
  })
  
  output$daterange_period2_ui <- renderUI({
    dateRangeInput(
      inputId = 'daterange_period2',
      label = 'Select a period',
      start = as.Date('2017-07-01'),
      end = as.Date('2017-08-01'),
      min = as.Date('2017-01-01'),
      max = as.Date('2017-12-31'),
      weekstart = 1,
      language = "en",
      separator = "to",
      format = "mm-dd-yyyy"
    )
  })
  
  callModule(
    module = valueBoxes, 
    id = 'valuebox_ui', 
    daterange_period1 = reactive(input$daterange_period2), 
    period2 = reactive(input$period2_checkbox), 
    daterange_period2 = reactive(input$daterange_period2)
  )
  
  callModule(
    module = drilldownchart,
    id = 'drilldownchart_ui',
    daterange_period1 = reactive(input$daterange_period1),
    daterange_period2 = reactive(input$daterange_period2)
  )
  
  session$onSessionEnded(stopApp)
}





