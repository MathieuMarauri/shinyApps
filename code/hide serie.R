
#' How to show/hide a serie in a highchart chart with a button

# Version 1 ---------------------------------------------------------------

# using visible argument in hc_add_serie

# necessary packages
library('shiny')
library('shinydashboard')
library('highcharter')

# ui
ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody(
    shinyWidgets::materialSwitch(
      inputId = "button",
      label = "Button",
      value = FALSE
    ),
    highchartOutput(outputId = "plot")
  )
)

# server
server <- function(input, output, session){
  
  output$plot <- renderHighchart({
    data_plot <- data.frame(categories = c("A", "B", "C", "D"),
                            serie1 = c(1563, 1458, 205, 695),
                            serie2 = c(562, 258, 17, 115))
    highchart() %>%
      hc_chart(
        type = 'bar'
      ) %>%
      hc_add_series(
        data = data_plot$serie1,
        name = 'Serie to hide/show',
        visible = input$button
      ) %>%
      hc_add_series(
        data = data_plot$serie2,
        name = 'Serie 2'
      ) %>%
      hc_xAxis(
        categories = data_plot$categories,
        title = list(text = 'Categories')
      ) %>%
      hc_plotOptions(bar = list(stacking = 'normal'))
  })
  
  session$onSessionEnded(stopApp)
}

shinyApp(ui = ui, server = server)


# Version 2 ---------------------------------------------------------------

# using javascript when the click is triggered

# necessary packages
library('shiny')
library('shinydashboard')
library('highcharter')
library('shinyjs')

jsCode <- "
shinyjs.toggleSerie = function(params) {
var serieToToggle = $('#plot').highcharts().get('idserie');
if(serieToToggle.visible){
serieToToggle.setVisible(false);
} 
else {
serieToToggle.setVisible(true);
}
}
"

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody(
    useShinyjs(),
    extendShinyjs(text = jsCode),
    shinyWidgets::materialSwitch(
      inputId = "button",
      label = "Button",
      value = FALSE
    ),
    highchartOutput(outputId = "plot")
  )
)

server <- function(input, output, session){
  
  output$plot <- renderHighchart({
    data_plot <- data.frame(categories = c("A", "B", "C", "D"),
                            serie1 = c(1563, 1458, 205, 695),
                            serie2 = c(562, 258, 17, 115))
    highchart() %>%
      hc_chart(
        type = 'bar'
      ) %>%
      hc_add_series(
        data = data_plot$serie1,
        name = 'Serie to hide/show',
        id = 'idserie'
      ) %>%
      hc_add_series(
        data = data_plot$serie2,
        name = 'Serie 2'
      ) %>%
      hc_xAxis(
        categories = data_plot$categories,
        title = list(text = 'Categories')
      ) %>%
      hc_plotOptions(bar = list(stacking = 'normal'))
  })
  
  onclick(id = "button", expr = {
    js$toggleSerie()
  })
  
  session$onSessionEnded(stopApp)
}

shinyApp(ui = ui, server = server)
