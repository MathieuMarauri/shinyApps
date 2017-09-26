
# how to rnder a table from file input with a sweetalert

# Packages ----------------------------------------------------------------

library('shiny')
library('shinydashboard')
# library('data.table')
library('DT')


# App ---------------------------------------------------------------------

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    fileInput(
      inputId = 'file',
      label = 'Select a file',
      multiple = FALSE,
      accept = c('.csv', '.txt')
    )
  ),
  dashboardBody(
    textOutput(outputId = 'text'),
    dataTableOutput(outputId = 'table')
  )
)

server <- function(input, output, session){
  
  raw_data <- reactive({
    req(input$file)
    data.table::fread(file = input$file$datapath)
  })
  
  output$table <- renderDataTable({
    datatable(raw_data(),
              options = list(
                scrollX = TRUE,
                scrollY = TRUE,
                ordering = TRUE,
                pageLength = 10,
                dom = 'tp',
                autoWidth = TRUE,
                columnDefs = list(
                  list(
                    width = '70px',
                    targets = 0
                  )
                )
              ),
              rownames = FALSE) %>%
      formatStyle(
        columns = 2,
                  background = styleColorBar(data = as.numeric(range(raw_data()[, 2])), 'lightblue'),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center'
        ) %>% 
      formatStyle(
        columns = 'categories',
        valueColumns = 'pass',
        color = styleEqual(c(0, 1), c('red', 'green'))
      )
  }, 
  server = TRUE)
  
  session$onSessionEnded(stopApp)
}

shinyApp(ui = ui, server = server)