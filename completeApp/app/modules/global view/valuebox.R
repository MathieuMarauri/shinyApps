
# UI ----------------------------------------------------------------------

valueBoxesUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = 'col-md-3',
      valueBoxOutput(outputId = ns("valuebox_phone"), width = 12)
    ),
    div(
      class = 'col-md-3',
      valueBoxOutput(outputId = ns("valuebox_client"), width = 12)
    ),
    div(
      class = 'col-md-3',
      valueBoxOutput(outputId = ns("valuebox_delay"), width = 12)
    ),
    div(
      class = 'col-md-3',
      valueBoxOutput(outputId = ns("valuebox_events"), width = 12)
    )
  )
}


# Server ------------------------------------------------------------------

valueBoxes <- function(input, output, session, daterange_period1, period2, daterange_period2) {
  
  browser <- reactive({
    set.seed(123456789)
    data.table(date = seq(from = as.Date('2017-01-01'), to = as.Date('2017-12-31'), by = 1),
               phone_call = floor(runif(min = 0, max = 50, n = 365)),
               client = floor(runif(min = 0, max = 10, n = 365)),
               mean_call_duration = floor(rnorm(mean = 2.5, sd = 2, n = 365)),
               events = floor(runif(min = 0, max = 10, n = 365)))
  })
  
  browser_period1 <- reactive(
    browser()[date >= daterange_period1()[1] & date <= daterange_period1()[2]]
  )
  
  browser_period2 <- reactive({
    req(daterange_period2())
    browser()[date >= daterange_period2()[1] & date <= daterange_period2()[2]]
  })
  
  output$valuebox_phone <- renderValueBox({
    data_period1 <- sum(browser_period1()$phone_call)
    data_period2 <- sum(browser_period2()$phone_call)
    evolution <- round((data_period1 - data_period2) / data_period2 * 100, digits = 0)
    subtitle <- HTML(paste0(
      '<p> Phone calls </p>',
      if (period2()) {
        paste0('<p>', 
               ifelse(test = evolution >= 0, yes = '+', no = ''),
               evolution, 
               '% vs period 2</p>')
      } else {
        '<p> on period 1</p>'
      }
    ))
    valueBox(
      value = data_period1,
      subtitle = subtitle,
      icon = icon('phone'),
      color = 'green'
    )
  })
  
  output$valuebox_client <- renderValueBox({
    data_period1 <- sum(browser_period1()$client)
    data_period2 <- sum(browser_period2()$client)
    evolution <- round((data_period1 - data_period2) / data_period2 * 100, digits = 0)
    subtitle <- HTML(paste0(
      '<p> Clients </p>',
      if (period2()) {
        paste0('<p>', 
               ifelse(test = evolution >= 0, yes = '+', no = ''),
               evolution, 
               '% vs period 2</p>')
      } else {
        '<p> on period 1</p>'
      }
    ))
    valueBox(
      value = data_period1,
      subtitle = subtitle,
      icon = icon('user'),
      color = 'blue'
    )
  })
  
  output$valuebox_delay <- renderValueBox({
    data_period1 <- mean(browser_period1()$mean_call_duration)
    data_period2 <- mean(browser_period2()$mean_call_duration)
    evolution <- round((data_period1 - data_period2) / data_period2 * 100, digits = 0)
    subtitle <- HTML(paste0(
      '<p> Minutes </p>',
      if (period2()) {
        paste0('<p>', 
               ifelse(test = evolution >= 0, yes = '+', no = ''),
               evolution, 
               '% vs period 2</p>')
      } else {
        '<p> on period 1</p>'
      }
    ))
    valueBox(
      value = round(data_period1, digits = 1),
      subtitle = subtitle,
      icon = icon('hourglass-half'),
      color = 'orange'
    )
  })
  
  output$valuebox_events <- renderValueBox({
    data_period1 <- sum(browser_period1()$events)
    data_period2 <- sum(browser_period2()$events)
    evolution <- round((data_period1 - data_period2) / data_period2 * 100, digits = 0)
    subtitle <- HTML(paste0(
      '<p> Events </p>',
      if (period2()) {
        paste0('<p>', 
               ifelse(test = evolution >= 0, yes = '+', no = ''),
               evolution, 
               '% vs period 2</p>')
      } else {
        '<p> on period 1</p>'
      }
    ))
    valueBox(
      value = data_period1,
      subtitle = subtitle,
      icon = icon('calendar-check-o'),
      color = 'red'
    )
  })
  
}