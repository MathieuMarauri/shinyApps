
# Drilldown with image pie ------------------------------------------------

# images are downloaded and resized with http://resizeimage.net/. Then they are hosted on github and the path is used with raw.

library(highcharter)
library(data.table)

browser <- readRDS('completeApp/app/input/browser.rds')

# level1
level1 <- browser[, .(y = sum(data)), by = name]
level1$drilldown <- level1$name

# level2
level2 <- lapply(X = level1$name,
                 FUN = function(x){
                   id <- tolower(x)
                   data <- browser[name == x]
                   data <- data[, .(name = categories, y = data)]
                   data <- list_parse2(data)
                   return(list(id = id, name = 'Total share', data = data))
                 })

highchart() %>% 
  hc_chart(type = 'pie') %>% 
  hc_add_series(name = 'Total share',data = list_parse(level1)) %>% 
  hc_drilldown(
    activeDataLabelStyle = list(
      textDecoration = 'none',
      fontStyle = 'none'
    ),
    series = level2
  ) %>% 
  hc_plotOptions(
    series = list(
      borderWidth = 0
    ),
    pie = list(
      innerSize = '50%',
      dataLabels = list(
        enabled = TRUE,
        format = '<b>{point.name}</b>: {point.y}'
      )
    )
  ) %>% 
  hc_chart(
    backgroundColor = NULL,
    events = list(
      drilldown = JS(
        "function(e){
          var image = 'none';
          if (e.point.name == 'chrome') {
            image = 'https://github.com/MathieuMarauri/shinyApps/raw/master/completeApp/app/www/chrome.png';
          } else if (e.point.name == 'msie') {
            image = 'https://github.com/MathieuMarauri/shinyApps/raw/master/completeApp/app/www/ie.png';
          } else if (e.point.name == 'firefox') {
            image = 'https://github.com/MathieuMarauri/shinyApps/raw/master/completeApp/app/www/firefox.png';
          } else if (e.point.name == 'safari') {
            image = 'https://github.com/MathieuMarauri/shinyApps/raw/master/completeApp/app/www/safari.png';
          } else if (e.point.name == 'opera') {
            image = 'https://github.com/MathieuMarauri/shinyApps/raw/master/completeApp/app/www/opera.png';
          }
          $('#browser_plot').css('background', 'url(' + image + ') no-repeat 50% 50%').css('background-size', '15%');
        }"
      ),
      drillup = JS(
        "function(e) { 
          $('#browser_plot').css('background-image', 'none'); 
        }"
      )
    )
  ) %>% 
  hc_elementId(id = 'browser_plot')


# Value boxes data --------------------------------------------------------

library('data.table')

set.seed(123456789)
valuebox_data <- data.table(date = seq(from = as.Date('2017-01-01'), to = as.Date('2017-12-31'), by = 1),
                            phone_call = runif(min = 0, max = 50, n = 365),
                            mean_call_duration = rnorm(mean = 2.5, sd = 2, n = 365),
                            events = runif(min = 0, max = 10, n = 365))


# Grouped stacked bar chart -----------------------------------------------

library('data.table')
library('highcharter')

set.seed(123456789)
grouped_stacked_data <- data.table(date = rep(seq(from = as.Date('2017-01-01'), to = as.Date('2017-12-31'), by = 1), each = 50),
                                   source = sample(x = c('Display', 'Affiliation', 'Paid search', 'Retargeting', 'Emailing'), 
                                                       replace = TRUE, 
                                                       size = 10 * 365, 
                                                       prob = c(0.1, 0.3, 0.2, 0.05, 0.35)),
                                   model = rep(paste0('model_', 1:10), times = 5 * 365),
                                   kpi1 = floor(runif(min = 100, max = 1000, n = 365 * 5 * 10)),
                                   kpi2 = floor(runif(min = 50, max = 200, n = 365 * 5 * 10)))

input <- list(daterange_analysis = c('2017-04-05', '2017-07-15'), 
              daterange_comparison = c('2017-02-05', '2017-05-15'),
              comparison = TRUE,
              kpi2 = TRUE)

# scroll bar on drilldown but not on drillup : https://stackoverflow.com/questions/32794237/how-to-remove-scroll-bar-in-drill-down-highchart

#' 
#' This function builds the data used to create one serie and its associated drilldown serie for a grouped-stacked bar chart 
#' 
#' @param data a data.table with the data to structure
#' @param kpi the name of the kpi to analyze
#' @param daterange a character vector of length 2 with the date limits
#' @param stack the name of the stack
#' 
#' @return a list with the 2 levels
#' 
barchartData <- function(data, kpi, daterange, stack){
  data <- data[date >= daterange[1] & date <= daterange[2], .(y = sum(get(kpi))), by = list(source, model)]
  data_level1 <- data[, .(y = sum(y)), by = source]
  data_level1 <- data_level1[, .(name = source, y, drilldown = paste0(tolower(source), '_', stack, '_', kpi))]
  setorderv(data_level1, cols = 'y', order = -1L)
  data_level2 <- lapply(X = data_level1$name,
                                 FUN = function(x){
                                   id <- paste0(tolower(x), '_', stack, '_', kpi)
                                   name <- paste0('source_', stack, '_', kpi)
                                   data <- data[source == x, .(name = model, y)]
                                   setorderv(data, cols = 'y', order = -1L)
                                   data <- list_parse2(data)
                                   stack <- stack
                                   return(list(id = id, name = name, data = data, stack = stack))
                                 })
  return(list(level1 = list_parse(data_level1), level2 = data_level2))
}

kpi1_analysis <- barchartData(data = grouped_stacked_data, kpi = 'kpi1', daterange = input$daterange_analysis, stack = 'analysis')
kpi2_analysis <- barchartData(data = grouped_stacked_data, kpi = 'kpi2', daterange = input$daterange_analysis, stack = 'analysis')
kpi1_comparison <- barchartData(data = grouped_stacked_data, kpi = 'kpi1', daterange = input$daterange_comparison, stack = 'comparison')
kpi2_comparison <- barchartData(data = grouped_stacked_data, kpi = 'kpi2', daterange = input$daterange_comparison, stack = 'comparison')

max_xAxis <- max(length(kpi1_comparison$level1), length(kpi1_analysis$level1))

highchart <- highchart() %>%
  hc_chart(
    type = 'bar',
    zoomType = 'xy'
  ) %>% 
  hc_title(
    title = 'Click to see details'
  ) %>% 
  hc_xAxis(
    type = 'category',
    min = 0,
    max = max_xAxis - 1,
    scrollbar = list(
      enabled = TRUE
    )
  ) %>%
  hc_plotOptions(
    bar = list(
      stacking = 'normal',
      showInLegend = FALSE,
      borderWidth = 0,
      dataLabels = list(
        enabled = FALSE
      )
    )
  ) %>%
  hc_add_series(
    name = 'analysis_kpi1',
    data = kpi1_analysis$level1,
    stack = 'analysis'
  ) %>%
  hc_add_series(
    name = 'comparaison_kpi1',
    data = kpi1_comparison$level1,
    stack = 'comparison'
  ) %>%
  hc_add_series(
    name = 'analysis_kpi2',
    data = kpi2_analysis$level1,
    stack = 'analysis'
  ) %>%
  hc_add_series(
    name = 'comparaison_kpi2',
    data = kpi2_comparison$level1,
    stack = 'comparison'
  ) %>%
  hc_drilldown(
    drillUpButton = list(
      relativeTo = 'spacingBox',
      position = list(
        x = 0,
        y = 0
      )
    ),
    activeAxisLabelStyle = list(
      textDecoration = 'none'
    ),
    allowPointDrilldown = FALSE,
    series = c(kpi1_analysis$level2, kpi1_comparison$level2, kpi2_analysis$level2, kpi2_comparison$level2)
  )
highchart



