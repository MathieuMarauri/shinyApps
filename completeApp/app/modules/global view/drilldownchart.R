
# UI ----------------------------------------------------------------------

drilldownchartUI <- function(id){
  ns <- NS(id)
  tagList(
    div(
      class = 'col-md-6',
      div(
        class = 'custom-box-container',
        div(
          class = 'custom-box-header',
          div(
            class = 'custom-box-header-left',
            h3(
              class = 'custom-box-title',
              'Number of visits by source and model'
            )
          ),
          div(
            class = 'custom-box-header-right',
            dropdown(
              h3('Some parameters'),
              pickerInput(
                inputId = ns('theme'),
                label = 'Select a theme',
                choices = c('Classic', 'Fianacial Times', 'Flat', 'Google', 'Firefox', 'Tufte', 'Hand Drawn')
                # choicesOpt = list(icon = c('fa-bar-chart-o', 'fa-newspaper-o', 'fa-minus', 'fa-google', 'fa-firefox', 'fa-square-o', 'fa-pensil'))
              ),
              h6('Reversed order'),
              switchInput(inputId = ns('order'), 
                          label = '<i class = "fa fa-line-chart"></i>'),
              style = 'jelly', # simple, bordered, minimal, stretch, jelly, gradient, fill, material-circle, material-flat, pill, float, unite
              animate = animateOptions(enter = animations$fading_entrances$fadeInLeftBig,
                                       exit = animations$fading_exits$fadeOutRightBig),
              size = 's',
              status = 'primary',
              icon = icon("gear"),
              width = "300px",
              right = FALSE,
              tooltip = tooltipOptions(title = 'Want to change something ?')
            )
          )
        ),
        div(
          highchartOutput(outputId = ns('barchart'), width = '100%', height = '300px')
        )
      )
    ),
    div(
      class = 'col-md-6',
      div(
        class = 'custom-box-container',
        div(
          class = 'custom-box-header',
          div(
            class = 'custom-box-header-left',
            h3(
              class = 'custom-box-title',
              'Browser market share'
            )
          )
        ),
        div(
          highchartOutput(outputId = ns('piechart'), width = '100%', height = '300px')
        )
      )
    )
  )
}

# Server ------------------------------------------------------------------

drilldownchart <- function(input, output, session, daterange_period1, daterange_period2){
  
  # barchart
  barchart_data <- reactive({
    set.seed(123456789)
    data.table(date = rep(seq(from = as.Date('2017-01-01'), to = as.Date('2017-12-31'), by = 1), each = 50),
               source = sample(x = c('Display', 'Affiliation', 'Paid search', 'Retargeting', 'Emailing'), 
                               replace = TRUE, 
                               size = 10 * 365, 
                               prob = c(0.1, 0.3, 0.2, 0.05, 0.35)),
               model = rep(paste0('model_', 1:10), times = 5 * 365),
               kpi1 = floor(runif(min = 100, max = 1000, n = 365 * 5 * 10)),
               kpi2 = floor(runif(min = 50, max = 200, n = 365 * 5 * 10)))
  })
  
  output$barchart <- renderHighchart({
    req(daterange_period1(), daterange_period2())
    kpi1_period1 <- barchartData(data = barchart_data(), kpi = 'kpi1', daterange = daterange_period1(), stack = 'period1')
    kpi2_period1 <- barchartData(data = barchart_data(), kpi = 'kpi2', daterange = daterange_period1(), stack = 'period1')
    kpi1_period2 <- barchartData(data = barchart_data(), kpi = 'kpi1', daterange = daterange_period2(), stack = 'period2')
    kpi2_period2 <- barchartData(data = barchart_data(), kpi = 'kpi2', daterange = daterange_period2(), stack = 'period2')
    
    max_xAxis <- max(length(kpi1_period1$level1), length(kpi1_period2$level1))
    
    highchart() %>%
      hc_chart(
        type = 'bar',
        zoomType = 'xy'
      ) %>% 
      hc_title(
        text = 'Click to see details'
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
        name = 'period1_kpi1',
        data = kpi1_period1$level1,
        stack = 'period1'
      ) %>%
      hc_add_series(
        name = 'period2_kpi1',
        data = kpi1_period2$level1,
        stack = 'period2'
      ) %>%
      hc_add_series(
        name = 'period1_kpi2',
        data = kpi2_period1$level1,
        stack = 'period1'
      ) %>%
      hc_add_series(
        name = 'period2_kpi2',
        data = kpi2_period2$level1,
        stack = 'period2'
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
        series = c(kpi1_period1$level2, kpi1_period2$level2, kpi2_period1$level2, kpi2_period2$level2)
      ) %>% 
      hc_add_theme(theme)
  })
  
  # piechart
  browser <- reactive(readRDS('input/browser.rds'))
  
  output$piechart <- renderHighchart({
    level1 <- browser()[, .(y = sum(data)), by = name][order(-y), ]
    level1$drilldown <- level1$name
    level2 <- lapply(X = level1$name,
                     FUN = function(x){
                       id <- tolower(x)
                       data <- browser()[name == x]
                       data <- data[, .(name = categories, y = data)][order(-y), ]
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
              $('#drilldownchart_ui-piechart').css('background', 'url(' + image + ') no-repeat 50% 50%').css('background-size', '15%');
             }"
          ),
          drillup = JS(
            "function(e) { 
              $('#drilldownchart_ui-piechart').css('background-image', 'none'); 
             }"
          )
        )
      ) %>% 
      hc_add_theme(theme)
  })
  
  
}
