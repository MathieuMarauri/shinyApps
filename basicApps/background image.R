
# Background image in body, header (with a link) and in drilldown chart.

# The images for the drilldown chart should be all the same size (http://resizeimage.net/) and preferably hosted on github (and used with raw path).

# Packages ----------------------------------------------------------------

library('shiny')
library('shinydashboard')
library('data.table')
library('highcharter')


# App ---------------------------------------------------------------------

ui <- dashboardPage(
  dashboardHeader(
    title = tags$a(
      href='https://www.netmarketshare.com/browser-market-share.aspx?qprid=2&qpcustomd=0',
      tags$img(
        src = 'https://github.com/MathieuMarauri/shinyApps/raw/master/data/browserLogos.png', 
        height = "120px"
      )
    ),
    titleWidth = 324
  ),
  dashboardSidebar(),
  dashboardBody(
    tags$style('
.main-header .logo {
  height: 114px
}

.left-side, .main-sidebar {
  padding-top: 110px
}

:root {
--font: "Century Gothic", CenturyGothic, AppleGothic, sans-serif;
--barca-blue: #00529F;
}

/* Title */
.main-header .logo {
  font-family: var(--font);
}

/* sidebar */
.skin-blue .main-sidebar {
  font-family: var(--font);
}

/* Color of the header right side */
.skin-blue .main-header .navbar {
  background-color: var(--barca-blue);
}

/* Color of the header left side */
.skin-blue .main-header .logo{
  background-color: var(--barca-blue);
}

/* Color of the header right side when hover*/
.skin-blue .main-header .logo:hover{
  background-color: var(--barca-blue);
}

/* Color of the header right side when hover*/
.skin-blue .main-header .navbar .sidebar-toggle {
  background-color: var(--barca-blue);
  color: var(--barca-blue);
}

/* Color of the header toggle when hover */
.skin-blue .main-header .navbar .sidebar-toggle:hover {
  background-color: var(--barca-blue);
  color: var(--barca-blue);
}

.content-wrapper {
  background-image: url("http://fcecrivains.com/wp-content/uploads/2014/11/pelouse10pc.jpg");
  background-repeat: repeat;
  background-size: 100%;
}
'),
    div(
      style = 'text-align: center;',
      div(
        style = 'background-color: #FFFFFF; width: 50%; display: inline-block;',
        highchartOutput(outputId = 'plot')
      )
    )
  )
)

server <- function(input, output){
  
  output$plot <- renderHighchart({
    browser <- readRDS('data/browser.rds')
    level1 <- browser[, .(y = sum(data)), by = name]
    level1$drilldown <- level1$name
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
                $('#plot').css('background', 'url(' + image + ') no-repeat 50% 50%').css('background-size', '15%');
             }"
          ),
          drillup = JS(
            "function(e) { 
               $('#plot').css('background-image', 'none'); 
             }"
          )
        )
      )
  })
}
shinyApp(ui = ui, server = server)

