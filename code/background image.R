
# How to add a background image in the body and in a drilldown chart.

# Packages ----------------------------------------------------------------

library('shiny')
library('shinydashboard')
library('data.table')
library('highcharter')


# App ---------------------------------------------------------------------

ui <- dashboardPage(
  dashboardHeader(
    title = tags$a(
      href='https://fr.wikipedia.org/wiki/Saison_2015-2016_du_FC_Barcelone',
      tags$img(
        src = 'https://vignette2.wikia.nocookie.net/logopedia/images/0/0e/Barcelona.png/revision/latest?cb=20161111145150', 
        height = "120px"
      )
    ),
    titleWidth = 324,
    tags$li(
      a(
        "Saison 2015-2016",
        style = "font-size: 36px; padding-top:44px; padding-right:5% ;"
      ),
      align = 'center',
      class = "dropdown"
    )
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
        style = 'background-color: rgba(0, 0, 0, 0.5); width: 50%; display: inline-block;',
        highchartOutput(outputId = 'plot')
      )
    )
  )
)

server <- function(input, output){
  
  output$plot <- renderHighchart({
    buts <- readRDS('../data/occaz_but.rds')
    
    buts <- setDT(buts)[, .(name = fnt, y = But, half_occasion = `1/2 occasions`, occasion = Occasions, drilldown = tolower(fnt))]
    
    plot_level1 <- list_parse(buts)
    
    plot_level2 <- lapply(X = buts[, name],
                          FUN = function(x){
                            id <- tolower(x)
                            name <- 'source_analyse'
                            data <- buts[name == x]
                            data <- melt(data = data[, .(but = y, half_occasion, occasion)],
                                         measure.vars = c('but', 'half_occasion', 'occasion'))
                            data <- list_parse2(data)
                            return(list(id = id, name = name, data = data))
                          })
    
    highchart() %>%
      hc_chart(type = "pie") %>%
      hc_title(text = "But et occasions des joueurs") %>%
      hc_subtitle(text = "Cliquer sur un joueur pour voir le detail de ses occasions") %>%
      hc_xAxis(type = "category") %>%
      hc_legend(enabled = FALSE) %>%
      hc_plotOptions(
        series = list(
          borderWidth = 0
        ),
        pie = list(
          innerSize = '67%',
          colors = c("#00529F", "#e5bd2b", "#93111d"),
          dataLabels = list(
            enabled = TRUE,
            format = '<b>{point.name}</b>: {point.y}'
          )
        )
      ) %>%
      hc_add_series(
        name = "Les buts inscrits",
        colorByPoint = FALSE,
        data = plot_level1,
        color = "#00529F",
        type = "column",
        dataLabels = list(
          enabled = TRUE
        )
      ) %>%
      hc_drilldown(
        allowPointDrilldown = TRUE,
        activeDataLabelStyle = list(
          textDecoration = 'none',
          fontStyle = 'none'
        ),
        drillUpButton = list(
          relativeTo = 'spacingBox'
        ),
        series = plot_level2
      ) %>%
      hc_chart(
        backgroundColor = NULL,
        events = list(
          drilldown = JS(
            "function(e){
              console.log(e.point.name);
              var image = 'none';
              if(e.point.name == 'Adriano') {
              image = 'http://img.uefa.com/imgml/TP/players/9/2015/324x324/98593.jpg';
              } else if (e.point.name == 'Alba') {
              image = 'http://img.uefa.com/imgml/TP/players/9/2015/324x324/250000036.jpg';
              } else if (e.point.name == 'Alves') {
              image = 'http://img.uefa.com/imgml/TP/players/1/2015/324x324/95724.jpg'
              } else if (e.point.name == 'Bartra') {
              image = 'http://thetopforward.com/uploads/0/Marc%20Bartra.jpg'
              } else if (e.point.name == 'Busquets') {
              image = 'http://img.uefa.com/imgml/TP/players/1/2015/324x324/250002704.jpg'
              } else if (e.point.name == 'Douglas') {
              image = 'http://img.uefa.com/imgml/TP/players/1/2015/324x324/250075674.jpg'
              } else if (e.point.name == 'El Haddadi') {
              image = 'http://img.uefa.com/imgml/TP/players/1/2015/324x324/250063866.jpg'
              } else if (e.point.name == 'Gumbau') {
              image = 'http://img.uefa.com/imgml/TP/players/1/2016/324x324/250086317.jpg'
              } else if (e.point.name == 'Iniesta') {
              image = 'http://img.uefa.com/imgml/TP/players/1/2015/324x324/58031.jpg'
              } else if (e.point.name == 'Mascherano') {
              image = 'http://www.thesportsdb.com/images/media/player/thumb/qrxpyw1431636012.jpg'
              } else if (e.point.name == 'Mathieu') {
              image = 'http://img.uefa.com/imgml/TP/players/1/2015/324x324/65012.jpg'
              } else if (e.point.name == 'Messi') {
              image = 'https://ucl.truevisions.tv/images/topscore/Lionel%20Messi.jpg'
              } else if (e.point.name == 'Neymar') {
              image = 'http://media.baogiaothong.vn/files/Baogiay/2015/04/01/145-0716.jpg'
              } else if (e.point.name == 'Pique') {
              image = 'http://img.uefa.com/imgml/TP/players/1/2015/324x324/93148.jpg'
              } else if (e.point.name == 'Rafinha') {
              image = 'http://img.uefa.com/imgml/TP/players/1/2015/324x324/250013895.jpg'
              } else if (e.point.name == 'Rakitic') {
              image = 'http://img.uefa.com/imgml/TP/players/9/2015/324x324/97744.jpg'
              } else if (e.point.name == 'Ramirez') {
              image = 'http://img.uefa.com/imgml/TP/players/1/2015/324x324/250042431.jpg'
              } else if (e.point.name == 'Roberto') {
              image = 'http://img.uefa.com/imgml/TP/players/1/2015/324x324/250006092.jpg'
              } else if (e.point.name == 'Suarez') {
              image = 'http://media.footalist.com/compos/uploads/y.jpg'
              } else if (e.point.name == 'Vermaelen') {
              image = 'http://img.uefa.com/imgml/TP/players/1/2015/324x324/71982.jpg'
              }
              $('#plot').css('background', 'url(\"'+image+'\") no-repeat 50% 70%').css({'background-size':'20%'});
            }"
      ),
      drillup = JS(
        "function(e){ console.log(e); $('#plot').css('background-image', 'none'); }"
      )
          )
      )
  })
}
shinyApp(ui = ui, server = server)

