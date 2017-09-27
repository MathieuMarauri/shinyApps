
# Header ------------------------------------------------------------------

header <- dashboardHeader(
  title = "Dashboard Shiny",
  tags$li(
    class = "dropdown",
    tags$a(
      href = "https://github.com/MathieuMarauri/shinyApps",  
      tags$img(
        height = "50px", 
        alt = "Github", 
        src = "https://github.com/MathieuMarauri/shinyApps/raw/master/data/githubLogo.png")
    )
  )
)


# Sidebar -----------------------------------------------------------------

sidebar <- dashboardSidebar(
  width = 243,
  sidebarMenu(
    id = "sidebar_menu",
    menuItem(
      text = "Global view",
      tabName = "globalview_menu",
      icon = icon("home")
    ),
    menuItem(
      text = "Map",
      tabName = "map_menu",
      icon = icon("globe")
    ),
    menuItem(
      text = "Tables",
      tabName = "table_menu",
      icon = icon('table')
    ),
    conditionalPanel(
      condition = 'input.sidebar_menu == "globalview_menu"',
      br(),
      uiOutput(outputId = "daterange_period1_ui"),
      materialSwitch(
        inputId = "period2_checkbox",
        label = "Second period?",
        status = "primary",
        right = TRUE,
        value = TRUE
      ),
      uiOutput(outputId = "daterange_period2_ui")
    )
  )
)


# Body --------------------------------------------------------------------

body <- dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  tabItems(
    tabItem(
      tabName = "globalview_menu",
      div(
        class = 'row',
        valueBoxesUI(id = 'valuebox_ui')
      ),
      div(
        class = 'row',
        drilldownchartUI(id = 'drilldownchart_ui')
      )
    ),
    tabItem(
      tabName = "map_menu"
    ),
    tabItem(
      tabName = 'table_menu'
    )
  )
)


# UI ----------------------------------------------------------------------

dashboardPage(
  header,
  sidebar,
  body
)
