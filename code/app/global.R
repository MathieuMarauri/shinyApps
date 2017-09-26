
# Packages ----------------------------------------------------------------

library('shiny')
library('shinydashboard')
library('shinyjs')
library('shinyBS')
library('shinyWidgets') # the dev version 0.3.4.930
library('highcharter')
library('DT')
library('data.table')


# Function ----------------------------------------------------------------

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


# Highchart theme ---------------------------------------------------------

hearder_color <- '#17C6A3'
danger_color <- '#FF5555'
warning_color <- '#FABB3D'
dark_grey <- '#374649'
soft_grey <- '#5F6B6D'
info_color <- '#67C2EF'
element_color <- soft_grey

theme <-  hc_theme(
  colors = c(hearder_color, dark_grey, danger_color, warning_color, info_color, soft_grey),
  chart = list(
    backgroundColor = "transparent",
    style = list(
      fontFamily = 'Century Gothic, sans-serif'
    )
  ),
  xAxis = list(
    gridLineColor = element_color,
    gridLineWidth = 1,
    gridLineDashStyle = 'dot',
    lineColor = element_color,
    lineWidth = 1,
    tickColor = element_color,
    labels = list(
      style = list(
        color = element_color
      )
    ),
    title = list(
      style = list(
        color = element_color
      )
    )
  ),
  yAxis = list(
    gridLineColor = element_color,
    gridLineWidth = 1,
    gridLineDashStyle = 'dot',
    lineColor = element_color,
    lineWidth = 1,
    tickColor = element_color,
    labels = list(
      style = list(
        color = element_color
      )
    ),
    title = list(
      style = list(
        color = element_color
      )
    )
  ),
  tooltip = list(
    backgroudColor = element_color,
    borderColor = element_color,
    borderRadius = 0,
    style = list(
      color = element_color
    )
  ),
  drilldown = list(
    activeAxisLabelStyle = list(
      textDecoration = 'none',
      fontStyle = 'italic',
      color = element_color
    )
  ),
  title = list(
    style = list(
      color = element_color,
      fontSize = '23px',
      fontWeight = 'bold'
    )
  ),
  legend = list(
    itemStyle = list(
      color = element_color
    ),
    itemHiddenStyle = list(
      color = '#222222'
    )
  )
)


# Modules -----------------------------------------------------------------

# tab global_view
source('modules/global view/valuebox.R')
source('modules/global view/drilldownchart.R')

