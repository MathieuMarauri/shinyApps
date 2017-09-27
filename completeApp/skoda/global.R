
# Pacakges ----------------------------------------------------------------

library('jsonlite')
library('shiny')
library('shinydashboard')
library('shinyjs')
library('shinyWidgets')
library('data.table')
library('highcharter')
library('magrittr')
library('ggplot2')
library('stringi')
library('DT')
library('gtools')
library('RMySQL')


# Global variables --------------------------------------------------------

skoda_green <- '#4BA82E'
skoda_grey <- '#9C9D9F'

theme <-  hc_theme(
  colors = c(skoda_green, '#9b59b6', '#f92672', '#ffffff', '#4acbe5'),
  chart = list(
    backgroundColor = "transparent",
    style = list(
      fontFamily = 'Century Gothic, sans-serif'
    )
  ),
  xAxis = list(
    gridLineColor = skoda_grey,
    gridLineWidth = 1,
    gridLineDashStyle = 'dot',
    lineColor = skoda_grey,
    lineWidth = 1,
    tickColor = skoda_grey,
    labels = list(
      style = list(
        color = skoda_grey
      )
    ),
    title = list(
      style = list(
        color = skoda_grey
      )
    )
  ),
  yAxis = list(
    gridLineColor = skoda_grey,
    gridLineWidth = 1,
    gridLineDashStyle = 'dot',
    lineColor = skoda_grey,
    lineWidth = 1,
    tickColor = skoda_grey,
    labels = list(
      style = list(
        color = skoda_grey
      )
    ),
    title = list(
      style = list(
        color = skoda_grey
      )
    )
  ),
  tooltip = list(
    backgroudColor = skoda_grey,
    borderColor = skoda_grey,
    borderRadius = 0,
    style = list(
      color = '#555555'
    )
  ),
  drilldown = list(
    activeAxisLabelStyle = list(
      textDecoration = 'none',
      fontStyle = 'italic',
      color = skoda_grey
    )
  ),
  title = list(
    style = list(
      color = '#ffffff',
      fontSize = '23px',
      fontWeight = 'bold'
    )
  ),
  legend = list(
    itemStyle = list(
      color = skoda_grey
    ),
    itemHiddenStyle = list(
      color = '#222222'
    )
  )
)


# Structure data ----------------------------------------------------------

#'
#' This function coerces a json string to a data table.
#' The json must contain 6 values in the proper order.
#'
#' @param json a string with json structure
#' @param origin a string defining the origin of the table so the columns names are properly set
#'
#' @return a data table
#'
jsonToDF <- function(json, origin = 'overview'){
  origin <- match.arg(arg = origin, choices = c('overview', 'bonusMalus'))
  if(origin == 'overview'){
    names <- c("district", "concession", "origin", "campagne", "vehicle_name", "nature_lead", "status")
  } else if(origin == 'bonusMalus'){
    names <- c('district', 'concession', 'status')
  }
  result <- fromJSON(json, flatten = TRUE)
  result <- unlist(result)
  result <- data.table(variables = names(result), count = as.vector(result))
  result[, (names) := tstrsplit(variables, ".", fixed = TRUE)]
  result <- result[, .SD, .SDcols = c(names, "count")]
  result <- dcast(
    data = result,
    formula = ... ~ status,
    value.var = 'count',
    fill = NA
  )
  return(result)
}

#'
#' This function transforms a table with a json column into a data.table.
#' The table must have a date column named leads_date and a json column named leads_data.
#'
#' @param daterange the dates to filter the table on.
#' @param data the initial table with the json column to coerce.
#' @param origin a string defining the origin of the table so the columns names are properly set
#'
#' @return a data.table with the first column being the date associated with the json string that is coerced to a table with the jsonToDF function
#'
listToDF <- function(daterange, data, origin = 'overview'){
  result <- lapply(X = daterange,
                   FUN = function(date) cbind(date = date, jsonToDF(json = data[leads_date == date, leads_data], origin = origin)))
  result <- rbindlist(result)
  return(result)
}

#'
#' This function transforms the bonusMalusDetails table into a data.table.
#'
#' @param daterange the dates to filter the table on.
#' @param data the initial table with the json column to coerce.
#'
#' @return a data.table with the first column being the date associated with the json string that is coerced to a table with the fromJSON function
#'
listToDF2 <- function(daterange, data){
  result <- lapply(X = daterange,
                   FUN = function(date) cbind(date = date, fromJSON(data[leads_date == date, leads_data], flatten = TRUE)))
  result <- rbindlist(result)
  return(result)
}


# RenderCustomValueBox ----------------------------------------------------

#' This function creates a custom value box
#'
#' @param kpi the name of the kpi displayed in the value box
#' @param titre the string that will be displayed as the subtitle of the value box
#' @param report_analyse_summary the table with the data for the analyse period
#' @param comparison boolean, should a comparison period be taken into account
#' @param report_comparaison_summary the table with the data for the comparison period
#' @param icon the icon used
#'
#' @return a custon value box
renderCustonValueBox <- function(kpi, titre, skoda_bank = FALSE, report_analyse_summary, comparison, report_comparaison_summary, icon = 'home'){
  analyse <- ceiling(sum(report_analyse_summary[kpi]))
  if(comparison){
    comparaison <- ceiling(sum(report_comparaison_summary[kpi]))
    pourcentage_evol <- round((analyse - comparaison) / comparaison * 100, digits = 0)
    valueBox(
      value = analyse,
      subtitle = HTML(paste0(
        '<p style = "font-size:16px; margin:0px"><b>', titre,' </b></p>',
        if(kpi == 'lead_brut' & skoda_bank) paste0('<p style = "margin-bottom:0px; font-size:12px;"><i> (dont ',
                                                   ceiling(sum(report_analyse_summary['skoda_bank'])),
                                                   ' skoda bank)</i></p>'),
        ifelse(test = comparaison > 0,
               yes = paste0('<p style = "font-size:16px;"> soit ',
                            ifelse(test = pourcentage_evol >= 0, yes = '+', no = ''),
                            pourcentage_evol, '% vs comparaison</p>'),
               no = '<p style = "font-size:16px;"> 0 en p\u00e9riode comparaison')
      )),
      icon = icon(icon),
      color = "blue"
    )
  } else{
    valueBox(
      value = analyse,
      subtitle = HTML(
        paste0(
          '<p style = "font-size:16px; margin:0px"><b>', titre,' </b></p>',
          if(kpi == 'lead_brut' & skoda_bank) paste0('<p style = "margin-bottom:0px; font-size:12px;"><i> (dont ',
                                                     ceiling(sum(report_analyse_summary['skoda_bank'])),
                                                     ' skoda bank)</i></p>')
        )
      ),
      icon = icon(icon),
      color = "blue"
    )
  }
}


# RenderBarchart ----------------------------------------------------------

#'
#' This functions creates a grouped drilldown bar chart.
#'
#'  @param kpi the name of the metric value
#'  @param dimension1 the name of the dimension used to grouped the data for the first level
#'  @param dimenson2 the name of the dimension used to grouped the data for the second level (the drilldown)
#'  @param titre the title of the plot
#'  @param report_analyse the table with the data relative to the analyse period
#'  @param comparison bolean, is a comparison period taken into account
#'  @param report_comparaison the table with the data relative to the comparison period
#'  @param skoda_bank boolean, should skoda bank leads be plotted
#'  @param scroll boolean, should a scroll bar be added
#'  @param repartition boolean, should results be given in percentage
#'
#'  @return the highchart bar chart
#'
renderBarchart <- function(kpi, dimension1, dimension2 = NULL, titre, report_analyse, comparison, report_comparaison, skoda_bank = FALSE, scroll = TRUE, repartition = FALSE){
  highchart <- highchart() %>%
    hc_chart(
      type = 'bar',
      zoomType = 'xy',
      style = list(fontFamily = 'Century Gothic, sans-serif')
    ) %>%
    hc_title(
      text = titre
    ) %>%
    hc_add_theme(theme)
  if(scroll){
    highchart <- highchart %>%
      hc_xAxis(
        type = 'category',
        min = 0,
        max = 4,
        scrollbar = list(
          enabled = TRUE
        )
      )
  } else{
    highchart <- highchart %>%
      hc_xAxis(
        type = 'category'
      )
  }

  if(!is.null(dimension2)){
    if(!(kpi == 'lead_brut' & skoda_bank & !repartition)){
      # Analyse
      # level 1
      bar_data_analyse <- report_analyse[, .(kpi = base::sum(.SD)), by = c(dimension1, dimension2), .SDcols = kpi]
      level_1_analyse <- bar_data_analyse[, .(kpi = sum(kpi)), by = dimension1]
      level_1_analyse <- level_1_analyse[, .(name = get(dimension1), y = kpi, drilldown = paste0(tolower(get(dimension1)), '_analyse'))]
      if(repartition){
        level_1_analyse[, y := round(y / sum(y) * 100, digits = 2)]
      }
      setorderv(level_1_analyse, cols = 'y', order = -1L)
      # level 2
      level_2_analyse <- lapply(X = level_1_analyse[, name],
                                FUN = function(x){
                                  id <- paste0(tolower(x), '_analyse')
                                  name <- 'source_analyse'
                                  data <- bar_data_analyse[get(dimension1) == x, .(name = get(dimension2), y = kpi)]
                                  if(repartition){
                                    data[, y := round(y / sum(y) * 100, digits = 2)]
                                  }
                                  setorderv(data, cols = 'y', order = -1L)
                                  data <- list_parse2(data)
                                  stack = 'analyse'
                                  return(list(id = id, name = name, data = data, stack = stack))
                                })
      if(comparison){
        # Comparaison
        # level 1
        bar_data_comparaison <- report_comparaison[, .(kpi = base::sum(.SD)), by = c(dimension1, dimension2), .SDcols = kpi]
        level_1_comparaison <- bar_data_comparaison[, .(kpi = sum(kpi)), by = dimension1]
        level_1_comparaison <- level_1_comparaison[, .(name = get(dimension1), y = kpi, drilldown = paste0(tolower(get(dimension1)), '_comparaison'))]
        if(repartition){
          level_1_comparaison[, y := round(y / sum(y) * 100, digits = 2)]
        }
        setorderv(level_1_comparaison, cols = 'y', order = -1L)
        # level 2
        level_2_comparaison <- lapply(X = level_1_comparaison[, name],
                                      FUN = function(x){
                                        id <- paste0(tolower(x), '_comparaison')
                                        name <- 'source_comparaison'
                                        data <- bar_data_comparaison[get(dimension1) == x, .(name = get(dimension2), y = kpi)]
                                        if(repartition){
                                          data[, y := round(y / sum(y) * 100, digits = 2)]
                                        }
                                        setorderv(data, cols = 'y', order = -1L)
                                        data <- list_parse2(data)
                                        stack = 'comparaison'
                                        return(list(id = id, name = name, data = data, stack = stack))
                                      })
        # plot
        highchart <- highchart %>%
          hc_plotOptions(
            bar = list(
              showInLegend = FALSE,
              borderWidth = 0,
              dataLabels = list(
                enabled = FALSE
              )
            )
          ) %>%
          hc_add_series(
            name = 'analyse',
            data = list_parse(level_1_analyse)
          ) %>%
          hc_add_series(
            name = 'comparaison',
            data = list_parse(level_1_comparaison)
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
            series = c(level_2_analyse, level_2_comparaison)
          )
      } else if(!comparison){
        highchart <- highchart %>%
          hc_plotOptions(
            bar = list(
              showInLegend = FALSE,
              borderWidth = 0,
              dataLabels = list(
                enabled = FALSE
              )
            )
          ) %>%
          hc_add_series(
            name = 'analyse',
            data = list_parse(level_1_analyse)
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
            series = level_2_analyse
          )
      }
    } else if(kpi == 'lead_brut' & skoda_bank & !repartition){
      # analyse
      bar_data_analyse <- report_analyse[`est skoda bank` == 0, .(kpi = sum(lead_brut)), by = c(dimension1, dimension2)]
      level_1_analyse <- bar_data_analyse[, .(kpi = sum(kpi)), by = dimension1]
      level_1_analyse <- level_1_analyse[, .(name = get(dimension1), y = kpi, drilldown = paste0(tolower(get(dimension1)), '_analyse'))]
      setorderv(level_1_analyse, cols = 'y', order = -1L)
      level_2_analyse <- lapply(X = level_1_analyse[, name],
                                FUN = function(x){
                                  id <- paste0(tolower(x), '_analyse')
                                  name <- 'source_analyse'
                                  data <- bar_data_analyse[get(dimension1) == x, .(name = get(dimension2), y = kpi)]
                                  setorderv(data, cols = 'y', order = -1L)
                                  data <- list_parse2(data)
                                  stack = 'analyse'
                                  return(list(id = id, name = name, data = data, stack = stack))
                                })
      # analyse skoda
      report_analyse_skoda <- report_analyse[`est skoda bank` > 0]
      if(nrow(report_analyse_skoda) > 0){
        bar_data_analyse_skoda <- report_analyse_skoda[, .(kpi = sum(lead_brut)), by = c(dimension1, dimension2)]
        level_1_analyse_skoda <- bar_data_analyse_skoda[, .(kpi = sum(kpi)), by = dimension1]
        level_1_analyse_skoda <- level_1_analyse_skoda[, .(name = get(dimension1), y = kpi, drilldown = paste0(tolower(get(dimension1)), '_analyse_skoda'))]
        setorderv(level_1_analyse_skoda, cols = 'y', order = -1L)
        level_2_analyse_skoda <- lapply(X = level_1_analyse_skoda[, name],
                                        FUN = function(x){
                                          id <- paste0(tolower(x), '_analyse_skoda')
                                          name <- 'source_analyse_skoda'
                                          data <- bar_data_analyse_skoda[get(dimension1) == x, .(name = get(dimension2), y = kpi)]
                                          setorderv(data, cols = 'y', order = -1L)
                                          data <- list_parse2(data)
                                          stack = 'analyse'
                                          return(list(id = id, name = name, data = data, stack = stack))
                                        })
      } else{
        level_1_analyse_skoda <- data.table(name = character(), y = numeric(), drilldown = character())
        level_2_analyse_skoda <- NULL
      }
      if(comparison){
        # comparaison
        bar_data_comparaison <- report_comparaison[`est skoda bank` == 0, .(kpi = sum(lead_brut)), by = c(dimension1, dimension2)]
        level_1_comparaison <- bar_data_comparaison[, .(kpi = sum(kpi)), by = dimension1]
        level_1_comparaison <- level_1_comparaison[, .(name = get(dimension1), y = kpi, drilldown = paste0(tolower(get(dimension1)), '_comparaison'))]
        setorderv(level_1_comparaison, cols = 'y', order = -1L)
        level_2_comparaison <- lapply(X = level_1_comparaison[, name],
                                      FUN = function(x){
                                        id <- paste0(tolower(x), '_comparaison')
                                        name <- 'source_comparaison'
                                        data <- bar_data_comparaison[get(dimension1) == x, .(name = get(dimension2), y = kpi)]
                                        setorderv(data, cols = 'y', order = -1L)
                                        data <- list_parse2(data)
                                        stack = 'comparaison'
                                        return(list(id = id, name = name, data = data, stack = stack))
                                      })
        # comparaison skoda
        report_comparaison_skoda <- report_comparaison[`est skoda bank` > 0]
        if(nrow(report_comparaison_skoda) > 0){
          bar_data_comparaison_skoda <- report_comparaison_skoda[, .(kpi = sum(lead_brut)), by = c(dimension1, dimension2)]
          level_1_comparaison_skoda <- bar_data_comparaison_skoda[, .(kpi = sum(kpi)), by = dimension1]
          level_1_comparaison_skoda <- level_1_comparaison_skoda[, .(name = get(dimension1), y = kpi, drilldown = paste0(tolower(get(dimension1)), '_comparaison_skoda'))]
          setorderv(level_1_comparaison_skoda, cols = 'y', order = -1L)
          level_2_comparaison_skoda <- lapply(X = level_1_comparaison_skoda[, name],
                                              FUN = function(x){
                                                id <- paste0(tolower(x), '_comparaison_skoda')
                                                name <- 'source_comparaison_skoda'
                                                data <- bar_data_comparaison_skoda[get(dimension1) == x, .(name = get(dimension2), y = kpi)]
                                                setorderv(data, cols = 'y', order = -1L)
                                                data <- list_parse2(data)
                                                stack = 'comparaison'
                                                return(list(id = id, name = name, data = data, stack = stack))
                                              })
        } else{
          level_1_comparaison_skoda <- data.table(name = character(), y = numeric(), drilldown = character())
          level_2_comparaison_skoda <- NULL
        }
        highchart <- highchart %>%
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
            name = 'analyse',
            data = list_parse(level_1_analyse),
            stack = 'analyse'
          ) %>%
          hc_add_series(
            name = 'comparaison',
            data = list_parse(level_1_comparaison),
            stack = 'comparaison'
          ) %>%
          hc_add_series(
            name = 'analyse_skoda',
            data = list_parse(level_1_analyse_skoda),
            stack = 'analyse'
          ) %>%
          hc_add_series(
            name = 'comparaison_skoda',
            data = list_parse(level_1_comparaison_skoda),
            stack = 'comparaison'
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
            series = c(level_2_analyse, level_2_comparaison, level_2_analyse_skoda, level_2_comparaison_skoda)
          )
      } else if(!comparison){
        highchart <- highchart %>%
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
            name = 'analyse',
            data = list_parse(level_1_analyse),
            stack = 'analyse'
          ) %>%
          hc_add_series(
            name = 'analyse_skoda',
            data = list_parse(level_1_analyse_skoda),
            stack = 'analyse'
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
            series = c(level_2_analyse, level_2_analyse_skoda)
          )
      }
    }
  } else if(is.null(dimension2)){
    # analyse
    bar_data_analyse <- report_analyse[, .(kpi = base::sum(.SD)), by = c(dimension1), .SDcols = kpi]
    bar_data_analyse <- bar_data_analyse[, .(name = get(dimension1), y = kpi)]
    if(repartition){
      bar_data_analyse[, y := round(y / sum(y) * 100, digits = 2)]
    }
    if(comparison){
      # comparaison
      bar_data_comparaison <- report_comparaison[, .(kpi = base::sum(.SD)), by = c(dimension1), .SDcols = kpi]
      bar_data_comparaison <- bar_data_comparaison[, .(name = get(dimension1), y = kpi)]
      if(repartition){
        bar_data_comparaison[, y := round(y / sum(y) * 100, digits = 2)]
      }
      # plot
      highchart <- highchart %>%
        hc_add_series(
          data = list_parse(bar_data_analyse),
          name = "Analyse"
        ) %>%
        hc_add_series(
          data = list_parse(bar_data_comparaison),
          name = "Comparaison"
        ) %>%
        hc_plotOptions(
          bar = list(
            showInLegend = FALSE,
            borderWidth = 0
          )
        )
    } else if(!comparison){
      highchart <- highchart %>%
        hc_add_series(
          data = list_parse(bar_data_analyse),
          name = "Analyse"
        ) %>%
        hc_plotOptions(
          bar = list(
            showInLegend = FALSE,
            borderWidth = 0
          )
        )
    }
  }
  highchart
}


# RenderFunnel ------------------------------------------------------------

#'
#' This function creates a funnel plot using renderHighchart.
#'
#' @param funnel_data the table with the data used in building the plot. Must have a proper structure.
#' @param titre the title of the plot
#' @param xAxis boolean, should x axis names be added
#' @param comparison boolean, should information on comparison period be added
#' @param skoda_bank boolean, should information on skoda bank leads be added
#' @param color the color of the plot
#'
#' @return a highchart
#'
renderFunnel <- function(funnel_data, titre = "global", xAxis = TRUE, comparison = FALSE, skoda_bank = FALSE, color = skoda_grey){
  if(xAxis){
    categories <- funnel_data$name
  } else{
    categories <- rep(' ', 4)
  }
  formatter <- JS(
    'function(){
       if(this.point.x == 0){
         return this.point.nb_indiv ;
       } else if(this.point.x == 3){
         return this.point.nb_indiv ;
       } else{
         return this.point.percent + "%";
       }
     }'
  )
  if(comparison & !skoda_bank){
    formatter_tooltip <- JS(
      'function(){
         var out = "<b>" + this.point.tooltip_name + "</b> : " + this.point.nb_indiv ;
         if(this.point.x == 0){
           out += "<br>vs " + this.point.nb_indiv_comp + " en p-1";
         } else{
           if(this.point.evol >= 0){
             var sign = "+" ;
           } else{
             var sign = "" ;
           }
           out += "<br> <b>Taux : </b>" + sign + this.point.evol + " pts vs p-1" ;
         }
         return out ;
       }'
    )
  } else if(comparison & skoda_bank){
    formatter_tooltip <- JS(
      'function(){
         var out = "<b>" + this.point.tooltip_name + "</b> : " + this.point.nb_indiv ;
         if(this.point.x == 0){
           out += "<br>dont " + this.point.skoda_bank + " skoda bank";
           out += "<br>vs " + this.point.nb_indiv_comp + " en p-1";
         } else{
           if(this.point.evol >= 0){
             var sign = "+" ;
           } else{
             var sign = "" ;
           }
           out += "<br> <b>Taux : </b>" + sign + this.point.evol + " pts vs p-1" ;
         }
         return out ;
      }'
    )
  } else if(!comparison & skoda_bank){
    formatter_tooltip <- JS(
      'function(){
        var out = "<b>" + this.point.tooltip_name + "</b> : " + this.point.nb_indiv ;
        if(this.point.x == 0){
          out += "<br>dont " + this.point.skoda_bank + " skoda bank";
        }
        return out ;
       }'
    )
  } else if(!comparison & !skoda_bank){
    formatter_tooltip <- JS(
      'function(){
         var out = "<b>" + this.point.tooltip_name + "</b> : " + this.point.nb_indiv ;
         return out ;
       }'
    )
  }
  highchart() %>%
    hc_chart(type = "bubble",
             inverted = TRUE) %>%
    hc_xAxis(
      categories = categories,
      gridLineDashStyle = "dash",
      gridLineWidth = 1.5,
      lineWidth = 0,
      tickWidth = 0,
      tickmarkPlacement = 'on',
      gridLineColor = skoda_grey,
      labels = list(
        style = list(
          color = '#ffffff',
          fontSize = '12px'
        )
      )
    ) %>%
    hc_yAxis(
      categories = c(titre, "other"),
      gridLineWidth = 0,
      lineWidth = 0,
      tickWidth = 0,
      showFirstLabel = FALSE,
      showLastLabel = FALSE,
      opposite = TRUE,
      labels = list(
        style = list(
          color = '#ffffff',
          fontSize = '23px',
          fontWeight = 'bold'
        )
      )
    ) %>%
    hc_add_series(
      data = funnel_data
    ) %>%
    hc_plotOptions(
      bubble = list(
        lineWidth = 2,
        minSize = '10%',
        maxSize = '25%',
        showInLegend = FALSE,
        marker = list(fillColor = NULL,
                      fillOpacity = 1),
        dataLabels = list(
          enabled = TRUE,
          formatter = formatter,
          style = list(textOutline = 'none'),
          inside = TRUE,
          align = "center",
          style = list(
            'text-anchor' = 'middle'
          )
        )
      )
    ) %>%
    hc_tooltip(
      useHTML = TRUE,
      formatter = formatter_tooltip
    ) %>%
    hc_add_theme(theme)
}

# RenderSpider ------------------------------------------------------------

#'
#' This function creates a spider plot from a properly structured table.
#'
#' @param plot_data the table with the data, must have correct structure
#' @param comparison, boolean, should data for a comparison period be added
#' @param plot_data_comparison the table with the data, must have correct structure. Used if comparison is set to true
#' @param title the title of the plot
#'
#' @return a spider plot
#'
renderSpider <- function(plot_data, comparison = FALSE, plot_data_comparison = NULL, title = 'Call center'){
  highchart <- highchart() %>%
    hc_title(
      text = title
    ) %>%
    hc_chart(
      type = "area",
      polar = TRUE
    ) %>%
    hc_xAxis(
      categories = plot_data$source,
      tickmarkPlacement = 'on',
      lineWidth = 0
    ) %>%
    hc_yAxis(
      gridLineInterpolation = 'polygon',
      lineWidth = 0,
      min = 0
    ) %>%
    hc_add_theme(theme)
  if(comparison){
    highchart <- highchart %>%
      hc_add_series(
        name = 'comparaison',
        data = plot_data_comparison$delai,
        color = '#9b59b6',
        pointPlacement = 'on'
      )
  }
  highchart <- highchart %>%
    hc_add_series(
      name = 'analyse',
      data = plot_data$delai,
      color = skoda_green,
      pointPlacement = 'on'
    )
  return(highchart)
}
