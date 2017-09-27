
options(shiny.maxRequestSize = 50*1024^2, shiny.launch.browser = TRUE)

server <- function(input, output, session){


  ###############  Functions and global variables

  # source('../parameters.R', local = TRUE)


  ###############  ReportLMT
  # reportLMT <- reactive({
  #   con <- dbConnect(MySQL(),
  #                    host = connexionSQL$HOST,
  #                    port = connexionSQL$PORT,
  #                    user = connexionSQL$USER,
  #                    password = connexionSQL$PASSWORD,
  #                    dbname = connexionSQL$DB)
  #   result <- dbSendQuery(con, "select * from reportLMT")
  #   result <- dbFetch(result)
  #   dbDisconnect(con)
  #   return(as.data.table(result))
  # })

  reportLMT <- reactive(readRDS('input/reportLMT.rds'))

  ###############  Onglet 1 : vue globale

  ###############  Inputs
  output$daterange_analyse_ui <- renderUI(
    dateRangeInput(
      inputId = "daterange_analyse",
      label = "Période d'analyse",
      start = as.Date(max(reportLMT()$leads_date), format = '%Y-%m-%d') - 6,
      end = max(reportLMT()$leads_date),
      min = min(reportLMT()$leads_date),
      max = max(reportLMT()$leads_date),
      weekstart = 1,
      language = "fr",
      separator = "à",
      format = "dd-mm-yyyy"
    )
  )

  output$daterange_comparaison_ui <- renderUI(
    if(input$comparison_checkbox){
      dateRangeInput(
        inputId = "daterange_comparaison",
        label = "Période de comparaison",
        start = as.Date(max(reportLMT()$leads_date), format = '%Y-%m-%d') - 14,
        end = as.Date(max(reportLMT()$leads_date), format = '%Y-%m-%d') - 7,
        min = min(reportLMT()$leads_date),
        max = max(reportLMT()$leads_date),
        weekstart = 1,
        language = "fr",
        separator = "à",
        format = "dd-mm-yyyy"
      )
    } else{
      NULL
    }
  )

  ###############  Table filtrée

  report_analyse <- reactive({
    req(input$daterange_analyse)
    daterange <- reportLMT()[leads_date >= input$daterange_analyse[1] & leads_date <= input$daterange_analyse[2], leads_date]
    result <- listToDF(daterange = daterange, data = reportLMT())
    # ajout variable source
    result$source <- ""
    result[substr(campagne,1,2) == "b_"]$source <- "S1"
    result[substr(campagne,1,2) == "e_"]$source <- "S2"
    result[substr(campagne,1,2) == "a_"]$source <- "S3"
    result[substr(campagne,1,4) == "evt_"]$source <- "Events"
    result[origin == "skoda_bank" | origin =="Mid-Term FS"| origin == "FDC_FS"]$origin <- "Skoda Bank ( + Mid Term)"
    result[substr(campagne,1,3) == "md_" | substr(campagne,1,4) == "fid_" |
             substr(campagne,1,4) == "tmk_" |
             stri_detect_fixed(origin, pattern ="call center") == TRUE |
             origin == "Fid_Octavia_Superb_2016"]$source <- "Direct marketing / Call Center"
    result[substr(campagne,1,2) == "s_" | substr(campagne,1,4) == "sea_" | campagne == "SEA"]$source <- "Paid search"
    result[substr(campagne,1,2) == "r_"]$source <- "S5"
    result[origin == "S4"]$source <- "S4"
    result[origin == "pilauka" & is.na(campagne)]$source <- "S6"
    result[((origin %in% c("skodaweb", "skodawebmobile", "skodaweb_concessionnaires","skodaweb_concessionnairesmobile") )
            & is.na(campagne) ) ]$source <- "S6"
    result[source == ""]$source <- "S1"
    # anonymisation
    modele <- sort(unique(result$vehicle_name))
    result$vehicle_name <- stri_replace_all_fixed(str = result$vehicle_name,
                                                  pattern = modele,
                                                  replacement = letters[1:length(modele)],
                                                  vectorize_all = FALSE)
    return(result)
  })

  report_comparaison <- reactive({
    req(input$daterange_comparaison)
    daterange <- reportLMT()[leads_date >= input$daterange_comparaison[1] & leads_date <= input$daterange_comparaison[2], leads_date]
    result <- listToDF(daterange = daterange, data = reportLMT())
    # ajout variable source
    result$source <- ""
    result[substr(campagne,1,2) == "b_"]$source <- "S1"
    result[substr(campagne,1,2) == "e_"]$source <- "S2"
    result[substr(campagne,1,2) == "a_"]$source <- "S3"
    result[substr(campagne,1,4) == "evt_"]$source <- "Events"
    result[origin == "skoda_bank" | origin =="Mid-Term FS"| origin == "FDC_FS"]$origin <- "Skoda Bank ( + Mid Term)"
    result[substr(campagne,1,3) == "md_" | substr(campagne,1,4) == "fid_" |
             substr(campagne,1,4) == "tmk_" |
             stri_detect_fixed(origin, pattern ="call center") == TRUE |
             origin == "Fid_Octavia_Superb_2016"]$source <- "Direct marketing / Call Center"
    result[substr(campagne,1,2) == "s_" | substr(campagne,1,4) == "sea_" | campagne == "SEA"]$source <- "Paid search"
    result[substr(campagne,1,2) == "r_"]$source <- "S5"
    result[origin == "S4"]$source <- "S4"
    result[origin == "pilauka" & is.na(campagne)]$source <- "S6"
    result[((origin %in% c("skodaweb", "skodawebmobile", "skodaweb_concessionnaires","skodaweb_concessionnairesmobile") )
            & is.na(campagne) ) ]$source <- "S6"
    result[source == ""]$source <- "S1"
    modele <- sort(unique(result$vehicle_name))
    result$vehicle_name <- stri_replace_all_fixed(str = result$vehicle_name,
                                                  pattern = modele,
                                                  replacement = letters[1:length(modele)],
                                                  vectorize_all = FALSE)
    return(result)
  })


  ###############  Description et objectifs
  output$description = renderUI({
    p("Une petite description du dashboard .......")
  })

  ###############  Value boxes

  # Data for value boxes
  report_analyse_summary <- reactive({
    result <- report_analyse()[, .(lead_brut,
                                   descendu,
                                   prise_rdv = prise_de_rendez_vous,
                                   abandon = abandon_avant_contact_concesssionnaire + abandon_apres_contact_concessionnaire,
                                   non_traite = descendu - contact_concessionnaire,
                                   skoda_bank = `est skoda bank`)]
    result <- colSums(result)
    return(result)
  })

  report_comparaison_summary <- reactive({
    result <- report_comparaison()[, .(lead_brut,
                                       descendu,
                                       prise_rdv = prise_de_rendez_vous,
                                       abandon = abandon_avant_contact_concesssionnaire + abandon_apres_contact_concessionnaire,
                                       non_traite = descendu - contact_concessionnaire,
                                       skoda_bank = `est skoda bank`)]
    result <- colSums(result)
    return(result)
  })

  # Value boxes
  output$valuebox_brut <- renderValueBox({
    if(input$comparison_checkbox){
      renderCustonValueBox(kpi = 'lead_brut',
                           titre = 'Leads bruts',
                           skoda_bank = input$checkbox_skodabank,
                           report_analyse_summary = report_analyse_summary(),
                           comparison = input$comparison_checkbox,
                           report_comparaison_summary = report_comparaison_summary(),
                           icon = 'address-card-o')
    } else{
      renderCustonValueBox(kpi = 'lead_brut',
                           titre = 'Leads bruts',
                           skoda_bank = input$checkbox_skodabank,
                           report_analyse_summary = report_analyse_summary(),
                           comparison = input$comparison_checkbox,
                           report_comparaison_summary = NULL,
                           icon = 'address-card-o')
    }
  })

  output$valuebox_descendu <- renderValueBox({
    if(input$comparison_checkbox){
      renderCustonValueBox(kpi = 'descendu',
                           titre = 'Leads descendus',
                           report_analyse_summary = report_analyse_summary(),
                           comparison = input$comparison_checkbox,
                           report_comparaison_summary = report_comparaison_summary(),
                           icon = 'phone')
    } else{
      renderCustonValueBox(kpi = 'descendu',
                           titre = 'Leads descendus',
                           report_analyse_summary = report_analyse_summary(),
                           comparison = input$comparison_checkbox,
                           report_comparaison_summary = NULL,
                           icon = 'phone')
    }
  })

  output$valuebox_rdv <- renderValueBox({
    if(input$comparison_checkbox){
      renderCustonValueBox(kpi = 'prise_rdv',
                           titre = 'Prises RDV',
                           report_analyse_summary = report_analyse_summary(),
                           comparison = input$comparison_checkbox,
                           report_comparaison_summary = report_comparaison_summary(),
                           icon = 'calendar-check-o')
    } else{
      renderCustonValueBox(kpi = 'prise_rdv',
                           titre = 'Prises RDV',
                           report_analyse_summary = report_analyse_summary(),
                           comparison = input$comparison_checkbox,
                           report_comparaison_summary = NULL,
                           icon = 'calendar-check-o')
    }
  })

  output$valuebox_abandons <- renderValueBox({
    if(input$comparison_checkbox){
      renderCustonValueBox(kpi = 'abandon',
                           titre = 'Abandons',
                           report_analyse_summary = report_analyse_summary(),
                           comparison = input$comparison_checkbox,
                           report_comparaison_summary = report_comparaison_summary(),
                           icon = 'close')
    } else{
      renderCustonValueBox(kpi = 'abandon',
                           titre = 'Abandons',
                           report_analyse_summary = report_analyse_summary(),
                           comparison = input$comparison_checkbox,
                           report_comparaison_summary = NULL,
                           icon = 'close')
    }
  })

  output$valuebox_nt_conces <- renderValueBox({
    if(input$comparison_checkbox){
      renderCustonValueBox(kpi = 'non_traite',
                           titre = 'Non traité',
                           report_analyse_summary = report_analyse_summary(),
                           comparison = input$comparison_checkbox,
                           report_comparaison_summary = report_comparaison_summary(),
                           icon = 'hourglass-half')
    } else{
      renderCustonValueBox(kpi = 'non_traite',
                           titre = 'Non traité',
                           report_analyse_summary = report_analyse_summary(),
                           comparison = input$comparison_checkbox,
                           report_comparaison_summary = NULL,
                           icon = 'hourglass-half')
    }
  })

  ###############  Bar charts

  ###############  Section title and dropdown menu
  output$barchart_section_title <- renderUI({
    names <- c('leads bruts' = 'lead_brut',
               'leads descendus' = 'descendu',
               'prises de RDV' = 'prise_de_rendez_vous',
               'commandes' = 'commande',
               'contacts concessionnaire' = 'contact_concessionnaire')
    HTML(paste0('<p class = "section_title">Nombre de ', names(names)[names == input$kpi_barchart], ' par ...</p>'))
  })

  output$highchart_source <- renderHighchart({
    if(input$comparison_checkbox){
      renderBarchart(kpi = input$kpi_barchart,
                     dimension1 = "source",
                     dimension2 = "campagne",
                     titre = "Source",
                     report_analyse = report_analyse(),
                     report_comparaison = report_comparaison(),
                     skoda_bank = input$checkbox_skodabank,
                     scroll = TRUE,
                     comparison = input$comparison_checkbox,
                     repartition = input$barchart_repartition)
    } else{
      renderBarchart(kpi = input$kpi_barchart,
                     dimension1 = "source",
                     dimension2 = "campagne",
                     titre = "Source",
                     report_analyse = report_analyse(),
                     report_comparaison = NULL,
                     skoda_bank = input$checkbox_skodabank,
                     scroll = TRUE,
                     comparison = input$comparison_checkbox,
                     repartition = input$barchart_repartition)
    }
  })

  output$highchart_model <- renderHighchart({
    if(input$comparison_checkbox){
      renderBarchart(kpi = input$kpi_barchart,
                      dimension1 = "vehicle_name",
                      dimension2 = "source",
                      titre = "Modèle",
                      report_analyse = report_analyse(),
                      report_comparaison = report_comparaison(),
                      skoda_bank = input$checkbox_skodabank,
                      scroll = TRUE,
                      comparison = input$comparison_checkbox,
                      repartition = input$barchart_repartition)
    } else{
      renderBarchart(kpi = input$kpi_barchart,
                      dimension1 = "vehicle_name",
                      dimension2 = "source",
                      titre = "Modèle",
                      report_analyse = report_analyse(),
                      report_comparaison = NULL,
                      skoda_bank = input$checkbox_skodabank,
                      scroll = TRUE,
                      comparison = input$comparison_checkbox,
                      repartition = input$barchart_repartition)
    }
  })

  output$highchart_lead <- renderHighchart({
    if(input$comparison_checkbox){
      renderBarchart(kpi = input$kpi_barchart,
                     dimension1 = "nature_lead",
                     titre = "Type de lead",
                     report_analyse = report_analyse(),
                     report_comparaison = report_comparaison(),
                     scroll = FALSE,
                     comparison = input$comparison_checkbox,
                     repartition = input$barchart_repartition)
    } else{
      renderBarchart(kpi = input$kpi_barchart,
                     dimension1 = "nature_lead",
                     titre = "Type de lead",
                     report_analyse = report_analyse(),
                     report_comparaison = NULL,
                     scroll = FALSE,
                     comparison = input$comparison_checkbox,
                     repartition = input$barchart_repartition)
    }
  })


  ###############  Funnel

  output$main_funnel <- renderHighchart({
    if(!input$comparison_checkbox){
      funnel_data <- report_analyse()[, .(lead_brut = sum(lead_brut), descendu = sum(descendu), contact_concessionnaire = sum(contact_concessionnaire),
                                          commande = sum(commande), skoda_bank = sum(`est skoda bank`))]
      funnel_data <- funnel_data[, .(name = c('Lead brut', 'Taux de descente', 'Taux de traitement', 'Taux de commande'),
                                     tooltip_name = c('Lead brut', 'Lead descendu', 'Lead traité', 'Commande'),
                                     x = 0:3,
                                     y = 0,
                                     z = c(round(lead_brut / lead_brut * 100, digits = 0),
                                           round(descendu / lead_brut * 100, digits = 0),
                                           round(contact_concessionnaire / lead_brut * 100, digits = 0),
                                           round(commande / lead_brut * 100, digits = 0)),
                                     percent = c(round(lead_brut / lead_brut * 100, digits = 0),
                                                 round(descendu / lead_brut * 100, digits = 0),
                                                 round(contact_concessionnaire / descendu * 100, digits = 0),
                                                 round(commande / contact_concessionnaire * 100, digits = 0)),
                                     nb_indiv = c(lead_brut, descendu, contact_concessionnaire, commande),
                                     skoda_bank)]
      renderFunnel(funnel_data = funnel_data, comparison = FALSE, skoda_bank = input$checkbox_skodabank)
    } else{
      funnel_data <- report_analyse()[, .(lead_brut = sum(lead_brut), descendu = sum(descendu), contact_concessionnaire = sum(contact_concessionnaire),
                                          commande = sum(commande), skoda_bank = sum(`est skoda bank`))]
      funnel_data <- funnel_data[, .(name = c('Lead brut', 'Taux de descente', 'Taux de traitement', 'Taux de commande'),
                                     tooltip_name = c('Lead brut', 'Lead descendu', 'Lead traité', 'Commande'),
                                     x = 0:3,
                                     y = 0,
                                     z = c(round(lead_brut / lead_brut * 100, digits = 0),
                                           round(descendu / lead_brut * 100, digits = 0),
                                           round(contact_concessionnaire / lead_brut * 100, digits = 0),
                                           round(commande / lead_brut * 100, digits = 0)),
                                     percent = c(round(lead_brut / lead_brut * 100, digits = 0),
                                                 round(descendu / lead_brut * 100, digits = 0),
                                                 round(contact_concessionnaire / descendu * 100, digits = 0),
                                                 round(commande / contact_concessionnaire * 100, digits = 0)),
                                     nb_indiv = c(lead_brut, descendu, contact_concessionnaire, commande),
                                     skoda_bank)]
      funnel_data_comparaison <- report_comparaison()[, .(lead_brut = sum(lead_brut), descendu = sum(descendu), contact_concessionnaire = sum(contact_concessionnaire),
                                                          commande = sum(commande))]
      funnel_data_comparaison <- funnel_data_comparaison[, .(percent = c(round(lead_brut / lead_brut * 100, digits = 0),
                                                                         round(descendu / lead_brut * 100, digits = 0),
                                                                         round(contact_concessionnaire / descendu * 100, digits = 0),
                                                                         round(commande / contact_concessionnaire * 100, digits = 0)),
                                                             nb_indiv = lead_brut)]
      funnel_data[, evol := percent - funnel_data_comparaison$percent]
      funnel_data[, nb_indiv_comp := funnel_data_comparaison$nb_indiv]
      renderFunnel(funnel_data = funnel_data, comparison = TRUE, skoda_bank = input$checkbox_skodabank)
    }
  })

  ###############  Dynamic funnel

  output$dynamic_funnel <- renderUI({
    if(input$comparison_checkbox){
      model_list <- unique(report_analyse()[[input$funnel_group_var]])
      length_model <- length(model_list)
      highchartOutput_list <- lapply(X = seq_len(length_model),
                                     FUN = function(i){
                                       renderHighchart({
                                         funnel_data <- report_analyse()[get(input$funnel_group_var) == model_list[i],
                                                                         .(lead_brut = sum(lead_brut),
                                                                           descendu = sum(descendu),
                                                                           contact_concessionnaire = sum(contact_concessionnaire),
                                                                           commande = sum(commande),
                                                                           skoda_bank = sum(`est skoda bank`))]
                                         funnel_data <- funnel_data[, .(name = c('Lead brut', 'Taux de descente', 'Taux de traitement', 'Taux de commande'),
                                                                        tooltip_name = c('Lead brut', 'Lead descendu', 'Lead traité', 'Commande'),
                                                                        x = 0:3,
                                                                        y = 0,
                                                                        z = c(round(lead_brut / lead_brut * 100, digits = 0),
                                                                              round(descendu / lead_brut * 100, digits = 0),
                                                                              round(contact_concessionnaire / lead_brut * 100, digits = 0),
                                                                              round(commande / lead_brut * 100, digits = 0)),
                                                                        percent = c(round(lead_brut / lead_brut * 100, digits = 0),
                                                                                    round(descendu / lead_brut * 100, digits = 0),
                                                                                    round(contact_concessionnaire / descendu * 100, digits = 0),
                                                                                    round(commande / contact_concessionnaire * 100, digits = 0)),
                                                                        nb_indiv = c(lead_brut, descendu, contact_concessionnaire, commande),
                                                                        skoda_bank)]
                                         funnel_data_comparaison <- report_comparaison()[get(input$funnel_group_var) == model_list[i],
                                                                         .(lead_brut = sum(lead_brut),
                                                                           descendu = sum(descendu),
                                                                           contact_concessionnaire = sum(contact_concessionnaire),
                                                                           commande = sum(commande))]
                                         funnel_data_comparaison <- funnel_data_comparaison[, .(percent = c(round(lead_brut / lead_brut * 100, digits = 0),
                                                                                    round(descendu / lead_brut * 100, digits = 0),
                                                                                    round(contact_concessionnaire / descendu * 100, digits = 0),
                                                                                    round(commande / contact_concessionnaire * 100, digits = 0)),
                                                                        nb_indiv = lead_brut)]
                                         funnel_data[, evol := percent - funnel_data_comparaison$percent]
                                         funnel_data[, nb_indiv_comp := funnel_data_comparaison$nb_indiv]
                                         renderFunnel(funnel_data = funnel_data, titre = model_list[i], xAxis = FALSE, skoda_bank = input$checkbox_skodabank,
                                                      color = skoda_green, comparison = TRUE)
                                       })
                                     }
      )
      highchartOutput_list <- c(cellWidths = '250px', highchartOutput_list)
      do.call(splitLayout, highchartOutput_list)
    } else{
      model_list <- unique(report_analyse()[[input$funnel_group_var]])
      length_model <- length(model_list)
      highchartOutput_list <- lapply(X = seq_len(length_model),
                                     FUN = function(i){
                                       renderHighchart({
                                         funnel_data <- report_analyse()[get(input$funnel_group_var) == model_list[i],
                                                                         .(lead_brut = sum(lead_brut),
                                                                           descendu = sum(descendu),
                                                                           contact_concessionnaire = sum(contact_concessionnaire),
                                                                           commande = sum(commande),
                                                                           skoda_bank = sum(`est skoda bank`))]
                                         funnel_data <- funnel_data[, .(name = c('Lead brut', 'Taux de descente', 'Taux de traitement', 'Taux de commande'),
                                                                        tooltip_name = c('Lead brut', 'Lead descendu', 'Lead traité', 'Commande'),
                                                                        x = 0:3,
                                                                        y = 0,
                                                                        z = c(round(lead_brut / lead_brut * 100, digits = 0),
                                                                              round(descendu / lead_brut * 100, digits = 0),
                                                                              round(contact_concessionnaire / lead_brut * 100, digits = 0),
                                                                              round(commande / lead_brut * 100, digits = 0)),
                                                                        percent = c(round(lead_brut / lead_brut * 100, digits = 0),
                                                                                    round(descendu / lead_brut * 100, digits = 0),
                                                                                    round(contact_concessionnaire / descendu * 100, digits = 0),
                                                                                    round(commande / contact_concessionnaire * 100, digits = 0)),
                                                                        nb_indiv = c(lead_brut, descendu, contact_concessionnaire, commande),
                                                                        skoda_bank)]
                                         renderFunnel(funnel_data = funnel_data, titre = model_list[i], xAxis = FALSE, skoda_bank = input$checkbox_skodabank,
                                                      color = skoda_green, comparison = FALSE)
                                       })
                                     }
      )
      highchartOutput_list <- c(cellWidths = '250px', highchartOutput_list)
      do.call(splitLayout, highchartOutput_list)
    }
  })


  ###############  Pie chart

  output$piechart_section_title <- renderUI({
    avant <- sum(report_analyse()$abandon_avant_contact_concesssionnaire) /
      sum(report_analyse()$abandon_avant_contact_concesssionnaire, report_analyse()$abandon_apres_contact_concessionnaire)
    avant <- round(avant * 100, digits = 1)
    HTML(paste0('<p class = "section_title">Abandon : ', avant, '% avant contact concessionnaire</p>'))
  })

  output$piechart_abandon <- renderHighchart({
    pie_data <- report_analyse()[, .('Achat annulé' = `abandon - achat annulé`,
                                     'Achat concurrence' = `abandon - achat concurrence`,
                                     'Injoignable' = `abandon - injoignable répondeur`,
                                     'Mauvaise qualité' = `abandon - mauvaise qualité`)]
    pie_data_values <- sort(colSums(pie_data), decreasing = TRUE)
    pie_data <- data.table(
      name = names(pie_data_values),
      y = pie_data_values
    ) %>%
      list_parse
    highchart() %>%
      hc_chart(
        type = 'pie',
        style = list(fontFamily = 'Century Gothic, sans-serif')
      ) %>%
      hc_title(
        text = 'Répartition des abandons'
      ) %>%
      hc_plotOptions(
        series = list(
          showInLegend = TRUE,
          borderWidth = 0,
          dataLabels = list(
            enabled = FALSE
          )
        )
      ) %>%
      hc_add_series(
        name = "Type d'abandon",
        data = pie_data
      ) %>%
      hc_add_theme(theme)
  })

  ###############  Spider chart
  output$spider_callcenter <- renderHighchart({
    plot_data <- report_analyse()[, .(delai = mean(delai_moyen_traitement_callcenter)), by = source]
    spider <- renderSpider(plot_data = plot_data, comparison = FALSE)
    if(input$comparison_checkbox){
      plot_data_comparison <- report_comparaison()[, .(delai = mean(delai_moyen_traitement_callcenter)), by = source]
      spider <- renderSpider(plot_data = plot_data, comparison = TRUE, plot_data_comparison = plot_data_comparison)
    }
    spider
  })

  output$spider_concessionnaire <- renderHighchart({
    plot_data <- report_analyse()[, .(delai = mean(delai_moyen_traitement_concessionnaire)), by = source]
    spider <- renderSpider(plot_data = plot_data, comparison = FALSE)
    if(input$comparison_checkbox){
      plot_data_comparison <- report_comparaison()[, .(delai = mean(delai_moyen_traitement_concessionnaire)), by = source]
      spider <- renderSpider(plot_data = plot_data, comparison = TRUE, plot_data_comparison = plot_data_comparison, title = 'Concessionaire')
    }
    spider
  })


  ###############  Onglet 2 : vue district

  ###############  Inputs

  output$daterange_map_ui <- renderUI(
    dateRangeInput(
      inputId = "daterange_map",
      label = "Période d'analyse",
      start = as.Date(max(reportLMT()$leads_date), format = '%Y-%m-%d') - 6,
      end = max(reportLMT()$leads_date),
      min = min(reportLMT()$leads_date),
      max = max(reportLMT()$leads_date),
      weekstart = 1,
      language = "fr",
      separator = "à",
      format = "dd-mm-yyyy"
    )
  )

  ###############  Data
  map_data <- reactive({
    req(input$daterange_map)
    daterange <- reportLMT()[leads_date >= input$daterange_map[1] & leads_date <= input$daterange_map[2], leads_date]
    result <- listToDF(daterange = daterange, data = reportLMT())
    # ajout variable source
    result$source <- ""
    result[substr(campagne,1,2) == "b_"]$source <- "S1"
    result[substr(campagne,1,2) == "e_"]$source <- "S2"
    result[substr(campagne,1,2) == "a_"]$source <- "S3"
    result[substr(campagne,1,4) == "evt_"]$source <- "Events"
    result[origin == "skoda_bank" | origin =="Mid-Term FS"| origin == "FDC_FS"]$origin <- "Skoda Bank ( + Mid Term)"
    result[substr(campagne,1,3) == "md_" | substr(campagne,1,4) == "fid_" |
             substr(campagne,1,4) == "tmk_" |
             stri_detect_fixed(origin, pattern ="call center") == TRUE |
             origin == "Fid_Octavia_Superb_2016"]$source <- "Direct marketing / Call Center"
    result[substr(campagne,1,2) == "s_" | substr(campagne,1,4) == "sea_" | campagne == "SEA"]$source <- "Paid search"
    result[substr(campagne,1,2) == "r_"]$source <- "S5"
    result[origin == "S4"]$source <- "S4"
    result[origin == "pilauka" & is.na(campagne)]$source <- "S6"
    result[((origin %in% c("skodaweb", "skodawebmobile", "skodaweb_concessionnaires","skodaweb_concessionnairesmobile") )
            & is.na(campagne) ) ]$source <- "S6"
    result[source == ""]$source <- "S1"
    return(result)
  })

  ###############  Map

  district_shape <- reactive(readRDS('input/district_shape.rds'))

  centroid_district <- reactive(readRDS('input/centroid_district.rds'))

  base_map <- reactive({
    ggplot(data = district_shape(),
                     mapping = aes(x = long, y = lat, group = group)) +
    coord_map() +
    labs(x = " ", y = " ") +
    theme_bw() +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          panel.border = element_blank(),
          legend.text = element_text(size = 20),
          legend.title = element_blank(),
          legend.position = 'bottom',
          legend.key.size = unit(2, 'cm'),
          panel.background = element_rect(fill = "transparent", colour = NA),
          plot.background = element_rect(fill = "transparent", colour = NA))
  })

  output$map <- renderPlot({
    low <- 'white'
    high <- skoda_green
    if(input$kpi_map == 'lead_brut'){
      report <- map_data()[, .(kpi = sum(lead_brut, na.rm = TRUE)), by = district]
      report[, taux := paste0(round(kpi / sum(kpi) * 100, digits = 0), '%')]
    } else if(input$kpi_map == 'descendu'){
      report <- map_data()[, .(kpi = sum(descendu, na.rm = TRUE),
                               taux = paste0(round(sum(descendu, na.rm = TRUE) / sum(lead_brut, na.rm = TRUE) * 100, digits = 0), '%')), by = district]
    } else if(input$kpi_map == 'malus_en_attente'){
      report <- map_data()[, .(kpi = sum(malus_en_attente, na.rm = TRUE),
                               taux = paste0(round(sum(malus_en_attente, na.rm = TRUE) / sum(concerne_bonus_malus, na.rm = TRUE) * 100, digits = 0), '%')), by = district]
    } else if(input$kpi_map == 'lead_traité'){
      report <- map_data()[, .(bonus = sum(bonus, na.rm = TRUE),
                               attente = sum(malus_en_attente, na.rm = TRUE),
                               traite = sum(`malus traité`, na.rm = TRUE),
                               descendu = sum(descendu, na.rm = TRUE)), by = district]
      report <- report[, .(district, kpi = bonus + attente + traite,
                           taux = paste0(round((bonus + attente + traite) / descendu * 100, digits = 0), '%'))]
    } else if(input$kpi_map == 'malus_traité'){
      report <- map_data()[, .(kpi = sum(`malus traité`, na.rm = TRUE),
                               taux = paste0(round(sum(`malus traité`, na.rm = TRUE) / sum(concerne_bonus_malus, na.rm = TRUE) * 100, digits = 0), '%')), by = district]
    } else if(input$kpi_map == 'delai_moyen'){
      report <- map_data()[, .(kpi = mean(delai_moyen_traitement_callcenter, na.rm = TRUE),
                               taux = ''), by = district]
    } else if(input$kpi_map == 'bonus'){
      report <- map_data()[, .(kpi = sum(bonus, na.rm = TRUE),
                               concerne = sum(concerne_bonus_malus, na.rm = TRUE)), by = district]
      report[, taux := paste0(round(kpi / concerne * 100, digits = 0), '%')]
    } else if(input$kpi_map == 'abandon_avant_contact_concesssionnaire'){
      report <- map_data()[, .(kpi = sum(abandon_avant_contact_concesssionnaire, na.rm = TRUE),
                               taux = paste0(round(sum(abandon_avant_contact_concesssionnaire, na.rm = TRUE) / sum(lead_brut, na.rm = TRUE) * 100, digits = 0), '%')),
                           by = district]
    } else if(input$kpi_map == 'abandon_apres_contact_concessionnaire'){
      report <- map_data()[, .(kpi = sum(abandon_apres_contact_concessionnaire, na.rm = TRUE),
                               taux = paste0(round(sum(abandon_apres_contact_concessionnaire, na.rm = TRUE) / sum(lead_brut, na.rm = TRUE) * 100, digits = 0), '%')),
                           by = district]
    } else if(input$kpi_map == 'commande'){
      report <- map_data()[, .(kpi = sum(commande, na.rm = TRUE),
                               taux = ''),
                           by = district]
    }
    district_shape <- merge(x = district_shape(),
                            y = report,
                            by.x = 'id',
                            by.y = 'district',
                            all.x = TRUE,
                            sort = FALSE)
    setorderv(district_shape, cols = 'order')
    centroid_district <- merge(x = centroid_district(),
                               y = report,
                               by.x = 'district_id',
                               by.y = 'district',
                               all.x = TRUE)
    centroid_district[, kpi := round(kpi, digits = 1)]
    base_map() +
      geom_polygon(data = district_shape, mapping = aes(fill = kpi)) +
      geom_path(colour = 'black') +
      scale_fill_gradient(low = low, high = high) +
      geom_text(data = centroid_district,
                mapping = aes(x = long, y = lat, group = district_id, label = kpi),
                size = 9,
                lineheight = 0.7) +
      geom_text(data = centroid_district,
                mapping = aes(x = long, y = lat, group = district_id, label = taux),
                size = 5,
                vjust = 2)
  })


  ###############  Onglet 3 : vue detail

  ###############  Tables brutes
  # bonus_malus_sql <- reactive({
  #   con <- dbConnect(MySQL(),
  #                    host = connexionSQL$HOST,
  #                    port = connexionSQL$PORT,
  #                    user = connexionSQL$USER,
  #                    password = connexionSQL$PASSWORD,
  #                    dbname = connexionSQL$DB)
  #   bonus_malus <- dbSendQuery(con, "select * from exportBonusMalus")
  #   bonus_malus <- dbFetch(bonus_malus)
  #   dbDisconnect(con)
  #   return(as.data.table(bonus_malus))
  # })
  
  bonus_malus_sql <- reactive(readRDS('input/export_bonus_malus.rds'))

  # bonus_malus_detail_sql <- reactive({
  #   con <- dbConnect(MySQL(),
  #                    host = connexionSQL$HOST,
  #                    port = connexionSQL$PORT,
  #                    user = connexionSQL$USER,
  #                    password = connexionSQL$PASSWORD,
  #                    dbname = connexionSQL$DB)
  #   bonus_malus_detail <- dbSendQuery(con, "select * from exportBonusMalusDetails")
  #   bonus_malus_detail <- dbFetch(bonus_malus_detail)
  #   dbDisconnect(con)
  #   return(as.data.table(bonus_malus_detail))
  # })
  
  bonus_malus_detail_sql <- reactive(readRDS('input/export_bonus_malus_details.rds'))

  ###############  Daterange input
  output$daterange_detail_ui <- renderUI({
    min <- min(reportLMT()$leads_date, bonus_malus_sql()$leads_date, bonus_malus_detail_sql()$leads_date)
    max <- max(reportLMT()$leads_date, bonus_malus_sql()$leads_date, bonus_malus_detail_sql()$leads_date)
    dateRangeInput(
      inputId = "daterange_detail",
      label = "Période d'analyse",
      start = as.Date(max, format = '%Y-%m-%d') - 6,
      end = max,
      min = min,
      max = max,
      weekstart = 1,
      language = "fr",
      separator = "à",
      format = "dd-mm-yyyy"
    )
  })

  ###############  Tidy tables
  overview_table <- reactive({
    req(input$daterange_detail)
    daterange <- reportLMT()[leads_date >= input$daterange_detail[1] & leads_date <= input$daterange_detail[2], leads_date]
    result <- listToDF(daterange = daterange, data = reportLMT())
    # ajout variable source
    result$source <- ""
    result[substr(campagne,1,2) == "b_"]$source <- "S1"
    result[substr(campagne,1,2) == "e_"]$source <- "S2"
    result[substr(campagne,1,2) == "a_"]$source <- "S3"
    result[substr(campagne,1,4) == "evt_"]$source <- "Events"
    result[origin == "skoda_bank" | origin =="Mid-Term FS"| origin == "FDC_FS"]$origin <- "Skoda Bank ( + Mid Term)"
    result[substr(campagne,1,3) == "md_" | substr(campagne,1,4) == "fid_" |
             substr(campagne,1,4) == "tmk_" |
             stri_detect_fixed(origin, pattern ="call center") == TRUE |
             origin == "Fid_Octavia_Superb_2016"]$source <- "Direct marketing / Call Center"
    result[substr(campagne,1,2) == "s_" | substr(campagne,1,4) == "sea_" | campagne == "SEA"]$source <- "Paid search"
    result[substr(campagne,1,2) == "r_"]$source <- "S5"
    result[origin == "S4"]$source <- "S4"
    result[origin == "pilauka" & is.na(campagne)]$source <- "S6"
    result[((origin %in% c("skodaweb", "skodawebmobile", "skodaweb_concessionnaires","skodaweb_concessionnairesmobile") )
            & is.na(campagne) ) ]$source <- "S6"
    result[source == ""]$source <- "S1"
    return(result)
  })

  bonus_malus <- reactive({
    req(input$daterange_detail)
    daterange <- bonus_malus_sql()[leads_date >= input$daterange_detail[1] & leads_date <= input$daterange_detail[2], leads_date]
    result <- listToDF(daterange = daterange, data = bonus_malus_sql(), origin = 'bonusMalus')
    return(result)
  })

  bonus_malus_detail <- reactive({
    req(input$daterange_detail)
    daterange <- bonus_malus_detail_sql()[leads_date >= input$daterange_detail[1] & leads_date <= input$daterange_detail[2], leads_date]
    result <- listToDF2(daterange = daterange, data = bonus_malus_detail_sql())
    return(result)
  })

  ###############  District input
  output$district_detail_ui <- renderUI({
    choices <- mixedsort(unique(bonus_malus()$district, bonus_malus_detail()$District))
    names(choices) <- paste0('D', choices)
    selectInput(
      inputId = 'district_detail',
      label = 'Sélectionner le district',
      choices = c('Tous', choices),
      multiple = FALSE
    )
  })

  # Table overview ---
  output$table_overview <- renderDataTable({
    output <- overview_table()[, .(Date = date,
                                   District = district,
                                   Concession = concession,
                                   Origine = origin,
                                   Source = source,
                                   Campagne = campagne,
                                   'Véhicule' = vehicle_name,
                                   'Achat annulé' = `abandon - achat annulé`,
                                   'Achat concurrence' = `abandon - achat concurrence`,
                                   'Injoignable' = `abandon - injoignable répondeur`,
                                   'Mauvaise qualité' = `abandon - mauvaise qualité`,
                                   'Avant concessionnaire' = abandon_avant_contact_concesssionnaire,
                                   'Après concessionnaire' = abandon_apres_contact_concessionnaire,
                                   Bonus = bonus,
                                   Commande = commande,
                                   'Bonus/Malus' = concerne_bonus_malus,
                                   'Contact concessionnaire' = contact_concessionnaire,
                                   'Callcenter' = delai_moyen_traitement_callcenter,
                                   'Concessionnaire' = delai_moyen_traitement_concessionnaire,
                                   'Lead descendu' = descendu,
                                   'Lead brut' = lead_brut,
                                   'Malus traité' = `malus traité`,
                                   'Malus en attente' = malus_en_attente,
                                   'Rendez-vous' = prise_de_rendez_vous,
                                   'Relance' = relance_a_effectuer)]
    datatable(
      output,
      options = list(scrollX = TRUE, scrollY = TRUE, ordering = TRUE, pageLength = 10, dom = 'tp', autoWidth = TRUE,
                     columnDefs = list(list(width = '70px', targets = 0))),
      rownames = FALSE,
      class = 'S1'
    )
  }, server = FALSE)

  output$download_overview_table <- downloadHandler(
    filename = function(){
      "overview.csv"
    },
    content = function(file){
      fwrite(x = overview_table(), file = file, sep = ";")
    }
  )

  ###############  Bonus/Malus

  ###############  Table Bonus/Malus aggrégée
  bonus_malus_agg <- reactive({
    req(input$district_detail)
    if(input$district_detail != 'Tous'){
      result <- bonus_malus()[district == input$district_detail,
                              lapply(.SD, sum),
                              .SDcols = names(bonus_malus())[-(1:4)],
                              by = concession]
    } else{
      result <- bonus_malus()[,
                              lapply(.SD, sum),
                              .SDcols = names(bonus_malus())[-(1:4)],
                              by = district]
    }
    return(result)
  })

  output$table_bonusmalus <- renderDataTable({
    output <- bonus_malus_agg()
    if(input$district_detail == 'Tous'){
      names(output)[1] <- 'District'
      output$District <- paste0('D', output$District)
    } else{
      names(output)[1] <- 'Concession'
    }
    datatable(output,
              options = list(scrollX = TRUE, scrollY = TRUE, ordering = TRUE, pageLength = 10, dom = 'tp', autoWidth = TRUE,
                             columnDefs = list(list(width = '70px', targets = 0))),
              rownames = FALSE) %>%
      formatStyle(columns = 2,
                  background = styleColorBar(as.numeric(range(bonus_malus_agg()[,2])), 'lightblue'),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center') %>%
      formatStyle(columns = 3,
                  background = styleColorBar(as.numeric(range(bonus_malus_agg()[,3])), 'lightblue'),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center') %>%
      formatStyle(columns = 4,
                  background = styleColorBar(as.numeric(range(bonus_malus_agg()[,4])), 'lightblue'),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center') %>%
      formatStyle(columns = 5,
                  background = styleColorBar(as.numeric(range(bonus_malus_agg()[,5])), 'lightblue'),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center') %>%
      formatStyle(columns = 6,
                  background = styleColorBar(as.numeric(range(bonus_malus_agg()[,6])), 'lightblue'),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center') %>%
      formatStyle(columns = 7,
                  background = styleColorBar(as.numeric(range(bonus_malus_agg()[,7])), 'lightblue'),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center') %>%
      formatStyle(columns = 8,
                  background = styleColorBar(as.numeric(range(bonus_malus_agg()[,8])), 'lightblue'),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center') %>%
      formatStyle(columns = 9,
                  background = styleColorBar(as.numeric(range(bonus_malus_agg()[,9])), 'lightblue'),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center') %>%
      formatStyle(columns = 10,
                  background = styleColorBar(as.numeric(range(bonus_malus_agg()[,10])), 'lightblue'),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center') %>%
      formatStyle(columns = 11,
                  background = styleColorBar(as.numeric(range(bonus_malus_agg()[,11])), 'lightblue'),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center')
  }, server = TRUE)

  output$download_bonusmalus_table <- downloadHandler(
    filename = function(){
      "bonusmalus.csv"
    },
    content = function(file){
      fwrite(x = bonus_malus_agg(), file = file, sep = ";")
    }
  )

  ###############  Bonus Malus Détails

  ###############  Table BonusMalusDetail filtrée
  bonus_malus_detail_filtre <- reactive({
    req(input$district_detail)
    result <- bonus_malus_detail()
    if(input$district_detail != 'Tous'){
      result <- bonus_malus_detail()[District == paste0('D', input$district_detail), ]
    }
    return(result)
  })

  ###############  Inputs : concession
  output$concession_detail_ui <- renderUI({
    selectInput(
      inputId = 'concession_detail',
      label = 'Sélectionner la concession',
      choices = c('Toutes', mixedsort(unique(bonus_malus_detail_filtre()$Concession))),
      multiple = FALSE
    )
  })

  ###############  Output table
  bonusmalusdetail_output <- reactive({
    result <- bonus_malus_detail_filtre()
    if(input$concession_detail != 'Toutes'){
      result <- bonus_malus_detail_filtre()[Concession == input$concession_detail, ]
    }
    return(result)
  })

  output$table_bonusmalusdetail <- renderDataTable({
    bonusmalusdetail_output()
  }, options = list(scrollX = TRUE, scrollY = TRUE, ordering = TRUE, pageLength = 10, dom = 'tp', autoWidth = TRUE,
                    columnDefs = list(list(width = '70px', targets = 0),
                                      list(width = '120px', targets = c(2, 3, 6, 14, 17, 18, 19, 20))),
                    ajax = list(serverSide = TRUE, processing = TRUE)),
  server = TRUE, rownames = FALSE)

  output$download_bonusmalusdetail_table <- downloadHandler(
    filename = function(){
      "bonusmalusdetail.csv"
    },
    content = function(file){
      fwrite(x = bonusmalusdetail_output(), file = file, sep = ";")
    }
  )


  ###############  Closing session

  session$onSessionEnded(stopApp)

}