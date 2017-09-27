
# UI ----------------------------------------------------------------------

dashboardPage(
  dashboardHeader(title = "Dashboard Shiny"),
  dashboardSidebar(
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
        uiOutput(outputId = "daterange_analyse_ui"),
        materialSwitch(
          inputId = "comparison_checkbox",
          label = "Comparison period?",
          status = "primary",
          right = TRUE,
          value = TRUE
        ),
        uiOutput(outputId = "daterange_comparaison_ui"),
        materialSwitch(
          inputId = "checkbox_skodabank",
          label = "Lead skoda bank ?",
          status = "primary",
          right = TRUE,
          value = FALSE
        )
      ),
      conditionalPanel(
        condition = 'input.sidebar_menu == "table_menu"',
        br(),
        br(),
        uiOutput(outputId = "daterange_detail_ui"),
        uiOutput(outputId = 'district_detail_ui'),
        uiOutput(outputId = 'concession_detail_ui')
      )
    )
  ),
  dashboardBody(
    useShinyjs(),
    tags$script(HTML("$('body').addClass('fixed');")),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
      tags$script(type ="text/javascript", src = "busy.js"),
      tags$script(HTML("$('body').addClass('sidebar-mini');"))
    ),
    tabItems(
      tabItem(
        tabName = "globalview_menu",
        div(
          class = 'row',
          div(
            class = 'col-md-4',
            valueBoxOutput(outputId = "valuebox_brut", width = 12)
          ),
          div(
            class = 'col-md-4',
            valueBoxOutput(outputId = "valuebox_descendu", width = 12)
          ),
          div(
            class = 'col-md-4',
            valueBoxOutput(outputId = "valuebox_rdv", width = 12)
          )
        ),
        div(
          class = 'div_barchart',
          div(
            style = 'position: relative;',
            div(
              style = 'display: inline-block; vertical-align: middle;',
              dropdown(
                h3("KPI à analyser"),
                pickerInput(
                  inputId = 'kpi_barchart',
                  label = 'Variable à analyser',
                  choices = c('Lead brut' = 'lead_brut',
                              'Lead descendu' = 'descendu',
                              'Prise de RDV' = 'prise_de_rendez_vous',
                              'Commande' = 'commande',
                              'Contact concessionnaire' = 'contact_concessionnaire')
                ),
                h6('Représentation en répartition'),
                checkboxInput(
                  inputId = 'barchart_repartition',
                  label = 'Répartition',
                  value = FALSE
                ),
                p('Cliquer sur une source ou un modèle pour avoir plus de détails.'),
                style = "jelly", # simple, bordered, minimal, stretch, jelly, gradient, fill, material-circle, material-flat, pill, float, unite
                animate = animateOptions(enter = animations$fading_entrances$fadeInLeftBig,
                                         exit = animations$fading_exits$fadeOutRightBig),
                size = 'xs',
                status = "danger",
                icon = icon("gear"),
                width = "300px",
                right = FALSE,
                tooltip = tooltipOptions(title = "Conversation par modèle ou par source ?")
              )
            ),
            div(
              style = 'display: inline-block; vertical-align: top;',
              uiOutput(outputId = 'barchart_section_title')
            )
          ),
          div(
            class = "highchart_overview",
            highchartOutput(outputId = "highchart_source", width = '100%', height = '300px')
          ),
          div(
            class = "highchart_overview",
            highchartOutput(outputId = "highchart_model", width = '100%', height = '300px')
          ),
          div(
            class = "highchart_overview",
            highchartOutput(outputId = "highchart_lead", width = '100%', height = '300px')
          )
        ),
        hr(),
        div(
          class = 'div_funnel',
          div(
            style = 'position: relative;',
            div(
              style = 'display: inline-block; vertical-align: top;',
              dropdown(
                radioButtons(
                  inputId = 'funnel_group_var',
                  label = 'Variable de croisement',
                  choices = c('Modèle' = 'vehicle_name',
                              'Source' = 'source'),
                  inline = FALSE
                ),
                style = "jelly", # simple, bordered, minimal, stretch, jelly, gradient, fill, material-circle, material-flat, pill, float, unite
                animate = animateOptions(enter = animations$fading_entrances$fadeInLeftBig,
                                         exit = animations$fading_exits$fadeOutRightBig),
                size = 'xs',
                status = "danger",
                icon = icon("gear"),
                width = "300px",
                right = FALSE,
                tooltip = tooltipOptions(title = "Sélectionner le kpi à analyser")
              )
            ),
            div(
              style = 'display: inline-block; vertical-align: top;',
              HTML('<p class = "section_title"> Conversion</p>')
            )
          ),
          splitLayout(
            style = 'height: 400px',
            highchartOutput(outputId = "main_funnel", width = '100%', height = '100%'),
            div(
              style = 'overflow-x: scroll;',
              uiOutput(outputId = 'dynamic_funnel')
            ),
            cellWidths = c('25%', '72%')
          )
        ),
        br(),
        div(
          class = 'div_funnel',
          fluidRow(
            column(
              width = 6,
              div(
                style = 'border-right-style: solid; border-right-width: 2px; border-right-color: #eeeeee;',
                uiOutput(outputId = 'piechart_section_title'),
                highchartOutput(outputId = "piechart_abandon", width = '100%', height = '300px')
              )
            ),
            column(
              width = 6,
              HTML('<p class = "section_title" style = "border-bottom: 40px;"> Temps de rappel par source</p>'),
              column(
                width = 6,
                highchartOutput(outputId = "spider_callcenter", width = '100%', height = '250px')
              ),
              column(
                width = 6,
                highchartOutput(outputId = "spider_concessionnaire", width = '100%', height = '250px')
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "map_menu",
        # div(class = "busy", img(src="loading.gif")),
        fluidRow(
          column(
            width = 8,
            plotOutput(outputId = "map", width = '100%')
          ),
          column(
            width = 4,
            uiOutput(outputId = "daterange_map_ui"),
            hr(),
            p("texte texte texte ...."),
            hr(),
            radioButtons(
              inputId = "kpi_map",
              label = "Sélectionnez un kpi",
              choices = c('Leads bruts' = 'lead_brut',
                          'Leads descendus' = 'descendu',
                          'Leads traités' = 'lead_traité',
                          'Délai moyen' = 'delai_moyen',
                          'Leads bonus' = 'bonus',
                          'Leads malus en attente de traitement' = 'malus_en_attente',
                          'Leads malus traités' = 'malus_traité',
                          'Abandon premier contact' = 'abandon_avant_contact_concesssionnaire',
                          'Abandon contact concessionnaire' = 'abandon_apres_contact_concessionnaire',
                          'Commandes' = 'commande'),
              selected = 'lead_brut',
              inline = FALSE,
              width = '100%'
            )
          )
        )
      ),
      tabItem(
        tabName = 'table_menu',
        div(
          style = '
position: relative;
border-bottom-width: 2px;
border-bottom-color: #222d23;
border-bottom-style: solid;
padding: 5px;',
          div(
            style = 'display: inline-block;',
            h2('Overview')
          ),
          div(
            style = 'display: inline-block; margin-left: 30px;',
            downloadButton(outputId = 'download_overview_table', label = 'Télécharger la table')
          )
        ),
        # hr(),
        dataTableOutput(outputId = 'table_overview'),
        hr(),
        div(
          style = '
          position: relative;
          border-bottom-width: 2px;
          border-bottom-color: #222d23;
          border-bottom-style: solid;
          padding: 5px;',
          div(
            style = 'display: inline-block;',
            h2('Bonus/Malus')
          ),
          div(
            style = 'display: inline-block; margin-left: 30px;',
            downloadButton(outputId = 'download_bonusmalus_table', label = 'Télécharger la table')
          )
        ),
        dataTableOutput('table_bonusmalus'),
        hr(),
        div(
          style = '
          position: relative;
          border-bottom-width: 2px;
          border-bottom-color: #222d23;
          border-bottom-style: solid;
          padding: 5px;',
          div(
            style = 'display: inline-block;',
            h2('Bonus/malus Détails')
          ),
          div(
            style = 'display: inline-block; margin-left: 30px;',
            downloadButton(outputId = 'download_bonusmalusdetail_table', label = 'Télécharger la table')
          )
        ),
        dataTableOutput('table_bonusmalusdetail')
      )
    )
  )
)
