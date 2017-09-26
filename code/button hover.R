
# how to show a custom button when mouse hovers/enters a specific div ?

# Step 1 ------------------------------------------------------------------

# show a div when mouse enter another div

# from : https://jsfiddle.net/yhRRe/1/

# necessary packages
library("shiny")
library("shinydashboard")
library('shinyjs')

# jscode 
jscode <- '
shinyjs.showDiv = function(){
$("#spanhovering").hover(function(event) {
$("#divtoshow").css({top: event.clientY, left: event.clientX}).show();
}, function() {
$("#divtoshow").hide();
});
}
'
# ui
ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody(
    useShinyjs(),
    extendShinyjs(text = jscode),
    div(style = 'border_color: black; border-style: solid; height: 80px; cursor: pointer;',
        id = 'spanhovering'),
    div(style = 'position: absolute; display: none; background-color: yellow; height: 20px; width: 20px;',
        id = 'divtoshow')
)
)

# server
server <- function(input, output, session){
  
  onevent(event = 'mouseenter', id = 'spanhovering', expr = js$showDiv()) # see ?onevent for other event types
  session$onSessionEnded(stopApp)
  
}

shinyApp(ui = ui, server = server)


# Step 2 ------------------------------------------------------------------

# center inside parent div

# from : http://howtocenterincss.com/#contentType=div&content.width=20px&content.height=20px&horizontal=center&vertical=middle&browser.IE=11

# necessary packages
library("shiny")
library("shinydashboard")
library('shinyjs')

# jscode 
jscode <- '
shinyjs.showDiv = function(){
$("#spanhovering").hover(function(event) {
$("#divtoshow").show();
}, function() {
$("#divtoshow").hide();
});
}
'
# ui
ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody(
    useShinyjs(),
    extendShinyjs(text = jscode),
    div(style = 'border_color: black; border-style: solid; height: 80px; cursor: pointer; display: flex; justify-content: center; align-items: center;',
        id = 'spanhovering',
        div(style = 'display: none; background-color: yellow; height: 20px; width: 20px;',
            id = 'divtoshow'))
  )
)

# server
server <- function(input, output, session){
  
  onevent(event = 'mouseenter', id = 'spanhovering', expr = js$showDiv()) # see ?onevent for other event types
  session$onSessionEnded(stopApp)
  
}

shinyApp(ui = ui, server = server)


# Step 3 ------------------------------------------------------------------

# make it a button

# necessary packages
library("shiny")
library("shinydashboard")
library('shinyjs')

# jscode 
jscode <- '
shinyjs.showDiv = function(){
$("#spanhovering").hover(function(event) {
$("#divtoshow").show();
}, function() {
$("#divtoshow").hide();
});
}
'
# ui
ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody(
    useShinyjs(),
    extendShinyjs(text = jscode),
    div(style = 'border_color: black; border-style: solid; height: 80px; cursor: pointer; display: flex; justify-content: center; align-items: center;',
        id = 'spanhovering',
        div(style = 'display: none; background-color: yellow; height: 20px; width: 20px;',
            id = 'divtoshow'))
  )
)

# server
server <- function(input, output, session){
  
  onevent(event = 'mouseenter', id = 'spanhovering', expr = js$showDiv()) # see ?onevent for other event types
  session$onSessionEnded(stopApp)
  
}

shinyApp(ui = ui, server = server)