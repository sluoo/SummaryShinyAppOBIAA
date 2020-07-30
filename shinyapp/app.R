library(shiny)
library(shinythemes)

#Define UI 
ui <- fluidPage(theme = shinytheme("simplex"),
  titlePanel("OBBIA Summary Shiny App"),
  
  sidebarLayout(
    sidebarPanel("sidebar panel"
                 
                 ),#sidebarPanel
    
    mainPanel(
      tabsetPanel(
        tabPanel("tab1","content1"),
        tabPanel("tab2","content2")
      )#tabsetPanel
      
    )#mainPanel
  
  )#sidebarLayout

  
  
  
  
  
  
  
)#fluidPage


#Define server
server <- function(input,output){
  
}

#RunApp 
shinyApp(ui=ui,server=server)