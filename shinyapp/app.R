library(shiny)
library(shinythemes)
library(ggplot2)
library(readr)
library(tidyverse)

#Dataset 

### Raw data
DTG_All_Applicants <- read_csv("~/Documents/OBIAA_Summary/DTG-All Applicants.csv")
dta <- DTG_All_Applicants


####Clean Raw Data

#Select necessary columns for analysis
#Remove denied funding cases 
dta1 <- (dta %>% 
           select(`Business Name`,Industry, BIA, City, Municipality,  Currency, `Matching Funds`, 
                  `Annual Revenue`,`Square Feet`, Staff, Status,`Digital Marketing $`,
                  Website, Software, `Digital Training`, Hardware, DSS,Region)
         %>% mutate(Currency = gsub("[\\$,]","",Currency))
         %>% rename(
           DigitalMarket = `Digital Marketing $`,
           DigitalTrain = `Digital Training`,
           AnnualRevenue = `Annual Revenue`,
           SquareFt = `Square Feet`)
         %>% mutate(DigitalMarket = as.numeric(gsub("[\\$,]","",DigitalMarket)),
                    DigitalTrain = as.numeric(gsub("[\\$,]","",DigitalTrain)),
                    Website = as.numeric(gsub("[\\$,]","",Website)),
                    Software = as.numeric(gsub("[\\$,]","",Software)),
                    Hardware = as.numeric(gsub("[\\$,]","",Hardware)),
                    AnnualRevenue = as.factor(AnnualRevenue),
                    SquareFt = as.factor(SquareFt))
         %>% filter(Status != "Denied"))

#Add 0 to NA for funding columns 
dta1$DigitalMarket[c(which(is.na(dta1$DigitalMarket==TRUE)))] <-0
dta1$Website[c(which(is.na(dta1$Website==TRUE)))] <-0
dta1$Software[c(which(is.na(dta1$Software==TRUE)))] <-0
dta1$DigitalTrain[c(which(is.na(dta1$DigitalTrain==TRUE)))] <-0
dta1$Hardware[c(which(is.na(dta1$Hardware==TRUE)))] <-0

#Define UI 
ui <- fluidPage(theme = shinytheme("simplex"),
  titlePanel("OBIAA Summary Shiny App"),
  
  sidebarLayout(
    sidebarPanel(h4("Variables of Interest"),
      selectInput("checkGroup",
                         "Select an Industry", 
                         choices = list("Accomodations",
                                        "Art Gallery",
                                        "Beauty",
                                        "Business Services",
                                        "Cafe",
                                        "Consumer Services",
                                        "Education",
                                        "Entertainment",
                                        "Financial Services",
                                        "Healthy and Beauty",
                                        "Health and Wellness", 
                                        "Medical Services",
                                        "Music",
                                        "Recreation",
                                        "Restaurant",
                                        "Retail",
                                        "Specialty Foods"),
                         selected = "Retail")#checkboxGroupInput   
                 
         
      ),#sidebalabels()
    
    mainPanel(
      tabsetPanel(
        tabPanel("SunBurst Plot",
                 plotOutput("sunburst")),#sunburst
        tabPanel("tab1",
                 plotOutput("allIndustries")),#tab1
        
        
        tabPanel("tab2",
                 plotOutput("boxPlot"))#tab2
      ),#tabsetPanel
    

    )#mainPanel
  )#sidebarLayout
)#fluidPage


#Define server
server <- function(input,output){
  output$sunburst <- renderSund2b({
    #Use of Funds by Industry 
    funds_grp <- (dta1 %>% 
                    group_by(Industry)
                  %>% summarise(`Digital Marketing`=sum(DigitalMarket),`Website`=sum(Website),
                                `Software`=sum(Software),`Digital Training`=sum(DigitalTrain),
                                `Hardware`=sum(Hardware))
                  %>% gather(key,value,`Digital Marketing`:Hardware))
    
    dta2 <-data.frame(funds_grp
                      %>% unite(seq,Industry:key,sep = "-"))
    
                  (sund2b(dta2,
                             rootLabel = "Total $ Value Issued",
                             #showLabels = TRUE,
                             colors = htmlwidgets::JS("d3.scaleOrdinal(d3.schemeCategory20b)"),
                             tooltip =  sund2bTooltip(followMouse = TRUE,
                                                      html = htmlwidgets::JS("function(nodedata, size, percent) {
  return '<span style=\"font-weight: bold;\">' + nodedata.name + '</span>' + ' ' + size
}
    ")
                             ) 
    ))
    
  })
  
    output$allIndustries <- renderPlot({
                (ggplot(dta1)
                 + geom_boxplot(aes(x=AnnualRevenue,y=Staff))
                 + facet_grid(.~SquareFt)
                 + scale_y_continuous(trans="log2")
                 + coord_flip()
                 + theme_minimal()
                 + xlab("Annual Revenue")
                 + ylab("Number of Employees")
                 + ggtitle("Analysis for All Industries"))})
  
    output$boxPlot <- renderPlot({
    (ggplot(filter(dta1,Industry==input$checkGroup))
     + geom_boxplot(aes(x=AnnualRevenue,y=Staff))
     + facet_grid(.~SquareFt)
     + scale_y_continuous(trans="log2")
     + coord_flip()
     + theme_minimal()
     + xlab("Annual Revenue")
     + ylab("Number of Employees")
     + ggtitle(paste("Analysis for",input$checkGroup,sep = " ")))
    
    
  })
}

#RunApp 
shinyApp(ui=ui,server=server)