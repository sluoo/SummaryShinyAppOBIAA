library(shiny)
library(DT)
library(shinythemes)
library(ggplot2)
library(tidyverse)
library(readr)
library(sunburstR)

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

###Sunburst Plot 
###Grouping funding by industry
funds_grp <- (dta1 %>% 
                group_by(Industry)
              %>% summarise(`Digital Marketing`=sum(DigitalMarket),`Website`=sum(Website),
                            `Software`=sum(Software),`Digital Training`=sum(DigitalTrain),
                            `Hardware`=sum(Hardware))
              %>% gather(key,value,`Digital Marketing`:Hardware))

###Generating sequence for sunburst
dta2 <-data.frame(funds_grp
                  %>% unite(seq,Industry:key,sep = "-"))

###Plot
s2b <- (sund2b(dta2,
               rootLabel = "Total $ Value Issued",
               #showLabels = TRUE,
               colors = htmlwidgets::JS("d3.scaleOrdinal(d3.schemeCategory20b)"),
               tooltip =  sund2bTooltip(followMouse = TRUE,
                                        html = htmlwidgets::JS("function(nodedata, size, percent) {
  return '<span style=\"font-weight: bold;\">' + nodedata.name + '</span>' + ' ' + size
}
    ")
               ) 
))#end_sun2b 

##Create table along with SunBurst Plot 
tbl <- (dta1 %>% 
          group_by(Industry)
        %>% summarise(`Digital Marketing`=sum(DigitalMarket),`Website`=sum(Website),
                      `Software`=sum(Software),`Digital Training`=sum(DigitalTrain),
                      `Hardware`=sum(Hardware)))
#tbl1 <- datatable(tbl)

options(shiny.trace = TRUE)
#Define UI
ui <- fluidPage(
  theme=shinytheme("simplex"), #theme
  titlePanel("OBIAA Summary Shiny Application"), #title
  tabsetPanel(
    
    tabPanel("Basic Summary",
             HTML(#adding text 
               paste(
                 h3("This is my app!"),'<br/>',
                 h4("Download your data using the choose file button"),'<br/>',
                 h4("Thank you for using the app!")
               )#paste
             )#HTML
             ),#endTab1,
    
    tabPanel("Distribution of Funding by Industry",
             fluidRow(
              headerPanel("Breakdown of Funding by Industry"),
              column(2),
              column(8,sund2bOutput("s2b"))),
              #column(2),
              column(11, dataTableOutput("tbl1"))
              ),#endTab2,
      
    tabPanel("Distribution of Staffing",
    #Add boxplot
    fluidRow(
    headerPanel("Distribution of Staffing by Revenue and Square Footage"),
    plotOutput("boxplot"),
    column(3,
      selectInput("selectInput",
                  h4("Please select industry for analysis:"),
                  choices = list("All",
                                      "Accomodations",
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
                                 selected = "All")#checkboxGroupInput  
           
           )
    )
  )#TabPanel3
    
    
    
    

    
    
    
    )#tabsetPanel

  
)#fluidpage

#Define Server
server <- function(input,output){
  #Adding Sunburst Plot 
  output$s2b <- renderSund2b({
    add_shiny(s2b)
  })
  
  #Adding table 
  output$tbl1 <- renderDataTable({
    datatable(tbl) %>% formatCurrency(
      c("Digital Marketing","Website","Software","Digital Training","Hardware"),"$",digits = 0)
  })
  
  #Adding Boxplots 
  
  
  
  
  
  
  
  
}#server

shinyApp(ui=ui,server=server)


























