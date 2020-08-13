library(shiny)
library(DT)
library(shinythemes)
library(ggplot2)
library(tidyverse)
library(tibble)
library(readr)
library(sunburstR)
library(plotly)
library(dplyr)


### Raw data
DTG_All_Applicants <- read_csv("DTG-All Applicants.csv")
dta <- DTG_All_Applicants

####Clean Raw Data

#Select necessary columns for analysis
#Remove denied funding cases 
dta1 <- (dta %>% 
           select(`Business Name`,Industry, BIA, City, Municipality,  Currency, `Matching Funds`, 
                  `Annual Revenue`,`Square Feet`, Staff, Status,`Digital Marketing $`,
                  Website, Software, `Digital Training`, Hardware, DSS,Region)
         %>% mutate(Currency = gsub("[\\$,]","",Currency))
         %>% rename(DigitalMarket = `Digital Marketing $`,
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

#Re-Code and Re-Order AnnualRevenue and SquareFoot
dta1 <- mutate(dta1, AnnualRevenue=fct_relevel(dta1$AnnualRevenue, c(
  "Up to $500,000",
  "$500,000 to $1 Million",
  "$1 Million to $1.5 Million",
  "$1.5 Million to $2 Million",
  "$2 Million to $3 Million",
  "$3 Million to $5 Million",
  "$5 Million to $7 Million",
  "$7 Million to $10 Million",
  "$15 Million to $20 Million")))

dta1 <- mutate(dta1, SquareFt=fct_relevel(dta1$SquareFt,c(
  "500 - 1000 Square Ft",
  "1001 - 2000 Square Ft",
  "2001 - 3000 Square Ft",
  "3000 + Square Ft")))

##Relabel
#Relabel 
#dta1$AnnualRevenue <-mapvalues(dta1$AnnualRevenue,
                               # from=c("Up to $500,000",
                               #        "$500,000 to $1 Million",
                               #        "$1 Million to $1.5 Million",
                               #        "$1.5 Million to $2 Million",
                               #        "$2 Million to $3 Million",
                               #        "$3 Million to $5 Million",
                               #        "$5 Million to $7 Million",
                               #        "$7 Million to $10 Million",
                               #        "$15 Million to $20 Million"),
                               # to = c("Up to $500K" = "Up to $500,000",
                               #        "$500K to $1M" = "$500,000 to $1 Million",
                               #        "$1M to $1.5M" = "$1 Million to $1.5 Million",
                               #        "$1.5M to $2M" = "$1.5 Million to $2 Million",
                               #        "$2M to $3M" = "$2 Million to $3 Million",
                               #        "$3M to $5M" = "$3 Million to $5 Million",
                               #        "$5M to $7M"="$5 Million to $7 Million",
                               #        "$7M to $10M"="$7 Million to $10 Million" ,
                               #        "$15M to $20M"= "$15 Million to $20 Million"))


dta1 <- (dta1 %>% mutate(AnnualRevenue = (dta1$AnnualRevenue %>% 
                                            fct_recode("Up to $500K" = "Up to $500,000",
                                                       "$500K to $1M" = "$500,000 to $1 Million",
                                                       "$1M to $1.5M" = "$1 Million to $1.5 Million",
                                                       "$1.5M to $2M" = "$1.5 Million to $2 Million",
                                                       "$2M to $3M" = "$2 Million to $3 Million",
                                                       "$3M to $5M" = "$3 Million to $5 Million",
                                                       "$5M to $7M"="$5 Million to $7 Million",
                                                       "$7M to $10M"="$7 Million to $10 Million" ,
                                                       "$15M to $20M"= "$15 Million to $20 Million"))))


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

###########Define UI
ui <- fluidPage(
  theme=shinytheme("flatly"), #theme
  titlePanel(h1("Ontario Digital Main Street Summary Analysis")), #title
  tabsetPanel(
    # tabPanel("Basic Summary",
    #          HTML(#adding text 
    #            paste(
    #              h3("Distribution of Staff"),'<br/>',
    #              h4("Download your data using the choose file button"),'<br/>',
    #              h4("Thank you for using the app!")
    #            )#paste
    #          )#HTML
    #          ),#endTab1,
    
    tabPanel("Distribution of Funding by Industry",
             fluidRow(
              headerPanel("Breakdown of Funding by Industry"),
              column(2),
              column(8,sund2bOutput("s2b"))),
              #column(2),
              column(11, dataTableOutput("tbl1"))
              ),#endTab2,
      
    tabPanel("Distribution of Staff Part 1",
    #Add boxplot
    fluidRow(
    headerPanel("Distribution of Staffing by Revenue and Square Footage"),
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
    ),
    fluidRow(
      column(11,offset = 0.75,
        plotlyOutput("boxplot"))
    ),
    fluidRow(
    column(3,
    numericInput("num","Convert Log Scale to Linear Scale:",value = 0),
    verbatimTextOutput("num"))
    )
  ),#TabPanel3
  
  tabPanel("Distribution of Staff Part 2",
    headerPanel("Distribution of Staffing by Revenue and Square Footage"),
  sidebarLayout( 
      sidebarPanel(
              selectInput("selectInd",
                          h4("Select an Industry:"),
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
                          selected = "Accomodations"),#checkboxGroupInput 
              
              selectInput("selectRev", 
                          h4("Select Annual Revenue Range:"),
                          choices=list("Up to $500K",
                                       "$500K to $1M",
                                       "$1M to $1.5M",
                                       "$1.5M to $2M",
                                       "$2M to $3M",
                                       "$3M to $5M",
                                       "$5M to $7M",
                                       "$7M to $10M",
                                       "$15M to $20M")), 
            selectInput("selectSize",
                        h4("Select Square Footage:"),
                        choices=list("500 - 1000 Square Ft",
                                     "1001 - 2000 Square Ft",
                                     "2001 - 3000 Square Ft",
                                     "3000 + Square Ft")),
            h4("Total Number of Businesses within Industry:"),
            textOutput("totalBiz"),
            h5("Number of Businesses within Field Selection:"),
            textOutput("numbiz"),
            h5("Median Number of Staffs within Field Selection:"),
            textOutput("median"),
            h5("Average Number of Staffs within Field Selection:"),
            textOutput("mean")),
            #h4("Average Number of Staffs for all industries:"),
            #textOutput("meanAll")),
          
      mainPanel(
        dataTableOutput("tbl2"))
      )#sideBarLayout
  )))#tab4

#############Define Server
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
  output$boxplot <- renderPlotly(
    if(input$selectInput=="All"){
      (ggplotly(ggplot(dta1)
       + geom_boxplot(aes(x=AnnualRevenue,y=Staff))
       + facet_grid(.~SquareFt)
       + scale_y_log10()
       + coord_flip()
       + theme_classic()
       + theme(axis.title.x = element_blank(),
               axis.title.y = element_blank())
       #+ xlab("Annual Revenue")
       #+ ylab("Number of Employees")
       + ggtitle("Analysis for all Industries")))}
     
    else{
      (ggplotly(ggplot(filter(dta1,Industry==input$selectInput))
                + geom_boxplot(aes(x=AnnualRevenue,y=Staff))
                + facet_grid(.~SquareFt)
                + scale_y_log10()
                + coord_flip()
                + theme_classic()
                + theme(axis.title.y = element_blank(),
                        axis.title.x = element_blank())
                + ggtitle(paste("Analysis for",input$selectInput,sep = " "))))
    }#endifelse
)#endRenderPlotly
  
 #Adding Number Converter 
  output$num <- renderPrint(10^(input$num))
  
  #Add filter table 
  output$tbl2 <- renderDataTable(
    dta2 <- (dta1 %>% select(Industry, AnnualRevenue, SquareFt,Staff)
             %>% filter(Industry == input$selectInd, 
                        AnnualRevenue == input$selectRev,
                        SquareFt == input$selectSize)))
  
  #Add medium and mean 
  output$median <- renderText(as.numeric((dta1 %>% select(Industry, AnnualRevenue, SquareFt,Staff)
                                            %>% filter(Industry == input$selectInd, 
                                                       AnnualRevenue == input$selectRev,
                                                       SquareFt == input$selectSize)
                                            %>% summarise(median(Staff)))))
  
  
  output$mean <- renderText(as.numeric((dta1 %>% select(Industry, AnnualRevenue, SquareFt,Staff)
                           %>% filter(Industry == input$selectInd, 
                                      AnnualRevenue == input$selectRev,
                                      SquareFt == input$selectSize)
                           %>% summarise(mean(Staff)))))
  
  output$numbiz <- renderText(as.numeric(dim(filter(dta1,Industry==input$selectInd,
                                                    AnnualRevenue==input$selectRev,
                                                    SquareFt==input$selectSize)))[1])
  
  output$totalBiz <- renderText(as.numeric(dim(filter(dta1,Industry==input$selectInd)))[1])
  #output$meanAll <- renderText(as.numeric(dta1 %>% select(Staff) %>% summarise(mean(Staff))))
}#server

shinyApp(ui=ui,server=server)























