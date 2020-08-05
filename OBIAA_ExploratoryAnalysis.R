library(tidyverse)
library(tibble)
library(plotly)
library(sunburstR)
library(readr)
library(plyr)

#Goal: Create shiny dashboards based on industry/space usage/staffing/revenue

### Raw dataa
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

#Relabel 
dta1$AnnualRevenue <-mapvalues(dta1$AnnualRevenue,
                               from=c("Up to $500,000"),
                               to = c("Up to $500K"))



mapvalues(x, from = c("beta", "gamma"), to = c("two", "three"))




dta1 <- mutate(dta1, SquareFt=fct_relevel(dta1$SquareFt,c(
  "500 - 1000 Square Ft",
  "1001 - 2000 Square Ft",
  "2001 - 3000 Square Ft",
  "3000 + Square Ft")))

#Check N/A 
tbl <- is.na.data.frame(dta1)
sumNA <- apply(tbl,2,sum)
print(sumNA)




###Question 1###
#Grouping funding by industry
funds_grp <- (dta1 %>% 
                group_by(Industry)
              %>% summarise(`Digital Marketing`=sum(DigitalMarket),`Website`=sum(Website),
                            `Software`=sum(Software),`Digital Training`=sum(DigitalTrain),
                            `Hardware`=sum(Hardware))
              %>% gather(key,value,`Digital Marketing`:Hardware))

#Generating sequence for sunburst
dta2 <-data.frame(funds_grp
        %>% unite(seq,Industry:key,sep = "-"))

#plot
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
))

tbl <- (dta1 %>% 
          group_by(Industry)
        %>% summarise(`Digital Marketing`=sum(DigitalMarket),`Website`=sum(Website),
                      `Software`=sum(Software),`Digital Training`=sum(DigitalTrain),
                      `Hardware`=sum(Hardware)))

tbl1 <- datatable(tbl)

###Question 2 ###
# Relationship between revenue and number of employees 
# Relationship between square feet and revenue 
#Outlier 178?? Dance Studio 
#Sort ordering of Annual Revenue + Staff 

#apply log transformation
plot <- print(ggplot(dta1)
         + geom_boxplot(aes(x=AnnualRevenue,y=Staff))
         + facet_grid(.~SquareFt)
         + scale_y_log10()
         + coord_flip()
         + theme_minimal()
         + xlab("Annual Revenue")
         + ylab("Number of Employees")
         + ggtitle("Analysis for All Industries"))

f <- ggplotly(plot)

#Concentrate on more customer centric industries and repeat analysis?? 
#Note: log Transformation on 
plot1 <- print(ggplot(filter(dta1,Industry==input$))
              + geom_boxplot(aes(x=AnnualRevenue,y=Staff))
              + facet_grid(.~SquareFt)
               scale_y_log10()
              + coord_flip()
              + theme_minimal()
              + xlab("Annual Revenue")
              + ylab("Number of Employees")
              + ggtitle("Analysis for All Industries"))
g <- ggplotly(plot)



copydta1 <- dta1 
copydta1$SquareFt <- mapvalues(
  copydta1$SquareFt, from = c("500 - 1000 Square Ft","1001 - 2000 Square Ft"),
  to = c("500-1000 SqFt","1001-2000 SqFt")
)







