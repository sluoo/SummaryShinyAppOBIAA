library(tidyverse)
library(tibble)
library(plotly)
library(sunburstR)

#Goal: Create shiny dashboards based on industry/space usage/staffing/revenue

### Raw dataa
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


#Check N/A 
tbl <- is.na.data.frame(dta1)
sumNA <- apply(tbl,2,sum)
print(sumNA)

###Question 1###

#Use of Funds by Industry 
funds_grp <- (dta1 %>% 
                group_by(Industry)
              %>% summarise(`Digital Marketing`=sum(DigitalMarket),`Website`=sum(Website),
                            `Software`=sum(Software),`Digital Training`=sum(DigitalTrain),
                            `Hardware`=sum(Hardware))
              %>% gather(key,value,`Digital Marketing`:Hardware))

dta2 <-data.frame(funds_grp
        %>% unite(seq,Industry:key,sep = "-"))

sunplot1 <- print(sund2b(dta2,
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

###Question 2 ###
# Relationship between revenue and number of employees 
# Relationship between square feet and revenue 
#Outlier 178?? Dance Studio 
#Sort ordering of Annual Revenue + Staff 
plot <- print(ggplot(dta1)
         + geom_boxplot(aes(x=AnnualRevenue,y=Staff))
         + facet_grid(.~SquareFt)
         + coord_flip())

fig <- ggplotly(plot)

fig
#Ask Kay if these number make sense... 
#View(filter(dta1,40 < Staff & Staff < 200))







