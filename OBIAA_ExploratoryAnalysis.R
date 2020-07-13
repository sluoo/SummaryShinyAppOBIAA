library(tidyverse)
library(tibble)

#Goal: Create shiny dashboards based on industry/space usage/staffing/revenue

### Raw data
dta <- DTG_Approved_Grants_Sorted_by_Date


####Clean Raw Data

#Select necessary columns for analysis
#Remove denied funding cases 
dta1 <- (dta %>% 
           select(`Business Name`,Industry, BIA, City, Municipality, Currency, `Matching Funds`, `Annual Revenue`,
                  `Square Feet`, Staff, Status,`How Did You Hear About the Grant`,`Digital Marketing $`,
                  Website, Software, `Digital Training`, Hardware,DSS, Region)
         %>% rename(ReferalGrant = `How Did You Hear About the Grant`,
                    DigitalMarket = `Digital Marketing $`,
                    DigitalTrain = `Digital Training`)
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

View(select(dta1,`Business Name`,`Annual Revenue`,
            `Industry`,`Staff`,`Square Feet`)
     %>% filter(Industry == "Medical Services")
     %>% filter(`Annual Revenue` == "Up to $500,000")
     %>% filter(`Square Feet` == "1001 - 2000 Square Ft"))

#Remove Castle Maker and predicted number of staff for Lacroix (not enough data)

#Seperate by Industry 
retailTbl <- (dta1
              %>% filter())











