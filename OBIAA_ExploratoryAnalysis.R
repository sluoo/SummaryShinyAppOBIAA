library(tidyverse)
library(tibble)
library(plotly)

#Goal: Create shiny dashboards based on industry/space usage/staffing/revenue

### Raw data
dta <- DTG_Approved_Grants_Sorted_by_Date


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
                    Hardware = as.numeric(gsub("[\\$,]","",Hardware)))
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

##How are funds potentially distributed/proportion for each area? 
totalAmount <- sum(dta1$Currency)
totalRows <- nrow(dta1)

#Number of businesses who requested funding for each area 
nDM = length(which(dta1$DigitalMarket != 0))
sumDM = sum(dta1$DigitalMarket)

nDT = length(which(dta1$DigitalTrain != 0))
sumDT = sum(dta1$DigitalTrain)

nWeb = length(which(dta1$Website != 0))
sumWeb = sum(dta1$Website)

nSoft = length(which(dta1$Software != 0))
sumSoft = sum(dta1$Software)

nHard = length(which(dta1$Hardware != 0))
sumHard = sum(dta1$Hardware)

all.n = c(nDM,nDT,nWeb,nSoft,nHard)
prop = all.n / sum(all.n)
ymax1 = cumsum(prop)
ymin1 = c(0,ymax1[1:4])
sumCol = c(sumDM,sumDT,sumWeb,sumSoft,sumHard)


data.donut <- data.frame(
  Category = c("Digital Marketing", "Digital Training", "Website",
               "Software", "Hardware"),
  prop = prop,
  ymax = ymax1, 
  ymin = ymin1,
  sum1 = sumCol
)

#Compute label/position
data.donut$label <- paste0(data.donut$Category, "\n Proportion:", round(data.donut$prop,3), 
                           "\n Total Funding:", "$",data.donut$sum1)/10000)

data.donut$labelPosition <- (data.donut$ymax + data.donut$ymin) / 2

g <- (ggplot(data.donut, aes(ymax=ymax,ymin=ymin,xmax=4,xmin=3,fill=Category))
       + geom_rect(show.legend = FALSE)
       + geom_label( x=3.5, aes(y=labelPosition, label=label), size=3) 
       + coord_polar(theta="y")
       + theme_void()
       + scale_fill_brewer(palette = 1))

print(g)




