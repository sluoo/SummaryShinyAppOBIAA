library(tidyverse)
dta <- read_csv("DTG_Approved.csv")

#Clean Raw Data
#Select necessary columns for analysis
dta1 <- (dta %>% 
           select(`Business Name`, City, Municipality, Currency, `Matching Funds`, `Annual Revenue`,
                  `Square Feet`, Staff, `How Did You Hear About the Grant`, Status, BIA,
                  `Digital Marketing $`,Website, Software, `Digital Training`, Hardware,DSS, Region)
         %>% rename(ReferalGrant = `How Did You Hear About the Grant`))

#Check N/A 
emptyNA <- (dta1 %>% 
              select(everything()))

