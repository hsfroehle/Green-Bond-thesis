#Hannah Froehle's code for "Green Bonds in a Carbon Pricing Environment"

#This script generates the results for the difference-in-differences estimation using panel data
setwd("~/Desktop/Sustainable Finance Thesis")
library(dplyr)
library(readr)
library(tidyr)
library(lfe)

#read in data
panel8 <- read_csv("~/Desktop/Sustainable Finance Thesis/final panel sample wide.csv")

cols <- c( "30/09/2015", "31/12/2015", "31/03/2016", "30/06/2016", "30/09/2016", "30/12/2016", "31/03/2017", "30/06/2017",
           "29/09/2017", "29/12/2017", "30/03/2018", "29/06/2018", "28/09/2018", "31/12/2018", "29/03/2019", "28/06/2019",
           "30/09/2019", "31/12/2019", "31/03/2020", "30/06/2020", "30/09/2020", "31/12/2020", "31/03/2021", "30/06/2021",
           "30/09/2021")
panel10 <- pivot_longer(panel8, cols= cols, names_to= "Yield Date", 
                        values_to= "Yield")
panel10

#remove outlier bonds

panel10 <- filter(panel10, Ticker != "SENGR")
panel10 <- filter(panel10, Ticker !="JIIN")

#set up EU countries list 

EU_ETS_cntrys <- (list("AT", "IT", "BE", "LV", "BG", "LT", "HR", "LU", 
                       "CY", "MT", "CZ", "NL", "DK", "PL", "EE", "PT", 
                       "FI", "RO", "FR", "SK", "DE", "SI", "GR", "ES", 
                       "HU", "SE", "IE", "IS", "NO", "GB", "LI"))

#add treatment dummy

panel10$treated_group <- dplyr::if_else(panel10$`Cntry of Incorp` %in% EU_ETS_cntrys, 1, 0)

#add dummies and analytical variables 


panel10 <- mutate(panel10, term= cut(as.numeric(Maturity-`Issue Date`), breaks = c(-Inf, 1826, 3652, Inf), labels=c("short","medium","long")))
panel10 <- mutate(panel10, call_dummy = ifelse(`Is Still Callable` == "N",0,1))
panel10 <- mutate(panel10, put_dummy = ifelse(Put == "N",0,1))
panel10 <- mutate(panel10, collat = ifelse(`Collat Type` == "SR UNSECURED" | `Collat Type` == "SUBORDINATED" | `Collat Type` == "UNSECURED" | `Collat Type` == "JR SUBORDINATED",0,1))

#remove one level from each dummy variable to address collinearity

panel10$year <- relevel(as.factor(format(panel10$`Issue Date`,'%Y')), ref= "2014")
panel10$firm <- relevel(factor(panel10$Ticker), ref="ENGIFP")
panel10$rating   <- relevel(factor(panel10$`BBG Composite`), ref="NR")
panel10$term <- relevel(factor(panel10$term), ref="short")
panel10$curr_var <- relevel(factor(panel10$Currency), ref= "USD")
panel10$country_fe <- relevel(factor(panel10$`Cntry of Incorp`), ref= "US")
panel10$sector_fe <- relevel(factor(panel10$`BICS Level 2`), ref="Utilities")
panel10$call_dummy <- relevel(factor(panel10$call_dummy), ref= "0")
panel10$put_dummy <- relevel(factor(panel10$put_dummy), ref= "0")
panel10$collat <- relevel(factor(panel10$collat), ref= "0")

#format date variable

panel10$`Yield Date` <-as.Date(panel10$`Yield Date`, "%d/%m/%Y")

#create treatment date dummies and leads and lags

panel10$reff <- dplyr::if_else(panel10$`Issue Date` >= "2017-02-15", 1, 0)

panel10$refp_1 <- dplyr::if_else(panel10$`Yield Date`< "2017-02-15" & 
                                     panel10$`Yield Date` >= "2016-02-15", 1, 0)

panel10$refp0 <- dplyr::if_else(panel10$`Yield Date`< "2018-02-15" & 
                                  panel10$`Yield Date` >= "2017-02-15", 1, 0)
panel10$refp1 <- dplyr::if_else(panel10$`Yield Date`< "2019-02-15" & 
                                  panel10$`Yield Date` >= "2018-02-15", 1, 0)
panel10$refp2 <- dplyr::if_else(panel10$`Yield Date`< "2020-02-15" & 
                                  panel10$`Yield Date` >= "2019-02-15", 1, 0)
panel10$refp3 <- dplyr::if_else(panel10$`Yield Date`< "2021-02-15" & 
                                  panel10$`Yield Date` >= "2020-02-15", 1, 0)
panel10$refp4 <- dplyr::if_else(panel10$`Yield Date` >= "2021-02-15", 1, 0)

#DiD with panel data and lead and lag treatment dummies
did_panel <- felm(Yield ~ refp_1*treated_group+ refp0*treated_group + refp1*treated_group + refp2*treated_group +
                    refp3*treated_group + refp4*treated_group +call_dummy + collat  + term +curr_var + rating
                  | firm + country_fe:year + sector_fe:year | 0 | sector_fe, data = panel10) 

#table 4.3:
summary(did_panel)
        
