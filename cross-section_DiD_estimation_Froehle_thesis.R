#Hannah Froehle's code for "Green Bonds in a Carbon Pricing Environment"

#This script generates the results for the difference-in-differences estimation using repeated cross-sectional data

setwd("~/Desktop/Sustainable Finance Thesis")
library(dplyr)
library(readr)
library(caret)
library(tidyr)
library(lfe)

#read in data
green_bonds <- read_csv("green bond data 01.11 hsf.csv")


#variable cleaning

green_bonds$Maturity <- as.Date(green_bonds$Maturity, "%d/%m/%Y")
green_bonds$`Issue Date` <-as.Date(green_bonds$`Issue Date`, "%d/%m/%Y")
green_bonds$`BBG Composite`[green_bonds$`BBG Composite`== "#N/A N/A"]= NA

#set up EU countries list 

EU_ETS_cntrys <- (list("AT", "IT", "BE", "LV", "BG", "LT", "HR", "LU", 
                       "CY", "MT", "CZ", "NL", "DK", "PL", "EE", "PT", 
                       "FI", "RO", "FR", "SK", "DE", "SI", "GR", "ES", 
                       "HU", "SE", "IE", "IS", "NO", "GB", "LI"))


#add in missing yield NAs

YAI_NAs <- filter(green_bonds, `Yield at Issue`=="#N/A N/A")

NA_yields1 <- read_csv("first yields for NAs.csv")
NA_yields2 <- read_csv("first yields for NAs2.csv")

colnames(NA_yields1)[1] <- "CUSIP"
colnames(NA_yields2)[1] <- "CUSIP"

colnames(NA_yields1)[2] <- "yield_1"
colnames(NA_yields2)[2] <- "yield_1"

NA_yields <- rbind(NA_yields1, NA_yields2)
fill_NAs <- inner_join(YAI_NAs, NA_yields)

  
fill_NAs$`Yield at Issue` <- fill_NAs$yield_1 
fill_NAs <- fill_NAs[,-21]

green_bonds_b <- anti_join(green_bonds, fill_NAs, by="CUSIP")

green_bonds_c <- rbind(green_bonds_b, fill_NAs)

green_bonds_c$`Yield at Issue`[green_bonds_c$`Yield at Issue`== "#N/A N/A"]= NA

#limit sample to EU and EU and ETS sectors
green_bonds_sample <- filter(green_bonds_c, `Cntry of Incorp` %in% EU_ETS_cntrys | `Cntry of Incorp` =="US")
green_bonds_ETS_sec <- filter(green_bonds_sample, `BICS Level 1` == "Utilities" | 
                                `BICS Level 1` == "Energy" | `BICS Level 1` == "Industrials" | 
                                `BICS Level 1` == "Materials" )

#remove California entities from sample

CA_entities <- list("SRE", "BE", "BRKHEC", "PEGI", "ENPH", "KOREAN", "BRKHEC")

green_bonds_CA <-  filter(green_bonds_ETS_sec, Ticker %in% CA_entities)

green_bonds_DID <- anti_join(green_bonds_ETS_sec, green_bonds_CA, by="CUSIP")

#dropping NAs from sample
green_bonds_DID <- drop_na(green_bonds_DID, `Yield at Issue`)

#add treated group dummies 
green_bonds_DID$treated_group <- dplyr::if_else(green_bonds_DID$`Cntry of Incorp` %in% EU_ETS_cntrys, 1, 0)

 

#add time dummies: specification with one treatment dummy
green_bonds_DID$reff <- dplyr::if_else(green_bonds_DID$`Issue Date` >= "2017-02-03", 1, 0)


#add time dummies: specification with lead and lag treatment dummies
green_bonds_DID$ref_2 <- dplyr::if_else(green_bonds_DID$`Issue Date`< "2016-02-15" & 
                                              green_bonds_DID$`Issue Date` >= "2015-02-15", 1, 0)
green_bonds_DID$ref_1 <- dplyr::if_else(green_bonds_DID$`Issue Date`< "2017-02-15" & 
                                              green_bonds_DID$`Issue Date` >= "2016-02-15", 1, 0)
green_bonds_DID$ref0 <- dplyr::if_else(green_bonds_DID$`Issue Date`< "2018-02-15" & 
                                            green_bonds_DID$`Issue Date` >= "2017-02-15", 1, 0)

green_bonds_DID$ref1 <- dplyr::if_else(green_bonds_DID$`Issue Date`< "2019-02-15" & 
                                            green_bonds_DID$`Issue Date` >= "2018-2-15", 1, 0)

green_bonds_DID$ref2 <- dplyr::if_else(green_bonds_DID$`Issue Date`< "2020-02-15" & 
                                            green_bonds_DID$`Issue Date` >= "2019-02-15", 1, 0)

green_bonds_DID$ref3 <- dplyr::if_else(green_bonds_DID$`Issue Date`< "2021-02-15" & 
                                            green_bonds_DID$`Issue Date` >= "2020-02-15", 1, 0)

green_bonds_DID$ref4 <- dplyr::if_else(green_bonds_DID$`Issue Date` >= "2021-02-15", 1, 0)

green_bonds_DID$`Yield at Issue`<- as.numeric(green_bonds_DID$`Yield at Issue`)


#factors and other analytical variables

green_bonds_DID <- mutate(green_bonds_DID, term= cut(as.numeric(Maturity-`Issue Date`), breaks = c(-Inf, 1826, 3652, Inf), labels=c("short","medium","long")))
green_bonds_DID <- mutate(green_bonds_DID, call_dummy = ifelse(`Is Still Callable` == "N",0,1))
green_bonds_DID <- mutate(green_bonds_DID, put_dummy = ifelse(Put == "N",0,1))
green_bonds_DID <- mutate(green_bonds_DID, collat = ifelse(`Collat Type` == "SR UNSECURED" | `Collat Type` == "SUBORDINATED" | `Collat Type` == "UNSECURED" | `Collat Type` == "JR SUBORDINATED",0,1))

#remove one level from each dummy variable to address collinearity

green_bonds_DID$year <- relevel(as.factor(format(green_bonds_DID$`Issue Date`,'%Y')), ref= "2014")
green_bonds_DID$firm <- relevel(factor(green_bonds_DID$Ticker), ref="ENGIFP")
green_bonds_DID$rating   <- relevel(factor(green_bonds_DID$`BBG Composite`), ref="NR")
green_bonds_DID$term <- relevel(factor(green_bonds_DID$term), ref="short")
green_bonds_DID$curr_var <- relevel(factor(green_bonds_DID$Currency), ref= "USD")
green_bonds_DID$country_fe <- relevel(factor(green_bonds_DID$`Cntry of Incorp`), ref= "US")
green_bonds_DID$sector_fe <- relevel(factor(green_bonds_DID$`BICS Level 2`), ref="Utilities")
green_bonds_DID$call_dummy <- relevel(factor(green_bonds_DID$call_dummy), ref= "0")
green_bonds_DID$put_dummy <- relevel(factor(green_bonds_DID$put_dummy), ref= "0")
green_bonds_DID$collat <- relevel(factor(green_bonds_DID$collat), ref= "0")

#Diff-in-diff regression, includeing firm fixed effects, country fixed effects, industry-year fixed effects

#DiD with one treatment dummy: equation 3.1 from the thesis
did1 <- felm(`Yield at Issue` ~ treated_group + reff*treated_group  +   call_dummy + collat +  term +curr_var + rating
             |firm+ country_fe + sector_fe:year | 0 | sector_fe, data = green_bonds_DID)

#DiD with lead and lag treatment dummies: equation 3.2 from the thesis
did2 <- felm(`Yield at Issue` ~ ref_2*treated_group + ref_1*treated_group + ref0*treated_group + ref1*treated_group + ref2*treated_group + ref3*treated_group
            +ref4*treated_group + call_dummy + collat  + term +curr_var + rating
            | firm + sector_fe:year + country_fe | 0 | sector_fe, data = green_bonds_DID, exactDOF=TRUE)

#Table 4.1
summary(did1)

#Table 4.2
summary(did2)

stargazer(did2)

