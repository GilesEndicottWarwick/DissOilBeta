require(tidyverse)
require(lubridate)
require(robustbase)
require(broom)
require(quantreg)
require(data.table)


#LOAD THE DATA IN (FROM IMPORTSHARESDATA.R AND INSERTOIILDATA.R)
load("C:/Users/giles/OneDrive/MBA/Dissertation/Data/SharesDataImported.Rdata");


#MAKE SP500 AND OIL THEIR OWN INDUSTRY
allDataIn = allDataIn %>%
            ungroup() %>%
            mutate(Industry = ifelse( Symbol %in% c("__WTC_D","^GSPC"),Symbol,Industry)) %>%
            mutate(Industry = ifelse( SymbolDesc  == "S&P 500",Symbol,Industry)) %>%
            mutate(Industry = ifelse( SubIndustry == "Passenger Airlines",SubIndustry,Industry));

#IM LOST HACK, THE ABOVE DOESNT WORK
allDataIn$Industry[which(allDataIn$Symbol=="^GSPC")]="^GSPC";

#START AND END DATES OF ANALYSIS
sdate = as.Date("1990-01-01");
edate = as.Date("2023-11-30");

#GET UNIQUE VALUES +  STORE MASTER DATA
uniqueList = allDataIn %>%
  select(Symbol,SymbolDesc,Industry,SubIndustry) %>%
  distinct();

#ALL DATES SEQUENCES
allDates = data.frame(Date=seq(from=sdate,to=edate,by="days"));

#MAKE DATE A DATE FORMAT FOR JOINING
allDataIn = allDataIn %>% 
  mutate( Date = ymd(Date)) %>% 
  select(-SymbolDesc,-Industry,-SubIndustry);

Skeleton_1 = uniqueList %>%
  cross_join(allDates) %>%
  left_join(allDataIn,by=c("Date","Symbol")) %>%
  arrange(Symbol,Date) %>%
  group_by(Symbol) %>%
  fill(SymbolValue, .direction="down");

#GET START AND END OF MONTH ONLY
allReturns_S1 = Skeleton_1 %>%
  filter(!is.na(SymbolValue)) %>%
  mutate(Year = year(Date)) %>%
  mutate(Month = month(Date)) %>%
  group_by(Symbol,SymbolDesc,Industry,SubIndustry,Year,Month) %>%
  #mutate(DateRank1 = frank(Date)) %>%
  mutate(DateRank2 = frank(desc(Date))) %>%
  #filter( DateRank1 ==1 | DateRank2 == 1);
  filter( DateRank2 == 1);

#CALC RAW RETURN OF MONTH
allReturns_S2 = allReturns_S1 %>%
  ungroup() %>%
  group_by(Symbol,SymbolDesc,Industry,SubIndustry) %>%
  arrange(Industry,SubIndustry,Symbol,Date) %>%
  mutate(LastSymbolValue = lag(SymbolValue)) %>%
  mutate(LastDate = lag(Date)) %>%
  filter(DateRank2 == 1) %>%
  #arrange(Industry,Year,Month,Symbol) %>%
  filter(!is.na(LastSymbolValue)) %>%
  filter(!is.na(SymbolValue)) %>%
  mutate(DaysBetween = as.numeric(Date - LastDate)) %>%
  group_by(Industry,Year,Month) %>%
  mutate(InverseValue = 1/ LastSymbolValue) %>%
  arrange(Industry,SubIndustry,Date,Symbol) %>%
  group_by(Industry,Year,Month,Date) %>%
  mutate(SymbolValueW = SymbolValue * InverseValue) %>%
  mutate(LastSymbolValueW = LastSymbolValue * InverseValue) %>%
  summarise(
            SymbolValue      = sum(SymbolValue),
            LastSymbolValue  = sum(LastSymbolValue),
            SymbolValueW     = sum(SymbolValueW ),
            LastSymbolValueW = sum(LastSymbolValueW ),
            DaysBetween      = mean(DaysBetween) 
            ) %>%
 # mutate(RawReturn = (SymbolValue - LastSymbolValue ) / LastSymbolValue ) %>%
  mutate(RawReturn = (SymbolValueW - LastSymbolValueW ) / LastSymbolValueW ) %>%
  mutate(ReturnAnnualized = RawReturn / DaysBetween * 365.25);

#CALC RETURNS FROM RISK FREE DATA
RiskFreeData =   Skeleton_1 %>%
  filter(Symbol == "^TNX") %>%
  filter(!is.na(SymbolValue)) %>%
  ungroup()  %>%
  mutate(Year = year(Date)) %>%
  mutate(Month = month(Date)) %>%
  group_by(Symbol,SymbolDesc,Year,Month) %>%
  summarise(SymbolValue = mean(SymbolValue,na.rm=T)) %>%
  ungroup();


#DROP COLUMNS
RiskFreeDataJoin = RiskFreeData %>%
  dplyr::select(Year,Month,RiskFree = SymbolValue);


#SUBTRACT RISK FREE RATE FROM RETURNS 
IndustryReturnsWithRiskFree = allReturns_S2 %>%
  left_join(RiskFreeDataJoin,by=c("Year","Month")) %>%
  mutate(RiskFree = RiskFree / 100.0) %>%
  mutate(AbnormalReturnAnnualized = ReturnAnnualized - RiskFree) %>%
  filter(!is.na(Industry)) %>%
  filter(Industry != "Commodities")


#GET OIL AS A COLUMN
OilData = IndustryReturnsWithRiskFree %>%
  filter(Industry == "__WTC_D") %>%
  ungroup() %>%
  dplyr::select(Year,
                Month,
                OilValue                     = SymbolValue,
                OilReturnAnnualized          = ReturnAnnualized,
                OilAbnormalReturnAnnualized  = AbnormalReturnAnnualized);

#GET S&P 500 AS A COLUMN
SP500Data = IndustryReturnsWithRiskFree %>%
  filter(Industry == "^GSPC") %>%
  ungroup() %>%
  dplyr::select(Year,
                Month,
                SP500Value                      = SymbolValue,
                SP500ReturnAnnualized           = ReturnAnnualized,
                SP500AbnormalReturnAnnualized   = AbnormalReturnAnnualized);

#ADD SP500 TO MAIN DATA
IndustryReturnsDataWithOil = IndustryReturnsWithRiskFree %>%
  left_join(OilData, by=c("Year","Month")) %>%
  left_join(SP500Data, by=c("Year","Month")) %>%
  dplyr::select(-RawReturn) %>%
  mutate(YearMonth = paste(Year,str_pad(Month,2,"left","0"),sep="")) %>%
  filter(Industry != "^GSPC") %>%
  filter(Industry != "__WTC_D")


#NOW HACK THE FORMAT TO THE SHARES FORMAT AND WE CAN USE THE SAME PROGRAM
IndustryReturnsDataWithOil_OUT = IndustryReturnsDataWithOil %>%
                                 mutate(Symbol = Industry) %>%
                                 mutate(SymbolDesc = Industry) %>%
                                 mutate(SubIndustry = Industry) %>%
                                 select(
                                   Symbol,
                                   SymbolDesc,
                                   Industry,
                                   SubIndustry,
                                   Date,                    
                                   SymbolValue,
                                   Year,
                                   Month,
                                   DaysBetween,
                                   ReturnAnnualized,            
                                   RiskFree,
                                   AbnormalReturnAnnualized,
                                   OilValue,
                                   OilReturnAnnualized,         
                                   OilAbnormalReturnAnnualized,
                                   SP500Value,
                                   SP500ReturnAnnualized,
                                   SP500AbnormalReturnAnnualized,
                                   YearMonth
                                 )


#SAVE THE OUTPUT FILE
save(IndustryReturnsDataWithOil_OUT,file="C:/Users/giles/OneDrive/MBA/Dissertation/Data/MonthlyIndustryReturns.Rdata");
