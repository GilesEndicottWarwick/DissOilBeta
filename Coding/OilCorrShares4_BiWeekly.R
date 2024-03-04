require(dplyr)
require(quantmod)
require(stringr)
require(lubridate)
require(tidyr)
require(data.table)


#LOAD THE DATA IN (FROM IMPORTSHARESDATA.R AND INSERTOIILDATA.R)
load("C:/Users/giles/OneDrive/MBA/Dissertation/Data/SharesDataImported.Rdata");

#START AND END DATES OF ANALYSIS
sdate = as.Date("1989-01-01");
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
                mutate(Week = week(Date)) %>%
                mutate(BiWeek = ceiling(Week / 2)) %>%
                group_by(Symbol,SymbolDesc,Industry,SubIndustry,Year,BiWeek) %>%
                mutate(DateRank2 = frank(desc(Date))) %>%
                filter( DateRank2 == 1);              
  


#CALC RAW RETURN OF MONTH
allReturns_S2 = allReturns_S1 %>%
                ungroup() %>%
                group_by(Symbol,SymbolDesc,Industry,SubIndustry) %>%
                arrange(Symbol,Date ) %>%
                mutate(LastSymbolValue = lag(SymbolValue)) %>%
                mutate(LastDate = lag(Date)) %>%
                filter(DateRank2 == 1) %>%
                arrange(Industry,Symbol,Date) %>%
                filter(!is.na(LastSymbolValue)) %>%
                filter(!is.na(SymbolValue)) %>%
                mutate(DaysBetween = as.numeric(Date - LastDate)) %>%
                mutate(InverseValue = 1/ LastSymbolValue) %>%
                mutate(SymbolValueW = SymbolValue * InverseValue) %>%
                mutate(LastSymbolValueW = LastSymbolValue * InverseValue) %>%
                group_by(Industry,SubIndustry,Symbol,SymbolDesc,Year,BiWeek,Date) %>%
                summarise(
                  SymbolValue     = sum(SymbolValue),
                  LastSymbolValue = sum(LastSymbolValue),
                  SymbolValueW     = sum(SymbolValueW ),
                  LastSymbolValueW = sum(LastSymbolValueW ),
                  DaysBetween     = mean(DaysBetween) 
                ) %>%
                mutate(RawReturnUW = (SymbolValue - LastSymbolValue ) / LastSymbolValue ) %>%
                mutate(RawReturn = (SymbolValueW - LastSymbolValueW ) / LastSymbolValueW ) %>%
                mutate(ReturnAnnualized = RawReturn / DaysBetween * 365.25);

#CALC RETURNS FROM RISK FREE DATA
RiskFreeData =   Skeleton_1 %>%
                  filter(Symbol == "^TNX") %>%
                  filter(!is.na(SymbolValue)) %>%
                  ungroup()  %>%
                  mutate(Year = year(Date)) %>%
                  mutate(Week = week(Date)) %>%
                  mutate(BiWeek = ceiling(Week / 2)) %>%
                  group_by(Symbol,SymbolDesc,Year,BiWeek) %>%
                  summarise(SymbolValue = mean(SymbolValue,na.rm=T)) %>%
                  ungroup();
  
 #DROP COLUMNS
RiskFreeDataJoin = RiskFreeData %>%
                    dplyr::select(Year,BiWeek,RiskFree = SymbolValue);

#SUBTRACT RISK FREE RATE FROM RETURNS 
ReturnsWithRiskFree = allReturns_S2 %>%
                          left_join(RiskFreeDataJoin,by=c("Year","BiWeek")) %>%
                          mutate(RiskFree = RiskFree / 100.0) %>%
                          mutate(AbnormalReturnAnnualized = ReturnAnnualized - RiskFree);
    
#GET OIL AS A COLUMN
OilData = ReturnsWithRiskFree %>%
              filter(Symbol == "__WTC_D") %>%
              ungroup() %>%
              dplyr::select(Year,
                            BiWeek,
                            OilValue                     = SymbolValue,
                            OilReturnAnnualized          = ReturnAnnualized,
                            OilAbnormalReturnAnnualized  = AbnormalReturnAnnualized);

#GET S&P 500 AS A COLUMN
SP500Data = ReturnsWithRiskFree %>%
      filter(Symbol == "^GSPC") %>%
      ungroup() %>%
      dplyr::select(Year,
                    BiWeek,
                    SP500Value                      = SymbolValue,
                    SP500ReturnAnnualized           = ReturnAnnualized,
                    SP500AbnormalReturnAnnualized   = AbnormalReturnAnnualized);
    
#ADD SP500 TO MAIN DATA
ReturnsDataWithOil = ReturnsWithRiskFree %>%
                        left_join(OilData, by=c("Year","BiWeek")) %>%
                        left_join(SP500Data, by=c("Year","BiWeek")) %>%
                        dplyr::select(-LastSymbolValue,-RawReturn) %>%
                        mutate(YearBiWeek = paste(Year,str_pad(BiWeek,2,"left","0"),sep=""))
    
#WRITE THE OUTPUT FILE
save(ReturnsDataWithOil,file="C:/Users/giles/OneDrive/MBA/Dissertation/Data/BiWeekly_ReturnsWithOil.Rdata");
         