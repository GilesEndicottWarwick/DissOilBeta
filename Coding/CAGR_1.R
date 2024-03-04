require(dplyr)
require(quantmod)
require(stringr)
require(lubridate)
require(tidyr)
require(data.table)


#LOAD THE DATA IN (FROM IMPORTSHARESDATA.R AND INSERTOIILDATA.R)
load("C:/Users/giles/OneDrive/MBA/Dissertation/Data/SharesDataImported.Rdata");

symbolList = allDataIn %>%
             dplyr::select(Symbol,SymbolDesc) %>%
             distinct()


#DO CAGR FOR EACH SYMBOL
CAGR_1 = allDataIn %>%
         filter(!is.na(SymbolValue)) %>%
         mutate(Data = ymd(Date)) %>%
         dplyr::select(Symbol,SymbolDesc,SymbolValue,Date) %>%
         group_by(Symbol,SymbolDesc) %>%
         mutate(M1 = frank(Date)) %>%
         mutate(M2 = frank(desc(Date))) %>%
         filter(M1 ==1 | M2 == 1) %>%
         mutate(LastSymbolValue = lag(SymbolValue)) %>%
         mutate(LastDate = lag(Date)) %>%
         filter(M2==1) %>%
         dplyr::select(-M1,-M2) %>%
         mutate(DaysBetween = as.numeric(ymd(Date) - ymd(LastDate))) %>%
         mutate(YearsBetween = DaysBetween / 365.25) %>%
         mutate(ValueDiff = SymbolValue - LastSymbolValue) %>%
         mutate(ValueRatio = SymbolValue / LastSymbolValue);

CAGR_2 = CAGR_1 %>%
         mutate(CAGR = (   ValueRatio ^ (1/YearsBetween ))-1) %>%
         dplyr::select(Symbol,SymbolDesc,YearsBetween,Date,LastDate,CAGR) %>%
         mutate(CAGR_Percent = CAGR*100)
