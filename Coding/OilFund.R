require(dplyr)
require(quantmod)
require(stringr)
require(lubridate)
require(tidyr)
require(data.table)


#LOAD THE DATA IN (FROM IMPORTSHARESDATA.R AND INSERTOIILDATA.R)
load("C:/Users/giles/OneDrive/MBA/Dissertation/Data/SharesDataImported.Rdata");

#START AND END DATES OF ANALYSIS
sdate = as.Date("1990-01-01");
edate = as.Date("2023-11-30");

#GET UNIQUE VALUES +  STORE MASTER DATA
uniqueList = allDataIn %>%
  select(Symbol,SymbolDesc,Industry,SubIndustry) %>%
  distinct();

#ALL DATES SEQUENCES
allDates = data.frame(Date=seq(from=sdate,to=edate,by="days"));

#ENERGY DATA ONLY
EnergyData = allDataIn %>%
             filter(Industry == "Energy") %>%
             mutate(Year= year(Date));

#COMPANIES PER YEAR
YearCompanyCount = EnergyData %>%
                   group_by(Year) %>%
                   summarise(nCompanies = length(unique(Symbol)));

#FIRST DAY OF EACH YEAR
FirstDayYear = EnergyData %>%
               group_by(Year) %>%
               mutate(DayRank = frank(Date,ties.method=c("min"))) %>%
               filter(DayRank == 1) %>%
               mutate(nCompanies = length(unique(Symbol))) %>%
               mutate(SymbolWeighting = 1 / (nCompanies*SymbolValue)) %>%
               select(Year,Symbol,SymbolWeighting);
    

#INNER JOIN FIRST YEAR DATES WITH TOTAL 
EnergyDataWithWeighting_1 = EnergyData %>%
                            inner_join(FirstDayYear,by=c("Year","Symbol")) %>%
                            mutate(WeightedValue = SymbolWeighting * SymbolValue) %>%
                            group_by(Date) %>%
                            summarise(IndexValue_1 = sum(WeightedValue)) %>%
                            mutate(Year= year(Date))

#GET LAST DAY OF EACH YEAR
LastDayYear =  EnergyDataWithWeighting_1 %>%
               mutate(Year = year(Date)) %>%
               group_by(Year) %>%
               mutate(Rank = frank(desc(Date))) %>%
               filter(Rank == 1) %>%
               arrange(Year) %>%
               ungroup() %>%
               mutate(YearProd = cumprod(IndexValue_1)) %>%
               mutate(YearJoin = Year + 1) %>%
               select(YearJoin,YearProd)

EnergyDataWithWeighting_2 = EnergyDataWithWeighting_1 %>%
                            left_join(LastDayYear,by=c("Year"="YearJoin")) %>%
                            mutate(YearProd = ifelse(Year ==1990,1,YearProd)) %>%
                            mutate(IndexValue_2 = IndexValue_1 * YearProd);



#write.table(EnergyDataWithWeighting_2,"clipboard-16384",sep=";",row.names=F,quote=F)

#ANNUAL RETURNS ARE JUST THE VALUE OF EnergyDataWithWeighting_1 ON THE LAST DAY OF THE YEAR
AnnualOilShareReturns = EnergyDataWithWeighting_1 %>%
                        group_by(Year) %>%
                        mutate(Rank = frank(desc(Date))) %>%
                        filter(Rank == 1) %>%
                        select(Year,Value = IndexValue_1) %>%
                        mutate(Symbol="OilFund")
    
  

        


