#LIBRARIES
require(tidyverse)
require(lubridate)
require(readxl)
require(partykit)
require(plotly)
require(robustbase)
require(data.table)

#LOAD WEEKLY DATA
load("C:/Users/giles/OneDrive/MBA/Dissertation/Data/MonthlyIndustryReturnsWeekly.Rdata");

#LOAD MONTHLY DATA
load("C:/Users/giles/OneDrive/MBA/Dissertation/Data/MonthlyIndustryReturns.Rdata");

#FIRST FILTERING AND PROCESSING
MonthlyData = IndustryReturnsDataWithOil_OUT;
WeeklyData  = IndustryReturnsDataWithOil_OUT_Weekly;

#COMBINE THEM 
AllData = (MonthlyData %>% mutate(Data = "Monthly")) %>%
          rbind(
            (WeeklyData %>% mutate(Data = "Weekly"))
          );

#WRITE THEM OUT FOR TABLEAU
save(AllData,file="C:/Users/giles/OneDrive/MBA/Dissertation/Data/ValidationData.Rdata")

#ADD THE TIMEFRAME TO BOTH DATASETS AND FILTER
MonthlyData = MonthlyData %>%
              mutate(TimeFrame1 = ifelse(Year %in% c(1990,1991),"1990-1991",NA)) %>%
              mutate(TimeFrame2 = ifelse(Year %in% c(1994,1995),"1994-1995",NA)) %>%
              mutate(TimeFrame3 = ifelse(Year %in% c(1998,1999),"1998-1999",NA)) %>%
              mutate(TimeFrame4 = ifelse(Year %in% c(2002,2003),"2002-2003",NA)) %>%
              mutate(TimeFrame5 = ifelse(Year %in% c(2006,2007),"2006-2007",NA)) %>%
              mutate(TimeFrame6 = ifelse(Year %in% c(2010,2011),"2010-2011",NA)) %>%
              mutate(TimeFrame7 = ifelse(Year %in% c(2014,2015),"2014-2015",NA)) %>%
              mutate(TimeFrame8 = ifelse(Year %in% c(2018,2019),"2018-2019",NA)) %>%
              mutate(TimeFrame9 = ifelse(Year %in% c(2022,2023),"2022-2023",NA)) %>%
              mutate(Timeframe = coalesce(TimeFrame1,TimeFrame2,TimeFrame3,TimeFrame4,TimeFrame5,TimeFrame6,TimeFrame7,TimeFrame8,TimeFrame9)) %>%
              select(-TimeFrame1,-TimeFrame2,-TimeFrame3,-TimeFrame4,-TimeFrame5,-TimeFrame6,-TimeFrame7,-TimeFrame8,-TimeFrame9) %>%
              filter(Industry == "Energy") %>%
              filter(!is.na(Timeframe))

WeeklyData = WeeklyData %>%
  mutate(TimeFrame1 = ifelse(Year %in% c(1990,1991),"1990-1991",NA)) %>%
  mutate(TimeFrame2 = ifelse(Year %in% c(1994,1995),"1994-1995",NA)) %>%
  mutate(TimeFrame3 = ifelse(Year %in% c(1998,1999),"1998-1999",NA)) %>%
  mutate(TimeFrame4 = ifelse(Year %in% c(2002,2003),"2002-2003",NA)) %>%
  mutate(TimeFrame5 = ifelse(Year %in% c(2006,2007),"2006-2007",NA)) %>%
  mutate(TimeFrame6 = ifelse(Year %in% c(2010,2011),"2010-2011",NA)) %>%
  mutate(TimeFrame7 = ifelse(Year %in% c(2014,2015),"2014-2015",NA)) %>%
  mutate(TimeFrame8 = ifelse(Year %in% c(2018,2019),"2018-2019",NA)) %>%
  mutate(TimeFrame9 = ifelse(Year %in% c(2022,2023),"2022-2023",NA)) %>%
  mutate(Timeframe = coalesce(TimeFrame1,TimeFrame2,TimeFrame3,TimeFrame4,TimeFrame5,TimeFrame6,TimeFrame7,TimeFrame8,TimeFrame9)) %>%
  select(-TimeFrame1,-TimeFrame2,-TimeFrame3,-TimeFrame4,-TimeFrame5,-TimeFrame6,-TimeFrame7,-TimeFrame8,-TimeFrame9) %>%
  filter(Industry == "Energy") %>%
  filter(!is.na(Timeframe))


#LIST TIMEFRAMES FOR LOOPING
allTimeFrames = MonthlyData %>% 
                  ungroup() %>%
                  select(Timeframe) %>%
                  distinct();

#STORE RESULTS
allOut = data.frame();

#LOOP THE TIMEFRAMES
for (iTime in 1:dim(allTimeFrames)[1])
{
  
  #GET THE TIMEFRAME
  thisTime = allTimeFrames$Timeframe[iTime];
  
  #CREATE THE SUBSETS OF dATA
  Subset_TF1 = MonthlyData %>%
    filter(Timeframe == thisTime);
  
  Subset_TF2 = WeeklyData %>%
    filter(Timeframe == thisTime);
  
  #CALC BETAS FOR TIMEFRAME 1
  LM_TF1A = lm(data = Subset_TF1,AbnormalReturnAnnualized ~ SP500AbnormalReturnAnnualized + OilAbnormalReturnAnnualized);
  LM_TF1B = lmrob(data = Subset_TF1,AbnormalReturnAnnualized ~ SP500AbnormalReturnAnnualized + OilAbnormalReturnAnnualized);
  
  LM_TF2A = lm(data = Subset_TF2,AbnormalReturnAnnualized ~ SP500AbnormalReturnAnnualized + OilAbnormalReturnAnnualized);
  LM_TF2B = lmrob(data = Subset_TF2,AbnormalReturnAnnualized ~ SP500AbnormalReturnAnnualized + OilAbnormalReturnAnnualized);
  
  #TIDY IT ALL UP 
  OUT_1 = tidy(LM_TF1A) %>% mutate(Symbol = "Energy", Regression = "OLS",Timeframe = thisTime, Disc = "Monthly") %>%
    rbind(tidy(LM_TF1B) %>% mutate(Symbol = "Energy",Regression = "ROB",Timeframe = thisTime, Disc = "Monthly")) %>%
    rbind(tidy(LM_TF2A) %>% mutate(Symbol = "Energy",Regression = "OLS",Timeframe = thisTime, Disc = "Weekly")) %>%
    rbind(tidy(LM_TF2B) %>% mutate(Symbol = "Energy",Regression = "ROB",Timeframe = thisTime, Disc = "Weekly"));
  
  #COMBINE OUTPUTS
  allOut = allOut %>% 
    rbind(OUT_1)

  
}


#VIEW THE RESULTS
FormatOut1 = allOut %>%
            filter(term != "(Intercept)") %>%
            select(Regression,Timeframe,term,estimate,Disc) %>%
            filter(Regression == "OLS") %>%
            select(-Regression)

FormatOut1_ROB = allOut %>%
                filter(term != "(Intercept)") %>%
                select(Regression,Timeframe,term,estimate,Disc) %>%
                filter(Regression == "ROB") %>%
                select(-Regression)

FormatOut2_ROB = FormatOut1_ROB %>%
             pivot_wider(names_from=c("Disc","term"),values_from=c("estimate"))

plot(FormatOut2$Monthly_OilAbnormalReturnAnnualized,FormatOut2$Weekly_OilAbnormalReturnAnnualized)

plot(FormatOut2_ROB$Monthly_OilAbnormalReturnAnnualized,FormatOut2_ROB$Weekly_OilAbnormalReturnAnnualized)
