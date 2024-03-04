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

#LOAD BI WEEKLY DATA
load("C:/Users/giles/OneDrive/MBA/Dissertation/Data/MonthlyIndustryReturnsBiWeekly.Rdata");

#LOAD MONTHLY DATA
load("C:/Users/giles/OneDrive/MBA/Dissertation/Data/MonthlyIndustryReturns.Rdata");

#FIRST FILTERING AND PROCESSING
MonthlyData   = IndustryReturnsDataWithOil_OUT;
BiWeeklyData  = IndustryReturnsDataWithOil_OUT_BiWeekly;
WeeklyData    = IndustryReturnsDataWithOil_OUT_Weekly;

#REMOVE ORGIN VERSIONS
rm(IndustryReturnsDataWithOil_OUT);
rm(IndustryReturnsDataWithOil_OUT_BiWeekly);
rm(IndustryReturnsDataWithOil_OUT_Weekly);

#COMBINE THEM 
AllData = (MonthlyData %>% mutate(Data = "Monthly")) %>%
          rbind(
            (WeeklyData %>% mutate(Data = "Weekly"))
          ) %>%
          rbind(
            (BiWeeklyData %>% mutate(Data = "BiWeekly"))
          );

#WRITE THEM OUT FOR TABLEAU
save(AllData,file="C:/Users/giles/OneDrive/MBA/Dissertation/Data/ValidationData.Rdata")


#FOR WEEKLY DATA USE EVERY 4TH DATA POINT AS A START DATE
WeeklyData_Limits= WeeklyData %>%
                   ungroup() %>%
                   filter(Symbol ==  "Energy") %>%
                   select(Date,Year,Week) %>%
                   mutate(Loop4A = 0:(dim(.)[1]-1)) %>%
                   mutate(Loop4B = Loop4A %% 4 ) %>%
                   filter(Loop4B == 0) %>%
                   select(Year,Week,Date) %>%
                   mutate(Date2Y = Date + 732 ) %>%
                   filter(Date2Y <= ymd("20231201"));

BiWeeklyData_Limits= BiWeeklyData %>%
                    ungroup() %>%
                    filter(Symbol ==  "Energy") %>%
                    select(Date,Year,BiWeek) %>%
                    mutate(Loop4A = 0:(dim(.)[1]-1)) %>%
                    mutate(Loop4B = Loop4A %% 2 ) %>%
                    filter(Loop4B == 0) %>%
                    select(Year,BiWeek,Date) %>%
                    mutate(Date2Y = Date + 732 ) %>%
                    filter(Date2Y <= ymd("20231201"));

MonthlyData_Limits = MonthlyData %>%
                      ungroup() %>%
                      filter(Symbol ==  "Energy") %>%
                      select(Date,Year,Month) %>%
                      select(Year,Month,Date) %>%
                      mutate(Date2Y = Date + 732 ) %>%
                      filter(Date2Y <= ymd("20231201"));


#STORAGE DATA FRAMES
WeeklyBetaOut  = data.frame();
BiWeeklyBetaOut  = data.frame();
MonthlyBetaOut = data.frame();

#LOOP WEEKLY DATA
for (iTime in 1:dim(WeeklyData_Limits)[1])
{
  #GET TIMEFRAME
  TimeStart = WeeklyData_Limits$Date[iTime];
  TimeEnd   = WeeklyData_Limits$Date2Y[iTime];
  
  #GET THE DATA
  Subset_Weekly = WeeklyData %>%
                  filter(Symbol ==  "Energy") %>%
                  filter(Date >=  TimeStart ) %>%
                  filter(Date <=  TimeEnd );
  
  #CALC BETAS FOR TIMEFRAME
  LM_TF1A = lm(data = Subset_Weekly,AbnormalReturnAnnualized ~ SP500AbnormalReturnAnnualized + OilAbnormalReturnAnnualized);
  LM_TF1B = lmrob(data = Subset_Weekly,AbnormalReturnAnnualized ~ SP500AbnormalReturnAnnualized + OilAbnormalReturnAnnualized);
  
  #TIDY IT ALL UP 
  OUT_1 = tidy(LM_TF1A) %>% mutate(Symbol = "Energy", Regression = "OLS",TimeStart = TimeStart, TimeEnd= TimeEnd, Disc = "Weekly") %>%
          rbind(tidy(LM_TF1B) %>% mutate(Symbol = "Energy",Regression = "ROB",TimeStart = TimeStart, TimeEnd= TimeEnd, Disc = "Weekly"));
  
  #COMBINE OUTPUTS
  WeeklyBetaOut = WeeklyBetaOut %>% 
                  rbind(OUT_1)
}


#LOOP BI-WEEKLY DATA
for (iTime in 1:dim(BiWeeklyData_Limits)[1])
{
  #GET TIMEFRAME
  TimeStart = BiWeeklyData_Limits$Date[iTime];
  TimeEnd   = BiWeeklyData_Limits$Date2Y[iTime];
  
  #GET THE DATA
  Subset_BiWeekly = BiWeeklyData %>%
    filter(Symbol ==  "Energy") %>%
    filter(Date >=  TimeStart ) %>%
    filter(Date <=  TimeEnd );
  
  #CALC BETAS FOR TIMEFRAME
  LM_TF1A = lm(data = Subset_BiWeekly,AbnormalReturnAnnualized ~ SP500AbnormalReturnAnnualized + OilAbnormalReturnAnnualized);
  LM_TF1B = lmrob(data = Subset_BiWeekly,AbnormalReturnAnnualized ~ SP500AbnormalReturnAnnualized + OilAbnormalReturnAnnualized);
  
  #TIDY IT ALL UP 
  OUT_1 = tidy(LM_TF1A) %>% mutate(Symbol = "Energy", Regression = "OLS",TimeStart = TimeStart, TimeEnd= TimeEnd, Disc = "BiWeekly") %>%
    rbind(tidy(LM_TF1B) %>% mutate(Symbol = "Energy",Regression = "ROB",TimeStart = TimeStart, TimeEnd= TimeEnd, Disc = "BiWeekly"));
  
  #COMBINE OUTPUTS
  BiWeeklyBetaOut = BiWeeklyBetaOut %>% 
                    rbind(OUT_1)
}

#LOOP MONTHLY DATA
for (iTime in 1:dim(MonthlyData_Limits)[1])
{
  #GET TIMEFRAME
  TimeStart = MonthlyData_Limits$Date[iTime];
  TimeEnd   = MonthlyData_Limits$Date2Y[iTime];
  
  #GET THE DATA
  Subset_Monthly = MonthlyData %>%
                  filter(Symbol ==  "Energy") %>%
                  filter(Date >=  TimeStart ) %>%
                  filter(Date <=  TimeEnd );
  
  #CALC BETAS FOR TIMEFRAME
  LM_TF1A = lm(data = Subset_Monthly,AbnormalReturnAnnualized ~ SP500AbnormalReturnAnnualized + OilAbnormalReturnAnnualized);
  LM_TF1B = lmrob(data = Subset_Monthly,AbnormalReturnAnnualized ~ SP500AbnormalReturnAnnualized + OilAbnormalReturnAnnualized);
  
  #TIDY IT ALL UP 
  OUT_1 = tidy(LM_TF1A) %>% mutate(Symbol = "Energy", Regression = "OLS",TimeStart = TimeStart, TimeEnd= TimeEnd, Disc = "Monthly") %>%
    rbind(tidy(LM_TF1B) %>% mutate(Symbol = "Energy",Regression = "ROB",TimeStart = TimeStart, TimeEnd= TimeEnd, Disc = "Monthly"));
  
  #COMBINE OUTPUTS
  MonthlyBetaOut = MonthlyBetaOut %>% 
    rbind(OUT_1)
}               
               

#COMBINE THE RESULTS
AllBetaOut = WeeklyBetaOut %>%
             rbind(BiWeeklyBetaOut) %>%
             rbind(MonthlyBetaOut)

#WRITE THE DATA TO A FILE
save(AllBetaOut,file="C:/Users/giles/OneDrive/MBA/Dissertation/Data/ValidationLarger.Rdata")

