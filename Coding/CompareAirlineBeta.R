#LIBRARIES
require(tidyverse)
require(lubridate)
require(readxl)
require(partykit)
require(plotly)
require(robustbase)
require(data.table)
require(broom)

#LOAD THE DATA FOR INDIVIDUAL ELEMENTS
load("C:/Users/giles/OneDrive/MBA/Dissertation/Data/MonthlyReturnsWithOil.Rdata")

DATA_1 = ReturnsDataWithOil %>%
         filter(SubIndustry == "Passenger Airlines") %>%
         filter(Year >= 2007) %>%
         mutate(TimeGroup1 = ifelse(Year >= 2007 & Year <= 2012 , TRUE, FALSE)) %>%
         mutate(TimeGroup2 = ifelse(Year >= 2018 & Year <= 2023 , TRUE, FALSE));

#GET LIST OF SYMBOLS
symbolList = DATA_1 %>%
             ungroup() %>%
             select(Symbol,SymbolDesc) %>%
             distinct();

#ALL OUTPUTS
allOut= data.frame();

#LOOP AIRLINES
for (iSymbol in 1:dim(symbolList)[1])
{
  #GET THE AIRLINES
  thisSymbol = symbolList$Symbol[iSymbol];
  
  #CREATE THE SUBSETS OF dATA
  Subset_TF1 = DATA_1 %>%
               filter(Symbol == thisSymbol) %>%
               filter(TimeGroup1 == TRUE);
  
  Subset_TF2 = DATA_1 %>%
              filter(Symbol == thisSymbol) %>%
              filter(TimeGroup2 == TRUE);
  
  #CALC BETAS FOR TIMEFRAME 1
  LM_TF1A = lm(data = Subset_TF1,AbnormalReturnAnnualized ~ SP500AbnormalReturnAnnualized + OilAbnormalReturnAnnualized);
  LM_TF1B = lmrob(data = Subset_TF1,AbnormalReturnAnnualized ~ SP500AbnormalReturnAnnualized + OilAbnormalReturnAnnualized);
  
  LM_TF2A = lm(data = Subset_TF2,AbnormalReturnAnnualized ~ SP500AbnormalReturnAnnualized + OilAbnormalReturnAnnualized);
  LM_TF2B = lmrob(data = Subset_TF2,AbnormalReturnAnnualized ~ SP500AbnormalReturnAnnualized + OilAbnormalReturnAnnualized);
  
  #TIDY IT ALL UP 
  OUT_1 = tidy(LM_TF1A) %>% mutate(Symbol = thisSymbol, Regression = "OLS",Timeframe = "2007-2012") %>%
          rbind(tidy(LM_TF1B) %>% mutate(Symbol = thisSymbol,Regression = "ROB",Timeframe = "2007-2012")) %>%
          rbind(tidy(LM_TF2A) %>% mutate(Symbol = thisSymbol,Regression = "OLS",Timeframe = "2018-2023")) %>%
          rbind(tidy(LM_TF2B) %>% mutate(Symbol = thisSymbol,Regression = "ROB",Timeframe = "2018-2023"));
  
  #COMBINE OUTPUTS
  allOut = allOut %>% 
           rbind(OUT_1)
}


#CREATE THE OUTPUT TABLE
Out_Format1A = allOut %>%
              filter(Regression == "OLS") %>%
              mutate(estimate = ifelse(p.value > 0.05,"Insig",round(estimate,4))) %>%
              filter(term != "(Intercept)") %>%
              select(Symbol,Timeframe,term,estimate);

Out_Format2A = Out_Format1A %>%
              pivot_wider(names_from = c("term","Timeframe"),values_from = "estimate")

Out_Format1B = allOut %>%
  filter(Regression == "ROB") %>%
  mutate(estimate = ifelse(p.value > 0.05,"Insig",round(estimate,4))) %>%
  filter(term != "(Intercept)") %>%
  select(Symbol,Timeframe,term,estimate);

Out_Format2B = Out_Format1B %>%
  pivot_wider(names_from = c("term","Timeframe"),values_from = "estimate")

Out_Format3A = allOut %>%
  filter(Regression == "OLS") %>%
  mutate(estimate = ifelse(p.value > 0.05,"Insig",round(estimate,4))) %>%
  filter(term != "(Intercept)") %>%
  select(Symbol,Timeframe,term,estimate,p.value);

Out_Format3A = Out_Format3A %>%
  pivot_wider(names_from = c("term","Timeframe"),values_from = c("estimate","p.value"))
