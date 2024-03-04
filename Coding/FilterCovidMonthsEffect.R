require(tidyverse)
require(robustbase)
require(quantreg)
require(foreach)
require(doParallel)
require(dplyr)
require(broom)

#LOAD THE DATA FOR WHOLE INDUSTRIES
load("C:/Users/giles/OneDrive/MBA/Dissertation/Data/MonthlyIndustryReturns.Rdata");

#CREATE A FILTERED AND NON FILTERED VERSION OF THE DATA
D1 = IndustryReturnsDataWithOil_OUT %>%
      filter(Symbol== "Energy") %>%
      ungroup() %>%
      mutate(RankDate = rank(desc(Date))) %>%
      filter(RankDate <= 12 * 5);

D1_All = D1;
D1_Fil = D1 %>%
              filter(!(YearMonth %in% c("202003","202004","202005")));



#RUN ALL THE CALCULATIONS
LM_OLS_ALL = lm(data = D1_All, AbnormalReturnAnnualized  ~ SP500ReturnAnnualized  +  OilReturnAnnualized);
LM_OLS_FIL = lm(data = D1_Fil, AbnormalReturnAnnualized  ~ SP500ReturnAnnualized  +  OilReturnAnnualized);

LM_ROB_ALL = lmrob(data = D1_All, AbnormalReturnAnnualized  ~ SP500ReturnAnnualized  +  OilReturnAnnualized);
LM_ROB_FIL = lmrob(data = D1_Fil, AbnormalReturnAnnualized  ~ SP500ReturnAnnualized  +  OilReturnAnnualized);

LM_QR_ALL = rq(data = D1_All, AbnormalReturnAnnualized  ~ SP500ReturnAnnualized  +  OilReturnAnnualized);
LM_QR_FIL = rq(data = D1_Fil, AbnormalReturnAnnualized  ~ SP500ReturnAnnualized  +  OilReturnAnnualized);

RES_ALL_OLS = tidy(LM_OLS_ALL) %>% select(term,estimate) %>%mutate(Data = "All") %>% mutate(Regression = "OLS");
RES_FIL_OLS = tidy(LM_OLS_FIL) %>% select(term,estimate) %>%mutate(Data = "Fil") %>% mutate(Regression = "OLS");
RES_ALL_ROB = tidy(LM_ROB_ALL) %>% select(term,estimate) %>%mutate(Data = "All") %>% mutate(Regression = "ROB");
RES_FIL_ROB = tidy(LM_ROB_FIL) %>% select(term,estimate) %>%mutate(Data = "Fil") %>% mutate(Regression = "ROB");
RES_ALL_QR  = tidy(LM_QR_ALL) %>% select(term,estimate) %>%mutate(Data = "All") %>% mutate(Regression = "QR");
RES_FIL_QR  = tidy(LM_QR_FIL) %>% select(term,estimate) %>%mutate(Data = "Fil") %>% mutate(Regression = "QR");

RES_ALL = RES_ALL_OLS %>%
          rbind(RES_FIL_OLS) %>%
          rbind(RES_ALL_ROB) %>%
          rbind(RES_FIL_ROB) %>%
          rbind(RES_ALL_QR) %>%
          rbind(RES_FIL_QR);

RES_ALL_EASY = RES_ALL %>%
               filter(term != "(Intercept)") %>%
               pivot_wider(values_from="estimate",names_from =c("Regression","Data"))
