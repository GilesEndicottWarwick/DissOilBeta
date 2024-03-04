require(tidyverse)
require(lubridate)
require(robustbase)
require(broom)
require(quantreg)

#LOAD THE DATA FILE IN (FROM OILCORRSHARES4.R)
load(file="C:/Users/giles/OneDrive/MBA/Dissertation/Data/MonthlyIndustryReturns.Rdata");

#INCLUDE INDUSTRY SECTOR 


#LIMIT TO 5 YEARS OF DATA
Data_S1 = IndustryReturnsDataWithOil_OUT %>%
            filter(Year > 1989);


#REMOVE PERFECT CORRELATION ENTRIES
Data_S1 = Data_S1 %>%
          filter(!(Symbol %in% c("CL=F","^GSPC","BZ=F","__WTC_D","^TNX","__WTC_D_REAL")))

#GET THE UNIQUE SYMBOLS
symbols = unique(Data_S1$Symbol);

#START THE DATASTORE
allRegData = data.frame();

#LOOP THROUGH ALL SYMBOLS
for (iSymbol in 1:length(symbols))
{
    #STORE THESE PARAMS
    thisSymbol = symbols[iSymbol];

    #LIMIT THE DATASET
    thisSymbolData = Data_S1 %>%
                     filter(Symbol == thisSymbol) %>%
                     as.data.frame();
    
    #DONT DO ANYTHING WITH LIMITED TIMEFRAME SHARES, NEED MIN 5 YEARS OF DATA
    if (dim(thisSymbolData)[1] >= 60-2 )
    {
      
      #SIMPLE OIL CORRELATION
      b1 = lm(   data = thisSymbolData, AbnormalReturnAnnualized ~ OilAbnormalReturnAnnualized);
      c1 = lmrob(data = thisSymbolData, AbnormalReturnAnnualized ~ OilAbnormalReturnAnnualized, setting = "KS2014");
      d1 = rq(   data = thisSymbolData, AbnormalReturnAnnualized ~ OilAbnormalReturnAnnualized  );
      
      #SP500 STANDARD BETA
      b2 = lm(data = thisSymbolData,    AbnormalReturnAnnualized ~ SP500AbnormalReturnAnnualized );
      c2 = lmrob(data = thisSymbolData, AbnormalReturnAnnualized ~ SP500AbnormalReturnAnnualized , setting = "KS2014");
      d2 = rq(   data = thisSymbolData, AbnormalReturnAnnualized ~ SP500AbnormalReturnAnnualized  );
      
      #COMBINED SP500 AND OIL
      b3 = lm(data = thisSymbolData,    AbnormalReturnAnnualized ~ OilAbnormalReturnAnnualized + SP500AbnormalReturnAnnualized);
      c3 = lmrob(data = thisSymbolData, AbnormalReturnAnnualized ~ OilAbnormalReturnAnnualized + SP500AbnormalReturnAnnualized, setting = "KS2014");
      d3 = rq(   data = thisSymbolData, AbnormalReturnAnnualized ~ OilAbnormalReturnAnnualized + SP500AbnormalReturnAnnualized  );
      
      #DUMMY MODEL FOR QQUANNTILE REGRESSSION
      d0 <- rq(AbnormalReturnAnnualized ~ 1,tau=0.5,data=thisSymbolData);

      rho <- function(u,tau=.5)u*(tau - (u < 0));
      d1_R1<- 1 - d1$rho/d0$rho;
      d2_R1<- 1 - d2$rho/d0$rho;
      d3_R1<- 1 - d3$rho/d0$rho;
      
      #TIDY RESULTS FOR STORAGE
      b1_Output = tidy(b1) %>%
        mutate(R2 = summary(b1)$adj.r.squared) %>%
        mutate(Model = "Oil Only") %>%
        mutate(Symbol = thisSymbol) %>%
        mutate(SymbolDesc = thisSymbolData$SymbolDesc[1] ) %>%
        mutate(Regression = "OLS")  %>%
        mutate(Industry   = thisSymbolData$Industry[1]) %>%
        mutate(SubIndustry   = thisSymbolData$SubIndustry[1]);
      
      c1_Output = tidy(c1) %>%
        mutate(R2 = summary(c1)$adj.r.squared) %>%
        mutate(Model = "Oil Only") %>%
        mutate(Symbol = thisSymbol) %>%
        mutate(SymbolDesc = thisSymbolData$SymbolDesc[1] ) %>%
        mutate(Regression = "lmrob Robust")  %>%
        mutate(Industry   = thisSymbolData$Industry[1]) %>%
        mutate(SubIndustry   = thisSymbolData$SubIndustry[1]);
      
      d1_Output = tidy(d1) %>%
        dplyr::select(-conf.low, -conf.high,   -tau  ) %>%
        mutate(std.error = 0) %>%
        mutate(statistic = 0) %>% 
        mutate(p.value = 0) %>%
        mutate(R2 = d1_R1 ) %>%
        mutate(Model = "Oil Only") %>%
        mutate(Symbol = thisSymbol) %>%
        mutate(SymbolDesc = thisSymbolData$SymbolDesc[1] ) %>%
        mutate(Regression = "Quantile Reg")  %>%
        mutate(Industry   = thisSymbolData$Industry[1]) %>%
        mutate(SubIndustry   = thisSymbolData$SubIndustry[1]);
      
      b2_Output = tidy(b2) %>%
        mutate(R2 = summary(b2)$adj.r.squared) %>%
        mutate(Model = "SP500 Only") %>%
        mutate(Symbol = thisSymbol) %>%
        mutate(SymbolDesc = thisSymbolData$SymbolDesc[1] ) %>%
        mutate(Regression = "OLS")  %>%
        mutate(Industry   = thisSymbolData$Industry[1]) %>%
        mutate(SubIndustry   = thisSymbolData$SubIndustry[1]);
      
      c2_Output = tidy(c2) %>%
        mutate(R2 = summary(c2)$adj.r.squared) %>%
        mutate(Model = "SP500 Only") %>%
        mutate(Symbol = thisSymbol) %>%
        mutate(SymbolDesc = thisSymbolData$SymbolDesc[1] ) %>%
        mutate(Regression = "lmrob Robust")  %>%
        mutate(Industry   = thisSymbolData$Industry[1]) %>%
        mutate(SubIndustry   = thisSymbolData$SubIndustry[1]);
      
      d2_Output = tidy(d2) %>%
        dplyr::select(-conf.low, -conf.high,   -tau  ) %>%
        mutate(std.error = 0) %>%
        mutate(statistic = 0) %>% 
        mutate(p.value = 0) %>%
        mutate(R2 = d2_R1 ) %>%
        mutate(Model = "SP500 Only") %>%
        mutate(Symbol = thisSymbol) %>%
        mutate(SymbolDesc = thisSymbolData$SymbolDesc[1] ) %>%
        mutate(Regression = "Quantile Reg")  %>%
        mutate(Industry   = thisSymbolData$Industry[1]) %>%
        mutate(SubIndustry   = thisSymbolData$SubIndustry[1]);
      
      b3_Output = tidy(b3) %>%
        mutate(R2 = summary(b3)$adj.r.squared) %>%
        mutate(Model = "Oil and SP500") %>%
        mutate(Symbol = thisSymbol) %>%
        mutate(SymbolDesc = thisSymbolData$SymbolDesc[1] ) %>%
        mutate(Regression = "OLS")  %>%
        mutate(Industry   = thisSymbolData$Industry[1]) %>%
        mutate(SubIndustry   = thisSymbolData$SubIndustry[1]);
      
      c3_Output = tidy(c3) %>%
        mutate(R2 = summary(c3)$adj.r.squared) %>%
        mutate(Model = "Oil and SP500") %>%
        mutate(Symbol = thisSymbol) %>%
        mutate(SymbolDesc = thisSymbolData$SymbolDesc[1] ) %>%
        mutate(Regression = "lmrob Robust")  %>%
        mutate(Industry   = thisSymbolData$Industry[1]) %>%
        mutate(SubIndustry   = thisSymbolData$SubIndustry[1]);
      
      d3_Output = tidy(d3) %>%
        dplyr::select(-conf.low, -conf.high,   -tau  ) %>%
        mutate(std.error = 0) %>%
        mutate(statistic = 0) %>% 
        mutate(p.value = 0) %>%
        mutate(R2 = d3_R1 ) %>%
        mutate(Model = "Oil and SP500") %>%
        mutate(Symbol = thisSymbol) %>%
        mutate(SymbolDesc = thisSymbolData$SymbolDesc[1] ) %>%
        mutate(Regression = "Quantile Reg")  %>%
        mutate(Industry   = thisSymbolData$Industry[1]) %>%
        mutate(SubIndustry   = thisSymbolData$SubIndustry[1]);
      
      #INPUT MISSIING R SQUARED STATS FOR QUANTILE REGRESSION
      #d2A = lm(data = data.frame(Fitted = d2$fitted.values, Actual = d2$y), Fitted ~ Actual);
      
      #USE QUANTILE REGRESSSION
      #d2_Output = tidy(d2) %>%
      #  mutate(R2 = summary(d2A)$adj.r.squared) %>%
      #  mutate(Model = "SP500 Only") %>%
      #  mutate(Symbol = thisSymbol) %>%
      ##  mutate(SymbolDesc = thisSymbolData$SymbolDesc[1] ) %>%
      ##  mutate(Regression = "Quantile Reg")  %>%
      #  mutate(Industry   = thisSymbolData$Industry[1]) %>%
      #  mutate(SubIndustry   = thisSymbolData$SubIndustry[1])%>%
      #  dplyr::select(-tau);*/
      
      #COMBINE ALL THE REGRESSION TOGETHER
      thisRegData = b2_Output %>%
                    rbind(c2_Output) %>%
                    rbind(d2_Output) %>%
                    rbind(b3_Output) %>%
                    rbind(c3_Output) %>%
                    rbind(d3_Output) %>%
                    rbind(b1_Output) %>%
                    rbind(c1_Output) %>%
                    rbind(d1_Output) %>%
                    mutate(nMonths = dim(thisSymbolData)[1]) %>%
                    mutate(FirstDate = min(thisSymbolData$YearMonth)) %>%
                    mutate(LastDate  = max(thisSymbolData$YearMonth));
        #%>%
                   # rbind(d2_Output);
      
      #STORE IT IN ONE DATASTORE
      allRegData = allRegData  %>%
                    rbind(thisRegData);
      
    }

  
  #SHOW PROGRESS
  print(paste("Component ",iSymbol, " ", thisSymbol," completed",sep=""));
  
  #INCREMENTAL SAVE
  if (iSymbol %% 20 == 0)
  {
   # save(allRegData,file="C:/Users/giles/OneDrive/MBA/Dissertation/Data/AllYear_Market_Beta_Calc.Rdata");
  }
  
} #END SYMBOL LOOP

save(allRegData,file="C:/Users/giles/OneDrive/MBA/Dissertation/Data/AllYear_Market_Beta_Calc_Industry.Rdata");

#GET USEFUL STUFF OUT
usefulOutput = allRegData %>%
               filter(Symbol %in% c("Energy","Passenger Airlines")) %>%
               filter( term != "(Intercept)") %>%
               filter(Model == "Oil and SP500") %>%
               select(Symbol,term,Regression,estimate,R2,p.value)
  

