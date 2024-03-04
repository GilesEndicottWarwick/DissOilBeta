require(tidyverse)
require(lubridate)
require(robustbase)
require(broom)
require(quantreg)

#LOAD THE DATA FILE IN (FROM OILCORRSHARES4.R)
load(file="C:/Users/giles/OneDrive/MBA/Dissertation/Data/MonthlyReturnsWithOil.Rdata");

#LIMIT TO 5 YEARS OF DATA
Data5Y = ReturnsDataWithOil %>%
            filter(Year > year(Sys.time())-6) %>%
            filter(Date >= ymd("20231130") - years(5));


#REMOVE PERFECT CORRELATION ENTRIES
Data5Y = Data5Y %>%
         filter(!(Symbol %in% c("CL=F","^GSPC","BZ=F","__WTC_D","^TNX","__WTC_D_REAL")))

#GET THE UNIQUE SYMBOLS
symbols = unique(Data5Y$Symbol);

#START THE DATASTORE
allRegData = data.frame();

#LOOP THROUGH ALL SYMBOLS
for (iSymbol in 1:length(symbols))
{
    #STORE THESE PARAMS
    thisSymbol = symbols[iSymbol];

    #LIMIT THE DATASET
    thisSymbolData = Data5Y %>%
                     filter(Symbol == thisSymbol) %>%
                     as.data.frame();
    
    #DONT DO ANYTHING WITH LIMITED TIMEFRAME SHARES, NEED 5 YEARS OF DATA
    if (dim(thisSymbolData)[1] >= 60-2 )
    {
      
      #SP500 STANDARD BETA
      b2 = lm(data = thisSymbolData,    AbnormalReturnAnnualized ~ SP500AbnormalReturnAnnualized );
      c2 = lmrob(data = thisSymbolData, AbnormalReturnAnnualized ~ SP500AbnormalReturnAnnualized , setting = "KS2014");
      #d2 = rq(   data = thisSymbolData, AbnormalReturnAnnualized ~ SP500AbnormalReturnAnnualized  );
      

      
      #TIDY RESULTS FOR STORAGE
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
                    rbind(c2_Output); #%>%
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
    save(allRegData,file="C:/Users/giles/OneDrive/MBA/Dissertation/Data/5Y_Market_Beta_Calc.Rdata");
  }
  
} #END SYMBOL LOOP

save(allRegData,file="C:/Users/giles/OneDrive/MBA/Dissertation/Data/5Y_Market_Beta_Calc.Rdata");
