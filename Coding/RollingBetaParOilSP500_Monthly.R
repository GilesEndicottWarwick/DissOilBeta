require(tidyverse)
require(robustbase)
require(quantreg)
require(foreach)
require(doParallel)
require(dplyr)
require(broom)

parPackages =c("dplyr","tidyverse","robustbase","quantreg","broom");

#PARALLEL COMPUTATION SETUP
cores    = detectCores();
Cluster1 = makeCluster(cores[1]-2) #not to overload your computer
registerDoParallel(Cluster1)


#LOAD THE DATA FOR WHOLE INDUSTRIES
load("C:/Users/giles/OneDrive/MBA/Dissertation/Data/MonthlyIndustryReturns.Rdata");

#COMBINE THE TWO DATASETS
ReturnsDataWithOil = (IndustryReturnsDataWithOil_OUT);

#GET AIRLINES AND ENERGY COMPANIES OUT
allSymbols = ReturnsDataWithOil %>%
             ungroup() %>%
             select(Industry,SubIndustry,Symbol,SymbolDesc) %>%
             distinct() %>%
             filter(Industry == "Energy" | SubIndustry == "Passenger Airlines") %>%
             mutate(Group = ifelse(SubIndustry == "Passenger Airlines","Passenger Airlines",Industry)) %>%
             filter(Symbol == "Energy")


#TRANSFER BACK TO OLD FORMAT
allReturnsData = ReturnsDataWithOil;

#HOW MANY TIEMSTEPS TO USE FOR BETA CALC IE 24 = 2Y BETA
betaCalcLength = ( 12 );

startYear = 1990;
endYear   = 2023;

#THE DATASTORAGE BIN
collectBetaData = data.frame();
collectRawData = data.frame();

#GET THE TIMING
startTime=proc.time();

#RUN THE LOOP
collectRegressionData = foreach(iSymbol = 1:dim(allSymbols)[1], .combine=rbind , .packages = parPackages ) %dopar% 
{

  #CHOOSE ONE COMPANY
  extractSymbol   = allSymbols$Symbol[iSymbol];
  thisSymbolGroup = allSymbols$Group[iSymbol];
  
  #EXTRACT ONE SYMBOL
  thisSymbol = allReturnsData %>%
                filter(Symbol== extractSymbol) %>%
                mutate(YearMonth = paste(Year,ifelse(Month<10,paste("0",Month,sep=""),Month),sep="")) %>%
                rename(SymbolRet = AbnormalReturnAnnualized) %>%
                rename(MarketRet = SP500AbnormalReturnAnnualized) %>%
                rename(OilRet =OilAbnormalReturnAnnualized) %>%
                ungroup() %>%
                dplyr::select(SymbolDesc,Year,YearMonth,SymbolRet,MarketRet,OilRet);
 

  #TEST LINEAR REGRESSION 
  testReg1A = lm(data = thisSymbol, MarketRet  ~  OilRet);
  summary(testReg1A)
  
  #TEST ROBUST REGRESSION
  testReg2A = lmrob(data = thisSymbol,  MarketRet  ~  OilRet )
  summary(testReg2A)
  
  #TEST QUANTILE REGRESSION
  testReg3A = rq(data = thisSymbol, MarketRet  ~  OilRet )
  summary(testReg3A)
  
  collectRegressionData = data.frame();
          
  #LOOP THROUGH TIME
  for( iTime in startYear:endYear)
  {
      #SUBSET THE DATA
      thisSubset = thisSymbol %>% filter (Year == iTime)
    
      #DO THIS REGRESSION 
      subsetReg1A = lm(data = thisSubset, SymbolRet ~ MarketRet + OilRet);

      #GET ALL DATA INCLUDING P VALUES FOR OLS
      T1A_OLS_OM = tidy(subsetReg1A) %>%
                   mutate(R2 = (summary(subsetReg1A))[[8]] ) %>%
                   mutate(Model = "Oil asF SP500") %>%
                   mutate(Regression = "OLS");
      
      T1_OLS_ALL = T1A_OLS_OM %>%
                   mutate(Symbol             = extractSymbol)  %>%
                   mutate(SymbolGroup        = thisSymbolGroup) %>%
                   mutate(SymbolDesc         = thisSubset$SymbolDesc[1])  %>%
                   mutate(Index              = iTime)  %>%
                   mutate(start              = min(thisSubset$YearMonth)) %>%
                   mutate(end                = max(thisSubset$YearMonth));
      
      
      #ROBUST REGRESSION 
      subsetReg2A = lmrob(data = thisSubset,  SymbolRet ~ MarketRet + OilRet );
      R2_ROB_OIL   = (summary(subsetReg2A))[[12]];
      
      #GET ALL DATA INCLUDING P VALUES FOR ROBUST REGRESSION
      T2A_ROB_OM = tidy(subsetReg2A) %>%
        mutate(R2 = (summary(subsetReg2A))[[12]] ) %>%
        mutate(Model = "Oil asF SP500") %>%
        mutate(Regression = "ROB");
      
      T2_ROB_ALL = T2A_ROB_OM %>%
                  mutate(Symbol             = extractSymbol)  %>%
                  mutate(SymbolGroup        = thisSymbolGroup) %>%
                  mutate(SymbolDesc         = thisSubset$SymbolDesc[1])  %>%
                  mutate(Index              = iTime)  %>%
                  mutate(start              = min(thisSubset$YearMonth)) %>%
                  mutate(end                = max(thisSubset$YearMonth));

      #QUANTILE REGRESSION
      subsetReg3A = rq(data = thisSubset, SymbolRet ~ MarketRet + OilRet );
      
      
      #GET ALL DATA INCLUDING P VALUES FOR QUANTILE REGRESSION
      T3A_QR_OM = tidy(subsetReg3A) %>%
        mutate(R2 = NA ) %>%
        mutate(Model = "Oil asF SP500") %>%
        mutate(Regression = "QR");
      
      T3_QR_ALL = T3A_QR_OM %>%
        mutate(Symbol             = extractSymbol)  %>%
        mutate(SymbolGroup        = thisSymbolGroup) %>%
        mutate(SymbolDesc         = thisSubset$SymbolDesc[1])  %>%
        mutate(Index              = iTime)  %>%
        mutate(start              = min(thisSubset$YearMonth)) %>%
        mutate(end                = max(thisSubset$YearMonth)) %>%
        rename(std.error          = conf.low) %>%
        rename(statistic          = conf.high) %>%
        rename(p.value            = tau) %>%
        mutate(p.value = NA) %>%
        mutate(std.error = NA) %>%
        mutate(statistic = NA)

      #COMBINE THE DATA
      collectRegressionData = collectRegressionData %>%
                              rbind(T1_OLS_ALL) %>%
                              rbind(T2_ROB_ALL) %>%
                              rbind(T3_QR_ALL);
    
  }  #END TIME LOOP 
  
  return(collectRegressionData)

}  #END SYMBOLS LOOP 

#stop cluster
stopCluster(Cluster1)

#END OF TIMING
endTime=proc.time();
timeTaken=(endTime-startTime)[3];
print(paste(timeTaken, " seconds were required",sep=""))



#WRITE AN OUTPUT FILE
save(collectRegressionData,file="C:/Users/giles/OneDrive/MBA/Dissertation/Data/RollingBetaIndustryMonthly.Rdata")

