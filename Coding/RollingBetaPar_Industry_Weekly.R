require(tidyverse)
require(robustbase)
require(quantreg)
require(foreach)
require(doParallel)
require(dplyr)
require(broom)
require(lubridate)

parPackages =c("dplyr","tidyverse","robustbase","quantreg","broom");

#PARALLEL COMPUTATION SETUP
cores    = detectCores();
Cluster1 = makeCluster(cores[1]-2) #not to overload your computer
registerDoParallel(Cluster1)

#LOAD THE DATA FOR INDIVIDUAL ELEMENTS
#load("C:/Users/giles/OneDrive/MBA/Dissertation/Data/MonthlyReturnsWithOil.Rdata")

#LOAD THE DATA FOR WHOLE INDUSTRIES
load("C:/Users/giles/OneDrive/MBA/Dissertation/Data/MonthlyIndustryReturnsWeekly.Rdata");

#COMBINE THE TWO DATASETS
ReturnsDataWithOil = data.frame() %>%
                     rbind(IndustryReturnsDataWithOil_OUT_Weekly);

#GET AIRLINES AND ENERGY COMPANIES OUT
allSymbols = ReturnsDataWithOil %>%
             ungroup() %>%
             select(Industry,SubIndustry,Symbol,SymbolDesc) %>%
             distinct() %>%
             filter(Industry == "Energy" | SubIndustry == "Passenger Airlines") %>%
             mutate(Group = ifelse(SubIndustry == "Passenger Airlines","Passenger Airlines",Industry))


#TRANSFER BACK TO OLD FORMAT
allReturnsData = ReturnsDataWithOil;

#HOW MANY TIEMSTEPS TO USE FOR BETA CALC IE 24 = 2Y BETA, 52 weeks = 1 year
betaCalcLength = ( 52 * 1 );

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
                mutate(YearWeek = paste(Year,ifelse(Week<10,paste("0",Week,sep=""),Week),sep="")) %>%
                rename(SymbolRet = AbnormalReturnAnnualized) %>%
                rename(MarketRet = SP500AbnormalReturnAnnualized) %>%
                rename(OilRet = OilAbnormalReturnAnnualized) %>%
                ungroup() %>%
                dplyr::select(SymbolDesc,YearWeek,SymbolRet,MarketRet,OilRet);

  #COMBINE THEM
  combData1 = thisSymbol %>%
              filter(!is.na(SymbolRet)) %>%
              filter(!is.na(MarketRet)) %>%
              filter(!is.na(OilRet));
  
  #COMBINE RAW DATA
  collectRawData = collectRawData %>% rbind(combData1);
  
  
  #TEST LINEAR REGRESSION 
  testReg1A = lm(data = combData1,SymbolRet ~ MarketRet + OilRet);
  testReg1B = lm(data = combData1,SymbolRet ~ MarketRet);
  summary(testReg1A)
  summary(testReg1B)
  
  #TEST ROBUST REGRESSION
  testReg2A = lmrob(data = combData1, SymbolRet ~ MarketRet + OilRet )
  testReg2B = lmrob(data = combData1, SymbolRet ~ MarketRet )
  summary(testReg2A)
  summary(testReg2B)
  
  #TEST QUANTILE REGRESSION
  testReg3A = rq(data = combData1, SymbolRet ~ MarketRet + OilRet )
  testReg3B = rq(data = combData1, SymbolRet ~ MarketRet )
  summary(testReg3A)
  summary(testReg3B)
  
  #CLAC BASIC LOOP PRINCIPLES
  numberTimeSteps = dim(combData1)[1];
  numberBetas     = dim(combData1)[1] - betaCalcLength + 1;
  
  collectBetaDataT = data.frame();
  collectRegressionData = data.frame();
          
  #LOOP THROUGH TIME
  for( iTime in 1:(dim(combData1)[1]-betaCalcLength+1))
  {
      #SUBSET THE DATA
      thisSubset = combData1[iTime:(iTime+betaCalcLength),];
    
      #DO THIS REGRESSION 
      subsetReg1A = lm(data = thisSubset,SymbolRet ~ MarketRet + OilRet);
      subsetReg1B = lm(data = thisSubset,SymbolRet ~ MarketRet);
      R2_OLS_OIL   = (summary(subsetReg1A))[[8]];
      R2_OLS_NOOIL = (summary(subsetReg1B))[[8]];
      OLS_OIL_IMP = R2_OLS_OIL - R2_OLS_NOOIL;
      
      #GET ALL DATA INCLUDING P VALUES FOR OLS
      T1A_OLS_OM = tidy(subsetReg1A) %>%
                   mutate(R2 = (summary(subsetReg1A))[[8]] ) %>%
                   mutate(Model = "Oil and SP500") %>%
                   mutate(Regression = "OLS");
      
      T1B_OLS_M = tidy(subsetReg1B) %>%
                  mutate(R2 = (summary(subsetReg1B))[[8]] ) %>%
                  mutate(Model = "SP500 Only") %>%
                  mutate(Regression = "OLS");
      
      T1_OLS_ALL = T1A_OLS_OM %>%
                   rbind(T1B_OLS_M) %>%
                   mutate(Symbol             = extractSymbol)  %>%
                   mutate(SymbolGroup        = thisSymbolGroup) %>%
                   mutate(SymbolDesc         = thisSubset$SymbolDesc[1])  %>%
                   mutate(Index              = iTime)  %>%
                   mutate(start              = min(thisSubset$YearWeek)) %>%
                   mutate(end                = max(thisSubset$YearWeek));
      
      #EXTRACT DESIRED OLS PARAMETERS
      marketCoeff    = subsetReg1B$coefficients;
      marketOilCoeff = subsetReg1A$coefficients;
      
      #ROBUST REGRESSION 
      subsetReg2A = lmrob(data = thisSubset, SymbolRet ~ MarketRet + OilRet );
      subsetReg2B = lmrob(data = thisSubset, SymbolRet ~ MarketRet );
      R2_ROB_OIL   = (summary(subsetReg2A))[[12]];
      R2_ROB_NOOIL = (summary(subsetReg2B))[[12]];
      ROB_OIL_IMP = R2_ROB_OIL - R2_ROB_NOOIL;
      
      #GET ALL DATA INCLUDING P VALUES FOR ROBUST REGRESSION
      T2A_ROB_OM = tidy(subsetReg2A) %>%
        mutate(R2 = (summary(subsetReg2A))[[12]] ) %>%
        mutate(Model = "Oil and SP500") %>%
        mutate(Regression = "ROB");
      
      T2B_ROB_M = tidy(subsetReg2B) %>%
        mutate(R2 = (summary(subsetReg2B))[[12]] ) %>%
        mutate(Model = "SP500 Only") %>%
        mutate(Regression = "ROB");
      
      T2_ROB_ALL = T2A_ROB_OM %>%
                  rbind(T2B_ROB_M) %>%
                  mutate(Symbol             = extractSymbol)  %>%
                  mutate(SymbolGroup        = thisSymbolGroup) %>%
                  mutate(SymbolDesc         = thisSubset$SymbolDesc[1])  %>%
                  mutate(Index              = iTime)  %>%
                  mutate(start              = min(thisSubset$YearWeek)) %>%
                  mutate(end                = max(thisSubset$YearWeek));
      

      
      
      #EXTRACT DESIRED ROBUST PARAMETERS
      marketCoeffRobust    = subsetReg2B$coefficients;
      marketOilCoeffRobust = subsetReg2A$coefficients;
      
      #QUANTILE REGRESSION
      subsetReg3A = rq(data = thisSubset, SymbolRet ~ MarketRet + OilRet );
      subsetReg3B = rq(data = thisSubset, SymbolRet ~ MarketRet );
      
      #EXTRACT DESIRED QUANTILE PARAMETERS
      marketCoeffQuantile    = subsetReg3B$coefficients;
      marketOilCoeffQuantile = subsetReg3A$coefficients;
      
      #GET ALL DATA INCLUDING P VALUES FOR QUANTILE REGRESSION
      T3A_QR_OM = tidy(subsetReg3A) %>%
        mutate(R2 = NA ) %>%
        mutate(Model = "Oil and SP500") %>%
        mutate(Regression = "QR");
      
      T3B_QR_M = tidy(subsetReg3B) %>%
        mutate(R2 = NA ) %>%
        mutate(Model = "SP500 Only") %>%
        mutate(Regression = "QR");
      
      T3_QR_ALL = T3A_QR_OM %>%
        rbind(T3B_QR_M) %>%
        mutate(Symbol             = extractSymbol)  %>%
        mutate(SymbolGroup        = thisSymbolGroup) %>%
        mutate(SymbolDesc         = thisSubset$SymbolDesc[1])  %>%
        mutate(Index              = iTime)  %>%
        mutate(start              = min(thisSubset$YearWeek)) %>%
        mutate(end                = max(thisSubset$YearWeek)) %>%
        rename(std.error          = conf.low) %>%
        rename(statistic          = conf.high) %>%
        rename(p.value            = tau) %>%
        mutate(p.value = NA) %>%
        mutate(std.error = NA) %>%
        mutate(statistic = NA)

      #FORM THE OUTPUT 
      thisOutputRow = data.frame(
                                Symbol             = extractSymbol,
                                SymbolGroup        = thisSymbolGroup,
                                SymbolDesc         = thisSubset$SymbolDesc[1],
                                Index              = iTime,
                                start              = min(thisSubset$YearWeek),
                                end                = max(thisSubset$YearWeek),
                                
                                marketBetaBasicOLS    = marketCoeff[2],
                                marketBetaOilModelOLS = marketOilCoeff[2],
                                oilBetaOilModelOLS    = marketOilCoeff[3],
                                OilBetaRsqEffOLS      = OLS_OIL_IMP,
                                
                                marketBetaBasicQua    = marketCoeffQuantile[2],
                                marketBetaOilModelQua = marketOilCoeffQuantile[2],
                                oilBetaOilModelQua    = marketOilCoeffQuantile[3],
                                
                                marketBetaBasicRob    = marketCoeffRobust[2],
                                marketBetaOilModelRob = marketOilCoeffRobust[2],
                                oilBetaOilModelRob    = marketOilCoeffRobust[3],
                                OilBetaRsqEffRob      = ROB_OIL_IMP
                                );
      
      #COMBINE THE DATA
      collectRegressionData = collectRegressionData %>%
                              rbind(T1_OLS_ALL) %>%
                              rbind(T2_ROB_ALL) %>%
                              rbind(T3_QR_ALL)
    
      #COMBINE TO MAINDATASET
      #collectBetaDataT =  collectBetaDataT %>% rbind(thisOutputRow)
      
  }  #END TIME LOOP 
  
  return(collectRegressionData)

}  #END SYMBOLS LOOP 

#stop cluster
stopCluster(Cluster1)

#END OF TIMING
endTime=proc.time();
timeTaken=(endTime-startTime)[3];
print(paste(timeTaken, " seconds were required",sep=""))


#CALCULATE R2 IMPROVEMENT OF OIL TERM
R2_OilImp =  collectRegressionData %>%
             select(Symbol,Model,Regression,R2,start,end) %>%
             distinct() %>%
             pivot_wider(names_from=c("Model"),values_from=c("R2")) %>%
             mutate(R2_Oil_Imp = `Oil and SP500` - `SP500 Only`) %>%
             select(Symbol,Regression,,start,end,R2_Oil_Imp)


collectRegressionData = collectRegressionData %>%
                        left_join(R2_OilImp,by=c("Symbol","Regression","start","end"));


#GET THE MONDAY OF EVERY YEARWEEK
DateSeq1 = data.frame(Date = seq(from=ymd("19890101"), to =ymd("20231231"),"days")) %>%
           mutate(Day = lubridate::wday(Date, label = TRUE)) %>%
           mutate(Week = week(Date)) %>%
           mutate(Year = year(Date)) %>%
           mutate(YearWeek = paste(Year,ifelse(Week<10,paste("0",Week,sep=""),Week),sep="")) %>%
           filter(Day == "Mon") %>%
           select(YearWeek,Date)

#ADD THE DATE OF THE MONDAY TO THE START YEARWEEK
collectRegressionData = collectRegressionData %>%
                        left_join(DateSeq1,by=c("start"="YearWeek"))

#WRITE AN OUTPUT FILE
save(collectRegressionData,file="C:/Users/giles/OneDrive/MBA/Dissertation/Data/RollingBetaWeekly.Rdata")

