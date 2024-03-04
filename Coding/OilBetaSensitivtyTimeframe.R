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

#LOAD THE DATA FOR INDIVIDUAL ELEMENTS
load("C:/Users/giles/OneDrive/MBA/Dissertation/Data/MonthlyReturnsWithOil.Rdata")

#LOAD THE DATA FOR WHOLE INDUSTRIES
load("C:/Users/giles/OneDrive/MBA/Dissertation/Data/MonthlyIndustryReturns.Rdata");

#COMBINE THE TWO DATASETS
ReturnsDataWithOil = ReturnsDataWithOil %>%
  rbind(IndustryReturnsDataWithOil_OUT);

#GET AIRLINES AND ENERGY COMPANIES OUT
allSymbols = ReturnsDataWithOil %>%
  group_by( Industry,SubIndustry,Symbol,SymbolDesc) %>%
  mutate(FirstDate = min(Date)) %>%
  mutate(LastDate  = max(Date)) %>%
  ungroup() %>%
  select(Industry,SubIndustry,Symbol,SymbolDesc,FirstDate,LastDate) %>%
  distinct() %>%
  filter(Industry == "Energy" | SubIndustry == "Passenger Airlines") %>%
  mutate(Group = ifelse(SubIndustry == "Passenger Airlines","Passenger Airlines",Industry))



#TRANSFER BACK TO OLD FORMAT
allReturnsData = ReturnsDataWithOil;

#MIN TIMESTEPS
minTimeFrame = 12;
maxTimeFrame = 12 * 20;

#MIN AND MAX FROM DATASET
firstData = min(allReturnsData$Date);
lastData  = max(allReturnsData$Date);

#GET LIST OF POSSIBLE DATES
EndDates = ReturnsDataWithOil %>%
           filter(Symbol =="Energy") %>%
           pull(Date) %>%
           unique();

#START DATES IS THE SAME
StartDates = EndDates;

#DATA STORGAE VESSEL
collectRegressionData = data.frame()
           

thisSymbolGroup = "Energy"

#CREATE ALL RUNS DATAFRAME
allRuns = expand.grid(StartDate = StartDates,EndDate = EndDates,Symbol =allSymbols$Symbol) %>%
          filter(EndDate > StartDate) %>%
          mutate(DiffDate = as.numeric(EndDate - StartDate)) %>%
          filter(DiffDate >= 364 ) %>%
          filter(DiffDate <= 366*10) %>%
          mutate(DiffYear = DiffDate /365) %>%
          mutate(DiffYearRem = DiffDate %% 365) %>%
          mutate(Month1 = month(StartDate)) %>%
          mutate(Month2 = month(EndDate)) %>%
  #        filter(Month1 == 1 & Month2 == 1) %>%
          filter(Month1 == Month2) %>%
          left_join(allSymbols %>% select(Symbol,FirstDate,LastDate) ,by=c("Symbol")) %>%
          filter(StartDate >= FirstDate) %>%
          filter(EndDate <= LastDate)

#GET THE TIMING
startTime=proc.time();  

#RUN THE LOOP USING CLUSTER
collectRegressionData = foreach(iRun = 1:dim(allRuns)[1], .combine=rbind , .packages = parPackages ) %dopar% 
    {

    #GRAB THE RELEVANT DATES
    thisEndDate   = allRuns$EndDate[iRun];
    thisStartDate = allRuns$StartDate[iRun];
    thisSymbol    = allRuns$Symbol[iRun];
    
    #GET THE DATASET
    thisDataset = allReturnsData %>%
                    filter(Symbol == thisSymbol) %>%
                    filter(Date >= thisStartDate) %>%
                    filter(Date <= thisEndDate);
    
    if(dim(thisDataset)[1] >= 12)
    {
  
      
      #DO THE ANALYSIS
      #DO THIS REGRESSION 
      subsetReg1A = lm(data = thisDataset, AbnormalReturnAnnualized ~ SP500AbnormalReturnAnnualized  +  OilAbnormalReturnAnnualized);
      
      #GET ALL DATA INCLUDING P VALUES FOR OLS
      T1A_OLS_OM = tidy(subsetReg1A) %>%
        mutate(R2 = (summary(subsetReg1A))[[8]] ) %>%
        mutate(Model = "Oil asF SP500 + Oil") %>%
        mutate(Regression = "OLS");
      
      T1_OLS_ALL = T1A_OLS_OM %>%
        mutate(Symbol             = thisSymbol)  %>%
        mutate(SymbolGroup        = thisDataset$Industry[1]) %>%
        mutate(SymbolDesc         = thisDataset$SymbolDesc[1])  %>%
        mutate(start              = min(thisDataset$YearMonth)) %>%
        mutate(end                = max(thisDataset$YearMonth));
      
      #ROBUST REGRESSION 
      subsetReg2A = lmrob(data = thisDataset,  AbnormalReturnAnnualized ~ SP500AbnormalReturnAnnualized  +  OilAbnormalReturnAnnualized );
      R2_ROB_OIL   = (summary(subsetReg2A))[[12]];
      
      #GET ALL DATA INCLUDING P VALUES FOR ROBUST REGRESSION
      T2A_ROB_OM = tidy(subsetReg2A) %>%
        mutate(R2 = (summary(subsetReg2A))[[12]] ) %>%
        mutate(Model = "Oil asF SP500 + Oil") %>%
        mutate(Regression = "ROB");
      
      T2_ROB_ALL = T2A_ROB_OM %>%
        mutate(Symbol             = thisSymbol)  %>%
        mutate(SymbolGroup        = thisDataset$Industry[1]) %>%
        mutate(SymbolDesc         = thisDataset$SymbolDesc[1])  %>%
        mutate(start              = min(thisDataset$YearMonth)) %>%
        mutate(end                = max(thisDataset$YearMonth));
      
      #QUANTILE REGRESSION
      subsetReg3A = rq(data = thisDataset,  AbnormalReturnAnnualized ~ SP500AbnormalReturnAnnualized  +  OilAbnormalReturnAnnualized );
      
      
      #GET ALL DATA INCLUDING P VALUES FOR QUANTILE REGRESSION
      T3A_QR_OM = tidy(subsetReg3A) %>%
        mutate(R2 = NA ) %>%
        mutate(Model = "Oil asF SP500 + Oil") %>%
        mutate(Regression = "QR");
      
      T3_QR_ALL = T3A_QR_OM %>%
        mutate(Symbol             = thisSymbol)  %>%
        mutate(SymbolGroup        = thisDataset$Industry[1]) %>%
        mutate(SymbolDesc         = thisDataset$SymbolDesc[1])  %>%
        mutate(start              = min(thisDataset$YearMonth)) %>%
        mutate(end                = max(thisDataset$YearMonth)) %>%
        rename(std.error          = conf.high) %>%
        rename(statistic          = conf.low) %>%
        rename(p.value            = tau) %>%
        mutate(p.value = NA) %>%
        mutate(std.error = NA) %>%
        mutate(statistic = NA)
      
      #COMBINE THE DATA
      collectRegressionData = rbind(T1_OLS_ALL) %>%
                              rbind(T2_ROB_ALL) %>%
                              rbind(T3_QR_ALL);
      #PRINT
      
      #RETURN RESULT TO CLUSTER MANAGER
      return(collectRegressionData)
      
    } #END IF 12 datapoints check
      
  
} # END OF END DATE LOOP

#stop cluster
stopCluster(Cluster1)

#END OF TIMING
endTime=proc.time();
timeTaken=(endTime-startTime)[3];
print(paste(timeTaken, " seconds were required",sep=""))

#WRITE AN OUTPUT FILE
save(collectRegressionData,file="C:/Users/giles/OneDrive/MBA/Dissertation/Data/RollingBetaSensitivtyFull.Rdata")
