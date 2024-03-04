require(tidyverse)
require(robustbase)
require(quantreg)
require(foreach)
require(doParallel)
require(dplyr)
require(broom)
require(data.table)

#LOAD THE DATA IN
load ("C:/Users/giles/OneDrive/MBA/Dissertation/Data/RollingBetaSensitivtyFull.Rdata")

#LETS START BY BASIC FILTERING
D1 = collectRegressionData %>%
     filter(Model  == "Oil asF SP500 + Oil") %>%
   #  filter(Symbol == "APA") %>%
     filter(term == "OilAbnormalReturnAnnualized") %>%
     select(SymbolGroup,Symbol,SymbolDesc,start,end,Regression,estimate) %>%
     mutate(start = paste(start,"01",sep="")) %>%
     mutate(end   = paste(end,"01",sep="")) %>%
     mutate(start = ymd(start)) %>%
     mutate(end = ymd(end)) %>%
     mutate(DiffDate = as.numeric(end - start)) %>%
     filter(!(Symbol %in% c("Energy","Passenger Airlines")))
  

#GET THE TEN YEAR VALUE FOR EACH START DATE
FINAL_VALUES = D1 %>%
               group_by(SymbolGroup,Symbol,SymbolDesc,end,Regression) %>%
               mutate(StartRank = frank(start)) %>%
               filter(StartRank == 1) %>%
               rename(FinalValue = estimate) %>%
               select(-start) %>%
               filter(DiffDate >= (365*10 -20)) %>%
               select(-DiffDate)
    

#COMBINE ALL 
D2 = D1 %>%
     left_join(FINAL_VALUES, by =c ("SymbolGroup","Symbol","SymbolDesc","end","Regression")) %>%
     mutate(Delta = FinalValue - estimate) %>%
     mutate(absDelta = abs(Delta)) %>%
     filter(!is.na(FinalValue)) %>%
     mutate(DiffYears = round(DiffDate/365));


#PROCESS AND SUMMARISE THESE TO SYMBOL LEVEL
D3 = D2 %>%
     ungroup() %>%
     group_by(SymbolGroup,Symbol,Regression,DiffYears) %>%
     summarise(
                meanAbsDelta      = mean(absDelta),
                medianAbsDelta    = median(absDelta), 
                meanRelAbsDelta   = mean(absDelta / FinalValue),
                medianRelAbsDelta = median(absDelta / FinalValue)
              )

#NOW DO OVER ALL SYMBOLS BUT KEEP INDUSTRY GROUPINGS
D4 = D2 %>%
      ungroup() %>%
      group_by(SymbolGroup,Regression,DiffYears) %>%
      summarise(
        meanAbsDelta      = mean(absDelta),
        medianAbsDelta    = median(absDelta), 
        meanRelAbsDelta   = mean(absDelta / abs(FinalValue)),
        medianRelAbsDelta = median(absDelta / abs(FinalValue))
      )

D4A = D2 %>%
      filter(SymbolGroup == "Energy") %>%
      filter(DiffYears == 1) %>%
      filter(Regression == "OLS") %>%
      ungroup()

D4B = D4 %>%
      filter(SymbolGroup == "Energy") %>%
      filter(DiffYears == 1) %>%
      filter(Regression == "OLS")

median(D4A$absDelta)

#CHEEKY TABLEAU EXPORT
write.table(D4,"clipboard-16384",sep=";",row.names=F,quote=F)
