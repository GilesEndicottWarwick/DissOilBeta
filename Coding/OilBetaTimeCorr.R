require(tidyverse)
require(robustbase)
require(quantreg)
require(foreach)
require(doParallel)
require(dplyr)
require(broom)

#LOAD THE INPUT FILE, IT COMES FROM RollingBetaPar.R
load("C:/Users/giles/OneDrive/MBA/Dissertation/Data/RollingBeta.Rdata")


#SIMPLIFY DATASET
TimeComp_1 = collectRegressionData %>%
             filter(term == "OilRet") %>%
             filter(Model == "Oil and SP500") %>%
             filter(Regression  == "OLS") %>%
             select(SymbolGroup,Symbol,SymbolDesc,start,end,estimate,Regression)

#GET COMBOS
CombosToRun = TimeComp_1 %>%
              select(SymbolGroup,Symbol,SymbolDesc) %>%
              distinct() %>%
              arrange(SymbolGroup,Symbol) %>%
              ungroup();

nIndustry = length(unique(CombosToRun$SymbolGroup));

#THE COLLECTION DATA FRAME
allCorDataOut = data.frame();

#thisIndustry = unique(CombosToRun$SymbolGroup) [iIndustry];
  
theseCompanies = CombosToRun %>%
                 distinct() %>%
                 arrange(Symbol)  %>%
                 select(Symbol) %>%
                 as.vector() %>%
                 unlist();
  
nCompanies = length(theseCompanies);
  
#LOOP THE COMPARISONS
for(iComp1 in 1:(nCompanies))
{
  for(iComp2 in 1:nCompanies)
  {
      
      #PICK OUT THE COMPANIES
      thisComp1 = theseCompanies[iComp1];
      thisComp2 = theseCompanies[iComp2];
    
      if (iComp1 == iComp2)
      {
        thisCor =1;
        thisN= 999;
      }else
      {
    
          #CREATE THE DATASET
          thisComparison = TimeComp_1 %>%
                           select(-SymbolDesc,-SymbolGroup) %>%
                           filter(Symbol %in% c(thisComp1,thisComp2)) %>%
                           pivot_wider(names_from = "Symbol",values_from = "estimate") %>%
                           rename(Comp_A = 4) %>%
                           rename(Comp_B = 5) %>%
                           filter(!is.na(Comp_A)) %>%
                           filter(!is.na(Comp_B));
          
          #CALC THE CORRELATION
          thisCor = cor(thisComparison$Comp_A,thisComparison$Comp_B);
          
          #NUMBER OF ENTRIES
          thisN = dim(thisComparison)[1];
          
      } 
      
      
      #BUILD DATAFRAME FOR OUTPUT
      outputRow = data.frame(
                             Comp1  = thisComp1,
                             Comp2  = thisComp2,
                             Cor    = thisCor,
                             nData  = thisN
                          );
      
      #BUILD UP DATA
      allCorDataOut = allCorDataOut %>%
                      rbind(outputRow);
    
  } #END COMP 2 LOOP
} #END COMP 1 LOOP
  

#ADD COLUMSN FOR FILTERING OUT COMPANIES BASED ON ROW COUNT
allCorDataOut = allCorDataOut %>%
                group_by(Comp1) %>%
                mutate(n1=max(ifelse(nData==999,0,nData))) %>%
                group_by(Comp2) %>%
                mutate(n2=max(ifelse(nData==999,0,nData))) %>%
                left_join(CombosToRun %>% select(Symbol,SymbolDesc),by=c("Comp1" = "Symbol")) %>%
                left_join(CombosToRun %>% select(Symbol,SymbolDesc),by=c("Comp2" = "Symbol")) %>%
                rename(Comp1Desc = 7) %>%
                rename(Comp2Desc = 8);

#RE-ADD INDUSTRY
allCorDataOut = allCorDataOut %>%
                left_join(CombosToRun %>% select(Symbol,SymbolGroup),by=c("Comp1"="Symbol")) %>%
                left_join(CombosToRun %>% select(Symbol,SymbolGroup),by=c("Comp2"="Symbol")) %>%
                rename(Comp1Group = 9) %>%
                rename(Comp2Group = 10)

#WRITE AN OUTPUT FILE
save(allCorDataOut,file="C:/Users/giles/OneDrive/MBA/Dissertation/Data/TimeOilBetaCorr.Rdata")
