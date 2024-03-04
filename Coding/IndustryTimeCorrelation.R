require(tidyverse)

#LOAD DATA IN
load("C:/Users/giles/OneDrive/MBA/Dissertation/Data/MonthlyIndustryReturns.Rdata");

#LIST OF INDUSTRIES
ListSymbols = IndustryReturnsDataWithOil_OUT %>%
              ungroup() %>%
              select(Symbol) %>%
              distinct() %>%
              unlist();

nIndustries = length(ListSymbols);

#DATA STORAGE
allCorrs = data.frame();

#OUTER LOOPS FOR COMPANIES
for (iComp1 in 1:(nIndustries-1))
{
  for (iComp2 in (iComp1+1):nIndustries)
  {
    #GRAB THE COMAPNIES
    thisComp1 = ListSymbols[iComp1];
    thisComp2 = ListSymbols[iComp2];
    
    #MAKE THE WHOLE TIME DATASET
    theseCompaniesData = IndustryReturnsDataWithOil_OUT %>%
                          filter(Symbol %in% c(thisComp1,thisComp2)) %>%
                          ungroup() %>%
                          select(Symbol,Year,Month,AbnormalReturnAnnualized) %>%
                          pivot_wider(names_from="Symbol",values_from ="AbnormalReturnAnnualized") %>%
                          rename(CompA = 3) %>%
                          rename(CompB = 4);
    
    #LOOP THE DATA IN TIME
    nTime = dim(theseCompaniesData)[1];
    timeStep = 24;
    
    for (iTime in 1:(nTime-timeStep))
    {
      timeStart = iTime;
      timeEnd   = iTime + timeStep -1 ;
      
      #GET ONE TIME CHUNK
      thisDataTime = theseCompaniesData[timeStart:timeEnd,]
      
      #GET THE CORRELATION BETWEEN THEM
      thisCor = cor(thisDataTime$CompA,thisDataTime$CompB)
      
      #BUILD A DATAFRAME WITH THE INFO
      thisDataRow = data.frame(
                                CompA = thisComp1,
                                CompB = thisComp2,
                                YearMonthStart = paste(theseCompaniesData$Year[timeStart],str_pad(theseCompaniesData$Month[timeStart],2,side="left",pad="0"),sep=""),
                                YearMonthEnd   = paste(theseCompaniesData$Year[timeEnd]  ,str_pad(theseCompaniesData$Month[timeEnd],2,side="left",pad="0"),sep=""),
                                Corr           = thisCor
                                );
      
      allCorrs = allCorrs %>% rbind(thisDataRow);
      
    }
    
  }
}

#
write.table(allCorrs,"clipboard-16384",sep=";",quote=F,row.names=F)


