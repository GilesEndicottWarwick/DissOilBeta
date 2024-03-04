require(dplyr)
require(quantmod)
require(stringr)
require(lubridate)
require(tidyr)
require(readxl)

#START AND END DATES OF ANALYSIS
sdate = as.Date("1989-01-01");
edate = as.Date("2023-12-31");

#FILE IN
filenameInMain  = "C:/Users/giles/OneDrive/MBA/Dissertation/Coding/Temp\\SP500.xlsx";
filenameInExtra = "C:/Users/giles/OneDrive/MBA/Dissertation/Coding/Temp\\ExtraSymbols.xlsx";

#FILE FOR WRITING OUT
filenameOut = "C:/Users/giles/OneDrive/MBA/Dissertation/Data/SharesDataImported.Rdata";

#READ THE FILE
SP500_components = read_excel(filenameInMain,sheet="Sheet2");
Extra_components = read_excel(filenameInExtra,sheet="Sheet2");

#LIMIT TO REQUIRED SIZE
ImportList = SP500_components %>%
             rbind(Extra_components);

#INIT FULL DATASET
allDataIn = data.frame();

#DOWNLOAD THE DATA
for (i in 1:dim(ImportList)[1])
{
  #THIS SYMBOL FORM LIST
  thisSymbol      = ImportList$Symbol[i];
  thisSymbolDesc  = ImportList$SymbolDesc[i];
  thisIndustry    = ImportList$`GICS Sector`[i];
  thisSubIndustry = ImportList$`GICS SubIndustry`[i];
  
  #RUN THE API
  dataIn = quantmod::getSymbols(thisSymbol, from=sdate, to=edate,auto.assign=F,warnings=F);
  
  # CONVERT THE DATA TO A USEFUL FORMAT
  # (KEEP ONLY CLOSING VALUE OF QUARTER)
  thisDataMod1 = as.data.frame(dataIn) %>%
    mutate(Date  = row.names(.)) %>%
    dplyr::select(c(7,6)) %>%
    rename(SymbolValue = 2 ) %>%
    mutate(Symbol      = thisSymbol) %>%
    mutate(SymbolDesc  = thisSymbolDesc) %>%
    mutate(Industry    = thisIndustry) %>%
    mutate(SubIndustry = thisSubIndustry);
  
  #ADD TO THE BIGGER DATASET
  allDataIn = allDataIn %>% rbind(thisDataMod1);
  
  #INTERMEDIATE SAVING?
  if ( i %% 25 ==0)
  {
    save(allDataIn,file=filenameOut)
    print(paste("Completed Entry ",i,sep=""));
  }
  
} 

#SAVE FINAL VERSIION TO DISK
save(allDataIn,file=filenameOut);

