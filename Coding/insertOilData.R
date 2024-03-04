require(tidyverse)
require(lubridate)
require(readxl)

#OPEN BIG DATASET
filenameIn1 = "C:/Users/giles/OneDrive/MBA/Dissertation/Data/SharesDataImported.Rdata";
load(filenameIn1)

#OPEN OIL DATA
oilDatafileIn =  "C:/Users/giles/OneDrive/MBA/Dissertation/Data/Giles_Endicott_12_23_2023_3_22_46_PM190_excel2007.xlsx"
oilDataIn = read_excel(oilDatafileIn,sheet="Price Data")

#ENGINEER  OIL DATA TO DESIRED FORMAT
oilDataNom = oilDataIn %>%
             dplyr::select(Date,Symbol=Ticker,SymbolValue=Close) %>%
             mutate(SymbolDesc = "WTI USD Nominal (Finaeon Source)") %>%
             mutate(Industry = "Commodities")  %>%
             mutate(SubIndustry = "Commodities") %>%
             mutate(Date = str_replace_all(Date,"/","-")) %>%
             dplyr::select(Date,SymbolValue,Symbol,SymbolDesc,Industry,SubIndustry);

oilDataReal = oilDataIn %>%
              dplyr::select(Date,Symbol=Ticker,SymbolValue=Real_Close) %>%
              mutate(Symbol ="__WTC_D_REAL") %>%
              mutate(SymbolDesc = "WTI USD Real (Finaeon Source)") %>%
              mutate(Industry = "Commodities")  %>%
              mutate(SubIndustry = "Commodities") %>%
              mutate(Date = str_replace_all(Date,"/","-")) %>%
              dplyr::select(Date,SymbolValue,Symbol,SymbolDesc,Industry,SubIndustry);

#COMBINE DATA SOURCES
allDataOut = allDataIn %>%
                rbind(oilDataNom) %>%
                rbind(oilDataReal);

#WRITE DATA OUT (BACKUP)
filenameOutBackup = "C:/Users/giles/OneDrive/MBA/Dissertation/Data/SharesDataImported_NoExtraOil.Rdata";
save(allDataIn,file=filenameOutBackup);

#WRITE DATA OUT
allDataIn = allDataOut;
filenameOutFinal= "C:/Users/giles/OneDrive/MBA/Dissertation/Data/SharesDataImported.Rdata";
save(allDataIn,file = filenameOutFinal);
