library(rvest)
library(purrr)
library(dplyr)
library(readxl)

#################################################################
############## FUNCTION TO GET THE STATS ########################
#################################################################

get_summary_table <- function(symbol){
  
  url <- paste0("https://finance.yahoo.com/quote/",symbol,"/key-statistics")
  df <- url %>%
    read_html() %>%
    html_table(header = FALSE) %>%
    map_df(bind_cols) %>%
    as_tibble()
  
  names(df) <- c("name", "value")
  df["stock"] <- symbol
  
  df
}

#################################################################
############## LOOP ALL COMPANIES ###############################
#################################################################

#THE DATASTORES
allSymbolsYahooCalcs = data.frame();
badSymbols           = data.frame();

#LIST OF SYMBOLS TO COLLECT
filenameInMain  = "C:/Users/giles/OneDrive/MBA/Dissertation/Coding/Temp\\SP500.xlsx";

#READ THE FILE
SP500_components = read_excel(filenameInMain,sheet="Sheet2");

#LOAD THE EXISTING COMPONENTS FROM PREVIOUS RUNS
filenameOut = "C:/Users/giles/OneDrive/MBA/Dissertation/Data/YahooFinanceStats.Rdata";
load(filenameOut);

#REMOVE EXISTING FROM TODO LIST
existingSymbols = unique(allSymbolsYahooCalcs$stock);
SP500_components = SP500_components %>%
                   filter( !(Symbol %in% existingSymbols ))

#LOOP THE COMPONENTS
for (iSymbol in 1:dim(SP500_components)[1])
{
  #GET ONE SYMBOL
  thisSymbol = SP500_components$Symbol[iSymbol]
  
  #GRAB THE DATA
  thisSymbolData = get_summary_table(thisSymbol)
  
  #COMBINE THE DATA IF VALID, OTHERWISE STORE BAD SYMBOLS
  if ((dim(thisSymbolData))[2] == 3)
  {
    allSymbolsYahooCalcs = allSymbolsYahooCalcs %>%
                         rbind(thisSymbolData);
    
    #INTERMEDIATE SAVING
    filenameOut = "C:/Users/giles/OneDrive/MBA/Dissertation/Data/YahooFinanceStats.Rdata";
    save(allSymbolsYahooCalcs,file=filenameOut);
    
  }else
  {
    #DEAL WITH PROBLEMATIC ENTRIES
    badSymbols = badSymbols %>%
                 rbind(data.frame(Symbol=thisSymbol));
    print(paste("Symbol ",thisSymbol," contained bad data",sep=""));
  }
  
  #PRINT PROGRESS
  if (iSymbol %% 25 == 0)
  {
    print(paste("Completed iteration ",iSymbol,sep=""));
  }
  
  #SHORT PAUSE
  Sys.sleep(5);
  #Sys.sleep(rnorm(n=1,mean=10,sd=10))
}

#SAVE THE DATASET
filenameOut = "C:/Users/giles/OneDrive/MBA/Dissertation/Data/YahooFinanceStats.Rdata";
save(allSymbolsYahooCalcs,file=filenameOut);

