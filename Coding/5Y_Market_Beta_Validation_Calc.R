require(tidyverse)
require(lubridate)
require(robustbase)
require(broom)
require(quantreg)

#PREREQUISITE RUN OF 5Y MARKET_BETA_VALIDATION.R

#LOAD THE DATAFILES
load("C:/Users/giles/Creative Cloud Files/MBA/Dissertation/Data/5Y_Market_Beta_Calc.Rdata");
load("C:/Users/giles/Creative Cloud Files/MBA/Dissertation/Data/YahooFinanceCalcs.Rdata")


#RESHAPE CALCLATED DATA
CalcBetaData = allRegData %>%
               filter(Regression == "OLS") %>%
               filter(term == "SP500AbnormalReturnAnnualized") %>%
               select(Industry,SubIndustry,Symbol,SymbolDesc,BetaCalc=estimate);

YahooBetaData = allSymbolsYahooCalcs %>%
                filter(name== "Beta (5Y Monthly)") %>%
                dplyr::select(Symbol = stock, YahooBeta = value)

#COMBINE TTHEM
CompareBeta = CalcBetaData %>%
                left_join(YahooBetaData,,by=c("Symbol")) %>%
                filter(!is.na(YahooBeta)) %>%
                filter(!is.na(BetaCalc)) %>%
                mutate( BetaCalc = as.numeric(BetaCalc)) %>%
                mutate( YahooBeta = as.numeric(YahooBeta)) %>%
                mutate(ErrorV = BetaCalc - YahooBeta )

#CREATE A MODEL
CompareBetaLm = lm(data = CompareBeta , YahooBeta ~ BetaCalc )

#LOOK AT IT
summary(CompareBetaLm)

#GET THE R SQUARED VALUE
rsq = (summary(CompareBetaLm))$r.squared;

#GET ERRORS
ErrorVaues = CompareBeta %>%
             mutate(ErrorRelAbs  = abs(ErrorV / BetaCalc )) %>%
             mutate(ErrorSq      = ErrorV * ErrorV ) %>%
             mutate(ErrorAbs     = abs(ErrorV))  %>%
             ungroup() %>%
             summarise(MSE = mean(ErrorSq),
                       MAE = mean(ErrorAbs),
                       MRE = mean(ErrorRelAbs)
                       );

save(CompareBeta,ErrorVaues,file="C:/Users/giles/Creative Cloud Files/MBA/Dissertation/Data/5Y_Market_Beta_Calc_out.Rdata")
            
