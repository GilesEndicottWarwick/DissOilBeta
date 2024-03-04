require(tidyverse)
require(plotly)

#############################################################################
################## OUTPUT SETUP #############################################
#############################################################################

outputRoot = "C:/Users/giles/OneDrive/MBA/Dissertation/Writeup/Pictures/"

#############################################################################
################## DATA ENGINEERING  ########################################
#############################################################################

#LOAD THE DATA IN
load("C:/Users/giles/OneDrive/MBA/Dissertation/Data/YahooFinanceStats.Rdata")

#REPIVOT IT
allSymbolsData = allSymbolsYahooCalcs %>%
                 filter(name %in% c("Market Cap (intraday)","Beta (5Y Monthly)","Total Debt/Equity (mrq)","Payout Ratio 4","Price/Sales (ttm)","Price/Book (mrq)","Operating Margin (ttm)")) %>%
                 pivot_wider(names_from=c("name"),values_from=c("value")) %>%
                 rename(MarketCap = `Market Cap (intraday)`) %>%
                 rename(YahooBeta = `Beta (5Y Monthly)`) %>%
                 rename(DebtEquity = `Total Debt/Equity (mrq)`) %>%
                 rename(PayoutRatio = `Payout Ratio 4`) %>%
                 rename(PriceSales = `Price/Sales (ttm)`) %>%
                 rename(PriceBook = `Price/Book (mrq)`) %>%
                 rename(OperatingMargin = `Operating Margin (ttm)`)



#GRAB THE MARKET CAP AND PROCESS
allSymbolsMarketCap = allSymbolsData %>%
                      filter(!is.na(stock)) %>%
                      select(stock,MarketCap,YahooBeta,DebtEquity,PayoutRatio,PriceSales,PriceBook,OperatingMargin) %>%
                      mutate(LastDigit = str_sub(MarketCap,-1,-1)) %>%
                      mutate(MarketCap1 = str_sub(MarketCap,1,-2)) %>%
                      mutate(Factor = ifelse(LastDigit == "B",1e9,1e12)) %>%
                      mutate(MarketCap = as.numeric(MarketCap1) * Factor ) %>%
                      mutate(DebtEquity = as.numeric(str_sub(DebtEquity,1,-2))) %>%
                      mutate(YahooBeta = as.numeric(YahooBeta)) %>%
                      mutate(PayoutRatio = as.numeric(str_sub(PayoutRatio,1,-2))) %>%
                      mutate(OperatingMargin = as.numeric(str_sub(OperatingMargin,1,-2))) %>%
                      mutate(PriceBook = as.numeric(str_sub(PriceBook,1,-2))) %>%
                      select(stock,MarketCap,YahooBeta,DebtEquity,PayoutRatio,PriceSales,PriceBook,OperatingMargin)


#GET OIL BETA VALUES
load("C:/Users/giles/OneDrive/MBA/Dissertation/Data/AllYear_Market_Beta_Calc.Rdata")

#FILTER TO RELEVANT OIL BETA VALUES
allOilBetas = allRegData %>%
              filter(term == "OilAbnormalReturnAnnualized") %>%
              filter(Model == "Oil and SP500") %>%
              filter(Regression == "OLS") %>%
              rename(OilBeta = estimate);


allMarketBetas = allRegData %>%
                  filter(term == "SP500AbnormalReturnAnnualized") %>%
                  filter(Model == "Oil and SP500") %>%
                  #filter(Model == "SP500 Only") %>%
                  filter(Regression == "OLS") %>%
                  rename(MarketBeta = estimate) %>%
                  select(Symbol,MarketBeta);
      
EnergyOilBetas = allOilBetas %>%
                 filter(Industry == "Energy") %>%
                 left_join(allSymbolsMarketCap,by=c("Symbol"="stock")) %>%
                 left_join(allMarketBetas,by=c("Symbol"="Symbol")) %>%
                 filter(p.value < 0.05) %>%
                 filter(nMonths > 250)

write.table(EnergyOilBetas,"clipboard-16384",sep=";",row.names=F,quote=F)

#HAVE A LOOK AT THE VALUES WITH A LIN LOG10 MODEL
LM1 = lm(data = EnergyOilBetas, OilBeta ~ log10(MarketCap))
summary(LM1)

LM1b = lm(data = EnergyOilBetas, OilBeta ~ SubIndustry )
summary(LM1b)

LM2 = lm(data = EnergyOilBetas, OilBeta ~ DebtEquity)
summary(LM2)

LM3 = lm(data = EnergyOilBetas, OilBeta ~ log10(MarketCap) + DebtEquity)
summary(LM3)

LM4 = lm(data = EnergyOilBetas, OilBeta ~ log10(MarketCap) + YahooBeta)
summary(LM4)

LM5 = lm(data = EnergyOilBetas, OilBeta ~ MarketBeta)
summary(LM5)

LM6 = lm(data = EnergyOilBetas, MarketBeta ~ log10(MarketCap))
summary(LM6)

LM7 = lm(data = EnergyOilBetas, OilBeta ~ MarketBeta)
summary(LM7)

LM8 = lm(data = EnergyOilBetas, OilBeta ~ PriceBook)
summary(LM8)


#ADD THAT MODEL TO THE DATA
EnergyOilBetas = EnergyOilBetas %>%
                 mutate(OilBetaLM = LM1$coefficients[1] + log10(MarketCap) * LM1$coefficients[2] )

#ADD THAT MODEL TO THE DATA
EnergyOilBetas = EnergyOilBetas %>%
                 mutate(MarketBetaLM = LM6$coefficients[1] + log10(MarketCap) * LM6$coefficients[2] )

#ADD THAT MODEL TO THE DATA
EnergyOilBetas = EnergyOilBetas %>%
  mutate(MarketOilBetaLM = LM7$coefficients[1] + MarketBeta * LM7$coefficients[2] )

#############################################################################
################## GLOBAL SETUP  ############################################
#############################################################################

#FONT FOR LABELS
FONT1 = list(family = 'Arial',
             size = 22,
             color = 'rgb(82, 82, 82)');

#FONT FOR Y AXIS TITLE
FONT2 = list(family = 'Arial',
             size = 22,
             color = 'rgb(82, 82, 82)');

markerSize1 = 10;

###############################################################################################
###################################### OIL BETA VS MARKET CAP #################################
###############################################################################################



xaxis1 <- list(title = list(text="Market Cap (2022 $)",font = FONT2),
               type = "log",
               showline = TRUE,
               showgrid = FALSE,
               showticklabels = TRUE,
               linecolor = 'rgb(204, 204, 204)',
               linewidth = 2,
               autotick = TRUE,
               #tick0=0,
              # dtick = 05,
               ticks = 'outside',
               tickcolor = 'rgb(204, 204, 204)',
               tickwidth = 2,
               #ticklen = 5,
               range=c(9.9,11.8),
               tickfont = FONT1,
               font = FONT1
               
);



yaxis1 <- list(title = list(text = "Oil-Beta",
                            font = FONT2),
               showline = FALSE,
               showticklabels = TRUE,
               linecolor = 'rgb(204, 204, 204)',
               linewidth = 2,
               autotick = FALSE,
               ticks = 'outside',
               tickcolor = 'rgb(204, 204, 204)',
               tickwidth = 2,
               ticklen = 0,
               tick0=0.0,
               dtick = 0.1,
               #ticklabelstep=1,
               showgrid=TRUE,
               gridcolor = 'rgb(204, 204, 204)',
               gridwidth=2,
               zeroline=FALSE,
               #ticksuffix = "  ",
               tickprefix = "  ",
               range=c(0.1,0.54),
               tickfont = FONT1,
               font = FONT1
);

f1 = plot_ly() %>%
     add_trace(
              data = EnergyOilBetas,
                 x = ~MarketCap,
                 y = ~OilBeta,
             # symbol = ~SubIndustry,
              name = ~Symbol,
              type = "scatter",
              mode = 'markers',
              marker= list(color = 'rgb(0, 0, 0)', size = markerSize1)
          ) %>%
     add_trace(
              data = EnergyOilBetas,
              x = ~MarketCap,
              y = ~OilBetaLM,
              type = "scatter",
              mode = 'lines'
     ) %>%
    add_text( data = EnergyOilBetas %>% filter(!(Symbol %in% c("OKE","OXY","VLO"))),
            x = ~MarketCap, 
            y = ~OilBeta,
            text = ~Symbol,
            textfont = list(size=20,
                            color = 'rgb(100,100,100)'), 
            textposition = "top right")

f1 <- f1 %>% layout( 
              xaxis = xaxis1,
              yaxis = yaxis1,
              showlegend=FALSE
              )

f1

#WRITE OUT THE PICTURE
reticulate::py_run_string("import sys");
fileOut = paste(outputRoot,"EnergyOilBetaMarketCap.png",sep="");
save_image(f1, fileOut, width = 600, height = 800)

###############################################################################################
###################################### MARKET BETA VS MARKET CAP #################################
###############################################################################################

xaxis2 <- list(title = list(text="Market Cap (2022 $)",font = FONT2),
               type = "log",
               showline = TRUE,
               showgrid = FALSE,
               showticklabels = TRUE,
               linecolor = 'rgb(204, 204, 204)',
               linewidth = 2,
               autotick = TRUE,
               #tick0=0,
               # dtick = 05,
               ticks = 'outside',
               tickcolor = 'rgb(204, 204, 204)',
               tickwidth = 2,
               #ticklen = 5,
               range=c(9.8,11.85),
               tickfont = FONT1,
               font = FONT1
               
);



yaxis2 <- list(title = list(text = "Market-Beta",
                            font = FONT2),
               showline = FALSE,
               showticklabels = TRUE,
               linecolor = 'rgb(204, 204, 204)',
               linewidth = 2,
               autotick = FALSE,
               ticks = 'outside',
               tickcolor = 'rgb(204, 204, 204)',
               tickwidth = 2,
               ticklen = 0,
               tick0=0.0,
               dtick = 0.25,
               #ticklabelstep=1,
               showgrid=TRUE,
               gridcolor = 'rgb(204, 204, 204)',
               gridwidth=2,
               zeroline=FALSE,
               #ticksuffix = "  ",
               tickprefix = "  ",
               range=c(0.5,1.59),
               tickfont = FONT1,
               font = FONT1
);

f2 = plot_ly() %>%
  add_trace(
    data = EnergyOilBetas,
    x = ~MarketCap,
    y = ~MarketBeta,
    name = ~Symbol,
    type = "scatter",
    mode = 'markers',
    marker= list(color = 'rgb(0, 0, 0)', size = markerSize1)
  ) %>%
  add_trace(
    data = EnergyOilBetas,
    x = ~MarketCap,
    y = ~MarketBetaLM,
    type = "scatter",
    mode = 'lines'
  ) %>%
  add_text( data = EnergyOilBetas %>% filter(!(Symbol %in% c("VLO","PXD","HES","OXY","WMB"))),
            x = ~MarketCap, 
            y = ~MarketBeta,
            text = ~Symbol,
            textfont = list(size=20,
                            color = 'rgb(100,100,100)'), 
            textposition = "top right")

f2 <- f2 %>% layout( 
  xaxis = xaxis2,
  yaxis = yaxis2,
  showlegend=FALSE
)

f2

#WRITE OUT THE PICTURE
reticulate::py_run_string("import sys");
fileOut = paste(outputRoot,"EnergyMarketBetaMarketCap.png",sep="");
save_image(f2, fileOut, width = 600, height = 800)

###############################################################################################
###################################### OIL BETA VS MARKET BETA ################################
###############################################################################################

xaxis3 <- list(title = list(text="Market-Beta",font = FONT2),
               showline = TRUE,
               showgrid = FALSE,
               showticklabels = TRUE,
               linecolor = 'rgb(204, 204, 204)',
               linewidth = 2,
               autotick = TRUE,
               #tick0=0,
               # dtick = 05,
               ticks = 'outside',
               tickcolor = 'rgb(204, 204, 204)',
               tickwidth = 2,
               #ticklen = 5,
               range=c(0.6,1.40),
               tickfont = FONT1 ,
               font = FONT1
               
);



yaxis3 <- list(title = list(text = "Oil-Beta",
                            font = FONT2),
               showline = FALSE,
               showticklabels = TRUE,
               linecolor = 'rgb(204, 204, 204)',
               linewidth = 2,
               autotick = FALSE,
               ticks = 'outside',
               tickcolor = 'rgb(204, 204, 204)',
               tickwidth = 2,
               ticklen = 0,
               tick0=0.0,
               dtick = 0.10,
               #ticklabelstep=1,
               showgrid=TRUE,
               gridcolor = 'rgb(204, 204, 204)',
               gridwidth=2,
               zeroline=FALSE,
               #ticksuffix = "  ",
               tickprefix = "  ",
               range=c(0.1,0.53),
               tickfont = FONT1,
               font = FONT1
);

f3 = plot_ly() %>%
  add_trace(
    data = EnergyOilBetas,
    x = ~MarketBeta,
    y = ~OilBeta,
    name = ~Symbol,
    type = "scatter",
    mode = 'markers',
    marker= list(color = 'rgb(0, 0, 0)', size = markerSize1)
  ) %>%
  add_trace(
    data = EnergyOilBetas,
    x = ~MarketBeta,
    y = ~MarketOilBetaLM,
    type = "scatter",
    mode = 'lines'
  ) %>%
  add_text( data = EnergyOilBetas %>% filter(!(Symbol %in% c())),
            x = ~MarketBeta, 
            y = ~OilBeta,
            text = ~Symbol,
            textfont = list(size=20,
                            color = 'rgb(100,100,100)'), 
            textposition = "top right")

f3 <- f3 %>% layout( 
  xaxis = xaxis3,
  yaxis = yaxis3,
  showlegend=FALSE
)

f3

#WRITE OUT THE PICTURE
reticulate::py_run_string("import sys");
fileOut = paste(outputRoot,"EnergyOilBetaMarketBeta.png",sep="");
save_image(f3, fileOut, width = 600, height = 800)



###############################################################################################
###################################### LOOK AT COMPANY DEBT ###################################
###############################################################################################


