require(dplyr)
require(quantmod)
require(stringr)
require(lubridate)
require(tidyr)
require(data.table)
require(plotly)

#LOAD THE DATA IN (FROM IMPORTSHARESDATA.R AND INSERTOIILDATA.R)
load("C:/Users/giles/OneDrive/MBA/Dissertation/Data/SharesDataImported.Rdata");

#CALC RETURNS FROM RISK FREE DATA
RiskFreeData =   allDataIn %>%
                  filter(Symbol == "^TNX") %>%
                  filter(!is.na(SymbolValue)) %>%
                  ungroup()  %>%
                  mutate(Year = year(Date)) %>%
                  group_by(Symbol,SymbolDesc,Year) %>%
                  summarise(SymbolValue = mean(SymbolValue,na.rm=T)) %>%
                  ungroup() %>%
                  select(Year,RiskFree=SymbolValue)


#S&P500
SP500_AnnualReturns = allDataIn %>%
                      select(-Industry,-SubIndustry) %>%
                      filter(Symbol == "^GSPC") %>%
                      filter(!is.na(SymbolValue)) %>%
                      ungroup()  %>%
                      mutate(Year = year(Date)) %>%
                      group_by(Symbol,SymbolDesc,Year) %>%
                      mutate(Rank1 = frank(Date)) %>%
                      mutate(Rank2 = frank(desc(Date))) %>%
                      filter(Rank1 ==1 | Rank2 == 1) %>%
                      arrange(Date) %>%
                      mutate(StartValue = lag(SymbolValue)) %>%
                      mutate(AnnualReturn = (SymbolValue-StartValue) / StartValue ) %>%
                      filter(Rank2 == 1) %>%
                      select(Year,Symbol,SymbolDesc,AnnualReturn)
  
  
#OIL PRICE RETURNS
Oil_AnnualReturns = allDataIn %>%
                    select(-Industry,-SubIndustry) %>%
                    filter(Symbol == "__WTC_D") %>%
                    filter(!is.na(SymbolValue)) %>%
                    ungroup()  %>%
                    mutate(Date = ymd(Date)) %>%
                    mutate(Year = year(Date)) %>%
                    group_by(Symbol,SymbolDesc,Year) %>%
                    mutate(Rank1 = frank(Date)) %>%
                    mutate(Rank2 = frank(desc(Date))) %>%
                    filter(Rank1 ==1 | Rank2 == 1) %>%
                    arrange(Date) %>%
                    mutate(StartValue = lag(SymbolValue)) %>%
                    mutate(AnnualReturn = (SymbolValue-StartValue) / StartValue ) %>%
                    filter(Rank2 == 1) %>%
                    select(Year,Symbol,SymbolDesc,AnnualReturn) %>%
                    filter(Year >= 1990 );


#OIL SHARES EXISTING 1990 TO 2023
OilDataCompanies = allDataIn %>%
                   filter(Industry == "Energy") %>%
                   group_by(Symbol,SymbolDesc) %>%
                   summarise(MinDate = min(Date),
                             MaxDate = max(Date)) %>%
                   filter(MinDate <= "1990-01-02");

OilCompanyShares = allDataIn %>%
                    select(-Industry,-SubIndustry) %>%
                    filter(Symbol %in% OilDataCompanies$Symbol) %>%
                    filter(!is.na(SymbolValue)) %>%
                    ungroup()  %>%
                    mutate(Year = year(Date)) %>%
                    group_by(Symbol,SymbolDesc,Year) %>%
                    mutate(Rank1 = frank(Date)) %>%
                    mutate(Rank2 = frank(desc(Date))) %>%
                    filter(Rank1 ==1 | Rank2 == 1) %>%
                    arrange(Date) %>%
                    mutate(StartValue = lag(SymbolValue)) %>%
                    mutate(AnnualReturn = (SymbolValue-StartValue) / StartValue ) %>%
                    filter(Rank2 == 1) %>%
                    select(Year,Symbol,SymbolDesc,AnnualReturn) %>%
                    group_by(Year) %>%
                    summarise(AnnualReturn = mean(AnnualReturn)) %>%
                    mutate(Symbol = "Oil Fund") %>%
                    mutate(SymbolDesc = "OilFund") %>%
                    select(Year,Symbol,SymbolDesc,AnnualReturn) 
        
#COMBINE DATA
AllReturns = SP500_AnnualReturns %>%
             rbind(Oil_AnnualReturns) %>%
             rbind(OilCompanyShares) %>%
             left_join(RiskFreeData,by=c("Year")) %>%
             mutate(AnnualReturnAb = 100*(AnnualReturn-(RiskFree/100)));

write.table(AllReturns,"clipboard-16384",sep=";",quote=F,row.names=F)

###################################################################################
################# MASTERR FONT SETTINGS ###########################################
###################################################################################

FONT1 = list(family = 'Arial',
             size = 22,
             color = 'rgb(82, 82, 82)');

FONT2 = list(family = 'Arial',
             size = 18,
             color = 'rgb(82, 82, 82)');

###################################################################################
################# CREATE THE CHARTS ###############################################
###################################################################################

xaxis <- list(title = "",
                showline = TRUE,
                showgrid = FALSE,
                showticklabels = TRUE,
                linecolor = 'rgb(204, 204, 204)',
                linewidth = 2,
                autotick = FALSE,
                tick0=1990,
                dtick = 10,
                ticks = 'outside',
                tickcolor = 'rgb(204, 204, 204)',
                tickwidth = 2,
                #ticklen = 5,
                range=c(1989.5,2023.6),
                tickfont = FONT1 ,
                xperiodalignment="middle"
                
);



yaxis <- list(title = list(text = "S&P 500 Abnormal Return (%)",
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
                tick0=0,
                dtick = 25,
                ticklabelstep=1,
                showgrid=TRUE,
                gridcolor = 'rgb(204, 204, 204)',
                gridwidth=2,
                zeroline=FALSE,
                ticksuffix = "% ",
                tickprefix = "  ",
                range=c(-74,110),
                tickfont = FONT1,
                font = FONT1,
                side="left",
                tickformat="+2"
);

yaxis2 <- list(title = list(text = "WTI Price Abnormal Return (%)",
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
              tick0=0,
              dtick = 25,
              ticklabelstep=1,
              showgrid=TRUE,
              gridcolor = 'rgb(204, 204, 204)',
              gridwidth=2,
              zeroline=FALSE,
              ticksuffix = "% ",
              tickprefix = "  ",
              range=c(-74,110),
              tickfont = FONT1,
              font = FONT1,
              side="left",
              tickformat="+2"
);

yaxis3 <- list(title = list(text = "Oil Companies Abnormal Return (%)",
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
               tick0=0,
               dtick = 25,
               ticklabelstep=1,
               showgrid=TRUE,
               gridcolor = 'rgb(204, 204, 204)',
               gridwidth=2,
               zeroline=FALSE,
               ticksuffix = "% ",
               tickprefix = "  ",
               range=c(-74,110),
               tickfont = FONT1,
               font = FONT1,
               side="left",
               tickformat="+2"
);


fig1 <- plot_ly(AllReturns %>% filter(Symbol == "^GSPC"),
                 x = ~Year, 
                 y = ~AnnualReturnAb, 
                 name = ~SymbolDesc,
                 type = 'bar',
                 marker= list(color = 'rgb(120, 120, 120)', size = 10)
) 



#APPLY AXIS SETTINGS
fig1 <- fig1 %>% layout(title = "",
                          xaxis = xaxis,
                          yaxis = yaxis
);


fig2 <- plot_ly(AllReturns %>% filter(Symbol == "__WTC_D"),
                x = ~Year, 
                y = ~AnnualReturnAb, 
                name = ~SymbolDesc,
                type = 'bar',
                marker= list(color = 'rgb(120, 120, 120)', size = 10)
) 



#APPLY AXIS SETTINGS
fig2 <- fig2 %>% layout(title = "",
                        xaxis = xaxis,
                        yaxis = yaxis2
);


fig3 <- plot_ly(AllReturns %>% filter(Symbol == "Oil Fund"),
                x = ~Year, 
                y = ~AnnualReturnAb, 
                name = ~SymbolDesc,
                type = 'bar',
                marker= list(color = 'rgb(120, 120, 120)', size = 10)
) 



#APPLY AXIS SETTINGS
fig3 <- fig3 %>% layout(title = "",
                        xaxis = xaxis,
                        yaxis = yaxis3
);


print(fig1)
print(fig2)
print(fig3)

###################################################################################
################# OUTPUT FILES   ##################################################
###################################################################################

outputRoot = "C:/Users/giles/OneDrive/MBA/Dissertation/Writeup/Pictures/"

#WRITE OUT THE PICTURE
reticulate::py_run_string("import sys");
fileOut = paste(outputRoot,"Returns_1.png",sep="");
save_image(fig1, fileOut, width = 500, height = 500)

#WRITE OUT THE PICTURE
reticulate::py_run_string("import sys");
fileOut = paste(outputRoot,"Returns_2.png",sep="");
save_image(fig2, fileOut, width = 500, height = 500)

#WRITE OUT THE PICTURE
reticulate::py_run_string("import sys");
fileOut = paste(outputRoot,"Returns_3.png",sep="");
save_image(fig3, fileOut, width = 500, height = 500);
