#LIBRARIES
require(tidyverse)
require(lubridate)
require(readxl)
require(partykit)
require(plotly)
require(robustbase)
require(data.table)

#LOAD ROLLING BETA DATA FROM RollingBetaParOil_Biweekly.R
load("C:/Users/giles/OneDrive/MBA/Dissertation/Data/RollingBetaIndustryBiWeekly.Rdata")

#FILTER ROLLING BETA TO ONLY ENERGY GROUP
OilBeta1Y_ROB = collectRegressionData %>%
                 filter(Symbol == "Energy") %>%
                 filter(Regression == "OLS") %>%
                 filter(term == "OilRet") %>%
                 select(Symbol,start,end,OilBeta=estimate) %>%
                 mutate(Year  = as.numeric(str_sub(start,1,4))) %>%
                 mutate(WeekStart = as.numeric(str_sub(start,5,6))) %>%
                 mutate(OilBetaLag = lag(OilBeta)) %>%
                 mutate(DeltaOilBeta = OilBeta - OilBetaLag);

#FILTER ROLLING BETA TO ONLY ENERGY GROUP
MarketBeta1Y_ROB = collectRegressionData %>%
                  filter(Symbol == "Energy") %>%
                  filter(Regression == "OLS") %>%
                  filter(term == "MarketRet") %>%
                  select(Symbol,start,end,MarketBeta=estimate) %>%
                  mutate(Year  = as.numeric(str_sub(start,1,4))) %>%
                  mutate(WeekStart = as.numeric(str_sub(start,5,6))) %>%
                  mutate(MarketBetaLag = lag(MarketBeta)) %>%
                  mutate(DeltaMarketBeta = MarketBeta - MarketBetaLag) %>%
                  select(Year,MarketBeta,DeltaMarketBeta)


#GET MACROECONOMIC DATA AND SUMMARISE TO TWO YEAR BLOCKS, IE REAL GDP GROWTH CHART
fileIn1= "C:/Users/giles/OneDrive/MBA/Dissertation/Data/API_NY.GDP.MKTP.KD.ZG_DS2_en_excel_v2_6298898.xls"

DataIn_1 = read_excel(fileIn1,sheet="Data",skip=2)

#THE COUNTRIES WE FILTER
InterestCountries = c("United States")

#FILTER TO THOSE
DataIn_2 = DataIn_1 %>%
  filter(`Country Name` %in% c("United States","World") ) %>%
  pivot_longer(cols = ! c("Country Name" ,,  "Country Code","Indicator Name","Indicator Code" )) %>%
  rename(Year= name) %>%
  rename(GDP_Growth = value) %>%
  rename(Country_Name=`Country Name`) %>%
  filter(!is.na(GDP_Growth)) %>%
  mutate(GDP_Growth = as.numeric(GDP_Growth)) %>%
  mutate(Year = as.numeric(Year)) %>%
  ungroup() %>%
  filter(Year >= 1989) %>%
  select(Year,Country=`Country Code`,GDP_Growth) %>%
  pivot_wider(names_from="Country",values_from = c("GDP_Growth"),names_prefix = "GDP_GROWTH_")



#REAL GDP GROWTH IN ABS TGERMS
#GDP_OUTPUT_1 = RollingBetaIndustryWeekly;

#LOOK AT GDP GROWTH IN VS LAST YEAR TERMS
GDP_OUTPUT_2A = DataIn_2 %>%
               mutate(Lag_USA = lag(GDP_GROWTH_USA)) %>%
               mutate(Lag_WLD = lag(GDP_GROWTH_WLD)) %>%
               filter(Year >= 1990) %>%
               mutate(GDP_Growth_USA_YOY = GDP_GROWTH_USA - Lag_USA) %>%
               mutate(GDP_Growth_WLD_YOY = GDP_GROWTH_WLD - Lag_WLD) %>%
               select(Year,GDP_Growth_USA_YOY,GDP_Growth_WLD_YOY)


#THE FINAL FEATURES FROM US REAL GDP GROWTH NUMBERS
GDP_REAL_ALL = DataIn_2 %>%
               left_join(GDP_OUTPUT_2A,by=c("Year")) %>%
               filter(Year >= 1990)


#GET OIL SUPPLY-DEMAND GAP DATA + OIL PRODUCTION DATA
DataOil2 = read_excel("C:/Users/giles/OneDrive/MBA/Dissertation/Data/OilSupplyDemandOECD.xlsx",sheet="Data")

DataOil3 = DataOil2 %>%
          mutate(OilDemand = str_replace_all(`Totaloildemand`,"\\s+","")) %>%
          mutate(OilProduction = str_replace_all(`CrudeoilandNGLproduction`,"\\s+","")) %>%
          mutate(OilDemand = 1/1000 * as.numeric(OilDemand)) %>%
          mutate(OilProduction = 1/1000 * as.numeric(OilProduction)) %>%
          dplyr::select( -`Totaloildemand` , -`CrudeoilandNGLproduction`  ) %>%
          mutate(DemandGap = OilDemand - OilProduction ) %>%
          rename(Year=Time) %>%
          mutate(GapPercSup = 100*DemandGap / OilProduction) %>%
          mutate(GapPercDem = 100*DemandGap / OilDemand) %>%
          select(Year,GapPercSup);

#LOOK AT YOY
DataOil4_YOY = DataOil3 %>%
                mutate(L1= lag(GapPercSup)) %>%
                mutate(GapPerc_YOY = GapPercSup - L1) %>%
                select(Year,GapPerc_YOY)



OilGapAll = DataOil3 %>%
            left_join(DataOil4_YOY,by=c("Year")) %>%
            filter(Year >= 1990);



#GET S+P 500 AS A CHECK.... IF GDP CORRELATES, DOESNT THE S+P 500?
load("C:/Users/giles/OneDrive/MBA/Dissertation/Data/SharesDataImported.Rdata");

SP500_0 = allDataIn %>%
        filter(Symbol =="^GSPC");
        
rm(allDataIn);

SP500_1 = SP500_0 %>%
          mutate(Year= year(Date)) %>%
          group_by(Year) %>%
          mutate(R1 = frank(Date)) %>%
          mutate(R2 = frank(desc(Date))) %>%
          filter(R1 == 1 | R2 ==1) %>%
          arrange(Date) %>%
          mutate(LastValue = lag(SymbolValue)) %>%
          filter(R2 ==1) %>%
          mutate(Return = (SymbolValue- LastValue) / LastValue) %>%
          select(Year,SP500Ret = Return);

SP500_2 = SP500_1 %>%
          ungroup() %>%
          mutate(ReturnLag = lag(SP500Ret)) %>%
          mutate(SP500_YOY = SP500Ret - ReturnLag ) %>%
          select(Year,SP500_YOY)


#GET RAW OIL PRICE AND MAKE A YEARLY AVERage
filenameIn = "C:/Users/giles/OneDrive/MBA/Dissertation/Data/SharesDataImported.Rdata";

#LOAD DATA IN 
load(filenameIn);


#FILTER TO RELEVANT SYMBOLS
theseSymbols = c(	"__WTC_D","__WTC_D_REAL");

oilData = allDataIn %>%
          filter(Symbol %in% theseSymbols) %>%
          mutate(Year = year(ymd(Date))) %>%
          group_by(Symbol,Year) %>%
          summarise(Value = mean(SymbolValue)) %>%
          filter(Year >= 1989) %>%
          pivot_wider(names_from =c("Symbol"),values_from = c("Value"));

#COMBINE TARGET (OIL BETA) AND FEATURES (GDP CHANGES AND OIL-SUPPLY-DEMAND CHANGES)
FeaturesAndTarget = OilBeta1Y_ROB %>%
                    left_join(GDP_REAL_ALL,by=c("Year")) %>%
                    left_join(OilGapAll,by=c("Year")) %>%
                    left_join(SP500_1,by=c("Year")) %>%
                    left_join(SP500_2,by=c("Year")) %>%
                    left_join(oilData ,by=c("Year")) %>%
                    left_join(MarketBeta1Y_ROB,by=c("Year")) %>%
                    select(-WeekStart,-start,-end) %>%
                    mutate(GDP_GROWTH_WLD_L1 = lag(GDP_GROWTH_WLD)) %>%
                    mutate(GDP_GROWTH_USA_L1 = lag(GDP_GROWTH_USA)) %>%
                    mutate(GDP_Growth_USA_YOY_L1 = lag(GDP_Growth_USA_YOY)) %>%
                    mutate(GDP_Growth_WLD_YOY_L1 = lag(GDP_Growth_WLD_YOY)) %>%
                    mutate(GapPercSup_L1 = lag(GapPercSup)) %>%
                    mutate(GapPerc_YOY = lag(GapPerc_YOY)) %>%
                    filter(Year != 1989) %>%
                    filter(Year != 2023)


#CHECK THE BIG RISK FEATURE
LM0A  = lm(data= FeaturesAndTarget,OilBeta      ~ SP500Ret  )
LM0B  = lm(data= FeaturesAndTarget,DeltaOilBeta ~ SP500Ret  )

summary(LM0A)
summary(LM0B)

LM00A  = lm(data= FeaturesAndTarget,OilBeta      ~ SP500_YOY  )
LM00B  = lm(data= FeaturesAndTarget,DeltaOilBeta ~ SP500_YOY  )

summary(LM00A)
summary(LM00B)

#CHECK KEY FEATURES
LM1A  = lm(data= FeaturesAndTarget,OilBeta      ~ GDP_GROWTH_USA  )
LM1B  = lm(data= FeaturesAndTarget,DeltaOilBeta ~ GDP_GROWTH_USA  )

LM2A  = lm(data= FeaturesAndTarget,OilBeta      ~ GDP_GROWTH_WLD  )
LM2B  = lm(data= FeaturesAndTarget,DeltaOilBeta ~ GDP_GROWTH_WLD  )

summary(LM1A)
summary(LM1B)
summary(LM2A)
summary(LM2B)

LM3A      = lm(data= FeaturesAndTarget,OilBeta      ~ GDP_Growth_USA_YOY  )
LM3B      = lm(data= FeaturesAndTarget,DeltaOilBeta ~ GDP_Growth_USA_YOY  )
LM3B_ROB  = lmrob(data= FeaturesAndTarget,DeltaOilBeta ~ GDP_Growth_USA_YOY  )

LM4A  = lm(data= FeaturesAndTarget,OilBeta      ~ GDP_Growth_WLD_YOY  )
LM4B  = lm(data= FeaturesAndTarget,DeltaOilBeta ~ GDP_Growth_WLD_YOY  )

summary(LM3A)
summary(LM3B)
summary(LM3B_ROB)
summary(LM4A)
summary(LM4B)

LM5A  = lm(data= FeaturesAndTarget,OilBeta      ~ GapPercSup  )
LM5B  = lm(data= FeaturesAndTarget,DeltaOilBeta ~ GapPercSup  )

LM6A  = lm(data= FeaturesAndTarget,OilBeta      ~ GapPerc_YOY  )
LM6B  = lm(data= FeaturesAndTarget,DeltaOilBeta ~ GapPerc_YOY  )

summary(LM5A)
summary(LM5B)
summary(LM6A)
summary(LM6B)

#CHECK LAGGED FEATURES
LM10A  = lm(data= FeaturesAndTarget,OilBeta      ~ GDP_GROWTH_USA_L1  )
LM10B  = lm(data= FeaturesAndTarget,DeltaOilBeta ~ GDP_GROWTH_USA_L1  )

LM20A  = lm(data= FeaturesAndTarget,OilBeta      ~ GDP_GROWTH_WLD_L1  )
LM20B  = lm(data= FeaturesAndTarget,DeltaOilBeta ~ GDP_GROWTH_WLD_L1  )

summary(LM10A)
summary(LM10B)
summary(LM20A)
summary(LM20B)

LM30A  = lm(data= FeaturesAndTarget,OilBeta      ~ GDP_Growth_USA_YOY  )
LM30B  = lm(data= FeaturesAndTarget,DeltaOilBeta ~ GDP_Growth_USA_YOY  )

LM40A  = lm(data= FeaturesAndTarget,OilBeta      ~ GDP_Growth_WLD_YOY  )
LM40B  = lm(data= FeaturesAndTarget,DeltaOilBeta ~ GDP_Growth_WLD_YOY  )

summary(LM30A)
summary(LM30B)
summary(LM40A)
summary(LM40B)

LM5A  = lm(data= FeaturesAndTarget,OilBeta      ~ GapPercSup  )
LM5B  = lm(data= FeaturesAndTarget,DeltaOilBeta ~ GapPercSup  )

LM6A  = lm(data= FeaturesAndTarget,OilBeta      ~ GapPerc_YOY  )
LM6B  = lm(data= FeaturesAndTarget,DeltaOilBeta ~ GapPerc_YOY  )

summary(LM5A)
summary(LM5B)
summary(LM6A)
summary(LM6B)


LM300B  = lm(data= ( FeaturesAndTarget %>% filter(!(Year %in% c(2020,2021,2022))) ) ,DeltaOilBeta ~ GDP_Growth_USA_YOY  )

summary(LM300B)

LM3000B      = lm(data= FeaturesAndTarget,DeltaOilBeta ~ GDP_Growth_USA_YOY   + GDP_Growth_USA_YOY_L1)

summary(LM3000B)



write.table(FeaturesAndTarget,"clipboard-16384",sep=";",quote=F,row.names=F)


LM7A  = lm(data= FeaturesAndTarget,OilBeta      ~ `__WTC_D`  )
LM7B  = lm(data= FeaturesAndTarget,DeltaOilBeta ~ `__WTC_D`  )

LM8A  = lm(data= FeaturesAndTarget,OilBeta      ~ `__WTC_D_REAL`  )
LM8B  = lm(data= FeaturesAndTarget,DeltaOilBeta ~ `__WTC_D_REAL`  )

summary(LM7A)
summary(LM7B)
summary(LM8A)
summary(LM8B)

#DOES MARKET BETA CORRELATE WITH OIL BETA
LM100 = lm(data = FeaturesAndTarget,OilBeta      ~ MarketBeta  )
summary(LM100)

LM101 = lm(data = FeaturesAndTarget,DeltaOilBeta      ~ DeltaMarketBeta  )
summary(LM101)



#FONT FOR LABELS
FONT1 = list(family = 'Arial',
             size = 18,
             color = 'rgb(82, 82, 82)');

#FONT FOR Y AXIS TITLE
FONT2 = list(family = 'Arial',
             size = 14,
             color = 'rgb(82, 82, 82)');

#############################################################################
################## SCATTER OF YOY GDPO VS YOY OIL BETA ######################
#############################################################################

#THE REGRESSION LINES
RegLines = data.frame(GDP_Growth_USA_YOY = seq(from = -20, to=20,by=1));
RegLines$y_OLS = predict(LM3B     ,newdata = RegLines)
RegLines$y_ROB = predict(LM3B_ROB ,newdata = RegLines)



                      #LINES FOR DISPLAY
theseYearsTR = c(1994,1999,2010,1992,2007,2021)
theseYearsTL = c(1990,2001,2006,2020,2022)

FeaturesAndTarget = FeaturesAndTarget %>%
                    mutate(YearProcTR = ifelse(Year %in% theseYearsTR,Year,"")) %>%
                    mutate(YearProcTL = ifelse(Year %in% theseYearsTL,Year,""))

xaxis1 <- list(title = list(text="YoY Change in US Real GDP Growth (%)",
                            font=FONT2),
              showline = FALSE,
              showgrid = FALSE,
              showticklabels = TRUE,
              linecolor = 'rgb(204, 204, 204)',
              linewidth = 2,
              autotick = FALSE,
              tick0=1990,
              dtick = 5,
              ticks = 'outside',
              tickcolor = 'rgb(204, 204, 204)',
              tickwidth = 2,
              ticklen = 10,
              range=c(-10,12),
              tickfont = FONT1 
              
);



yaxis1 <- list(title = list(text = "YoY Change in Oil Beta",
                           font = FONT2),
              showline = FALSE,
              showticklabels = TRUE,
              linecolor = 'rgb(204, 204, 204)',
              linewidth = 2,
              autotick = FALSE,
              ticks = 'outside',
              tickcolor = 'rgb(204, 204, 204)',
              tickwidth = 2,
              ticklen = 10,
              tick0=0,
              dtick = 0.2,
              ticklabelstep=1,
              showgrid=FALSE,
              gridcolor = 'rgb(204, 204, 204)',
              gridwidth=2,
              zeroline=TRUE,
              #ticksuffix = "  ",
              tickprefix = " ",
              range=c(-0.6,1.0),
              tickfont = FONT1,
              font = FONT1
);


#OUTPUT SCATTER CHART FOR CHOSEN RELATIONSHIP
fig1 = plot_ly(FeaturesAndTarget,
              x = ~ GDP_Growth_USA_YOY, 
              y = ~DeltaOilBeta    , 
              name = '',
              text = ~start,
              type = 'scatter',
              mode = 'markers',
              marker= list(color = 'rgb(0, 0, 0)', size = 10)
) %>%
  
  #ADD TOP RIGHT HAND TEXT LABELS
  add_text( data = FeaturesAndTarget,
            x = ~GDP_Growth_USA_YOY *1, 
            y = ~DeltaOilBeta    *1,
            text = ~YearProcTR,
            showlegend=FALSE,
            textfont = list(size=12,
                            color = 'rgb(100,100,100)'), 
            textposition = "top right") %>%
  
  #ADD TOP LEFT HAND TEXT LABELS
  add_text( data = FeaturesAndTarget,
            x = ~GDP_Growth_USA_YOY *1, 
            y = ~DeltaOilBeta    *1,
            text = ~YearProcTL,
            showlegend=FALSE,
            textfont = list(size=12,
                            color = 'rgb(100,100,100)'), 
            textposition = "top left") %>%
  
  #ADD REGRESSION LINES 
  add_trace(data = RegLines,
            x = ~GDP_Growth_USA_YOY, 
            y = ~y_OLS,
            type = 'scatter',
            mode = 'lines+markers',
            line = list(color = 'rgb(100, 100, 100)', size = 10,dash = "dash"),
            marker = list(color = 'red', opacity=0)
            ) %>%
  
  add_trace(data = RegLines,
            x = ~GDP_Growth_USA_YOY, 
            y = ~y_ROB,
            type = 'scatter',
            mode = 'lines+markers',
            line = list(color = 'rgba(0, 255, 0,0)', size = 10 ),
            marker = list(color = 'red', opacity=0)
  ) #%>%
  
 # add_annotations(
 #   x    = 10.5,
 #   y    = 0.28,
 #   text = paste("y=",round(LM3B$coefficients[2],4),"x+\n",round(LM3B$coefficients[1],4),sep=""),
 #   showarrow = FALSE
 # )


#ADD AXES
fig1 <- fig1 %>% layout(title = "",
                      xaxis = xaxis1,
                      yaxis = yaxis1,
                      showlegend =F
);
        


#WRITE OUT THE PICTURE
reticulate::py_run_string("import sys");
outputRoot = "C:/Users/giles/OneDrive/MBA/Dissertation/Writeup/Pictures/"
fileOut = paste(outputRoot,"OilBeta_GDP_Scatter.png",sep="");
save_image(fig1, fileOut, width = 600, height = 600)



fig2 = plot_ly(FeaturesAndTarget,
               x = ~GDP_Growth, 
               y = ~OilBeta, 
               name = '',
               text = ~start,
               type = 'scatter',
               mode = 'markers',
               marker= list(color = 'rgb(0, 0, 0)', size = 10)
) %>%
  add_text( data = FeaturesAndTarget,
            x = ~GDP_Growth*1, 
            y = ~OilBeta*1,
            text = ~Year,
            showlegend=FALSE,
            textfont = list(size=12,
                            color = 'rgb(100,100,100)'), 
            textposition = "top right")


#############################################################################
################## time history OF YOY GDPO VS YOY OIL BETA #################
#############################################################################


fig3 = plot_ly(
        data = FeaturesAndTarget,
        x = ~Year,
        y = ~GDP_Growth_USA_YOY,
        type= "scatter",
        mode= "lines and markers",
        name= "Change in Annual USA GDP Growth (Diff of %)",
        line = list(color = 'rgb(0, 0, 0)', 
                    width = 2,
                    dash="solid")
        ) %>%
        add_trace(
          data = FeaturesAndTarget,
          x = ~Year,
          y = ~10*DeltaOilBeta,
          name= "Change in Annual Oil-Beta (10x)",
          type= "scatter",
          mode= "lines and markers",
          line = list(color = 'rgb(0, 0, 0)', 
                      width = 2,
                      dash="dash")
        )

xaxis3 <- list(title = "",
               showline = FALSE,
               showgrid = FALSE,
               showticklabels = TRUE,
               linecolor = 'rgb(204, 204, 204)',
               linewidth = 2,
               autotick = FALSE,
               tick0=1990,
               dtick = 5,
               ticks = 'outside',
               tickcolor = 'rgb(204, 204, 204)',
               tickwidth = 2,
               ticklen = 10,
               range=c(1990,2023),
               tickfont = FONT1 
               
);



yaxis3 <- list(title = list(text = "Change in Annual Value (YoY)",
                            font = FONT2),
               showline = FALSE,
               showticklabels = TRUE,
               linecolor = 'rgb(204, 204, 204)',
               linewidth = 2,
               autotick = FALSE,
               ticks = 'outside',
               tickcolor = 'rgb(204, 204, 204)',
               tickwidth = 2,
               ticklen = 10,
               tick0=0,
               dtick = 2,
               ticklabelstep=1,
               showgrid=TRUE,
               gridcolor = 'rgb(204, 204, 204)',
               gridwidth=2,
               zeroline=TRUE,
               #ticksuffix = "  ",
               tickprefix = " ",
               tickformat="+2",
               range=c(-5,9),
               tickfont = FONT1,
               font = FONT1
);

#FONT FOR LEGEND
SETUP_LEGEND3 = list(
  font = list(
    family = 'Arial',
    size = 14,
    color = 'rgb(82, 82, 82)'
  ),
  x=0.05,
  y=0.90,
  bordercolor = 'rgba(255, 255, 255,0)',
  borderwidth = 2,
  bgcolor='rgba(0,0,0,0)'
);

fig3 = fig3 %>%
       layout(
         xaxis  = xaxis3,
         yaxis  = yaxis3,
         legend = SETUP_LEGEND3
       )

fig3


#WRITE OUT THE PICTURE
reticulate::py_run_string("import sys");
outputRoot = "C:/Users/giles/OneDrive/MBA/Dissertation/Writeup/Pictures/"
fileOut = paste(outputRoot,"OilBeta_GDP_TimeHist.png",sep="");
save_image(fig3, fileOut, width = 600, height = 600);



#############################################################################
################## SCATTER OF DELTA MARKET BETA VS DELTA OIL BETA  ##########
#############################################################################

#THE REGRESSION LINES
RegLines2 = data.frame(DeltaMarketBeta = seq(from = -20, to=20,by=1));
RegLines2$y_OLS = predict(LM101     ,newdata = RegLines2)
RegLines2$y_ROB = predict(LM101 ,   newdata = RegLines2)





#LINES FOR DISPLAY
theseYearsTR2 = c(2021,1999,2001,1994,2020,2000,2009,2022,2005)
theseYearsTL2 = c(1993,2007,2017,2006,2003,1995)

FeaturesAndTarget = FeaturesAndTarget %>%
  mutate(YearProcTR2 = ifelse(Year %in% theseYearsTR2,Year,"")) %>%
  mutate(YearProcTL2 = ifelse(Year %in% theseYearsTL2,Year,""))

xaxis101 <- list(title = list(text="YoY Change in Market Beta",
                            font=FONT2),
               showline = FALSE,
               showgrid = FALSE,
               showticklabels = TRUE,
               linecolor = 'rgb(204, 204, 204)',
               linewidth = 2,
               autotick = FALSE,
               tick0=1990,
               dtick = 1.0,
               ticks = 'outside',
               tickcolor = 'rgb(204, 204, 204)',
               tickwidth = 2,
               ticklen = 10,
               range=c(-2.5,2.5),
               tickfont = FONT1 ,
               tickformat = "+2"
               
);



yaxis101 <- list(title = list(text = "YoY Change in Oil Beta",
                            font = FONT2),
               showline = FALSE,
               showticklabels = TRUE,
               linecolor = 'rgb(204, 204, 204)',
               linewidth = 2,
               autotick = FALSE,
               ticks = 'outside',
               tickcolor = 'rgb(204, 204, 204)',
               tickwidth = 2,
               ticklen = 10,
               tick0=0,
               dtick = 0.25,
               ticklabelstep=1,
               showgrid=FALSE,
               gridcolor = 'rgb(204, 204, 204)',
               gridwidth=2,
               zeroline=TRUE,
               #ticksuffix = "  ",
               tickprefix = " ",
               range=c(-0.6,1.0),
               tickfont = FONT1,
               font = FONT1,
               tickformat= "+2"
);


#OUTPUT SCATTER CHART FOR CHOSEN RELATIONSHIP
fig101 = plot_ly(FeaturesAndTarget,
               x = ~ DeltaMarketBeta, 
               y = ~ DeltaOilBeta    , 
               name = "",
               text = ~start,
               type = 'scatter',
               mode = 'markers',
               marker= list(color = 'rgb(0, 0, 0)', size = 10)
) %>%
  
  #ADD TOP RIGHT HAND TEXT LABELS
  add_text( data = FeaturesAndTarget,
            x = ~DeltaMarketBeta *1, 
            y = ~DeltaOilBeta    *1,
           
            # text = ~Year,
            text = ~YearProcTR2,
            showlegend=FALSE,
            textfont = list(size=12,
                            color = 'rgb(100,100,100)'), 
            textposition = "top right") %>%
  
  #ADD TOP LEFT HAND TEXT LABELS
  add_text( data = FeaturesAndTarget,
            x = ~DeltaMarketBeta *1, 
            y = ~DeltaOilBeta    *1,
            text = ~YearProcTL2,
            showlegend=FALSE,
            textfont = list(size=12,
                            color = 'rgb(100,100,100)'), 
            textposition = "top left") %>%
  
  #ADD REGRESSION LINES 
  add_trace(data = RegLines2,
            x = ~DeltaMarketBeta, 
            y = ~y_OLS,
            type = 'scatter',
            mode = 'lines+markers',
            line = list(color = 'rgb(100, 100, 100)', size = 10,dash = "dash"),
            marker = list(color = 'red', opacity=0)
  ) %>%
  
  add_trace(data = RegLines2,
            x = ~DeltaMarketBeta, 
            y = ~y_ROB,
            type = 'scatter',
            mode = 'lines+markers',
            line = list(color = 'rgba(0, 255, 0,0)', size = 10 ),
            marker = list(color = 'red', opacity=0)
  ) #%>%

# add_annotations(
#   x    = 10.5,
#   y    = 0.28,
#   text = paste("y=",round(LM3B$coefficients[2],4),"x+\n",round(LM3B$coefficients[1],4),sep=""),
#   showarrow = FALSE
# )


#ADD AXES
fig101 <- fig101 %>% layout(title = "",
                        xaxis = xaxis101,
                        yaxis = yaxis101,
                        showlegend =F
);


fig101

#WRITE OUT THE PICTURE
reticulate::py_run_string("import sys");
outputRoot = "C:/Users/giles/OneDrive/MBA/Dissertation/Writeup/Pictures/"
fileOut = paste(outputRoot,"OilBeta_MarketBeta01.png",sep="");
save_image(fig101, fileOut, width = 600, height = 600);


#############################################################################
################## SCATTER OF MARKET BETA VS OIL BETA  ######################
#############################################################################

#THE REGRESSION LINES
RegLines3 = data.frame(MarketBeta = seq(from = -20, to=20,by=1));
RegLines3$y_OLS = predict(LM100     ,newdata = RegLines3)
RegLines3$y_ROB = predict(LM100,   newdata = RegLines3)





#LINES FOR DISPLAY
theseYearsTR3 = c(2021,2020,2009,2022,2005,2002)
theseYearsTL3 = c(1993,2017,2006,2003,2000,1999)

FeaturesAndTarget = FeaturesAndTarget %>%
  mutate(YearProcTR3 = ifelse(Year %in% theseYearsTR3,Year,"")) %>%
  mutate(YearProcTL3 = ifelse(Year %in% theseYearsTL3,Year,""))

xaxis100 <- list(title = list(text="Market Beta  (1Y bi-weekly)",
                              font=FONT2),
                 showline = FALSE,
                 showgrid = FALSE,
                 showticklabels = TRUE,
                 linecolor = 'rgb(204, 204, 204)',
                 linewidth = 2,
                 autotick = FALSE,
                 tick0=1990,
                 dtick = 1.0,
                 ticks = 'outside',
                 tickcolor = 'rgb(204, 204, 204)',
                 tickwidth = 2,
                 ticklen = 10,
                 range=c(-1.0,2.0),
                 tickfont = FONT1 ,
                 tickformat = "2"
                 
);



yaxis100 <- list(title = list(text = "Oil Beta (1Y bi-weekly)",
                              font = FONT2),
                 showline = FALSE,
                 showticklabels = TRUE,
                 linecolor = 'rgb(204, 204, 204)',
                 linewidth = 2,
                 autotick = FALSE,
                 ticks = 'outside',
                 tickcolor = 'rgb(204, 204, 204)',
                 tickwidth = 2,
                 ticklen = 10,
                 tick0=0,
                 dtick = 0.25,
                 ticklabelstep=1,
                 showgrid=FALSE,
                 gridcolor = 'rgb(204, 204, 204)',
                 gridwidth=2,
                 zeroline=TRUE,
                 #ticksuffix = "  ",
                 tickprefix = " ",
                 range=c(-00,1.0),
                 tickfont = FONT1,
                 font = FONT1
                 #tickformat= "+2"
);


#OUTPUT SCATTER CHART FOR CHOSEN RELATIONSHIP
fig100 = plot_ly(FeaturesAndTarget,
                 x = ~ MarketBeta, 
                 y = ~ OilBeta    , 
                 name = "",
                 text = ~start,
                 type = 'scatter',
                 mode = 'markers',
                 marker= list(color = 'rgb(0, 0, 0)', size = 10)
) %>%
  
  #ADD TOP RIGHT HAND TEXT LABELS
  add_text( data = FeaturesAndTarget,
            x = ~MarketBeta *1, 
            y = ~OilBeta    *1,
            
             #text = ~Year,
            text = ~YearProcTR3,
            showlegend=FALSE,
            textfont = list(size=12,
                            color = 'rgb(100,100,100)'), 
            textposition = "top right") %>%
  
  #ADD TOP LEFT HAND TEXT LABELS
  add_text( data = FeaturesAndTarget,
            x = ~MarketBeta *1, 
            y = ~OilBeta    *1,
            text = ~YearProcTL3,
            showlegend=FALSE,
            textfont = list(size=12,
                            color = 'rgb(100,100,100)'), 
            textposition = "top left") %>%
  
  #ADD REGRESSION LINES 
  add_trace(data = RegLines3,
            x = ~MarketBeta, 
            y = ~y_OLS,
            type = 'scatter',
            mode = 'lines+markers',
            line = list(color = 'rgb(100, 100, 100)', size = 10,dash = "dash"),
            marker = list(color = 'red', opacity=0)
  ) %>%
  
  add_trace(data = RegLines3,
            x = ~MarketBeta, 
            y = ~y_ROB,
            type = 'scatter',
            mode = 'lines+markers',
            line = list(color = 'rgba(0, 255, 0,0)', size = 10 ),
            marker = list(color = 'red', opacity=0)
  ) #%>%

# add_annotations(
#   x    = 10.5,
#   y    = 0.28,
#   text = paste("y=",round(LM3B$coefficients[2],4),"x+\n",round(LM3B$coefficients[1],4),sep=""),
#   showarrow = FALSE
# )


#ADD AXES
fig100 <- fig100 %>% layout(title = "",
                            xaxis = xaxis100,
                            yaxis = yaxis100,
                            showlegend =F
);


fig100

#WRITE OUT THE PICTURE
reticulate::py_run_string("import sys");
outputRoot = "C:/Users/giles/OneDrive/MBA/Dissertation/Writeup/Pictures/"
fileOut = paste(outputRoot,"OilBeta_MarketBeta02.png",sep="");
save_image(fig100, fileOut, width = 600, height = 600);

