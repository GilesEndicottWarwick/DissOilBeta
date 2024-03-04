#LIBRARIES
require(tidyverse)
require(lubridate)
require(readxl)
require(partykit)
require(plotly)
require(robustbase)
require(data.table)

#LOAD ROLLING BETA DATA FROM RollingBetaParOil_Monthly.R
load("C:/Users/giles/OneDrive/MBA/Dissertation/Data/RollingBetaIndustryMonthly.Rdata")

#FILTER ROLLING BETA TO ONLY ENERGY GROUP
OilBeta1Y_ROB = collectRegressionData %>%
                 filter(Symbol == "Energy") %>%
                 filter(Regression == "ROB") %>%
                 filter(term == "OilRet") %>%
                 select(Symbol,start,end,OilBeta=estimate) %>%
                 mutate(Year  = as.numeric(str_sub(start,1,4))) %>%
                 mutate(WeekStart = as.numeric(str_sub(start,5,6))) %>%
                 mutate(OilBetaLag = lag(OilBeta)) %>%
                 mutate(DeltaOilBeta = OilBeta - OilBetaLag)


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
  

#COMBINE TARGET (OIL BETA) AND FEATURES (GDP CHANGES AND OIL-SUPPLY-DEMAND CHANGES)
FeaturesAndTarget = OilBeta1Y_ROB %>%
                    left_join(GDP_REAL_ALL,by=c("Year")) %>%
                    left_join(OilGapAll,by=c("Year")) %>%
                    left_join(SP500_1,by=c("Year")) %>%
                    left_join(SP500_2,by=c("Year")) %>%
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









write.table(FeaturesAndTarget,"clipboard-16384",sep=";",quote=F,row.names=F)

#FONT FOR LABELS
FONT1 = list(family = 'Arial',
             size = 18,
             color = 'rgb(82, 82, 82)');

#FONT FOR Y AXIS TITLE
FONT2 = list(family = 'Arial',
             size = 14,
             color = 'rgb(82, 82, 82)');

#############################################################################
################## S & P 500 INDEX HISTORY ##################################
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
              range=c(-0.4,0.8),
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
#fileOut = paste(outputRoot,"OilBeta_GDP_Scatter.png",sep="");
save_image(fig1, fileOut, width = 600, height = 400)



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
fig2 


