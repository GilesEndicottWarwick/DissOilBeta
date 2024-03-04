#LIBRARIES
require(tidyverse)
require(lubridate)
require(readxl)
require(partykit)
require(plotly)

#LOAD ROLLING BETA DATA
load("C:/Users/giles/OneDrive/MBA/Dissertation/Data/RollingBeta.Rdata")

#FILTER ROLLING BETA TO ONLY ENERGY GROUP
OilBeta2Y_ROB = collectRegressionData %>%
                 filter(Symbol == "Energy") %>%
                 filter(Regression == "ROB") %>%
                 filter(term == "OilRet") %>%
                 select(Symbol,start,end,OilBeta=estimate) %>%
                 mutate(YearStart  = as.numeric(str_sub(start,1,4))) %>%
                 mutate(MonthStart = as.numeric(str_sub(start,5,6))) %>%
                 filter(MonthStart == 1) %>%
                 mutate(OilBetaLag = lag(OilBeta)) %>%
                 mutate(DeltaOilBeta = OilBeta - OilBetaLag)


#GET MACROECONOMIC DATA AND SUMMARISE TO TWO YEAR BLOCKS, IE REAL GDP GROWTH CHART
fileIn1= "C:/Users/giles/OneDrive/MBA/Dissertation/Data/API_NY.GDP.MKTP.KD.ZG_DS2_en_excel_v2_6298898.xls"

DataIn_1 = read_excel(fileIn1,sheet="Data",skip=2)

#THE COUNTRIES WE FILTER
InterestCountries = c("United States")

#FILTER TO THOSE
DataIn_2 = DataIn_1 %>%
  filter(`Country Name` %in% InterestCountries ) %>%
  pivot_longer(cols = ! c("Country Name" ,,  "Country Code","Indicator Name","Indicator Code" )) %>%
  rename(Year= name) %>%
  rename(GDP_Growth = value) %>%
  rename(Country_Name=`Country Name`) %>%
  filter(!is.na(GDP_Growth)) %>%
  mutate(GDP_Growth = as.numeric(GDP_Growth)) %>%
  mutate(Year = as.numeric(Year)) %>%
  ungroup() %>%
  filter(Year >= 1989) %>%
  select(Year,GDP_Growth)

#MOVE TO FORMAT OLF TWO YEARS
seq1 = seq(from=1990,to=2023,by=1)
SKEL1 = expand.grid(start = seq1,end = seq1) %>%
        filter(end-start == 1)

SKEL2 = data.frame(start = seq1) %>%
        mutate(end = start+ 1) %>%
        left_join(DataIn_2,by=c("start" = "Year")) %>%
        left_join(DataIn_2,by=c("end" = "Year")) %>%
        pivot_longer(cols = starts_with("GDP")) %>%
        select(-name);

#REAL GDP GROWTH IN ABS TGERMS
GDP_OUTPUT_1 = SKEL2 %>%
             group_by(start,end) %>%
             summarise(
               meanRealGDPGrowth = mean(value),
               minRealGDPGrowth  = min(value),
               maxRealGDPGrowth  = max(value),
               nNegRealGDPGrowth = sum(ifelse(value<0,1,0)),
               .groups="drop"
             )

#LOOK AT GDP GROWTH IN VS LAST YEAR TERMS
GDP_OUTPUT_2A = DataIn_2 %>%
               mutate(Lag1 = lag(GDP_Growth)) %>%
               filter(Year >= 1990) %>%
               mutate(Diff_YOY = GDP_Growth - Lag1) %>%
               select(Year,Diff_YOY)

SKEL3 = data.frame(start = seq1) %>%
          mutate(end = start+ 1) %>%
          left_join(GDP_OUTPUT_2A,by=c("start" = "Year")) %>%
          left_join(GDP_OUTPUT_2A,by=c("end" = "Year")) %>%
          pivot_longer(cols = starts_with("Diff")) %>%
          select(-name);

GDP_OUTPUT_2 = SKEL3 %>%
              group_by(start,end) %>%
              summarise(
                meanRealGDPGrowthYOY = mean(value),
                minRealGDPGrowthYOY  = min(value),
                maxRealGDPGrowthYOY  = max(value),
                nNegRealGDPGrowthYOY = sum(ifelse(value<0,1,0)),
                .groups="drop"
              )

#THE FINAL FEATURES FROM US REAL GDP GROWTH NUMBERS
GDP_REAL_ALL = GDP_OUTPUT_1 %>%
               left_join(GDP_OUTPUT_2,by=c("start","end"))





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

SKEL_OIL_1 = data.frame(start = seq1) %>%
              mutate(end = start+ 1) %>%
              left_join(DataOil3,by=c("start" = "Year")) %>%
              left_join(DataOil3,by=c("end" = "Year")) %>%
              pivot_longer(cols = starts_with("Gap")) %>%
              select(-name);

SKEL_OIL_2 = data.frame(start = seq1) %>%
              mutate(end = start+ 1) %>%
              left_join(DataOil4_YOY,by=c("start" = "Year")) %>%
              left_join(DataOil4_YOY,by=c("end" = "Year")) %>%
              pivot_longer(cols = starts_with("Gap")) %>%
              select(-name);

OILGAP_OUTPUT_1 = SKEL_OIL_1 %>%
                  group_by(start,end) %>%
                  summarise(
                    meanOilGapPerc = mean(value),
                    minOilGapPerc  = min(value),
                    maxOilGapPerc  = max(value),
                    nNegOilGapPerc = sum(ifelse(value<0,1,0)),
                    .groups="drop"
                  )

OILGAP_OUTPUT_2 = SKEL_OIL_2 %>%
                  group_by(start,end) %>%
                  summarise(
                    meanOilGapPercYOY = mean(value),
                    minOilGapPercYOY  = min(value),
                    maxOilGapPercYOY  = max(value),
                    nNegOilGapPercYOY = sum(ifelse(value<0,1,0)),
                    .groups="drop"
                  )

OilGapAll = OILGAP_OUTPUT_1 %>%
            left_join(OILGAP_OUTPUT_2,by=c("start","end"))

#FILTER OIL BETA DOWN TO JAN STARTING 2Y EVALUATIONS






#COMBINE TARGET (OIL BETA) AND FEATURES (GDP CHANGES AND OIL-SUPPLY-DEMAND CHANGES)
FeaturesAndTarget = OilBeta2Y_ROB %>%
                    left_join(GDP_REAL_ALL,by=c("YearStart"="start")) %>%
                    left_join(OilGapAll,by=c("YearStart"="start"))

LM1 = lm(data = FeaturesAndTarget, OilBeta ~ meanRealGDPGrowth + minRealGDPGrowth + maxRealGDPGrowth + nNegRealGDPGrowth + 
                                      meanRealGDPGrowthYOY + minRealGDPGrowthYOY + maxRealGDPGrowthYOY + nNegRealGDPGrowthYOY )

LM2 = lm(data = FeaturesAndTarget, DeltaOilBeta ~ minRealGDPGrowth + nNegRealGDPGrowth + minRealGDPGrowthYOY +  nNegRealGDPGrowthYOY )

LM3 = lm(data = FeaturesAndTarget, OilBeta ~  maxRealGDPGrowth + maxRealGDPGrowthYOY )
LM4 = lm(data = FeaturesAndTarget, OilBeta ~  meanRealGDPGrowth + meanRealGDPGrowthYOY )


LM5 = lm(data = FeaturesAndTarget, OilBeta ~  meanOilGapPerc  )
summary(LM5)

LM6 = lm(data= FeaturesAndTarget[,c(4,10:17,19:26)],OilBeta ~ .)
summary(LM6)

LM7 = lm(data= FeaturesAndTarget,OilBeta ~ meanRealGDPGrowthYOY    + minOilGapPerc         + nNegOilGapPerc       )
summary(LM7)

LM8 = lm(data= FeaturesAndTarget,OilBeta ~ meanRealGDPGrowthYOY    + minOilGapPerc       )
summary(LM8)

LM9 = lm(data= FeaturesAndTarget,OilBeta ~  nNegRealGDPGrowthYOY  +nNegOilGapPerc )
summary(LM9)

LM10  = lm(data= FeaturesAndTarget,DeltaOilBeta ~ meanRealGDPGrowthYOY )
summary(LM10)


write.table(FeaturesAndTarget,"clipboard-16384",sep=";",quote=F,row.names=F)

fig1 = plot_ly(FeaturesAndTarget,
              x = ~meanOilGapPerc, 
              y = ~OilBeta, 
              name = '',
              text = ~start,
              type = 'scatter',
              mode = 'markers',
              marker= list(color = 'rgb(0, 0, 0)', size = 10)
) %>%
  add_text( data = FeaturesAndTarget,
            x = ~meanOilGapPerc*1, 
            y = ~OilBeta*1,
            text = ~start,
            showlegend=FALSE,
            textfont = list(size=12,
                            color = 'rgb(100,100,100)'), 
            textposition = "top right")
fig1  

fig2 = plot_ly(FeaturesAndTarget,
               x = ~meanRealGDPGrowthYOY, 
               y = ~OilBeta, 
               name = '',
               text = ~start,
               type = 'scatter',
               mode = 'markers',
               marker= list(color = 'rgb(0, 0, 0)', size = 10)
) %>%
  add_text( data = FeaturesAndTarget,
            x = ~meanRealGDPGrowthYOY*1, 
            y = ~OilBeta*1,
            text = ~start,
            showlegend=FALSE,
            textfont = list(size=12,
                            color = 'rgb(100,100,100)'), 
            textposition = "top right")
fig2 


