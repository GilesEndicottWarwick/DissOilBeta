#LIBRARIES
require(tidyverse)
require(lubridate)
require(robustbase)
require(broom)
require(quantreg)
require(plotly)
require(data.table)

#############################################################################
################## OUTPUT SETUP #############################################
#############################################################################

outputRoot = "C:/Users/giles/OneDrive/MBA/Dissertation/Writeup/Pictures/"

#############################################################################
################## INPUT ###################################################
#############################################################################

#LOAD FILE IN (COMES FROM AllTimeMarketBetaAnalysis.R)
load("C:/Users/giles/OneDrive/MBA/Dissertation/Data/AllYear_Market_Beta_Calc.Rdata")

excludeSymbolList = c("GC=F")


#############################################################################
################## GLOBAL SETUP  ############################################
#############################################################################

#FONT FOR LABELS
FONT1 = list(family = 'Arial',
             size = 24,
             color = 'rgb(82, 82, 82)');

#FONT FOR Y AXIS TITLE
FONT2 = list(family = 'Arial',
             size = 20,
             color = 'rgb(82, 82, 82)');

#FONT FOR LISTING
FONT3 = list(family = 'Arial',
             size = 15,
             color = 'rgb(82, 82, 82)');

#############################################################################
################## MARKET BETA TABLES #######################################
#############################################################################

MarketBetaSummary = allRegData %>%
  filter(Model =="SP500 Only") %>%
  mutate(Industry2 = ifelse(SubIndustry == "Passenger Airlines","Passenger Airlines",Industry)) %>%
  filter(!(Symbol %in% excludeSymbolList)) %>%
  group_by(Industry2,term,Regression) %>%
  summarise(
    nTotal         = n(),
    nSignificant   = sum(ifelse(p.value < 00.05,1,0)),
    nSignificantPerc   = round( 100*sum(ifelse(p.value < 00.05,1,0)) / n(),1),
    MeanBeta       = round(mean(estimate),4),
    MedianBeta     = round(median(estimate),4),
    sdBeta         = round(sd(estimate),4),
    meanRSQ        = round(mean(R2),4),
    medianRSQ      = round(median(R2),4)
  ) %>%
  ungroup() %>%
  filter(term != "(Intercept)") %>%
  select(-term);

#WRITE OUTPUT TO CSV FOR IMPORT TO WORD
write.table(MarketBetaSummary,paste(outputRoot,"marketBetaTable1.csv",sep=""),sep=",",quote=F,row.names=F)


#COMPARE MARKET BETA FOR DIFF METHODOLOGIES
MarketRegressionSummary_S1 = allRegData %>%
                              filter(Model =="SP500 Only") %>%
                              filter(!(Symbol %in% excludeSymbolList)) %>%
                              filter(term == "SP500AbnormalReturnAnnualized") %>%
                              filter(nMonths > 0 ) %>%
                              dplyr::select(Symbol,Regression,MarketBeta = estimate) %>%
                              pivot_wider(id_cols=c("Symbol"),names_from = c("Regression"),values_from = "MarketBeta");

#CORRELATION COEFFICIENT
methodCorr = cor(MarketRegressionSummary_S1[,2:4]);

methodRsq = methodCorr^2


#############################################################################
################## MARKET BETA CHARTS #######################################
#############################################################################

MarketBetaSummary_BetaValue = allRegData %>%
  filter(Model =="SP500 Only") %>%
  mutate(Industry2 = ifelse(SubIndustry == "Passenger Airlines","Passenger Airlines",Industry)) %>%
  filter(!(Symbol %in% excludeSymbolList)) %>%
  filter(term == "SP500AbnormalReturnAnnualized") %>%
  filter(Regression == "OLS")


#BOXPLOT OF BETA VALUE SPLIT BY INDUSTRY
xaxis <- list(title = list(text = "Beta Estimate",
                           font = FONT3),
              showline = TRUE,
              showgrid = FALSE,
              zeroline = FALSE,
              showticklabels = TRUE,
              linecolor = 'rgb(204, 204, 204)',
              linewidth = 2,
              autotick = FALSE,
              tick0=0.0,
              dtick = 1.0,
              ticks = 'outside',
              tickcolor = 'rgb(204, 204, 204)',
              tickwidth = 2,
              #ticklen = 5,
              range=c(0.0,3.0),
              tickfont = FONT2 
              
);



yaxis <- list(title = list(text = "",
                           font = FONT3),
              showline = FALSE,
              showticklabels = TRUE,
              linecolor = 'rgb(204, 204, 204)',
              ticks = 'outside',
              tickcolor = 'rgb(204, 204, 204)',
              tickwidth = 2,
              ticklen = 0,
              ticklabelstep=1,
              showgrid=FALSE,
              gridcolor = 'rgb(204, 204, 204)',
              gridwidth=2,
              tickfont = FONT3,
              font = FONT3,
              autorange="reversed"
);

fig1 = plot_ly(
  data = MarketBetaSummary_BetaValue,
  y    = ~Industry2,
  x    = ~estimate,
  split = ~Regression,
  type = "box",
  #fillcolor = 'rgba(0,0,0,0)',
  line = list(color='rgba(0,0,0,1)'),
  marker = list(color = 'rgb(7,40,89)',
                outliercolor = 'rgba(0, 0, 0, 1.0)',
                fillcolor = 'rgb(255,255,255)',
                line =  list(outliercolor = 'rgba(0, 0, 0, 1.0)',
                             outlierwidth = 0)
  )
);

#ADD ANNOTATION FOR NUMBER OF COMPANIES
#fig1 = fig1 %>%
#       add_annotations(
#         data = MarketBetaSummary_BetaValue,
#         x = 2,
#         text = "n=5",
#         yshift = 10,
#         showarrow = FALSE
#       )


#ADD AXIS FORMATTING
fig1 <- fig1 %>% layout(title = "",
                        xaxis = xaxis,
                        yaxis = yaxis,
                        boxmode="group"
);

fig1



#BOXPLOT OF RSQ VALUE SPLIT BY INDUSTRY 
xaxis <- list(title = list(text = "r² of Market CAPM",
                           font = FONT3),
              showline = TRUE,
              showgrid = FALSE,
              zeroline = FALSE,
              showticklabels = TRUE,
              linecolor = 'rgb(204, 204, 204)',
              linewidth = 2,
              autotick = FALSE,
              tick0=0.0,
              dtick = 0.25,
              ticks = 'outside',
              tickcolor = 'rgb(204, 204, 204)',
              tickwidth = 2,
              #ticklen = 5,
              range=c(0.0,0.75),
              tickfont = FONT2 
              
);



yaxis <- list(title = list(text = "",
                           font = FONT3),
              showline = FALSE,
              showticklabels = TRUE,
              linecolor = 'rgb(204, 204, 204)',
              ticks = 'outside',
              tickcolor = 'rgb(204, 204, 204)',
              tickwidth = 2,
              ticklen = 0,
              ticklabelstep=1,
              showgrid=FALSE,
              gridcolor = 'rgb(204, 204, 204)',
              gridwidth=2,
              tickfont = FONT3,
              font = FONT3,
              autorange="reversed"
);

fig2 = plot_ly(
  data = MarketBetaSummary_BetaValue,
  y    = ~Industry2,
  x    = ~R2,
  split = ~Regression,
  type = "box",
  #fillcolor = 'rgba(0,0,0,0)',
  line = list(color='rgba(0,0,0,1)'),
  marker = list(color = 'rgb(7,40,89)',
                outliercolor = 'rgba(0, 0, 0, 1.0)',
                fillcolor = 'rgb(255,255,255)',
                line =  list(outliercolor = 'rgba(0, 0, 0, 1.0)',
                             outlierwidth = 0)
  )
);

#STACK THEM 
fig2 = fig2 %>% layout (boxmode="group")

#ADD AXIS FORMATTING
fig2 <- fig2 %>% layout(title = "",
                        xaxis = xaxis,
                        yaxis = yaxis
);

fig2

#WRITE FIGURES 1 AND 2 OUT
#WRITE OUT THE PICTURE
reticulate::py_run_string("import sys");
fileOut = paste(outputRoot,"MarketBeta_Beta.png",sep="");
save_image(fig1, fileOut, width = 500, height = 400)

#WRITE OUT THE PICTURE
reticulate::py_run_string("import sys");
fileOut = paste(outputRoot,"MarketBeta_R2.png",sep="");
save_image(fig2, fileOut, width = 500, height = 400)

#############################################################################
################## DIFFERENCE FROM ROBUST METHODS ###########################
#############################################################################

dataRobEffect = allRegData %>%
                filter(Model =="SP500 Only") %>%
                mutate(Industry2 = ifelse(SubIndustry == "Passenger Airlines","Passenger Airlines",Industry)) %>%
                filter(!(Symbol %in% excludeSymbolList)) %>%
                filter(term == "SP500AbnormalReturnAnnualized") %>%
                filter(FirstDate <=199001) %>%
                select(Industry2,Symbol,Regression,estimate) %>%
                pivot_wider(names_from = "Regression",values_from="estimate") %>%
                mutate(LmRobDiff = `lmrob Robust` - `OLS`) %>%
                mutate(qrDiff = `Quantile Reg` - `OLS`) %>%
                mutate(robDiff = `Quantile Reg` - `lmrob Robust`)

dataRobEffectEnergy = dataRobEffect %>%
                      filter(Industry2 == "Energy");

#BOXPLOT OF BETA VALUE SPLIT BY INDUSTRY
xaxis100 <- list(title = list(text = "Beta (MM Robust) - Beta (OLS)",
                           font = FONT3),
              showline = TRUE,
              showgrid = FALSE,
              zeroline = FALSE,
              showticklabels = TRUE,
              linecolor = 'rgb(204, 204, 204)',
              linewidth = 2,
              autotick = FALSE,
              tick0=0.0,
              dtick = 0.50,
              ticks = 'outside',
              tickcolor = 'rgb(204, 204, 204)',
              tickwidth = 2,
              #ticklen = 5,
              range=c(-0.75,0.5),
              tickfont = FONT2 
              
);



yaxis100 <- list(title = list(text = "",
                           font = FONT3),
              showline = FALSE,
              showticklabels = TRUE,
              linecolor = 'rgb(204, 204, 204)',
              ticks = 'outside',
              tickcolor = 'rgb(204, 204, 204)',
              tickwidth = 2,
              ticklen = 0,
              ticklabelstep=1,
              showgrid=FALSE,
              gridcolor = 'rgb(204, 204, 204)',
              gridwidth=2,
              tickfont = FONT3,
              font = FONT3,
              autorange="reversed"
);

fig100 = plot_ly(
  data = dataRobEffect,
  y    = ~Industry2,
  x    = ~LmRobDiff ,
  type = "box",
  #fillcolor = 'rgba(0,0,0,0)',
  line = list(color='rgba(0,0,0,1)'),
  marker = list(color = 'rgb(7,40,89)',
                outliercolor = 'rgba(0, 0, 0, 1.0)',
                fillcolor = 'rgb(255,255,255)',
                line =  list(outliercolor = 'rgba(0, 0, 0, 1.0)',
                             outlierwidth = 0)
  )
);

#ADD ANNOTATION FOR NUMBER OF COMPANIES
#fig1 = fig1 %>%
#       add_annotations(
#         data = MarketBetaSummary_BetaValue,
#         x = 2,
#         text = "n=5",
#         yshift = 10,
#         showarrow = FALSE
#       )


#ADD AXIS FORMATTING
fig100 <- fig100 %>% layout(title = "",
                        xaxis = xaxis100,
                        yaxis = yaxis100,
                        boxmode="group"
);

fig100




#BOXPLOT OF BETA VALUE SPLIT BY INDUSTRY
xaxis101 <- list(title = list(text = "Beta (QR) - Beta (OLS)",
                              font = FONT3),
                 showline = TRUE,
                 showgrid = FALSE,
                 zeroline = FALSE,
                 showticklabels = TRUE,
                 linecolor = 'rgb(204, 204, 204)',
                 linewidth = 2,
                 autotick = FALSE,
                 tick0=0.0,
                 dtick = 0.5,
                 ticks = 'outside',
                 tickcolor = 'rgb(204, 204, 204)',
                 tickwidth = 2,
                 #ticklen = 5,
                 range=c(-0.75,0.5),
                 tickfont = FONT2 
                 
);



yaxis101 <- list(title = list(text = "",
                              font = FONT3),
                 showline = FALSE,
                 showticklabels = TRUE,
                 linecolor = 'rgb(204, 204, 204)',
                 ticks = 'outside',
                 tickcolor = 'rgb(204, 204, 204)',
                 tickwidth = 2,
                 ticklen = 0,
                 ticklabelstep=1,
                 showgrid=FALSE,
                 gridcolor = 'rgb(204, 204, 204)',
                 gridwidth=2,
                 tickfont = FONT3,
                 font = FONT3,
                 autorange="reversed"
);

fig101 = plot_ly(
  data = dataRobEffect,
  y    = ~Industry2,
  x    = ~qrDiff ,
  type = "box",
  #fillcolor = 'rgba(0,0,0,0)',
  line = list(color='rgba(0,0,0,1)'),
  marker = list(color = 'rgb(7,40,89)',
                outliercolor = 'rgba(0, 0, 0, 1.0)',
                fillcolor = 'rgb(255,255,255)',
                line =  list(outliercolor = 'rgba(0, 0, 0, 1.0)',
                             outlierwidth = 0)
  )
);

#ADD ANNOTATION FOR NUMBER OF COMPANIES
#fig1 = fig1 %>%
#       add_annotations(
#         data = MarketBetaSummary_BetaValue,
#         x = 2,
#         text = "n=5",
#         yshift = 10,
#         showarrow = FALSE
#       )


#ADD AXIS FORMATTING
fig101 <- fig101 %>% layout(title = "",
                            xaxis = xaxis101,
                            yaxis = yaxis101,
                            boxmode="group"
);

fig101


#BOXPLOT OF BETA VALUE SPLIT BY INDUSTRY
xaxis102 <- list(title = list(text = "Beta (QR) - Beta (MM)",
                              font = FONT3),
                 showline = TRUE,
                 showgrid = FALSE,
                 zeroline = FALSE,
                 showticklabels = TRUE,
                 linecolor = 'rgb(204, 204, 204)',
                 linewidth = 2,
                 autotick = FALSE,
                 tick0=0.0,
                 dtick = 0.25,
                 ticks = 'outside',
                 tickcolor = 'rgb(204, 204, 204)',
                 tickwidth = 2,
                 #ticklen = 5,
                 range=c(-0.4,0.4),
                 tickfont = FONT2 
                 
);



yaxis102 <- list(title = list(text = "",
                              font = FONT3),
                 showline = FALSE,
                 showticklabels = TRUE,
                 linecolor = 'rgb(204, 204, 204)',
                 ticks = 'outside',
                 tickcolor = 'rgb(204, 204, 204)',
                 tickwidth = 2,
                 ticklen = 0,
                 ticklabelstep=1,
                 showgrid=FALSE,
                 gridcolor = 'rgb(204, 204, 204)',
                 gridwidth=2,
                 tickfont = FONT3,
                 font = FONT3,
                 autorange="reversed"
);

fig102 = plot_ly(
  data = dataRobEffect,
  y    = ~Industry2,
  x    = ~robDiff ,
  type = "box",
  #fillcolor = 'rgba(0,0,0,0)',
  line = list(color='rgba(0,0,0,1)'),
  marker = list(color = 'rgb(7,40,89)',
                outliercolor = 'rgba(0, 0, 0, 1.0)',
                fillcolor = 'rgb(255,255,255)',
                line =  list(outliercolor = 'rgba(0, 0, 0, 1.0)',
                             outlierwidth = 0)
  )
);

#ADD ANNOTATION FOR NUMBER OF COMPANIES
#fig1 = fig1 %>%
#       add_annotations(
#         data = MarketBetaSummary_BetaValue,
#         x = 2,
#         text = "n=5",
#         yshift = 10,
#         showarrow = FALSE
#       )


#ADD AXIS FORMATTING
fig102 <- fig102 %>% layout(title = "",
                            xaxis = xaxis102,
                            yaxis = yaxis102,
                            boxmode="group"
);

fig102

#WRITE OUT THE PICTURE
reticulate::py_run_string("import sys");
fileOut = paste(outputRoot,"MarketBeta_Diff1.png",sep="");
save_image(fig100, fileOut, width = 400, height = 500)

#WRITE OUT THE PICTURE
reticulate::py_run_string("import sys");
fileOut = paste(outputRoot,"MarketBeta_Diff2.png",sep="");
save_image(fig101, fileOut, width = 400, height = 500)

#WRITE OUT THE PICTURE
reticulate::py_run_string("import sys");
fileOut = paste(outputRoot,"MarketBeta_Diff3.png",sep="");
save_image(fig102, fileOut, width = 400, height = 500)#WRITE OUT THE PICTURE

#############################################################################
################## MARKET + OIL BETA TABLES #################################
#############################################################################

MarketAndOilTable = allRegData %>%
  filter(Model =="Oil and SP500") %>%
  mutate(Industry2 = ifelse(SubIndustry == "Passenger Airlines","Passenger Airlines",Industry)) %>%
  filter(!(Symbol %in% excludeSymbolList)) %>%
  filter(term == "OilAbnormalReturnAnnualized") %>%
  mutate(estimate  = ifelse(p.value < 0.05, estimate, NA)) %>%
  group_by(Industry2,term,Regression) %>%
  summarise(
    nTotal         = n(),
    nSignificant   = sum(ifelse(p.value < 00.05,1,0)),
    nSignificantPerc   = round( 100*sum(ifelse(p.value < 00.05,1,0)) / n(),1),
    MeanBeta       = round(mean(estimate,na.rm=T),1),
    MedianBeta     = round(median(estimate,na.rm=T),1),
    sdBeta         = round(sd(estimate,na.rm=T),2),
    
    meanRSQ        = round(mean(R2),2),
    medianRSQ      = round(median(R2),2)
  ) %>%
  ungroup() %>%
  filter(term != "(Intercept)") %>%
  select(-term);


#COMPARE MARKET BETA FOR DIFF METHODOLOGIES
Oil_MarketRegressionSummary_S1 = allRegData %>%
  filter(Model =="Oil and SP500") %>%
  filter(!(Symbol %in% excludeSymbolList)) %>%
  filter(term == "OilAbnormalReturnAnnualized") %>%
  filter(nMonths > 0 ) %>%
  dplyr::select(Symbol,Regression,OilBeta = estimate) %>%
  pivot_wider(id_cols=c("Symbol"),names_from = c("Regression"),values_from = "OilBeta");

methodCorrOil = cor(Oil_MarketRegressionSummary_S1[,2:4])

#############################################################################
################## MARKET + OIL BETA CHARTS #################################
#############################################################################


MarketAndOilBetaSummary_OilBetaValue = allRegData %>%
  filter(Model =="Oil and SP500") %>%
  mutate(Industry2 = ifelse(SubIndustry == "Passenger Airlines","Passenger Airlines",Industry)) %>%
  filter(!(Symbol %in% excludeSymbolList)) %>%
  filter(term == "OilAbnormalReturnAnnualized") %>%
  filter(Regression == "OLS")


#BOXPLOT OF BETA VALUE SPLIT BY INDUSTRY
xaxis <- list(title = list(text = "Oil Beta Estimate",
                           font = FONT3),
              showline = TRUE,
              showgrid = FALSE,
              zeroline = TRUE,
              zerolinecolor = 'rgb(204, 204, 204)',
              zerolinewidth = 2,
              showticklabels = TRUE,
              linecolor = 'rgb(204, 204, 204)',
              linewidth = 2,
              autotick = FALSE,
              tick0=0.0,
              dtick = 0.50,
              ticks = 'outside',
              tickcolor = 'rgb(204, 204, 204)',
              tickwidth = 2,
              #ticklen = 5,
              range=c(-0.5,0.75),
              tickfont = FONT1 
              
);



yaxis <- list(title = list(text = "",
                           font = FONT3),
              showline = FALSE,
              showticklabels = TRUE,
              linecolor = 'rgb(204, 204, 204)',
              ticks = 'outside',
              tickcolor = 'rgb(204, 204, 204)',
              tickwidth = 2,
              ticklen = 0,
              ticklabelstep=1,
              showgrid=FALSE,
              gridcolor = 'rgb(204, 204, 204)',
              gridwidth=2,
              tickfont = FONT3,
              font = FONT3,
              autorange = "reversed"
);

fig10 = plot_ly(
  data = MarketAndOilBetaSummary_OilBetaValue,
  y    = ~Industry2,
  x    = ~estimate,
  split = ~Regression,
  type = "box",
  #fillcolor = 'rgba(0,0,0,0)',
  line = list(color='rgba(0,0,0,1)'),
  marker = list(color = 'rgb(7,40,89)',
                outliercolor = 'rgba(0, 0, 0, 1.0)',
                fillcolor = 'rgb(255,255,255)',
                line =  list(outliercolor = 'rgba(0, 0, 0, 1.0)',
                             outlierwidth = 0)
  )
);

#FONT FOR LEGEND
SETUP_LEGEND10 = list(
  font = list(
    family = 'Arial',
    size = 14,
    color = 'rgb(82, 82, 82)'
  ),
  x=0.55,
  y=0.00,
  bordercolor = 'rgba(255, 255, 255,0)',
  borderwidth = 2,
  bgcolor='rgba(0,0,0,0)'
);

#STACK THEM 
fig10 = fig10 %>% layout (boxmode="group")

#ADD AXIS FORMATTING
fig10 <- fig10 %>% layout(title = "",
                          xaxis = xaxis,
                          yaxis = yaxis,
                          legend = SETUP_LEGEND10
);

fig10

#WRITE OUT THE PICTURE
reticulate::py_run_string("import sys");
fileOut = paste(outputRoot,"OilBeta_Beta.png",sep="");
save_image(fig10, fileOut, width = 500, height = 400)


#GET VALUES FOR TEXT
OilBetaSummaryTable = MarketAndOilBetaSummary_OilBetaValue %>%
                      mutate(Industry = ifelse(SubIndustry == "Passenger Airlines","Passenger Airlines",Industry)) %>%
                      group_by(Industry) %>%
                      summarise(MeanEstimate = mean(estimate),
                                MedianEstimate = median(estimate)
                            )

#############################################################################
################## INCREMENT IN R2 USING OIL TERM ###########################
#############################################################################

R2_Oil_Imp_S1 = allRegData %>%
                mutate(Industry = ifelse(SubIndustry == "Passenger Airlines","Passenger Airlines",Industry)) %>%
                filter(!(Symbol %in% excludeSymbolList)) %>%
                filter(term == "(Intercept)") %>%
                filter(Model %in% c("Oil and SP500","SP500 Only")) %>%
                dplyr::select(-term,-estimate,-std.error,-statistic,-p.value,-nMonths,-FirstDate,-LastDate) %>%
                pivot_wider(id_cols = c("Symbol","SymbolDesc","Regression", "Industry","SubIndustry"),
                            names_from = Model,values_from = R2) %>%
                mutate(R2_Oil_Improvement = `Oil and SP500` - `SP500 Only`) %>%
                mutate(R2_Oil_Imp_rel = R2_Oil_Improvement / `SP500 Only` ) %>%
                rename(R2_oil_Market = `Oil and SP500`, R2_Market = `SP500 Only`) %>%
                filter(Regression == "OLS")

R2_Oil_Imp_S2 = allRegData %>%
                mutate(Industry = ifelse(SubIndustry == "Passenger Airlines","Passenger Airlines",Industry)) %>%
                filter(!(Symbol %in% excludeSymbolList)) %>%
                filter(term == "OilAbnormalReturnAnnualized") %>%
                filter(Model =="Oil and SP500") %>%
                dplyr::select(Symbol,p.value,nMonths,Regression,OilBeta=estimate) %>%
                filter(Regression == "OLS")

R2_Oil_Imp_S3 = R2_Oil_Imp_S1 %>%
                left_join(R2_Oil_Imp_S2,by=c("Symbol","Regression")) %>%
                mutate(R2_Oil_Improvement = ifelse(p.value < 0.05 , R2_Oil_Improvement,NA)) %>%
                mutate(R2_Oil_Imp_rel = ifelse(p.value < 0.05 , R2_Oil_Imp_rel,NA)) %>%
                filter(Regression == "OLS")
  



xaxis <- list(title = list(text = "Partial r² of Oil Term",
                           font = FONT3),
              showline = TRUE,
              showgrid = FALSE,
              zeroline = TRUE,
              zerolinecolor = 'rgb(204, 204, 204)',
              zerolinewidth = 2,
              showticklabels = TRUE,
              linecolor = 'rgb(204, 204, 204)',
              linewidth = 2,
              autotick = FALSE,
              tick0=0.0,
              dtick = 0.1,
              ticks = 'outside',
              tickcolor = 'rgb(204, 204, 204)',
              tickwidth = 2,
              #ticklen = 5,
              range=c(-0.05,0.21),
              tickfont = FONT1 
              
);



yaxis <- list(title = list(text = "",
                           font = FONT3),
              showline = FALSE,
              showticklabels = TRUE,
              linecolor = 'rgb(204, 204, 204)',
              ticks = 'outside',
              tickcolor = 'rgb(204, 204, 204)',
              tickwidth = 2,
              ticklen = 0,
              ticklabelstep=1,
              showgrid=FALSE,
              gridcolor = 'rgb(204, 204, 204)',
              gridwidth=2,
              tickfont = FONT3,
              font = FONT3,
              autorange = "reversed"
);

fig11 = plot_ly(
  data = R2_Oil_Imp_S1,
  y    = ~Industry,
  x    = ~R2_Oil_Improvement,
  split = ~Regression,
  type = "box",
  #fillcolor = 'rgba(0,0,0,0)',
  line = list(color='rgba(0,0,0,1)'),
  marker = list(color = 'rgb(7,40,89)',
                outliercolor = 'rgba(0, 0, 0, 1.0)',
                fillcolor = 'rgb(255,255,255)',
                line =  list(outliercolor = 'rgba(0, 0, 0, 1.0)',
                             outlierwidth = 0)
  )
);

#STACK THEM 
fig11 = fig11 %>% layout (boxmode="group")

#ADD AXIS FORMATTING
fig11 <- fig11 %>% layout(title = "",
                          xaxis = xaxis,
                          yaxis = yaxis,
                          showlegend = FALSE
);

fig11

#WRITE OUT THE PICTURE
reticulate::py_run_string("import sys");
fileOut = paste(outputRoot,"OilBeta_P_RSQ.png",sep="");
save_image(fig11, fileOut, width = 500, height = 400)



#PUT OUT FOR TABLEAU
write.table(R2_Oil_Imp_S3,paste(outputRoot,"R2_imp_table_tablea.csv",sep=""),quote=F,row.names=F,sep=";")

#############################################################################
################## TABLE OF BIGGEST OIL BETA STOCKS  ########################
#############################################################################


Table2 = MarketAndOilBetaSummary_OilBetaValue %>%
         filter(nMonths >= 120) %>%
          filter(p.value < 0.05) %>%
         arrange(-abs(estimate)) %>%
         mutate(R1 = rank(-abs(estimate))) %>%
         select(R1,Symbol,SymbolDesc,SubIndustry,estimate,p.value,R2)  %>%
         filter(R1 <= 10) %>%
         left_join(R2_Oil_Imp_S1 %>% select(Symbol,R2_Oil_Improvement),by=c("Symbol"))
      

#############################################################################
################## CHANGE IN BETA DUE TO OIL TERM ###########################
#############################################################################

betaChangeExtent = allRegData %>%
                   filter(term == "SP500AbnormalReturnAnnualized") %>%
                   filter(Regression == "OLS") %>%
                   filter(Model %in% c("Oil and SP500","SP500 Only")) %>%
                   filter(p.value < 0.05 ) %>%
                   select(-statistic,-std.error) %>%
                   pivot_wider(values_from = c("estimate","p.value","R2"),names_from = "Model") %>%
                   mutate(ChangeBeta = `estimate_Oil and SP500` - `estimate_SP500 Only`) %>%
                   mutate(ChangeBetaAbs = abs(ChangeBeta)) %>%
                   #filter(nMonths == 407) %>%
                   filter(nMonths >= 120) %>%
                   arrange(desc(ChangeBetaAbs)) %>%
                   mutate(Rank = frank(desc(ChangeBetaAbs)));


betaChangeTableOut = betaChangeExtent %>%
                     filter(Rank <= 10) %>%
                     select(Rank,Symbol,SymbolDesc,SubIndustry,`Market Beta` = `estimate_SP500 Only`,`Market Beta inc. Oil Model` = `estimate_Oil and SP500`,ChangeBeta) %>%
                     mutate(`Market Beta` = round(`Market Beta`,2)) %>%
                     mutate(`Market Beta inc. Oil Model` = round(`Market Beta inc. Oil Model`,2)) %>%
                     mutate(ChangeBeta = round(ChangeBeta,2)) %>%
                     mutate(ChangeBeta = as.character(ChangeBeta)) %>%
                     mutate(ChangeBeta = ifelse(ChangeBeta > 0 ,paste("+",ChangeBeta,sep=""),ChangeBeta));

#COPY TABLE TO CLIPBOARD
write.table(betaChangeTableOut, "clipboard-16384",quote=F,row.names=F,sep=";")

#WRITE TABLE TO A CSV
fileOut = paste(outputRoot,"MarketBeta_changes_companies.txt",sep="");
write.table(betaChangeTableOut, fileOut,quote=F,row.names=F,sep=";")


#############################################################################
################## INDUSTRY PORTOFLIO VALUES FOR TEXT #######################
#############################################################################

d1 = allRegData %>%
     filter(Symbol %in% c("Energy"))




#############################################################################
################## OIL COMPANIES WITH LOW CORRELATION TO OIL ################
#############################################################################


AllEnergyOilBetas = allRegData %>%
                    filter(Industry %in% c ("Energy")) %>%
                    filter(term %in% c("OilAbnormalReturnAnnualized")) %>%
                    filter(Model %in% c("Oil and SP500")) %>%
                    group_by(Regression) %>%
                    arrange(Regression,-estimate) %>%
                    mutate(R1 = rank(desc(estimate))) %>%
                    mutate(medianValue = median(estimate))

TopOilBetas2 = allRegData %>%
                      filter(term %in% c("OilAbnormalReturnAnnualized")) %>%
                      filter(Model %in% c("Oil and SP500")) %>%
                      filter(nMonths > 120) %>%
                      filter(Regression == "OLS") %>%
                      group_by(Regression) %>%
                      arrange(Regression,-estimate) %>%
                      mutate(R1 = rank(desc(estimate))) %>%
                      mutate(medianValue = median(estimate)) %>%
                      filter(R1 <= 10 )
