require(tidyverse)
require(robustbase)
require(quantreg)
require(foreach)
require(doParallel)
require(dplyr)
require(broom)
require(plotly)
require(lubridate)


#############################################################################
################## OUTPUT SETUP #############################################
#############################################################################

outputRoot = "C:/Users/giles/Creative Cloud Files/MBA/Dissertation/Writeup/Pictures/"


#############################################################################
################## LOAD DATA ################################################
#############################################################################

#LOAD THE DATAFILE IN
load("C:/Users/giles/Creative Cloud Files/MBA/Dissertation/Data/RollingBeta.Rdata");

###############################################################
#########   GLOBAL FONT LIST  #################################
###############################################################

FONT1 = list(family = 'Arial',
             size = 16,
             color = 'rgb(82, 82, 82)');

FONT2 = list(family = 'Arial',
             size = 16,
             color = 'rgb(82, 82, 82)');

###############################################################
#########   CREATE A CHART OF OLS ROLLING BETA DAT ############
###############################################################

ChartData0_Energy = collectRegressionData %>%
                     filter(Regression == "OLS") %>%
                     filter(Symbol == "Energy") %>%
                     filter(term == "OilRet") %>%
                     mutate(start = ymd(paste(start,"01",sep=""))) %>%
                     mutate(end   = ymd(paste(end,"01",sep="")));

ChartData0_Airlines = collectRegressionData %>%
                      filter(Regression == "OLS") %>%
                      filter(Symbol == "Passenger Airlines") %>%
                      filter(term == "OilRet") %>%
                      mutate(start = ymd(paste(start,"01",sep=""))) %>%
                      mutate(end   = ymd(paste(end,"01",sep="")));

ChartData0_Energy_ROB = collectRegressionData %>%
                        filter(Regression == "ROB") %>%
                        filter(Symbol == "Energy") %>%
                        filter(term == "OilRet") %>%
                        mutate(start = ymd(paste(start,"01",sep=""))) %>%
                        mutate(end   = ymd(paste(end,"01",sep="")));

ChartData0_Airlines_ROB = collectRegressionData %>%
                          filter(Regression == "ROB") %>%
                          filter(Symbol == "Passenger Airlines") %>%
                          filter(term == "OilRet") %>%
                          mutate(start = ymd(paste(start,"01",sep=""))) %>%
                          mutate(end   = ymd(paste(end,"01",sep="")));

ChartData0_Energy_QR = collectRegressionData %>%
  filter(Regression == "QR") %>%
  filter(Symbol == "Energy") %>%
  filter(term == "OilRet") %>%
  mutate(start = ymd(paste(start,"01",sep=""))) %>%
  mutate(end   = ymd(paste(end,"01",sep="")));

ChartData0_Airlines_QR = collectRegressionData %>%
  filter(Regression == "QR") %>%
  filter(Symbol == "Passenger Airlines") %>%
  filter(term == "OilRet") %>%
  mutate(start = ymd(paste(start,"01",sep=""))) %>%
  mutate(end   = ymd(paste(end,"01",sep="")));

#CREATE THE BASIC CHARTS
fig0A <- plot_ly(data=ChartData0_Energy,
                 x = ~start, 
                 y = ~estimate, 
                 linetype = ~Model,
                 type = 'scatter',
                 mode = 'lines',
                 name = "OLS",
                 line = list(color = 'rgb(0, 0, 0)', 
                             width = 2,
                             dash="solid")
                 #marker= list(color = 'rgb(0, 0, 0)', size = 10)
) %>% add_trace( 
        data = ChartData0_Energy_ROB,
        x = ~start, 
        y = ~estimate, 
        type = "scatter",
        mode = "lines",
        name = "MM Robust Reg.",
        line = list(color = 'rgb(0, 0, 0)', 
                    width = 2,
                    dash="dash")
      ) %>% add_trace( 
        data = ChartData0_Energy_QR,
        x = ~start, 
        y = ~estimate, 
        type = "scatter",
        mode = "lines",
        name = "Quantile Reg.",
        line = list(color = 'rgb(0, 0, 0)', 
                    width = 2,
                    dash="dot")
      )

fig0B <- plot_ly(data=ChartData0_Airlines,
                 x = ~start, 
                 y = ~estimate, 
                 linetype = ~Model,
                 type = 'scatter',
                 mode = 'lines',
                 name = "OLS",
                 line = list(color = 'rgb(0, 0, 0)', 
                             width = 2,
                             dash="solid")
                 #marker= list(color = 'rgb(0, 0, 0)', size = 10)
) %>% add_trace( 
  data = ChartData0_Airlines_ROB,
  x = ~start, 
  y = ~estimate, 
  type = "scatter",
  mode = "lines",
  name = "MM Robust Reg.",
  line = list(color = 'rgb(0, 0, 0)', 
              width = 2,
              dash="dash")
) %>% add_trace( 
  data = ChartData0_Airlines_QR,
  x = ~start, 
  y = ~estimate, 
  type = "scatter",
  mode = "lines",
  name = "Quantile Reg.",
  line = list(color = 'rgb(0, 0, 0)', 
              width = 2,
              dash="dot")
)

xaxis0A <- list(title = "Regression Start Date",
                showline = FALSE,
                showgrid = FALSE,
                showticklabels = TRUE,
                linecolor = 'rgb(204, 204, 204)',
                linewidth = 2,
                autotick = FALSE,
                #tick0=1990,
                dtick = "M48",
                ticks = 'outside',
                tickcolor = 'rgb(204, 204, 204)',
                tickwidth = 2,
                #ticklen = 5,
                #range=c(,2024.1),
                tickfont = FONT1 
                
);

yaxis0A <- list(title = list(text = "Energy Industry\nOil Beta Estimate (2Y)",
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
                dtick = 0.2,
                ticklabelstep=1,
                showgrid=TRUE,
                gridcolor = 'rgb(204, 204, 204)',
                gridwidth=2,
                zeroline=FALSE,
                ticksuffix = " ",
                tickprefix = "   ",
                #showtickprefix = TRUE,
                range=c(-0.1,1.1),
                tickfont = FONT1,
                tickformat="2",
                font = FONT2
);


yaxis0B <- list(title = list(text = "Passenger Airlines\nOil Beta Estimate (2Y)",
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
                tick0=-1.2,
                dtick = 0.4,
                ticklabelstep=1,
                showgrid=TRUE,
                gridcolor = 'rgb(204, 204, 204)',
                gridwidth=2,
                zeroline=FALSE,
                ticksuffix = " ",
                tickprefix = "   ",
                #showtickprefix = TRUE,
                range=c(-1.3,0.98),
                tickfont = FONT1,
                tickformat="2",
                font = FONT2
);
xaxis0B=xaxis0A;


SETUP_LEGEND_A = list(
  font = list(
    family = 'Arial',
    size = 14,
    color = 'rgb(82, 82, 82)'
  ),
  x=0.10,
  y=0.99,
  bordercolor = 'rgb(255, 255, 255)',
  borderwidth = 2
);

SETUP_LEGEND_B= list(
  font = list(
    family = 'Arial',
    size = 14,
    color = 'rgb(82, 82, 82)'
  ),
  x=0.85,
  y=0.05,
  bordercolor = 'rgb(255, 255, 255)',
  borderwidth = 2
);


fig0A = fig0A %>%
  layout(
    xaxis  = xaxis0A,
    yaxis  = yaxis0A,
    legend = SETUP_LEGEND_A
  );

fig0B = fig0B %>%
  layout(
    xaxis  = xaxis0B,
    yaxis  = yaxis0B,
    legend = SETUP_LEGEND_B
  );


fig0A
fig0B

#WRITE OUT THE PICTURE
reticulate::py_run_string("import sys");
fileOut = paste(outputRoot,"Beta_Roling_2Y_Energy.png",sep="");
save_image(fig0A, fileOut, width = 1200, height = 400)

#WRITE OUT THE PICTURE
reticulate::py_run_string("import sys");
fileOut = paste(outputRoot,"Beta_Roling_2Y_Air.png",sep="");
save_image(fig0B, fileOut, width = 1200, height = 400)

##############################################################################################################################
#########   CREATE A CHART OF OLS ROLLING BETA DATA, SHOWING R2 FOR MARKET BETA, MARKET AND OIL BETA AND THE DIFFERENCE ######
##############################################################################################################################

#LIMIT TO ENERGY COMPANIES AND SUMMARIZE
ChartData1 = collectRegressionData %>%
             filter(Regression == "OLS") %>%
             select(Symbol,SymbolDesc,start,end,Model,R2,R2_Oil_Imp) %>%
             distinct();

ChartData2 = ChartData1 %>% select(-R2_Oil_Imp) %>%
             rbind(
                    ChartData1 %>%
                    select(-R2) %>%
                    rename(R2= R2_Oil_Imp) %>%
                    mutate(Model="R2 Oil Imp") %>%
                    distinct()
                   ) %>%
              arrange(Symbol,start,Model) %>%
              group_by(start,end,Model) %>%
              summarise(R2= mean(R2)) %>%
              ungroup() %>%
              filter(!is.na(start)) %>%
              filter(!is.na(end)) %>%
              mutate(start = ymd(paste(start,"01",sep=""))) %>%
              mutate(end   = ymd(paste(end,"01",sep="")));


#CREATE THE BASIC CHARTS
fig1a <- plot_ly(data=(ChartData2 %>% filter(Model == "SP500 Only")),
               x = ~start, 
               y = ~R2, 
               linetype = ~Model,
               type = 'scatter',
               mode = 'lines',
               line = list(color = 'rgb(0, 0, 0)', 
                           width = 2,
                           dash="solid")
               #marker= list(color = 'rgb(0, 0, 0)', size = 10)
) %>%
add_trace(data=(ChartData2 %>% filter(Model =="Oil and SP500")),
                 x = ~start, 
                 y = ~R2, 
                 type = 'scatter',
                 mode = 'lines',
                 line = list(color = 'rgb(0, 0, 0)', 
                             width = 2,
                             dash = "dot",
                             dot =1)
                 #marker= list(color = 'rgb(0, 0, 0)', size = 10)
) 


fig1c = plot_ly(data=(ChartData2 %>% filter(Model == "R2 Oil Imp")),
                x = ~start, 
                y = ~R2, 
                linetype = ~Model,
                type = 'scatter',
                mode = 'lines',
                line = list(color = 'rgb(0, 0, 0)', 
                            width = 2,
                            dash="solid")
                #marker= list(color = 'rgb(0, 0, 0)', size = 10)
)

#FONTS FOR USE
FONT1 = list(family = 'Arial',
             size = 18,
             color = 'rgb(82, 82, 82)');

#FONTS FOR USE
FONT2 = list(family = 'Arial',
             size = 14,
             color = 'rgb(82, 82, 82)');

xaxis1a <- list(title = "",
               showline = FALSE,
               showgrid = FALSE,
               showticklabels = TRUE,
               linecolor = 'rgb(204, 204, 204)',
               linewidth = 2,
               autotick = FALSE,
               #tick0=1990,
               dtick = "M48",
               ticks = 'outside',
               tickcolor = 'rgb(204, 204, 204)',
               tickwidth = 2,
               #ticklen = 5,
               #range=c(,2024.1),
               tickfont = FONT1 
               
);

xaxis1c <- list(title = "Regression Start Date",
                showline = FALSE,
                showgrid = FALSE,
                showticklabels = TRUE,
                linecolor = 'rgb(204, 204, 204)',
                linewidth = 2,
                autotick = FALSE,
                #tick0=1990,
                dtick = "M48",
                ticks = 'outside',
                tickcolor = 'rgb(204, 204, 204)',
                tickwidth = 2,
                #ticklen = 5,
                #range=c(,2024.1),
                tickfont = FONT1 
                
);



#SETUP AXES
yaxis1A <- list(title = list(text = "r2 of CAPM",
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
                dtick = 0.2,
                ticklabelstep=1,
                showgrid=TRUE,
                gridcolor = 'rgb(204, 204, 204)',
                gridwidth=2,
                zeroline=FALSE,
                ticksuffix = " ",
                tickprefix = "   ",
                #showtickprefix = TRUE,
                range=c(-0.01,0.72),
                tickfont = FONT1,
                tickformat="2",
                font = FONT2
);

yaxis1C <- list(title = list(text = "Partial r2 of oil term",
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
               dtick = 0.1,
               ticklabelstep=1,
               showgrid=TRUE,
               gridcolor = 'rgb(204, 204, 204)',
               gridwidth=2,
               zeroline=FALSE,
               ticksuffix = " ",
               tickprefix = "   ",
               #showtickprefix = TRUE,
               range=c(-0.01,0.42),
               tickfont = FONT1,
               tickformat="2",
               font = FONT2
);

SETUP_LEGEND1 = list(
  font = list(
    family = 'Arial',
    size = 18,
    color = 'rgb(82, 82, 82)'
  ),
  x=0.02,
  y=0.86,
  bordercolor = 'rgba(255, 255, 255,0)',
  borderwidth = 2,
  bgcolor='rgba(0,0,0,0)'
);

#ADD AXES
fig1a = fig1a %>%
  layout(
    xaxis  = xaxis1a,
    yaxis  = yaxis1A,
    legend = SETUP_LEGEND1
  );

fig1c = fig1c %>%
        layout(
          xaxis  = xaxis1c,
          yaxis = yaxis1C
        );

fig1a;
fig1c;



###################################################################################
################# OUTPUT FILES   ##################################################
###################################################################################

outputRoot = "C:/Users/giles/Creative Cloud Files/MBA/Dissertation/Writeup/Pictures/"

#WRITE OUT THE PICTURE
reticulate::py_run_string("import sys");
fileOut = paste(outputRoot,"R2_Rolling_Energy.png",sep="");
save_image(fig1a, fileOut, width = 1000, height = 300);

#WRITE OUT THE PICTURE
reticulate::py_run_string("import sys");
fileOut = paste(outputRoot,"R2_Imp_Rolling_Energy.png",sep="");
save_image(fig1c, fileOut, width = 1000, height = 300);



###################################################################################
######## HOW MANY P-VALUES WERE UNDER THRESHOLD AT EACH STAGE   ###################
###################################################################################


pValueAnalysis1 = collectRegressionData %>%
                  filter(Regression == "OLS") %>%
                  filter(Model == "Oil and SP500") %>%
                  filter(term == "OilRet") %>%
                  select(SymbolGroup,start,end,estimate,p.value) %>%
                  mutate(pValueSig = ifelse(p.value < 0.05,1,0)) %>%
                  group_by(SymbolGroup,start,end) %>%
                  summarise(
                             nTotal = n(),
                             nSig   = sum(pValueSig),
                             pMean  = mean(p.value),
                             pMed   = median(p.value),
                             oilBetaMean = mean(estimate),
                             oilBetaMed  = median(estimate)
                            ) %>%
                  ungroup() %>%
                  mutate(percSig = 100 * nSig / nTotal) %>%
                  mutate(start = ymd(paste(start,"01",sep="")))

write.table(pValueAnalysis1,"clipboard-16384",sep=";",quote=FALSE,row.names=F)


###################################################################################
######## HOW MANY P-VALUES WERE UNDER THRESHOLD AT EACH STAGE   ###################
###################################################################################


#CREATE THE BASIC CHARTS
fig2a <- plot_ly(data=(pValueAnalysis1 %>% filter(SymbolGroup == "Energy")),
                 x = ~start, 
                 y = ~percSig, 
                 type = 'scatter',
                 mode = 'lines',
                 line = list(color = 'rgb(0, 0, 0)', 
                             width = 2,
                             dash="solid")
                 #marker= list(color = 'rgb(0, 0, 0)', size = 10)
) 


#FONTS FOR USE
FONT1 = list(family = 'Arial',
             size = 18,
             color = 'rgb(82, 82, 82)');

#FONTS FOR USE
FONT2 = list(family = 'Arial',
             size = 14,
             color = 'rgb(82, 82, 82)');

xaxis2a <- list(title = "Regression Start Date",
                showline = FALSE,
                showgrid = FALSE,
                showticklabels = TRUE,
                linecolor = 'rgb(204, 204, 204)',
                linewidth = 2,
                autotick = FALSE,
                #tick0=1990,
                dtick = "M48",
                ticks = 'outside',
                tickcolor = 'rgb(204, 204, 204)',
                tickwidth = 2,
                #ticklen = 5,
                #range=c(,2024.1),
                tickfont = FONT1 
                
);


#SETUP AXES
yaxis2A <- list(title = list(text = "% of companies with significant oil beta",
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
                ticksuffix = " ",
                tickprefix = "   ",
                #showtickprefix = TRUE,
                range=c(-1,105),
                tickfont = FONT1,
                tickformat="2",
                font = FONT2
);

#ADD AXES SETTINGS
fig2a = fig2a %>%
  layout(
    xaxis  = xaxis2a,
    yaxis  = yaxis2A
  );


fig2a

#WRITE OUT THE PICTURE
reticulate::py_run_string("import sys");
fileOut = paste(outputRoot,"propP_Rolling_Energy.png",sep="");
save_image(fig2a, fileOut, width = 1000, height = 300);


###################################################################################
######## HOW MANY P-VALUES WERE UNDER THRESHOLD AT EACH STAGE   ###################
###################################################################################
