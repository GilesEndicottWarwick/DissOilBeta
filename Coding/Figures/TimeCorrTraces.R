require(tidyverse)
require(robustbase)
require(quantreg)
require(foreach)
require(doParallel)
require(dplyr)
require(broom)

#LOAD THE INPUT FILE, IT COMES FROM RollingBetaPar.R
load("C:/Users/giles/OneDrive/MBA/Dissertation/Data/RollingBeta.Rdata")

#############################################################################
################## OUTPUT SETUP #############################################
#############################################################################

outputRoot = "C:/Users/giles/OneDrive/MBA/Dissertation/Writeup/Pictures/"


#############################################################################
################## CONVERT DATES ############################################
#############################################################################

collectRegressionData = collectRegressionData %>%
                        filter(!is.na(start)) %>%
                        mutate(start = ymd(paste(start,"01",sep="")));


#############################################################################
################## CUSTOM X LABELS ##########################################
#############################################################################

#for tickvals and ticktext
customXLabs  = c(ymd("19920101"),ymd("19960101"),ymd("20000101"),ymd("20040101"),ymd("20080101"),ymd("20120101"),ymd("20160101"),ymd("20200101"));
customXTexts = c("1992\n-\n1994","1996\n-\n1998","2000\n-\n2002","2004\n-\n2006","2008\n-\n2010","2012\n-\n2014","2016\n-\n2018","2020\n-\n2022");

###############################################################
#########   GLOBAL FONT LIST  #################################
###############################################################

FONT1 = list(family = 'Arial',
             size = 16,
             color = 'rgb(82, 82, 82)');

FONT2 = list(family = 'Arial',
             size = 16,
             color = 'rgb(82, 82, 82)');

#############################################################################
############### ENERGY COMPANIES PLOT #######################################
#############################################################################

#PLOT TWO COMPONENTS
componentsPlotEnergy = c("DVN","APA","MRO","HES","EOG")

thisRegressionDataEnergy = collectRegressionData %>%
                     filter(Symbol %in% componentsPlotEnergy ) %>%
                     filter(term == "OilRet") %>%
                     filter( Model == "Oil and SP500") %>%
                     filter(Regression == "OLS");


#PLOT THE CHART FOR ENERGY FIRMS
f1 = plot_ly() 

#ADD ALL TRACES
for (i in 1:length(componentsPlotEnergy))
{
  f1 = f1 %>% add_trace(
                        data = thisRegressionDataEnergy %>% filter(Symbol == componentsPlotEnergy[i]),
                        name = componentsPlotEnergy[i],
                        x = ~start,
                        y = ~estimate,
                        type = "scatter",
                        mode = "lines"
                      );
  
}


xaxis0A <- list(title = "Regression Period",
                showline = FALSE,
                showgrid = FALSE,
                showticklabels = TRUE,
                linecolor = 'rgb(204, 204, 204)',
                #linewidth = 2,
                #autotick = FALSE,
                #tick0=1990,
                #dtick = "M48",
                #ticks = 'outside',
                tickcolor = 'rgb(204, 204, 204)',
                tickwidth = 2,
                #ticklen = 5,
                #range=c(,2024.1),
                tickvals = customXLabs,
                ticktext = customXTexts,
                tickfont = FONT1 
                
);

yaxis0A <- list(title = list(text = "Oil Beta Estimate\n (Monthly 2Y)",
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
                dtick = 0.5,
                ticklabelstep=1,
                showgrid=TRUE,
                gridcolor = 'rgb(204, 204, 204)',
                gridwidth=2,
                zeroline=FALSE,
                ticksuffix = " ",
                tickprefix = "   ",
                #showtickprefix = TRUE,
                range=c(-0.99,1.99),
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
  x=100,
  y=0.85,
  bordercolor = 'rgba(100, 100, 100,1)',
  borderwidth = 0,
  bgcolor='rgba(255, 255, 255,1)'
);

f1 = f1 %>%
     layout(
       xaxis = xaxis0A,
       yaxis = yaxis0A,
       legend = SETUP_LEGEND1
     )

f1

#WRITE OUT THE PICTURE
reticulate::py_run_string("import sys");
fileOut = paste(outputRoot,"TraceOilBetaEnergy.png",sep="");
save_image(f1, fileOut, width = 1000, height = 400);

#############################################################################
############### AIRLINES COMPANIES PLOT #####################################
#############################################################################

#PLOT TWO COMPONENTS
componentsPlotAirlines = c("LUV","UAL","AAL","DAL","ALK")

thisRegressionDataAirlines = collectRegressionData %>%
  filter(Symbol %in% componentsPlotAirlines ) %>%
  filter(term == "OilRet") %>%
  filter( Model == "Oil and SP500") %>%
  filter(Regression == "OLS");


#PLOT THE CHART FOR ENERGY FIRMS
f2 = plot_ly() 

#ADD ALL TRACES
for (i in 1:length(componentsPlotAirlines))
{
  f2 = f2 %>% add_trace(
    data = thisRegressionDataAirlines %>% filter(Symbol == componentsPlotAirlines[i]),
    name = componentsPlotAirlines[i],
    x = ~start,
    y = ~estimate,
    type = "scatter",
    mode="lines"
  );
  
}


xaxis2A <- list(title = "Regression Period",
                showline = FALSE,
                showgrid = FALSE,
                showticklabels = TRUE,
                linecolor = 'rgb(204, 204, 204)',
                #linewidth = 2,
                #autotick = FALSE,
                #tick0=1990,
                #dtick = "M48",
                #ticks = 'outside',
                tickcolor = 'rgb(204, 204, 204)',
                tickwidth = 2,
                #ticklen = 5,
                range=c(ymd("20070101"),ymd("20211230")),
                tickvals = customXLabs,
                ticktext = customXTexts,
                tickfont = FONT1 
                
);

yaxis2A <- list(title = list(text = "Oil Beta Estimate\n (Monthly 2Y)",
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
                dtick = 0.5,
                ticklabelstep=1,
                showgrid=TRUE,
                gridcolor = 'rgb(204, 204, 204)',
                gridwidth=2,
                zeroline=FALSE,
                ticksuffix = " ",
                tickprefix = "   ",
                #showtickprefix = TRUE,
                range=c(-2.3,0.99),
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
  x=100,
  y=0.90,
  bordercolor = 'rgba(255, 255, 255,0)',
  borderwidth = 2,
  bgcolor='rgba(255, 255, 255,1)'
);

f2 = f2 %>%
  layout(
    xaxis = xaxis2A,
    yaxis = yaxis2A,
    legend = SETUP_LEGEND1
  )

f2

#WRITE OUT THE PICTURE
reticulate::py_run_string("import sys");
fileOut = paste(outputRoot,"TraceOilBetaAirlines.png",sep="");
save_image(f2, fileOut, width = 1000, height = 400);
