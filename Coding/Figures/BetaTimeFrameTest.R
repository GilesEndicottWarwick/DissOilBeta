require(tidyverse)
require(plotly)


#FILE CREATED BY ROLLINGBTEAPARTODAYTIMEFRAME.R
load("C:/Users/giles/OneDrive/MBA/Dissertation/Data/RollingBetaTimeframe.Rdata")

#WHERE TO PUT THE IMAGES
outputRoot = "C:/Users/giles/OneDrive/MBA/Dissertation/Writeup/Pictures/"

#FIGURE 1 WILL BE MEDIAN / AVERAGE OIL BETA 
PlotFigure1 = collectRegressionData %>%
              filter(term == "OilRet") %>%
              filter(Symbol != "Energy") %>%
              select(start,end,Symbol,SymbolGroup,estimate,Regression) %>%
              group_by(start,end,SymbolGroup,Regression) %>%
              summarise(
                  medianEstimate = median(estimate),
                  meanEstimate   = mean(estimate),
                  .groups="drop"
              ) %>%
              ungroup() %>%
              mutate(StartProc = ymd(paste(start,"01",sep=""))) %>%
              mutate(EndProc   = ymd(paste(end,"01",sep=""))) %>%
              mutate(MonthsBetween = interval(StartProc,EndProc) %/% months(1)) %>%
              select(-start,-end,-StartProc,-EndProc) %>%
              mutate(YearsBetween = MonthsBetween /12)



FONT1 = list(family = 'Arial',
             size = 18,
             color = 'rgb(82, 82, 82)');

FONT2 = list(family = 'Arial',
             size = 14,
             color = 'rgb(82, 82, 82)');


###################################################################################
################# OIL BETA AVERAGE LENGTH TEST  ###################################
###################################################################################

xaxis <- list(title = "Number of Included Years (baseline Nov 2023)",
              showline = TRUE,
              showgrid = FALSE,
              showticklabels = TRUE,
              linecolor = 'rgb(204, 204, 204)',
              linewidth = 2,
              autotick = FALSE,
              tick0=0,
              dtick = 5,
              ticks = 'outside',
              tickcolor = 'rgb(204, 204, 204)',
              tickwidth = 2,
              #ticklen = 5,
              range=c(0,34),
              tickfont = FONT1 
              
);



yaxis <- list(title = list(text = "Energy Stocks \nMean Oil Beta Estimate",
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
              #ticksuffix = "  ",
              tickprefix = "  ",
              range=c(0.1,0.85),
              tickfont = FONT1,
              font = FONT1,
              side="left"
);



fig <- plot_ly(PlotFigure1 %>% filter(Regression =="OLS" & SymbolGroup == "Energy"),
               x = ~YearsBetween, 
               y = ~meanEstimate, 
               name = 'OLS Reg.',
               type = 'scatter',
               mode = 'lines',
               line = list(color = 'rgb(0, 0, 0)', width = 3,dash="solid")
               #marker= list(color = 'rgb(0, 0, 0)', size = 10)
) %>%
  add_trace(
    data = PlotFigure1 %>% filter(Regression =="ROB" & SymbolGroup == "Energy"),
    x = ~YearsBetween, 
    y = ~meanEstimate, 
    name = 'MM Rob. Reg.',
    type = 'scatter',
    mode = 'lines',
    line = list(color = 'rgb(0, 0, 0)', 
                width = 3,
                dash = "dash")
  ) %>%
  add_trace(
    data = PlotFigure1 %>% filter(Regression =="QR" & SymbolGroup == "Energy"),
    x = ~YearsBetween, 
    y = ~meanEstimate, 
    name = 'Quantile Reg.',
    type = 'scatter',
    mode = 'lines',
    line = list(color = 'rgb(0, 0, 0)', 
                width = 3,
                dash = "dot")
  )

#FONT FOR LEGEND
SETUP_LEGEND = list(
  font = list(
    family = 'Arial',
    size = 14,
    color = 'rgb(82, 82, 82)'
  ),
  x=0.82,
  y=0.91,
  bordercolor = 'rgb(255, 255, 255)',
  borderwidth = 2
);

#APPLY AXIS SETTINGS
fig <- fig %>% layout(title = "",
                      xaxis = xaxis,
                      yaxis = yaxis,
                      showlegend = TRUE,
                      legend = SETUP_LEGEND
);


print(fig)

#WRITE OUT THE PICTURE
reticulate::py_run_string("import sys");
fileOut = paste(outputRoot,"nMonths_OilBetaTest_Energy.png",sep="");
save_image(fig, fileOut, width = 1000, height = 300)

###################################################################################
################# SAME BUT AIRLINES  ##############################################
###################################################################################


xaxis2 <- list(title = "Number of Included Years (baseline Nov 2023)",
              showline = TRUE,
              showgrid = FALSE,
              showticklabels = TRUE,
              linecolor = 'rgb(204, 204, 204)',
              linewidth = 2,
              autotick = FALSE,
              tick0=0,
              dtick = 5,
              ticks = 'outside',
              tickcolor = 'rgb(204, 204, 204)',
              tickwidth = 2,
              #ticklen = 5,
              range=c(0,34),
              tickfont = FONT1 
              
);



yaxis2 <- list(title = list(text = "Airline Stocks \nMean Oil Beta Estimate",
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
              #ticksuffix = "  ",
              tickprefix = "  ",
              range=c(-0.5,0.5),
              tickfont = FONT1,
              font = FONT1,
              side="left"
);



fig2 <- plot_ly(PlotFigure1 %>% filter(Regression =="OLS" & SymbolGroup == "Passenger Airlines"),
               x = ~YearsBetween, 
               y = ~meanEstimate, 
               name = 'OLS Reg.',
               type = 'scatter',
               mode = 'lines',
               line = list(color = 'rgb(0, 0, 0)', width = 3,dash="solid")
               #marker= list(color = 'rgb(0, 0, 0)', size = 10)
) %>%
  add_trace(
    data = PlotFigure1 %>% filter(Regression =="ROB" & SymbolGroup == "Passenger Airlines"),
    x = ~YearsBetween, 
    y = ~meanEstimate, 
    name = 'MM Rob. Reg.',
    type = 'scatter',
    mode = 'lines',
    line = list(color = 'rgb(0, 0, 0)', 
                width = 3,
                dash = "dash")
  ) %>%
  add_trace(
    data = PlotFigure1 %>% filter(Regression =="QR" & SymbolGroup == "Passenger Airlines"),
    x = ~YearsBetween, 
    y = ~meanEstimate, 
    name = 'Quantile Reg.',
    type = 'scatter',
    mode = 'lines',
    line = list(color = 'rgb(0, 0, 0)', 
                width = 3,
                dash = "dot")
  )

#FONT FOR LEGEND
SETUP_LEGEND = list(
  font = list(
    family = 'Arial',
    size = 14,
    color = 'rgb(82, 82, 82)'
  ),
  x=0.82,
  y=0.91,
  bordercolor = 'rgb(255, 255, 255)',
  borderwidth = 2
);

#APPLY AXIS SETTINGS
fig2 <- fig2 %>% layout(title = "",
                      xaxis = xaxis2,
                      yaxis = yaxis2,
                      showlegend = TRUE,
                      legend = SETUP_LEGEND
);


print(fig2)

#WRITE OUT THE PICTURE
reticulate::py_run_string("import sys");
fileOut = paste(outputRoot,"nMonths_OilBetaTest_Airlines.png",sep="");
save_image(fig2, fileOut, width = 1000, height = 300)