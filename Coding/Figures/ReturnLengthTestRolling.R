require(tidyverse)
require(plotly)

#LOAD THE DATA IN 
load("C:/Users/giles/OneDrive/MBA/Dissertation/Data/ValidationLarger.Rdata")

#BASIC FILTERS ON DATE
AllBetaOut = AllBetaOut %>%
             filter(Date >= ymd("19900101"))

#############################################################################
################## OUTPUT SETUP #############################################
#############################################################################

outputRoot = "C:/Users/giles/OneDrive/MBA/Dissertation/Writeup/Pictures/"

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

#THE BASIC CHART
#CREATE THE BASIC CHARTS
fig0A <- plot_ly(data = AllBetaOut %>% 
                   filter(Disc =="Weekly") %>% 
                   filter(term =="OilAbnormalReturnAnnualized") %>%
                   filter(Regression =="OLS"),
                 x = ~TimeStart, 
                 y = ~estimate, 
                # linetype = ~Model,
                 type = 'scatter',
                 mode = 'lines',
                 name = "Weekly",
                 line = list(color = 'rgb(0, 0, 0)', 
                             width = 2,
                             dash="solid")
                 #marker= list(color = 'rgb(0, 0, 0)', size = 10)
) %>% add_trace( 
  data  = AllBetaOut %>% 
    filter(Disc=="BiWeekly") %>% 
    filter(term=="OilAbnormalReturnAnnualized") %>%
    filter(Regression =="OLS"),
  x = ~TimeStart, 
  y = ~estimate, 
  type = "scatter",
  mode = "lines",
  name = "Bi-Weekly",
  line = list(color = 'rgb(0, 0, 0)', 
              width = 2,
              dash="dashdot")
) %>% add_trace( 
  data = AllBetaOut %>% 
    filter(Disc=="Monthly") %>% 
    filter(term=="OilAbnormalReturnAnnualized") %>%
    filter(Regression =="OLS"),
  x = ~TimeStart, 
  y = ~estimate, 
  type = "scatter",
  mode = "lines",
  name = "Monthly",
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

yaxis0A <- list(title = list(text = "Energy Industry\nOil-Beta Estimate (OLS 2Y)",
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
                range=c(-0.29,1.09),
                tickfont = FONT1,
                tickformat="2",
                font = FONT2
);

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

fig0A = fig0A %>%
  layout(
    xaxis  = xaxis0A,
    yaxis  = yaxis0A,
    legend = SETUP_LEGEND_A
  );

fig0A

###############################################################
#########   CREATE A CHART OF MM ROBUST  ROLLING BETA DAT #####
###############################################################

#THE BASIC CHART
#CREATE THE BASIC CHARTS
fig0B <- plot_ly(data = AllBetaOut %>% 
                   filter(Disc =="Weekly") %>% 
                   filter(term =="OilAbnormalReturnAnnualized") %>%
                   filter(Regression =="ROB"),
                 x = ~TimeStart, 
                 y = ~estimate, 
                 # linetype = ~Model,
                 type = 'scatter',
                 mode = 'lines',
                 name = "Weekly",
                 line = list(color = 'rgb(0, 0, 0)', 
                             width = 2,
                             dash="solid")
                 #marker= list(color = 'rgb(0, 0, 0)', size = 10)
) %>% add_trace( 
  data  = AllBetaOut %>% 
    filter(Disc=="BiWeekly") %>% 
    filter(term=="OilAbnormalReturnAnnualized") %>%
    filter(Regression =="ROB"),
  x = ~TimeStart, 
  y = ~estimate, 
  type = "scatter",
  mode = "lines",
  name = "Bi-Weekly",
  line = list(color = 'rgb(0, 0, 0)', 
              width = 2,
              dash="dashdot")
) %>% add_trace( 
  data = AllBetaOut %>% 
    filter(Disc=="Monthly") %>% 
    filter(term=="OilAbnormalReturnAnnualized") %>%
    filter(Regression =="ROB"),
  x = ~TimeStart, 
  y = ~estimate, 
  type = "scatter",
  mode = "lines",
  name = "Monthly",
  line = list(color = 'rgb(0, 0, 0)', 
              width = 2,
              dash="dot")
)

xaxis0B <- list(title = "Regression Start Date",
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

yaxis0B <- list(title = list(text = "Energy Industry\nOil-Beta Estimate (MM-ROB 2Y)",
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
                range=c(-0.29,1.09),
                tickfont = FONT1,
                tickformat="2",
                font = FONT2
);

SETUP_LEGEND_B = list(
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
fileOut = paste(outputRoot,"ReturnTimeframeOLS.png",sep="");
save_image(fig0A, fileOut, width = 1000, height = 300);

#WRITE OUT THE PICTURE
reticulate::py_run_string("import sys");
fileOut = paste(outputRoot,"ReturnTimeframeRob.png",sep="");
save_image(fig0B, fileOut, width = 1000, height = 300);

