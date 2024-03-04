require(tidyverse)
require(plotly)


#LOAD DATA IN
load("C:/Users/giles/Creative Cloud Files/MBA/Dissertation/Data/5Y_Market_Beta_Calc_out.Rdata");

###################################################################################
################# MASTERR FONT SETTINGS ###########################################
###################################################################################

FONT1 = list(family = 'Arial',
             size = 34,
             color = 'rgb(82, 82, 82)');

FONT2 = list(family = 'Arial',
             size = 30,
             color = 'rgb(82, 82, 82)');


###################################################################################
################# CREATE SCATTER CHART ############################################
###################################################################################

yaxis1 <- list(title = list(text = "Calculated 5Y Monthly Beta",
                           font = FONT2),
              showline = TRUE,
              showticklabels = TRUE,
              linecolor = 'rgb(82, 82, 82)',
              linewidth = 2,
              autotick = FALSE,
              ticks = 'outside',
              tickcolor = 'rgb(82, 82, 82)',
              tickwidth = 2,
              ticklen = 20,
              tick0=0,
              dtick = 1,
              ticklabelstep=1,
              showgrid=TRUE,
              gridcolor = 'rgb(204, 204, 204)',
              gridwidth=2,
              zeroline=FALSE,
              ticksuffix = " ",
              tickprefix = "",
              range=c(-0,3.5),
              tickfont = FONT1,
              font = FONT1,
              side="left",
              tickformat="2"
);

xaxis1 <- list(title = list(text = "Yahoo Finance published 5Y Monthly Market Beta",
                           font = FONT2),
              showline = TRUE,
              showticklabels = TRUE,
              linecolor = 'rgb(82, 82, 82)',
              linewidth = 2,
              autotick = FALSE,
              ticks = 'outside',
              tickcolor = 'rgb(82, 82, 82)',
              tickwidth = 2,
              ticklen = 20,
              tick0=0,
              dtick = 1,
              ticklabelstep=1,
              showgrid=TRUE,
              gridcolor = 'rgb(204, 204, 204)',
              gridwidth=2,
              zeroline=FALSE,
              #ticksuffix = " ",
              #tickprefix = "  ",
              range=c(-0,3.5),
              tickfont = FONT1,
              font = FONT1,
              side="left",
              tickformat="2"
);

fig1 = plot_ly( data = CompareBeta,
                x = ~BetaCalc,
                y = ~YahooBeta,
                type="scatter",
                mode="markers",
                marker= list(color = 'rgba(120, 120, 120,255)', size = 7)
              );

fig1 = fig1 %>%
       layout(
         xaxis=xaxis1,
         yaxis=yaxis1
       )

###################################################################################
################# OUTPUT FILES   ##################################################
###################################################################################

outputRoot = "C:/Users/giles/Creative Cloud Files/MBA/Dissertation/Writeup/Pictures/"

#WRITE OUT THE PICTURE
reticulate::py_run_string("import sys");
fileOut = paste(outputRoot,"Validation_1.png",sep="");
save_image(fig1, fileOut, width = 1000, height = 1000)
