require(plotly)
require(robustbase)
require(quantreg)
require(tidyverse)
require(lubridate)

#LOAD THE DATA IN
load("C:/Users/giles/Creative Cloud Files/MBA/Dissertation/Data/MonthlyReturnsWithOil.Rdata");

#WHERE TO PUT THE IMAGES
outputRoot = "C:/Users/giles/Creative Cloud Files/MBA/Dissertation/Writeup/Pictures/"

#A TIME LIMIT
startDate = ymd("20200101")
endDate   = ymd("20211231")

#GRAB S&P 500 DATA
SP500 = ReturnsDataWithOil %>%
        filter(Symbol =="^GSPC");

#FONT FOR LABELS
FONT1 = list(family = 'Arial',
             size = 14,
             color = 'rgb(82, 82, 82)');

xaxis1 <- list(title = "Annualized Monthly S&P 500 Abnormal Return (%)",
               showline = TRUE,
               showgrid = FALSE,
               showticklabels = TRUE,
               linecolor = 'rgb(204, 204, 204)',
               linewidth = 2,
               autotick = FALSE,
               tick0=0,
               dtick = 1.5,
               ticks = 'outside',
               tickcolor = 'rgb(204, 204, 204)',
               tickwidth = 2,
               #ticklen = 5,
               range=c(-3,3),
               tickformat="+.0%",
               tickfont = FONT1 
               
);

yaxis1 <- list(title = "Proportion of Months (%)",
               showgrid=TRUE,
               gridcolor = 'rgb(204, 204, 204)',
               gridwidth=2,
               showline = FALSE,
               showticklabels = TRUE,
               linecolor = 'rgb(204, 204, 204)',
               linewidth = 2,
               autotick = FALSE,
               tick0=0,
               dtick = 2.5,
               ticks = 'outside',
               tickcolor = 'rgb(204, 204, 204)',
               tickwidth = 2,
               ticklen = 5,
               range=c(0,12),
               tickformat="2",
               tickfont = FONT1 
               
);

#THE BASIC CHART
fig1 = plot_ly(
              data = SP500,
              x = ~AbnormalReturnAnnualized,
              type = "histogram",
              histfunc='sum',
              histnorm = "percent",
              colors = ~Symbol,
              marker= list(color = 'rgb(120, 120, 120)', size = 10),
              nbinsx=50
              );

fig1  = fig1 %>%
        layout(
          bargap=0.1,
          xaxis=xaxis1,
          yaxis=yaxis1
          
        )

fig1


#WRITE OUT THE PICTURE
reticulate::py_run_string("import sys");
fileOut = paste(outputRoot,"Histogram_Returns_SP500.png",sep="");
save_image(fig1, fileOut, width = 500, height = 400)


xaxis2 <- list(title = "Annualized Monthly WTI Oil Abnormal Return (%)",
               showline = TRUE,
               showgrid = FALSE,
               showticklabels = TRUE,
               linecolor = 'rgb(204, 204, 204)',
               linewidth = 2,
               autotick = FALSE,
               tick0=0,
               dtick = 4,
               ticks = 'outside',
               tickcolor = 'rgb(204, 204, 204)',
               tickwidth = 2,
               #ticklen = 5,
               range=c(-11,11),
               tickformat="+.0%",
               tickfont = FONT1 
               
);

yaxis2 <- list(title = "Proportion of Months (%)",
               showgrid=TRUE,
               gridcolor = 'rgb(204, 204, 204)',
               gridwidth=2,
               showline = FALSE,
               showticklabels = TRUE,
               linecolor = 'rgb(204, 204, 204)',
               linewidth = 2,
               autotick = FALSE,
               tick0=0,
               dtick = 5,
               ticks = 'outside',
               tickcolor = 'rgb(204, 204, 204)',
               tickwidth = 2,
               ticklen = 5,
               range=c(0,24),
               tickformat="2",
               tickfont = FONT1 
               
);

#THE BASIC CHART FOR OIL AB RETURNS
fig2 = plot_ly(
  data = SP500,
  x = ~OilAbnormalReturnAnnualized,
  type = "histogram",
  histfunc='sum',
  histnorm = "percent",
  #colors = ~Symbol,
  nbinsx=80,
  marker= list(color = 'rgb(120, 120, 120)', size = 10)
);

fig2  = fig2 %>%
  layout(
    bargap=0.1,
    xaxis =xaxis2,
    yaxis = yaxis2
  )

fig2


#WRITE OUT THE PICTURE
reticulate::py_run_string("import sys");
fileOut = paste(outputRoot,"Oil_Returns_SP500.png",sep="");
save_image(fig2, fileOut, width = 500, height = 400)

