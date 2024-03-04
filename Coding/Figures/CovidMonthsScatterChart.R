require(plotly)
require(robustbase)
require(quantreg)
require(tidyverse)
require(lubridate)

#LOAD THE DATA IN
load("C:/Users/giles/OneDrive/MBA/Dissertation/Data/MonthlyIndustryReturns.Rdata");

#WHERE TO PUT THE IMAGES
outputRoot = "C:/Users/giles/OneDrive/MBA/Dissertation/Writeup/Pictures/"

#LISTS TO KEEP TEXT
list_SP  = c("202003","202005","202004");
list_Oil = c("202003","202005","202004");

# GRAB JUST ENERGY DATA
EnergyScatter = IndustryReturnsDataWithOil_OUT %>%
                ungroup() %>%
                filter(Industry == "Energy") %>%
                mutate(YearMonth2 = ifelse( YearMonth %in% list_SP,YearMonth,"")) %>%
                mutate(YearMonth3 = ifelse( YearMonth %in% list_Oil,YearMonth,"")) %>%
                mutate(RankDate = rank(desc(Date))) %>%
                filter(RankDate <= (12*5))

#FONT FOR LABELS
FONT1 = list(family = 'Arial',
             size = 14,
             color = 'rgb(82, 82, 82)');


#S+P 500 CORR CHART
fig3 <- plot_ly() %>% 
  add_trace(data = EnergyScatter,
              x = ~SP500AbnormalReturnAnnualized*100, 
              y = ~AbnormalReturnAnnualized*100,
              text = ~Year,
              type = 'scatter',
              mode = 'markers',
              marker= list(color = 'rgb(0, 0, 0)', size = 6)) %>%
      add_text( data = EnergyScatter,
                x = ~SP500AbnormalReturnAnnualized*100, 
                y = ~AbnormalReturnAnnualized*100,
                text = ~YearMonth2,
                textfont = list(size=12,
                               color = 'rgb(100,100,100)'), 
                               textposition = "top right")

xaxis3 <- list(title = "S&P 500 Abnormal Return (%)",
               showline = TRUE,
               showgrid = FALSE,
               showticklabels = TRUE,
               linecolor = 'rgb(204, 204, 204)',
               linewidth = 2,
               autotick = FALSE,
               tick0=0,
               dtick = 100,
               ticks = 'outside',
               tickcolor = 'rgb(204, 204, 204)',
               tickwidth = 2,
               #ticklen = 5,
               range=c(-300,300),
               tickformat="+2",
               tickfont = FONT1 
               
);

yaxis3 <- list(title = "Energy Sector Abnormal Return (%)",
               showline = TRUE,
               showgrid = FALSE,
               showticklabels = TRUE,
               linecolor = 'rgb(204, 204, 204)',
               linewidth = 2,
               autotick = FALSE,
               tick0=0,
               dtick = 200,
               ticks = 'outside',
               tickcolor = 'rgb(204, 204, 204)',
               tickwidth = 2,
               #ticklen = 5,
               range=c(-700,700),
               tickformat="+2",
               tickfont = FONT1 
               
);

fig3= fig3 %>%
      layout(xaxis = xaxis3,
             yaxis = yaxis3,
             showlegend = FALSE)

fig3

#OIL CORR CHART

fig4 <- plot_ly() %>% 
  add_trace(data = EnergyScatter,
            x = ~OilAbnormalReturnAnnualized*100, 
            y = ~AbnormalReturnAnnualized*100,
            text = ~Year,
            type = 'scatter',
            mode = 'markers',
            showlegend=FALSE,
            marker= list(color = 'rgb(0, 0, 0)', size = 6)) %>%
  add_text( data = EnergyScatter,
            x = ~OilAbnormalReturnAnnualized*100, 
            y = ~AbnormalReturnAnnualized*100,
            text = ~YearMonth3,
            showlegend=FALSE,
            textfont = list(size=12,
                            color = 'rgb(100,100,100)'), 
            textposition = "top right")


xaxis4 <- list(title = "WTI Oil Abnormal Return (%)",
               showline = TRUE,
               showgrid = FALSE,
               showticklabels = TRUE,
               linecolor = 'rgb(204, 204, 204)',
               linewidth = 2,
               autotick = FALSE,
               tick0=0,
               dtick = 400,
               ticks = 'outside',
               tickcolor = 'rgb(204, 204, 204)',
               tickwidth = 2,
               #ticklen = 5,
               range=c(-1250,1250),
               tickformat="+2",
               tickfont = FONT1 
               
);

yaxis4 <- list(title = "Energy Sector Abnormal Return (%)",
               showline = TRUE,
               showgrid = FALSE,
               showticklabels = TRUE,
               linecolor = 'rgb(204, 204, 204)',
               linewidth = 2,
               autotick = FALSE,
               tick0=0,
               dtick = 200,
               ticks = 'outside',
               tickcolor = 'rgb(204, 204, 204)',
               tickwidth = 2,
               #ticklen = 5,
               range=c(-700,800),
               tickformat="+2",
               tickfont = FONT1 
               
);

SETUP_LEGEND = list(
  font = list(
    family = 'Arial',
    size = 14,
    color = 'rgb(82, 82, 82)'
  ),
  x=0.65,
  y=0.00,
  bordercolor = 'rgb(255, 255, 255)',
  borderwidth = 2
);

fig4= fig4 %>%
  layout(xaxis = xaxis4,
         yaxis = yaxis4,
         legend = SETUP_LEGEND,
         showlegend = FALSE)

fig4= fig4 %>%
  layout(showlegend = TRUE)




fig4

#WRITE OUT THE PICTURE
reticulate::py_run_string("import sys");
fileOut = paste(outputRoot,"Monthly_ScatterEnergy_SP500.png",sep="");
save_image(fig3, fileOut, width = 500, height = 500)

#WRITE OUT THE PICTURE
reticulate::py_run_string("import sys");
fileOut = paste(outputRoot,"Monthly_ScatterEnergy_Oil.png",sep="");
save_image(fig4, fileOut, width = 500, height = 500)
