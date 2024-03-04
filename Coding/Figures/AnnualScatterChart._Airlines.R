require(plotly)
require(robustbase)
require(quantreg)
require(tidyverse)
require(lubridate)

#LOAD THE DATA IN
load("C:/Users/giles/OneDrive/MBA/Dissertation/Data/MonthlyIndustryReturnsAnnual.Rdata");

#WHERE TO PUT THE IMAGES
outputRoot = "C:/Users/giles/OneDrive/MBA/Dissertation/Writeup/Pictures/"

# GRAB JUST ENERGY DATA
EnergyScatter = IndustryReturnsDataWithOil_OUT_Annual %>%
                filter(Industry == "Passenger Airlines")

#FONT FOR LABELS
FONT1 = list(family = 'Arial',
             size = 14,
             color = 'rgb(82, 82, 82)');

#REGRESSION ANALYSIS
LM_SP_OLS   = lm( data = EnergyScatter, AbnormalReturnAnnualized ~ SP500AbnormalReturnAnnualized );
LM_SP_LMROB = lmrob( data = EnergyScatter, AbnormalReturnAnnualized ~ SP500AbnormalReturnAnnualized );
LM_SP_QR    = rq( data = EnergyScatter, AbnormalReturnAnnualized ~ SP500AbnormalReturnAnnualized );

#PREDICTION STAGE
LinesFitSP500 = data.frame(SP500AbnormalReturnAnnualized = seq(from = -100, to = 120, by =10));
  
LinesFitSP500$OLSfit = predict(LM_SP_OLS,newdata = LinesFitSP500)
LinesFitSP500$ROBfit = predict(LM_SP_LMROB,newdata = LinesFitSP500)
LinesFitSP500$RQfit  = predict(LM_SP_QR,newdata = LinesFitSP500)

#REGRESSION ANALYSIS (OIL)
LM_OIL_OLS   = lm( data = EnergyScatter, AbnormalReturnAnnualized ~ OilAbnormalReturnAnnualized );
LM_OIL_LMROB = lmrob( data = EnergyScatter, AbnormalReturnAnnualized ~ OilAbnormalReturnAnnualized );
LM_OIL_QR    = rq( data = EnergyScatter, AbnormalReturnAnnualized ~ OilAbnormalReturnAnnualized );

#PREDICTION STAGE
LinesFitOil = data.frame(OilAbnormalReturnAnnualized = seq(from = -100, to = 120, by =10));

LinesFitOil$OLSfit = predict(LM_OIL_OLS  ,newdata = LinesFitOil)
LinesFitOil$ROBfit = predict(LM_OIL_LMROB,newdata = LinesFitOil)
LinesFitOil$RQfit  = predict(LM_OIL_QR   ,newdata = LinesFitOil)



#S+P 500 CORR CHART
fig3 <- plot_ly() %>% 
  add_trace(data = EnergyScatter,
              x = ~SP500AbnormalReturnAnnualized*1.00, 
              y = ~AbnormalReturnAnnualized*1.00,
              text = ~Year,
              type = 'scatter',
              mode = 'markers',
              marker= list(color = 'rgb(0, 0, 0)', size = 6)) %>%
      add_text( data = EnergyScatter,
                x = ~SP500AbnormalReturnAnnualized*1.00, 
                y = ~AbnormalReturnAnnualized*1.00,
                text = ~Year,
                textfont = list(size=12,
                               color = 'rgb(100,100,100)'), 
                               textposition = "top right")


fig3 = fig3 %>%
      add_trace(data= LinesFitSP500,
                x= ~SP500AbnormalReturnAnnualized,
                y= ~OLSfit,
                type = 'scatter',
                text = "OLS Fit",
                mode = "lines",
                line = list(color = 'rgb(0, 0, 0)', width = 1,dash="solid")
                ) %>%
  add_trace(data= LinesFitSP500,
            x= ~SP500AbnormalReturnAnnualized,
            y= ~ROBfit,
            type = 'scatter',
            text = "ROBfit",
            mode = "lines",
            line = list(color = 'rgb(0, 0, 0)', width = 1,dash="dash")
  ) %>%
  add_trace(data= LinesFitSP500,
            x= ~SP500AbnormalReturnAnnualized,
            y= ~RQfit,
            type = 'scatter',
            text = "QRfit",
            mode = "lines",
            line = list(color = 'rgb(0, 0, 0)', width = 1,dash="dot")
  )

xaxis3 <- list(title = "S&P 500 Abnormal Return (%)",
               showline = TRUE,
               showgrid = FALSE,
               showticklabels = TRUE,
               linecolor = 'rgb(204, 204, 204)',
               linewidth = 2,
               autotick = FALSE,
               tick0=0,
               dtick = 0.15,
               ticks = 'outside',
               tickcolor = 'rgb(204, 204, 204)',
               tickwidth = 2,
               #ticklen = 5,
               range=c(-0.50,0.40),
               tickformat="+.0%",
               tickfont = FONT1 
               
);

yaxis3 <- list(title = "US Airline Sector Abnormal Return (%)",
               showline = TRUE,
               showgrid = FALSE,
               showticklabels = TRUE,
               linecolor = 'rgb(204, 204, 204)',
               linewidth = 2,
               autotick = FALSE,
               tick0=0,
               dtick = 0.25,
               ticks = 'outside',
               tickcolor = 'rgb(204, 204, 204)',
               tickwidth = 2,
               #ticklen = 5,
               range=c(-0.55,1.00),
               tickformat="+.0%",
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
            x = ~OilAbnormalReturnAnnualized*1.00, 
            y = ~AbnormalReturnAnnualized*1.00,
            text = ~Year,
            type = 'scatter',
            mode = 'markers',
            showlegend=FALSE,
            marker= list(color = 'rgb(0, 0, 0)', size = 6)) %>%
  add_text( data = EnergyScatter,
            x = ~OilAbnormalReturnAnnualized*1.00, 
            y = ~AbnormalReturnAnnualized*1.00,
            text = ~Year,
            showlegend=FALSE,
            textfont = list(size=12,
                            color = 'rgb(100,100,100)'), 
            textposition = "top right")


fig4 = fig4 %>%
  add_trace(data= LinesFitOil,
            x= ~OilAbnormalReturnAnnualized,
            y= ~OLSfit,
            name = 'OLS',
            type = 'scatter',
            text = "OLS Fit",
            mode = "lines",
            line = list(color = 'rgb(0, 0, 0)', width = 2,dash="solid")
  ) %>%
  add_trace(data= LinesFitOil,
            x= ~OilAbnormalReturnAnnualized,
            y= ~ROBfit,
            name = 'MM Robust Reg.',
            type = 'scatter',
            text = "ROBfit",
            mode = "lines",
            line = list(color = 'rgb(0, 0, 0)', width = 2,dash="dash")
  ) %>%
  add_trace(data= LinesFitOil,
            x= ~OilAbnormalReturnAnnualized,
            y= ~RQfit,
            name = 'Quantile Reg.',
            type = 'scatter',
            text = "QRfit",
            mode = "lines",
            line = list(color = 'rgb(0, 0, 0)', width = 2,dash="dot")
  )

xaxis4 <- list(title = "WTI Oil Abnormal Return (%)",
               showline = TRUE,
               showgrid = FALSE,
               showticklabels = TRUE,
               linecolor = 'rgb(204, 204, 204)',
               linewidth = 2,
               autotick = FALSE,
               tick0=0,
               dtick = 0.5,
               ticks = 'outside',
               tickcolor = 'rgb(204, 204, 204)',
               tickwidth = 2,
               #ticklen = 5,
               range=c(-0.80,1.20),
               tickformat="+.0%",
               tickfont = FONT1 
               
);

yaxis4 <- list(title = "US Airline Sector Abnormal Return (%)",
               showline = TRUE,
               showgrid = FALSE,
               showticklabels = TRUE,
               linecolor = 'rgb(204, 204, 204)',
               linewidth = 2,
               autotick = FALSE,
               tick0=0,
               dtick = 0.25,
               ticks = 'outside',
               tickcolor = 'rgb(204, 204, 204)',
               tickwidth = 2,
               #ticklen = 5,
               range=c(-0.55,1.0),
               tickformat="+.0%",
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




fig3
fig4

#WRITE OUT THE PICTURE
reticulate::py_run_string("import sys");
fileOut = paste(outputRoot,"ScatterAirlines_SP500.png",sep="");
save_image(fig3, fileOut, width = 500, height = 500)

#WRITE OUT THE PICTURE
reticulate::py_run_string("import sys");
fileOut = paste(outputRoot,"ScatterAirlines_Oil.png",sep="");
save_image(fig4, fileOut, width = 500, height = 500)
