require(tidyverse)
require(plotly)
require(robustbase)
require(quantreg)


#INPUT PARAMETERS
nPoints=20
noise=1;

#WHERE TO PUT THE IMAGES
outputRoot = "C:/Users/giles/OneDrive/MBA/Dissertation/Writeup/Pictures/"

####################################################################
############ OVERALL CHART SETTINGS ################################
####################################################################

#FONT FOR LABELS
FONT1 = list(family = 'Arial',
             size = 14,
             color = 'rgb(82, 82, 82)');

SETUP_LEGEND = list(
  font = list(
    family = 'Arial',
    size = 14,
    color = 'rgb(82, 82, 82)'
  ),
  x=0.65,
  y=0.05,
  bordercolor = 'rgb(255, 255, 255)',
  borderwidth = 2
);


####################################################################
############ THE CLEAN DATASET CHART ###############################
####################################################################


#CREATE SOME DATA
Data1 = data.frame( x= runif(min = -5,max = 5, n= nPoints)) %>%
        mutate(y = x * 1.0) %>%
        mutate(yNoise = runif(min = -noise,max = noise, n= nPoints)) %>%
        mutate(yTotal = y + yNoise) %>%
        arrange(x)

#CREATE LINEAR FITS OF THE DATA
LM1 = lm   (data = Data1, yTotal ~ x );
LM2 = lmrob(data = Data1, yTotal ~ x );
LM3 = rq   (data = Data1, yTotal ~ x );


#CREATE PREDICTIONS
LM1_P = predict(LM1,newdata = Data1);
LM2_P = predict(LM2,newdata = Data1);
LM3_P = predict(LM3,newdata = Data1);

#ADD PREDICTIONS TO DATASET
Data2 = Data1 %>%
        mutate(LM1_Predict = LM1_P) %>%
        mutate(LM2_Predict = LM2_P) %>%
        mutate(LM3_Predict = LM3_P);

#PLOT WITH PLOTLY
fig1 = plot_ly() %>%
      add_trace(
                name = "Measurements",
                data = Data2,
                x = ~x,y =~yTotal,
                type="scatter",
                mode = "markers",
                marker= list(color = 'rgb(0, 0, 0)', size = 6)
              );

fig1 = fig1 %>%
         add_trace(
          name = "OLS",
          data = Data2,
          x = ~x,
          y =~LM1_Predict,
          type="scatter",
          mode = "lines",
          line = list(color = 'rgb(0, 0, 0)', width = 1,dash="solid")
              ) %>%
        add_trace(
          name = "MM Robust LS",
          data = Data2,
          x = ~x,
          y =~LM2_Predict,
          type="scatter",
          mode = "lines",
          line = list(color = 'rgb(0, 0, 0)', width = 1,dash="dash")
        ) %>%
        add_trace(
          name = "Quantile Regression",
          data = Data2,
          x = ~x,
          y =~LM3_Predict,
          type="scatter",
          mode = "lines",
          line = list(color = 'rgb(0, 0, 0)', width = 1,dash="dot")
        )
  
xaxis1 <- list(title = "",
               showline = TRUE,
               showgrid = FALSE,
               showticklabels = TRUE,
               linecolor = 'rgb(204, 204, 204)',
               linewidth = 2,
               autotick = FALSE,
               tick0=0,
               dtick = 1.0,
               ticks = 'outside',
               tickcolor = 'rgb(204, 204, 204)',
               tickwidth = 2,
               #ticklen = 5,
               range=c(-5,5),
               tickformat="+2",
               tickfont = FONT1 
               
);

yaxis1 <- list(title = "",
               showline = TRUE,
               showgrid = FALSE,
               showticklabels = TRUE,
               linecolor = 'rgb(204, 204, 204)',
               linewidth = 2,
               autotick = FALSE,
               tick0=0,
               dtick = 1.0,
               ticks = 'outside',
               tickcolor = 'rgb(204, 204, 204)',
               tickwidth = 2,
               #ticklen = 5,
               range=c(-7.50,7.50),
               tickformat="+2",
               tickfont = FONT1 
               
);


fig1 = fig1 %>%
       layout(
          xaxis = xaxis1,
          yaxis = yaxis1,
         legend = SETUP_LEGEND
       )

####################################################################
############ THE DIRTY DATASET CHART ###############################
####################################################################

#INFECT A POINT 
Data2_Dirty = Data2;

Data2_Dirty$yTotal[2] = 4.6;
Data2_Dirty$yTotal[4] = 3.4;

#CREATE LINEAR FITS OF THE DATA
LM1D = lm   (data = Data2_Dirty, yTotal ~ x );
LM2D = lmrob(data = Data2_Dirty, yTotal ~ x );
LM3D = rq   (data = Data2_Dirty, yTotal ~ x );


#CREATE PREDICTIONS
LM1D_P = predict(LM1D,newdata = Data1);
LM2D_P = predict(LM2D,newdata = Data1);
LM3D_P = predict(LM3D,newdata = Data1);

#ADD PREDICTIONS TO DATASET
Data2_Dirty = Data2_Dirty %>%
  mutate(LM1D_Predict = LM1D_P) %>%
  mutate(LM2D_Predict = LM2D_P) %>%
  mutate(LM3D_Predict = LM3D_P);

#PLOT WITH PLOTLY
fig2 = plot_ly() %>%
  add_trace(
  name = "Measurements",
  data = Data2_Dirty,
  x = ~x,y =~yTotal,
  type="scatter",
  mode = "markers",
  marker= list(color = 'rgb(0, 0, 0)', size = 6)
) %>%
  add_trace(
    name = "OLS",
    data = Data2_Dirty,
    x = ~x,
    y =~LM1D_Predict,
    type="scatter",
    mode = "lines",
    line = list(color = 'rgb(0, 0, 0)', width = 1,dash="solid")
  ) %>%
  add_trace(
    name = "MM Robust LS",
    data = Data2_Dirty,
    x = ~x,
    y =~LM2D_Predict,
    type="scatter",
    mode = "lines",
    line = list(color = 'rgb(0, 0, 0)', width = 1,dash="dash")
  ) %>%
  add_trace(
    name = "Quantile Regression",
    data = Data2_Dirty,
    x = ~x,
    y =~LM3D_Predict,
    type="scatter",
    mode = "lines",
    line = list(color = 'rgb(0, 0, 0)', width = 1,dash="dot")
  )

xaxis2 <- list(title = "",
               showline = TRUE,
               showgrid = FALSE,
               showticklabels = TRUE,
               linecolor = 'rgb(204, 204, 204)',
               linewidth = 2,
               autotick = FALSE,
               tick0=0,
               dtick = 1.0,
               ticks = 'outside',
               tickcolor = 'rgb(204, 204, 204)',
               tickwidth = 2,
               #ticklen = 5,
               range=c(-5,5),
               tickformat="+2",
               tickfont = FONT1 
               
);

yaxis2 <- list(title = "",
               showline = TRUE,
               showgrid = FALSE,
               showticklabels = TRUE,
               linecolor = 'rgb(204, 204, 204)',
               linewidth = 2,
               autotick = FALSE,
               tick0=0,
               dtick = 1.0,
               ticks = 'outside',
               tickcolor = 'rgb(204, 204, 204)',
               tickwidth = 2,
               #ticklen = 5,
               range=c(-7.5,7.5),
               tickformat="+2",
               tickfont = FONT1 
               
);

fig2 = fig2 %>%
  layout(
    xaxis = xaxis2,
    yaxis = yaxis2,
    legend = SETUP_LEGEND
  )

fig2

#WRITE OUT THE PICTURE
reticulate::py_run_string("import sys");
fileOut = paste(outputRoot,"RobustEffect1.png",sep="");
save_image(fig1, fileOut, width = 500, height = 500)

#WRITE OUT THE PICTURE
reticulate::py_run_string("import sys");
fileOut = paste(outputRoot,"RobustEffect2.png",sep="");
save_image(fig2, fileOut, width = 500, height = 500)
