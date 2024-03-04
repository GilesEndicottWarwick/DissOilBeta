require(tidyverse)
require(plotly)

n=50;

#############################################################################
################## OUTPUT SETUP #############################################
#############################################################################

outputRoot = "C:/Users/giles/OneDrive/MBA/Dissertation/Writeup/Pictures/"


#############################################################################
################## CREATE DATA ##############################################
#############################################################################

lim_low = -1.5;
lim_high = 1.5

X = data.frame(x = runif(n,-1,1)) %>%
    mutate(Y1 = x * -1.0) %>%
    mutate(Y2 = x * -1.0 + runif(n,-1,1)) %>%
    mutate(Y3 = x * 0.0 + runif(n,-1,1) ) %>%
    mutate(Y4 = x * 0.75 + runif(n,-1,1)) %>%
    mutate(Y5 = x * 1.0);

#GET ALL CORRELATRION VALUES
COR_1 = round(cor(X$x,X$Y1),2);
COR_2 = round(cor(X$x,X$Y2),2);
COR_3 = round(cor(X$x,X$Y3),2);
COR_4 = round(cor(X$x,X$Y4),2);
COR_5 = round(cor(X$x,X$Y5),2);

#FONT FOR LABELS
FONT1 = list(family = 'Arial',
             size = 16,
             color = 'rgb(82, 82, 82)');

xaxis1 <- list(
               showline = FALSE,
               showgrid = TRUE,
               showticklabels = FALSE,
               linecolor = 'rgb(204, 204, 204)',
               linewidth = 0,
               autotick = FALSE,
               tick0=0,
               dtick = 0.5,
               ticks = 'outside',
               tickcolor = 'rgb(255, 255, 255)',
               tickwidth = 0,
               #ticklen = 5,
               range=c(lim_low,lim_high),
               #tickformat="",
               tickfont = FONT1 
               
);

yaxis1 = xaxis1;

F1 = plot_ly() %>%
     add_trace(data = X,
               x= ~x,
               y= ~Y1,
               type="scatter",
               mode= "markers",
               marker= list(color = 'rgb(0, 0, 0)', size = 6)) %>%
               layout(
                      xaxis = xaxis1,
                      yaxis = yaxis1,
                      showlegend=F
                      )


F1
     

F2 = plot_ly() %>%
  add_trace(data = X,
            x= ~x,
            y= ~Y2,
            type="scatter",
            mode= "markers",
            marker= list(color = 'rgb(0, 0, 0)', size = 6)) %>%
            layout(
              xaxis = xaxis1,
              yaxis = yaxis1,
              showlegend=F
            )


F2

F3 = plot_ly() %>%
  add_trace(data = X,
            x= ~x,
            y= ~Y3,
            type="scatter",
            mode= "markers",
            marker= list(color = 'rgb(0, 0, 0)', size = 6)) %>%
            layout(
              xaxis = xaxis1,
              yaxis = yaxis1,
              showlegend=F
            )


F3

F4 = plot_ly() %>%
  add_trace(data = X,
            x= ~x,
            y= ~Y4,
            type="scatter",
            mode= "markers",
            marker= list(color = 'rgb(0, 0, 0)', size = 6)) %>%
            layout(
              xaxis = xaxis1,
              yaxis = yaxis1,
              showlegend=F
            )


F4


F5 = plot_ly() %>%
  add_trace(data = X,
            x= ~x,
            y= ~Y5,
            type="scatter",
            mode= "markers",
            marker= list(color = 'rgb(0, 0, 0)', size = 6)) %>%
              layout(
                xaxis = xaxis1,
                yaxis = yaxis1,
                showlegend=F
              )



F5

#ANNOTATIONS FOR EACH CHART WITH THE R CORRELATION VALUE
annotations = list( 
  list( 
    x = 0.1,  
    y = 1.0,  
    text = paste("r=",COR_1,", r² = " , sprintf("%.2f",COR_1 * COR_1),sep=""),  
    xref = "paper",  
    yref = "paper",  
    xanchor = "center",  
    yanchor = "bottom",  
    showarrow = FALSE,
    font = FONT1
  ),  
  list( 
    x = 0.3,  
    y = 1,  
    text = paste("r = ",COR_2,", r² = " , sprintf("%.2f",COR_2 * COR_2),sep=""),  
    xref = "paper",  
    yref = "paper",  
    xanchor = "center",  
    yanchor = "bottom",  
    showarrow = FALSE ,
    font = FONT1
  ),  
  list( 
    x = 0.5,  
    y = 1.0,  
    text = paste("r = ",COR_3,", r² = " , sprintf("%.2f",COR_3 * COR_3),sep=""),   
    xref = "paper",  
    yref = "paper",  
    xanchor = "center",  
    yanchor = "bottom",  
    showarrow = FALSE ,
    font = FONT1
  ),
  list( 
    x = 0.7,  
    y = 1.0,  
    text = paste("r = ",COR_4,", r² = " , sprintf("%.2f",COR_4 * COR_4),sep=""),   
    xref = "paper",  
    yref = "paper",  
    xanchor = "center",  
    yanchor = "bottom",  
    showarrow = FALSE ,
    font = FONT1
  ),
  list( 
    x = 0.9,  
    y = 1.0,  
    text = paste("r = ",COR_5,", r² = " , sprintf("%.2f",COR_5 * COR_5),sep=""), 
    xref = "paper",  
    yref = "paper",  
    xanchor = "center",  
    yanchor = "bottom",  
    showarrow = FALSE ,
    font = FONT1
  )
  )


F_ALL = subplot(F1,F2,F3,F4,F5,nrows=1 ,margin = 0.01) %>%
        layout ( showlegend=F) %>%
        layout (annotations = annotations)

F_ALL

#WRITE OUT THE PICTURE
reticulate::py_run_string("import sys");
fileOut = paste(outputRoot,"CorrelationDemo.png",sep="");
save_image(F_ALL, fileOut, width = 1000, height = 200)
