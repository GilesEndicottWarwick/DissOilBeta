require(tidyverse)
require(plotly)
require(readxl)


#DataOil = read.csv("C:/Users/giles/Creative Cloud Files/MBA/Dissertation/Data/world-oil-supply-and-demand-1971-2020.csv",
#                    header=T,skip= 4);

DataOil2 = read_excel("C:/Users/giles/Creative Cloud Files/MBA/Dissertation/Data/OilSupplyDemandOECD.xlsx",sheet="Data")

#DATA FROM https://stats.oecd.org/BrandedView.aspx?oecd_bv_id=oil-data-en&doi=data-00474-en#
#cite as IEA (2023), "World oil statistics", IEA Oil Information Statistics (database), https://doi.org/10.1787/data-00474-en (accessed on 25 December 2023).

DataOil3 = DataOil2 %>%
           mutate(OilDemand = str_replace_all(`Totaloildemand`,"\\s+","")) %>%
           mutate(OilProduction = str_replace_all(`CrudeoilandNGLproduction`,"\\s+","")) %>%
           mutate(OilDemand = 1/1000 * as.numeric(OilDemand)) %>%
           mutate(OilProduction = 1/1000 * as.numeric(OilProduction)) %>%
           dplyr::select( -`Totaloildemand` , -`CrudeoilandNGLproduction`  ) %>%
           mutate(DemandGap = OilDemand - OilProduction ) %>%
           rename(Year=Time)


FONT1 = list(family = 'Arial',
             size = 22,
             color = 'rgb(82, 82, 82)');

FONT2 = list(family = 'Arial',
             size = 18,
             color = 'rgb(82, 82, 82)');


###################################################################################
################# OIL SUPPLY AND DEMAND HISTORY ###################################
###################################################################################

xaxis <- list(title = "",
              showline = TRUE,
              showgrid = FALSE,
              showticklabels = TRUE,
              linecolor = 'rgb(204, 204, 204)',
              linewidth = 2,
              autotick = FALSE,
              tick0=1990,
              dtick = 10,
              ticks = 'outside',
              tickcolor = 'rgb(204, 204, 204)',
              tickwidth = 2,
              #ticklen = 5,
              range=c(1974.99,2023.6),
              tickfont = FONT1 
              
);



yaxis <- list(title = list(text = "Oil Production & Demand (M To)",
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
              dtick = 500,
              ticklabelstep=1,
              showgrid=TRUE,
              gridcolor = 'rgb(204, 204, 204)',
              gridwidth=2,
              zeroline=FALSE,
              #ticksuffix = "  ",
              tickprefix = "  ",
              #range=c(0.1,399),
              tickfont = FONT1,
              font = FONT1,
              side="left"
);



fig <- plot_ly(DataOil3,
               x = ~Year, 
               y = ~OilProduction, 
               name = 'World Oil Production',
               type = 'scatter',
               mode = 'lines',
               line = list(color = 'rgb(0, 0, 0)', width = 3)
               #marker= list(color = 'rgb(0, 0, 0)', size = 10)
) %>%
add_trace(
  x = ~Year, 
  y = ~OilDemand, 
  name = 'World Oil Demand',
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
                                   size = 20,
                                   color = 'rgb(82, 82, 82)'
                                 ),
                     x=0.0,
                     y=0.90,
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

###################################################################################
################# OIL DEMAND SUPPLY GAP  ##########################################
###################################################################################

xaxis2 <- list(title = "",
              showline = TRUE,
              showgrid = FALSE,
              showticklabels = TRUE,
              linecolor = 'rgb(204, 204, 204)',
              linewidth = 2,
              autotick = FALSE,
              tick0=1990,
              dtick = 10,
              ticks = 'outside',
              tickcolor = 'rgb(204, 204, 204)',
              tickwidth = 2,
              #ticklen = 5,
              range=c(1974.5,2023.6),
              tickfont = FONT1 ,
              xperiodalignment="middle"
              
);



yaxis2 <- list(title = list(text = "Oil Demand-Supply Gap (M To)",
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
              dtick = 50,
              ticklabelstep=1,
              showgrid=TRUE,
              gridcolor = 'rgb(204, 204, 204)',
              gridwidth=2,
              zeroline=FALSE,
              #ticksuffix = "  ",
              tickprefix = "  ",
              #range=c(0.1,399),
              tickfont = FONT1,
              font = FONT1,
              side="left"
);



fig2 <- plot_ly(DataOil3,
               x = ~Year, 
               y = ~DemandGap, 
               name = 'World Oil Demand-Supply Gap',
               type = 'bar',
               marker= list(color = 'rgb(120, 120, 120)', size = 10)
) 



#APPLY AXIS SETTINGS
fig2 <- fig2 %>% layout(title = "",
                      xaxis = xaxis2,
                      yaxis = yaxis2
);


print(fig2)

###################################################################################
################# CREATE GAP AS % OF SUPPLY #######################################
###################################################################################

DataOil4 = DataOil3 %>%
           mutate(GapPercSup = 100*DemandGap / OilProduction) %>%
           mutate(GapPercDem = 100*DemandGap / OilDemand);

xaxis2b <- list(title = "",
               showline = TRUE,
               showgrid = FALSE,
               showticklabels = TRUE,
               linecolor = 'rgb(204, 204, 204)',
               linewidth = 2,
               autotick = FALSE,
               tick0=1990,
               dtick = 10,
               ticks = 'outside',
               tickcolor = 'rgb(204, 204, 204)',
               tickwidth = 2,
               #ticklen = 5,
               range=c(1974.5,2023.6),
               tickfont = FONT1 ,
               xperiodalignment="middle"
               
);



yaxis2b <- list(title = list(text = "Oil Demand-Supply Gap (%)",
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
               dtick = 1,
               ticklabelstep=1,
               showgrid=TRUE,
               gridcolor = 'rgb(204, 204, 204)',
               gridwidth=2,
               zeroline=FALSE,
               #ticksuffix = "  ",
               tickprefix = "  ",
               #range=c(0.1,399),
               tickfont = FONT1,
               font = FONT1,
               side="left"
);



fig2b <- plot_ly(DataOil4,
                x = ~Year, 
                y = ~GapPercSup, 
                name = 'World Oil Demand-Supply Gap (%)',
                type = 'bar',
                marker= list(color = 'rgb(120, 120, 120)', size = 10)
) 



#APPLY AXIS SETTINGS
fig2b <- fig2b %>% layout(title = "",
                        xaxis = xaxis2b,
                        yaxis = yaxis2b
);


print(fig2b)



###################################################################################
################# CREATE SUBPLOT ##################################################
###################################################################################

FIG3= subplot(fig,
              fig2,
              nrows = 1,
              margin = 0.07)

print(FIG3)

###################################################################################
################# OUTPUT FILES   ##################################################
###################################################################################

outputRoot = "C:/Users/giles/Creative Cloud Files/MBA/Dissertation/Writeup/Pictures/"

#WRITE OUT THE PICTURE
reticulate::py_run_string("import sys");
fileOut = paste(outputRoot,"Oil_Demand.png",sep="");
save_image(fig, fileOut, width = 500, height = 500)

#WRITE OUT THE PICTURE
reticulate::py_run_string("import sys");
fileOut = paste(outputRoot,"Oil_DemandGap_abs.png",sep="");
save_image(fig2, fileOut, width = 500, height = 500)

#WRITE OUT THE PICTURE
reticulate::py_run_string("import sys");
fileOut = paste(outputRoot,"Oil_DemandGap.png",sep="");
save_image(fig2b, fileOut, width = 500, height = 500)

#WRITE OUT THE PICTURE
reticulate::py_run_string("import sys");
fileOut = paste(outputRoot,"Oil_DemandGap_sm.png",sep="");
save_image(fig2b, fileOut, width = 250, height = 250)
