require(plotly)


#LOAD INPUT DATAFILE FROM OilBetaTimeCorr.Rdata
load("C:/Users/giles/OneDrive/MBA/Dissertation/Data/TimeOilBetaCorrBiWeekly.Rdata")


#############################################################################
################## OUTPUT SETUP #############################################
#############################################################################

outputRoot = "C:/Users/giles/OneDrive/MBA/Dissertation/Writeup/Pictures/"

#############################################################################
################## ENERGY HEATMAP ###########################################
#############################################################################

HeatmapData = allCorDataOut %>%
              filter(Comp1Group == "Energy" & Comp2Group == "Energy") %>%
              mutate(Comp1 = ifelse(Comp1=="Energy"," Energy\nSector",Comp1)) %>%
              mutate(Comp2 = ifelse(Comp2=="Energy"," Energy\nSector",Comp2)) %>%
              filter(n1 > 350) %>%
              filter(n2 > 350) %>%
              mutate(CorFormat = sprintf("%3.2f",Cor)) %>%
              mutate(ColorText = ifelse(Cor < 0.3,'rgb(255,150,0)','rgb(255,0,255)')) %>%
              mutate(ColorText = 'rgb(255,255,255)')

#THE MAIN CHART VARIABLES
fig_HM1 = plot_ly(
                  data = HeatmapData,
                  x = ~Comp1,
                  y = ~Comp2,
                  z = ~Cor,
                  type = "heatmap",
                  colors = "Greys",
                  showscale=FALSE,
                  zmin = 0,
                  zmax =1
                  );

#ADD THE LABELS FOR HIGH CORRELATIONS (WHITE)
fig_HM1 <- fig_HM1 %>% add_annotations(data = HeatmapData %>% filter(Cor > 0.3),
                                       x = ~Comp1,
                                       y = ~Comp2,
                                       text = ~CorFormat,
                                       xref = "x",
                                       yref = "y",
                                       showarrow = FALSE,
                                       ax = 0,
                                       ay = 0,
                                       font=list(size = 14,
                                                 color = 'rgb(255,255,255)' )
                                      )

#ADD THE LABELS FOR HIGH CORRELATIONS (GREY)
fig_HM1 <- fig_HM1 %>% add_annotations(data = HeatmapData %>% filter(Cor <= 0.3),
                                       x = ~Comp1,
                                       y = ~Comp2,
                                       text = ~CorFormat,
                                       xref = "x",
                                       yref = "y",
                                       showarrow = FALSE,
                                       ax = 0,
                                       ay = 0,
                                       font=list(size=14,
                                                 color = 'rgb(100,100,100)' )
)


#FONT FOR LABELS
FONT1 = list(family = 'Arial',
             size = 14,
             color = 'rgb(82, 82, 82)'
            # color = 'rgb(0, 0, 0)'
            );

xaxis_HM1 <- list(title = "",
               showline = FALSE,
               showgrid = FALSE,
               showticklabels = TRUE,
               linecolor = 'rgb(204, 204, 204)',
               linewidth = 2,
               autotick = FALSE,
               ticks = 'outside',
               tickcolor = 'rgb(204, 204, 204)',
               tickwidth = 2,
               tickfont = FONT1 
);

yaxis_HM1 <- list(title = "",
                  showline = FALSE,
                  showgrid = FALSE,
                  showticklabels = TRUE,
                  linecolor = 'rgb(204, 204, 204)',
                  linewidth = 2,
                  autotick = FALSE,
                  ticks = 'outside',
                  tickcolor = 'rgb(204, 204, 204)',
                  tickwidth = 2,
                  tickfont = FONT1,
                  autorange = "reversed"
);

#REMOVE LEGEND
fig_HM1 = fig_HM1 %>%
          layout(
                  xaxis = xaxis_HM1,
                  yaxis = yaxis_HM1,
                  showlegend = FALSE
                );

#DISPLAY OUTPUT
fig_HM1

#WRITE OUT THE PICTURE
reticulate::py_run_string("import sys");
fileOut = paste(outputRoot,"OilBetaHistoryCorrplot_Energy.png",sep="");
save_image(fig_HM1, fileOut, width = 1100, height = 1000)


#############################################################################
################## AIRLINES HEATMAP ###########################################
#############################################################################

HeatmapData2 = allCorDataOut %>%
  filter(Comp1Group == "Passenger Airlines" & Comp2Group == "Passenger Airlines") %>%
  mutate(Comp1 = ifelse(Comp1=="Passenger Airlines"," Passenger\nAirlines",Comp1)) %>%
  mutate(Comp2 = ifelse(Comp2=="Passenger Airlines"," Passenger\nAirlines",Comp2)) %>%
  filter(n1 > 100) %>%
  filter(n2 > 100) %>%
  mutate(CorFormat = sprintf("%3.2f",Cor)) %>%
  mutate(ColorText = ifelse(Cor < 0.35,'rgb(255,150,0)','rgb(255,0,255)')) %>%
  mutate(ColorText = 'rgb(255,255,255)')

#THE MAIN CHART VARIABLES
fig_HM2 = plot_ly(
  data = HeatmapData2,
  x = ~Comp1,
  y = ~Comp2,
  z = ~Cor,
  type = "heatmap",
  colors = "Greys",
  showscale=FALSE,
  zmin = 0,
  zmax =1
);

#ADD THE LABELS FOR HIGH CORRELATIONS (WHITE)
fig_HM2 <- fig_HM2 %>% add_annotations(data = HeatmapData2 %>% filter(Cor > 0.35),
                                       x = ~Comp1,
                                       y = ~Comp2,
                                       text = ~CorFormat,
                                       xref = "x",
                                       yref = "y",
                                       showarrow = FALSE,
                                       ax = 0,
                                       ay = 0,
                                       font=list(size = 14,
                                                 color = 'rgb(255,255,255)' )
)

#ADD THE LABELS FOR HIGH CORRELATIONS (GREY)
fig_HM2 <- fig_HM2 %>% add_annotations(data = HeatmapData2 %>% filter(Cor <= 0.35),
                                       x = ~Comp1,
                                       y = ~Comp2,
                                       text = ~CorFormat,
                                       xref = "x",
                                       yref = "y",
                                       showarrow = FALSE,
                                       ax = 0,
                                       ay = 0,
                                       font=list(size=14,
                                                 color = 'rgb(100,100,100)' )
)


#FONT FOR LABELS
FONT1 = list(family = 'Arial',
             size = 14,
             color = 'rgb(82, 82, 82)'
             # color = 'rgb(0, 0, 0)'
);

xaxis_HM2 <- list(title = "",
                  showline = FALSE,
                  showgrid = FALSE,
                  showticklabels = TRUE,
                  linecolor = 'rgb(204, 204, 204)',
                  linewidth = 2,
                  autotick = FALSE,
                  ticks = 'outside',
                  tickcolor = 'rgb(204, 204, 204)',
                  tickwidth = 2,
                  tickfont = FONT1 
);

yaxis_HM2 <- list(title = "",
                  showline = FALSE,
                  showgrid = FALSE,
                  showticklabels = TRUE,
                  linecolor = 'rgb(204, 204, 204)',
                  linewidth = 2,
                  autotick = FALSE,
                  ticks = 'outside',
                  tickcolor = 'rgb(204, 204, 204)',
                  tickwidth = 2,
                  tickfont = FONT1,
                  autorange = "reversed"
);

#REMOVE LEGEND
fig_HM2 = fig_HM2 %>%
  layout(
    xaxis = xaxis_HM2,
    yaxis = yaxis_HM2,
    showlegend = FALSE
  );

#DISPLAY OUTPUT
fig_HM2

#WRITE OUT THE PICTURE
reticulate::py_run_string("import sys");
fileOut = paste(outputRoot,"OilBetaHistoryCorrplot_Airlines.png",sep="");
save_image(fig_HM2, fileOut, width = 550, height = 500)
