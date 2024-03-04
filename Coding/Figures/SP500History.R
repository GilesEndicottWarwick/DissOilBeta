require(tidyverse)
require(plotly)
require(lubridate)
require(data.table)
require(readxl)

#############################################################################
################## OUTPUT SETUP #############################################
#############################################################################

outputRoot = "C:/Users/giles/OneDrive/MBA/Dissertation/Writeup/Pictures/"


#############################################################################
################## LOAD DATA ################################################
#############################################################################

#FILE FOR WRITING OUT
filenameIn = "C:/Users/giles/OneDrive/MBA/Dissertation/Data/SharesDataImported.Rdata";

#LOAD DATA IN 
load(filenameIn);

SymbolList = allDataIn %>%
              select(Symbol,SymbolDesc) %>%
              distinct();

#FILTER TO RELEVANT SYMBOLS
theseSymbols = c(	"^GSPC","^TNX","CL=F","__WTC_D","__WTC_D_REAL");

thisDataset = allDataIn %>%
              filter(Symbol %in% theseSymbols ) %>%
              select(-Industry) %>%
              select(-SubIndustry) %>%
              mutate(Date = ymd(Date)) %>%
              mutate(Year = year(Date)) %>%
              mutate(Quarter = quarter(Date)) %>%
              filter(!is.na(SymbolValue)) %>%
              group_by(Symbol,SymbolDesc,Year,Quarter) %>%
              mutate(QuarterRank = frank(desc(Date)))

QuarterData = thisDataset %>%
              filter(QuarterRank == 1) %>%
              mutate(YearFractional = Year + (Quarter/4))  %>%
              ungroup();
#rm(allDataIn);

#############################################################################
################## GLOBAL SETUP  ############################################
#############################################################################

#FONT FOR LABELS
FONT1 = list(family = 'Arial',
             size = 20,
             color = 'rgb(82, 82, 82)');

#FONT FOR Y AXIS TITLE
FONT2 = list(family = 'Arial',
             size = 20,
             color = 'rgb(82, 82, 82)');

#############################################################################
################## S & P 500 INDEX HISTORY ##################################
#############################################################################



xaxis <- list(title = "",
              showline = TRUE,
              showgrid = FALSE,
              showticklabels = TRUE,
              linecolor = 'rgb(204, 204, 204)',
              linewidth = 2,
              autotick = FALSE,
              tick0=1990,
              dtick = 5,
              ticks = 'outside',
              tickcolor = 'rgb(204, 204, 204)',
              tickwidth = 2,
              #ticklen = 5,
              range=c(1989.99,2024.1),
              tickfont = FONT1 
  
);



yaxis <- list(title = list(text = "S&P 500 Index Price ($)",
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
              dtick = 1000,
              ticklabelstep=1,
              showgrid=TRUE,
              gridcolor = 'rgb(204, 204, 204)',
              gridwidth=2,
              zeroline=FALSE,
              #ticksuffix = "  ",
              tickprefix = " ",
              #range=c(0.1,399),
              tickfont = FONT1,
              font = FONT1
              );



fig <- plot_ly(QuarterData %>% filter(Symbol =="^GSPC"),
               x = ~YearFractional, 
               y = ~SymbolValue, 
               name = 'High 2014',
               type = 'scatter',
               mode = 'lines',
               line = list(color = 'rgb(0, 0, 0)', width = 3)
               #marker= list(color = 'rgb(0, 0, 0)', size = 10)
) 


fig <- fig %>% layout(title = "",
                      xaxis = xaxis,
                      yaxis = yaxis
                      );

print(fig)


#WRITE OUT THE PICTURE
reticulate::py_run_string("import sys");
fileOut = paste(outputRoot,"SP500History.png",sep="");
save_image(fig, fileOut, width = 500, height = 600)

#############################################################################
################## RISK FREE RATE HISTORY ###################################
#############################################################################


xaxis2 <- list(title = "",
              showline = TRUE,
              showgrid = FALSE,
              showticklabels = TRUE,
              linecolor = 'rgb(204, 204, 204)',
              linewidth = 2,
              autotick = FALSE,
              tick0=1990,
              dtick = 5,
              ticks = 'outside',
              tickcolor = 'rgb(204, 204, 204)',
              tickwidth = 2,
              #ticklen = 5,
              range=c(1989.99,2024.1),
              tickfont = FONT1 
              
);



yaxis2 <- list(title = list(text = "US 10 Year Treasury Bill Rate (%)",
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
              dtick = 2.0,
              #ticklabelstep=1,
              showgrid=TRUE,
              gridcolor = 'rgb(204, 204, 204)',
              gridwidth=2,
              zeroline=FALSE,
              #ticksuffix = "  ",
              tickprefix = "  ",
              range=c(-0.0,10.1),
              tickfont = FONT1,
              font = FONT1
);



fig2 <- plot_ly(QuarterData %>% filter(Symbol =="^TNX"),
               x = ~YearFractional, 
               y = ~SymbolValue, 
               name = 'High 2014',
               type = 'scatter',
               mode = 'lines',
               line = list(color = 'rgb(0, 0, 0)', width = 3)
               #marker= list(color = 'rgb(0, 0, 0)', size = 10)
) 


fig2 <- fig2 %>% layout(title = "",
                      xaxis = xaxis2,
                      yaxis = yaxis2
);

print(fig2)


#WRITE OUT THE PICTURE
reticulate::py_run_string("import sys");
fileOut = paste(outputRoot,"rr_rate_History.png",sep="");
save_image(fig2, fileOut, width = 1000, height = 400)



#############################################################################
################## WTI OIL HISTORY ##########################################
#############################################################################


xaxis3 <- list(title = "",
               showline = TRUE,
               showgrid = FALSE,
               showticklabels = TRUE,
               linecolor = 'rgb(204, 204, 204)',
               linewidth = 2,
               autotick = FALSE,
               tick0=1990,
               dtick = 5,
               ticks = 'outside',
               tickcolor = 'rgb(204, 204, 204)',
               tickwidth = 2,
               #ticklen = 5,
               range=c(1989.99,2024.1),
               tickfont = FONT1 
               
);



yaxis3 <- list(title = list(text = "WTI Oil Price ($)",
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
               dtick = 40.0,
               #ticklabelstep=1,
               showgrid=TRUE,
               gridcolor = 'rgb(204, 204, 204)',
               gridwidth=2,
               zeroline=FALSE,
               #ticksuffix = "  ",
               tickprefix = "  ",
               range=c(-0.0,150),
               tickfont = FONT1,
               font = FONT1
);



fig3 <- plot_ly(QuarterData %>% filter(Symbol =="__WTC_D"),
                x = ~YearFractional, 
                y = ~SymbolValue, 
               # name = 'High 2014',
                type = 'scatter',
                mode = 'lines',
                line = list(color = 'rgb(0, 0, 0)', width = 3)
                #marker= list(color = 'rgb(0, 0, 0)', size = 10)
) 


fig3 <- fig3 %>% layout(title = "",
                        xaxis = xaxis3,
                        yaxis = yaxis3
);

print(fig3)


#WRITE OUT THE PICTURE
reticulate::py_run_string("import sys");
fileOut = paste(outputRoot,"Oil_History.png",sep="");
save_image(fig3, fileOut, width = 1000, height = 600)


#############################################################################
################## WTI OIL HISTORY (REAL) ###################################
#############################################################################


xaxis4 <- list(title = "",
               showline = TRUE,
               showgrid = FALSE,
               showticklabels = TRUE,
               linecolor = 'rgb(204, 204, 204)',
               linewidth = 2,
               autotick = FALSE,
               tick0=1990,
               dtick = 5,
               ticks = 'outside',
               tickcolor = 'rgb(204, 204, 204)',
               tickwidth = 2,
               #ticklen = 5,
               range=c(1989.99,2024.1),
               tickfont = FONT1 
               
);



yaxis4 <- list(title = list(text = "WTI Oil Price ( Real $)",
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
               dtick = 40.0,
               #ticklabelstep=1,
               showgrid=TRUE,
               gridcolor = 'rgb(204, 204, 204)',
               gridwidth=2,
               zeroline=FALSE,
               #ticksuffix = "  ",
               tickprefix = "  ",
               range=c(-0.0,201),
               tickfont = FONT1,
               font = FONT1
);



fig4 <- plot_ly(QuarterData %>% filter(Symbol =="__WTC_D_REAL"),
                x = ~YearFractional, 
                y = ~SymbolValue, 
                #name = 'High 2014',
                type = 'scatter',
                mode = 'lines',
                line = list(color = 'rgb(0, 0, 0)', width = 3)
                #marker= list(color = 'rgb(0, 0, 0)', size = 10)
) 


fig4 <- fig4 %>% layout(title = "",
                        xaxis = xaxis4,
                        yaxis = yaxis4
);

print(fig4)


#WRITE OUT THE PICTURE
reticulate::py_run_string("import sys");
fileOut = paste(outputRoot,"Oil_History_Real.png",sep="");
save_image(fig4, fileOut, width = 1000, height = 600)

#############################################################################
################## WTI OIL HISTORY (NOM + REAL SUBPLOT) #####################
#############################################################################

fig5 <- subplot(fig3,
                fig4,
                nrows=1,
                margin = 0.05) 
fig5


#############################################################################
################## S&p 500 annotated version ################################
#############################################################################

fig6 = fig;

FONT_ANNOTATION = list(family = 'Arial',
             size = 18,
             color = 'rgb(82, 82, 82)');

annotations1 = 
  list(
        list(  #DOT COM BUBBLE
          x = 2000.0,
          y = 1600,
          text = "Dot Com\nBubble Collapse",
          xref = "x",
          yref = "y",
          showarrow = TRUE,
          arrowhead = 7,
          ax = -20,
          ay = -150,
          font = FONT_ANNOTATION
        ),
        list(  #WORLD FINANCIAL CRISIS
          x = 2008.75,
          y = 1166+200,
          text = "World Finanical\nCrisis",
          xref = "x",
          yref = "y",
          showarrow = TRUE,
          arrowhead = 7,
          ax = 25,
          ay = -170,
          font = FONT_ANNOTATION
        ),
        list(  #COVID
          x = 2020.25,
          y = 2500,
          text = "Coronavirus\nPandemic",
          xref = "x",
          yref = "y",
          showarrow = TRUE,
          arrowhead = 7,
          ax = -0,
          ay = 100,
          font = FONT_ANNOTATION
        )
  );


fig6 = fig6 %>%
      layout(annotations = annotations1)
fig6;

#WRITE OUT THE PICTURE
reticulate::py_run_string("import sys");
fileOut = paste(outputRoot,"SP500_History_Events.png",sep="");
save_image(fig6, fileOut, width = 500, height = 500)

#WRITE OUT THE PICTURE
reticulate::py_run_string("import sys");
fileOut = paste(outputRoot,"SP500_History_Events_Wide.png",sep="");
save_image(fig6, fileOut, width = 700, height = 500)

#############################################################################
################## Oil Price annotated version ##############################
#############################################################################

FONT_ANNOTATION = list(family = 'Arial',
                       size = 20,
                       color = 'rgb(82, 82, 82)');


fig7 = fig3;

annotations2 = 
  list(
    list(  #DOT COM BUBBLE
      x = 1990.75,
      y = 40+3,
      text = "Gulf War",
      xref = "x",
      yref = "y",
      showarrow = TRUE,
      arrowhead = 7,
      ax = 20,
      ay = -80,
      font = FONT_ANNOTATION
    ),
    list(  #2007
      x = 2008.0,
      y = 139,
      text = "2007 Oil Demand\nShock",
      xref = "x",
      yref = "y",
      showarrow = TRUE,
      arrowhead = 7,
      ax = -140,
      ay = 0,
      font = FONT_ANNOTATION
    ),
    list(  #WORLD FINANCIAL CRISIS
      x = 2009,
      y = 40-4,
      text = "Global Finanical\nCrisis",
      xref = "x",
      yref = "y",
      showarrow = TRUE,
      arrowhead = 7,
      ax = -60,
      ay = 80,
      font = FONT_ANNOTATION
    ),
    list(  #OVERSUPPLY
      x = 2015.25,
      y = 80+4,
      text = "Global\nOversupply",
      xref = "x",
      yref = "y",
      showarrow = TRUE,
      arrowhead = 7,
      ax = 40,
      ay = -90,
      font = FONT_ANNOTATION
    ),
    list(  #COVID
      x = 2020.25-0.5,
      y = 20.5,
      text = "Coronavirus\nPandemic",
      xref = "x",
      yref = "y",
      showarrow = TRUE,
      arrowhead = 7,
      ax = -90,
      ay = 0,
      font = FONT_ANNOTATION
    ),
    list(  #UKRAINE
      x = 2022.5+0.0,
      y = 107+4,
      text = "Ukraine\nWar",
      xref = "x",
      yref = "y",
      showarrow = TRUE,
      arrowhead = 7,
      ax = -40,
      ay = -75,
      font = FONT_ANNOTATION
    )
  );

fig7 = fig7 %>%
  layout(annotations = annotations2)

fig7;

#WRITE OUT THE PICTURE
reticulate::py_run_string("import sys");
fileOut = paste(outputRoot,"Oil_History_Events.png",sep="");
save_image(fig7, fileOut, width = 1000, height = 600)

#WRITE OUT THE PICTURE
reticulate::py_run_string("import sys");
fileOut = paste(outputRoot,"Oil_History_Events_Wide.png",sep="");
save_image(fig7, fileOut, width = 700, height = 500)

#############################################################################
################## LOAD DATA ################################################
#############################################################################

fileIn1= "C:/Users/giles/OneDrive/MBA/Dissertation/Data/API_NY.GDP.MKTP.KD.ZG_DS2_en_excel_v2_6298898.xls"

DataIn_1 = read_excel(fileIn1,sheet="Data",skip=2)

#THE COUNTRIES WE FILTER
InterestCountries = c("United States","World")

#FILTER TO THOSE
DataIn_2 = DataIn_1 %>%
  filter(`Country Name` %in% InterestCountries ) %>%
  pivot_longer(cols = ! c("Country Name" ,,  "Country Code","Indicator Name","Indicator Code" )) %>%
  rename(Year= name) %>%
  rename(GDP_Growth = value) %>%
  rename(Country_Name=`Country Name`) %>%
  filter(!is.na(GDP_Growth)) %>%
  mutate(GDP_Growth = as.numeric(GDP_Growth)) %>%
  mutate(Year = as.numeric(Year)) %>%
  ungroup();

#############################################################################
################## GLOBAL SETUP  ############################################
#############################################################################

#FONT FOR LABELS
FONT1 = list(family = 'Arial',
             size = 20,
             color = 'rgb(82, 82, 82)');

#FONT FOR Y AXIS TITLE
FONT2 = list(family = 'Arial',
             size = 24,
             color = 'rgb(82, 82, 82)');

#############################################################################
################## DRAW CHART ###############################################
#############################################################################



xaxis8 <- list(title = "",
               showline = TRUE,
               showgrid = FALSE,
               showticklabels = TRUE,
               linecolor = 'rgb(204, 204, 204)',
               linewidth = 2,
               autotick = FALSE,
               tick0=1990,
               dtick = 5,
               ticks = 'outside',
               tickcolor = 'rgb(204, 204, 204)',
               tickwidth = 2,
               #ticklen = 5,
               range=c(1989.99,2024.1),
               tickfont = FONT1 
               
);



yaxis8 <- list(title = list(text = "Real GDP Growth (%)",
                            font = FONT1),
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
               dtick = 2.0,
               ticklabelstep=1,
               showgrid=TRUE,
               gridcolor = 'rgb(204, 204, 204)',
               gridwidth=2,
               zeroline=FALSE,
               ticksuffix = " ",
               tickprefix = "   ",
               #showtickprefix = TRUE,
               range=c(-4,6.1),
               tickfont = FONT1,
               tickformat="+2",
               font = FONT1
);

#FONT FOR LEGEND
SETUP_LEGEND8 = list(
  font = list(
    family = 'Arial',
    size = 18,
    color = 'rgb(82, 82, 82)'
  ),
  x=0.05,
  y=0.00,
  bordercolor = 'rgba(255, 255, 255,0)',
  borderwidth = 2,
  bgcolor='rgba(0,0,0,0)'
);


fig8 <- plot_ly(DataIn_2 ,
                x = ~Year, 
                y = ~GDP_Growth, 
                linetype = ~Country_Name,
                type = 'scatter',
                mode = 'lines+markers',
                line = list(color = 'rgb(0, 0, 0)', width = 3),
                marker= list(color = 'rgb(0, 0, 0)', size = 7)
) 


fig8 <- fig8 %>% layout(title = "",
                        xaxis = xaxis8,
                        yaxis = yaxis8,
                        legend = SETUP_LEGEND8
);

print(fig8);

###################################################################################
################# OUTPUT FILES   ##################################################
###################################################################################

outputRoot = "C:/Users/giles/OneDrive/MBA/Dissertation/Writeup/Pictures/"

#WRITE OUT THE PICTURE
reticulate::py_run_string("import sys");
fileOut = paste(outputRoot,"GDP_Growth.png",sep="");
save_image(fig8, fileOut, width = 500, height = 500)


#############################################################################
################## DRAW CHART ###############################################
#############################################################################

