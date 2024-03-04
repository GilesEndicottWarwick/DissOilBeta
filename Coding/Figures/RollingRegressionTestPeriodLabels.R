
#CREATE THE BASIC CHARTS
fig0C <- plot_ly(data=ChartData0_Energy,
                 x = ~start, 
                 y = ~estimate, 
                 linetype = ~Model,
                 type = 'scatter',
                 mode = 'lines',
                 name = "OLS",
                 line = list(color = 'rgb(0, 0, 0)', 
                             width = 2,
                             dash="solid")
                 #marker= list(color = 'rgb(0, 0, 0)', size = 10)
) %>% add_trace( 
  data = ChartData0_Energy_ROB,
  x = ~start, 
  y = ~estimate, 
  type = "scatter",
  mode = "lines",
  name = "MM Robust Reg.",
  line = list(color = 'rgb(0, 0, 0)', 
              width = 2,
              dash="dash")
) %>% add_trace( 
  data = ChartData0_Energy_QR,
  x = ~start, 
  y = ~estimate, 
  type = "scatter",
  mode = "lines",
  name = "Quantile Reg.",
  line = list(color = 'rgb(0, 0, 0)', 
              width = 2,
              dash="dot")
)

tickValsIn = c(ymd("19920101"),ymd("19960101"),ymd("20000101"))

xaxis0C = list(title = "Regression Period",
                showline = FALSE,
                showgrid = FALSE,
               showticklabels = TRUE,
               # linecolor = 'rgb(204, 204, 204)',
               #linewidth = 2,
               #  autotick = FALSE,
               # tick0=1990,
               # dtick = "M48",
               # ticks = 'outside',
                tickcolor = 'rgb(204, 204, 204)',
                 tickwidth = 2,
               ticklen = 5,
               #range=c(,2024.1),
                tickfont = FONT1 ,
               # tickformat = "%Y",
               #ticksuffix = "",
               tickvals = c(ymd("19920101"),ymd("19960101"),ymd("20000101"),ymd("20040101"),ymd("20080101"),ymd("20120101"),ymd("20160101"),ymd("20200101")),
               ticktext = c("1992\n-\n1994","1996\n-\n1998","2000\n-\n2002","2004\n-\n2006","2008\n-\n2010","2012\n-\n2014","2016\n-\n2018","2020\n-\n2022"),
               ticksuffix = ""
               
);

fig0C = fig0C %>%
  layout(
    xaxis  = xaxis0C,
    yaxis  = yaxis0A,
    legend = SETUP_LEGEND_A
  );

fig0C
