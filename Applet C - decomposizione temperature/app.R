library(shiny)
library(bslib)
library(corrplot)

# Importazione database e creazione costanti globali ----------------------
dati <- read.csv("Seasonal cycle from MERRA2 using base period 1980-2015.csv", skip = 1)
temp_num <- dati$Anomaly
temp_serie <- ts(temp_num, start = c(1880, 1), end = c(2024, 5), frequency = 12)
temp_serie_NA <- ts(temp_num, start = c(1880, 1), end = c(2024, 12), frequency = 12)

# Creazione widget --------------------------------------------------------
slider_frequenza = sliderInput("freq",
                               strong("Periodo"),
                               min = 1,
                               max = 60,
                               value = 12,
                               step = 1)

slider_frequenza_mobile = sliderInput("freq_mm",
                                      strong("Semiampiezza media mobile"),
                                      min = 0,
                                      max = 60,
                                      value = 6,
                                      step = 1)


# Interfaccia utente ------------------------------------------------------
ui = page_sidebar(

title = "Trend, stagionalità e decomposizione additiva - Serie storica anomalia temperatura globale 1880/2024",

sidebar = sidebar(slider_frequenza, slider_frequenza_mobile,  position = "right"),

fluidPage(
          layout_columns(
                          card(plotOutput("periodo")),
                          card(plotOutput("periodo_norm")),
                          card(plotOutput("stag"))
                         ),
          card(plotOutput("dati_trend"), height = "600px"),
          card(plotOutput("detrend"), height = "600px"),
          card(plotOutput("decomposta"), height = "600px"),
        )
)

# Funzione Server ---------------------------------------------------------
server = function(input, output)
{
 
 output$periodo = renderPlot(
 {
   f <- input$freq
   colori_arc <- rainbow(length(temp_serie_NA)/f+ceiling((25*12/f)))
   colori_arc <- colori_arc[floor((25*12/f)):length(colori_arc)]
   temp_num_NA <- c(temp_num, rep(NA, f-(length(temp_num)%%f)))
   M <- matrix(temp_num_NA, nrow=f)
   M <- ts(M)
   plot(M, 
        plot.type = "single", 
        col = colori_arc,
        main = "Suddivisione per periodi",
        type = "l",
        xlab = "Tempo (mesi)", 
        ylim = c(-3.5,3.2),
        ylab = "Anomalia di temperatura (°C)")
   if(f!=1) colorlegend(colori_arc, c(1880, 2024), xlim = c(1, f), ylim = c(2.7,3.2),
                        vertical=FALSE, at=c(0.05,0.95), lim.segment = c(0,0))
 
 output$periodo_norm = renderPlot(
   {
     temp_medie=colMeans(matrix(temp_num_NA,nrow=f))
     M_norm=t(t(M) - temp_medie)
     M_norm <- ts(M_norm)
     plot(M_norm, 
          plot.type = "single", 
          col = colori_arc,
          main = "Suddivisione per periodi (normalizzata)",
          xlab = "Tempo (mesi)", 
          ylim = c(-3.5,3.2),
          ylab = "Anomalia di temperatura (°C)")
     if(f!=1) colorlegend(colori_arc, c(1880, 2024), xlim = c(1, f), ylim = c(2.7,3.2),
                          vertical=FALSE, at=c(0.05,0.95), lim.segment = c(0,0))
  
 
 
 output$dati_trend = renderPlot(
   {
     f <- input$freq
     f1 <- input$freq_mm
     if(f1==0) pesi<-c(1) else pesi <- c(1/2, rep(1,f1*2-1), 1/2)/(f1*2)
     temp_filter <- filter(temp_serie, pesi)
     plot(cbind(temp_filter, temp_serie),
          plot.type = "single",
          main =  "Dati osservati con trend",
          xlab = "Anno",
          ylab = "Anomalia di temperatura (°C)",
          ylim = c(-3.2,2.5),
          col = c("red","blue"),
          lwd = c(2,0.7),
          lty = c(1,2))
     legend("topleft",
            title = "Legenda",
            legend = c( "Dati osservati", "Trend"),
            fill = c("blue","red"),
            horiz = FALSE,
            cex = 0.8)
 
 
 
 output$stag = renderPlot(
   {
     temp_detrend <- as.numeric(temp_serie - temp_filter)
     temp_detrend <- c(temp_detrend, rep(NA, f-(length(temp_detrend)%%f)))
     M_detrend <- matrix(temp_detrend, nrow = f)
     temp_medie_detrend <- rep(0,f)
     for (i in 1:f)
     {
       temp_medie_detrend[i] <- mean(na.omit(M_detrend[i,])) 
     }
     temp_medie_detrend <- ts(temp_medie_detrend)
     plot(temp_medie_detrend,
          main = "Componente stagionale",
          xlab = "Tempo (mesi)",
          ylab = "Anomalia di temperatura (°C)",
          col = "red",
          type = "l",
          cex = 0.8)
     
   
     output$detrend = renderPlot(
       {
         plot(ts(temp_detrend, start = c(1880, 1), frequency = 12),
              plot.type = "single",
              main =  "Dati detrendizzati",
              xlab = "Anno",
              ylab = "Anomalia di temperatura (°C)",
              ylim = c(-2.5,2.5),
              col = "blue",
              lwd = 0.7,
              lty = 2)

         
 
 output$decomposta = renderPlot(
   {
     temp_medie_detrend <- rep(as.numeric(temp_medie_detrend), ceiling(length(temp_num)/f))
     temp_medie_detrend <- temp_medie_detrend[1:length(temp_serie)]
     temp_medie_detrend <- ts(temp_medie_detrend, start = 1880, frequency = 12)
     
     err <- temp_detrend - as.numeric(temp_medie_detrend)[1:length(temp_detrend)]
     err <- ts(err, start = 1880, frequency = 12)
    
     Osservati <- temp_serie
     Trend <- temp_filter
     Stagionalità <- temp_medie_detrend
     Errore <- err
     
     plot(cbind(Osservati, Trend, Stagionalità, Errore),
          plot.type = "multiple",
          main = "Decomposizione additiva",
          xlab = "Anno",
          ylab = "Anomalia di temperatura (°C)",
          col = "black")
   })
   })
   })
   })
   })
}) 
}


# Esecuzione App ----------------------------------------------------------
shinyApp(ui = ui, server = server)

