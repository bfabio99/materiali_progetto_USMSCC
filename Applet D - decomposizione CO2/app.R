library(shiny)
library(bslib)
library(corrplot)


# Importazione database e creazione costanti globali ----------------------
load("mauna_loa.Rdata")
load("monte_cimone.Rdata")
loa<-mauna_loa
cimo<-cimone
loa_num<-as.numeric(loa)
cimo_num<-as.numeric(cimo)


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

checkbox_trend = checkboxGroupInput("trend",
                                    strong("Visualizza Trend"),
                                    choices = list("Trend Monte Cimone" = 1,
                                                   "Trend Mauna Loa" = 2),
                                    selected = 1
                                   )

checkbox_osservati = checkboxGroupInput("oss",
                                   strong("Visualizza Dati"),
                                   choices = list ("Dati Monte Cimone" = 1,
                                                   "Dati Mauna Loa" = 2),
                                   selected = 1)



# Definizione funzioni ----------------------------------------------------
grafico_trend = function(dati, colori, spessore, tipo_linea, leg_labels)
{
  plot(dati,
       plot.type = "single",
       main =  "Dati osservati / Trend",
       xlab = "Anno",
       ylab = "Concentrazione (ppm)",
       col = colori,
       lwd = spessore,
       lty = tipo_linea)
  legend("topleft",
         title = "Legenda",
         legend = leg_labels,
         fill = colori,
         horiz = FALSE,
         cex = 1) 
}

# Interfaccia utente ------------------------------------------------------
ui = page_sidebar(

title = "Trend a confronto - Serie storiche concentrazione anidride carbonica Monte Cimone e Mauna Loa",

sidebar = sidebar(slider_frequenza, slider_frequenza_mobile,  checkbox_osservati, checkbox_trend,  position = "right"),

fluidPage(
          layout_columns(
                         card(plotOutput("periodo_cimo")),
                         card(plotOutput("periodo_norm_cimo")),
                         card(plotOutput("stag_cimo"))
                        ),
          layout_columns(
                         card(plotOutput("periodo_loa")),
                         card(plotOutput("periodo_norm_loa")),
                         card(plotOutput("stag_loa"))
                        ),
          card(plotOutput("dati_trend"), height = "600px"),
          
                         card(plotOutput("detrendizzato_cimo")),
                         card(plotOutput("detrendizzato_loa")),
          
          layout_columns(
                         card(plotOutput("decomposta_cimo")),
                         card(plotOutput("decomposta_loa")),
                        )
        )
)

# Funzione Server ---------------------------------------------------------
server = function(input, output)
{
 
 output$periodo_cimo = renderPlot(
 {
   f <- input$freq
   colori_arc <- rainbow(length(cimo)/f+ceiling((25*12/f)))
   colori_arc_cimo <- colori_arc[floor((25*12/f)):length(colori_arc)]
   cimo_num_NA <- c(cimo_num, rep(NA, f-(length(cimo_num)%%f)))
   M <- matrix(cimo_num_NA, nrow=f)
   M_cimo <- ts(M)
   plot(M_cimo, 
        plot.type = "single", 
        col = colori_arc_cimo,
        main = "Sudd. per periodi - Monte Cimone",
        type = "l",
        xlab = "Tempo (mesi)", 
        # xaxt = "n",
        ylim = c(320,440),
        ylab = "Concentrazione (ppm)")
   if(f!=1) colorlegend(colori_arc_cimo, c(1979, 2021), xlim = c(1, f), ylim = c(430,440),
                        vertical=FALSE, at=c(0.05,0.95), lim.segment = c(0,0))
 
   
   output$periodo_loa = renderPlot(
     {
       colori_arc <- rainbow(length(cimo)/f+ceiling((25*12/f)))
       colori_arc_loa <- colori_arc[floor((25*12/f)):length(colori_arc)]
       loa_num_NA <- c(loa_num, rep(NA, f-(length(loa_num)%%f)))
       M <- matrix(loa_num_NA, nrow=f)
       M_loa <- ts(M)
       plot(M_loa, 
            plot.type = "single", 
            col = colori_arc_loa,
            main = "Sudd. per periodi - Mauna Loa",
            type = "l",
            xlab = "Tempo (mesi)", 
            # xaxt = "n",
            ylim = c(310,435),
            ylab = "Concentrazione (ppm)")
       if(f!=1) colorlegend(colori_arc_loa, c(1958, 2021), xlim = c(1, f), ylim = c(425,435),
                            vertical=FALSE, at=c(0.05,0.95), lim.segment = c(0,0))  
   
   
 output$periodo_norm_cimo = renderPlot(
   {
     cimo_medie=colMeans(matrix(cimo_num_NA,nrow=f))
     M_norm=t(t(M_cimo) - cimo_medie)
     M_norm_cimo <- ts(M_norm)
     plot(M_norm_cimo, 
          plot.type = "single", 
          col = colori_arc_cimo,
          main = "Sudd. per periodi (norm.) - Monte Cimone",
          xlab = "Tempo (mesi)", 
          # xaxt = "n",
          ylim = c(-12,12),
          ylab = "Concentrazione (ppm)")
     if(f!=1) colorlegend(colori_arc_cimo, c(1979, 2021), xlim = c(1, f), ylim = c(10,12),
                          vertical=FALSE, at=c(0.05,0.95), lim.segment = c(0,0))

     
 output$periodo_norm_loa = renderPlot(
   {
     loa_medie=colMeans(matrix(loa_num_NA,nrow=f))
     M_norm=t(t(M_loa) - loa_medie)
     M_norm_loa <- ts(M_norm)
     plot(M_norm_loa, 
          plot.type = "single", 
          col = colori_arc_loa,
          main = "Sudd. per periodi (norm.) - Mauna Loa",
          xlab = "Tempo (mesi)", 
          # xaxt = "n",
          ylim = c(-8,10),
          ylab = "Concentrazione (ppm)")
     if(f!=1) colorlegend(colori_arc_loa, c(1958, 2021), xlim = c(1, f), ylim = c(8.5,10),
                          vertical=FALSE, at=c(0.05,0.95), lim.segment = c(0,0))
 
 
 output$dati_trend = renderPlot(
   {
     f1 <- input$freq_mm
     if(f1==0) pesi<-c(1) else pesi <- c(1/2, rep(1,f1*2-1), 1/2)/(f1*2)
     cimo_filter <- filter(cimo, pesi)
     loa_filter <- filter(loa, pesi)
     box_oss <- input$oss
     box_trend <- input$trend
     l <- length(box_trend)
     L <- length(box_oss)
     
     if(l==0)
     {
      if(L==1)
      {
       if(box_oss==1) grafico_trend(cimo, "blue", 0.7, 2, "Osservati Monte Cimone") 
       if(box_oss==2) grafico_trend(loa, "brown", 0.7, 2, "Osservati Mauna Loa") 
      }
       if(L==2) grafico_trend(cbind(cimo, loa), c("blue","brown"), c(0.7,0.7), c(2,2), c("Osservati Monte Cimone","Osservati Mauna Loa")) 
     }
     
     if(l==1)
     {
      if(L==0 && box_trend==1) grafico_trend(cimo_filter, "darkgreen", 2, 1, "Trend Monte Cimone")
      if(L==0 && box_trend==2) grafico_trend(loa_filter, "red", 2, 1, "Trend Mauna Loa") 
      if(L==1)
      {
       if(box_trend==1)
       {
        if(box_oss==1) grafico_trend(cbind(cimo, cimo_filter), c("blue","darkgreen"), c(0.7,2), c(2,1), c("Osservati Monte Cimone", "Trend Monte Cimone"))
        if(box_oss==2) grafico_trend(cbind(loa, cimo_filter), c("brown","darkgreen"), c(0.7,2), c(2,1), c("Osservati Mauna Loa", "Trend Monte Cimone"))
       }
      if(box_trend==2)
      {
       if(box_oss==2) grafico_trend(cbind(loa, loa_filter), c("brown","red"), c(0.7,2), c(2,1), c("Osservati Mauna Loa", "Trend Mauna Loa"))
       if(box_oss==1) grafico_trend(cbind(cimo, loa_filter), c("blue","red"), c(0.7,2), c(2,1), c("Osservati Monte Cimone", "Trend Mauna Loa"))
      }
      }
       if(L==2) grafico_trend(cbind(cimo, loa, cimo_filter), c("blue","brown","darkgreen"), c(0.7,0.7,2), c(2,2,1), c("Osservati Monte Cimone", "Osservati Mauna Loa", "Trend Monte Cimone"))
     }
       
     if(l==2)
     {
      if(L==0) grafico_trend(cbind(cimo_filter,loa_filter), c("darkgreen","red"), c(2,2), c(1,1), c("Trend Monte Cimone","Trend Mauna Loa"))
      if(L==1)
      {
       if(box_oss==1) grafico_trend(cbind(cimo, cimo_filter, loa_filter), c("blue","darkgreen","red"), c(0.7,2,2), c(2,1,1), c("Osservati Monte Cimone", "Trend Monte Cimone", "Trend Mauna Loa"))
       if(box_oss==2) grafico_trend(cbind(cimo_filter, loa, loa_filter), c("darkgreen","brown","red"), c(2,0.7,2), c(1,2,1), c("Trend Monte Cimone","Osservati Mauna Loa", "Trend Mauna Loa"))
      }
       if(L==2) grafico_trend(cbind(cimo, cimo_filter, loa, loa_filter), c("blue","darkgreen","brown","red"), c(0.7,2,0.7,2), c(2,1,2,1), c("Osservati Monte Cimone", "Trend Monte Cimone","Osservati Mauna Loa", "Trend Mauna Loa"))
     }
     
   output$detrendizzato_cimo = renderPlot(
   {
     plot(ts(as.numeric(cimo)-as.numeric(cimo_filter),start=1980, frequency=12),
          plot.type = "single",
          main =  "Dati detrendizzati - Monte Cimone",
          xlab = "Anno",
          ylab = "Concentrazione (ppm)",
          col = "blue",
          lwd = 1,
          lty = 2,
          ylim = c(-11,10)) 
     
     
     output$detrendizzato_loa = renderPlot(
       {
         plot(ts(as.numeric(loa)-as.numeric(loa_filter),start=1958, frequency=12),
              plot.type = "single",
              main =  "Dati detrendizzati - Mauna Loa",
              xlab = "Anno",
              ylab = "Concentrazione (ppm)",
              col = "brown",
              lwd = 1,
              lty = 2,
              ylim = c(-5,5)) 
     
     
   output$stag_cimo = renderPlot(
     {
       cimo_cut <- as.numeric(cimo)
       cimo_cut <- ts(cimo_cut[13:346], start = c(1980,1), end = c(2007,10), frequency = 12)
       cimo_filter <- as.numeric(cimo_filter)
       cimo_filter <- ts(cimo_filter[13:346], start = c(1980,1), end = c(2007,10), frequency = 12)
       cimo_detrend <- as.numeric(cimo_cut - cimo_filter)
       cimo_detrend <- c(cimo_detrend, rep(NA, f-(length(cimo_detrend)%%f)))
       M_detrend_cimo <- matrix(cimo_detrend, nrow = f)
       cimo_medie_detrend <- rep(0,f)
       for (i in 1:f)
       {
         cimo_medie_detrend[i] <- mean(na.omit(M_detrend_cimo[i,])) 
       }
       cimo_medie_detrend <- ts(cimo_medie_detrend)
       plot(cimo_medie_detrend,
            main = "Componente stagionale - Monte Cimone",
            xlab = "Tempo (mesi)",
            ylab = "Concentrazione (ppm)",
            col = "red",
            type = "l",
            # ylim = c(-12,12),
            cex = 0.8)
         
         
   output$stag_loa = renderPlot(
     {
       loa_cut <- as.numeric(loa)
       loa_cut <- ts(loa_cut[85:768], start = c(1965,12), end = c(2021,12), frequency = 12)
       loa_filter <- as.numeric(loa_filter)
       loa_filter <- ts(loa_filter[85:768], start = c(1965,12), end = c(2021,12), frequency = 12)
       loa_detrend <- as.numeric(loa_cut - loa_filter)
       loa_detrend <- c(loa_detrend, rep(NA, f-(length(loa_detrend)%%f)))
       M_detrend_loa <- matrix(loa_detrend, nrow = f)
       loa_medie_detrend <- rep(0,f)
       for (i in 1:f)
       {
         loa_medie_detrend[i] <- mean(na.omit(M_detrend_loa[i,])) 
       }
       loa_medie_detrend <- ts(loa_medie_detrend)
       plot(loa_medie_detrend,
            main = "Componente stagionale - Mauna Loa",
            xlab = "Tempo (mesi)",
            ylab = "Concentrazione (ppm)",
            col = "red",
            type = "l",
            # ylim = c(-8,10),
            cex = 0.8)     
       

  output$decomposta_cimo = renderPlot(
   {
     cimo_detrend <- cimo_detrend[1:length(cimo_cut)]
     cimo_medie_detrend <- rep(as.numeric(cimo_medie_detrend), ceiling(length(cimo_num)/f))
     cimo_medie_detrend <- cimo_medie_detrend[13:length(cimo_filter)]
     cimo_medie_detrend <- ts(cimo_medie_detrend, start = c(1980,1), frequency = 12)
     
     err <- cimo_detrend - as.numeric(cimo_medie_detrend)[1:length(cimo_detrend)]
     err <- ts(err, start = c(1980,1), frequency = 12)
     
     Osservati <- cimo
     Trend <- cimo_filter
     Stagionalità <- cimo_medie_detrend
     Errore <- err
     
     plot(cbind(Osservati, Trend, Stagionalità, Errore),
          plot.type = "multiple",
          main = "Decomposizione additiva - Monte Cimone",
          xlab = "Anno",
          ylab = "Concentrazione (ppm)",
          col = "black")
   
   
 output$decomposta_loa = renderPlot(
   {
     loa_detrend <- loa_detrend[1:length(loa_cut)]
     loa_medie_detrend <- rep(as.numeric(loa_medie_detrend), ceiling(length(loa_num)/f))
     loa_medie_detrend <- loa_medie_detrend[85:length(loa_num)]
     loa_medie_detrend <- ts(loa_medie_detrend, start = c(1965,12), frequency = 12)
     
     err <- loa_detrend - as.numeric(loa_medie_detrend)[1:length(loa_detrend)]
     err <- ts(err, start = c(1965,12), frequency = 12)
     
     Osservati <- loa
     Trend <- loa_filter
     Stagionalità <- loa_medie_detrend
     Errore <- err
     
     plot(cbind(Osservati, Trend, Stagionalità, Errore),
          plot.type = "multiple",
          main = "Decomposizione additiva - Mauna Loa",
          xlab = "Anno",
          ylab = "Concentrazione (ppm)",
          col = "black")
     })  
    })
  })
   })     
   })
   })
   })
   })
   })
   })
   }) 
}


# Esecuzione App ----------------------------------------------------------
shinyApp(ui = ui, server = server)

