library(shiny)
library(bslib)
library(corrplot)

# Importazione database e creazione costanti globali ----------------------
dati <- read.csv("Seasonal cycle from MERRA2 using base period 1980-2015.csv", skip = 1)
temp_num <- dati$Anomaly
temp_serie <- ts(temp_num, start = c(1880, 1), end = c(2024, 5), frequency = 12)
temp_serie_NA <- ts(temp_num, start = c(1880, 1), end = c(2024, 12), frequency = 12)

load("mauna_loa.Rdata")
load("monte_cimone.Rdata")
loa<-mauna_loa
cimo<-cimone


# Creazione widget --------------------------------------------------------
slider_frequenza = sliderInput("freq",
                               strong("Periodo"),
                               min = 1,
                               max = 60,
                               value = 12,
                               step = 1)

lista_dataset = selectInput("data",
                            label = strong("Dataset"), 
                            choices = list("Temperatura globale" = 1, "Monte Cimone" = 2, "Mauna Loa" = 3), 
                            selected = 1)

tipo_linea = radioButtons("lin",
                          label = strong("Tipo di tratto"), 
                          choices = list("Spezzata" = "l",
                                         "Punti" = "p",
                                         "Spezzata con punti" = "b"), 
                          selected = "l")



# Definizione funzioni ----------------------------------------------------
arcobaleno = function(l,f)
{
 arc <- rainbow(l/f+ceiling((25*12/f)))
 arc <- arc[floor((25*12/f)):length(arc)]
 return(arc)
}

grafico_stag = function(serie, f, titolo, asse_y, leg_labels, ylim_graf, ylim_leg, lin)
{ 
 serie_num <- as.numeric(serie)
 l = length(serie_num)
 colori <- arcobaleno(l, f)
 serie_completata_NA <- c(serie_num, rep(NA, f-(l%%f)))
 M <- matrix(serie_completata_NA, nrow=f)
 M <- ts(M)
 plot(M, 
      plot.type = "single", 
      col = colori,
      main = titolo,
      type = lin,
      xlab = "Tempo (mesi)", 
      ylab = asse_y,
      ylim = ylim_graf)
 if(f!=1) colorlegend(colori, leg_labels, xlim = c(1, f), ylim = ylim_leg,
                      vertical = FALSE,at=c(0.03,0.97), lim.segment = c(0,0))
}


grafico_stag_norm = function(serie, f, titolo, asse_y, leg_labels, ylim_graf, ylim_leg, lin)
{
 serie_num <- as.numeric(serie)
 l <- length(serie_num)
 colori <- arcobaleno(l, f)
 serie_completata_NA <- c(serie_num, rep(NA, f-(l%%f)))
 M <- matrix(serie_completata_NA, nrow=f)
 M <- ts(M)
 medie <- colMeans(matrix(serie_completata_NA,nrow=f))
 M_norm=t(t(M) - medie)
 M_norm <- ts(M_norm)
 plot(M_norm, 
      plot.type = "single", 
      col = colori,
      main = titolo,
      xlab = "Tempo (mesi)", 
      ylab = asse_y,
      type = lin,
      ylim = ylim_graf)
 if(f!=1) colorlegend(colori, leg_labels, xlim = c(1, f), ylim = ylim_leg,
                      vertical = FALSE, at=c(0.03,0.97), lim.segment = c(0,0))
}

# Interfaccia utente ------------------------------------------------------
ui = page_sidebar(

title = "Serie storiche - Ricerca del periodo della stagionalità",

sidebar = sidebar(lista_dataset, slider_frequenza, tipo_linea, position = "right"),

layout_columns(card(plotOutput("periodo")),
               card(plotOutput("periodo_norm")))
)

# Funzione Server ---------------------------------------------------------
server = function(input, output)
{
 output$periodo = renderPlot(
 {
  f <- input$freq
  tipo <- input$lin
  if(input$data==1)
 {
  serie = temp_serie
  titolo_1 = expression("Anomalia temperatura globale annuale media")
  titolo_2 = expression("Anomalia temperatura globale annuale media - normalizzata")
  asse_y = "Anomalia di temperatura (°C)"
  leg_labels = c(1880, 2024)
  ylim_grafico = c(-3.5,3.2)
  ylim_legenda = c(2.7,3.2)
  ylim_grafico_norm = c(-3.5,3.2)
  ylim_legenda_norm = c(2.7,3.2)
 }
 if(input$data==2)
 {
   serie = cimo
   titolo_1 = expression(paste("Concentrazione CO"[2]," Monte Cimone"))
   titolo_2 = expression(paste("Concentrazione CO"[2]," Monte Cimone - normalizzata"))
   asse_y = "Concentrazione (ppm)"
   leg_labels = c(1980, 2021)
   ylim_grafico = c(320,440)
   ylim_legenda = c(430,440)
   ylim_grafico_norm = c(-12,12)
   ylim_legenda_norm = c(10,12)
 }
 if(input$data==3)
 {
   serie = loa
   titolo_1 = expression(paste("Concentrazione CO"[2]," Mauna Loa"))
   titolo_2 = expression(paste("Concentrazione CO"[2]," Mauna Loa - normalizzata"))
   asse_y = "Concentrazione (ppm)"
   leg_labels = c(1958, 2021)
   ylim_grafico = c(310,435)
   ylim_legenda = c(425,435)
   ylim_grafico_norm = c(-8,10)
   ylim_legenda_norm = c(8.5,10)
 }
  
 grafico_stag(serie, f, titolo_1, asse_y, leg_labels, ylim_grafico, ylim_legenda, tipo)
 output$periodo_norm = renderPlot(grafico_stag_norm(serie, f, titolo_2, asse_y, leg_labels, ylim_grafico_norm, ylim_legenda_norm, tipo))
 })
}


# Esecuzione App ----------------------------------------------------------
shinyApp(ui = ui, server = server)

