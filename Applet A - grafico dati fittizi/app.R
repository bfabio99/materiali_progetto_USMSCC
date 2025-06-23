library(shiny)
library(bslib)
library(corrplot)
library(DT)


# Definizione funzioni ----------------------------------------------------
storica = function(serie)
{
  serie <- ts(serie, start = c(2000, 1), end = c(2005, 12), frequency = 12)
  return(serie)
}



# Importazione database e creazione costanti globali ----------------------
t <- c(11.4,11.6,12.44,13.38,14.38,14.96,15.35,15.2,14.53,13.42,12.43,11.68)

arcobaleno <- rainbow(6)
anni <- c("2000","2001","2002","2003","2004","2005")
mesi = c("Gen","Feb","Mar","Apr","Mag","Giu","Lug","Ago","Set","Ott","Nov","Dic")

errori <- matrix(rep(0, 41*6*12), nrow=41)
for(e in 0:40) errori[e+1,] <- runif(6*12, -e/10, e/10)

# Creazione widget --------------------------------------------------------
slider_anomalia = sliderInput("anomalia",
                               "Media di riferimento per l'anomalia",
                               min = -20,
                               max = 20,
                               value = 0,
                               step = 0.1)

tipo_grafico = radioButtons("graf",
                            label = "Grafico", 
                            choices = list("Dati originali" = 1,
                                           "Anomalia" = 2,
                                           "Dati per anno" = 3,
                                           "Anomalia per anno" = 4,
                                           "Dati per anno norm." = 5,
                                           "Anomalia per anno norm." = 6), 
                            selected = 1)

tipo_linea = radioButtons("linea",
                           label = "Tipo di tratto", 
                           choices = list("Spezzata" = "l",
                                          "Punti" = "p",
                                          "Spezzata con punti" = "b"), 
                           selected = "l")

limiti_asse_y = sliderInput("limy",
                            "Limiti asse ordinate",
                            min = -25,
                            max = 50,
                            value = c(8,18),
                            step = 1)

slider_griglia = sliderInput("griglia",
                              "Linee orizzontali",
                              min = 0,
                              max = 5,
                              value = 0,
                              step = 0.1)

slider_k1 = sliderInput("k1",
                        "k1",
                         min = -2,
                         max = 2,
                         value = 0,
                         step = 0.1)

slider_k2 = sliderInput("k2",
                        "k2",
                        min = -2,
                        max = 2,
                        value = 0,
                        step = 0.1)

slider_k3 = sliderInput("k3",
                        "k3",
                        min = -2,
                        max = 2,
                        value = 0,
                        step = 0.1)

slider_k4 = sliderInput("k4",
                        "k4",
                        min = -2,
                        max = 2,
                        value = 0,
                        step = 0.1)

slider_k5 = sliderInput("k5",
                        "k5",
                        min = -2,
                        max = 2,
                        value = 0,
                        step = 0.1)

slider_errore = sliderInput("err",
                            "Semiampiezza errore",
                            min = 0,
                            max = 4,
                            value = 0,
                            step = 0.1)

# Interfaccia utente ------------------------------------------------------
ui = fluidPage(
  
titlePanel(h3("Serie storiche - Temperature e anomalie")),
    
sidebarPanel(width = 2,
             h4("Parametri Dati"),
             slider_k1,
             slider_k2,
             slider_k3,
             slider_k4,
             slider_k5,
             slider_errore
            ),
   
mainPanel(navbarPage(title = "",
                     tabPanel("Dati",dataTableOutput("tabella")),
                     tabPanel("Grafico", plotOutput("grafico", height = "650px")),
                     selected = "Grafico"
                    ),
          width = 8
         ),
   
sidebarPanel(width = 2,
             h4("Parametri Grafico"),
             tipo_grafico,
             slider_anomalia,
             tipo_linea,
             limiti_asse_y,
             slider_griglia
            )
)


# Funzione Server ---------------------------------------------------------
server = function(input, output)
{
 output$grafico = renderPlot(
 {
  g <- as.numeric(input$graf)
  a <- input$anomalia
  var <- c(input$k1,input$k2,input$k3,input$k4,input$k5)
  T <- t
  temp_num <- T
  
  for(j in 1:5)
  {
   T = T + var[j]
   temp_num = c(temp_num, T)
  }
  
  l <- length(temp_num)
  e <- input$err
  temp_num <- temp_num + errori[10*e+1,]
  
  temp_serie <- storica(temp_num)
  temp_table <- ts(matrix(temp_serie, nrow=12), start=c(2000,2001,2002,2003,2004,2005), frequency = 12)
  
  
  if(g==1) dati <- temp_serie
  if(g==2) dati <- temp_serie - rep(a,l)
  if(g==3 || g==5) dati <- ts(matrix(temp_serie, nrow=12))
  if(g==4 || g==6) dati <- ts(matrix(temp_serie - rep(a,l), nrow=12))
  if(g==5) dati <- ts(t(t(dati) - colMeans(dati)))
  if(g==6) dati <- ts(t(t(dati) - colMeans(dati)))
    
  plot(dati, 
       plot.type = "single", 
       col = arcobaleno,
       main = NULL,
       type = input$linea,
       xlab = "Tempo", 
       ylab = "Temperatura (°C)",
       ylim = input$limy,
       lwd = 2
       )
  
  grl <- input$griglia
  if(grl>0) for(k in seq(-60,60,grl)) abline(h=k, lwd=0.3, lty = 2, col = "gray") 
  
  if(g==3 || g==4 || g==5 || g==6)
  {
   legend("topleft",
         title = "Legenda",
         legend = rev(anni),
         fill = rev(arcobaleno),
         horiz = FALSE,
         cex = 1,
         inset = 0.01)  
  }
  
})
  
  output$tabella = renderDT(
  {
   var <- c(input$k1,input$k2,input$k3,input$k4,input$k5)
   T <- t
   temp_num <- T
    
   for(j in 1:5)
   {
    T = T + var[j]
    temp_num = c(temp_num, T)
   }
   
   l <- length(temp_num)
   e <- input$err
   temp_num <- temp_num + errori[10*e+1,]
   
   temp_serie <- storica(temp_num)
   temp_table <- ts(matrix(temp_serie, nrow=12), start=c(2000,2001,2002,2003,2004,2005), frequency = 12)
   
   datatable(temp_table,
             colnames = anni,
             rownames = mesi,
             options = list(pageLength = 12,
                            lengthChange = FALSE,
                            searching = FALSE,
                            paging = FALSE,
                            ordering = FALSE,
                            info = FALSE)
             )
   })
}


# Esecuzione App ----------------------------------------------------------
shinyApp(ui = ui, server = server)

