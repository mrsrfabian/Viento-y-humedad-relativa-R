# Auto detect text files and perform LF normalization
* text=auto
#Generar el dibujo de barras de viento
wind_barb <- function(x, mlength=0.1, wblength=0.025) {
  
  # Calculate which / how many barbs
  # any triangles (50)
  fif <- floor(x /50)
  # and then look for longer lines for remaining speed (10)
  tn <- floor( (x - fif* 50)/10)
  # and then look for shorter lines for remaining speed (5)
  fv <- floor( (x - fif* 50 - tn* 10)/5)
  
  # Spacing & barb length
  yadj <- 0.5+mlength
  dist <- (yadj-0.5) / 10    
  xadj <- 0.5+wblength
  xfadj <- 0.5+wblength/2        
  
  # Create grobs
  main_grob <- linesGrob(0.5, c(0.5, yadj ))
  
  # 50 windspeed
  if(fif != 0) {
    fify <- c(yadj, yadj-dist*seq_len(2* fif) )
    fifx <- c(0.5, xadj)[rep(1:2, length=length(fify))]
    fif_grob <- pathGrob(fifx, fify, gp=gpar(fill="black"))
  } else {
    fif_grob <- NULL
    fify <- yadj+dist
  }
  
  # Ten windspeed
  if(tn != 0) {
    tny <- lapply(seq_len(tn) , function(x) min(fify) - dist*c(x, x-1))  
    tn_grob <- do.call(gList, 
                       mapply(function(x,y) 
                         linesGrob(x=x, y=y, gp=gpar(fill="black")),
                         x=list(c(0.5, xadj)), y=tny, SIMPLIFY=FALSE))
  } else {
    tn_grob <- NULL
    tny <- fify
  }
  
  # Five windspeed
  if(fv != 0) {
    fvy <- lapply(seq_len(fv) , function(x) min(unlist(tny)) -dist* c(x, x-0.5))
    fv_grob <- do.call(gList, 
                       mapply(function(x,y) 
                         linesGrob(x=x, y=y, gp=gpar(fill="black")),
                         x=list(c(0.5, xfadj)), y=fvy, SIMPLIFY=FALSE))
  } else {
    fv_grob <- NULL
  }    
  
  # Draw    
  #grid.newpage()
  grid.draw(gList(main_grob, fif_grob, tn_grob, fv_grob))
}
#lectura del archivo
dat <- read.csv("viento.csv")
#Librería usadas
library(gridBase)
library(grid)
#Visor gráfico
dev.new()
#Con el archivo csv se plotean los puntos x,y 
#ylim son los vectores de el eje Y de 1000 a 0 (Presión)
#xlim son los vectores de el eje X de 1 a 30 (Días)
#pch es para la figura para mostrar los puntos
#xaxt y yaxt tienen el valor de "none" para que no muestre valores en los ejes
#xlab y ylab tienen el valor "" para que no muestre el nombre de los ejes
#main es el título del diagrama
#sub es el subítulo del diagrama
with(dat, plot(x,y,ylim=c(1000, 0), xlim=c(1, 30), pch=16, xaxt="none",
  yaxt="none",  xlab= "", ylab= "",
  main= "Viento en altura para estación de Guadalajara", 
  sub="Realizado por: Fabián Salazar Vázquez"))
#Colocamos el nombre del eje x "Días"
#side=3 es encima
#line es el tipo de línea
#font es el tipo de fuente
mtext("Días", side=1, line=3, font=1)
#Colocamos el nombre del eje y "Presión (hPa)"
mtext("Presión (hPa)", side=2, line=3)
#Colocamos valores al eje x
#1 es para que se coloque debajo
#seq(0,30,1) es para que sean del 0 al 30 de 1 en 1
#las es para ponerlos perpendicular al eje
#cex.axis es el zoom de los valores
#font es el tipo de fuente
axis(1, seq(0,30,1),las=2, cex.axis=0.8, font=3)
#Colocamos valores al eje y
#2 es para que se coloque a la izquierda
#seq(1000,0,-100) es para que sean del 1000 al 0 de -100 en -100
#las es para ponerlos perpendicular al eje
#cex.axis es el zoom de los valores
#font es el tipo de fuente
axis(2, seq(1000,0,-100),las=2, cex.axis=0.8, font=3)
#with es con y los argumenos
#dat es para llamar al csv antes leído
#text es para que ponga texto en el gráfico
#x y y son los valores ploteados de presión y días
#labels es para que coloque el texto en esos puntos de los valores de razón de mezcla
#cex es el zoom de los valores
#font es el tipo de fuente
#pos es la posición, en este caso junto a los puntos
with(dat, text(x,y,labels = mixr, cex=0.8, font=0, pos=3))
#funciones para plotear las barras
vps <- baseViewports()
#ploteo de las barras
pushViewport(vps$inner, vps$figure, vps$plot)
# Plot
for (i in 1:nrow(dat)) {
  pushViewport(viewport(
    x=unit(dat$x[i], "native"),
    y=unit(dat$y[i], "native"), 
    angle=dat$direction[i]))
  wind_barb(dat$speed[i])
  popViewport()
}
popViewport(3)

