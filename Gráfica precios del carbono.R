
rm(list=ls()) # borrar todo en la memoria
options(digits = 10) # decimales
options(scipen = 100) # para evitar notacion scientifica que afectaba la leyenda de la grafica
knitr::opts_chunk$set(echo = FALSE) # Para que predeterminadamente no salga el output de los codigos.

#######################
####  Directorio  #####
#######################
#### (Cambiar solo despues del "~/ a donde este guardado la carpeta) 
knitr::opts_knit$set(root.dir = "~/Desktop/Mercado Carbono")  # Directorio

######################

knitr::opts_chunk$set(out.width = '100%', dpi=300) # Resolucion y tamanio de figuras
knitr::opts_chunk$set(fig.env="figure") # Latex figure environment

# tinytex::install_tinytex() # Por si no está instalado o acualizado correr esta linea.




# Para instalar paquetes que no esten descargados que sean necesarios
lista.de.paquete1 <- c("tidyverse","stargazer", "lubridate", "patchwork")
nuevo.paquete1 <- lista.de.paquete1[!(lista.de.paquete1 %in% 
                                             installed.packages()[,"Package"])]
if(length(nuevo.paquete1)) install.packages(nuevo.paquete1, dependencies = TRUE, force = TRUE)



{r Paquetes reequeridos, include=FALSE}
library(dplyr)
library(ggplot2)




P_c <- read.csv("DatosPIBPrecio.csv")
nrow(P_c)
P_c <- P_c[1:169,]
head(P_c)
tail(P_c)
summary(P_c)
str(P_c)




P_c$Fecha = as.Date(P_c$Fecha, format = "%Y-%m-%d") # convert date, to date format
P_c$Anio <- as.factor(P_c$Anio)
P_c$California_Quebec <- gsub("\\$","", P_c$California_Quebec)
P_c$California_Quebec <- as.numeric(P_c$California_Quebec)

str(P_c)
typeof(P_c$Fecha)
typeof(P_c$Anio)




str(P_c)




P_cG <- ggplot(P_c) + geom_point(aes(x = Fecha, y = UE, size = PIB_UE, color = "UE"), alpha = 0.4) +
       geom_point(aes(x = Fecha, y = California_Quebec, size = PIB_CaliforniaQuebec, color = "CALQUE"), alpha = 0.4) +
       geom_point(aes(x = Fecha, y = RGGI, size = PIB_RGGI, color = "RGGI"), alpha = 0.4) +
       geom_point(aes(x = Fecha, y = Corea, size = PIB_Corea, color = "Corea"), alpha = 0.4) +
       geom_point(aes(x = Fecha, y = NuevaZelanda, size = PIB_NuevaZelanda, color = "NuevaZelanda"), alpha = 0.4)




P_cG <- P_cG + 
       scale_color_manual(name = "Jurisdicción", values = c(UE = "Red", CALQUE = "Orange", RGGI = "Blue", Corea = "DarkGreen", NuevaZelanda = "Black")) + scale_size_continuous(name = "PIB")





P_cG <- P_cG + theme_minimal()



P_cG




# Agregamos una linea que representa el inicio de la jornada de sana distancia:
P_cG <- P_cG +
       geom_vline(aes(xintercept = as.numeric(as.Date("2020-03-23")), linetype = "SanaDistancia"))


# luego otra linea que representa llegada de vacunas y disminucion de casos
P_cG <- P_cG +
       geom_vline(aes(xintercept = as.numeric(as.Date("2021-01-20")), linetype = "PrimerasVacunas"))


# Agregamos una ultima linea representando el inicio de la tercera ola:
P_cG <- P_cG +
       geom_vline(aes(xintercept = as.numeric(as.Date("2021-07-01")), linetype = "TerceraOla"))


# Protocolo de Kyoto entró en vigor
P_cG <- P_cG +
       geom_vline(aes(xintercept = as.numeric(as.Date("2015-02-16")), linetype = "PKyotoVigor"))

# Fecha de firma acuerdo de Paris
P_cG <- P_cG +
       geom_vline(aes(xintercept = as.numeric(as.Date("2016-12-04")), linetype = "TParisVigor"))

# Agregamos una leyenda para las fechas importantes:

P_cG <- P_cG +
       scale_linetype_manual(name = "Fechas Importantes", values =
                                    c(SanaDistancia = 6,
                                      PrimerasVacunas = 2,
                                      TerceraOla = 1, PKyotoVigor = 4,
                                      TParisVigor = 3))





P_cG




P_cG <- P_cG +   
       xlab("Año") + # nombre de eje x
       ylab("Precio (USD/tCO2e)") + # nombre de eje y
       labs(caption = "Elaborado por WRI México con datos de ICAP, Banco Mundial, rggi, CARB, bea y Quebec International") +
       ggtitle("Precios del carbono en el mundo y PIB") + # titulo de grafica
       theme(axis.title.x = element_text(color="Black", size=10), # tamanio y color de titulo del eje x
             axis.title.y = element_text(color = " Black", size=10), # tamanio y color del titulo del eje y
             
             axis.text.x = element_text(size=10), # tamanio y color de los numeros del eje x
             axis.text.y = element_text(size=10),# tamanio y color de los numeros del eje y
             legend.key.height = unit(0.5, "cm"),
             legend.title = element_text(size=8), # tamanio del titulo del indice
             legend.text = element_text(size=6), # tamanio del texto del indice
             legend.position = "right",
             legend.justification = c(1,1), # posicion de inidice
             
             plot.title = element_text(color="DarkBlue", # color del titulo de grafica
                                       size=15, # tamanio del titulo de grafica
                                       family="Courier"))  # tipo de letra del titulo de grafica




P_cG


