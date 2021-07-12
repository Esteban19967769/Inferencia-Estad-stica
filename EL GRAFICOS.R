
# UNIVERSIDADES -----------------------------------------------------------


library(readr)
library(haven)
ACUMULADOS <- read_dta("UNIVERSIDADES.dta")
View(ACUMULADOS)

View(ACUMULADOS)

dimnames(ACUMULADOS)
str(ACUMULADOS)

library(ggplot2)
library(dplyr)
library(plotly)
library(hrbrthemes)

#Crear base con solo mrun y fecha obtencion titulo
#Crear nueva variable que cuente en uno cada fila
ACUMULADOS <- ACUMULADOS %>% 
  mutate(Cantidad = 1)

#reemplazar 2020 por 2019 en titulados
ACUMULADOS$fecha_obtencion_titulo[ACUMULADOS$fecha_obtencion_titulo == 2020] <- 2019

#me quedo con tres variables
series_tiempoUNI <- ACUMULADOS[,c(2,7,16)]
#creo un dataframe con la cantidad de gente titulada por añ0
UNIVERSIDAD <- aggregate(Cantidad ~ fecha_obtencion_titulo, series_tiempoUNI, sum)

# plot
ggplot(data = UNIVERSIDAD , aes(x = fecha_obtencion_titulo, y = Cantidad)) + 
  geom_line(color="red") +
  geom_point(size=4) +
  labs(title = "Cantidad de titulados en Universidades - Chile", 
       subtitle = "Años 2007 a 2019",
       x = "Año de titulación",
       y = "Cantidad de personas tituladas")

#################################################################


# CFT ---------------------------------------------------------------------

library(readxl)
ACUMULADOSCFT <- read_excel("ACUMULADOSCFT.xlsx")
View(ACUMULADOSCFT)

#Crear base con solo mrun y fecha obtencion titulo
#Crear nueva variable que cuente en uno cada fila
ACUMULADOSCFT <- ACUMULADOSCFT %>% 
  mutate(Cantidad = 1)

#reemplazar 2020 por 2019 en titulados
ACUMULADOSCFT$FECHA_OBTENCION_TITULO[ACUMULADOSCFT$FECHA_OBTENCION_TITULO == 2020] <- 2019

#me quedo con tres variables
series_tiempoCFT <- ACUMULADOSCFT[,c(2,7,16)]
#creo un dataframe con la cantidad de gente titulada por añ0
CFT <- aggregate(Cantidad ~ FECHA_OBTENCION_TITULO, series_tiempoCFT, sum)

# plot
ggplot(data = CFT , aes(x = FECHA_OBTENCION_TITULO, y = Cantidad)) + 
  geom_line(color="blue") +
  geom_point(size=4) +
  labs(title = "Cantidad de titulados en CFT - Chile", 
       subtitle = "Años 2007 a 2019",
       x = "Año de titulación",
       y = "Cantidad de personas tituladas")


# IP ----------------------------------------------------------------------


library(readxl)
ACUMULADOSIP <- read_excel("ACUMULADOSIP.xlsx")
View(ACUMULADOSIP)

#Crear base con solo mrun y fecha obtencion titulo
#Crear nueva variable que cuente en uno cada fila
ACUMULADOSIP <- ACUMULADOSIP %>% 
  mutate(Cantidad = 1)

#reemplazar 2020 por 2019 en titulados
ACUMULADOSIP$FECHA_OBTENCION_TITULO[ACUMULADOSIP$FECHA_OBTENCION_TITULO == 2020] <- 2019

#me quedo con tres variables
series_tiempoIP <- ACUMULADOSIP[,c(2,7,16)]
#creo un dataframe con la cantidad de gente titulada por añ0
IP <- aggregate(Cantidad ~ FECHA_OBTENCION_TITULO, series_tiempoIP, sum)

# plot
ggplot(data = IP , aes(x = FECHA_OBTENCION_TITULO, y = Cantidad)) + 
  geom_line(color="green") +
  geom_point(size=4) +
  labs(title = "Cantidad de titulados en IP - Chile", 
       subtitle = "Años 2007 a 2019",
       x = "Año de titulación",
       y = "Cantidad de personas tituladas")

#crear NUEVA COLUMNA INDICANDO QUE ES U, CFT O IP
IP <- IP %>% 
  mutate(Tipo_Institución = "Instituto Profesional")

CFT <- CFT %>% 
  mutate(Tipo_Institución = "Centro de formación técnica")
#RENOMBRER VARIABLES
names (UNIVERSIDAD) = c("FECHA_OBTENCION_TITULO", "Cantidad")

UNIVERSIDAD <- UNIVERSIDAD %>% 
  mutate(Tipo_Institución = "Universidad")

#UNIR dataframes para obtener un consolidado
CONSOLIDADO <- rbind(UNIVERSIDAD, CFT, IP)

#N°1GRÁFICO DEFINITIVO

ggplot(data = CONSOLIDADO,
       mapping = aes(x = FECHA_OBTENCION_TITULO,
                     y = Cantidad,
                     color = Tipo_Institución)) + geom_line() + geom_point() +
  labs(title = "Cantidad de titulados en Chile por año", 
       subtitle = "Años 2007 a 2019",
       x = "Año de titulación",
       y = "Cantidad de personas tituladas")
  



# SEGUNDO GRÁFICO DE TORTAS -----------------------------------------------
#require(reshape)

#DatosTdocambiar = rename(TORTA_UNIVERSIDAD, c(HASTA DOS A???OS DE ATRASO="HASTA DOS AÑOS DE ATRASO"))
#str(TORTA_UNIVERSIDAD$TIPOSALIDA)
#TORTA_UNIVERSIDAD <- ACUMULADOS[,c(1,9)]

#tabla_Universidad <- table(TORTA_UNIVERSIDAD$TIPOSALIDA)
#pie(tabla_Universidad)


IP2 <- ACUMULADOSIP[,c(9)]
IP2 <- IP2 %>% 
  mutate(Tipo_Institución = "Instituto Profesional")

CFT2 <- ACUMULADOSCFT[,c(9)]
CFT2 <- CFT2 %>% 
  mutate(Tipo_Institución = "Centro Formación Técnica")


UNIVERSIDAD2 <- ACUMULADOS[,c(9)]
UNIVERSIDAD2 <- UNIVERSIDAD2 %>% 
  mutate(Tipo_Institución = "Universidad")
names (UNIVERSIDAD2) = c("TIPOSALIDA", "Tipo_Institución")


# GRAFICOS 2 PARTE --------------------------------------------------------
#ME CREA LA TABLA DE PORCENTAJE
test.CFT = CFT2 %>% group_by(TIPOSALIDA) %>%
  summarise(count=n()) %>%
  mutate(porcentaje=count/sum(count)) 

ggplot(test.CFT, aes(x=TIPOSALIDA, y=porcentaje, fill=TIPOSALIDA))+
  geom_bar(stat="identity", position= "dodge") +
  geom_text(aes(label=paste0(round(porcentaje*100,3),"%")), color="black",vjust=0) +
  scale_x_discrete("Tipo de salida") +     # configuración eje X (etiqueta del eje)
  scale_y_continuous("Porcentaje",labels=scales::percent) + #Configuración eje y
  labs(title = "Gráfico de barras Centro de formación técnica",
       subtitle = "Frecuencia relativa de la variable tipo de salida") 

################################################
#ME CREA LA TABLA DE PORCENTAJE
test.U = UNIVERSIDAD2 %>% group_by(TIPOSALIDA) %>%
  summarise(count=n()) %>%
  mutate(porcentaje=count/sum(count)) 

ggplot(test.U, aes(x=TIPOSALIDA, y=porcentaje, fill=TIPOSALIDA))+
  geom_bar(stat="identity", position= "dodge") +
  geom_text(aes(label=paste0(round(porcentaje*100,3),"%")), color="black",vjust=0) +
  scale_x_discrete("Tipo de salida") +     # configuración eje X (etiqueta del eje)
  scale_y_continuous("Porcentaje",labels=scales::percent) + #Configuración eje y
  labs(title = "Gráfico de barras Universidad",
       subtitle = "Frecuencia relativa de la variable tipo de salida") 


#################################################
#ME CREA LA TABLA DE PORCENTAJE
test.IP = IP2 %>% group_by(TIPOSALIDA) %>%
  summarise(count=n()) %>%
  mutate(porcentaje=count/sum(count)) 

ggplot(test.IP, aes(x=TIPOSALIDA, y=porcentaje, fill=TIPOSALIDA))+
  geom_bar(stat="identity", position= "dodge") +
  geom_text(aes(label=paste0(round(porcentaje*100,3),"%")), color="black",vjust=0) +
  scale_x_discrete("Tipo de salida") +     # configuración eje X (etiqueta del eje)
  scale_y_continuous("Porcentaje",labels=scales::percent) + #Configuración eje y
  labs(title = "Gráfico de barras Instituto Profesional",
       subtitle = "Frecuencia relativa de la variable tipo de salida")
  theme_void()

  ####################################################
  
  