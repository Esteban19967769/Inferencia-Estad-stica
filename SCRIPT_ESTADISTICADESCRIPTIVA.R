  
#CTRL+MAYUS+R: me crea comentario

# 1. Instalar y/o cargar paquetes -----------------------------------------

install.packages("tidyverse")
library(tidyverse)
install.packages("dplyr")
library(dplyr)
install.packages("readxl")
library(readxl)
install.packages("modeest")
library(modeest)
install.packages("FinCal")
library(FinCal)
install.packages("fdth")
library(fdth)
install.packages("ggplot2")
library(ggplot2)
install.packages("moments")
library(moments)
install.packages("rlang")
library(rlang)

# 2. LLamar a la base de datos --------------------------------------------

Base <- read_excel("????????????????????", sheet = 1)

# Letra a) -----------------------------------------------------------------

#Tipos de variables
str(Base)

#Resumen estadístico de la base de datos
summary(Base)


# Letra b) -----------------------------------------------------------------

#EDAD
edad_mean <- mean(Base$Edad, na.rm = T)
edad_mean

edad_devesta <- sd(Base$Edad, na.rm = T)
edad_devesta

edad_moda <- mfv(Base$Edad)
edad_moda

edad_asimetria <- ((edad_mean-edad_moda)/edad_devesta)
edad_asimetria

#COMPROBAR ASIMETRÍA
skewness(Base$Edad, na.rm = T)  

hist(x = Base$Edad, main = "Histograma Edad", 
     xlab = "Edad", ylab = "Frecuencia absoluta",
     col = "orange")

-------------------------------------------------------
#TOTAL USD

USD_devesta <- sd(Base$TotalUSD, na.rm = T)
USD_devesta

USD_moda <- mfv(Base$TotalUSD)
USD_moda

USD_mean <- mean(Base$TotalUSD, na.rm = T)
USD_mean

USD_asimetria <- ((USD_mean-USD_moda)/USD_devesta)
USD_asimetria

#COMPROBAR ASIMETRÍA
skewness(Base$TotalUSD, na.rm = T)  

hist(x = Base$TotalUSD, main = "Histograma Total_USD", 
     xlab = "Total_USD", ylab = "Frecuencia absoluta",
     col = "blue")

# Letra c) -----------------------------------------------

#Construir regla de Sturges

Base_HOMBRE <- Base[Base$Genero=="Hombre",]
dim(Base_HOMBRE) #93 observaciones

dist_B_H <- fdt(Base_HOMBRE$TotalUSD,breaks="Sturges") # calcula la distribución de frecuencias utilizando la regla Sturge
dist_B_H

hist(Base_HOMBRE$TotalUSD, breaks = "Sturges", col = "green", main = "Histograma Total_USD_Hombres",
     xlab = "Total_USD", ylab = "Frecuencia absoluta")

cv_H <- sd(Base_HOMBRE$TotalUSD)/mean(Base_HOMBRE$TotalUSD)
cv_H

############################################

  Base_MUJER <- Base[Base$Genero=="Mujer",]
  dim(Base_MUJER) #7 observaciones
  
  k1<-1 + (3.322*log10(7))
  k1
  
  nclass.Sturges(Base_MUJER$TotalUSD)
  min_m <- min(Base_MUJER$TotalUSD)
  max_m <- max(Base_MUJER$TotalUSD)
  rango_m <- max_m - min_m
  rango_m/4
  
  dist_B_M <- fdt(Base_MUJER$TotalUSD, start = 25, end = 140, h=22) # calcula la distribución de frecuencias utilizando la regla Sturge
  dist_B_M
  
  hist(Base_MUJER$TotalUSD, breaks = "Sturges", col = "pink", main = "Histograma Total_USD_Mujeres",
       xlab = "Total_USD", ylab = "Frecuencia absoluta")
  
  cv_M <- sd(Base_MUJER$TotalUSD)/mean(Base_MUJER$TotalUSD)
  cv_M
  
#############################################

Base_Casado <- Base[Base$EstadoCivil=="Casado",]
dim(Base_Casado) #84 observaciones

k2<-1 + (3.322*log10(84))
k2

nclass.Sturges(Base_Casado$TotalUSD)
min_C <- min(Base_Casado$TotalUSD) #13.23
max_C <- max(Base_Casado$TotalUSD) #287.59
rango_C <- max_C - min_C #274.36
rango_C/8 #34.295

dist_B_C <- fdt(Base_Casado$TotalUSD, start = 13.23, end = 287.59, h=34.295) # calcula la distribución de frecuencias utilizando la regla Sturge
dist_B_C

hist(Base_Casado$TotalUSD, breaks = "Sturges", col = "brown", main = "Histograma Total_USD_Casados",
     xlab = "Total_USD", ylab = "Frecuencia absoluta")

cv_C <- sd(Base_Casado$TotalUSD)/mean(Base_Casado$TotalUSD)
cv_C

#############################################

Base_Soltero <- Base[Base$EstadoCivil=="Soltero",]
dim(Base_Soltero) #16 observaciones

k3<-1 + (3.322*log10(16))
k3 #5

nclass.Sturges(Base_Soltero$TotalUSD)
min_S <- min(Base_Soltero$TotalUSD) #29.5
max_S <- max(Base_Soltero$TotalUSD) #192.8
rango_S <- max_S - min_S #163.3
rango_S/5 #32.66

dist_B_S <- fdt(Base_Soltero$TotalUSD, start = 28, end = 198, h=33) # calcula la distribución de frecuencias utilizando la regla Sturge
dist_B_S

hist(Base_Soltero$TotalUSD, breaks = "Sturges", col = "purple", main = "Histograma Total_USD_Soltero",
     xlab = "Total_USD", ylab = "Frecuencia absoluta")

cv_S <- sd(Base_Soltero$TotalUSD)/mean(Base_Soltero$TotalUSD)
cv_S


# Letra d) ----------------------------------------------------------------------

#d1) para hacerlo, creo una nueva variable con mutate

Base <- Base %>% 
  mutate(CUPO = case_when(TotalUSD <58 ~ "Normal",
                          TotalUSD >=58 & TotalUSD<=96 ~ "Extra",
                          TotalUSD>96 ~ "Alto")) 

#summary
Base %>% group_by(CUPO) %>% 
  summarise(mean = mean(Edad), sum =sum(Edad), n = n(), sd = sd(Edad), cv = round(sd/mean*100,2), min = min(Edad), max = max(Edad),
            P25 = quantile(Edad, prob = c(0.25)), P50 = quantile(Edad, prob = c(0.5)), P75 = quantile(Edad, prob = c(0.75))) %>% 
  ungroup()

#gráfica
ggplot(Base, aes(x=CUPO, y=Edad)) + 
  geom_boxplot(alpha=0.3) +
  theme(legend.position="none") + geom_boxplot(color="blue", fill="orange", alpha=0.2)

#d2) Percentil 80
Base %>% group_by(CUPO) %>% 
  summarise(P80 = quantile(Edad, prob = c(0.8))) %>% 
  ungroup()



# Letra E) ----------------------------------------------------------------------


Base %>% group_by(Genero) %>% 
  summarise(mean = mean(Edad), sum =sum(Edad), n = n(), sd = sd(Edad), cv = round(sd/mean*100,2), min = min(Edad), max = max(Edad),
            P25 = quantile(Edad, prob = c(0.25)), P50 = quantile(Edad, prob = c(0.5)), P75 = quantile(Edad, prob = c(0.75))) %>% 
  ungroup()

#
ggplot(Base, aes(x=Genero, y=Edad)) + 
  geom_boxplot(alpha=0.3) +
  theme(legend.position="none") + geom_boxplot(color="red", fill="red", alpha=0.2)

Base %>% group_by(Genero) %>% 
  summarise(mean = mean(TotalUSD), sum =sum(TotalUSD), n = n(), sd = sd(TotalUSD), cv = round(sd/mean*100,2), min = min(TotalUSD), max = max(TotalUSD),
            P25 = quantile(TotalUSD, prob = c(0.25)), P50 = quantile(TotalUSD, prob = c(0.5)), P75 = quantile(TotalUSD, prob = c(0.75))) %>% 
  ungroup()

#
ggplot(Base, aes(x=Genero, y=TotalUSD)) + 
  geom_boxplot(alpha=0.3) +
  theme(legend.position="none") + geom_boxplot(color="blue", fill="blue", alpha=0.2)
  

# Letra f) ----------------------------------------------------------------

install.packages("hrbrthemes")
library(hrbrthemes)

ggplot(Base, aes(x=Edad, y=TotalUSD)) + 
  geom_point(
    color="black",
    fill="#69b3a2",
    shape=22,
    alpha=0.5,
    size=6,
    stroke = 1
  ) +
  theme_ipsum()

cor(Base$Edad, Base$TotalUSD)


# Letra G) ----------------------------------------------------------------

#crear nueva variable
Base <- Base %>% 
  mutate(CupoPROM = TotalUSD/CantidadArticulos)

mean_CUPOPROM <- mean(Base$CupoPROM)
sd_CUPOPROM <- sd(Base$CupoPROM)

CV_CupoProm <- (sd_CUPOPROM/mean_CUPOPROM)*100
CV_CupoProm

Regular_REGULAR <- Base[Base$TipoCliente=="Regular",]
cv_REGULAR <- sd(Regular_REGULAR$CupoPROM)/mean(Regular_REGULAR$CupoPROM)*100
cv_REGULAR

Regular_PROMOCIONAL <- Base[Base$TipoCliente=="Promocional",]
cv_PROMOCIONAL <- sd(Regular_PROMOCIONAL$CupoPROM)/mean(Regular_PROMOCIONAL$CupoPROM)*100
cv_PROMOCIONAL


# Listo -------------------------------------------------------------------


