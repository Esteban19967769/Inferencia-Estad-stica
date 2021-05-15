#INFERENCIA ESTADÍSTICA INTERVALOS

#https://fhernanb.github.io/Manual-de-R/ic.html#ejemplo-59



#1 CREO MI PEQUEÑA BASE
pan <- c(1,3,4,4,2,6,4,6,3,2,6,8,3,7) 
azucar <- c(5,6,8,4,2,6,8,3,7,2,8,6,4,3)
base_de_datos <- data.frame(pan, azucar) #creo base de datos
promediomuestral = 8 #fijo media poblacional
sigma = 2 #fijo devesta poblacional

#2. Intervalo confianza media con varianza conocida
install.packages("BSDA")
library(BSDA)
zsum.test(mean.x=10,sigma.x=2, n.x=14,conf.level=0.90)

#3. Intervalo confianza media con varianza DESCONOCIDA
mean(base_de_datos$pan) #4.214286
sd(base_de_datos$pan) #2.082106
t.test(x=base_de_datos$pan, conf.level=0.90)$conf.int

#4. Intervalo confianza para la PROPORCIÓN
prop.test(x=275, n=500, conf.level=0.90)$conf.int  #x número éxito y n muestra

#5. Intervalo confianza para la VARIANZA Y DESVIACIÓN ESTÁNDAR
n <- length(base_de_datos$pan)
alfa <- 0.05
L_1 <- ((n-1)*var(base_de_datos$pan)/qchisq(1-alfa/2,n-1))
L_2 <- (n-1) * var(base_de_datos$pan) / qchisq(alfa /2,n-1)
IC_VAR <- c(L_1,L_2)
IC_VAR  #INTERVALO DE varianzas
IC_SD <- sqrt(IC_VAR)
IC_SD   #ITNERVALO DE DEVESTA



#5. Intervalo DIFERENCIAS de MEDIAS CON VARIANZAS CONOCIDAS

library(BSDA)
zsum.test(mean.x=81,sigma.x=5.2,n.x=25,mean.y=76,sigma.y=3.4,n.y=36, conf.level=0.99)


#6. Intervalo DIFERENCIAS de VARIANZAS DESCONOCIDAS PARA SABER SI SON IGUALES O DISTINTAS
var.test(x=base_de_datos$pan, y=base_de_datos$azucar,
         conf.level=0.95)$conf.int

#7. Intervalo DIFERENCIAS de medias con VARIANZAS DESCONOCIDAS DISTINTAS
t.test(x=base_de_datos$pan, y=base_de_datos$azucar,
       paired=FALSE, var.equal=FALSE,
       conf.level = 0.95)$conf.int

#8. Intervalo DIFERENCIAS de medias con VARIANZAS DESCONOCIDAS IGUALES
t.test(x=base_de_datos$pan, y=base_de_datos$azucar,
       paired=FALSE, var.equal=TRUE,
       conf.level = 0.95)$conf.int

#9. Intervalo DIFERENCIAS de medias con MUESTRAS PAREADAS
t.test(x=base_de_datos$pan, y=base_de_datos$azucar,
       paired=TRUE,
       conf.level = 0.95)$conf.int

#10. Intervalo DIFERENCIAS de PROPORCIONES
prop.test(x=c(75, 80), n=c(1500, 2000), conf.level=0.90)$conf.int #X1 Y X2 SON 75 Y 80 MIENTRAS N1 Y N2 1500 Y2000

