###################################################
## Modelizaci?n ventas tabaco con R
## 
## MSMK, MBABDS, Abril 2021
##
###################################################


## Borramos todo y liberamos memoria
rm(list=ls());gc()

##Cambiamos el directorio de trabajo (?ste es el mio, cada uno pone el suyo)
setwd("C:/00. Deloitte Carlos R/05. Eventos, charlas y workshops/08. M?ster MSMK/MBABDS+PEBDAA/Ejercicio R - Ventas Tabaco")

## Cargo todas las librer?as que voy a usar

#install.packages('ISOweek')

library(lubridate)
library(reshape2) # Contiene la funci?n melt y reshape muy ?tiles para transformar las bbddd
library(ggplot2)
library(ISOweek) # Con ISOweek calcula la semana del a?o en formato ISO:  http://en.wikipedia.org/wiki/ISO_week_date


## Cargamos los datos a analizar

bbdd<-read.csv2("ejercicioTabaco.csv")
# Lo definimos con un nombre gen?rico para que el c?digo me sirva  siempre

## Exploramos los datos

head(bbdd)
# View(bbdd) 
dim(bbdd)

summary(bbdd) # Me devuelve un resumen para cada variable


## Convertimos la variable fecha a tipo Date, esto es importante para utilizar este variable como una fecha y no como letras
bbdd$fecha<-as.Date(dmy(bbdd$fecha))  # dmy es para formato tipo d?a mes a?o


## Dibujamos nuestra variable a explicar:
plot(bbdd$fecha,bbdd$vtas_tabaco,type="l")

# Correlaciones
# Calculamos la correlacion con las ventas (eliminamos las columnas 1 y 2 porque en este caso son fechas y meses)
cor(bbdd[,-c(1:2)])[,1] 

# Calculamos la correlacion entre todas las variables (eliminamos las columnas 1 y 2 porque en este caso son fechas y meses)

cor(bbdd[,-c(1:2)])[,]
plot(bbdd[,], main = "Matriz de correlación")

## MODELIZACI?N

## Mostramos por consola todas las variables de la bbdd listas para copiar y pegar
cat(paste("+",colnames(bbdd),sep="",collapse="\n"))

# Comentamos las variables que no queremos introducir en el modelo

fit<-lm(vtas_tabaco~
        #+mes
        #+fecha
        #+vtas_tabaco
        +pernoctaciones_espana
        +precio
        +subida_iva_Jul_2010
        +subida_iva_Sep_2012
        +ley_antitabaco
        +enero
        +febrero
        +tasa_paro
        ,bbdd) 

summary(fit)

# Repetimos el modelo sin incluir la tasa de paro que es no significativa

fit<-lm(vtas_tabaco~
          #+mes
          #+fecha
          #+vtas_tabaco
          +pernoctaciones_espana
        +precio
        +subida_iva_Jul_2010
        +subida_iva_Sep_2012
        +ley_antitabaco
        +enero
        +febrero
        #+tasa_paro
        ,bbdd) 

summary(fit)

attributes(fit)

## Cuidado con el R2 que sale reflejado sin intercept. Al hacer un modelo sin intercept este R2 es diferente
## C?lculo a mano del R2:
#R2<-1 - crossprod(residuals(fit)) / crossprod(fit$model[,1] - mean(fit$model[,1])) 
#R2


 # Ajuste
plot(bbdd$fecha,bbdd$vtas_tabaco,type="l")
lines(bbdd$fecha,fit$fitted.values,col=2,lty=2)#fitted.values es el ajuste de nuestro modelo


# Diagnosis
par(mfrow=c(2,2)) # Divido el gr?fico en 4
plot(fit)
dev.off() # borra el gr?fico


# Aportes
aportes<-colSums(model.matrix(fit)) * fit$coefficients
aportes<-abs(aportes)/sum(abs(aportes))
aportes

pie(aportes,labels=paste(names(aportes),": ",round(aportes*100,2),"%",sep=""))


## Exportar coeficientes a excel para pintar graficos


write.csv2(fit$coefficients, file="coeficientes_modelo.csv")


