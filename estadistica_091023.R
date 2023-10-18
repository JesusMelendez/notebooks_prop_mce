##### Datos no agrupados ####


edades  <- c (28,24,28,29,21,21,28,26,25,24,
              24,26,23,33,24,29,26,30,26,29)

# promedio

promedio_edades <- mean(edades)

#mediana

mediana_edades <- median(edades)

#resumen estadÃ­stico edades
resumen_st_edades <- summary(edades)

#boxplot(edades,horizontal = TRUE)

#boxplot(edades,col = rgb(0,0.5,1, alpha = 0.5),border  = 4)

#boxplot(edades,col = "white",border  = 4)

boxplot(edades,col = "white",border  = 4,add = TRUE)


#calificaciones

calificaciones <- c(7.98,9.3,7.8,9,8.55,8.3,8.7,8.4,8.56,9.2,
                    8.9,8.5,9.3,7.9,8.9,7.47,9.1,8.7,8.03,8.9)

#promedio cal

promedio_cal <- mean(calificaciones)

#mediana califi

mediana_calificaciones <- median(calificaciones)

#resumen esta calificaciones

resumen_st_cal <- summary(calificaciones)
print(resumen_st_cal)

#grafica

boxplot(calificaciones,col = "green")


## Datos agregados - Ej Edades ##


#ordenamos los datos ascendentemente

edades_ordenadas <- sort(edades)

#valor mÃ¡ximo

edades_max <- max(edades)

#valor mÃ­nimo

edades_min <- min(edades)

#rango 
rango_edades <- edades_max - edades_min

#opcionalmente existe la funciÃ³ range

range(edades)
#guarda una tupla con los valores maximo y minimo
#despues con la funcion *diff* se obtiene el rango


no_intervalos <- nclass.Sturges(edades)

#tamaÃ±o de los intervalos

amplitud <- rango_edades/no_intervalos#sturges


#tabla de frecuencia abs

tabla_freq_edades  <- transform(table(cut(edades,breaks=no_intervalos)))

#tabla de frecuencia acumualdas

freq_ac_edades <- cumsum (tabla_freq_edades[,2])
tabla_freq_edades_ac <- cbind(tabla_freq_edades,freq_ac_edades)

#tabla de frecuencia relativa

n <- length(edades)
freq_rela_edades <- tabla_freq_edades[,2]/n
tabla_frecuencia <- cbind(tabla_freq_edades_ac,freq_rela_edades)


#grafica freq abs


barplot(tabla_frecuencia[,2],main="Frecuencia absoluta (edades)",
        xlab="Intervalos",ylab="Frecuencia",col=c("darkgray","darkblue","red"))


## ##

barplot(tabla_frecuencia[,4],main="Frecuencia Relativa (edades)",
        xlab="Intervalos",ylab="Frecuencia",col=c("gold","pink","brown"))


pie(tabla_frecuencia[,2],
    labels= c ("(21,23]","(23,25]","(25,27]","(27,29]","(29,31]","(31,33]"),
    col = 1:6,
    main = "Grafica de sectores")


pie_labels <- paste0(tabla_frecuencia[,1],
                     " = ",
                     round(100*tabla_frecuencia[,2]/n,2),"%")
pie(tabla_frecuencia[,2],
    labels = pie_labels,
    main = "GrÃ¡fica de sectores")

# clase 161023

# plot(tabla_frecuencia) genera una matriz de correlació de vars

plot(tabla_frecuencia[,2], type="h",main="Gráfica de bastón",xlab="Intervalos",ylab="Frecuencia")

## Histograma

hist(x=edades,main = "Histograma de Edad", col ="#7DB954",border="#f00000",
     breaks = no_intervalos,xlab = "Edad",ylab="Frecuencia")


hist(x=edades,prob=TRUE,main="Histograma con curva normal",col ="#FFA700",border="#f00000",
     ylab="Densidad")
x <- seq (min(edades),max(edades),lenght=20)
f <- dnorm(x,mean = mean(edades),sd=sd(edades))
lines(x,f,col="blue",lwd=2)

plot(freq_ac_edades,type="b",
     xlab="intervalos",ylab = "Frecuencia acumulada",
     main="Gráfico Ojiva",col="#FFA700",
     pch=19,las=1,bty="l")


## caso calificaciones ###############################################################################



#tabla de frecuencia abs
no_intervalos_cal <- nclass.Sturges(calificaciones)






#tabla de frecuencia abs

tabla_freq_cal  <- transform(table(cut(calificaciones,breaks=no_intervalos_cal)))

#tabla de frecuencia acumualdas

freq_ac_cal <- cumsum (tabla_freq_cal[,2])
tabla_freq_cal_ac <- cbind(tabla_freq_cal,freq_ac_cal)

## tabla de frecuencia relativa calificaciones

n_cal <- length(calificaciones)
freq_rela_cali <- tabla_freq_cal[,2]/n
tabla_frecuencia_cali <- cbind(tabla_freq_cal_ac,freq_rela_cali)

## graficas cal ##

plot(tabla_frecuencia_cali[,2], type="h",main="Gráfica de bastón",xlab="Intervalos",ylab="Frecuencia")

## Histograma

hist(x=calificaciones,main = "Histograma de calificaciones", col ="#7DB954",border="#f00000",
     breaks = no_intervalos,xlab = "calificaciones",ylab="Frecuencia")

hist(x=calificaciones,prob=TRUE,main="Histograma con curva normal",col ="#FFA700",border="#f00000",
     ylab="Densidad")
x <- seq (min(calificaciones),max(calificaciones),lenght=20)
f <- dnorm(x,mean = mean(calificaciones),sd=sd(calificaciones))
lines(x,f,col="blue",lwd=2)


plot(freq_ac_cal,type="b",
     xlab="intervalos",ylab = "Frecuencia acumulada calificaciones",
     main="Gráfico Ojiva",col="#FFA700",
     pch=19,las=1,bty="l")



pie(tabla_frecuencia_cali[,2],
    labels= c ("(7.47,7.78]","(7.78,8.08]","(8.08,8.38]","(8.38,8.69]","(8.69,9]","(9,9.3]"),
    col = 1:6,
    main = "Grafica de sectores")


pie_labels <- paste0(tabla_frecuencia_cali[,1],
                     " = ",
                     round(100*tabla_frecuencia_cali[,2]/n,2),"%")
pie(tabla_frecuencia_cali[,2],
    labels = pie_labels,
    main = "Gráfica de sectores")