##### Datos no agrupados ####


edades  <- c (28,24,28,29,21,21,28,26,25,24,
              24,26,23,33,24,29,26,30,26,29)

# promedio

promedio_edades <- mean(edades)

#mediana

mediana_edades <- median(edades)

#resumen estadístico edades
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

#valor máximo

edades_max <- max(edades)

#valor mínimo

edades_min <- min(edades)

#rango 
rango_edades <- edades_max - edades_min

#opcionalmente existe la funció range

range(edades)
#guarda una tupla con los valores maximo y minimo
#despues con la funcion *diff* se obtiene el rango

#sturges
no_intervalos <- nclass.Sturges(edades)

#tamaño de los intervalos

amplitud <- rango_edades/no_intervalos


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
    main = "Gráfica de sectores")








