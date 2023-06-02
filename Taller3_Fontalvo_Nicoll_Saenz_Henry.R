#PUNTO 1

barometroh <- DatosBarometro_1_[DatosBarometro_1_$q1 == 1,]
barometrom <- DatosBarometro_1_[DatosBarometro_1_$q1 == 2,]

error1 <-sqrt(var(barometroh$PPR_Index)/835
              +var(barometrom$PPR_Index)/827)
error1

(mean(barometroh$PPR_Index) - mean(barometrom$PPR_Index)) - 
  error1 * qt(0.975,827+835-2)
(mean(barometroh$PPR_Index) - mean(barometrom$PPR_Index)) + 
  error1 * qt(0.975,827+835-2)
#el intervalo de confianza es: [-0.22, 0.36] incluye al cero por lo que
#la diferencia no es significativa

boxplot(split(DatosBarometro_1_$PPR_Index, DatosBarometro_1_$q1), 
        xlab = "Indice", ylab = "Sexo",
        names = c("Mujer","Hombre"), horizontal=TRUE, 
        col = c("#EE6A50","#FF7F50"))

#PUNTO 2
#a)
sexo<-factor(DatosBarometro_1_$q1,levels=c(1, 2),labels=c("Hombre","Mujer"))
estrato<-c(DatosBarometro_1_$estratosec)

table(sexo,estrato)

#HIPOTESIS ALTERNATIVA: Existe relación entre el sexo y el estrato socioeconomico.
#HIPOTESIS NULA: No existe relación entre el sexo y el estrato socioeconomico.

chisq.test(sexo,estrato)
# con un p-value del 0.9563, no se puede rechazar la hipotesis nula,
#no hay asociación estadística significativa
plot(table(sexo,estrato), col=c("#FFDEAD", "#EED5D2", "lightsteelblue1"),
     main="Estrato economico de hombres y mujeres")

#b) 

#HIPOTESIS ALTERNATIVA: Existe correlación entre ambos índices.
#HIPOTESIS NULA: No existe correlación entre los índices.

cor.test(DatosBarometro_1_$AOM_Index,DatosBarometro_1_$PPR_Index)    
#Con un p-value de 6.626e-10, se puede rechazar la hipotesis nula,
#si hay una correlación entre los indices.
plot(DatosBarometro_1_$AOM_Index, DatosBarometro_1_$PPR_Index,
     xlab='AOM', ylab='PPR', col=("#473C8B"), pch=19)

#c)
#HIPOTESIS ALTERNATIVA: Existe relación entre las medias del indice AOM de hombres y mujeres
#HIPOTESIS NULA: No existe relación entre las medias del indice AOM de hombres y mujeres.

t.test(DatosBarometro_1_$PPR_Index~DatosBarometro_1_$q1)
#con un p-value de 0.6396 no se puede rechazar la hipotesis nula, lo que quiere
#decir que si hay una diferencia significativa entre las medias del AOM de hombre y mujeres.
boxplot(split(DatosBarometro_1_$AOM_Index, DatosBarometro_1_$q1), 
        xlab = "Indice AOM", ylab = "Sexo",
        names = c("Mujer","Hombre"),horizontal=TRUE, 
        col=c("#CD2990", "#E066FF"))

#PUNTO 3 - COMPARACIÓN DE PROPORCIONES
#regiones<-factor(DatosBarometro_1_$estratopri,levels=c(1, 2, 3, 4, 5, 6),
 #                labels=c("Atlantica","Bogotá", "Central", "Oriental", "Pacífica", "Amazonía"))
sexo<-factor(DatosBarometro_1_$q1,levels=c(1, 2),labels=c("Hombre","Mujer"))
estrato<-c(DatosBarometro_1_$estratosec)

table(sexo,estrato)
#Se quiere comparar la proporciones de homres y mujeres en estrato alto
#Se considera exito que hay más hombres en estrato alto que mujeres en estrato alto

#PROPORCION DE HOMBRES Y MUJERES ESTRATO ALTO
ph <- 141/827
pm <- 138/835

errormuestral <- sqrt(ph*(1-ph)/827 + pm*(1-pm)/835)
(ph - pm) - errormuestral * qnorm(0.975) 
(ph - pm) + errormuestral * qnorm(0.975)
#INTERVALO [-0.0307, 0.0411]
#La diferencia no es significatIva ya que incluye al cero dentro del intervalo

#Prueba de hipótesis  
#Hipótesis científica: NO hay diferencia entre a proporcion de hombres en estrato alto 
#y la proporción de mujeres 
#Hipótesis estadística: No hay diferencia entre la proporción de hombres en estrato alto 
#y la proporcion de mujeres
#Hipótesis nula; Hay diferencia entre la proporcion de hombres y mujeres en estrato alto

prop.test(c(141,138),c(827,835))

#DECISIÓN
#Como el p-valor es 0.8263 se puede rechazar la hipótesos nula con todos los niveles 
#de signmificancia

#Conclusión
#Como se rechaza la hipótesis nula entonces no hay diferencia significativa entre la 
#proporción en estrato alto de ombres  mujeres
sexo<-factor(DatosBarometro_1_$q1,levels=c(1, 2),labels=c("Hombre","Mujer"))
estrato<-c(DatosBarometro_1_$estratosec)
estrato3 <- estrato >2


barplot(table(estrato3,sexo), col=c("#66CDAA", "#EED5D2"),
     main="Estrato economico de hombres y mujeres")
legend(x = "topright", legend = c("2 o 1", "3"), fill = c("#66CDAA", "#EED5D2"), 
       title = "estratos")

#, "lightsteelblue1"