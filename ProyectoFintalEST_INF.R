#Nota: Importar la base de datos como "datos"

library(ggplot2)
library(dplyr)
library(hrbrthemes)

donut <- function(vector, fill, line, title) {
  cat <- unique(vector)
  count <- c( length(vector[vector == cat[1]]), length(vector[vector == cat[2]]) )
  
  donut <- data.frame(
    category = cat,
    count = count
  )
  
  # Compute percentages
  donut$fraction <- donut$count / sum(donut$count)
  
  # Compute the cumulative percentages (top of each rectangle)
  donut$ymax <- cumsum(donut$fraction)
  
  # Compute the bottom of each rectangle
  donut$ymin <- c(0, head(donut$ymax, n=-1))
  
  # Compute label position
  donut$labelPosition <- (donut$ymax + donut$ymin) / 2
  
  # Compute a good label
  donut$label <- paste0(donut$category, "\n", donut$count)
  
  # Make the plot
  ggplot(donut, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
    geom_rect() +
    labs(title=title) +
    geom_text( x=1.5, aes(y=labelPosition, label=label, color=colors), size=6) +
    scale_fill_manual(values = fill) +
    scale_color_manual(values = line) +
    coord_polar(theta="y") +
    xlim(c(0, 4)) +
    theme_void() +
    theme(legend.position = "none")

}

#Resumen de los datos

summary(datos)

datos %>% ggplot(aes(x=Score)) + 
  geom_density(fill="#FEFFBF", color="#EAEEAE", alpha=0.7) + theme_bw() + 
  labs(x="Puntaje", y="Frecuencia", title = "Frecuencias del puntaje obtenido.")

datos %>% ggplot(aes(x=Edad)) + 
  geom_density(fill="#FCDCDF", color="#ECBCBF", alpha=0.9) + theme_bw() +
  labs(x="Edad", y="Frecuencia", title = "Frecuencias de las edades.")

datos %>% ggplot(aes(x=60/Treac)) + 
  geom_density(fill="#F498C2", color="#E180B2", alpha=0.7) + theme_bw() + 
  labs(x="Tiempo de reación", y="Frecuencia", title = "Frecuencias de los tiempos de reación.")

datos %>% ggplot(aes(x=CantObs)) + 
  geom_density(fill="#C997D6", color="#A977C6", alpha=0.7) + theme_bw() + 
  labs(x="Cantidad de obstaculos", y="Frecuencia", title = "Frecuencias de la cantidad de obstáculos.")

#Donas para: antecedentes, blanco/negro y sexooooo

donut(datos$Sexo, c( "#B0EFEF","#FFB7B2" ), c("#28262b", "#28262b"), "Proporción de géneros")
donut(datos$AntC, c("#C5EBFE", "#A5F8CE"), c("#28262b", "#28262b"), "Proporción de antecedentes")
donut(datos$ColorF, c("#f4f3ee", "#28262b"), c("#28262b", "#28262b"), "Proporción de colores de fondo")

#8 Pruebas para las variables

#Numericas
ks.test(datos$Edad,"pnorm",mean(datos$Edad),sd(datos$Edad))
ks.test(datos$Score,"pnorm",mean(datos$Score),sd(datos$Score))
ks.test(datos$Treac,"pnorm",mean(datos$Treac),sd(datos$Treac))
ks.test(datos$CantObs,"pnorm",mean(datos$CantObs),sd(datos$CantObs))

#Los p-valores son, respectivamente, 6.094e-08, 0.05977, 0.07084, 9.532e-09
#Se garantiza la no normalidad de las variables edad y cantidad de obstáculos.
#Las variables puntaje y tiempo de reaccion no son normales con un nivel de significancia de 0.1 

#Categoricas
length(datos[ datos$Sexo == "Hombre.", ]$Sexo)
length(datos[ datos$Sexo == "Mujer.", ]$Sexo)

length(datos[ datos$AntC == "Sí.", ]$Sexo)
length(datos[ datos$AntC == "No.", ]$Sexo)

length(datos[ datos$ColorF == "Negro.", ]$Sexo)
length(datos[ datos$ColorF == "Blanco.", ]$Sexo)

#Se garantiza un mínimo de 5 datos en cada categoría

#5 pruebas estadísticas

err <- function( var1, var2 ) {
  sqrt(var(var1)/length(var1) + var(var2)/length(var2))
}

interval <- function( var1, var2, max ) {
  (mean(var1) - mean(var2)) + max * err( var1, var2 ) * qt(0.975, length(var1) + length(var2) - 2)
}

#1.  Existe correlación entre el puntaje y el tiempo de reacción

#El calculo del tiempo de reacción es 60 segundos entre la cantidad de inputs que el jugador realizó
Treac <- 60/datos$Treac

cor.test( datos$Score, Treac )
# p-value = 6.297e-05, existe correlacion de -0.4320079.
# p-value = 6.297e-05, existe correlacion de -0.4320079.

summary(lm(datos$Score~Treac))
#El p-value es mínimo, por lo tanto la regresión lineal es un modelo adecuado

ggplot(datos, aes(x=Score, y=60/Treac)) +
  labs(x = "Puntaje", y = "Tiempo de reación", title= "Correlación entre el puntaje y el tiempo de reación") +
  theme_bw() +
  geom_point() +
  geom_smooth(method=lm , color="red", se=FALSE)


#2. Existe correlación entre la cantidad de obstáculos y el tiempo de reacción.
cor.test( datos$CantObs, Treac )

summary(lm(datos$CantObs~Treac))
#El p-value es mayor a 0.1 por lo tanto la regresión lineal no es un modelo adecuado.

# p-value = 0.1856, no se garantiza correlación para ninguno de los niveles de significancia.
ggplot(datos, aes(x=CantObs, y=60/Treac)) +
  labs(x = "Cantidad de obstáculos", y = "Tiempo de reación", title= "Correlación entre la cantidad de obstáculos y el tiempo de reacción") +
  theme_bw() +
  geom_point() +
  geom_smooth(method=lm , color="#CCFF66", se=FALSE)

#3. Existe diferencias entre el promedio del score entre hombres y mujeres.

masc <- datos[ datos$Sexo == "Hombre.", ]
fem <- datos[ datos$Sexo == "Mujer.", ]

err(masc$Score, fem$Score)
interval( masc$Score, fem$Score, -1 )
interval( masc$Score, fem$Score, 1 )

#El intervalo es positivo, por lo tanto, existe diferencia y el puntaje obtenido por los hombres por lo general es mayor al de las mujeres ( 0.5631783, 3.421784 )
boxplot(split(datos$Score, datos$Sexo), col = c("#5D2EAC", "#58FCEC"), horizontal = TRUE, xlab = "Puntaje.", ylab = "Género", names = c("Masculino", "Femenino"), main = "Comparacion entre el puntaje obtenido por hombres y mujeres" )

#4. Existe diferencia entre las medias de los antecedentes y la edad.

vet <- datos[ datos$AntC == "Sí.", ]
nov <- datos[ datos$AntC == "No.", ]

err(vet$Edad, nov$Edad)
interval( vet$Edad, nov$Edad, -1 )
interval( vet$Edad, nov$Edad, 1 )

#El intervalo contiene al 0, por lo cual no se puede garantizar diferencia significativa entre la diferencia de ambas medias.

boxplot(split(datos$Edad, datos$AntC), col = c("#41EAD4", "#B91372"), horizontal = TRUE, xlab = "Edad.", ylab = "Experiencia previa.",  main = "Comparacion entre la edad y la experiencia previa." )

#5. Hay relación entre el color del juego y el sexo.

chisq.test( datos$Sexo, datos$ColorF )
#p-value = 0.8228, por lo cual no se puede garantizar la diferencia para ningun nivel de significancia

barplot(table( datos$ColorF, datos$Sexo), beside=TRUE, col = c("#F5F5F5", "#444444"), legend.text = TRUE,  args.legend = list(x = "bottomleft", inset = c(-0.1, -0.22)), main = "Diferencia entre el género y el color del juego", xlab = "Categoría",ylab = "Frecuencia")

