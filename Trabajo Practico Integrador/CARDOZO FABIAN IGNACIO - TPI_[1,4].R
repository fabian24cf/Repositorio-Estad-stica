#Cargamos la Libreria "readxl"
library(readxl)
library(modeest)

# a). Vamos a determinar la cantidad optima de intervalos a utilizar 

#Empezamos guardando archivo Excel en una Variable y Leyendola
Archivo <- file.choose()
datos <- read_excel(Archivo)

#En una variable guardamos los datos extraidos del vector "TIEMPO SEMANAL en HS. DEDIC. EST"
tiempo <- datos$`TIEMPO SEMANAL en HS. DEDIC. EST.`
tiempo <- as.numeric(tiempo)
tiempo <- tiempo[is.finite(tiempo)] 


# Usamos la fórmula de Sturges
n <- length(tiempo)
k <- ceiling(1 + 3.322*log10(n))
k

# c).Determinaremos el rango de valores de la variable "Tiempo de estudio semanal"
rango <- range(tiempo)

# Calculamos la amplitud (ancho de clase) utilizando la fórmula:
# (máximo - mínimo) / k , donde k es el número de intervalos según Sturges
# Usamos ceiling() para redondear hacia arriba y asegurar que todos los datos queden cubiertos
amplitud <- ceiling((rango[2] - rango[1]) / k)

# Visualizamos el rango y la amplitud obtenida
rango
amplitud

# Construimos los puntos de corte (breaks) para los intervalos de clase
# seq() genera una secuencia desde el valor mínimo redondeado hacia abajo (floor)
# hasta el máximo redondeado hacia arriba (ceiling) + amplitud,
# con un paso igual a la amplitud calculada
breaks <- seq(floor(rango[1]), ceiling(rango[2]) + amplitud, by= amplitud)
# Clasificamos los datos en intervalos utilizando cut()
# right = FALSE indica que los intervalos son de la forma [a, b)
# es decir, incluyen el límite inferior y excluyen el superior (excepto el último)
clases <- cut(tiempo, breaks = breaks, right = FALSE)
# Visualizamos las primeras clasificaciones de intervalos para verificar el resultado
head(clases)

# Calculamos la tabla de frecuencias absolutas (fa) para cada intervalo
Tabla_Tiempo <- table(clases)
# Calculamos la frecuencia absoluta acumulada (faa)
f_acum <- cumsum(Tabla_Tiempo)
# Calculamos la frecuencia relativa (fr) como proporción sobre el total
f_rel <- prop.table(Tabla_Tiempo)
# Calculamos la frecuencia relativa acumulada (fra)
f_rel_acum <- cumsum(f_rel)

# Construimos un Data Frame que consolida toda la información en una tabla de frecuencias
Tiempo_Semanal <- data.frame(
  Tiempo_Estad = levels(clases),                  # Intervalos de clase
  Frecuencia = as.vector(Tabla_Tiempo),           # Frecuencia absoluta
  Frec_Acum = as.vector(f_acum),                  # Frecuencia absoluta acumulada
  Frec_Relativa = round(as.vector(f_rel), 4),     # Frecuencia relativa 
  Frec_Rel_Acum = round(as.vector(f_rel_acum), 4) # Frecuencia relativa acumulada 
 )
Tiempo_Semanal[4, ]


# d). Transformamos la variable "SATISFACCIÓN CON LA CARRERA" en un factor con etiquetas descriptivas
# levels = define los códigos originales (1, 2, 3, 4)
# labels = asigna el nombre real de cada categoría (Muy satisfecho, Satisfecho, etc.)
satisfaccion <- factor(datos$`SATISFACCIÓN CON LA CARRERA`,
                       levels = c(1, 2, 3, 4),
                       labels = c("Muy satisfecho", "Satisfecho", "Insatisfecho", "Muy insatisfecho"))


# b). Calculamos la tabla de frecuencias absolutas (fa) para cada categoría de satisfacción
Tabla_satisfaccion <- table(satisfaccion)
# Calculamos la frecuencia absoluta acumulada (faa) sumando las frecuencias categoría por categoría
frec_acum <- cumsum(Tabla_satisfaccion)
frec_rel <- prop.table(Tabla_satisfaccion)
frec_rel_acum <- cumsum(frec_rel)

# Construimos un Data Frame que consolida toda la información en una tabla de frecuencias
Tabla_final <- data.frame(
  Satisfaccion= names(Tabla_satisfaccion),
  Frecue = as.vector(Tabla_satisfaccion),
  Frecu_Acum = as.vector(frec_acum),
  Frecu_Rela = round(as.vector(frec_rel), 4),
  Frecu_Rela_Acum = round(as.vector(frec_rel_acum), 4)
  
)

# Mostramos la tabla de frecuencias final
Tabla_final

# 3. Calculo de las Medidas Descriptivas de Tendencia Central.(Satisfacción)


# Convertimos la variable ordinal en numérica para poder calcular estadísticas.
# Se transforma una variable categórica ordinal en numérica para aplicar medidas estadísticas.

datos$Satifaccion <- as.numeric(factor(datos$`SATISFACCIÓN CON LA CARRERA`,
                                       levels = c(1, 2, 3, 4),
                                       labels = c("Muy satisfecho", "Satisfecho", "Insatisfecho", "Muy insatisfecho")))

# Definimos la variable y calculamos medidas de tendencia central.
# Se calculan media, mediana, moda y cuartiles para la variable discreta.
variable_discreta <- "Satifaccion"
media_discreta <- mean(datos[[variable_discreta]], na.rm= TRUE)
mediana_discreta <- median(datos[[variable_discreta]], na.rm= TRUE)
moda_discreta <- mlv(datos[[variable_discreta]], method = "mfv")
cuartiles <- quantile(datos[[variable_discreta]],probs=c(0.25,0.5,0.75), na.rm=TRUE)

# Creamos las tablas para poder mostrar los resultados

message("\nEstadisticos Descriptivoss: ")
Tabla_Discreta <- data.frame(
  Media = round(media_discreta, 4),
  Mediana = round(mediana_discreta, 4),
  Moda = moda_discreta
 
)
# Tabla para cuartiles.
Tabla_Cuartil <- data.frame(
Cuartil = c("Q1 (25%)", "Q2 (Mediana)", "Q3 (75%)"),
Valor = as.numeric(cuartiles)
)
print(Tabla_Discreta)
print(Tabla_Cuartil)



# Medidas para Variable Continua (Tiempo de estudio)


datos$tiempo <- tiempo
variable_continua <- "tiempo"
k <- ceiling(1 + 3.322*log10(nrow(datos))) # Obtenemos el número óptimo de clases a formar, es decir que Se determina 
# el número de intervalos para agrupar los datos.

# Se calculan los cortes de clase para agrupar los datos. Se define el rango y amplitud de cada clase.
min_valor <- floor(min(datos[[variable_continua]], na.rm=TRUE))
max_valor <- ceiling(max(datos[[variable_continua]], na.rm=TRUE))
amplitud <-ceiling((max_valor - min_valor)/k)
max_topes <- min_valor + amplitud * k
cortes <- seq(min_valor, max_topes, by = amplitud)


datos$clases <- cut(datos[[variable_continua]], breaks = cortes, 
                    rigth = FALSE, include.lowest= TRUE)

marca_clases <- (head(cortes,  -1) + tail(cortes, -1)) /2

# Se calcula frecuencia absoluta, acumulada, relativa y relativa acumulada.
tabla_clases <- table(datos$clases)
fr_acum <- cumsum(tabla_clases)
fr_rel <- prop.table(tabla_clases)
fr_rel_acum <- cumsum(fr_rel)

# Se construye la Tabla de Frecuencia
tabla_de_frecuencia <- data.frame(
  Intervalo = names(tabla_clases),
  Marca = as.vector(marca_clases),
  Frec_Abs = as.vector(tabla_clases),
  Frec_Acum = as.vector(fr_acum),
  Frec_Rel = round(as.vector(fr_rel), 4),
  Frec_Rel_Acum = round(as.vector(fr_rel_acum), 4)
  
)

print(tabla_de_frecuencia)


# Calculos Estadísticos Agrupados para Variable Continua

#Calculo de Media

frecuencias <- as.vector(tabla_clases)
media_continua <- sum(marca_clases * frecuencias) / sum(frecuencias)

# Calculo de la Moda

ind_modal <- which.max(frecuencias)
Lim_inf <- cortes[ind_modal]
f_m <- frecuencias[ind_modal]
f_1 <- ifelse(ind_modal == 1, 0, frecuencias[ind_modal-1]) 
f_2 <- ifelse(ind_modal== length(frecuencias), 0,frecuencias[ind_modal+1] )
moda_continua <- Lim_inf + ((f_m - f_1) / ((f_m - f_1) + (f_m - f_2))) * amplitud
 
#Calculo de Mediana

n_total <- sum(frecuencias)             
n_2 <- n_total / 2                                         # Mitad del total
clase_mediana_index <- which(tabla_de_frecuencia$Frec_Acum >= n_2)[1]  # Índice de la clase mediana

L <- cortes[clase_mediana_index]                          # Límite inferior de la clase mediana
F_anterior <- ifelse(clase_mediana_index == 1, 0, tabla_de_frecuencia$Frec_Acum[clase_mediana_index - 1])
f_mediana <- tabla_de_frecuencia$Frec_Abs[clase_mediana_index]  # Frecuencia de la clase mediana

mediana_continua <- L + ((n_2 - F_anterior) / f_mediana) * amplitud


# Medidas de dispersión

varianza_continua <- sum(frecuencias * (tabla_de_frecuencia$marca_clase - media_continua)^2) / (n_total - 1)
desvio_continua <- sqrt(varianza_continua)  # Desvío estándar continuo
coef_var_continua <- (desvio_continua / media_continua) * 100  # Coeficiente de variación
                         
# Mostrar resultados agrupados
                         
message("\n* Resultados AGRUPADOS - VARIABLE CONTINUA (", variable_continua, ")")
continua_stats <- data.frame(
Media = round(media_continua, 4),
Moda = round(moda_continua, 4),
Mediana = round(mediana_continua, 4)
)                            
print(continua_stats, row.names = FALSE)


# Cálculo de cuartiles

cuartiles <- quantile(datos[[variable_continua]],probs=c(0.25,0.5,0.75), na.rm=TRUE)
Tabla_Cuartil_Continua <- data.frame(
  Cuartil = c("Q1 (25%)", "Q2 (Mediana)", "Q3 (75%)"),
  Valor = as.numeric(cuartiles)
)

print(Tabla_Cuartil_Continua)



# 4. Histograma de tiempo semanal


# Cargamos la librería
library(ggplot2)

# Extraer y preparar la variable de tiempo
tiempo <- datos$`TIEMPO SEMANAL en HS. DEDIC. EST.`
tiempo <- as.numeric(tiempo)
tiempo <- tiempo[is.finite(tiempo)]


# Construir histograma con ggplot2
ggplot(data.frame(tiempo = tiempo), aes(x = tiempo)) +
  geom_histogram(breaks = brks,
                 closed = "right",       # Intervalos tipo (a, b]
                 fill = "skyblue",
                 color = "white") +
  labs(title = "Histograma de tiempos",
       x = "Tiempo semanal (hs)",
       y = "Frecuencia absoluta") +
  theme_minimal()


# Diagrama circular de satisfacción

library(ggplot2)
library(dplyr)

# Calculamos el porcentaje desde la tabla real
Tabla_final <- Tabla_final %>%
  mutate(Porcentaje = Frecu_Rela * 100)

# Creamos el gráfico circular con ggplot2

ggplot(Tabla_final, aes(x = "", y = Porcentaje, fill = Satisfaccion)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Nivel de Satisfacción con la Carrera") +
  theme_void() +
  theme(legend.position = "right") +
  guides(fill = guide_legend(title = "Categoría"))

