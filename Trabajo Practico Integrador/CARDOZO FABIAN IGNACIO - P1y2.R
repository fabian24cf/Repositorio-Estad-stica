#Cargamos la Libreria "readxl"
library(readxl)

# a). Vamos a determinar la cantidad optima de intervalos a utilizar 

#Empezamos guardando archivo Excel en una Variable y Leyendola
Archivo <- file.choose()
datos <- read_excel(Archivo)

#En una variable guardamos los datos extraidos del vector "TIEMPO SEMANAL en HS. DEDIC. EST"
tiempo <- datos$`TIEMPO SEMANAL en HS. DEDIC. EST.`

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



