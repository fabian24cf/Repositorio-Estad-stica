
# 

library(readxl)

archivo <- file.choose()
datos <- read_excel(archivo)
Tabla_Plataforma <- table(datos$Plataforma_Trabajo)
Tabla_Plataforma

tabla <- table(datos$Plataforma_Trabajo)
f_rel <- prop.table(tabla)

Tabla_Frecuencias <- data.frame(
  Plataformas = names(tabla),
  Frecuencias = as.vector(tabla),
  Frecuencias_Rel = round(as.vector(f_rel), 3)
  
)
Tabla_Frecuencias
print(Tabla_Frecuencias, row.names = FALSE)


tabla2 <- table(datos$Tickets_Soporte)
f_acum2 <- cumsum(tabla2)
f_rel2 <- prop.table(tabla2)
f_rel_acum2 <- cumsum(f_rel2)

tabla2

Tabla_Tickets <- data.frame(
  Tickets = names(tabla2),
  Frec = as.vector(tabla2),
  Frecu_rel = round(as.vector(f_rel2), 3),
  Fre_rel_Accum = round(as.vector(f_rel_acum2), 3)
)
Tabla_Tickets


set.seed(123)
tiempos <- round(rnorm(103, mean = 55, sd = 15), 1)
tiempos <- ifelse(tiempos < 0, 0, tiempos)
tiempos

n <- length(tiempos)
k <- ceiling(1 + 3.322 * log(n))
k

rango <- range(tiempos)
amplitud <- ceiling((rango[2] - rango[1]) / k)
rango
amplitud


breaks <- seq(floor(rango[1]), ceiling(rango[2]) + amplitud, by = amplitud)
clases <- cut(tiempos, breaks = breaks, right = FALSE)
head(clases)



tabla3 <- table(clases)
Frec_acum_time <- cumsum(tabla3)
Frec_rel_time <- prop.table(tabla3)
Frec_rel_acum_time <- cumsum(Frec_rel_time)


Tabla_Tiempos <- data.frame(
  Conexion = levels(clases),
  Fr_T = as.vector(tabla3),
  Fr_Acum_T = as.vector(Frec_acum_time),
  Fr_Rel_T = round(as.vector(Frec_rel_time), 3),
  Fr_Rel_Acum_T = round(as.vector(Frec_acum_time), 3)
  
)

Tabla_Tiempos

