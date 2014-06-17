# Estadistica para Ingenieros (CO3321)

# Proyecto 2
# Grupo
# Katyuska Coello 07-40767
# Jon Ricchiutti 07-41431
# Luis Esparragoza 08-10337
# Rogelio Chinea 07-40760
# Michael Woo 09-10912

#--- Definición de Funciones ---#

# Prueba de Hipótesis para diferencia de medias (con tamaño de muestra mayor
# a 30 y varianzas desconocidas)
#
# Argumentos:
# muestra1 : Vector que contiene valores tomados de una muestra aleatoria.
# muestra2 : Vector que contiene valores tomados de una muestra aleatoria.
# alfa : Nivel de significación con el que se realizará la prueba.
#
# Retorna:
# Muestra por pantalla el resultado de la prueba de hipótesis:
# Si es rechazada o no la hipótesis nula.
ph_difMedias = function(muestra1,muestra2,alfa) {
    # Cálculo de tamaño de muestras
    n1 = length(muestra1)
    n2 = length(muestra2)
    # Cálculo de medias muestrales
    med1 = mean(muestra1)
    med2 = mean(muestra2)
    # Cálculo de varianzas muestrales
    var1 = var(muestra1)
    var2 = var(muestra2)

    # Cálculo de estadístico de prueba
    ep = (med1-med2)/sqrt((var1^2/n1)+(var2^2/n2))

    # Cálculo de la región de rechazo: valor crítico para una prueba de 
    # hipótesis bilateral
    z = qnorm(alfa/2)
    intervalo = c(-z,z)

    # Impresión de resultados
    impresion_resultados(ep,intervalo,alfa)
}

# Prueba de Hipótesis para diferencia de medias con información del p-valor.
# Tamaño de muestra mayor a 30 y varianzas desconocidas.

#ph_difMediasPValor = function()

# Realiza una prueba de hipotésis para:
# H0 : Los datos del argumento 'muestra' provienen de una distribución Normal.
# Ha : Los datos del argumento 'muestra' no provienen de una distribución
#      Normal.
#
# Nota: Al poseer como datos la media y la varianza no es necesario restar dos
#       grados de libertad al momento de buscar el valor críticio para
#       ji-cuadrado.
#
# Argumentos:
#
# Retorna:
prueba_normal = function(muestra,alfa,media,varianza) {
    # Definición de intervalos
    intervalo = c(140)
    marcaClase = c()
    for (i in 1:35) {
        intervalo = c(intervalo,intervalo[i]+1.3)
        marcaClase = c(marcaClase,intervalo[i]+0.65)
    }
    
    # Construcción de la tabla de frecuencias
    tablaFrecuencias = cbind(table(cut(muestra[,1],intervalo)))
    
    # Construcción del estadístico de prueba
    frecuencias = tablaFrecuencias[,1]
    k = length(frecuencias)
    nDatos = sum(frecuencias)
    desvEstandar = sqrt(varianza)
    # REVISAR Y PREGUNTAR LO QUE VIENE A CONTINUACION
    probabilidad = pnorm(intervalo[2:35],media,desvEstandar)-
                   pnorm(intervalo[1:34],media,desvEstandar)
    estadistico = sum((frecuencias-(nDatos*probabilidad))^2/
                      (nDatos*probabilidad))

    # Cálculo del valor crítico para la prueba ji-cuadrada
    ji = qchisq(1-alfa,k-1)

    # Impresión de resultados y conclusiones
    if (estadistico > ji) {
        cat("El estadístico de prueba: ",estadistico," es mayor al valor ",
            "crítico: ",ji,", por lo cual pertenece a la región de rechazo: ",
            "(",ji,",Inf). Para un nivel de significación de: ",alfa," se ",
            "rechaza la hipótesis nula: Los datos no provienen de una ",
            "distribución normal.\n")
    } else {
         cat("El estadístico de prueba: ",estadistico," es menor al valor ",
            "crítico: ",ji,", por lo cual no pertenece a la región de ",
            "rechazo: (",ji,",Inf). Para un nivel de significación de: ",
            alfa," no se rechaza la hipótesis nula: Los datos provienen de ",
            "una distribución normal.\n")
    }
}

# Impresión de resultados para una prueba de hipótesis de diferencia de medias.
#
# Argumentos:
# estadistico : Estadistico de prueba relativo a la prueba de hipótesis
#               realizada.
# intervalo : Intervalo que representa la Región de Rechazo de la prueba de
#             hipótesis realizada.
# alfa : Nivel de significación con el que se realiza la prueba.
# 
# Retorna:
# Muestra por pantalla si el estadístico de prueba pertenece o no a la
# región de rechazo de la prueba. Concluyendo si la hipótesis nula es rechazada
# o no.
impresion_resultados = function(estadistico,intervalo,alfa) {
    cat("Para un nivel de significancia de ",alfa,":\n\n")
    if(estadistico < intervalo[1] || estadistico > intervalo[2]) {
        cat("El estadistico de prueba ",ep," está en la RR por pertenecer a ",
            "la región (-Inf,",intervalo[1],") U (",intervalo[2],",Inf), ",
            "por lo tanto se rechaza la hipótesis nula.\n")
    } else {
        cat("El estadistico de prueba ",ep," no está en la RR por no ",
            "pertenecer a la región (-Inf,",intervalo[1],") U (",
            intervalo[2],",Inf), por lo tanto no se rechaza la hipótesis ",
            "nula.\n")   
    }
}

# Mientras más pequeño el p-valor mas fuerte es la evidencia de que se debe
# rechazar la hipótesis nula. Si el nivel de significancia alfa es mayor o
# igual al p-valor obtenido, la hipotesis nula es rechazada.

#--- Carga de datos ---#

#Datos pregunta 1
aragua = read.table("datos/aramus.txt")
dCapital = read.table("datos/dcmus.txt")
miranda = read.table("datos/mirmus.txt")
zulia = read.table("datos/zulmus.txt")
#Datos pregunta 2
alturas = read.table("datos/alturas.txt")
#Datos pregunta 3
autos = read.table("datos/autos.txt",header=TRUE,row.names=1)
