# Estadistica para Ingenieros (CO3321)

# Proyecto 1
# Grupo
# Katyuska Coello 07-40767
# Jon Ricchiutti 07-41431
# Luis Esparragoza 08-10337
# Rogelio Chinea 07-40760
# Michael Woo 09-10912

#--- Definicion de Funciones ---#

# Coeficiente de Variacion:
#
# Argumentos:
# x : Vector datos de una variable aleatoria.
#
# Retorna:
# El valor del coeficiente de variacion.
cv = function(x) {
    coeff = sd(x)/mean(x)
    return(coeff)
}

# Concatenación de título
#
# Argumentos:
# variable : Nombre de la variable
# estado : Nombre del estado que represante la variable de estudio
#
# Retorna:
# El titulo del histograma de frecuencias de 'variable' del estado 'estado'.
concat_titulo = function (variable, estado) {
    titulo = paste(paste(paste("Histograma de ",variable, sep=""),
                   ", del estado ",sep=""),estado,sep="")

    return(titulo)
}

# Análisis descriptivo
#
# Argumentos:
# variable : Vector con datos tomados de una muestra aleatoria.
# name_var : Nombre de la variable descrita en el vector de datos aleatorios.
# name_estado : Nombre del estado del cual fue tomada la muestra de datos
#               aleatorios.
#
# Retorna:
# Muestra por pantalla:
# - Histograma de frecuencias.
# - Diagrama de cajas correspondiente.
# - Media.
# - Mediana.
# - El primer cuartil.
# - El tercer cuartil.
# - Desviación Estándar.
# - Coeficiente de Variación.
analisis_descriptivo = function(variable, name_var, name_estado) {
    # Histograma de frecuencia
    par(mfrow=c(1,2))
    hist(variable, main=concat_titulo(name_var,name_estado), 
         xlab=name_var, ylab="Frecuencia",col="green")
    # Gráficos de cajas
    boxplot(variable, col="blue")
    # Media
    md = mean(variable)
    # Mediana
    mdana = median(variable)
    # Primer Cuartil
    q1 = quantile(variable,0.25)
    # Tercer Cuartil
    q3 = quantile(variable,0.75)
    # Desviacion Estándar
    de = sd(variable)
    # Coeficiente de Variación
    coeff = cv(variable)

    return (c(c("Media: ",md),
              c("Mediana: ",mdana),
              c("Primer Cuartil: ",q1),
              c("Tercer Cuartil: ",q3),
              c("Desviación Estándar: ",de),
              c("Coeficiente de Variación: ",coeff)))
}

# Intervalo de Confianza de Media muestral simple.
#
# Argumentos:
# variable : Vector con los valores de una muestra aleatoria, o valores
#            que describen a la variable aleatoria.
# confianza : Nivel de confianza con el que se estimará el intervalo.
#
# Retorna:
# El intervalo de confianza para la media de 'variable' con un nivel de
# confianza de 'confianza'%.
intervalo_conf_simple = function(variable,confianza) {
    n = length(variable)
    #Funcion qt: Calcula el valor de t-student para alfa/2 con n-1 grados de
    #            libertad.
    cuantil = qt((1-confianza)/2,(n-1))
    lim_sup = mean(variable)+(abs(cuantil)*sqrt(var(variable)/n))
    lim_inf = mean(variable)-(abs(cuantil)*sqrt(var(variable)/n))

    return (c(lim_inf,lim_sup))
}

# Intervalo de Confianza para Diferencia de medias muestrales.
# 
# Argumentos:
# var1 : Vector con los valores de una muestra aleatoria.
# var2 : Vector con los valores de una muestra aleatoria.
# confianza : Nivel de confianza con el que se estimará el intervalo.
#
# Retorna:
# El intervalo de confianza para la diferencia de medias de 'var1' y 'var2' con
# un nivel de confianza del 'confianza'%.
intervalo_conf_diff = function(var1, var2, confianza) {
    n1 = length(var1)
    n2 = length(var2)
    cuantil = qt((1-confianza)/2,(n1+n2-2))
    sp = sqrt(((n1-1)*var(var1)^2 + (n2-1)*var(var2)^2)/n1+n2-2)
    xdiff = mean(var1)-mean(var2)
    lim_sup = xdiff+(abs(cuantil)*sp*sqrt(1/n1+1/n2))
    lim_inf = xdiff-(abs(cuantil)*sp*sqrt(1/n1+1/n2))

    return (c(xdiff,lim_inf,lim_sup))
}

# Intervalo de Confianza para Proporciones.
#
# Argumentos:
# variable : Vector con los valores de una muestra aleatoria.
# valor : Valor de comparación para la proporción de datos respecto a él.
# confianza : Nivel de confianza con el que se estimará el intervalo.
#
# Retorna:
# El intervalo de confianza para la proporción de datos de la variable 
# 'variable' respecto a 'valor' con un nivel de confianza del 'confianza'%.
intervalo_conf_prop = function(variable,valor,confianza) {
    # Proporción de acuerdo con el argumento 'valor'
    tablaAux = table(variable)
    p = sum(tablaAux[names(tablaAux)>valor])/length(variable)
    cuantil = qt((1-confianza)/2,(length(variable)-1))
    lim_sup = p+(abs(cuantil)*sqrt((p*(1-p))/length(variable)))
    lim_inf = p-(abs(cuantil)*sqrt((p*(1-p))/length(variable)))
   
    return (c(lim_inf,lim_sup))
}

#--- Carga de datos para operaciones ---#
nebraskaLif = read.table("nebraska_lif.txt",header=TRUE)
texasLif = read.table("texas_lif.txt",header=TRUE)
araHijos = read.table("numhijos/aramus_5.txt")
dcHijos = read.table("numhijos/dcmus_5.txt")
mirHijos = read.table("numhijos/mirmus_5.txt")
zulHijos = read.table("numhijos/zulmus_5.txt")

#Para obtener los resultados de cada pregunta, descomentar el codigo bajo el
#identificador de la pregunta.

#--- Pregunta 1 ---#

# Analisis descriptivo para el estado de Nebraska
#neb = analisis_descriptivo(nebraskaLif[,2],"Tierra dedicada a la actividad agrícola","Nebraska")
#cat("\n\t\tEstado Nebraska\n")
#print(neb)

# Analisis descriptivo para el estado de Texas
#tex = analisis_descriptivo(texasLif[,2],"Tierra dedicada a la actividad agrícola","Texas")
#cat("\n\t\tEstado Texas\n")
#print(tex)

#--- Pregunta 2 ---#

# Intervalo de confianza para las medias muestrales correspondientes a la
# cantidad de tierra dedicada a la actividad agricola para 2007 y 2002. Se
# utilizo una significacion de 90%.
#inCoeff = intervalo_conf_diff(texasLif[,2],texasLif[,3],0.90)
#if (inCoeff[1] > 0) {
#    cat("\nLa media del tamaño de tierras dedicadas a la actividad agrícola",
#        " en el año 2007 es mayor a la del año 2002.\n")
#} else {
#    cat("\nLa media del tamaño de tierras dedicadas a la actividad agrícola",
#        " en el año 2007 es menor a la del año 2002.\n")
#}

#--- Pregunta 3 ---#

#--- Pregunta 4 ---#

# Analisis descriptivo para el estado Aragua
#ara = analisis_descriptivo(araHijos[,1],"Número hijos nacidos vivos","Aragua")
#cat("\n\t\tEstado Aragua\n")
#print(ara)
# Analisis descriptivo para el Distrito Capital
#dc = analisis_descriptivo(dcHijos[,1],"Número hijos nacidos vivos","Distrito Capital")
#cat("\n\t\tDistrito Capital\n")
#print(dc)
# Analisis descriptivo para el estado Miranda
#mir = analisis_descriptivo(mirHijos[,1],"Número hijos nacidos vivos","Miranda")
#cat("\n\t\tEstado Miranda\n")
#print(mir)
# Analisis descriptivo para el estado Zulia
#zul = analisis_descriptivo(zulHijos[,1],"Número hijos nacidos vivos","Zulia")
#cat("\n\t\tEstado Zulia\n")
#print(zul)

# Intervalo de Confianza para el estado Aragua
#cat("\n\tIntervalo de Confianza para el estado Aragua\n")
#print(intervalo_conf_prop(araHijos[,1],3,0.88))
# Intervalo de Confianza para el Distrito Capital
#cat("\n\tIntervalo de Confianza para el Distrito Capital\n")
#print(intervalo_conf_prop(dcHijos[,1],3,0.88))
# Intervalo de Confianza para el estado Miranda
#cat("\n\tIntervalo de Confianza para el estado Miranda\n")
#print(intervalo_conf_prop(mirHijos[,1],3,0.88))
# Intervalo de Confianza para el estado Zulia
#cat("\n\tIntervalo de Confianza para el estado Zulia\n")
#print(intervalo_conf_prop(zulHijos[,1],3,0.88))
