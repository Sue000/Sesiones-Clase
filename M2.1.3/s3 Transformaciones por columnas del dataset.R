# Programa Big Data y Business Intelligence
# M2.1.3-Integración de Datos - RStudio
# Enrique Onieva Caracuel

packages <- c("reshape2","lubridate","stringr","arules","ggplot2","clusterSim","dplyr")
new <- packages[!(packages %in% installed.packages()[,"Package"])]

if(length(new)) install.packages(new)
a=lapply(packages, require, character.only=TRUE)


rm(list = ls());cat("\014")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
dir()



#####################################################################
# Parte 01: Seleccionando filas y columnas
# En esta sección vamos a aprender a seleccionar o filtrar, en base
# a algún criterio determinadas filas/columnas de nuestros datos
#####################################################################
rm(list=ls())

cliente = read.csv("dat/dimension_cliente.csv",sep="\t",fileEncoding = "UTF-8")
twitter = read.csv("Twitter.csv")



# Selección de determinadas columnas (consecutivas)
seleccion = cliente[,1:3]
head(seleccion)

# Selección de determinadas columnas (NO consecutivas)
seleccion = cliente[,c(1,4:6,10)]
head(seleccion)

# Selección por nombre de la columna
seleccion = cliente[,c("idCliente","Nombre","NIF")]
head(seleccion)

# Descartar columnas
seleccion = cliente[,-(1:10)]
head(seleccion)

seleccion = cliente[,c(-1,-2,-4,-6,-8,-10,-11,-12,-13)]
head(seleccion)


# Selección con paquete dplyr
seleccion = cliente %>% select(idCliente,NIF,Nombre)
head(seleccion)




# Selección de columnas según un criterio sobre sus valores
criterio = substring(colnames(cliente), 1,1)=="C"
seleccion = cliente[,criterio]
head(seleccion)

# La "clave" es que criterio tiene que tener tantos TRUE/FALSE 
# como COLUMNAS haya en los datos
criterio = sapply(twitter, is.numeric)
seleccion = twitter[,criterio]
head(seleccion)

criterio = apply(twitter,2,function(x) length(unique(x))) > 1
seleccion = twitter[,criterio]
head(seleccion)


# Selección con dplyr
seleccion = twitter %>% select_if(is.numeric)
# Si el criterio es "más complejo", mejor no hacerlo con dplyr



# seleccion de filas
# Igual que antes, la "clave" es que criterio tiene que tener 
# tantos TRUE/FALSE como FILAS haya en los datos
criterio  = cliente$Sexo=="H"
seleccion = cliente[criterio,]

criterio  = cliente$idCliente<50
seleccion = cliente[criterio,]





#####################################################################
# Parte 02: Operando en función de valores en columnas
# Aquí vamos a trabajar la creación y modificación de columnas en función
# de filtros sobre los valores de otras columnas de los datos
#####################################################################

# Asignación de valores en función de otras columna
cliente$nuevacolumna = "Mujer"
criterio = cliente$Sexo=="H"
cliente[criterio,"nuevacolumna"] = "Hombre"
cliente$nuevacolumna[criterio] = "Hombre"
head(cliente[,c("Sexo","nuevacolumna")])

# De la misma manera 
cliente$nuevacolumna = ifelse(cliente$Sexo=="H","Hombre","Mujer")
# o con el paquete dplyr
cliente = cliente %>% 
  mutate(nuevacolumna = ifelse(Sexo=="H","Hombre","Mujer"))


# Particionado de una tabla en función de un atributo
clientepartido = split(cliente,cliente$Sexo)
hombres = clientepartido[[1]]
mujeres = clientepartido[[2]]
head(hombres)
head(mujeres)


# Partiendo y calculando algo en cada parte
clientepartido = split(cliente,cliente$Comunidad)
for (parte in clientepartido){
  comunidad = parte$Comunidad[1]
  hombres = sum(parte$Sexo=="H")
  mujeres = sum(parte$Sexo=="M")
  print(paste0("En ",comunidad," hay ",mujeres," Mujeres y ",hombres," Hombres"))
  write.csv(parte,paste0("clientes_",comunidad,".csv"),row.names = FALSE)
}

# Mutando con el paquete dplyr
cliente = cliente %>% 
  group_by(Comunidad) %>% 
  mutate(texto=paste0("En ",Comunidad," hay ",sum(Sexo=="M")," Mujeres y ",sum(Sexo=="H")," Hombres"))







#####################################################################
# Parte 03: Transformaciones por columnas
#####################################################################
rm(list=ls())

cliente = read.csv("dat/dimension_cliente.csv",sep="\t",fileEncoding = "UTF-8")

# Unión de columnas
cliente$Apellidos = paste(cliente$Apellido1,cliente$Apellido2)
cliente[1:5,c("Apellido1","Apellido2","Apellidos")]

cliente$Nombrecompleto = paste(cliente$Apellidos,cliente$Nombre,sep=", ")
cliente[1:5,c("Apellidos","Nombre","Nombrecompleto")]

# En algunos casos, más útil es la función paste0
cliente$Nombrecompleto = paste0(cliente$Apellido1," ",cliente$Apellido2,", ",cliente$Nombre)
cliente[1:5,c("Apellido1","Apellido2","Nombre","Nombrecompleto")]

# comando mutate de dplyr
# añade una columna nueva Nombrecompleto que tenga como resultado....(SUE)
cliente = cliente %>% 
  mutate(Nombrecompleto = paste0(Apellido1," ",Apellido2,", ",Nombre))






# División de columnas
partido         = colsplit(cliente$Correo,"@",names=c("nick","dominio"))
cliente$nick    = partido$nick
cliente$dominio = partido$dominio
cliente[1:5,c("Correo","nick","dominio")]









#####################################################################
# Parte 04: El formato fecha
# La librería lubridate es la más utilizada para el trabajo con fechas.
# Hay que tener en cuenta que de una fecha, se puede extraer gran
# cantidad de información.
# Para trabajar con ello, tenemos que convertor al formato POSIXlt.
#####################################################################
rm(list=ls())

twitter = read.csv("Twitter.csv")
cliente = read.csv("dat/dimension_cliente.csv",sep="\t",fileEncoding = "UTF-8")

twitter$fecha1 = as.POSIXlt(twitter$tiempo, format="%Y-%m-%d %H:%M")
head(twitter[,c("tiempo","fecha1")])

cliente$fecha1 = as.POSIXlt(cliente$Nacimiento, format="%d/%m/%Y")
head(cliente[,c("Nacimiento","fecha1")])

# Hay algunas funciones predefinidas para extraer información
twitter$mesnum    = month(twitter$fecha1)
twitter$mesnom    = month(twitter$fecha1,label=TRUE)
twitter$mesnomcom = month(twitter$fecha1,label=TRUE,abbr=FALSE)
head(twitter[,c("tiempo","mesnum","mesnom","mesnomcom")])


twitter$diames           = mday(twitter$fecha1)
twitter$diasemananum     = wday(twitter$fecha1)
twitter$diasemananom     = wday(twitter$fecha1,label=TRUE)
twitter$diasemananomcom  = wday(twitter$fecha1,label=TRUE,abbr=FALSE)
twitter$diaanio          = yday(twitter$fecha1)
head(twitter[,c("tiempo","diames","diasemananum","diasemananom","diasemananomcom","diaanio")])


twitter$anio = year(twitter$fecha1)
twitter$semana = week(twitter$fecha1)
twitter$minuto = minute(twitter$fecha1)
twitter$hora = hour(twitter$fecha1)
head(twitter[,c("tiempo","anio","semana","hora","minuto")])



# También podemos cambiar el formato de la fecha (buscad la documentación de strptime)
twitter$fecha2 = format(twitter$fecha1,"%d/%m/%y")
twitter$fecha3 = format(twitter$fecha1,"%d/%m/%Y")
twitter$fecha4 = format(twitter$fecha1,"Día %j del año estelar %Y")
head(twitter[,c("fecha1","fecha2","fecha3","fecha4")])





# Podemos restar fechas... 
# (hay que ordenarlo por la fecha antes)
twitter = twitter[order(twitter$fecha1,decreasing = TRUE),]

# luego creamos una columna (auxiliar) con la fecha inmediatamente siguiente
twitter$fecha1_ant = c(twitter$fecha1[-1],twitter$fecha1[nrow(twitter)])
head(twitter[,c("fecha1","fecha1_ant")])

# y basta con restar ambas columnas
twitter$dif_ant    = difftime(twitter$fecha1,twitter$fecha1_ant,units="hours")
head(twitter[,c("fecha1","fecha1_ant","dif_ant")])









#####################################################################
# Parte 05: Expresiones regulares
# En ocasiones, puede ser interesante buscar patrones dentro del 
# texto para comprobar determinadas cosas, por ejemplo, si un 
# número de teléfono tiene 9 dítitos
#####################################################################
rm(list=ls())

cliente = read.csv("dat/dimension_cliente.csv",sep="\t",fileEncoding = "UTF-8",stringsAsFactors = FALSE)

# Comprobación de si una cadena cumple una expresión regular
cliente$tieneun2 = str_detect(cliente$NIF, "2")
cliente$empiezapor2 = str_detect(cliente$NIF, "^2")
cliente$tiene9cosas = str_detect(cliente$NIF, "[[:alnum:]]{9}")
cliente$dnicorrecto = str_detect(cliente$NIF, "[[:digit:]]{8}[[:alpha:]]")
cliente$contenidocorrecto = str_detect(cliente$NIF, "[[:digit:]]{8}([[:punct:]]|[[:space:]])?[[:upper:]]")

head(cliente[,c("NIF","tieneun2","empiezapor2","tiene9cosas","dnicorrecto","contenidocorrecto")],10)

# Más info... https://stat.ethz.ch/R-manual/R-devel/library/base/html/regex.html
# https://www.cheatography.com/davechild/cheat-sheets/regular-expressions/
# Podéis experimentar con Expresiones regulares en el siguiente enlace: https://regexr.com/
# 


# Reemplazamiento basado en expresiones regulares
cliente$NIF2 = str_replace(cliente$NIF,"([[:digit:]]{8})([[:space:]])?([[:alpha:]])", "\\1-\\3")
cliente$NIF3 = str_replace(cliente$NIF2, "^([[:digit:]]{7})([[:space:]])?([[:alpha:]])", "0\\1-\\3")
cliente$NIF4 = str_to_upper(cliente$NIF3)
head(cliente[,c("NIF","NIF2","NIF3","NIF4")],10)








#####################################################################
# Parte 06: Discretización, normalización, dummy variables...
#####################################################################
rm(list=ls())

clima = read.csv("dat/clima.csv",sep=";",stringsAsFactors = FALSE,dec=",")

# Discretización
clima$Prec_int = discretize(clima$Precipitaciones,categories = 3,method="interval")
clima$Prec_fre = discretize(clima$Precipitaciones,categories = 3,method="frequency")
clima$Prec_clu = discretize(clima$Precipitaciones,categories = 3,method="cluster")
clima$Prec_fix = discretize(clima$Precipitaciones,categories = c(0,20,40,Inf),method="fixed")

ggplot(data=clima, aes(x=Precipitaciones,y=1,col=Prec_int)) + geom_jitter() + ggtitle("Discretización por Intervalo")
ggplot(data=clima, aes(x=Precipitaciones,y=1,col=Prec_fre)) + geom_jitter() + ggtitle("Discretización por Frecuencia")
ggplot(data=clima, aes(x=Precipitaciones,y=1,col=Prec_clu)) + geom_jitter() + ggtitle("Discretización por Cluster")
ggplot(data=clima, aes(x=Precipitaciones,y=1,col=Prec_fix)) + geom_jitter() + ggtitle("Discretización Fija")

# Interesante: se le pueden dar nombres a los cortes
clima$Prec_fix_name = discretize(clima$Precipitaciones, categories = c(0,20,40,Inf), method = "fixed", labels = c("Poco","Moderado","Mucho"))






# Normalización
clima$Prec_nor = data.Normalization(clima$Precipitaciones,type="n4",normalization="column")
clima$Prec_sta = data.Normalization(clima$Precipitaciones,type="n1",normalization="column")

summary(clima[,c("Prec_nor","Prec_sta")])






# Dummy variables
dummies = model.matrix(~clima$Prec_int)
head(dummies)

# Ha creado una variable "inútil", y sólo N-1 niveles 
# (truco para que que te genere todos los niveles)
dummies = model.matrix(~clima$Prec_int+0)
head(dummies)

clima = cbind(clima,dummies)

