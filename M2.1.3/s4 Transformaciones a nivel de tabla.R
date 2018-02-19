# Programa Big Data y Business Intelligence
# M2.1.3-Integración de Datos - RStudio
# Enrique Onieva Caracuel



packages <- c("lubridate","stringr","dplyr","reshape2","ggplot2","tidyr")
new <- packages[!(packages %in% installed.packages()[,"Package"])]

if(length(new)) install.packages(new)
a=lapply(packages, require, character.only=TRUE)


rm(list = ls());cat("\014")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
dir()






#####################################################################
# Parte 01: Agregaciones
#####################################################################
rm(list=ls())

# Cargo y "arreglo" los datos
datos = read.csv("dat/transaccionesv2.csv",stringsAsFactors = FALSE)
datos$fecha = as.POSIXlt(datos$fecha)
datos$mes  = month(datos$fecha, label = TRUE, abbr = FALSE)
datos$tipo = str_replace(datos$tipo, "es_", "")


# un pequeño (pero necesario) paréntesis de calidad -> valores duplicados
ndup = sum(duplicated(datos, incomparables = FALSE))
rep = duplicated(datos, incomparables = FALSE)
datos = datos[!rep,]

# Para entender cómo funciona duplicated()
duplicated(c(1,1,1,1,2,3,2,2,2,4,4,5))


# vamos a seleccionar sólo 5 tipos de negocios más frecuentes...
# INCLUYE uno de los 5 negocios que m?s movimientos ha tenido(SUE)
frecuencias = data.frame(table(datos$tipo))
negocios    = as.character(frecuencias$Var1[order(frecuencias$Freq,decreasing = TRUE)[1:5]])
filtro      = datos$tipo %in% negocios
datos       = datos[filtro,]
write.csv(datos,"transaccionesv3.csv",row.names = FALSE)






# Agregación simple
agregacion1 = aggregate(importetotal~tipo,data = datos,sum)

# Agregación de varias columnas
agregacion2 = aggregate(cbind(importetotal,transacciones)~tipo,data = datos,sum)

# Agregación en función de varias columnas 
agsum  = aggregate(cbind(importetotal,transacciones)~tipo+mes+cp,data = datos,sum)
View(agsum)

# Agregación con otras funciones...
agmean = aggregate(cbind(importetotal,transacciones)~tipo+mes+cp,data = datos,mean)
agmin  = aggregate(cbind(importetotal,transacciones)~tipo+mes+cp,data = datos,min)
agmax  = aggregate(cbind(importetotal,transacciones)~tipo+mes+cp,data = datos,max)
agsd   = aggregate(cbind(importetotal,transacciones)~tipo+mes+cp,data = datos,sd)


# Agregación por "otras" funciones
agp10  = aggregate(cbind(importetotal,transacciones)~tipo+mes+cp,data = datos,FUN = quantile,0.1)
agp90  = aggregate(cbind(importetotal,transacciones)~tipo+mes+cp,data = datos,FUN = quantile,0.9)


# Con el paquete dplyr -> mayor flexibilidad usando group_by() y summarise()
# El problema es que no permite (fácilmente) datos POSIXlt

ag.tipo = datos %>% select(-fecha) %>% group_by(tipo) %>%
  summarise(media = mean(importetotal),
            mediana = quantile(importetotal,0.5),
            total = sum(importetotal),
            transacctiones = sum(transacciones))

ag.mes = datos %>% select(-fecha) %>% group_by(mes) %>%
  summarise(media = mean(importetotal),
            mediana = quantile(importetotal,0.5),
            total = sum(importetotal),
            transacctiones = sum(transacciones))

ag.mes.tipo = datos %>% select(-fecha) %>% group_by(mes,tipo) %>%
  summarise(media = mean(importetotal),
            mediana = quantile(importetotal,0.5),
            total = sum(importetotal),
            transacctiones = sum(transacciones))










#####################################################################
# Parte 02: Moldeado, fundido y trasposición de tablas
#####################################################################
rm(list=ls())

datos = read.csv("transaccionesv3.csv",stringsAsFactors = FALSE)


# Lo sencillo es cuando moldeamos dejando por filas cosas "únicas"
casttotal = dcast(datos,fecha+cp~tipo, value.var = "importetotal")
casttotal[is.na(casttotal)]=0 #en este caso... 
#por importe total para ese d?a y para ese tipo de negocio(SUE)

# en caso de que no, se puede usar una función de agregación...
castcp0 = dcast(datos,cp~tipo, value.var = "importetotal")

# Porque quiero que agrege si tiene m?s de una dato, calculando lo que quiera(SUE)
castcp1 = dcast(datos,cp~tipo, value.var = "importetotal",fun.aggregate = sum)

# o también se puede hacer un paso previo de agregación...
datos2 = aggregate(importetotal~cp+tipo,data=datos,sum)
castcp2 = dcast(datos2,cp~tipo, value.var = "importetotal")

# Al igual que con la agregación, podemos usar cualquier función de agregado
# Agrego con la media de transacciones(SUE)
castcp3 = dcast(datos,cp~mes, value.var = "transacciones",fun.aggregate = mean)


# Se pueden combinar varios atributos en las columnas
# Tipo mes y codigo postal combinado en una columna(SUE)
castcp4 = dcast(datos,tipo~mes+cp, value.var = "transacciones",fun.aggregate = mean)


# Cast con tidyr (el primo de dplyr), pero antes hay que agregar...
# (...creo que más complejo...)
castdplyr = datos %>%  #AGRUPO POR CODI PSOTAL Y TIPO(sue)
  group_by(cp,tipo) %>% 
  summarise(importetotal=mean(importetotal)) %>%
  spread(tipo,importetotal)




# Fundido de tablas básico, reducir una tabla a 2 columnas
melt0 = melt(castcp0)

# Si se detecta una columna de ID...
melt0 = melt(casttotal)

# Aunque podemos especificarl el ID a usar
melt1 = melt(casttotal,id.vars = "fecha")

# con id fecha y código postal(SUE)
melt2 = melt(casttotal,id.vars = c("fecha","cp"))
melt2$fecha = as.POSIXlt(melt2$fecha, format="%Y-%m-%d")

# Las tablas fundidas son útiles para pintar varias columnas de una vez
ggplot(melt2,aes(x=fecha,y=value))+geom_point()
ggplot(melt2,aes(x=fecha,y=value,col=variable))+geom_line()
ggplot(melt2,aes(x=fecha,y=value,col=variable))+geom_line()+facet_wrap("cp", scales = "free")



melt3 = melt2[melt2$fecha<as.POSIXlt("2015-06-01"),]
ggplot(melt3,aes(x=fecha,y=value,col=variable))+geom_line()+facet_wrap("cp", scales = "free")





# Traspuesta ("Giro" de la tabla)
traspuesta = t(castcp4)

# Como véis, no es una operación "inmediata", y requiere de ciertos retoques a la tabla
colnames(traspuesta)=traspuesta[1,]
traspuesta = data.frame(traspuesta)
traspuesta = traspuesta[2:nrow(traspuesta),]
traspuesta$cp_mes = rownames(traspuesta)
traspuesta = traspuesta[,c(6,1:5)]





#####################################################################
# Parte 03: Unión de tablas ("Joins")
#####################################################################
rm(list=ls())

# Controlando el tipo de join que hacemos...
# (En el ejemplo, no hay ventas que no tengan clientes, para entenderlo mejor... un ejemplo MUY sencillo)
df1 = data.frame(CustomerId = c(1:6), Product = c(rep("Toaster",3), rep("Radio",3)))
df2 = data.frame(CustomerId = c(2,4,6,8,9), State = c(rep("Alabama", 2), rep("Ohio", 3)))
df1
df2

mergeleft  = merge(df1,df2,by="CustomerId",all.x=TRUE ,all.y=FALSE)
mergeleft

mergerigth = merge(df1,df2,by="CustomerId",all.x=FALSE,all.y=TRUE)
mergerigth

mergeouter = merge(df1,df2,by="CustomerId",all.x=TRUE ,all.y=TRUE)
mergeouter

mergeinner = merge(df1,df2,by="CustomerId",all.x=FALSE,all.y=FALSE)
mergeinner


# Mucho cuidado con qué pasa cuando tenemos "ids" repetidos!!
df1 = data.frame(CustomerId = c(1,2,3,1,2,3), Product = c(rep("Toaster",3), rep("Radio",3)))
df2 = data.frame(CustomerId = c(2,4,2,8,9), State = c(rep("Alabama", 2), rep("Ohio", 3)))
df1
df2

mergeleft  = merge(df1,df2,by="CustomerId",all.x=TRUE ,all.y=FALSE)
mergeleft

mergerigth = merge(df1,df2,by="CustomerId",all.x=FALSE,all.y=TRUE)
mergerigth

mergeouter = merge(df1,df2,by="CustomerId",all.x=TRUE ,all.y=TRUE)
mergeouter

mergeinner = merge(df1,df2,by="CustomerId",all.x=FALSE,all.y=FALSE)
mergeinner
