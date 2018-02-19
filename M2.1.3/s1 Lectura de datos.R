# Programa Big Data y Business Intelligence
# M2.1.3-Integración de Datos - RStudio
# Enrique Onieva Caracuel

packages <- c("xlsx", "XML","jsonlite","data.table")
new <- packages[!(packages %in% installed.packages()[,"Package"])]

if(length(new)) install.packages(new)
a=lapply(packages, require, character.only=TRUE)


rm(list = ls());cat("\014")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
dir()






#####################################################################
# Parte 01: Lectura de datos con diferentes formatos
# En este ejercicio cargaremos el mismo conjunto de datos almacenado 
# en diferentes formatos, así como evaluaremos los tiempos de carga
# según el formato particular
# Fuente de los datos: http://opendata.euskadi.eus/catalogo/-/calendario-laboral-de-euskadi-2017/
#####################################################################
rm(list=ls())

# Leemos los datos con diferentes funciones y parámetros, según cómo se encuentren almacenados
calendario2017_csv     = read.csv("dat/calendario_laboral_2017.csv",sep=";")
calendario2017_tab     = read.table("dat/calendario_laboral_2017.csv",sep=";",header = TRUE)
calendario2017_xls     = read.xlsx("dat/calendario_laboral_2017.xls", sheetIndex = 1)
calendario2017_xls2    = read.xlsx2("dat/calendario_laboral_2017.xls", sheetIndex = 1)
calendario2017_xml     = xmlToDataFrame("dat/calendario_laboral_2017.xml")
calendario2017_csv_web = read.csv("http://opendata.euskadi.eus/contenidos/ds_eventos/calendario_laboral_2017/opendata/calendario_laboral_2017.csv",sep=";")


# Voy a hacer 5 lecturas de los datos, de cada una de las maneras vistas, y 
# a medir el tiempo que ha tardado cada operación
tiempos=data.frame(csv=numeric(),
                   tab=numeric(),
                   xls=numeric(),
                   xls2=numeric(),
                   xml=numeric(),
                   csvweb=numeric())
for (i in 1:5){
  print(paste("Iteración: ",i))
  inicio=Sys.time()
  calendario2017_csv = read.csv("dat/calendario_laboral_2017.csv",sep=";")
  csv = as.numeric(Sys.time()-inicio)
  
  inicio=Sys.time()
  calendario2017_tab = read.table("dat/calendario_laboral_2017.csv",sep=";",header = TRUE)
  tab = as.numeric(Sys.time()-inicio)
  
  inicio=Sys.time()
  calendario2017_xls = read.xlsx("dat/calendario_laboral_2017.xls", sheetIndex = 1)
  xls = as.numeric(Sys.time()-inicio)
  
  inicio=Sys.time()
  calendario2017_xls2 = read.xlsx2("dat/calendario_laboral_2017.xls", sheetIndex = 1)
  xls2 = as.numeric(Sys.time()-inicio)
  
  inicio=Sys.time()
  calendario2017_xml = xmlToDataFrame("dat/calendario_laboral_2017.xml")
  xml = as.numeric(Sys.time()-inicio)
  
  inicio=Sys.time()
  calendario2017_csv_web = read.csv("http://opendata.euskadi.eus/contenidos/ds_eventos/calendario_laboral_2017/opendata/calendario_laboral_2017.csv",sep=";")
  csvweb = as.numeric(Sys.time()-inicio)
  
  tiempos = rbind(tiempos,data.frame(csv=csv,
                                     tab=tab,
                                     xls=xls,
                                     xls2=xls2,
                                     xml=xml,
                                     csvweb=csvweb))
}

# Y calculo los promedios de cada una de las columnas
colMeans(tiempos)









#####################################################################
# Parte 02: Lectura del formato JSON
# El formato JSON requiere lectura de una manera "menos standar"
# Fuente de datos: http://opendata.euskadi.eus/catalogo/-/recursos-sociosanitarios-de-euskadi/
#####################################################################
rm(list=ls())

# Debo construir la ruta (completa) al fichero
json_file = paste(getwd(),"dat/Mapa_RR_SOSA_BIZKAIA_es.json",sep="/")

# Cargo la información (y obtengo una lista)
recursos_json = fromJSON(txt = json_file)

# Me quedo con el primer elemento (los datos). Puede haber más elementos,
# ya que dentro del json puede haber varias tablas, o una tabla e información adicional
recursos_json = recursos_json[[1]]

# Leemos los mismos datos de maneras "standar"
recursos_csv  = read.csv("dat/Mapa_RR_SOSA_BIZKAIA_es.csv",sep=";")
recursos_xlsx = read.xlsx2("dat/Mapa_RR_SOSA_BIZKAIA_es.xlsx", sheetIndex = 1)
recursos_xls  = read.xlsx2("dat/Mapa_RR_SOSA_BIZKAIA_es.xls", sheetIndex = 1)
# En este caso, por la fuente, no hay la misma información en el JSON que en los otros


# Al igual que antes, voy a medir tiempos...
tiempos=data.frame(csv=numeric(),
                   xls=numeric(),
                   xlsx=numeric(),
                   json=numeric())

for (i in 1:5){
  print(paste("Iteración: ",i))
  inicio=Sys.time()
  recursos_csv  = read.csv("dat/Mapa_RR_SOSA_BIZKAIA_es.csv",sep=";")
  csv = as.numeric(Sys.time()-inicio)
  
  inicio=Sys.time()
  recursos_xlsx  = read.xlsx2("dat/Mapa_RR_SOSA_BIZKAIA_es.xlsx", sheetIndex = 1)
  xlsx = as.numeric(Sys.time()-inicio)
  
  inicio=Sys.time()
  recursos_xls  = read.xlsx2("dat/Mapa_RR_SOSA_BIZKAIA_es.xls", sheetIndex = 1)
  xls = as.numeric(Sys.time()-inicio)
  
  inicio=Sys.time()
  recursos_json = fromJSON(txt = json_file)
  recursos_json = recursos_json[[1]]
  json = as.numeric(Sys.time()-inicio)
  
  tiempos = rbind(tiempos,data.frame(csv=csv,
                                     xls=xls,
                                     xlsx=xlsx,
                                     json=json))
}

# Y calculo los tiempos medios
colMeans(tiempos)







#####################################################################
# Parte 03: read.csv() frente a fread() para ficheros más "Big"
# En esta parte veremos cómo el tamaño de los ficheros, nos puede
# limitar mucho la carga de los mismos. 
# El ser cuidadoso con la elección de funciones, nos puede ayudar a 
# agilizar muchos cálculos
#####################################################################
rm(list=ls())

inicio=Sys.time()
shapes_csv = read.csv("dat/shapes_recortado.csv")
print(paste0("Tiempo usando read.csv: ",(Sys.time()-inicio)))

inicio=Sys.time()
shapes_csv = read.table("dat/shapes_recortado.csv",sep=",")
print(paste0("Tiempo usando read.table: ",(Sys.time()-inicio)))

# fread se encuentra dentro del paquete data.table, que contiene múltiples
# funciones para trabajar de manera "rápida" con grandes volúmenes de datos
inicio=Sys.time()
shapes_fread = fread("dat/shapes_recortado.csv")
print(paste0("Tiempo usando fread: ",(Sys.time()-inicio)))



# También vamos a comprobar (para los más "técnicos") como el uso de un bucle
# para recorrer todas las filas de un dataset no es una buena idea.
# R está pensado (y optimizado) para el trabajo con columnas completas
# (De hecho, lo voy a hacer con pocas filas, para no "romper" R)
shapes = shapes_fread[1:20000,]

inicio=Sys.time()
shapes$nueva = sqrt(shapes$shape_id)
print(paste0("Tiempo trabajando por columna: ",(Sys.time()-inicio)))

inicio=Sys.time()
for (i in nrow(shapes)){
  shapes$nueva[i] = sqrt(shapes$shape_id[i])
}
print(paste0("Tiempo trabajando con un for: ",(Sys.time()-inicio)))










#####################################################################
# Parte 04: Algunos parámetros adicionales a las funciones de lectura
# Con este ejercicio vamos a ver cómo, a través de la lectura, podemos 
# configurar la operación para adaptarse a casi cualquier requisito de los
# datos con los que trabajamos
#####################################################################
rm(list=ls())

# Establecer un separador de columnas particular
dcliente = read.csv("dat/diMEnsion_cliente.csv",sep="\t")
# ¿Qué le pasa, por ejemplo a los nombres? ¿deberían de ser factores?
head(dcliente$Nombre)


# parámetro stringAsFactor
dcliente = read.csv("dat/dimension_cliente.csv",sep="\t",stringsAsFactors = FALSE)
# Ahora bien, pero todavía falla algo (bastante evidente)
head(dcliente$Nombre)


# Definiendo la codificación del fichero de entrada
dcliente = read.csv("dat/dimension_cliente.csv",sep="\t",stringsAsFactors = FALSE,fileEncoding = "UtF-8")
head(dcliente$Nombre)



# Selección de ciertas filas (comenzando desde el principio)
dclienteshort = read.csv("dat/dimension_cliente.csv",sep="\t",stringsAsFactors = FALSE,fileEncoding = "UTF-8",nrows = 2)
dclienteshort$Nombre


# Selección de ciertas filas (en cualquier parte del fichero)
dclienteshort = read.csv("dat/dimension_cliente.csv",sep="\t",stringsAsFactors = FALSE,fileEncoding = "UTF-8",nrows = 2,skip=2,header = FALSE)
dclienteshort$V3
# Ojo con lo que pasa, al saltarnos 2 líneas y decirle que no hay cabecera... (mira la tabla dcliente)


# Para excel, entre otros muchos parámetros, es particularmente interesante especificar el número
# de hoja particular a leer (también se puede hacer por nombre con sheetName)
dcliente = read.xlsx2("dat/Datawarehouse.xlsx",stringsAsFactors = FALSE, sheetIndex = 2)


# En caso de numéricos, podemos establecer el separador decimal
clima = read.csv("dat/clima.csv",sep=";")
class(clima$Temperatura)
mean(clima$Temperatura)
head(clima$Temperatura)

clima = read.csv("dat/clima.csv",sep=";",dec=",")
class(clima$Temperatura)
mean(clima$Temperatura)
# Ojo, que los números suelen dar problemas, sobre todo, cuando se combina el 
# separador decimal "coma" y el "punto" como separador de miles
# (Recordad que en el seminario vimos varias maneras de arreglarlo)




