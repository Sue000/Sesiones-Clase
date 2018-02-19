# Programa Big Data y Business Intelligence
# M2.1.3-Integración de Datos - RStudio
# Enrique Onieva Caracuel

packages <- c("XML","dplyr","ggplot2","ggrepel","rvest","stringr","tidyr")
new <- packages[!(packages %in% installed.packages()[,"Package"])]

if(length(new)) install.packages(new)
a=lapply(packages, require, character.only=TRUE)


rm(list = ls());cat("\014")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
dir()




#####################################################################
# Parte 01: Tomando tablas de internet
# Con este ejercicio aprenderemos a leer tablas directamente colgadas
# en páginas webs en R. Para poder trabajar con ellas, o almacenarlas
# para su tratamiento con otras herramientas
#####################################################################
rm(list=ls())

# Datos sobre aerolíneas
url   = "http://www.theacsi.org/index.php?option=com_content&view=article&id=147&catid=&Itemid=212&i=Airlines"
aerolineas = readHTMLTable(url, header=TRUE,which=1,stringsAsFactors=F)


# Clasificación de la liga
url   = "http://www.marca.com/futbol/primera/clasificacion.html"

clasificacion = readHTMLTable(url,which=1,stringsAsFactors=FALSE)
# Observa la tabla que hemos obtenido y razona por qué la ha "tomado" así
# Odviamente, todo tiene arreglo..
colnames(clasificacion) = clasificacion[2,]
clasificacion = clasificacion[-c(1,2),]



# Si no usas el which, te devuelve todas las tablas que encuentra... habría que ver cuál queremos...
# Para ello, podéis explorar el código html de la web, o explorar las tablas obtenidas en una
# primera lectura (posiblemente mucho más sencillo)
url   = "http://www.marca.com/estadisticas/futbol/primera/?cid=MENUMIGA35903&s_kw=estadisticas"
liga = readHTMLTable(url, header=TRUE,stringsAsFactors=FALSE)

tabla1= liga[[1]]
tabla2= liga[[2]]
# y, en este caso, Habría que arreglarle los encabezados








#####################################################################
# Parte 02: Leyendo y concatenando datos "iguales"
# Muchas veces, no queremos leer sólo una página/tabla/dataset
# Aquí veremos como leer datos que están "partidos" en diferentes sitios
#####################################################################
rm(list=ls())

# Lista de urls de las que descargar las clasificaciones
urls = c("http://www.marca.com/estadisticas/futbol/primera/2003_04/jornada_38/",
         "http://www.marca.com/estadisticas/futbol/primera/2004_05/jornada_38/",
         "http://www.marca.com/estadisticas/futbol/primera/2005_06/jornada_38/",
         "http://www.marca.com/estadisticas/futbol/primera/2006_07/jornada_38/",
         "http://www.marca.com/estadisticas/futbol/primera/2007_08/jornada_38/")

# De una manera profesional, mediante uso de la familia apply y do.call
datos = lapply(urls,readHTMLTable,header=TRUE,stringsAsFactors=FALSE,which=2,encoding = "UTF-8")
datos = do.call(rbind,datos)
View(datos)

# otra manera "menos nativa", pero "más flexible", ya que nos permitirá un procesamiento
# individualizado de cada tabla
# Va copiando el año de cada tabla, y lo pega en una nueva columna liga(SUE)
datos=c()
for (i in urls){
  print(i)
  parte = readHTMLTable(i,header=TRUE,stringsAsFactors=FALSE,which=2,encoding = "UTF-8")
  parte$liga = substring(i, 50,53)
  datos = rbind(datos,parte)
}
View(datos)
# De igual manera, pero iterando, además, por la web a explorar
# Esta operación dependerá mucho del formato... 
# Construye URL de forma dinámica (SUE)
datos=c()
for (i in 2000:2015){
  url = paste0("http://www.marca.com/estadisticas/futbol/primera/",i,"_",substring(as.character(i+1),3,4),"/jornada_38/")
  print(url)
  parte = readHTMLTable(url,header=TRUE,stringsAsFactors=FALSE,which=2,encoding = "UTF-8")
  parte$liga = i
  datos = rbind(datos,parte)
}
# y guardamos los resultados
write.csv(datos, "liga.csv",row.names = FALSE)

# Podemos trabajar con ellos
# Hago una tabla resumen(SUE)
colnames(datos)[1] = "clasificacion"
colnames(datos)[2] = "equipo"
colnames(datos)[3] = "p.jugados"
colnames(datos)[4] = "p.ganados"
colnames(datos)[5] = "p.empatados"
colnames(datos)[6] = "p.perdidos"
colnames(datos)[7] = "g.favor"
colnames(datos)[8] = "g.contra"
colnames(datos)[9] = "puntos"

datos[,-2] = apply(datos[,-2],2,as.numeric)

resumen = datos %>% group_by(equipo) %>% 
  summarise(ligas = n(),
            puntos = mean(puntos),
            goles = mean(g.favor)) %>%
  filter(ligas>1)

ggplot(resumen,aes(x=puntos,y=goles,size=ligas,col=ligas)) + 
  geom_point() + geom_text_repel(aes(label=equipo))




# De igual manera que hacemos con una lista de URLs, podemos hacer con una lista de ficheros.
# En este caso, con todos los ficheros que contienen la palatra "tweet" en su nombre
# Datos de mi cuenta de Twitter descargados de https://analytics.twitter.com/ 
# (Sólo nos da datos de cuentas de las que somos propietarios)
files = list.files(pattern = "tweet", path = "./dat", full.names = TRUE)
datos = lapply(files, read.csv, encoding = "UTF-8")
datos = do.call(rbind,datos)
write.csv(datos,"Twitter.csv",row.names = FALSE)
View(datos)









#####################################################################
# Parte 03: Scrappeando webs (más avanzado)
# El scrapeado de datos consiste en la obtención de datos directamente de 
# la web.
# Estos datos nos pueden ser muy útiles en muchos proyectos, ya que nos
# permitirá obtener datos de multitud de fuentes.
# Usaremos el la extensión SelectorGadget de Chrome:
# https://chrome.google.com/webstore/detail/selectorgadget/mhjhnkcfbdhnjickkkdbjoemdmbfginb
#####################################################################


# Scrapeando Google Scholar
rm(list=ls())
url = "https://scholar.google.es/citations?user=2ww8fYYAAAAJ&hl=es"

# Esta web "no tiene una tabla" per se, como los casos anteriores, así que el siguiente código fallará
res = readHTMLTable(url)

# Por eso, tenemos que usar "otro enfoque"
# Que busque sobre las etiquetas que he buscado(SUE)
todo = read_html(url)

anio   = todo %>% html_nodes("#gsc_a_b .gsc_a_h") %>% html_text()
citas  = todo %>% html_nodes(".gsc_a_ac")         %>% html_text()
titulo = todo %>% html_nodes(".gsc_a_at")         %>% html_text()
# También obtendríamos lo mismo con algo como: anio = html_text(html_nodes(todo,".gsc_a_y"))
# Lo junto todo en un dataframe(SUE)
datos = data.frame(anio,citas,titulo)
write.csv(datos, "scholar.csv",row.names = FALSE)


# También podemos utilizar el comando html_table
# Que mire dentro de todo, qué tablas hay(SUE)
tabla = todo %>% html_table()
tabla1 = tabla[[1]]
tabla2 = tabla[[2]]
View(tabla1)
View(tabla2)



# Scrapeando meneame.net
rm(list=ls())
url = "https://www.meneame.net/"
todo = read_html(url)

titular     = todo %>% html_nodes("h2 a")                     %>% html_text(trim = TRUE)
resumen     = todo %>% html_nodes("#newswrap .news-content")  %>% html_text(trim = TRUE)
comentarios = todo %>% html_nodes("#newswrap .comments")      %>% html_text(trim = TRUE)
meneos      = todo %>% html_nodes("#newswrap .votes a")       %>% html_text(trim = TRUE)
fuente      = todo %>% html_nodes("h2 a")                     %>% html_attr("href")
# (El scrapeo de un enlace es un poco diferente, al escrapeo del texto)

datos = data.frame(titular,resumen,comentarios,meneos,fuente,stringsAsFactors = FALSE)
write.csv(datos, "meneame.csv",row.names = FALSE)

# Vamos a ver qué es lo más comentado/meneado
datos$comentarios = str_replace(datos$comentarios,"comentarios","")
datos$comentarios = as.numeric(datos$comentarios)
datos$meneos = as.numeric(datos$meneos)

datos[which.max(datos$comentarios),]
datos[which.max(datos$meneos),]


