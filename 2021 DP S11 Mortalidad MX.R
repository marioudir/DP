
# UNIVERSIDAD NACIONAL AUTÓNOMA DE MÉXICO
# Escuela Nacional de Estudios Superiores Unidad Morelia
# Geohistoria | Estudios Sociales y Gestión Local

# Dinámica poblacional
# Mortalidad en México en 2020

# Mario Martínez Salgado
# mmartinez@enesmorelia.unam.mx


# 0. Preliminares

# (Des)cargar paquetes que vamos a usar

#install.packages("foreign") 
#install.packages("ggplot2") 
#install.packages("tidyverse") 
#install.packages("stringr") 

library(foreign)
library(ggplot2)
library(tidyverse)
library(stringr)

# Limpiar área de trabajo

rm(list = ls())


# Definir DP como carpeta de trabajo

# Menú: Session > Set Working Directory > Choose Directory... 

setwd("C:/Users/marius/Desktop/DP")


# 1. DESCARGA DE INFORMACIÓN ####

# 1.1 Defunciones
# INEGI: registros de mortalidad de 2020
# inegi.org.x > DATOS > Programas > Reg Admin > Vitales > Mortalidad > ...
# Microtados > Def registradas (mort gral) > 2020

# o poner el siguiente enlace en el buscador:
# https://www.inegi.org.mx/contenidos/programas/mortalidad/microdatos/defunciones/2020/defunciones_base_datos_2020_dbf.zip

# 1.2  Población y nacimientos
# CONAPO: Conciliación Demográfica de México: Población a mitad de año y, 
# fecundidad y nacimientos (1950-2050).

# Poner los siguientes enlaces en el buscador: 
# http://www.conapo.gob.mx/work/models/CONAPO/Datos_Abiertos/Proyecciones2018/pob_mit_proyecciones.csv
# http://www.conapo.gob.mx/work/models/CONAPO/Datos_Abiertos/Proyecciones2018/tef_nac_proyecciones.csv

# 1.3 Descomprimir los archivos y guardarlos en la carpeta DP del Escritorio


## 2. CARGAR LOS DATOS A LA SESIÓN DE TRABAJO ####

# 2.1 Defunciones ocurridas en 2020

def.reg20 <- read.dbf("DEFUN20.dbf")

# Nota al pie: en 2018 se registraron 722,611 defunciones y en 2019 747,784.


# Variables en la base de datos

names(def.reg20)


# Año de ocurrencia de las muertes registradas en 2020

table(def.reg20$ANIO_OCUR, useNA = "always")

# 98.46% de las muertes registradas en 2020 ocurrieron en 2020.


# Para el análisis nos quedamos con las muertes ocurridas y registradas en 2020,
# y sólo con algunas variables

def.mx <- def.reg20[def.reg20$ANIO_OCUR == 2020,
                    c("ENT_OCURR", "MUN_OCURR", "SEXO", "EDAD", "EDAD_AGRU", 
                      "CAUSA_DEF", "RAZON_M", "MATERNAS", "PRESUNTO")]


# 2.2 Población a mitad de año y nacimientos

pob <- read.csv("pob_mit_proyecciones.csv")
head(pob)

nac <- read.csv("tef_nac_proyecciones.csv")
head(nac)

# Población en el país y nacimientos (pob$CVE_GEO == 0) en 2020 (pob$AÑO == 2020)

pob.mx <- pob[pob$CVE_GEO == 0 & pob$AÑO == 2020,]
nac.mx <- nac[nac$CVE_GEO == 0 & nac$AÑO == 2020,]

# Población estimada
sum(pob.mx$POBLACION, na.rm = TRUE)

# Nacimientos estimados
sum(nac.mx$NACIMIENTOS, na.rm = TRUE)

# Nota al pie: el Censo de 2020 contó 126,014,024 personas; y en 2020 sólo se 
# registraron 1,629,211 nacimientos (en 2019 fueron 2,092,214)


## 3. TASA BRUTA DE MORTALIDAD ####

# 3.1 Variable para conteo de defunciones
def.mx$cont <- 1

# 3.2 Numerador y denominador
def.mx.tot <- sum(def.mx$cont)
pob.mx.tot <- sum(pob.mx$POBLACION)

# 3.3 Tasa
TBM <- def.mx.tot/pob.mx.tot*1000
round(TBM, digits = 1)

# 3.4 Lectura: 
# En 2020 la tasa de mortalidad en México fue de 8.37 muertes
# por cada mil habitantes.

# ¿Cómo podemos saber cuál es TBM de Michoacán en 2020?



## 4. TASA DE MORTALIDAD POR EDAD ESPECÍFICA ####

# 4.1 Defunciones por edad
table(def.mx$EDAD, useNA = "always")

# En la página 8 (14 del pdf) del documento "Descripcion_BD_Defunciones_2020" 
# se describe la variable EDAD. 

# ¿Qué consideraciones hay que tener para trabajar con esta variable?

# Recodificación de la variable edad
def.mx$EDAD[def.mx$EDAD == 4998] <- NA
def.mx$EDAD[def.mx$EDAD < 4000] <- 4000
def.mx$EDAD <- def.mx$EDAD - 4000 # ¿Por qué restar 4000 a EDAD?

# Resultado
table(def.mx$EDAD, useNA = "always")


# 4.2 Cálculo de la tasa de mortalidad del grupo 40 a 44 años
def.mx.g4044 <- sum(def.mx$cont[def.mx$EDAD %in% c(40:44)])
pob.mx.g4044 <- sum(pob.mx$POBLACION[pob.mx$EDAD %in% c(40:44)])

TM.g4044 <- def.mx.g4044/pob.mx.g4044*1000
round(TM.g4044, digits = 1)

# 4.3 Calcular la TM para el grupo de edad 70 a 74

def.mx.g7074 <- sum(def.mx$cont[def.mx$EDAD %in% c(70:74)])
pob.mx.g7074 <- sum(pob.mx$POBLACION[pob.mx$EDAD %in% c(70:74)])

TM.g7074 <- def.mx.g7074/pob.mx.g7074*1000
round(TM.g7074, digits = 1)


# 4.4 Lectura:


# 4.5 Gráfico de la tasa de mortalidad por grupo de edad

# Insumos: 
# Defunciones por grupo de edad
table(def.mx$EDAD_AGRU, useNA = "always")

# NOTA 1: La variable EDAD_AGRU no es numérica, es un vector de caracteres 
# o un factor. Lo sabemos por la presencia de los códigos "01" a "09".
class(def.mx$EDAD_AGRU)

# Para facilitar la operación con esta variable la redefinimos
# como vector numérico
def.mx$gedad <- as.numeric(def.mx$EDAD_AGRU)

# NOTA 2: ¿A qué grupo de edad representa cada código? 
# Respuesta: ver página 17 (23 del pdf) de la Descripción de la base de datos
def.mx$gedad[def.mx$gedad <= 5] <- 5
def.mx$gedad[def.mx$gedad %in% c(23:29)] <- 23
def.mx$gedad[def.mx$gedad == 30] <- NA

table(def.mx$gedad)

def.mx.ged <- aggregate(def.mx$cont, 
                        by = list(def.mx$gedad),
                        FUN = sum, na.rm = TRUE)
head(def.mx.ged)

# Población por grupo de edad
pob.mx$gedad <- as.numeric(cut(pob.mx$EDAD,
                               c(-1,4,9,14,19,24,29,34,39,44,
                                 49,54,59,64,69,74,79,84,89))) 
head(pob.mx)
pob.mx$gedad <- pob.mx$gedad + 4 # ¿Por qué sumo 4 a gedad?
pob.mx$gedad[is.na(pob.mx$gedad)] <- 23 # NA's forman código 23 (90 y +)

pob.mx.ged <- aggregate(pob.mx$POBLACION, by = list(pob.mx$gedad), 
                        FUN = sum, na.rm = TRUE)

mx.ged <- merge(def.mx.ged, pob.mx.ged, 
                by = c("Group.1"), all = FALSE)

names(mx.ged) <- c("edad", "def", "pob")

mx.ged$TM <- mx.ged$def/mx.ged$pob*1000

mx.ged$edad <- factor(mx.ged$edad, levels = c(5:23),
                      labels = c("0-4","5-9","10-14","15-19","20-24",
                                 "25-29","30-34","35-39","40-44","45-49",
                                 "50-54","55-59","60-64","65-69","70-74",
                                 "75-79","80-84","85-89","90 y +"))
head(mx.ged)

ggplot(mx.ged, aes(x = edad, y = TM, group = 1)) +
  geom_line() +
  geom_point() +
  labs(title = "Tasas de mortalidad por edad \nMéxico, 2020", 
       x = "Edad", y = "Defunciones por mil habitantes")


# 4.6 ¿Hay diferencias en la TM por sexo?

def.mx.sex <- aggregate(def.mx$cont, 
                        by = list(def.mx$gedad, def.mx$SEXO), 
                        FUN = sum, na.rm = TRUE)

pob.mx.sex <- aggregate(pob.mx$POBLACION, 
                        by = list(pob.mx$gedad, pob.mx$SEXO), 
                        FUN = sum, na.rm = TRUE)

head(def.mx.sex)
head(pob.mx.sex)

pob.mx.sex$Group.2[pob.mx.sex$Group.2 == "Hombres"] <- 1
pob.mx.sex$Group.2[pob.mx.sex$Group.2 == "Mujeres"] <- 2

mx.sex <- merge(def.mx.sex, pob.mx.sex,
                by = c("Group.1", "Group.2"), 
                all = FALSE)

names(mx.sex) <- c("edad", "sexo", "def", "pob")

mx.sex$TM <- mx.sex$def/mx.sex$pob*1000

mx.sex$edad <- factor(mx.sex$edad, levels = c(5:23),
                      labels = c("0-4","5-9","10-14","15-19","20-24",
                                 "25-29","30-34","35-39","40-44","45-49",
                                 "50-54","55-59","60-64","65-69","70-74",
                                 "75-79","80-84","85-89","90 y +"))

mx.sex$sexo <- factor(mx.sex$sexo,
                      levels = c(1:2), 
                      labels = c("Hombres","Mujeres"))

ggplot(mx.sex, aes(x = edad, y = TM, group = sexo)) +
  geom_line(aes(color = sexo)) +
  geom_point(aes(color = sexo)) +
  labs(title = "Tasas de mortalidad por edad y sexo \nMéxico, 2020", 
       x = "Edad", y = "Defunciones por mil habitantes")



## 5. PORCENTAJE DE MUERTES POR CAUSAS ESPECÍFICAS ####

# 5.1 Causas de defunción
table(def.mx$CAUSA_DEF, useNA = "always")

# En la página 7 (13 del pdf) se describe la variable CAUSA_DEF. 
# ¿Qué consideraciones hay que tener para trabajar con esta variable?

# 5.2 Catálogo de la Clasificación Internacional de Enfermedades
catalogo <- read.dbf("CATMINDE.dbf")
head(catalogo)
names(catalogo)[1] <- "CAUSA_DEF"


# 5.3 Muertes por diabetes mellitus

causa <- filter(catalogo, str_detect(DESCRIP, "mellitus"))

# Nota al pie: Según INEGI Suicidio = lesiones autoinfligidas. Buscar 
# causas con la palabra "autoinfligid"


# 5.3.1 Variable para conteo de defunciones
causa$diabetes <- 1

# 5.4 Pegado de variable que identifica las muertes por diabetes mellitus
def.mx <- merge(def.mx, causa, by = c("CAUSA_DEF"), all = TRUE)

# 5.5 Total de muertes por diabetes mellitus
diabetes <- sum(def.mx$diabetes, na.rm = TRUE)

# 5.6 Porcentaje
PMCE.diabetes <- diabetes/def.mx.tot*100
round(PMCE.diabetes, digits = 1)

# 5.7 Lectura:

# 5.8 Tasa de mortalidad por causa específica
TMCE.diabetes <- diabetes/pob.mx.tot*100000
round(TMCE.diabetes, digits = 1)


## 6. TASA DE MORTALIDAD INFANTIL ####

# 6.1 Cálculo de la tasa
pob.ed0 <- sum(nac.mx$NACIMIENTOS, na.rm = TRUE)
def.ed0 <- sum(def.mx$cont[def.mx$EDAD < 1], na.rm = TRUE)

TMI <- def.ed0/pob.ed0*1000
round(TMI, digits = 1)

# 6.2 Lectura: 


## 7. RAZÓN DE MORTALIDAD MATERNA ####

# 7.1 Defunciones maternas. 
# En la página 20 (26 del pdf) se describe la variable RAZON_M. 
table(def.mx$RAZON_M, useNA = "always")

# 7.2 Cálculo del indicador
def.mx.mat <- sum(def.mx$RAZON_M, na.rm = TRUE)

RMM <- def.mx.mat/pob.ed0*100000
round(RMM, digits = 1)

# 7.4 Lectura:


## 8. TBM POR ESTADO ####

# 8.1 Insumos: defunciones y población

# Defunciones

def.ent <- aggregate(def.mx$cont,
                     by = list(def.mx$ENT_OCURR),
                     FUN = sum, na.rm = TRUE)

head(def.ent)

def.ent$Group.1 <- as.numeric(def.ent$Group.1)


# Población

entidades <- pob[pob$CVE_GEO > 0 & pob$AÑO == 2020,]

pob.ent <- aggregate(entidades$POBLACION,
                     by = list(entidades$CVE_GEO),
                     FUN = sum, na.rm = TRUE)

head(pob.ent)


# Objeto con las defunciones y población por edad

def.pob.ent <- merge(def.ent, pob.ent, by = c("Group.1"), all.y = TRUE)

names(def.pob.ent) <- c("CVE_ENT", "muertes", "poblacion")

def.pob.ent$TBM <- def.pob.ent$muertes/def.pob.ent$poblacion*1000


# 8.2. Mapa temático: TBM por entidad

#install.packages("sp")
#install.packages("rgdal")
#install.packages("RColorBrewer")
#install.packages("classInt")

library(sp)
library(rgdal)
library(RColorBrewer)
library(classInt)

# Cargar mapa

mx.map <- NULL
mx.map <- readOGR(".", "estados_sorted",
                  stringsAsFactors = FALSE, GDAL1_integer64_policy = TRUE)

mx.map@data

# Pegar datos
mx.map <- merge(mx.map, def.pob.ent, by.x = "CVE_ENT")

#Vistazo a los datos
mx.map@data

# Mapa con 5 categorías
colores <- brewer.pal(5, "OrRd")

spplot(mx.map,"TBM", 
       main = "Tasa Bruta de Mortalidad por Entidad. México, 2020",
       col.regions = colores, cuts = 4) 

