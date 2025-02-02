
# UNIVERSIDAD NACIONAL AUT�NOMA DE M�XICO
# Escuela Nacional de Estudios Superiores Unidad Morelia
# Geohistoria | Estudios Sociales y Gesti�n Local

# Din�mica poblacional
# Medici�n de la fecundidad

# Mario Mart�nez Salgado
# mmartinez@enesmorelia.unam.mx


## TASA GLOBAL DE FECUNDIDAD EN MICHOAC�N.

# 0. Limpiarel �rea de trabajo (Environment)
rm(list = ls())


# 1. Descargar los datos del Censo 2020 de Michoac�n en formato *.csv
# https://www.inegi.org.mx/contenidos/programas/ccpv/2020/microdatos/Censo2020_CA_mich_csv.zip

# 1.1 Descargar el archivo de Descripci�n de la base de datos
#https://www.inegi.org.mx/contenidos/programas/ccpv/2020/microdatos/diccionario_cuestionario_ampliado_cpv2020.xlsx

# 1.2 Crear una carpeta en el escritorio y nombrarla DP

# 1.3 Descomprimir el fichero Censo2020_CA_mich_csv.zip, extraer los archivos y 
#     ponerlos en la carpeta DP. Tambi�n poner en esa carpeta el archivo 
#     Descripci�n de la base de datos.xlsx


# 2. Crear la carpeta DP en el escritorio y definirla como carpeta de trabajo 
#    (Men�: Session > Set Working Directory > Choose Directory...) 
setwd("C:/Users/marius/Desktop/DP")


# 3. Cargar los datos
per <- read.csv("Personas16.csv")
names(per)


# 4. Selecci�n de la poblaci�n y variables de trabajo
per <- per[per$SEXO == 3 & per$EDAD %in% c(15:49), 
           c("ENT","MUN","ID_VIV", "TAMLOC", "FACTOR", "EDAD", "NIVACAD",
             "HIJOS_NAC_VIVOS","FECHA_NAC_A","FECHA_NAC_M")]


# 5. Limpieza de los datos
per$HIJOS_NAC_VIVOS[per$HIJOS_NAC_VIVOS >= 98] <- NA
per$FECHA_NAC_A[per$FECHA_NAC_A == 9999] <- NA
per$FECHA_NAC_M[per$FECHA_NAC_M == 99] <- NA
per$NIVACAD[per$NIVACAD == 99] <- NA


# 6. Identificaci�n de los hijos nacidos entre enero y diciembre de 2019
per$hijo <- NA
per$hijo <- ifelse((per$FECHA_NAC_A == 2019 & 
                      per$FECHA_NAC_M %in% c(1:12)),1,0)
per$hijo[is.na(per$HIJOS_NAC_VIVOS)] <- NA
per$hijo[per$HIJOS_NAC_VIVOS > 0 & 
           (is.na(per$FECHA_NAC_A) | is.na(per$FECHA_NAC_M))] <- NA


# 7. Se excluyen los casos donde no se puede identificar el momento de 
# nacimiento del �ltimo hijo nacido vivo.
per <- per[!is.na(per$hijo),]


# 8. Se ponderan los datos utilizando la variable FACTOR
per$hijo <- per$hijo*per$FACTOR
per$mujer <- per$FACTOR


# 9. Se crea objeto con el n�mero de nacimientos y de mujeres por edad
hijos <- aggregate(per$hijo, by = list(per$EDAD),
                   FUN = sum, na.rm = TRUE)
mujeres <- aggregate(per$mujer, by = list(per$EDAD),
                     FUN = sum, na.rm = TRUE)

datos <- merge(hijos, mujeres, by = c("Group.1"), all = FALSE)
names(datos) <- c("Edad", "nacimientos", "mujeres")


# 10. Se calcula la Tasa Global de Fecundidad por edad desplegada
# y la TGF
datos$Fecundidad <- datos$nacimientos/datos$mujeres

sum(datos$Fecundidad) # TGF

# 10.1 Descargar los tabulados de Fecundidad para Michoac�n calculados con los 
#      datos del Censo 2020 y comparar los resultados
# https://www.inegi.org.mx/contenidos/programas/ccpv/2020/tabulados/ampliado/cpv2020_a_mic_02_fecundidad.xlsx

# 11. Se crea la variable grupo de edad quinquenal para graficar
# las tasas de fecundidad por grupo de edad.
datos$gedad[datos$Edad %in% c(15:19)] <- 1
datos$gedad[datos$Edad %in% c(20:24)] <- 2
datos$gedad[datos$Edad %in% c(25:29)] <- 3
datos$gedad[datos$Edad %in% c(30:34)] <- 4
datos$gedad[datos$Edad %in% c(35:39)] <- 5
datos$gedad[datos$Edad %in% c(40:44)] <- 6
datos$gedad[datos$Edad %in% c(45:49)] <- 7
datos$gedad <- factor(datos$gedad, levels = c(1:7), 
                      labels = c("15-19","20-24","25-29","30-34",
                                 "35-39","40-44","45-49"))

datos.g5 <- aggregate(datos$Fecundidad, by = list(datos$gedad),
                      FUN = sum, na.rm = TRUE)
names(datos.g5) <- c("Edad", "Fecundidad")

# 11.1 (Instalar y) cargar la librer�a 'ggplot2' graficar las tasas
#install.packages("ggplot2")
library(ggplot2)
ggplot(datos.g5, aes(x=Edad, y=Fecundidad, group=1)) +
  geom_line() +
  geom_point() +
  labs(title="Tasas de fecundidad por edad. Michoac�n, 2019", 
       x="Grupo de edad", y="Hijos por mujer")


# 12 Fecundidad por tama�o de localidad
# 12.1 Objeto con el n�mero de hijos y de mujeres por edad
hijos.loc <- aggregate(per$hijo, by = list(per$EDAD, per$TAMLOC),
                       FUN = sum, na.rm = TRUE)
mujeres.loc <- aggregate(per$mujer, by = list(per$EDAD, per$TAMLOC),
                         FUN = sum, na.rm = TRUE)
fecun.loc <- merge(hijos.loc, mujeres.loc, by = c("Group.1", "Group.2"), all = FALSE)
names(fecun.loc) <- c("Edad", "Localidad", "nacimientos", "mujeres")


# 12.2 C�lculo del n�mero de hijos por mujer
fecun.loc$Fecundidad <- fecun.loc$nacimientos/fecun.loc$mujeres


# 12.3 C�lculo de la TGF por tama�o de localidad
tasas.loc <- aggregate(fecun.loc$Fecundidad, by = list(fecun.loc$Localidad),
                       FUN = sum, na.rm = TRUE)
names(tasas.loc) <- c("Localidad", "Tasa")
View(tasas.loc)

# �Qu� significa cada c�digo en la variable 'Localidad'?


# 12.4 Gr�fico de las tasas de fecundidad por edad y tama�o de localidad
per$gedad[per$EDAD %in% c(15:19)] <- 1
per$gedad[per$EDAD %in% c(20:24)] <- 2
per$gedad[per$EDAD %in% c(25:29)] <- 3
per$gedad[per$EDAD %in% c(30:34)] <- 4
per$gedad[per$EDAD %in% c(35:39)] <- 5
per$gedad[per$EDAD %in% c(40:44)] <- 6
per$gedad[per$EDAD %in% c(45:49)] <- 7

hijos.loc2 <- aggregate(per$hijo, by = list(per$gedad, per$TAMLOC),
                        FUN = sum, na.rm = TRUE)
mujeres.loc2 <- aggregate(per$mujer, by = list(per$gedad, per$TAMLOC),
                          FUN = sum, na.rm = TRUE)
fecun.loc2 <- merge(hijos.loc2, mujeres.loc2, by = c("Group.1", "Group.2"), all = FALSE)
names(fecun.loc2) <- c("Edad", "Localidad", "nacimientos", "mujeres")
fecun.loc2$Localidad <- factor(fecun.loc2$Localidad, levels = c(1:5), 
                               labels = c("-2500habs", "2,500-14,999", 
                                          "15mil-49,999","50mil-99,999","+100mil"))
fecun.loc2$Edad <- factor(fecun.loc2$Edad, levels = c(1:7), 
                          labels = c("15-19","20-24","25-29","30-34",
                                     "35-39","40-44","45-49"))

fecun.loc2$Fecundidad <- fecun.loc2$nacimientos/fecun.loc2$mujeres

ggplot(fecun.loc2, aes(x=Edad, y=Fecundidad, group=Localidad)) +
  geom_line(aes(color=Localidad))+
  geom_point(aes(color=Localidad))+
  scale_color_brewer(palette="Dark2") +
  labs(title="Fecundidad por edad y tama�o de localidad \nMichoac�n, 2019", 
       x="Grupo de edad", y="Hijos por mujer")


# 13. Fecundidad por nivel de educaci�n
# 13.1 Crear variable con el nivel de educaci�n

table(per$NIVACAD, useNA = "always")

per$nivesc <- NA
per$nivesc[per$NIVACAD %in% c(0,1)] <- 1 # Nula
per$nivesc[per$NIVACAD %in% c(2,3,6)] <- 2 # B�sica
per$nivesc[per$NIVACAD %in% c(4,5,7,9)] <- 3 # Media 
per$nivesc[per$NIVACAD %in% c(8,10:14)] <- 4 # Superior

table(per$nivesc, useNA = "always")


# 13.2 Objeto con el n�mero de nacimientos y de mujeres por edad y nivel
# de escolaridad
hijos.esc <- aggregate(per$hijo, by = list(per$gedad, per$nivesc),
                       FUN = sum, na.rm = TRUE)
mujeres.esc <- aggregate(per$mujer, by = list(per$gedad, per$nivesc),
                         FUN = sum, na.rm = TRUE)
fecun.esc <- merge(hijos.esc, mujeres.esc, by = c("Group.1", "Group.2"), all = FALSE)
names(fecun.esc) <- c("Edad", "Nivel", "nacimientos", "mujeres")


# 13.3 C�lculo del n�mero de hijos por mujer
fecun.esc$Fecundidad <- fecun.esc$nacimientos/fecun.esc$mujeres*5


# 13.4 C�lculo de la TGF por nivel de educaci�n
tasas.esc <- aggregate(fecun.esc$Fecundidad, by = list(fecun.esc$Nivel),
                       FUN = sum, na.rm = TRUE)
names(tasas.esc) <- c("Nivel", "Tasa")


# 13.5 Gr�fico
fecun.esc$Nivel <- factor(fecun.esc$Nivel, levels = c(1:4), 
                          labels = c("Nula", "B�sica", "Media", "Superior"))

fecun.esc$Edad <- factor(fecun.esc$Edad, levels = c(1:7), 
                         labels = c("15-19","20-24","25-29","30-34",
                                    "35-39","40-44","45-49"))

ggplot(fecun.esc, aes(x=Edad, y=Fecundidad, group=Nivel)) +
  geom_line(aes(color=Nivel)) +
  geom_point(aes(color=Nivel)) +
  scale_color_brewer(palette="Dark2") +
  labs(title="Fecundidad por edad y nivel de educaci�n \nMichoac�n, 2019", 
       x="Grupo de edad", y="Hijos por mujer")


## LABORATORIO

# 1. Replicar todo el ejercicio para el Estado para el que hicieron
#    la pir�mide de poblaci�n, y hacer un an�lisis comparativo de los 
#    resultados con los de Michoac�n.
