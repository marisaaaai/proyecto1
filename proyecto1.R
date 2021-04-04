#title: "Proyecto1"
#author: "GRUPO 6"
#date: "22/02/2021"
library(haven)
library(dplyr)
library(tidyverse)
library(NbClust) #Para determinar el número de clusters óptimo
library(factoextra) #Para hacer gráficos bonitos de clustering
library(fpc) #para hacer el plotcluster
library(cluster) #Para calcular la silueta


setwd("C:/Users/LUIS PEDRO/proyecto1")
x2009 <- read_sav("2009.sav")
x2010 <- read_sav("2010.sav")
x2011 <- read_sav("2011.sav")
x2012 <- read_sav("2012.sav")
x2013 <- read_sav("2013.sav")
x2014 <- read_sav("2014.sav")
x2015 <- read_sav("2015.sav")
x2016 <- read_sav("2016.sav")
x2017 <- read_sav("2017.sav")
x2018 <- read_sav("2018.sav")
x2019 <- read_sav("2019.sav")

#Arreglo del dataset de 2009
x2009$Mupreg <- x2009$mupreg
x2009$TipoIns <- 0
x2009$ViaPar <-0
x2009$Paisrep <-9999
x2009$Muprep <- x2009$muprep
x2009$PuebloPP <- x2009$Gretnp
x2009$Paisnacp <- 9999
x2009$Escolap <- 9
x2009$Paisrem <- 9999
x2009$PuebloPM <- x2009$Gretnm
x2009$Paisnacm <- 9999
x2009$Escolam <- 9
x2009$Añoocu <- 2009

#Arreglos del dataset 2010
x2010$Añoocu <- 2010
x2010$Mupreg <- x2010$mupreg
x2010$TipoIns <- 0
x2010$ViaPar <-0
x2010$Paisrep <-9999
x2010$Muprep <- x2010$muprep
x2010$PuebloPP <- x2010$Gretnp
x2010$Paisnacp <- 9999
x2010$Mupnap <- x2010$mupnap
x2010$Paisrem <- 9999
x2010$Mupnam <- x2010$mupnam
x2010$Muprem <- x2010$muprem
x2010$PuebloPM <- x2010$grupetma
x2010$Paisnacm <- 9999

#Arreglo de 2011
x2011$Añoocu <- 2011
x2011$TipoIns <- 0
x2011$ViaPar <-0
x2011$PuebloPP <- x2011$Gretnp
x2011$Paisnacp <- 9999
x2011$Paisrem <- 9999
x2011$PuebloPM <- x2011$grupetma
x2011$Paisnacm <- 9999
x2011$Paisrep <-9999
x2011$Mupnap <- 9999
x2011$Mupnam <- 9999

#Arreglo 2012
x2012$TipoIns <- 0
x2012$ViaPar <-0
x2012$Areag <- 9
x2012$PuebloPP <- x2012$Gretnp
x2012$PuebloPM <- x2012$grupetma
x2012$Mupnam <- x2012$munnam
x2012$Añoocu <- 2012

#Arreglo 2013
x2013$TipoIns <- 0
x2013$ViaPar <-0
x2013$Areag <- 9
x2013$Ocupap <- x2013$Ciuopad
x2013$Ocupam <- x2013$Ciuomad
x2013$Añoocu <- 2013

#Arreglo 2014
x2014$TipoIns <- 0
x2014$ViaPar <-0
x2014$Areag <- 9
x2014$Mupnap <- x2014$Munpnap
x2014$Naciop <- 9999
x2014$Naciom <- 9999
x2014$Añoocu <- 2014
x2014$Ocupam <- x2014$ciuomad
x2014$Añoocu <- 2014

#Arreglo 2015
x2015$Areag <- 9
x2015$Naciop <- 9999
x2015$Naciom <- 9999
x2015$Mupnap <- x2015$Munpnap
x2015$Añoocu <- 2015


#Arreglo 2016
x2016$Areag <- 9
x2016$Naciop <- 9999
x2016$Naciom <- 9999
x2016$Mupnap <- x2016$Munpnap
x2016$Añoocu <- 2016


#Arreglo 2017
x2017$Areag <- 9
x2017$Naciop <- 9999
x2017$Naciom <- 9999
x2017$Mupnap <- x2017$Munpnap
x2017$Añoocu <- 2017


#Arreglo 2018
x2018$Areag <- 9
x2018$Naciop <- 9999
x2018$Naciom <- 9999
x2018$Mupnap <- x2018$Munpnap
x2018$Añoocu <- 2018


#Arreglo 2019
x2019$Areag <- 9
x2019$Naciop <- 9999
x2019$Naciom <- 9999
x2019$Mupnap <- x2019$Munpnap
x2019$Añoocu <- 2019


datosx2009 <- (x2009 %>% select(Depreg, Mupreg, Mesreg, TipoIns, Añoreg, Depocu,Mupocu,Areag,Libras, Onzas, Diaocu, Mesocu, Añoocu, Sexo, Tipar, ViaPar, Edadp, Paisrep,Deprep,Muprep, PuebloPP, Escivp,Paisnacp,Depnap,Mupnap, Naciop, Escolap, Ocupap, Edadm, Paisrem, Deprem, Muprem, PuebloPM, Escivm, Paisnacm, Depnam,Mupnam,Naciom, Escolam, Ocupam, Asisrec, Sitioocu, Tohite, Tohinm, Tohivi))
datosx2010 <- (x2010 %>% select(Depreg, Mupreg, Mesreg, TipoIns, Añoreg, Depocu,Mupocu,Areag,Libras, Onzas, Diaocu, Mesocu, Añoocu, Sexo, Tipar, ViaPar, Edadp, Paisrep,Deprep,Muprep, PuebloPP, Escivp,Paisnacp,Depnap,Mupnap, Naciop, Escolap, Ocupap, Edadm, Paisrem, Deprem, Muprem, PuebloPM, Escivm, Paisnacm, Depnam,Mupnam,Naciom, Escolam, Ocupam, Asisrec, Sitioocu, Tohite, Tohinm, Tohivi))
datosx2011 <- (x2011 %>% select(Depreg, Mupreg, Mesreg, TipoIns, Añoreg, Depocu,Mupocu,Areag,Libras, Onzas, Diaocu, Mesocu, Añoocu, Sexo, Tipar, ViaPar, Edadp, Paisrep,Deprep,Muprep, PuebloPP, Escivp,Paisnacp,Depnap,Mupnap, Naciop, Escolap, Ocupap, Edadm, Paisrem, Deprem, Muprem, PuebloPM, Escivm, Paisnacm, Depnam,Mupnam,Naciom, Escolam, Ocupam, Asisrec, Sitioocu, Tohite, Tohinm, Tohivi))
datosx2012 <- (x2012 %>% select(Depreg, Mupreg, Mesreg, TipoIns, Añoreg, Depocu,Mupocu,Areag,Libras, Onzas, Diaocu, Mesocu, Añoocu, Sexo, Tipar, ViaPar, Edadp, Paisrep,Deprep,Muprep, PuebloPP, Escivp,Paisnacp,Depnap,Mupnap, Naciop, Escolap, Ocupap, Edadm, Paisrem, Deprem, Muprem, PuebloPM, Escivm, Paisnacm, Depnam,Mupnam,Naciom, Escolam, Ocupam, Asisrec, Sitioocu, Tohite, Tohinm, Tohivi))
datosx2013 <- (x2013 %>% select(Depreg, Mupreg, Mesreg, TipoIns, Añoreg, Depocu,Mupocu,Areag,Libras, Onzas, Diaocu, Mesocu, Añoocu, Sexo, Tipar, ViaPar, Edadp, Paisrep,Deprep,Muprep, PuebloPP, Escivp,Paisnacp,Depnap,Mupnap, Naciop, Escolap, Ocupap, Edadm, Paisrem, Deprem, Muprem, PuebloPM, Escivm, Paisnacm, Depnam,Mupnam,Naciom, Escolam, Ocupam, Asisrec, Sitioocu, Tohite, Tohinm, Tohivi))
datosx2014 <- (x2014 %>% select(Depreg, Mupreg, Mesreg, TipoIns, Añoreg, Depocu,Mupocu,Areag,Libras, Onzas, Diaocu, Mesocu, Añoocu, Sexo, Tipar, ViaPar, Edadp, Paisrep,Deprep,Muprep, PuebloPP, Escivp,Paisnacp,Depnap,Mupnap, Naciop, Escolap, Ocupap, Edadm, Paisrem, Deprem, Muprem, PuebloPM, Escivm, Paisnacm, Depnam,Mupnam,Naciom, Escolam, Ocupam, Asisrec, Sitioocu, Tohite, Tohinm, Tohivi))
datosx2015 <- (x2015 %>% select(Depreg, Mupreg, Mesreg, TipoIns, Añoreg, Depocu,Mupocu,Areag,Libras, Onzas, Diaocu, Mesocu, Añoocu, Sexo, Tipar, ViaPar, Edadp, Paisrep,Deprep,Muprep, PuebloPP, Escivp,Paisnacp,Depnap,Mupnap, Naciop, Escolap, Ocupap, Edadm, Paisrem, Deprem, Muprem, PuebloPM, Escivm, Paisnacm, Depnam,Mupnam,Naciom, Escolam, Ocupam, Asisrec, Sitioocu, Tohite, Tohinm, Tohivi))
datosx2016 <- (x2016 %>% select(Depreg, Mupreg, Mesreg, TipoIns, Añoreg, Depocu,Mupocu,Areag,Libras, Onzas, Diaocu, Mesocu, Añoocu, Sexo, Tipar, ViaPar, Edadp, Paisrep,Deprep,Muprep, PuebloPP, Escivp,Paisnacp,Depnap,Mupnap, Naciop, Escolap, Ocupap, Edadm, Paisrem, Deprem, Muprem, PuebloPM, Escivm, Paisnacm, Depnam,Mupnam,Naciom, Escolam, Ocupam, Asisrec, Sitioocu, Tohite, Tohinm, Tohivi))
datosx2017 <- (x2017 %>% select(Depreg, Mupreg, Mesreg, TipoIns, Añoreg, Depocu,Mupocu,Areag,Libras, Onzas, Diaocu, Mesocu, Añoocu, Sexo, Tipar, ViaPar, Edadp, Paisrep,Deprep,Muprep, PuebloPP, Escivp,Paisnacp,Depnap,Mupnap, Naciop, Escolap, Ocupap, Edadm, Paisrem, Deprem, Muprem, PuebloPM, Escivm, Paisnacm, Depnam,Mupnam,Naciom, Escolam, Ocupam, Asisrec, Sitioocu, Tohite, Tohinm, Tohivi))
datosx2018 <- (x2018 %>% select(Depreg, Mupreg, Mesreg, TipoIns, Añoreg, Depocu,Mupocu,Areag,Libras, Onzas, Diaocu, Mesocu, Añoocu, Sexo, Tipar, ViaPar, Edadp, Paisrep,Deprep,Muprep, PuebloPP, Escivp,Paisnacp,Depnap,Mupnap, Naciop, Escolap, Ocupap, Edadm, Paisrem, Deprem, Muprem, PuebloPM, Escivm, Paisnacm, Depnam,Mupnam,Naciom, Escolam, Ocupam, Asisrec, Sitioocu, Tohite, Tohinm, Tohivi))
datosx2019 <- (x2019 %>% select(Depreg, Mupreg, Mesreg, TipoIns, Añoreg, Depocu,Mupocu,Areag,Libras, Onzas, Diaocu, Mesocu, Añoocu, Sexo, Tipar, ViaPar, Edadp, Paisrep,Deprep,Muprep, PuebloPP, Escivp,Paisnacp,Depnap,Mupnap, Naciop, Escolap, Ocupap, Edadm, Paisrem, Deprem, Muprem, PuebloPM, Escivm, Paisnacm, Depnam,Mupnam,Naciom, Escolam, Ocupam, Asisrec, Sitioocu, Tohite, Tohinm, Tohivi))


totx2009 <- datosx2009 %>%
  mutate_if(is.labelled, list(as_factor))
totx2010 <- datosx2010 %>%
  mutate_if(is.labelled, list(as_factor))
totx2011 <- datosx2011 %>%
  mutate_if(is.labelled, list(as_factor))
totx2012 <- datosx2012 %>%
  mutate_if(is.labelled, list(as_factor))
totx2013 <- datosx2013 %>%
  mutate_if(is.labelled, list(as_factor))
totx2014 <- datosx2014 %>%
  mutate_if(is.labelled, list(as_factor))
totx2015 <- datosx2015 %>%
  mutate_if(is.labelled, list(as_factor))
totx2016 <- datosx2016 %>%
  mutate_if(is.labelled, list(as_factor))
totx2017 <- datosx2017 %>%
  mutate_if(is.labelled, list(as_factor))
totx2018 <- datosx2018 %>%
  mutate_if(is.labelled, list(as_factor))
totx2019 <- datosx2019 %>%
  mutate_if(is.labelled, list(as_factor))



total <- do.call("rbind", list(totx2009, totx2010, totx2011, totx2012, totx2013, totx2014, totx2015, totx2016, totx2017, totx2018, totx2019))
#summary(total)
#str(total)
total$Edadm<- as.numeric(factor(total$Edadm))
total$Edadm <- ifelse(total$Edadm >=2000, 0, total$Edadm)
total$Edadp<- as.numeric(factor(total$Edadp))
total$Edadp <- ifelse(total$Edadp >=2000, 0, total$Edadp)
total$Añoreg <- ifelse(total$Añoreg == 9, 2009, ifelse(total$Añoreg == 10, 2010, total$Añoreg))
#Histograma de variables cuant
#año de ocurrencia de embarazo
hist(x= total$Añoocu, main=paste("Histograma de Añoocu"))

#peso de bebe en libras
Librass <- as.numeric(levels(total$Libras))[total$Libras]
hist(x= Librass, main=paste("Histograma de Libras"))
#peso del bebe en onzas
Onzass <- as.numeric(levels(total$Onzas))[total$Onzas]
hist(x= Onzass, main=paste("Histograma de Onzas"))
#total de hijos tenidos
Tohitee <- as.numeric(levels(total$Tohite))[total$Tohite]
hist(x= Tohitee, main=paste("Histograma de Tohite"))
#total de hijos tenidos muertos
Tohinmm <- as.numeric(levels(total$Tohinm))[total$Tohinm]
hist(x= Tohinmm, main=paste("Histograma de Tohinm"), xlim= range(0,5))
#total de hijos tenidos vivos
Tohivii <- as.numeric(levels(total$Tohivi))[total$Tohivi]
hist(x= Tohivii, main=paste("Histograma de Tohivi"), xlim= range(0,15))
#edad del padre
hist(x= total$Edadp, main=paste("Histograma de Edadp"))
#edad de la madre
hist(x= total$Edadm, main=paste("Histograma de Edadm"))

#tablas de frecuencia de variables categoricas
#install.packages("epiDisplay")
library(epiDisplay)
#departamento de registro
tab1(total$Depreg, sort.group = "decreasing")
#municipio de registro
table(total$Mupreg)
#mes de registro
tab1(total$Mesreg, sort.group = "decreasing")
#tipo de inscripcion
tab1(total$TipoIns, sort.group = "decreasing")
#año de registro
tab1(total$Añoreg, sort.group = "decreasing")
#departamento de ocurrencia
tab1(total$Depocu, sort.group = "decreasing")
#municipio de ocurrencia
table(total$Mupocu)
#area geografica de ocurrencia
tab1(total$Areag, sort.group = "decreasing")
#dia de ocurrencia
tab1(total$Diaocu, sort.group = "decreasing")
#mes de ocurrencia
tab1(total$Mesocu, sort.group = "decreasing")
#sexo del bebe
tab1(total$Sexo, sort.group = "decreasing")
#tipo de parto
tab1(total$Tipar, sort.group = "decreasing")
#Via parto
tab1(total$ViaPar, sort.group = "decreasing")
#pais de residencia del padre
table(total$Paisrep)
#departamento de residencia del padre
tab1(total$Deprep, sort.group = "decreasing")
#municipio de residencia del padre
table(total$Muprep)
#Puebloe tnico del padre
tab1(total$PuebloPP, sort.group = "decreasing")
#estado civil del padre
tab1(total$Escivp, sort.group = "decreasing")
#pais de nacimeinto del padre
tab1(total$Paisnacp, sort.group = "decreasing")
#municipio de nacimiento del padre
table(total$Mupnap)
#nacionalidad del padre
tab1(total$Naciop, sort.group = "decreasing")
#escolaridad del padre
tab1(total$Escolap, sort.group = "decreasing")
#ocupacion del padre
table(total$Ocupap)

#Codigo no corrido
#pais residencia de la madre
table(total$Paisrem)
#Departamento residencia de la madre
tab1(total$Deprem, sort.group = "decreasing")
#municipio de residencia de la madre
table(total$Muprem)
#Pueblo etnico de la madre
tab1(total$PuebloPM, sort.group = "decreasing")
#estado civil de la madre
tab1(total$Escivm, sort.group = "decreasing")
#pais de nacimiento de la madre
table(total$Paisnacm)
#departamento de nacimiento de la madre
tab1(total$Depnam, sort.group = "decreasing")
#Municipio de nacimeinto de la madre
table(total$Mupnam)
#nacionalidad de la madre
tab1(total$Naciom, sort.group = "decreasing")
#Escolaridad de la madre
tab1(total$Escolam, sort.group = "decreasing")
#Ocupacion de la madre
table(total$Ocupam)
#Asistencia recibido
tab1(total$Asisrec, sort.group = "decreasing")
#Sitio de ocurrencia de los nacimeintos
tab1(total$Sitioocu, sort.group = "decreasing")

#Filtro de la base de datos
menores<- total %>% filter(Edadm <= 18, Edadm >=10)
menores2 <- total %>% filter(Edadm <=9)
#Cruce de variables
#Cruce de edades
plot(x=menores$Edadm,y=menores$Edadp)
#cruce de pueblo de la madre con hijos tenidos
plot(y=as.numeric(factor(menores$PuebloPM)),x=as.numeric(factor(menores$Tohite)),ylab="Pueblo de la madre", xlab="Total de hijos tenidos")
#de educacion de la madre con hijos tenidos
plot(y=as.numeric(factor(menores$Escolam)),x=as.numeric(factor(menores$Tohite)),ylab="Educacion de la madre", xlab="Total de hijos tenidos")
#Cruce de estado civil del padre y madre
plot(x=as.numeric(factor(menores$Escivp)),y=as.numeric(factor(menores$Escivm)),ylab="Estado civil de la madre", xlab="Estado civil del padre")
#Cruce de estado civil de la madre y pueblo de la madre
plot(x=as.numeric(factor(menores$Escivm)),y=as.numeric(factor(menores$PuebloPM)),ylab="Pueblo de la madre", xlab="Estado civil de la madre")
#Cruce de estado civil de la madre e hijos tenidos
plot(x=as.numeric(factor(menores$Tohite)), y=as.numeric(factor(menores$Escivm)), xlab="Hijos tenidos", ylab="Estado civil de la madre")
#Cruce de estado civil y educacion de la madre
plot(x=as.numeric(factor(menores$Escolam)), y=as.numeric(factor(menores$Escivm)), xlab="Educacion de la madre", ylab="Estado civil de la madre")

#Segundo analisis exploratorio
tab1(menores$Tohinm, sort.group = "decreasing")
tab1(menores2$Tohinm, sort.group = "decreasing")
tab1(menores$Tohivi,sort.group =  "decreasing")
tab1(menores2$Tohivi,sort.group =   "decreasing")
tab1(menores$Edadp,sort.group =  "decreasing")
tab1(menores2$Edadp,sort.group =  "decreasing")
tab1(menores$Escolam,sort.group = "decreasing")
tab1(menores2$Escolam,sort.group = "decreasing")
tab1(menores$Edadm,sort.group = "decreasing")
tab1(menores2$Edadm,sort.group = "decreasing")
#Base de datos a usar es menores para clustering
#Diagrama de codo para saber cuantos grupos hacer

#Diagrama de codo 2009
wss <- (nrow(datosx2009[,c(5,9,10,13,17,29,43,44,45)])-1)*sum(apply(datosx2009[,c(5,9,10,13,17,29,43,44,45)],2,var))

for (i in 2:10) 
  wss[i] <- sum(kmeans(datosx2009[,c(5,9,10,13,17,29,43,44,45)], centers=i)$withinss)

plot(1:10, wss, type="b", xlab="Number of Clusters (2009)",  ylab="Within groups sum of squares")

#Diagrama de codo 2010
wss <- (nrow(datosx2010[,c(5,9,10,13,17,29,43,44,45)])-1)*sum(apply(datosx2010[,c(5,9,10,13,17,29,43,44,45)],2,var))

for (i in 2:10) 
  wss[i] <- sum(kmeans(datosx2010[,c(5,9,10,13,17,29,43,44,45)], centers=i)$withinss)

plot(1:10, wss, type="b", xlab="Number of Clusters (2010)",  ylab="Within groups sum of squares")

#Diagrama de codo 2011
wss <- (nrow(datosx2011[,c(5,9,10,13,17,29,43,44,45)])-1)*sum(apply(datosx2011[,c(5,9,10,13,17,29,43,44,45)],2,var))

for (i in 2:10) 
  wss[i] <- sum(kmeans(datosx2011[,c(5,9,10,13,17,29,43,44,45)], centers=i)$withinss)

plot(1:10, wss, type="b", xlab="Number of Clusters (2011)",  ylab="Within groups sum of squares")


#Diagrama de codo 2012
wss <- (nrow(datosx2012[,c(5,9,10,13,17,29,43,44,45)])-1)*sum(apply(datosx2012[,c(5,9,10,13,17,29,43,44,45)],2,var))

for (i in 2:10) 
  wss[i] <- sum(kmeans(datosx2012[,c(5,9,10,13,17,29,43,44,45)], centers=i)$withinss)

plot(1:10, wss, type="b", xlab="Number of Clusters (2012)",  ylab="Within groups sum of squares")

#Diagrama de codo 2013
wss <- (nrow(datosx2013[,c(5,9,10,13,17,29,43,44,45)])-1)*sum(apply(datosx2013[,c(5,9,10,13,17,29,43,44,45)],2,var))

for (i in 2:10) 
  wss[i] <- sum(kmeans(datosx2013[,c(5,9,10,13,17,29,43,44,45)], centers=i)$withinss)

plot(1:10, wss, type="b", xlab="Number of Clusters (2013)",  ylab="Within groups sum of squares")

#Diagrama de codo 2014
wss <- (nrow(datosx2014[,c(5,9,10,13,17,29,43,44,45)])-1)*sum(apply(datosx2014[,c(5,9,10,13,17,29,43,44,45)],2,var))

for (i in 2:10) 
  wss[i] <- sum(kmeans(datosx2014[,c(5,9,10,13,17,29,43,44,45)], centers=i)$withinss)

plot(1:10, wss, type="b", xlab="Number of Clusters (2014)",  ylab="Within groups sum of squares")
#Diagrama de codo 2015
wss <- (nrow(datosx2015[,c(5,9,10,13,17,29,43,44,45)])-1)*sum(apply(datosx2015[,c(5,9,10,13,17,29,43,44,45)],2,var))

for (i in 2:10) 
  wss[i] <- sum(kmeans(datosx2015[,c(5,9,10,13,17,29,43,44,45)], centers=i)$withinss)

plot(1:10, wss, type="b", xlab="Number of Clusters (2015)",  ylab="Within groups sum of squares")
#Diagrama de codo 2016
wss <- (nrow(datosx2016[,c(5,9,10,13,17,29,43,44,45)])-1)*sum(apply(datosx2016[,c(5,9,10,13,17,29,43,44,45)],2,var))

for (i in 2:10) 
  wss[i] <- sum(kmeans(datosx2016[,c(5,9,10,13,17,29,43,44,45)], centers=i)$withinss)

plot(1:10, wss, type="b", xlab="Number of Clusters (2016)",  ylab="Within groups sum of squares")
#Diagrama de codo 2017
wss <- (nrow(datosx2017[,c(5,9,10,13,17,29,43,44,45)])-1)*sum(apply(datosx2017[,c(5,9,10,13,17,29,43,44,45)],2,var))

for (i in 2:10) 
  wss[i] <- sum(kmeans(datosx2017[,c(5,9,10,13,17,29,43,44,45)], centers=i)$withinss)

plot(1:10, wss, type="b", xlab="Number of Clusters (2017)",  ylab="Within groups sum of squares")
#Diagrama de codo 2018
wss <- (nrow(datosx2018[,c(5,9,10,13,17,29,43,44,45)])-1)*sum(apply(datosx2018[,c(5,9,10,13,17,29,43,44,45)],2,var))

for (i in 2:10) 
  wss[i] <- sum(kmeans(datosx2018[,c(5,9,10,13,17,29,43,44,45)], centers=i)$withinss)

plot(1:10, wss, type="b", xlab="Number of Clusters (2018)",  ylab="Within groups sum of squares")
#Diagrama de codo 2019
wss <- (nrow(datosx2019[,c(5,9,10,13,17,29,43,44,45)])-1)*sum(apply(datosx2019[,c(5,9,10,13,17,29,43,44,45)],2,var))

for (i in 2:10) 
  wss[i] <- sum(kmeans(datosx2019[,c(5,9,10,13,17,29,43,44,45)], centers=i)$withinss)

plot(1:10, wss, type="b", xlab="Number of Clusters (2019)",  ylab="Within groups sum of squares")



#Clustering por k-Means
#Cluster 2009 
datos<-datosx2009
datos2009completos<-datosx2009[complete.cases(datosx2009),]
km<-kmeans(datosx2009[,c(5,9,10,13,17,29,43,44,45)],3,iter.max =100)
datos$grupo<-km$cluster

g1<- datos[datos$grupo==1,]
prop.table(table(g1$Edadm))*100
nrow(g1)
summary(g1)

g2<- datos[datos$grupo==2,]
prop.table(table(g2$Edadm))*100
g3<- datos[datos$grupo==3,]
prop.table(table(g3$Edadm))*100

plotcluster(datosx2009[,c(5,9,10,13,17,29,43,44,45)],km$cluster) #grafica la ubicaciÃ³n de los clusters
#Cluster 2010
datos<-datosx2010
datos2010completos<-datosx2010[complete.cases(datosx2010),]
km<-kmeans(datosx2010[,c(5,9,10,13,17,29,43,44,45)],2,iter.max =100)
datos$grupo<-km$cluster

g1<- datos[datos$grupo==1,]
prop.table(table(g1$Edadm))*100
nrow(g1)
summary(g1)

g2<- datos[datos$grupo==2,]
prop.table(table(g2$Edadm))*100


plotcluster(datosx2010[,c(5,9,10,13,17,29,43,44,45)],km$cluster) #grafica la ubicaciÃ³n de los clusters

#Cluster 2011
datos<-datosx2011
datos2011completos<-datosx2011[complete.cases(datosx2011),]
km<-kmeans(datosx2011[,c(5,9,10,13,17,29,43,44,45)],2,iter.max =100)
datos$grupo<-km$cluster

g1<- datos[datos$grupo==1,]
prop.table(table(g1$Edadm))*100
nrow(g1)
summary(g1)

g2<- datos[datos$grupo==2,]
prop.table(table(g2$Edadm))*100


plotcluster(datosx2011[,c(5,9,10,13,17,29,43,44,45)],km$cluster) 

#Cluster 2012
datos<-datosx2012
datos2012completos<-datosx2012[complete.cases(datosx2012),]
km<-kmeans(datosx2012[,c(5,9,10,13,17,29,43,44,45)],2,iter.max =100)
datos$grupo<-km$cluster

g1<- datos[datos$grupo==1,]
prop.table(table(g1$Edadm))*100
nrow(g1)
summary(g1)

g2<- datos[datos$grupo==2,]
prop.table(table(g2$Edadm))*100


plotcluster(datosx2012[,c(5,9,10,13,17,29,43,44,45)],km$cluster) 
#Cluster 2013
datos<-datosx2013
datos2013completos<-datosx2013[complete.cases(datosx2013),]
km<-kmeans(datosx2013[,c(5,9,10,13,17,29,43,44,45)],2,iter.max =100)
datos$grupo<-km$cluster

g1<- datos[datos$grupo==1,]
prop.table(table(g1$Edadm))*100
nrow(g1)
summary(g1)

g2<- datos[datos$grupo==2,]
prop.table(table(g2$Edadm))*100


plotcluster(datosx2013[,c(5,9,10,13,17,29,43,44,45)],km$cluster) 
#Cluster 2014
datos<-datosx2014
datos2014completos<-datosx2014[complete.cases(datosx2014),]
km<-kmeans(datosx2014[,c(5,9,10,13,17,29,43,44,45)],2,iter.max =100)
datos$grupo<-km$cluster

g1<- datos[datos$grupo==1,]
prop.table(table(g1$Edadm))*100
nrow(g1)
summary(g1)

g2<- datos[datos$grupo==2,]
prop.table(table(g2$Edadm))*100


plotcluster(datosx2014[,c(5,9,10,13,17,29,43,44,45)],km$cluster)
#Cluster 2015
datos<-datosx2015
datos2015completos<-datosx2015[complete.cases(datosx2015),]
km<-kmeans(datosx2015[,c(5,9,10,13,17,29,43,44,45)],2,iter.max =100)
datos$grupo<-km$cluster

g1<- datos[datos$grupo==1,]
prop.table(table(g1$Edadm))*100
nrow(g1)
summary(g1)

g2<- datos[datos$grupo==2,]
prop.table(table(g2$Edadm))*100


plotcluster(datosx2015[,c(5,9,10,13,17,29,43,44,45)],km$cluster)
#Cluster 2016
datos<-datosx2016
datos2016completos<-datosx2016[complete.cases(datosx2016),]
km<-kmeans(datosx2016[,c(5,9,10,13,17,29,43,44,45)],2,iter.max =100)
datos$grupo<-km$cluster

g1<- datos[datos$grupo==1,]
prop.table(table(g1$Edadm))*100
nrow(g1)
summary(g1)

g2<- datos[datos$grupo==2,]
prop.table(table(g2$Edadm))*100


plotcluster(datosx2016[,c(5,9,10,13,17,29,43,44,45)],km$cluster)
#Cluster 2017
datos<-datosx2017
datos2017completos<-datosx2017[complete.cases(datosx2017),]
km<-kmeans(datosx2017[,c(5,9,10,13,17,29,43,44,45)],2,iter.max =100)
datos$grupo<-km$cluster

g1<- datos[datos$grupo==1,]
prop.table(table(g1$Edadm))*100
nrow(g1)
summary(g1)

g2<- datos[datos$grupo==2,]
prop.table(table(g2$Edadm))*100


plotcluster(datosx2017[,c(5,9,10,13,17,29,43,44,45)],km$cluster)
#Cluster 2018
datos<-datosx2018
datos2018completos<-datosx2018[complete.cases(datosx2018),]
km<-kmeans(datosx2018[,c(5,9,10,13,17,29,43,44,45)],2,iter.max =100)
datos$grupo<-km$cluster

g1<- datos[datos$grupo==1,]
prop.table(table(g1$Edadm))*100
nrow(g1)
summary(g1)

g2<- datos[datos$grupo==2,]
prop.table(table(g2$Edadm))*100


plotcluster(datosx2018[,c(5,9,10,13,17,29,43,44,45)],km$cluster)
#Cluster 2019
datos<-datosx2019
datos2019completos<-datosx2019[complete.cases(datosx2019),]
km<-kmeans(datosx2019[,c(5,9,10,13,17,29,43,44,45)],2,iter.max =100)
datos$grupo<-km$cluster

g1<- datos[datos$grupo==1,]
prop.table(table(g1$Edadm))*100
nrow(g1)
summary(g1)

g2<- datos[datos$grupo==2,]
prop.table(table(g2$Edadm))*100


plotcluster(datosx2019[,c(5,9,10,13,17,29,43,44,45)],km$cluster)

#Evaluacion de silueta

#Silueta 2009
silkm<-silhouette(km$cluster,dist(datosx2009[,c(5,9,10,13,17,29,43,44,45)]))
mean(silkm[,3])


