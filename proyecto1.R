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
library(e1071)
library(mclust)
library(rpart)
library(caret)
library(tree)
library(rpart.plot)
library(randomForest)

#setwd("C:/Users/LUIS PEDRO/proyecto1")
setwd("C:/Users/Marisa Montoya}/proyecto1")
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
view(datosx2011)

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
total$Libras<- as.numeric(factor(total$Libras))
total$Libras <- ifelse(total$Libras >=99, 0, total$Libras)
total$Onzas<- as.numeric(factor(total$Onzas))
total$Onzas <- ifelse(total$Onzas>=99, 0, total$Onzas)
total$Añoocu<- as.numeric(factor(total$Añoocu))
total$Tohite<- as.numeric(factor(total$Tohite))
total$Tohite <- ifelse(total$Tohite >=99, 0, total$Tohite)
total$Tohivi<- as.numeric(factor(total$Tohivi))
total$Tohivi <- ifelse(total$Tohivi >=99, 0, total$Tohivi)
total$Tohinm<- as.numeric(factor(total$Tohinm))
total$Tohinm <- ifelse(total$Tohinm >=99, 0, total$Tohinm)
total$Añoreg <- as.numeric(factor(total$Añoreg))                       
total$Añoreg <- ifelse(total$Añoreg == 9, 2009, ifelse(total$Añoreg == 10, 2010, total$Añoreg))
#view(datosx2009)

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
str(menores)
head(menores)
tail(menores)
any(is.na(menores$Tohivi))



menoresomit1 <- na.omit(menores)
summary(menoresomit1)
any(is.na(menoresomit1))
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

tab1(menores$Deprem,sort.group = "decreasing")
tab1(menores2$Deprem,sort.group = "decreasing")
tab1(menores$Libras,sort.group = "decreasing")
tab1(menores$Onzas,sort.group = "decreasing")
tab1(menores2$Libras,sort.group = "decreasing")
tab1(menores2$Onzas,sort.group = "decreasing")

tab1(menores$Tohite, sort.group = "decreasing")
tab1(menores2$Tohite, sort.group = "decreasing")
#Base de datos a usar es menores para clustering
#Diagrama de codo para saber cuantos grupos hacer

#Diagrama de codo de menores
wss <- (nrow(menoresomit1[,c(5,9,10,13,17,29,43,44,45)])-1)*sum(apply(menoresomit1[,c(5,9,10,13,17,29,43,44,45)],2,var))

for (i in 2:10) 
  wss[i] <- sum(kmeans(menoresomit1[,c(5,9,10,13,17,29,43,44,45)], centers=i)$withinss)

plot(1:10, wss, type="b", xlab="Number of Clusters (menores)",  ylab="Within groups sum of squares")



#Clustering por k-Means
#Cluster menores
datos<-menoresomit1
datosmenorescompletos<-menoresomit1[complete.cases(menoresomit1),]
km<-kmeans(menoresomit1[,c(5,9,10,13,17,29,43,44,45)],2,iter.max =100)
datos$grupo<-km$cluster

g1<- datos[datos$grupo==1,]
prop.table(table(g1$Edadp))*100
nrow(g1)
summary(g1)

g2<- datos[datos$grupo==2,]
prop.table(table(g2$Edadp))*100
nrow(g2)
summary(g2)


plotcluster(menoresomit1[,c(5,9,10,13,17,29,43,44,45)],km$cluster) #grafica la ubicaciÃ³n de los clusters

#la base de datos ha usar es menores
menores<- menores %>% filter(Edadp >=10)

menores$OnzasLib <- menores$Onzas * 0.0625
menores$pesoBebe <- menores$Libras + menores$OnzasLib
menores$ClasPeso <- ifelse(menores$pesoBebe <=5.29109, "BajoPeso", ifelse(menores$pesoBebe <=9.47988, "PesoIdeal", "SobrePeso"))
menores$ClasPeso <- as.factor(menores$ClasPeso)
set.seed(123)
porciento<-0.7
datos<-subset(menores, select = -c(9,10,46,47) )
datos$Paisnacm <-as.factor(datos$Paisnacm)
datos$Escolam <-as.factor(datos$Escolam)
datos$Paisrem <-as.factor(datos$Paisrem)
datos$Escolap <-as.factor(datos$Escolap)
datos$Paisnacp <-as.factor(datos$Paisnacp)
datos$Paisrep <-as.factor(datos$Paisrep)
datos$ViaPar <-as.factor(datos$ViaPar)
datos$TipoIns <-as.factor(datos$TipoIns)
#20 escivp
datos<-subset(datos, select = -c(1:3,8:10,20,21,24,26,41) )
trainRowsNumber<-sample(1:nrow(datos),porciento*nrow(datos))
train<-datos[trainRowsNumber,]
test<-datos[-trainRowsNumber,]

#Arbol de clasificacion
decisiontree <- train(ClasPeso ~ ., data=train, method="rpart", trControl = trainControl(method = "cv"), na.action= na.exclude)
arbolModeloClasificacion<-rpart(ClasPeso~.,train,method = "class")

#NaiveBayes
modelo<-naiveBayes(train$ClasPeso~., data=train)
modelo 
summary(modelo)
predBayes<-predict(modelo, newdata = test[,1:32])
cm<-caret::confusionMatrix(predBayes,test$ClasPeso)
cm #La eficiencia del Naive Bayes es de 0.7734

