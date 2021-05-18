#title: "Proyecto1"
#author: "GRUPO 6"
#date: "22/02/2021"
options(java.parameters = c("-XX:+UseConcMarkSweepGC", "-Xmx8192m"))
library(xlsx)
library(RWeka)
library("ggpubr")
#library(dplyr)    
library(haven)
library(dplyr)
library(tidyverse)
#library(NbClust) #Para determinar el número de clusters óptimo
#library(factoextra) #Para hacer gráficos bonitos de clustering
#library(fpc) #para hacer el plotcluster
#library(cluster) #Para calcular la silueta
library(e1071)
library(mclust)
#library(rpart)
library(caret)
#library(tree)
#library(rpart.plot)
#library(randomForest)


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
rm(x2009)
rm(x2010)
rm(x2011)
rm(x2012)
rm(x2013)
rm(x2014)
rm(x2015)
rm(x2016)
rm(x2017)
rm(x2018)
rm(x2019)

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

rm(datosx2009)
rm(datosx2010)
rm(datosx2011)
rm(datosx2012)
rm(datosx2013)
rm(datosx2014)
rm(datosx2015)
rm(datosx2016)
rm(datosx2017)
rm(datosx2018)
rm(datosx2019)


total <- do.call("rbind", list(totx2009, totx2010, totx2011, totx2012, totx2013, totx2014, totx2015, totx2016, totx2017, totx2018, totx2019))
#summary(total)
#str(total)
rm(totx2009)
rm(totx2010)
rm(totx2011)
rm(totx2012)
rm(totx2013)
rm(totx2014)
rm(totx2015)
rm(totx2016)
rm(totx2017)
rm(totx2018)
rm(totx2019)
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

#la base de datos ha usar es menores
menores<- total %>% filter(Edadm <= 18, Edadm >=10)
rm(total)

menores<- menores %>% filter(Edadp >=10)
completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}
menores<- completeFun(menores, c("Libras", "Onzas","Muprem", "Naciom"))
#lapply(menores,function(x) { length(which(is.na(x)))})
#datos <- datos[,colSums(is.na(datos))==0]
menores$OnzasLib <- menores$Onzas * 0.0625
menores$pesoBebe <- menores$Libras + menores$OnzasLib
menores$ClasPeso <- ifelse(menores$pesoBebe <=5.29109, "BajoPeso", ifelse(menores$pesoBebe <=9.47988, "PesoIdeal", "SobrePeso"))
menores$ClasPeso <- as.factor(menores$ClasPeso)
set.seed(123)
porciento<-0.7
#Se remueve libras, onzas, onzasLib y pesobebe
datos<-subset(menores, select = -c(9,10,46,47) )
datos <- datos[,colSums(is.na(datos))==0]
datos$Paisnacm <-as.factor(datos$Paisnacm)
datos$Escolam <-as.factor(datos$Escolam)
datos$Paisrem <-as.factor(datos$Paisrem)
datos$Escolap <-as.factor(datos$Escolap)
datos$Paisnacp <-as.factor(datos$Paisnacp)
datos$Paisrep <-as.factor(datos$Paisrep)
datos$ViaPar <-as.factor(datos$ViaPar)
datos$TipoIns <-as.factor(datos$TipoIns)
#Eliminacion de ViaPar y TipoIns
datos<-subset(datos, select = -c(4) )
datos<-subset(datos, select = -c(12) )
rm(menores)
datos<-subset(datos, select = -c(1:3,7:9,18,19,22,24,39) )
datos <- datos %>% 
  slice(seq(0.2 * n()))

trainRowsNumber<-sample(1:nrow(datos),porciento*nrow(datos))
train<-datos[trainRowsNumber,]
test<-datos[-trainRowsNumber,]
NB <- make_Weka_classifier("weka/classifiers/functions/MultilayerPerceptron")
NB 
WOW(NB)
nnodos='29'

modelo.bp<-NB(datos$ClasPeso~., data=datos,subset = trainRowsNumber, control=Weka_control(H=nnodos, N=100, G=TRUE), options=NULL)
test$prediccionWeka<-predict(modelo.bp, newdata = test[,1:29])
cfmWeka<-confusionMatrix(test$prediccionWeka,test$ClasPeso)
cfmWeka
table(menores$ClasPeso)
