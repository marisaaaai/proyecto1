#title: "Proyecto1"
#author: "GRUPO 6"
#date: "22/02/2021"
library(haven)
library(dplyr)

setwd("C:\Users\Marisa Montoya}\proyecto1")
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
