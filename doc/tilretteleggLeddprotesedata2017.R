rm(list = ls())
library(indikatoR)
setwd('E:/Felles SKDE/Eksterne oppdrag til SKDE/Indikatorprosjekt_2016/Analyse/Figurer og R-koder brukt i rapporten/R-kode/indikatoR/doc/')

library(xlsx)
library(tidyr)


########### Hofteprotese 2014 #####################################

protesedata <- xlsx::read.xlsx('E:/Felles SKDE/Eksterne oppdrag til SKDE/Indikatorprosjekt_2016/2017_Oppdatering/Mottatt data/Leddprotese/hofte2014.xlsx',
                       1, startRow=4)

names(protesedata)[c(1,3)] <- c('bohf', 'aldersgruppe')
protesedata$bohf <- as.character(protesedata$bohf)
protesedata$bohf <- iconv(protesedata$bohf, from = 'UTF-8', to = '')
protesedata$aldersgruppe <- as.character(protesedata$aldersgruppe)

# Fyll inn manglende HF
for (i in which(!is.na(protesedata$bohf))) {
  protesedata$bohf[(i+1):(i+4)] <- protesedata$bohf[i]
}

# Fjern total
protesedata <- protesedata[-which(is.na(protesedata$aldersgruppe)), ]
protesedata <- protesedata[ , -c(2, 6)]
# protesedata <- protesedata[-which(protesedata$bohf=='Total'), ]
protesedata$bohf[which(protesedata$bohf=='Total')] <- 'Norge'

# Tallkoder for aldersgrupper
konvAldgr <- data.frame(ald_gr_ny=1:4, ald_gr_gml=c('40-62', '63-69', '70-75', '>=76'))
protesedata$aldersgruppe <- konvAldgr$ald_gr_ny[match(protesedata$aldersgruppe, konvAldgr$ald_gr_gml)]

# Kjønn som variabel ermann med tallverdi 0 og 1
protesedata <- gather(protesedata, ermann, antall, 3:4)
protesedata$ermann[protesedata$ermann == 'Kvinne'] <- 0
protesedata$ermann[protesedata$ermann == 'Mann'] <- 1
protesedata$ermann <- as.numeric(protesedata$ermann)

# Legg til variabel for år
protesedata$aar <- 2014

Hofte2014 <- protesedata[, c(5,1,3,2,4)]

########### Hofteprotese 2015 #####################################

protesedata <- xlsx::read.xlsx('E:/Felles SKDE/Eksterne oppdrag til SKDE/Indikatorprosjekt_2016/2017_Oppdatering/Mottatt data/Leddprotese/hofte2015.xlsx',
                               1, startRow=4)

names(protesedata)[c(1,3)] <- c('bohf', 'aldersgruppe')
protesedata$bohf <- as.character(protesedata$bohf)
protesedata$bohf <- iconv(protesedata$bohf, from = 'UTF-8', to = '')
protesedata$aldersgruppe <- as.character(protesedata$aldersgruppe)

# Fyll inn manglende HF
for (i in which(!is.na(protesedata$bohf))) {
  protesedata$bohf[(i+1):(i+4)] <- protesedata$bohf[i]
}

# Fjern total
protesedata <- protesedata[-which(is.na(protesedata$aldersgruppe)), ]
protesedata <- protesedata[ , -c(2, 6)]
# protesedata <- protesedata[-which(protesedata$bohf=='Total'), ]
protesedata$bohf[which(protesedata$bohf=='Total')] <- 'Norge'

# Tallkoder for aldersgrupper
konvAldgr <- data.frame(ald_gr_ny=1:4, ald_gr_gml=c('40-62', '63-69', '70-75', '>=76'))
protesedata$aldersgruppe <- konvAldgr$ald_gr_ny[match(protesedata$aldersgruppe, konvAldgr$ald_gr_gml)]

# Kjønn som variabel ermann med tallverdi 0 og 1
protesedata <- gather(protesedata, ermann, antall, 3:4)
protesedata$ermann[protesedata$ermann == 'Kvinne'] <- 0
protesedata$ermann[protesedata$ermann == 'Mann'] <- 1
protesedata$ermann <- as.numeric(protesedata$ermann)

# Legg til variabel for år
protesedata$aar <- 2015

Hofte2015 <- protesedata[, c(5,1,3,2,4)]

########### Hofteprotese 2016 #####################################

protesedata <- xlsx::read.xlsx('E:/Felles SKDE/Eksterne oppdrag til SKDE/Indikatorprosjekt_2016/2017_Oppdatering/Mottatt data/Leddprotese/hofte2016.xlsx',
                               1, startRow=4)

names(protesedata)[c(1,3)] <- c('bohf', 'aldersgruppe')
protesedata$bohf <- as.character(protesedata$bohf)
protesedata$bohf <- iconv(protesedata$bohf, from = 'UTF-8', to = '')
protesedata$aldersgruppe <- as.character(protesedata$aldersgruppe)

# Fyll inn manglende HF
for (i in which(!is.na(protesedata$bohf))) {
  protesedata$bohf[(i+1):(i+4)] <- protesedata$bohf[i]
}

# Fjern total
protesedata <- protesedata[-which(is.na(protesedata$aldersgruppe)), ]
protesedata <- protesedata[ , -c(2, 6)]
# protesedata <- protesedata[-which(protesedata$bohf=='Total'), ]
protesedata$bohf[which(protesedata$bohf=='Total')] <- 'Norge'

# Tallkoder for aldersgrupper
konvAldgr <- data.frame(ald_gr_ny=1:4, ald_gr_gml=c('40-62', '63-69', '70-75', '>=76'))
protesedata$aldersgruppe <- konvAldgr$ald_gr_ny[match(protesedata$aldersgruppe, konvAldgr$ald_gr_gml)]

# Kjønn som variabel ermann med tallverdi 0 og 1
protesedata <- gather(protesedata, ermann, antall, 3:4)
protesedata$ermann[protesedata$ermann == 'Kvinne'] <- 0
protesedata$ermann[protesedata$ermann == 'Mann'] <- 1
protesedata$ermann <- as.numeric(protesedata$ermann)

# Legg til variabel for år
protesedata$aar <- 2016

Hofte2016 <- protesedata[, c(5,1,3,2,4)]

########### Hofte samlet ######################################


Hofteproteser <- rbind(Hofte2014, Hofte2015, Hofte2016)

bohfnr <- data.frame(bohfnr=c(1,2,3,4,6,7,8,10,11,12,13,14,15,16,19,20,21,22,23,9999),
                     bohfnavn= c('Finnmark','UNN','Nordland','Helgeland','Nord-Trøndelag','St.Olavs',
                                 'Møre og Romsdal','Førde','Bergen','Fonna','Stavanger','Østfold','Akershus',
                                 'Oslo','Innlandet','Vestre Viken','Vestfold','Telemark','Sørlandet','Norge'))

Hofteproteser$bohfnr <- bohfnr$bohfnr[match(Hofteproteser$bohf, bohfnr$bohfnavn)]
Hofteproteser <- Hofteproteser[, c(1,2,6,3,4,5)]

write.csv2(Hofteproteser, 'Hofteproteser2014-1016.csv', row.names = F)



########## Kneproteser ########################


########### Kneprotese 2014 #####################################

protesedata <- xlsx::read.xlsx('E:/Felles SKDE/Eksterne oppdrag til SKDE/Indikatorprosjekt_2016/2017_Oppdatering/Mottatt data/Leddprotese/kne2014.xlsx',
                               1, startRow=4)

names(protesedata)[c(1,3)] <- c('bohf', 'aldersgruppe')
protesedata$bohf <- as.character(protesedata$bohf)
protesedata$bohf <- iconv(protesedata$bohf, from = 'UTF-8', to = '')
protesedata$aldersgruppe <- as.character(protesedata$aldersgruppe)

# Fyll inn manglende HF
for (i in which(!is.na(protesedata$bohf))) {
  protesedata$bohf[(i+1):(i+4)] <- protesedata$bohf[i]
}

# Fjern total
protesedata <- protesedata[-which(is.na(protesedata$aldersgruppe)), ]
protesedata <- protesedata[ , -c(2, 6)]
# protesedata <- protesedata[-which(protesedata$bohf=='Total'), ]
protesedata$bohf[which(protesedata$bohf=='Total')] <- 'Norge'

# Tallkoder for aldersgrupper
konvAldgr <- data.frame(ald_gr_ny=1:4, ald_gr_gml=c('40-61', '62-68', '69-74', '>=75'))
protesedata$aldersgruppe <- konvAldgr$ald_gr_ny[match(protesedata$aldersgruppe, konvAldgr$ald_gr_gml)]

# Kjønn som variabel ermann med tallverdi 0 og 1
protesedata <- gather(protesedata, ermann, antall, 3:4)
protesedata$ermann[protesedata$ermann == 'Kvinne'] <- 0
protesedata$ermann[protesedata$ermann == 'Mann'] <- 1
protesedata$ermann <- as.numeric(protesedata$ermann)

# Legg til variabel for år
protesedata$aar <- 2014

Kne2014 <- protesedata[, c(5,1,3,2,4)]


########### Kneprotese 2015 #####################################

protesedata <- xlsx::read.xlsx('E:/Felles SKDE/Eksterne oppdrag til SKDE/Indikatorprosjekt_2016/2017_Oppdatering/Mottatt data/Leddprotese/kne2015.xlsx',
                               1, startRow=4)

names(protesedata)[c(1,3)] <- c('bohf', 'aldersgruppe')
protesedata$bohf <- as.character(protesedata$bohf)
protesedata$bohf <- iconv(protesedata$bohf, from = 'UTF-8', to = '')
protesedata$aldersgruppe <- as.character(protesedata$aldersgruppe)

# Fyll inn manglende HF
for (i in which(!is.na(protesedata$bohf))) {
  protesedata$bohf[(i+1):(i+4)] <- protesedata$bohf[i]
}

# Fjern total
protesedata <- protesedata[-which(is.na(protesedata$aldersgruppe)), ]
protesedata <- protesedata[ , -c(2, 6)]
# protesedata <- protesedata[-which(protesedata$bohf=='Total'), ]
protesedata$bohf[which(protesedata$bohf=='Total')] <- 'Norge'

# Tallkoder for aldersgrupper
konvAldgr <- data.frame(ald_gr_ny=1:4, ald_gr_gml=c('40-61', '62-68', '69-74', '>=75'))
protesedata$aldersgruppe <- konvAldgr$ald_gr_ny[match(protesedata$aldersgruppe, konvAldgr$ald_gr_gml)]

# Kjønn som variabel ermann med tallverdi 0 og 1
protesedata <- gather(protesedata, ermann, antall, 3:4)
protesedata$ermann[protesedata$ermann == 'Kvinne'] <- 0
protesedata$ermann[protesedata$ermann == 'Mann'] <- 1
protesedata$ermann <- as.numeric(protesedata$ermann)

# Legg til variabel for år
protesedata$aar <- 2015

Kne2015 <- protesedata[, c(5,1,3,2,4)]

########### Kneprotese 2016 #####################################

protesedata <- xlsx::read.xlsx('E:/Felles SKDE/Eksterne oppdrag til SKDE/Indikatorprosjekt_2016/2017_Oppdatering/Mottatt data/Leddprotese/kne2016.xlsx',
                               1, startRow=4)

names(protesedata)[c(1,3)] <- c('bohf', 'aldersgruppe')
protesedata$bohf <- as.character(protesedata$bohf)
protesedata$bohf <- iconv(protesedata$bohf, from = 'UTF-8', to = '')
protesedata$aldersgruppe <- as.character(protesedata$aldersgruppe)

# Fyll inn manglende HF
for (i in which(!is.na(protesedata$bohf))) {
  protesedata$bohf[(i+1):(i+4)] <- protesedata$bohf[i]
}

# Fjern total
protesedata <- protesedata[-which(is.na(protesedata$aldersgruppe)), ]
protesedata <- protesedata[ , -c(2, 6)]
# protesedata <- protesedata[-which(protesedata$bohf=='Total'), ]
protesedata$bohf[which(protesedata$bohf=='Total')] <- 'Norge'

# Tallkoder for aldersgrupper
konvAldgr <- data.frame(ald_gr_ny=1:4, ald_gr_gml=c('40-61', '62-68', '69-74', '>=75'))
protesedata$aldersgruppe <- konvAldgr$ald_gr_ny[match(protesedata$aldersgruppe, konvAldgr$ald_gr_gml)]

# Kjønn som variabel ermann med tallverdi 0 og 1
protesedata <- gather(protesedata, ermann, antall, 3:4)
protesedata$ermann[protesedata$ermann == 'Kvinne'] <- 0
protesedata$ermann[protesedata$ermann == 'Mann'] <- 1
protesedata$ermann <- as.numeric(protesedata$ermann)

# Legg til variabel for år
protesedata$aar <- 2016

Kne2016 <- protesedata[, c(5,1,3,2,4)]


########### Kne samlet ######################################

Kneproteser <- rbind(Kne2014, Kne2015, Kne2016)

Kneproteser$bohfnr <- bohfnr$bohfnr[match(Kneproteser$bohf, bohfnr$bohfnavn)]
Kneproteser <- Kneproteser[, c(1,2,6,3,4,5)]

write.csv2(Kneproteser, 'Kneproteser2014-1016.csv', row.names = F)


############### Produser figurer basert på ferdigjusterte rater fra Frank ##############

Kneproteser_just <- read.table('E:/Felles SKDE/Eksterne oppdrag til SKDE/Indikatorprosjekt_2016/2017_Oppdatering/Tilrettelagt_data/Rater_Kneproteser.csv',
                               sep = ',', header=TRUE)

Kneproteser_just <- Kneproteser_just[Kneproteser_just$aar != 'Snitt', ]

names(Kneproteser_just)[2] <- 'bohf'
Kneproteser_just <- Kneproteser_just[, c(1,2,3,5,4)]
Kneproteser_just$andel <- Kneproteser_just$andel*100

outfile <- 'Kneproteser.pdf'
tittel <- 'Rater kneproteser etter boområde'
terskel_bo2 <- 40
decreasing=F
terskel=30
til100 = F
terskel <- terskel_bo2
indikatorFigRaterGrVar2017(Andeler=Kneproteser_just, outfile=outfile, tittel=tittel, width=600, height=600, til100=til100,
                       decreasing=decreasing, terskel=terskel, minstekrav=NA, maal=NA,
                       xtekst ='Antall pr. 100 000 innbyggere')


Hofteproteser_just <- read.table('E:/Felles SKDE/Eksterne oppdrag til SKDE/Indikatorprosjekt_2016/2017_Oppdatering/Tilrettelagt_data/Rater_Hofteproteser.csv',
                               sep = ',', header=TRUE)

Hofteproteser_just <- Hofteproteser_just[Hofteproteser_just$aar != 'Snitt', ]

names(Hofteproteser_just)[2] <- 'bohf'
Hofteproteser_just <- Hofteproteser_just[, c(1,2,3,5,4)]
Hofteproteser_just$andel <- Hofteproteser_just$andel*100

outfile <- 'Hofteproteser.pdf'
tittel <- 'Rater hofteproteser etter boområde'

indikatorFigRaterGrVar2017(Andeler=Hofteproteser_just, outfile=outfile, tittel=tittel, width=600, height=600, til100=til100,
                           decreasing=decreasing, terskel=terskel, minstekrav=NA, maal=NA,
                           xtekst ='Antall pr. 100 000 innbyggere')





