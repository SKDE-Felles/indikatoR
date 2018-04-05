rm(list = ls())
library(indikatoR)
setwd('E:/Felles SKDE/Eksterne oppdrag til SKDE/Indikatorprosjekt_2016/Analyse/Figurer og R-koder brukt i rapporten/R-kode/indikatoR/doc/')

library(xlsx)
library(tidyr)

############# Les og tilrettelegg SPSS-data ###################################
library(foreign)
hjertedata2017 <- read.spss('E:/Felles SKDE/Eksterne oppdrag til SKDE/Indikatorprosjekt_2016/2017_Oppdatering/Mottatt data/Hjerteinfarkt/2017-12-22 Styringsindikatorer Norsk hjerteinfarktregister(3).sav', to.data.frame=TRUE)
hjertedata2017 <- hjertedata2017[hjertedata2017$Innleggelsesaar!=2013, ]

hjertedata2017$BoOmraadeHelseforetak <- trimws(as.character(hjertedata2017$BoOmraadeHelseforetak))
navn_map <- data.frame(gml=sort(unique(hjertedata2017$BoOmraadeHelseforetak)), ny=c('Akershus', 'Helgeland', 'Bergen', 'Finnmark',
                                                                                               'Fonna', 'Førde', 'Møre og Romsdal', 'Nord-Trøndelag',
                                                                                               'Stavanger', 'Nordland', 'Oslo', 'St. Olavs', 'Innlandet',
                                                                                               'Telemark', 'Vestfold', 'Østfold', 'Sørlandet',
                                                                                               'Ukjent', 'UNN', 'Vestre Viken'))
hjertedata2017$BoOmraadeHelseforetak <- navn_map$ny[match(hjertedata2017$BoOmraadeHelseforetak, navn_map$gml)]
hjertedata2017$BoOmraadeHelseforetak <- as.character(hjertedata2017$BoOmraadeHelseforetak)
names(hjertedata2017)[names(hjertedata2017)=='PatientGender'] <- 'ErMann'
hjertedata2017$ErMann <- as.numeric(hjertedata2017$ErMann)
hjertedata2017$ErMann[hjertedata2017$ErMann == 2] <- 0
hjertedata2017$kiCnevner <- NA
hjertedata2017$kiCnevner[!is.na(hjertedata2017$kiCteller)] <- 1
hjertedata2017$kiEnevner <- NA
hjertedata2017$kiEnevner[!is.na(hjertedata2017$kiEteller)] <- 1
hjertedata2017$kiCteller <- as.numeric(hjertedata2017$kiCteller) - 1
hjertedata2017$IndCalderstertiler <- as.numeric(hjertedata2017$IndCalderstertiler)
hjertedata2017$kiEteller <- as.numeric(hjertedata2017$kiEteller) - 1
hjertedata2017$IndEalderstertiler <- as.numeric(hjertedata2017$IndEalderstertiler)
names(hjertedata2017)[names(hjertedata2017)=='Innleggelsesaar'] <- 'aar'

############ Trombolyse Bo ##################################################


Revaskularisering_bo_ny <- hjertedata2017

names(Revaskularisering_bo_ny)[names(Revaskularisering_bo_ny)=='BoOmraadeHelseforetak'] <- 'BoHF_txt'
names(Revaskularisering_bo_ny)[names(Revaskularisering_bo_ny)=='kiCteller'] <- 'Teller'
names(Revaskularisering_bo_ny)[names(Revaskularisering_bo_ny)=='kiCnevner'] <- 'Nevner'
names(Revaskularisering_bo_ny)[names(Revaskularisering_bo_ny)=='IndCalderstertiler'] <- 'ald_gr'

Revaskularisering_bo_2017 <- Revaskularisering_bo_ny[, c('aar', 'BoHF_txt', "ald_gr", "ErMann", "Teller", "Nevner")]
Revaskularisering_bo_2017 <- Revaskularisering_bo_2017[Revaskularisering_bo_2017$aar >= 2015, ]
Revaskularisering_bo_2017 <- Revaskularisering_bo_2017[which(Revaskularisering_bo_2017$Nevner == 1), ]
Revaskularisering_bo_2017$AldKjGr <- Revaskularisering_bo_2017$ald_gr +Revaskularisering_bo_2017$ErMann*max(Revaskularisering_bo_2017$ald_gr)
Revaskularisering_bo_2017 <- Revaskularisering_bo_2017[ , -which(names(Revaskularisering_bo_2017)=="ald_gr")]
Revaskularisering_bo_2017 <- Revaskularisering_bo_2017[ , -which(names(Revaskularisering_bo_2017)=="ErMann")]

Revaskularisering_bo_2017 <- aggregate(Revaskularisering_bo_2017[, c("Teller", "Nevner")], by=list(aar=Revaskularisering_bo_2017$aar, BoHF_txt=Revaskularisering_bo_2017$BoHF_txt,
                                                                                                       AldKjGr=Revaskularisering_bo_2017$AldKjGr), sum)
Landet <- aggregate(Revaskularisering_bo_2017[, c("Teller", "Nevner")], by=list(aar=Revaskularisering_bo_2017$aar,
                                                                                    AldKjGr=Revaskularisering_bo_2017$AldKjGr), sum)
Landet$BoHF_txt <- 'Norge'

Revaskularisering_bo_2017 <- rbind(Revaskularisering_bo_2017, Landet[, c(1,5,2:4)])
names(Revaskularisering_bo_2017)[names(Revaskularisering_bo_2017)=='Teller'] <- 'Antall'
names(Revaskularisering_bo_2017)[names(Revaskularisering_bo_2017)=='Nevner'] <- 'N'
names(Revaskularisering_bo_2017)[names(Revaskularisering_bo_2017)=='BoHF_txt'] <- 'bohf'

Antall <- Revaskularisering_bo_2017

outfile <- 'revask_bo_just.pdf'
tittel <- 'Trombolyse/angiografi/PCI innen 30 min., pr. boområde, justert'
terskel <- 30

indikatorFigAndelGrVar_aldKjJust2017(Antall=Antall, outfile=outfile, tittel=tittel, width=600, height=600,
                                     decreasing=F, terskel=terskel, minstekrav = 50, minstekravTxt = 'God', maalTxt = 'Meget god',
                                     maal = 80, til100=F, legPlass='top')

################ Tromsbolyse Behandler ########################################################

Revaskularisering_sh_ny <- hjertedata2017

names(Revaskularisering_sh_ny)[names(Revaskularisering_sh_ny)=='FørsteSykehus'] <- 'BoHF_txt'
Revaskularisering_sh_ny$BoHF_txt <- trimws(as.character(Revaskularisering_sh_ny$BoHF_txt))
names(Revaskularisering_sh_ny)[names(Revaskularisering_sh_ny)=='kiCteller'] <- 'Teller'
names(Revaskularisering_sh_ny)[names(Revaskularisering_sh_ny)=='kiCnevner'] <- 'Nevner'
names(Revaskularisering_sh_ny)[names(Revaskularisering_sh_ny)=='IndCalderstertiler'] <- 'ald_gr'

Revaskularisering_bo_2017 <- Revaskularisering_sh_ny[, c('aar', 'BoHF_txt', "ald_gr", "ErMann", "Teller", "Nevner")]
Revaskularisering_bo_2017 <- Revaskularisering_bo_2017[Revaskularisering_bo_2017$aar >= 2015, ]
Revaskularisering_bo_2017 <- Revaskularisering_bo_2017[which(Revaskularisering_bo_2017$Nevner == 1), ]
Revaskularisering_bo_2017$AldKjGr <- Revaskularisering_bo_2017$ald_gr +Revaskularisering_bo_2017$ErMann*max(Revaskularisering_bo_2017$ald_gr)
Revaskularisering_bo_2017 <- Revaskularisering_bo_2017[ , -which(names(Revaskularisering_bo_2017)=="ald_gr")]
Revaskularisering_bo_2017 <- Revaskularisering_bo_2017[ , -which(names(Revaskularisering_bo_2017)=="ErMann")]

Revaskularisering_bo_2017 <- aggregate(Revaskularisering_bo_2017[, c("Teller", "Nevner")], by=list(aar=Revaskularisering_bo_2017$aar, BoHF_txt=Revaskularisering_bo_2017$BoHF_txt,
                                                                                                       AldKjGr=Revaskularisering_bo_2017$AldKjGr), sum)
Landet <- aggregate(Revaskularisering_bo_2017[, c("Teller", "Nevner")], by=list(aar=Revaskularisering_bo_2017$aar,
                                                                                    AldKjGr=Revaskularisering_bo_2017$AldKjGr), sum)
Landet$BoHF_txt <- 'Norge'

Revaskularisering_bo_2017 <- rbind(Revaskularisering_bo_2017, Landet[, c(1,5,2:4)])
names(Revaskularisering_bo_2017)[names(Revaskularisering_bo_2017)=='Teller'] <- 'Antall'
names(Revaskularisering_bo_2017)[names(Revaskularisering_bo_2017)=='Nevner'] <- 'N'
names(Revaskularisering_bo_2017)[names(Revaskularisering_bo_2017)=='BoHF_txt'] <- 'bohf'

Antall <- Revaskularisering_bo_2017

outfile <- 'revask_sh_just.pdf'
tittel <- 'Trombolyse/angiografi/PCI innen 30 min., pr. sykehus, justert'
terskel <- 30

indikatorFigAndelGrVar_aldKjJust2017(Antall=Antall, outfile=outfile, tittel=tittel, width=600, height=600,
                                     decreasing=F, terskel=terskel, minstekrav = 50, minstekravTxt = 'God', maalTxt = 'Meget god',
                                     maal = 80, til100=F, legPlass='top')

tmp <- aggregate(Antall$Antall, by=list(aar=Antall$aar, sh=Antall$bohf), sum)
AntTilfeller <- tidyr::spread(tmp, 'aar', 'x')
rownames(AntTilfeller) <- AntTilfeller$sh
AntTilfeller <- AntTilfeller[, -1]

tmp <- aggregate(Antall$N, by=list(aar=Antall$aar, sh=Antall$bohf), sum)
N <- tidyr::spread(tmp, 'aar', 'x')
rownames(N) <- N$sh
N <- N[, -1]
N[is.na(N)] <- 0

outfile <- 'revask_sh.pdf'
tittel <- 'Trombolyse/angiografi/PCI innen 30 min., pr. sykehus'
terskel <- 10

indikatorFigAndelGrVar2017(AntTilfeller=AntTilfeller, N=N, outfile=outfile, tittel=tittel, sideTxt='Behandlende sykehus',
                           width=600, height=800, decreasing=F, terskel=terskel,
                           skriftStr=1, pktStr=1, minstekrav = 50, minstekravTxt = 'God',
                           maal = 80, maalTxt='Meget god', legPlass='top')



########## Angio - BO ##########################################

Revaskularisering_bo_ny <- hjertedata2017

names(Revaskularisering_bo_ny)[names(Revaskularisering_bo_ny)=='BoOmraadeHelseforetak'] <- 'BoHF_txt'
names(Revaskularisering_bo_ny)[names(Revaskularisering_bo_ny)=='kiEteller'] <- 'Teller'
names(Revaskularisering_bo_ny)[names(Revaskularisering_bo_ny)=='kiEnevner'] <- 'Nevner'
names(Revaskularisering_bo_ny)[names(Revaskularisering_bo_ny)=='IndEalderstertiler'] <- 'ald_gr'

Revaskularisering_bo_2017 <- Revaskularisering_bo_ny[, c('aar', 'BoHF_txt', "ald_gr", "ErMann", "Teller", "Nevner")]
Revaskularisering_bo_2017 <- Revaskularisering_bo_2017[Revaskularisering_bo_2017$aar >= 2014, ]
Revaskularisering_bo_2017 <- Revaskularisering_bo_2017[which(Revaskularisering_bo_2017$Nevner == 1), ]
Revaskularisering_bo_2017$AldKjGr <- Revaskularisering_bo_2017$ald_gr +Revaskularisering_bo_2017$ErMann*max(Revaskularisering_bo_2017$ald_gr)
Revaskularisering_bo_2017 <- Revaskularisering_bo_2017[ , -which(names(Revaskularisering_bo_2017)=="ald_gr")]
Revaskularisering_bo_2017 <- Revaskularisering_bo_2017[ , -which(names(Revaskularisering_bo_2017)=="ErMann")]

Revaskularisering_bo_2017 <- aggregate(Revaskularisering_bo_2017[, c("Teller", "Nevner")], by=list(aar=Revaskularisering_bo_2017$aar, BoHF_txt=Revaskularisering_bo_2017$BoHF_txt,
                                                                                                   AldKjGr=Revaskularisering_bo_2017$AldKjGr), sum)
Landet <- aggregate(Revaskularisering_bo_2017[, c("Teller", "Nevner")], by=list(aar=Revaskularisering_bo_2017$aar,
                                                                                AldKjGr=Revaskularisering_bo_2017$AldKjGr), sum)
Landet$BoHF_txt <- 'Norge'

Revaskularisering_bo_2017 <- rbind(Revaskularisering_bo_2017, Landet[, c(1,5,2:4)])
names(Revaskularisering_bo_2017)[names(Revaskularisering_bo_2017)=='Teller'] <- 'Antall'
names(Revaskularisering_bo_2017)[names(Revaskularisering_bo_2017)=='Nevner'] <- 'N'
names(Revaskularisering_bo_2017)[names(Revaskularisering_bo_2017)=='BoHF_txt'] <- 'bohf'

Antall <- Revaskularisering_bo_2017

outfile <- 'angio_bo_just.pdf'
tittel <- 'Andel med angio innen 72 timer, pr. boområde, justert'
terskel <- 30

indikatorFigAndelGrVar_aldKjJust2017(Antall=Antall, outfile=outfile, tittel=tittel, width=600, height=600,
                                     decreasing=F, terskel=terskel, minstekrav = 50, minstekravTxt = 'God', maalTxt = 'Meget god',
                                     maal = 80, til100=F, legPlass='top')


############# Angio - behandler #####################################

Revaskularisering_sh_ny <- hjertedata2017

names(Revaskularisering_sh_ny)[names(Revaskularisering_sh_ny)=='FørsteSykehus'] <- 'BoHF_txt'
Revaskularisering_sh_ny$BoHF_txt <- trimws(as.character(Revaskularisering_sh_ny$BoHF_txt))
names(Revaskularisering_sh_ny)[names(Revaskularisering_sh_ny)=='kiEteller'] <- 'Teller'
names(Revaskularisering_sh_ny)[names(Revaskularisering_sh_ny)=='kiEnevner'] <- 'Nevner'
names(Revaskularisering_sh_ny)[names(Revaskularisering_sh_ny)=='IndEalderstertiler'] <- 'ald_gr'

Revaskularisering_bo_2017 <- Revaskularisering_sh_ny[, c('aar', 'BoHF_txt', "ald_gr", "ErMann", "Teller", "Nevner")]
Revaskularisering_bo_2017 <- Revaskularisering_bo_2017[Revaskularisering_bo_2017$aar >= 2014, ]
Revaskularisering_bo_2017 <- Revaskularisering_bo_2017[which(Revaskularisering_bo_2017$Nevner == 1), ]
Revaskularisering_bo_2017$AldKjGr <- Revaskularisering_bo_2017$ald_gr +Revaskularisering_bo_2017$ErMann*max(Revaskularisering_bo_2017$ald_gr)
Revaskularisering_bo_2017 <- Revaskularisering_bo_2017[ , -which(names(Revaskularisering_bo_2017)=="ald_gr")]
Revaskularisering_bo_2017 <- Revaskularisering_bo_2017[ , -which(names(Revaskularisering_bo_2017)=="ErMann")]

Revaskularisering_bo_2017 <- aggregate(Revaskularisering_bo_2017[, c("Teller", "Nevner")], by=list(aar=Revaskularisering_bo_2017$aar, BoHF_txt=Revaskularisering_bo_2017$BoHF_txt,
                                                                                                   AldKjGr=Revaskularisering_bo_2017$AldKjGr), sum)
Landet <- aggregate(Revaskularisering_bo_2017[, c("Teller", "Nevner")], by=list(aar=Revaskularisering_bo_2017$aar,
                                                                                AldKjGr=Revaskularisering_bo_2017$AldKjGr), sum)
Landet$BoHF_txt <- 'Norge'

Revaskularisering_bo_2017 <- rbind(Revaskularisering_bo_2017, Landet[, c(1,5,2:4)])
names(Revaskularisering_bo_2017)[names(Revaskularisering_bo_2017)=='Teller'] <- 'Antall'
names(Revaskularisering_bo_2017)[names(Revaskularisering_bo_2017)=='Nevner'] <- 'N'
names(Revaskularisering_bo_2017)[names(Revaskularisering_bo_2017)=='BoHF_txt'] <- 'bohf'

Antall <- Revaskularisering_bo_2017

outfile <- 'angio_sh_just.pdf'
tittel <- 'Andel med angio innen 72 timer, pr. sykehus, justert'
terskel <- 30

indikatorFigAndelGrVar_aldKjJust2017(Antall=Antall, outfile=outfile, tittel=tittel, width=600, height=600,
                                     decreasing=F, terskel=terskel, minstekrav = 50, minstekravTxt = 'God', maalTxt = 'Meget god',
                                     maal = 80, til100=F, legPlass='top')

tmp <- aggregate(Antall$Antall, by=list(aar=Antall$aar, sh=Antall$bohf), sum)
AntTilfeller <- tidyr::spread(tmp, 'aar', 'x')
rownames(AntTilfeller) <- AntTilfeller$sh
AntTilfeller <- AntTilfeller[, -1]

tmp <- aggregate(Antall$N, by=list(aar=Antall$aar, sh=Antall$bohf), sum)
N <- tidyr::spread(tmp, 'aar', 'x')
rownames(N) <- N$sh
N <- N[, -1]
N[is.na(N)] <- 0

outfile <- 'angio_sh.pdf'
tittel <- 'Andel med angio innen 72 timer, pr. sykehus'
terskel <- 10

indikatorFigAndelGrVar2017(AntTilfeller=AntTilfeller, N=N, outfile=outfile, tittel=tittel, sideTxt='Behandlende sykehus',
                           width=600, height=800, decreasing=F, terskel=terskel,
                           skriftStr=1, pktStr=1, minstekrav = 50, minstekravTxt = 'God',
                           maal = 80, maalTxt='Meget god', legPlass='top')













################ Trombolyse BO - Gammelt datasett #############################
#
# Revaskularisering_bo_2017 <- read.table('E:/Felles SKDE/Eksterne oppdrag til SKDE/Indikatorprosjekt_2016/2017_Oppdatering/Mottatt data/Hjerteinfarkt/Hjerteinfarkt_bo_2017.csv',
#                          sep = ';', stringsAsFactors = F, header = T)
#
# Revaskularisering_bo_2017 <- Revaskularisering_bo_2017[,-1]
# names(Revaskularisering_bo_2017)[names(Revaskularisering_bo_2017)=='PatientGender'] <- 'ErMann'
# Revaskularisering_bo_2017$ErMann[Revaskularisering_bo_2017$ErMann == 2] <- 0
#
# navn_map <- data.frame(gml=sort(unique(Revaskularisering_bo_2017$BoOmraadeHelseforetak)), ny=c('Akershus', 'Helgeland', 'Bergen', 'Finnmark',
#                                                                                                'Fonna', 'Førde', 'Møre og Romsdal', 'Nord-Trøndelag',
#                                                                                                'Stavanger', 'Nordland', 'Oslo', 'St. Olavs', 'Innlandet',
#                                                                                                'Telemark', 'Vestfold', 'Østfold', 'Sørlandet',
#                                                                                              'Ukjent', 'UNN', 'Vestre Viken'))
# Revaskularisering_bo_2017$BoOmraadeHelseforetak <- navn_map$ny[match(Revaskularisering_bo_2017$BoOmraadeHelseforetak, navn_map$gml)]
# names(Revaskularisering_bo_2017)[names(Revaskularisering_bo_2017)=='BoOmraadeHelseforetak'] <- 'BoHF_txt'
# names(Revaskularisering_bo_2017)[names(Revaskularisering_bo_2017)=='Innleggelsesaar'] <- 'aar'
# names(Revaskularisering_bo_2017)[names(Revaskularisering_bo_2017)=='kiCteller'] <- 'under30min'
# names(Revaskularisering_bo_2017)[names(Revaskularisering_bo_2017)=='kiCnevner'] <- 'Nevner'
# names(Revaskularisering_bo_2017)[names(Revaskularisering_bo_2017)=='Alder3gr_kiC'] <- 'ald_gr'
#
# Revaskularisering_bo_2017 <- Revaskularisering_bo_2017[, c('aar', 'BoHF_txt', "ald_gr", "ErMann", "under30min", "Nevner")]
# Revaskularisering_bo_2017 <- Revaskularisering_bo_2017[Revaskularisering_bo_2017$aar >= 2015, ]
# Revaskularisering_bo_2017 <- Revaskularisering_bo_2017[Revaskularisering_bo_2017$Nevner == 1, ]
# Revaskularisering_bo_2017$AldKjGr <- Revaskularisering_bo_2017$ald_gr +Revaskularisering_bo_2017$ErMann*max(Revaskularisering_bo_2017$ald_gr)
# Revaskularisering_bo_2017 <- Revaskularisering_bo_2017[ , -which(names(Revaskularisering_bo_2017)=="ald_gr")]
# Revaskularisering_bo_2017 <- Revaskularisering_bo_2017[ , -which(names(Revaskularisering_bo_2017)=="ErMann")]
#
# Revaskularisering_bo_2017 <- aggregate(Revaskularisering_bo_2017[, c("under30min", "Nevner")], by=list(aar=Revaskularisering_bo_2017$aar, BoHF_txt=Revaskularisering_bo_2017$BoHF_txt,
#                                                                AldKjGr=Revaskularisering_bo_2017$AldKjGr), sum)
# Landet <- aggregate(Revaskularisering_bo_2017[, c("under30min", "Nevner")], by=list(aar=Revaskularisering_bo_2017$aar,
#                                                                            AldKjGr=Revaskularisering_bo_2017$AldKjGr), sum)
# Landet$BoHF_txt <- 'Norge'
#
# Revaskularisering_bo_2017 <- rbind(Revaskularisering_bo_2017, Landet[, c(1,5,2:4)])
# names(Revaskularisering_bo_2017)[names(Revaskularisering_bo_2017)=='under30min'] <- 'Antall'
# names(Revaskularisering_bo_2017)[names(Revaskularisering_bo_2017)=='Nevner'] <- 'N'
# names(Revaskularisering_bo_2017)[names(Revaskularisering_bo_2017)=='BoHF_txt'] <- 'bohf'
#
# Antall <- Revaskularisering_bo_2017
#
# outfile <- ''
# tittel <- 'Trombolyse/angiografi/PCI innen 30 min., pr. boområde, justert'
# terskel <- 30
#
# indikatorFigAndelGrVar_aldKjJust2017(Antall=Antall, outfile=outfile, tittel=tittel, width=600, height=600,
#                                  decreasing=F, terskel=terskel, minstekrav = 50, minstekravTxt = 'God', maalTxt = 'Meget god',
#                                  maal = 80, til100=F, legPlass='top')
#
#





