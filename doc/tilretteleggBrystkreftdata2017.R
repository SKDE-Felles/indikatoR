rm(list = ls())
library(indikatoR)
setwd('E:/Felles SKDE/Eksterne oppdrag til SKDE/Indikatorprosjekt_2016/Analyse/Figurer og R-koder brukt i rapporten/R-kode/indikatoR/doc/')

library(xlsx)
library(tidyr)


############ Brystbevarende - bo ##########################################################

brystbevarende_bo <- xlsx::read.xlsx('E:/Felles SKDE/Eksterne oppdrag til SKDE/Indikatorprosjekt_2016/2017_Oppdatering/Mottatt data/Brystkreft/brystkreft_indikator.xlsx',
                               1, startRow=1, encoding = 'UTF-8')

# brystbevarende_bo$bohf <- iconv(brystbevarende_bo$bohf, from = 'UTF-8', to = '')

brystbevarende_bo <- brystbevarende_bo[,-2]
names(brystbevarende_bo)[3:5] <- c('AldKjGr', 'Antall', 'N')
brystbevarende_bo <- brystbevarende_bo[,c(1,2,4,5,3)]
head(brystbevarende_bo)

tittel <- c('Brystbevarende kirurgi ved tumorstørrelse 0-30 mm', 'pr. boområde, justert')
outfile <- 'brystbevarende_bo.pdf'

setwd('E:/Felles SKDE/Eksterne oppdrag til SKDE/Indikatorprosjekt_2016/2017_Oppdatering/')

terskel <- 15
indikatorFigAndelGrVar_aldKjJust2017(Antall=brystbevarende_bo, outfile=outfile, tittel=tittel, width=600, height=600,
                                     decreasing=F, terskel=terskel, minstekrav = 70, minstekravTxt = 'Min',
                                     maal = 80, maalTxt='Mål', til100=FALSE, legPlass='top') #

############ Brystbevarende - beh justert ##########################################################
rm(list = ls())

sykehusnavn <- xlsx::read.xlsx('E:/Felles SKDE/Eksterne oppdrag til SKDE/Indikatorprosjekt_2016/2017_Oppdatering/Mottatt data/Brystkreft/brystkreft_indikator.xlsx',
                   4, startRow=1, encoding = 'UTF-8')

brystbevarende_beh <- xlsx::read.xlsx('E:/Felles SKDE/Eksterne oppdrag til SKDE/Indikatorprosjekt_2016/2017_Oppdatering/Mottatt data/Brystkreft/brystkreft_indikator.xlsx',
                                     2, startRow=1, encoding = 'UTF-8')

brystbevarende_beh <- merge(brystbevarende_beh, sykehusnavn[,c('BehSh_nr', 'BehSh_kort')],
                            by.x = c("BehSh_nr"), by.y = c("BehSh_nr"), all.x = TRUE, all.y = FALSE)
brystbevarende_beh <- brystbevarende_beh[,c(-1, -3)]
brystbevarende_beh <- brystbevarende_beh[,c(1, 5, 3, 4, 2)]
names(brystbevarende_beh)[2:5] <- c('bohf', 'Antall', 'N', 'AldKjGr')
# brystbevarende_beh <- brystbevarende_beh[,-3]
# names(brystbevarende_beh)[2:5] <- c('bohf', 'AldKjGr', 'Antall', 'N')
# brystbevarende_beh <- brystbevarende_beh[,c(1,2,4,5,3)]



tittel=c('Andel med brystbevarende kirurgi for tumorstørrelse 0-30 mm,', 'pr. behandlende sykehus, justert')
outfile <- 'brystbevarende_beh_just.pdf'
decreasing=F
setwd('E:/Felles SKDE/Eksterne oppdrag til SKDE/Indikatorprosjekt_2016/2017_Oppdatering/')

terskel <- 15
indikatorFigAndelGrVar_aldKjJust2017(Antall=brystbevarende_beh, outfile=outfile, tittel=tittel, width=600, height=600,
                                     decreasing=F, terskel=terskel, minstekrav = 70, minstekravTxt = 'Min', sideTxt ='Behandlende sykehus',
                                     maal = 80, maalTxt='Mål', til100=FALSE, legPlass='top') #


############ Brystbevarende - beh ujustert ##########################################################

brystbevarende_beh_ujust <- aggregate(brystbevarende_beh[, c("Antall", "N")], by=list(aar=brystbevarende_beh$aar, bohf=brystbevarende_beh$bohf), sum)


AntTilfeller <- tidyr::spread(brystbevarende_beh_ujust[,1:3], 'aar', 'Antall')
row.names(AntTilfeller) <- AntTilfeller$bohf
AntTilfeller <- AntTilfeller[,-1]

N <- tidyr::spread(brystbevarende_beh_ujust[,c(1,2,4)], 'aar', 'N')
row.names(N) <- N$bohf
N <- N[,-1]
N[is.na(N)] <- 0

outfile <- 'Brystbevarende_sh.pdf'
tittel=c('Andel med brystbevarende kirurgi for tumorstørrelse 0-30 mm,', 'pr. behandlende sykehus')

decreasing=F
terskel=15

setwd('E:/Felles SKDE/Eksterne oppdrag til SKDE/Indikatorprosjekt_2016/2017_Oppdatering/')

indikatorFigAndelGrVar(AntTilfeller=AntTilfeller, N=N, outfile=outfile, tittel=tittel, sideTxt='Behandlende sykehus',
                       skriftStr=1, width=600, height=600, decreasing=decreasing,
                       terskel=terskel, minstekrav = 70, maal = 80, legPlass='top')








