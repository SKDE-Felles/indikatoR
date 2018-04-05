rm(list = ls())
library(indikatoR)
setwd('E:/Felles SKDE/Eksterne oppdrag til SKDE/Indikatorprosjekt_2016/Analyse/Figurer og R-koder brukt i rapporten/R-kode/indikatoR/doc/')

library(xlsx)
library(tidyr)



################ BO - ferdigjustert  ############################
rm(list = ls())

tmp <- read.table('E:/Felles SKDE/Eksterne oppdrag til SKDE/Indikatorprosjekt_2016/2017_Oppdatering/Tilrettelagt_data/Hoftebrudd_preoperativ_liggetid_Bo_2014_2016.csv',
                  sep = ',', header=TRUE)
decreasing=F
Andeler <- tmp[, -c(3,5,7,8)]
names(Andeler)[names(Andeler)=='andel24'] <- 'andel'
names(Andeler)[names(Andeler)=='bohf_txt'] <- 'bohf'
outfile <- 'HofteOp24_bo.pdf'
tittel <- c('Andel hoftebrudd operert innen 24 timer (alders- og kjønnsjustert),', 'pr. boområde/opptaksområde')

terskel=40
indikatorFigAndelGrVar_preberegnet2017(Andeler=Andeler, outfile=outfile, tittel=tittel, width=600, height=600,
                                   til100=F, decreasing=decreasing, terskel=terskel, minstekrav=NA, maal=NA)


Andeler <- tmp[, -c(3,4,7,8)]
names(Andeler)[names(Andeler)=='andel48'] <- 'andel'
names(Andeler)[names(Andeler)=='bohf_txt'] <- 'bohf'
outfile <- 'HofteOp48_bo.pdf'
tittel <- c('Andel hoftebrudd operert innen 48 timer (alders- og kjønnsjustert),', 'pr. boområde/opptaksområde')

indikatorFigAndelGrVar_preberegnet2017(Andeler=Andeler, outfile=outfile, tittel=tittel, width=600, height=600,
                                   decreasing=decreasing, terskel=terskel, minstekrav=NA, maal=NA)



########## Sykehus #########################################
rm(list = ls())
sykehusnavn <- read.table('E:/Felles SKDE/Eksterne oppdrag til SKDE/Indikatorprosjekt_2016/Analyse/Figurer og R-koder brukt i rapporten/R-kode/indikatoR/doc/sykehusnavn.csv',
                          header=TRUE, sep=";", encoding = 'UFT-8', stringsAsFactors = F, strip.white=TRUE)
sykehusnavn_ny <- read.table('E:/Felles SKDE/Eksterne oppdrag til SKDE/Indikatorprosjekt_2016/Analyse/Figurer og R-koder brukt i rapporten/R-kode/indikatoR/doc/sykehusnavn_2017.csv',
                             header=TRUE, sep=";", encoding = 'UFT-8', stringsAsFactors = F, strip.white=TRUE)
sykehusnavn <- merge(sykehusnavn_ny, sykehusnavn[, c("BehSh", "BehSh_kort")], by.x = c("BehSh"), by.y = c("BehSh"), all.x = TRUE, all.y = FALSE)
sykehusnavn$BehSh_kort[is.na(sykehusnavn$BehSh_kort)] <- sykehusnavn$BehSh_txt[is.na(sykehusnavn$BehSh_kort)]

names(sykehusnavn) <- c("BehSh_nr","BehSh_lang", "BehSh_kort")


# tmp <- read.table('E:/Felles SKDE/Eksterne oppdrag til SKDE/Indikatorprosjekt_2016/2017_Oppdatering/Tilrettelagt_data/Hoftebrudd_preoperativ_liggetid_Beh_2014_2016.csv',
#                   sep = ',', header=TRUE, strip.white=TRUE)
tmp <- read.table('E:/Felles SKDE/Eksterne oppdrag til SKDE/Indikatorprosjekt_2016/2017_Oppdatering/Tilrettelagt_data/Hoftebrudd_preoperativ_liggetid_BehHF_2014_2016.csv',
                  sep = ',', header=TRUE, strip.white=TRUE)

# tmp <- merge(tmp, sykehusnavn[,c('BehSh_nr', 'BehSh_kort')],
#       by.x = c("behsh"), by.y = c("BehSh_nr"), all.x = TRUE, all.y = FALSE)


# tmp <- tmp[, c(9,3:8)]
names(tmp)[1]<- 'behsh'
tmp <- tmp[, -3]
# tmp$behsh[is.na(tmp$behsh)] <- 'Norge'
# head(Hoftebrudd_Preoperativ_liggetid_sh_v2)
# head(tmp)

Andeler <- tmp[ , -c(3,4,7)]
Andeler <- Andeler[, c(1,2,4,3)]
names(Andeler)[names(Andeler)=='andel24'] <- 'andel'
names(Andeler)[names(Andeler)=='Antall'] <- 'antall'
names(Andeler)[names(Andeler)=='behsh'] <- 'bohf'
outfile <- 'HofteOp24_beh_hf.pdf'
tittel <- c('Andel operert innen 24 timer,', 'pr. behandlende HF')
terskel <- 10

setwd('E:/Felles SKDE/Eksterne oppdrag til SKDE/Indikatorprosjekt_2016/2017_Oppdatering/')

indikatorFigAndelGrVar_preberegnet2017(Andeler=Andeler, outfile=outfile, tittel=tittel, width=600, height=800,
                                   decreasing=F, terskel=terskel, minstekrav=NA, maal=NA,
                                   skriftStr=1, pktStr=1.0, sideTxt='Behandlende HF')


Andeler <- tmp[ , -c(3,4,6)]
Andeler <- Andeler[, c(1,2,4,3)]
names(Andeler)[names(Andeler)=='andel48'] <- 'andel'
names(Andeler)[names(Andeler)=='Antall'] <- 'antall'
names(Andeler)[names(Andeler)=='behsh'] <- 'bohf'
outfile <- 'HofteOp48_beh_hf.pdf'
tittel <- c('Andel operert innen 48 timer,', 'pr. behandlende HF')
terskel <- 10

setwd('E:/Felles SKDE/Eksterne oppdrag til SKDE/Indikatorprosjekt_2016/2017_Oppdatering/')

indikatorFigAndelGrVar_preberegnet2017(Andeler=Andeler, outfile=outfile, tittel=tittel, width=600, height=800,
                                       decreasing=F, terskel=terskel, minstekrav=NA, maal=NA,
                                       skriftStr=1, pktStr=1.0, sideTxt='Behandlende HF')





