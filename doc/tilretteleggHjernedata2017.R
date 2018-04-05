rm(list = ls())
library(indikatoR)
setwd('E:/Felles SKDE/Eksterne oppdrag til SKDE/Indikatorprosjekt_2016/Analyse/Figurer og R-koder brukt i rapporten/R-kode/indikatoR/doc/')

library(xlsx)
library(tidyr)


hjernedata <- read.table('E:/Felles SKDE/Eksterne oppdrag til SKDE/Indikatorprosjekt_2016/2017_Oppdatering/Mottatt data/Hjerneslag/Utlevert_fil_2016.csv',
                         sep = ';', stringsAsFactors = F, header = T, fileEncoding = 'UTF-8-BOM')

hjernedata$Year <- 2016

# write.csv2(hjernedata, 'E:/Felles SKDE/Eksterne oppdrag til SKDE/Indikatorprosjekt_2016/2017_Oppdatering/Mottatt data/Hjerneslag/Utlevert_fil_med_aar_2016.csv',
#            row.names = F)


################ BO - Trombolyse ############################################

rm(list = ls())
Hjerneslag_tromsbolyse_bo_gml <- read.table('E:/Felles SKDE/Eksterne oppdrag til SKDE/Indikatorprosjekt_2016/Analyse/Figurer og R-koder brukt i rapporten/R-kode/indikatoR/doc/csv/Trombolyse_BO.csv',
                                        sep = ';', header = T, encoding = 'native', stringsAsFactors = F)
Hjerneslag_tromsbolyse_bo_2016 <- read.table('E:/Felles SKDE/Eksterne oppdrag til SKDE/Indikatorprosjekt_2016/2017_Oppdatering/Tilrettelagt_data/Trombolyse_BO_v3.csv',
                                             sep = ';', header = T, encoding = 'native', stringsAsFactors = F)
Hjerneslag_tromsbolyse_bo_2016$BoHF_txt[Hjerneslag_tromsbolyse_bo_2016$BoHF_txt=='OUS'] <- 'Oslo'


Antall <- rbind.data.frame(Hjerneslag_tromsbolyse_bo_gml, Hjerneslag_tromsbolyse_bo_2016)

Antall <- Antall[, -2]
Antall$AldKjGr <- Antall$ald_gr +
  Antall$ErMann*max(Antall$ald_gr)
Antall <- Antall[, -c(3,4)]
names(Antall)[c(2,4)] <- c('bohf', 'N')

# Antall <- Hjerneslag_tromsbolyse_bo
Antall$TellerE[is.na(Antall$TellerE)] <- 0

names(Antall)[3] <- 'Antall'


tittel <- 'Andel med trombolyse innen 40 min., pr. boområde, justert'
outfile <- 'Trombolyse_bo_justert_2017.pdf'

setwd('E:/Felles SKDE/Eksterne oppdrag til SKDE/Indikatorprosjekt_2016/2017_Oppdatering/')

terskel <- 30
indikatorFigAndelGrVar_aldKjJust2017(Antall=Antall, outfile=outfile, tittel=tittel, width=600, height=600,
                                 decreasing=F, terskel=terskel, minstekrav = 30, minstekravTxt = 'Moderat',
                                 maal = 50, maalTxt='Høy', til100=FALSE, legPlass='top') #

################   Bo - Behandlet i slagenhet #################################
rm(list = ls())
bohf_navn <- read.table('E:/Felles SKDE/Eksterne oppdrag til SKDE/Indikatorprosjekt_2016/Analyse/Figurer og R-koder brukt i rapporten/R-kode/indikatoR/doc/bohf_navn.csv', header=TRUE, sep=";", encoding = 'UFT-8')
names(bohf_navn) <- c("bohf_nr","bohf_gammel", "bohf_navn")

Hjerneslag_behandlet_slagenhet_bo_gml <- read.table('E:/Felles SKDE/Eksterne oppdrag til SKDE/Indikatorprosjekt_2016/Analyse/Figurer og R-koder brukt i rapporten/R-kode/indikatoR/doc/csv/Beh_slagenhet_BO.csv',
                                                sep = ';', header = T, encoding = 'native', stringsAsFactors = F)
Hjerneslag_behandlet_slagenhet_bo_2016 <- read.table('E:/Felles SKDE/Eksterne oppdrag til SKDE/Indikatorprosjekt_2016/2017_Oppdatering/Tilrettelagt_data/Beh_slagenhet_BO.csv',
                                             sep = ';', header = T, encoding = 'native', stringsAsFactors = F)
Hjerneslag_behandlet_slagenhet_bo_2016$BoHF_txt[Hjerneslag_behandlet_slagenhet_bo_2016$BoHF_txt=='OUS'] <- 'Oslo'

Hjerneslag_behandlet_slagenhet_bo_gml <- rbind.data.frame(Hjerneslag_behandlet_slagenhet_bo_gml, Hjerneslag_behandlet_slagenhet_bo_2016)


Hjerneslag_behandlet_slagenhet_bo_gml <- merge(Hjerneslag_behandlet_slagenhet_bo_gml, bohf_navn[,c('bohf_nr', 'bohf_gammel')],
                                           by.x = c("BoHF"), by.y = c("bohf_nr"), all.x = TRUE, all.y = FALSE)
Hjerneslag_behandlet_slagenhet_bo_gml$BoHF_txt <- Hjerneslag_behandlet_slagenhet_bo_gml$bohf_gammel
Hjerneslag_behandlet_slagenhet_bo_gml <- Hjerneslag_behandlet_slagenhet_bo_gml[ , -which(names(Hjerneslag_behandlet_slagenhet_bo_gml)=="bohf_gammel")]
Hjerneslag_behandlet_slagenhet_bo_gml <- Hjerneslag_behandlet_slagenhet_bo_gml[ , -which(names(Hjerneslag_behandlet_slagenhet_bo_gml)=="BoHF")]
Hjerneslag_behandlet_slagenhet_bo_gml$AldKjGr <- Hjerneslag_behandlet_slagenhet_bo_gml$ald_gr +
  Hjerneslag_behandlet_slagenhet_bo_gml$ErMann*max(Hjerneslag_behandlet_slagenhet_bo_gml$ald_gr)
Hjerneslag_behandlet_slagenhet_bo_gml <- Hjerneslag_behandlet_slagenhet_bo_gml[, -c(3,4)]
names(Hjerneslag_behandlet_slagenhet_bo_gml)[c(2,4)] <- c('bohf', 'N')

Antall <- Hjerneslag_behandlet_slagenhet_bo_gml

names(Antall)[3] <- 'Antall'
tittel <- 'Andel behandlet i slagenhet, pr. boområde, justert'
outfile <- 'BehSlagenhet_bo_justert.pdf'
terskel <- 30

setwd('E:/Felles SKDE/Eksterne oppdrag til SKDE/Indikatorprosjekt_2016/2017_Oppdatering/')

indikatorFigAndelGrVar_aldKjJust2017(Antall=Antall, outfile=outfile, tittel=tittel, width=600, height=600,
                                 decreasing=F, terskel=terskel, minstekrav = 80, minstekravTxt = 'Moderat',
                                 maal = 90, maalTxt='Høy', til100=FALSE)




############## BEH - Trombolyse ##############################################
rm(list = ls())
sykehusnavn <- read.table('E:/Felles SKDE/Eksterne oppdrag til SKDE/Indikatorprosjekt_2016/Analyse/Figurer og R-koder brukt i rapporten/R-kode/indikatoR/doc/sykehusnavn.csv', header=TRUE, sep=";", encoding = 'UFT-8')
names(sykehusnavn) <- c("BehSh_nr","BehSh_lang", "BehSh_kort")

Hjerneslag_tromsbolyse_sh_v2 <- read.table('E:/Felles SKDE/Eksterne oppdrag til SKDE/Indikatorprosjekt_2016/Analyse/Figurer og R-koder brukt i rapporten/R-kode/indikatoR/doc/csv/Trombolyse_BEH_v2.csv', sep = ';', header = T, encoding = 'native')
Hjerneslag_tromsbolyse_sh_v2 <- merge(Hjerneslag_tromsbolyse_sh_v2, sykehusnavn[,c('BehSh_nr', 'BehSh_kort')],
                                      by.x = c("BehSh"), by.y = c("BehSh_nr"), all.x = TRUE, all.y = FALSE)
Hjerneslag_tromsbolyse_sh_v2$Helseenhet <- Hjerneslag_tromsbolyse_sh_v2$BehSh_kort
Hjerneslag_tromsbolyse_sh_v2 <- Hjerneslag_tromsbolyse_sh_v2[ , -which(names(Hjerneslag_tromsbolyse_sh_v2)=="BehSh_kort")]
Hjerneslag_tromsbolyse_sh_v2 <- Hjerneslag_tromsbolyse_sh_v2[, -which(names(Hjerneslag_tromsbolyse_sh_v2)=="BehSh")]

Hjerneslag_tromsbolyse_sh_2016 <- read.table('E:/Felles SKDE/Eksterne oppdrag til SKDE/Indikatorprosjekt_2016/2017_Oppdatering/Tilrettelagt_data/Trombolyse_BEH_v3.csv',
                                             sep = ';', header = T, encoding = 'native', stringsAsFactors = F)


Hjerneslag_tromsbolyse_sh_2016 <- merge(Hjerneslag_tromsbolyse_sh_2016, sykehusnavn[,c('BehSh_nr', 'BehSh_kort')],
                                      by.x = c("BehSh"), by.y = c("BehSh_nr"), all.x = TRUE, all.y = FALSE)
Hjerneslag_tromsbolyse_sh_2016$Helseenhet <- Hjerneslag_tromsbolyse_sh_2016$BehSh_kort
Hjerneslag_tromsbolyse_sh_2016 <- Hjerneslag_tromsbolyse_sh_2016[ , -which(names(Hjerneslag_tromsbolyse_sh_2016)=="BehSh_kort")]
Hjerneslag_tromsbolyse_sh_2016 <- Hjerneslag_tromsbolyse_sh_2016[, -which(names(Hjerneslag_tromsbolyse_sh_2016)=="BehSh")]


Hjerneslag_tromsbolyse_sh_v2 <- rbind(Hjerneslag_tromsbolyse_sh_v2, Hjerneslag_tromsbolyse_sh_2016)

AntTilfeller <- tidyr::spread(Hjerneslag_tromsbolyse_sh_v2[,1:3], 'Aar', 'TellerE')
rownames(AntTilfeller) <- AntTilfeller$Helseenhet
AntTilfeller <- AntTilfeller[, -1]
N <- tidyr::spread(Hjerneslag_tromsbolyse_sh_v2[,c(1,2,4)], 'Aar', 'NevnerE')
rownames(N) <- N$Helseenhet
N <- N[, -1]
N[is.na(N)] <- 0

outfile <- 'Trombolyse_sh_2017.pdf'
tittel <- 'Andel med trombolyse innen 40 min., pr. sykehus'
terskel <- 5

setwd('E:/Felles SKDE/Eksterne oppdrag til SKDE/Indikatorprosjekt_2016/2017_Oppdatering/')

indikatorFigAndelGrVar2017(AntTilfeller=AntTilfeller, N=N, outfile=outfile, tittel=tittel, sideTxt='Behandlende sykehus',
                       width=600, height=800, decreasing=F, terskel=terskel,
                       skriftStr=1, pktStr=1, minstekrav = 30, minstekravTxt = 'Moderat',
                       maal = 50, maalTxt='Høy', legPlass='top')



############## BEH - Behandlet slagenhet ##############################################
rm(list = ls())
sykehusnavn <- read.table('E:/Felles SKDE/Eksterne oppdrag til SKDE/Indikatorprosjekt_2016/Analyse/Figurer og R-koder brukt i rapporten/R-kode/indikatoR/doc/sykehusnavn.csv', header=TRUE, sep=";", encoding = 'UFT-8')
names(sykehusnavn) <- c("BehSh_nr","BehSh_lang", "BehSh_kort")

Hjerneslag_behandlet_slagenhet_sh_gml <- read.table('E:/Felles SKDE/Eksterne oppdrag til SKDE/Indikatorprosjekt_2016/Analyse/Figurer og R-koder brukt i rapporten/R-kode/indikatoR/doc/csv/Beh_slagenhet_BEH_V2.csv', sep = ';', header = T, encoding = 'native')
Hjerneslag_behandlet_slagenhet_sh_2016 <- read.table('E:/Felles SKDE/Eksterne oppdrag til SKDE/Indikatorprosjekt_2016/2017_Oppdatering/Tilrettelagt_data/Beh_slagenhet_BEH_V2.csv',
                                             sep = ';', header = T, encoding = 'native', stringsAsFactors = F)
Hjerneslag_behandlet_slagenhet_sh_gml <- rbind(Hjerneslag_behandlet_slagenhet_sh_gml, Hjerneslag_behandlet_slagenhet_sh_2016)

Hjerneslag_behandlet_slagenhet_sh_gml <- merge(Hjerneslag_behandlet_slagenhet_sh_gml, sykehusnavn[,c('BehSh_nr', 'BehSh_kort')],
                                               by.x = c("BehSh"), by.y = c("BehSh_nr"), all.x = TRUE, all.y = FALSE)
Hjerneslag_behandlet_slagenhet_sh_gml$Helseenhet <- Hjerneslag_behandlet_slagenhet_sh_gml$BehSh_kort
Hjerneslag_behandlet_slagenhet_sh_gml <- Hjerneslag_behandlet_slagenhet_sh_gml[ , -which(names(Hjerneslag_behandlet_slagenhet_sh_gml)=="BehSh_kort")]
Hjerneslag_behandlet_slagenhet_sh_gml <- Hjerneslag_behandlet_slagenhet_sh_gml[, -which(names(Hjerneslag_behandlet_slagenhet_sh_gml)=="BehSh")]

AntTilfeller <- tidyr::spread(Hjerneslag_behandlet_slagenhet_sh_gml[,1:3], 'Aar', 'TellerC')
rownames(AntTilfeller) <- AntTilfeller$Helseenhet
AntTilfeller <- AntTilfeller[, -1]
N <- tidyr::spread(Hjerneslag_behandlet_slagenhet_sh_gml[,c(1,2,4)], 'Aar', 'NevnerC')
rownames(N) <- N$Helseenhet
N <- N[, -1]
N[is.na(N)] <- 0
tittel <- 'Andel behandlet i slagenhet, pr. sykehus'
figurnavn <- 'BehSlagenhet_sh'
outfile <- 'BehSlagenhet_sh.pdf'
terskel <- 5

setwd('E:/Felles SKDE/Eksterne oppdrag til SKDE/Indikatorprosjekt_2016/2017_Oppdatering/')

indikatorFigAndelGrVar2017(AntTilfeller=AntTilfeller, N=N, outfile=outfile, tittel=tittel, sideTxt='Behandlende sykehus',
                       minstekrav = 80, maal = 90, skriftStr=1, pktStr=1,
                       width=600, height=800, decreasing=F, terskel=terskel,
                       minstekravTxt = 'Moderat', maalTxt='Høy')


