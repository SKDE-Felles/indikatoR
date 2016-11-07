
##### gammel Sh ######

raatall <- read.table('E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Brystkreft/Resultater/Tilpassede_datasett_KT/Brystbevarende_sh.csv', header=TRUE, sep=";", encoding = 'UFT-8')

AntTilfeller <- raatall[, 2:4]
AntTotalt <- raatall[, 5:7]
colnames(AntTilfeller) <- c('2013','2014','2015')
rownames(AntTilfeller) <- as.character(raatall$Behandlende.sykehus)
colnames(AntTotalt) <- c('2013','2014','2015')
rownames(AntTotalt) <- as.character(raatall$Behandlende.sykehus)
Brystbevarende_sh <- list(AntTilfeller=AntTilfeller, AntTotalt=AntTotalt)
save(Brystbevarende_sh, file = "Brystbevarende_sh.RData")


raatall <- read.table('E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Brystkreft/Resultater/Tilpassede_datasett_KT/Ki67_sh.csv', header=TRUE, sep=";", encoding = 'UFT-8')
BrystKi67sh2015 <- raatall[ , c(1, 4,7,10,13)]
names(BrystKi67sh2015) <- c('Behandlende sykehus', 'Lav <15%', 'Intermediær 15-30%', 'Høy >30%', 'Ukjent')
save(BrystKi67sh2015, file = "BrystKi672015_sh.RData")

raatall <- read.table('E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Brystkreft/Resultater/Tilpassede_datasett_KT/BrystkreftHistologi_sh.csv', header=TRUE, sep=";", encoding = 'UFT-8')
BrystkreftHistologi_sh <- raatall[ , c(1, 4,7,10,13)]
names(BrystkreftHistologi_sh) <- c('Behandlende sykehus', 'Grad 1', 'Grad 2', 'Grad 3', 'Ukjent')
save(BrystkreftHistologi_sh, file = "BrystkreftHistologi_sh.RData")

Hoftebrudd_Preoperativ_liggetid_sh <- read.table('E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Hoftebrudd/Resultater/Preoperativ_liggetid_behandler.csv', sep = ';', header = T, encoding = 'native')
save(Hoftebrudd_Preoperativ_liggetid_sh, file = "Hoftebrudd_Preoperativ_liggetid_sh.RData")

Hoftebrudd_Produksjon_sh <- read.table('E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Hoftebrudd/Resultater/Hoftebruddteknikk_behsh.csv',
                                       sep = ';', header = T, encoding = 'native', strip.white=TRUE)
save(Hoftebrudd_Produksjon_sh, file = "Hoftebrudd_Produksjon_sh.RData")

Hoftebrudd_Produksjon_hf <- read.table('E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Hoftebrudd/Resultater/Hoftebruddteknikk_behHF_v2.csv',
                                       sep = ';', header = T, encoding = 'native', strip.white=TRUE)
Hoftebrudd_Produksjon_hf <- Hoftebrudd_Produksjon_hf[, -1]
names(Hoftebrudd_Produksjon_hf)[names(Hoftebrudd_Produksjon_hf)=='BehHF_txt'] <- 'Behandlende.sykehus'
save(Hoftebrudd_Produksjon_hf, file = "Hoftebrudd_Produksjon_hf.RData")


## produksjon

Hofteprotese_Produksjon_sh <- read.table('E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Proteser/Resultater/Gammelt/Hofte_Proteser_behandler.csv',
                                         sep = ';', header = T, encoding = 'native', strip.white=TRUE)
save(Hofteprotese_Produksjon_sh, file = "Hofteprotese_Produksjon_sh.RData")

Kneprotese_Produksjon_sh <- read.table('E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Proteser/Resultater/Gammelt/Kne_Proteser_behandler.csv',
                                       sep = ';', header = T, encoding = 'native', strip.white=TRUE)
save(Kneprotese_Produksjon_sh, file = "Kneprotese_Produksjon_sh.RData")

Angio_sh <- read.table('E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Hjerteinfarkt/Resultater/Angio_BEH.csv',
                       sep = ';', header = T, encoding = 'native', strip.white=TRUE)
save(Angio_sh, file = "Angio_sh.RData")

Revaskularisering_sh <- read.table('E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Hjerteinfarkt/Resultater/Revaskularisering_BEH.csv',
                                   sep = ';', header = T, encoding = 'native', strip.white=TRUE)
save(Revaskularisering_sh, file = "Revaskularisering_sh.RData")

Hjerneslag_behandlet_slagenhet_sh <- read.table('E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Hjerneslag/Resultater/Beh_slagenhet_BEH.csv', sep = ';', header = T, encoding = 'native')
save(Hjerneslag_behandlet_slagenhet_sh, file = "Hjerneslag_behandlet_slagenhet_sh.RData")

Hjerneslag_tromsbolyse_sh <- read.table('E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Hjerneslag/Resultater/Trombolyse_BEH.csv', sep = ';', header = T, encoding = 'native')
save(Hjerneslag_tromsbolyse_sh, file = "Hjerneslag_tromsbolyse_sh.RData")


############# BEHF ###################
Hoftebrudd_Preoperativ_liggetid_hf <- read.table('E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Hoftebrudd/Resultater/Preoperativ_liggetid_BehHF_justert.csv', sep = ';', header = T, encoding = 'native')
Hoftebrudd_Preoperativ_liggetid_hf$behhf <- Hoftebrudd_Preoperativ_liggetid_hf$behhf_txt
Hoftebrudd_Preoperativ_liggetid_hf <- Hoftebrudd_Preoperativ_liggetid_hf[ , -which(names(Hoftebrudd_Preoperativ_liggetid_hf)=="behhf_txt")]
save(Hoftebrudd_Preoperativ_liggetid_hf, file = "Hoftebrudd_Preoperativ_liggetid_hf.RData")

Angio_hf <- read.table('E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Hjerteinfarkt/Resultater/Angio_BEHHF_v2.csv',
                       sep = ';', header = T, encoding = 'native', strip.white=TRUE)
Angio_hf <- Angio_hf[, -2]
save(Angio_hf, file = "Angio_hf.RData")

Hjerneslag_behandlet_slagenhet_hf <- read.table('E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Hjerneslag/Resultater/Beh_slagenhet_BEH_V2_HF.csv', sep = ';', header = T, encoding = 'native')
Hjerneslag_behandlet_slagenhet_hf <- Hjerneslag_behandlet_slagenhet_hf[, -2]
save(Hjerneslag_behandlet_slagenhet_hf, file = "Hjerneslag_behandlet_slagenhet_hf.RData")

Revaskularisering_hf <- read.table('E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Hjerteinfarkt/Resultater/Revaskularisering_BEHHF_v2.csv',
                                   sep = ';', header = T, encoding = 'native', strip.white=TRUE)
Revaskularisering_hf <- Revaskularisering_hf[ , -2]
save(Revaskularisering_hf, file = "Revaskularisering_hf.RData")




Hjerneslag_tromsbolyse_hf <- read.table('E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Hjerneslag/Resultater/Trombolyse_BEH_v2_HF.csv', sep = ';', header = T, encoding = 'native')
Hjerneslag_tromsbolyse_hf <-Hjerneslag_tromsbolyse_hf[, -2]
save(Hjerneslag_tromsbolyse_hf, file = "Hjerneslag_tromsbolyse_hf.RData")



#
# Hoftebrudd_Preoperativ_liggetid_ujust <- read.table('C:/GIT/indikatoR/doc/Preoperativ_liggetid_grunnlagsdata.csv', sep = ';',
#                                                     header = T, encoding = 'native')
#
# Hoftebrudd_Preoperativ_liggetid_ujust$ErMann <- as.character(Hoftebrudd_Preoperativ_liggetid_ujust$ErMann)
# Hoftebrudd_Preoperativ_liggetid_ujust$ErMann[Hoftebrudd_Preoperativ_liggetid_ujust$ErMann=='Kvinner'] <- 0
# Hoftebrudd_Preoperativ_liggetid_ujust$ErMann[Hoftebrudd_Preoperativ_liggetid_ujust$ErMann=='Menn'] <- 1
# Hoftebrudd_Preoperativ_liggetid_ujust$ErMann <- as.numeric(Hoftebrudd_Preoperativ_liggetid_ujust$ErMann)
# Hoftebrudd_Preoperativ_liggetid_ujust$aldersgr <- Hoftebrudd_Preoperativ_liggetid_ujust$alder_ny
# levels(Hoftebrudd_Preoperativ_liggetid_ujust$aldersgr) <- 1:4
# Hoftebrudd_Preoperativ_liggetid_ujust$aldersgr <- as.numeric(Hoftebrudd_Preoperativ_liggetid_ujust$aldersgr)
# Hoftebrudd_Preoperativ_liggetid_ujust$AldKjGr <- Hoftebrudd_Preoperativ_liggetid_ujust$aldersgr +
#   Hoftebrudd_Preoperativ_liggetid_ujust$ErMann*max(Hoftebrudd_Preoperativ_liggetid_ujust$aldersgr)
# Hoftebrudd_Preoperativ_liggetid_ujust <- Hoftebrudd_Preoperativ_liggetid_ujust[, c('aar', 'bohf', 'Antall24', 'Antall48', 'Antall', 'AldKjGr')]
# Hoftebrudd_Preoperativ_liggetid_ujust$bohf <- as.character(Hoftebrudd_Preoperativ_liggetid_ujust$bohf)
#
# aux <- aggregate(Hoftebrudd_Preoperativ_liggetid_ujust[, c('Antall24', 'Antall48', 'Antall')], by = list(aar=Hoftebrudd_Preoperativ_liggetid_ujust$aar,
#                                                                                                          AldKjGr=Hoftebrudd_Preoperativ_liggetid_ujust$AldKjGr), sum)
# aux$bohf <- 'Norge'
# aux <- aux[, c(1,6,3,4,5,2)]
#
# Hoftebrudd_Preoperativ_liggetid_bo_ujust <- rbind(Hoftebrudd_Preoperativ_liggetid_ujust, aux)
#
# save(Hoftebrudd_Preoperativ_liggetid_bo_ujust, file = "Hoftebrudd_Preoperativ_liggetid_bo_ujustert.RData")







#   raatall2 <- read.table('C:/GIT/indikatoR/doc/BrystEkspresjon.csv', header=TRUE, sep=";", encoding = 'UFT-8')
#   BrystEkspresjon_PrAar <- raatall2
#   colnames(BrystEkspresjon_PrAar) <- c('Behandlende.sykehus', '2013grad1', '2014grad1', '2015grad1',
#                                        '2013grad2', '2014grad2', '2015grad2', '2013grad3', '2014grad3',
#                                        '2015grad3', '2013Ukjent', '2014Ukjent', '2015Ukjent')
#
#   BrystEkspresjon <- cbind(rowSums(raatall2[2:4], na.rm = T), rowSums(raatall2[5:7], na.rm = T),
#                            rowSums(raatall2[8:10], na.rm = T), rowSums(raatall2[11:13], na.rm = T))
#   rownames(BrystEkspresjon) <- as.character(raatall2$Behandlende.sykehus)
#   colnames(BrystEkspresjon) <- c('Grad1', 'Grad2', 'Grad3', 'Ukjent')


# save(BrystEkspresjon, BrystEkspresjon_PrAar, file = "BrystEkspresjon.RData")

## Brystkreft - Ki 67  Versjon 1 med rater  ###############################
#
#   raatall3 <- read.table('C:/GIT/indikatoR/doc/ki67_bo.csv', header=TRUE, sep=";", encoding = 'UFT-8')
#
#   gruppe1 <- raatall3[raatall3$Ekspresjon == 'Lav <15%', c("BoHF", "Innbyggere", "rateSnitt")]
#   gruppe2 <- raatall3[raatall3$Ekspresjon == 'Intermediær 15-30%', c("BoHF", "rateSnitt")]
#   gruppe3 <- raatall3[raatall3$Ekspresjon == 'Høy >30%', c("BoHF", "rateSnitt")]
#   gruppe4 <- raatall3[raatall3$Ekspresjon == 'Ukjent', c("BoHF", "rateSnitt")]
#   BrystKi67 <- merge(gruppe1, gruppe2, by.x = 'BoHF', by.y = 'BoHF', suffixes = c('', '_intermediaer'))
#   BrystKi67 <- merge(BrystKi67, gruppe3, by.x = 'BoHF', by.y = 'BoHF', suffixes = c('', '_hoey'))
#   BrystKi67 <- merge(BrystKi67, gruppe4, by.x = 'BoHF', by.y = 'BoHF', suffixes = c('', '_ukjent'))
#   names(BrystKi67)[names(BrystKi67) == 'rateSnitt'] <- 'rateSnitt_lav'
#
#   BrystKi67$BoHF <- as.character(BrystKi67$BoHF)
#   BrystKi67 <- rbind(BrystKi67, c('Landet', sum(BrystKi67$Innbyggere), raatall3$Norge[c(1,22,43,64)]))
#   BrystKi67[,-1] <- apply(BrystKi67[,-1], 2, as.numeric)
#   save(BrystKi67, BrystKi67, file = "BrystKi67.RData")



#############################################################################################
## Brystkreft - Aksille andel ujustert ######################################################
#
# raatall <- read.table('C:/GIT/indikatoR/doc/BrystkreftAksille_sh.csv', header=TRUE, sep=";", encoding = 'UFT-8')
#
# AntTilfeller <- raatall[, 2:4]
# AntTotalt <- raatall[, 5:7]
# colnames(AntTilfeller) <- c('2013','2014','2015')
# rownames(AntTilfeller) <- as.character(raatall$Behandlende.sykehus)
# colnames(AntTotalt) <- c('2013','2014','2015')
# rownames(AntTotalt) <- as.character(raatall$Behandlende.sykehus)
#
# BrystAksille_sh <- list(AntTilfeller=AntTilfeller, AntTotalt=AntTotalt)
# save(BrystAksille_sh, file = "BrystAksille_sh.RData")
#
# raatall <- read.table('C:/GIT/indikatoR/doc/BrystkreftAksille_Bo.csv', header=TRUE, sep=";", encoding = 'UFT-8')
# AntTilfeller <- raatall[, 2:4]
# AntTotalt <- raatall[, 5:7]
# colnames(AntTilfeller) <- c('2013','2014','2015')
# rownames(AntTilfeller) <- as.character(raatall$'Boområde')
# colnames(AntTotalt) <- c('2013','2014','2015')
# rownames(AntTotalt) <- as.character(raatall$'Boområde')
#
# BrystAksille_bo <- list(AntTilfeller=AntTilfeller, AntTotalt=AntTotalt)
# save(BrystAksille_bo, file = "BrystAksille_bo.RData")
#
# raatall <- read.table('C:/GIT/indikatoR/doc/BrystkreftAksille_Bo_aldersgr.csv', header=TRUE, sep=";", encoding = 'UFT-8')
# AntTilfeller <- raatall[, 2:10]
# AntTotalt <- raatall[, 11:19]
# colnames(AntTilfeller) <- c('2013gr1','2014gr1','2015gr1', '2013gr2','2014gr2','2015gr2', '2013gr3','2014gr3','2015gr3')
# rownames(AntTilfeller) <- as.character(raatall$'Boområde')
# colnames(AntTotalt) <- c('2013gr1','2014gr1','2015gr1', '2013gr2','2014gr2','2015gr2', '2013gr3','2014gr3','2015gr3')
# rownames(AntTotalt) <- as.character(raatall$'Boområde')
#
# BrystAksille_bo_aldersgr <- list(AntTilfeller=AntTilfeller, AntTotalt=AntTotalt)
# save(BrystAksille_bo_aldersgr, file = "BrystAksille_bo_aldersgr.RData")
#



