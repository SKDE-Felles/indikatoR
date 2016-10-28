#' Les og preprosesser data for fremstilling i figur
#'
#' Denne funksjonenen laster dataen fra forskjellige kilder og
#' omformer den til formen forventet av funksjonene i IndikatoR
#'
#' @return En rekke datasett organisert som forventet av funksjonene i IndikatoR
#' @export
#'
indikatorLastdataOgPreprosesser <- function()
{
  currentDir <- getwd()
  setwd('C:/GIT/indikatoR/data/')

  #############################################################################################
  ## Brystkreft - brystbevarende  #####################################################

  raatall <- read.table('E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Brystkreft/Resultater/Tilpassede_datasett_KT/Brystbevarende_sh.csv', header=TRUE, sep=";", encoding = 'UFT-8')

  AntTilfeller <- raatall[, 2:4]
  AntTotalt <- raatall[, 5:7]
  colnames(AntTilfeller) <- c('2013','2014','2015')
  rownames(AntTilfeller) <- as.character(raatall$Behandlende.sykehus)
  colnames(AntTotalt) <- c('2013','2014','2015')
  rownames(AntTotalt) <- as.character(raatall$Behandlende.sykehus)

  Brystbevarende_sh <- list(AntTilfeller=AntTilfeller, AntTotalt=AntTotalt)
  save(Brystbevarende_sh, file = "Brystbevarende_sh.RData")


  raatall <- read.table('E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Brystkreft/Resultater/Tilpassede_datasett_KT/Brystbevarende_bo.csv', header=TRUE, sep=";", encoding = 'UFT-8')

  AntTilfeller <- raatall[, 2:4]
  AntTotalt <- raatall[, 5:7]
  colnames(AntTilfeller) <- c('2013','2014','2015')
  rownames(AntTilfeller) <- as.character(raatall$'Boområde')
  colnames(AntTotalt) <- c('2013','2014','2015')
  rownames(AntTotalt) <- as.character(raatall$'Boområde')

  Brystbevarende_bo <- list(AntTilfeller=AntTilfeller, AntTotalt=AntTotalt)
  save(Brystbevarende_bo, file = "Brystbevarende_bo.RData")

  raatall <- read.table('E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Brystkreft/Resultater/Tilpassede_datasett_KT/Brystbevarende_bo_aldersgr.csv', header=TRUE, sep=";", encoding = 'UFT-8')
  AntTilfeller <- raatall[, 2:10]
  AntTotalt <- raatall[, 11:19]
  colnames(AntTilfeller) <- c('2013gr1','2014gr1','2015gr1', '2013gr2','2014gr2','2015gr2', '2013gr3','2014gr3','2015gr3')
  rownames(AntTilfeller) <- as.character(raatall$'Boområde')
  colnames(AntTotalt) <- c('2013gr1','2014gr1','2015gr1', '2013gr2','2014gr2','2015gr2', '2013gr3','2014gr3','2015gr3')
  rownames(AntTotalt) <- as.character(raatall$'Boområde')

  Brystbevarende_bo_aldersgr <- list(AntTilfeller=AntTilfeller, AntTotalt=AntTotalt)
  save(Brystbevarende_bo_aldersgr, file = "Brystbevarende_bo_aldersgr.RData")


  #############################################################################################
  ## Brystkreft - Ki 67 andeler i stabel ujustert #############################################

  raatall <- read.table('E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Brystkreft/Resultater/Tilpassede_datasett_KT/Ki67_sh.csv', header=TRUE, sep=";", encoding = 'UFT-8')
  BrystKi67sh2015 <- raatall[ , c(1, 4,7,10,13)]
  names(BrystKi67sh2015) <- c('Behandlende sykehus', 'Lav <15%', 'Intermediær 15-30%', 'Høy >30%', 'Ukjent')
  save(BrystKi67sh2015, file = "BrystKi672015_sh.RData")

  raatall <- read.table('E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Brystkreft/Resultater/Tilpassede_datasett_KT/ki67_bo.csv', header=TRUE, sep=";", encoding = 'UFT-8')
  BrystKi67Bo2015 <- raatall[ , c(1, 4,7,10,13)]
  names(BrystKi67Bo2015) <- c('Boområde', 'Lav <15%', 'Intermediær 15-30%', 'Høy >30%', 'Ukjent')
  save(BrystKi67Bo2015, file = "BrystKi672015_bo.RData")

  #### Med aldersgrupper

  raatall <- read.table('E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Brystkreft/Resultater/Tilpassede_datasett_KT/ki67_bo_aldersgr.csv', header=TRUE, sep=";", encoding = 'UFT-8')
  BrystKi67Bo2015_aldersgr <- raatall[ , seq(1,dim(raatall)[2],by=3)]
  names(BrystKi67Bo2015_aldersgr) <- c('Boområde', 'Lav <15% aldgr 1', 'Lav <15% aldgr 2', 'Lav <15% aldgr 3',
                                       'Intermediær 15-30% aldgr 1', 'Intermediær 15-30% aldgr 2', 'Intermediær 15-30% aldgr 3',
                                       'Høy >30% aldgr 1', 'Høy >30% aldgr 2', 'Høy >30% aldgr 3',
                                       'Ukjent aldgr 1', 'Ukjent aldgr 2', 'Ukjent aldgr 3')
  save(BrystKi67Bo2015_aldersgr, file = "BrystKi67Bo2015_aldersgr.RData")



  #############################################################################################
  ## Brystkreft - Histologi andeler i stabel  #################################################
  raatall <- read.table('E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Brystkreft/Resultater/Tilpassede_datasett_KT/BrystkreftHistologi_sh.csv', header=TRUE, sep=";", encoding = 'UFT-8')
  BrystkreftHistologi_sh <- raatall[ , c(1, 4,7,10,13)]
  names(BrystkreftHistologi_sh) <- c('Behandlende sykehus', 'Grad 1', 'Grad 2', 'Grad 3', 'Ukjent')
  save(BrystkreftHistologi_sh, file = "BrystkreftHistologi_sh.RData")

  raatall <- read.table('E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Brystkreft/Resultater/Tilpassede_datasett_KT/BrystkreftHistologi_bo.csv', header=TRUE, sep=";", encoding = 'UFT-8')
  BrystkreftHistologi_bo <- raatall[ , c(1, 4,7,10,13)]
  names(BrystkreftHistologi_bo) <- c('Behandlende sykehus', 'Grad 1', 'Grad 2', 'Grad 3', 'Ukjent')
  save(BrystkreftHistologi_bo, file = "BrystkreftHistologi_bo.RData")

  raatall <- read.table('E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Brystkreft/Resultater/Tilpassede_datasett_KT/BrystkreftHistologi_bo_aldersgr.csv', header=TRUE, sep=";", encoding = 'UFT-8')
  BrystkreftHistologi_bo2015_aldersgr <- raatall[ , seq(1,dim(raatall)[2],by=3)]
  names(BrystkreftHistologi_bo2015_aldersgr) <- c('Boområde', 'Grad 1 aldgr 1', 'Grad 1 aldgr 2', 'Grad 1 aldgr 3',
                                       'Grad 2 aldgr 1', 'Grad 2 aldgr 2', 'Grad 2 aldgr 3',
                                       'Grad 3 aldgr 1', 'Grad 3 aldgr 2', 'Grad 3 aldgr 3',
                                       'Ukjent aldgr 1', 'Ukjent aldgr 2', 'Ukjent aldgr 3')
  save(BrystkreftHistologi_bo2015_aldersgr, file = "BrystkreftHistologi_bo2015_aldersgr.RData")

  ##############################################################################################
  ################  Alder og kjønnsdata #########################################################

  Innb2015aldkj <- read.table('C:/GIT/indikatoR/doc/Innbyggere2015aldkj.csv', sep = ';', header = T, encoding = 'UTF-8')
  save(Innb2015aldkj, file = "Innb2015aldkj.RData")


  ##############################################################################################
  ################  Hoftebrudd #########################################################

  ### Ferdig justerte tall ######################################

  Hoftebrudd_Preoperativ_liggetid_bo_justert <- read.table('E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Hoftebrudd/Resultater/Preoperativ_liggetid.csv', sep = ';', header = T, encoding = 'native')
  save(Hoftebrudd_Preoperativ_liggetid_bo_justert, file = "Hoftebrudd_Preoperativ_liggetid_bo_justert.RData")

  Hoftebrudd_Preoperativ_liggetid_sh <- read.table('E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Hoftebrudd/Resultater/Preoperativ_liggetid_behandler.csv', sep = ';', header = T, encoding = 'native')
  save(Hoftebrudd_Preoperativ_liggetid_sh, file = "Hoftebrudd_Preoperativ_liggetid_sh.RData")


  Hoftebrudd_Produksjon_sh <- read.table('E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Hoftebrudd/Resultater/Hoftebruddteknikk_behsh.csv',
                                         sep = ';', header = T, encoding = 'native', strip.white=TRUE)
  save(Hoftebrudd_Produksjon_sh, file = "Hoftebrudd_Produksjon_sh.RData")


  Hoftebrudd_Produksjon_bo <- read.table('E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Hoftebrudd/Resultater/Hoftebruddteknikk.csv',
                                         sep = ';', header = T, encoding = 'native', strip.white=TRUE)
  save(Hoftebrudd_Produksjon_bo, file = "Hoftebrudd_Produksjon_bo.RData")

  ##########      UNDER ARBEID      #####################################################################
  ##########      UNDER ARBEID      #####################################################################
  ##########      UNDER ARBEID      #####################################################################
  ##########      UNDER ARBEID      #####################################################################

  Hoftebrudd_Produksjon_bo_aldkjgr <- read.table('E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Hoftebrudd/Resultater/Hoftebrudd_BO_teknikk_raadata_alt.csv',
                                                 sep = ';', header = T, encoding = 'native', strip.white=TRUE)
  Hoftebrudd_Produksjon_bo_aldkjgr <- Hoftebrudd_Produksjon_bo_aldkjgr[,-1]
  Hoftebrudd_Produksjon_bo_aldkjgr$Bo <- as.character(Hoftebrudd_Produksjon_bo_aldkjgr$Bo)
  Hoftebrudd_Produksjon_bo_aldkjgr$Bo[Hoftebrudd_Produksjon_bo_aldkjgr$Bo=='Landet'] <- 'Norge'
  Hoftebrudd_Produksjon_bo_aldkjgr$AldKjGr <- Hoftebrudd_Produksjon_bo_aldkjgr$alder +
    Hoftebrudd_Produksjon_bo_aldkjgr$ermann*max(Hoftebrudd_Produksjon_bo_aldkjgr$alder)
  Hoftebrudd_Produksjon_bo_aldkjgr <- Hoftebrudd_Produksjon_bo_aldkjgr[,-(3:4)]
  Hoftebrudd_Produksjon_bo_aldkjgr2015 <- Hoftebrudd_Produksjon_bo_aldkjgr[Hoftebrudd_Produksjon_bo_aldkjgr$aar==2015, ]
  Hoftebrudd_Produksjon_bo_aldkjgr2015 <- Hoftebrudd_Produksjon_bo_aldkjgr2015[, -2]
  Hoftebrudd_Produksjon_bo_aldkjgr2015 <- tidyr::spread(Hoftebrudd_Produksjon_bo_aldkjgr2015, 'teknikk', 'Antall')

  tmp <- Hoftebrudd_Produksjon_bo_aldkjgr2015
  tmp[is.na(tmp)] <- 0

  vekt <- tmp[tmp$Bo=='Norge', 6]/sum(tmp[tmp$Bo=='Norge', 6])
  vektFrame <- data.frame('AldKjGr'=sort(unique(tmp$AldKjGr)), 'vekt'=vekt)

  tmp[, 3:5] <- tmp[, 3:5]/tmp$Totalt
  tmp[which(is.nan(tmp[, 3])),3:5] <- 0
  tmp <- merge(tmp, vektFrame, by='AldKjGr', all.x=T)
  tmp[, c(3:5)] <- tmp[, c(3:5)] * tmp$vekt
  Hoftebrudd_andeler_2015_bo_justert <- aggregate(tmp[, c(3:6)], by=list(bohf=tmp$Bo), sum)
  Hoftebrudd_andeler_2015_bo_justert[,2:4] <- Hoftebrudd_andeler_2015_bo_justert[,2:4]/rowSums(Hoftebrudd_andeler_2015_bo_justert[,2:4])
  rownames(Hoftebrudd_andeler_2015_bo_justert) <- Hoftebrudd_andeler_2015_bo_justert$bohf
  Hoftebrudd_andeler_2015_bo_justert <- Hoftebrudd_andeler_2015_bo_justert[, -1]
  names(Hoftebrudd_andeler_2015_bo_justert)[names(Hoftebrudd_andeler_2015_bo_justert)=='Totalt'] <- 'N'
  names(Hoftebrudd_andeler_2015_bo_justert)[names(Hoftebrudd_andeler_2015_bo_justert)=='Pinning'] <- 'To skruer eller pinner'
  Hoftebrudd_andeler_2015_bo_justert <- Hoftebrudd_andeler_2015_bo_justert[, c(2,1,3,4)]

  ## Kan ta med ekstra kategori "ingen" for tilfellet at aldersgruppe ikke har noen av metodene (f.eks. Finnmark og Førde)
#   tmp$Ingen <- 0
#   tmp$Ingen[tmp$Totalt==0] <- 1
#   tmp <- merge(tmp, vektFrame, by='AldKjGr', all.x=T)
#   tmp[, c(3:5,7)] <- tmp[, c(3:5,7)] * tmp$vekt
#   andeler <- aggregate(tmp[, c(3:5,7)], by=list(bohf=tmp$Bo), sum)
#   andeler$sum <- rowSums(andeler[,-1])

  save(Hoftebrudd_andeler_2015_bo_justert, file = "Hoftebrudd_andeler_2015_bo_justert.RData")

  ##########      UNDER ARBEID      #####################################################################
  ##########      UNDER ARBEID      #####################################################################
  ##########      UNDER ARBEID      #####################################################################
  ##########      UNDER ARBEID      #####################################################################

  Hoftebrudd_rater <- read.table('E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Hoftebrudd/Resultater/Hoftebrudd_justerterater_totalt_bohf_fo.csv',
             sep = ';', header = T, encoding = 'native', strip.white=TRUE)

  Hoftebrudd_rater <- Hoftebrudd_rater[, -1]
  Hoftebrudd_rater <- Hoftebrudd_rater[, c(2,1,3,4,5)]
  names(Hoftebrudd_rater) <- c('aar', 'bohf', 'andel', 'antall', 'N')
  save(Hoftebrudd_rater, file = "Hoftebrudd_rater.RData")
  ########## Proteser#####################################################################

  Hofteproteser_rater <- read.table('E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Proteser/Resultater/Rater_hofteproteser.csv',
                                         sep = ';', header = T, encoding = 'native', strip.white=TRUE)

  Hofteproteser_rater <- Hofteproteser_rater[, -2]
  Hofteproteser_rater <- Hofteproteser_rater[, c(1:3,5,4)]
  names(Hofteproteser_rater) <- c('aar', 'bohf', 'andel', 'antall', 'N')
  save(Hofteproteser_rater, file = "Hofteproteser_rater.RData")

  Kneproteser_rater <- read.table('E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Proteser/Resultater/Rater_kneproteser.csv',
                                    sep = ';', header = T, encoding = 'native', strip.white=TRUE)

  Kneproteser_rater <- Kneproteser_rater[, -2]
  Kneproteser_rater <- Kneproteser_rater[, c(1:3,5,4)]
  names(Kneproteser_rater) <- c('aar', 'bohf', 'andel', 'antall', 'N')
  save(Kneproteser_rater, file = "Kneproteser_rater.RData")

  ## produksjon

  Hofteprotese_Produksjon_sh <- read.table('E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Proteser/Resultater/Hofte_Proteser_behandler.csv',
                                         sep = ';', header = T, encoding = 'native', strip.white=TRUE)
  save(Hofteprotese_Produksjon_sh, file = "Hofteprotese_Produksjon_sh.RData")

  Kneprotese_Produksjon_sh <- read.table('E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Proteser/Resultater/Kne_Proteser_behandler.csv',
                                           sep = ';', header = T, encoding = 'native', strip.white=TRUE)
  save(Kneprotese_Produksjon_sh, file = "Kneprotese_Produksjon_sh.RData")

  ############ Hjerteinfarkt ############################################################
  ###################################################################################


  Angio_sh <- read.table('E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Hjerteinfarkt/Resultater/Angio_BEH.csv',
             sep = ';', header = T, encoding = 'native', strip.white=TRUE)
  save(Angio_sh, file = "Angio_sh.RData")

  Angio_bo <- read.table('E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Hjerteinfarkt/Resultater/Angio_BO.csv',
             sep = ';', header = T, encoding = 'native', strip.white=TRUE)
  Angio_bo <- Angio_bo[, -c(2,8)]
  Angio_bo$AldKjGr <- Angio_bo$ald_gr +Angio_bo$ErMann*max(Angio_bo$ald_gr)
  Angio_bo <- Angio_bo[, -c(3,4)]
  names(Angio_bo)[c(2,5)] <- c('bohf', 'N')
  save(Angio_bo, file = "Angio_bo.RData")

  Revaskularisering_sh <- read.table('E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Hjerteinfarkt/Resultater/Revaskularisering_BEH.csv',
             sep = ';', header = T, encoding = 'native', strip.white=TRUE)
  save(Revaskularisering_sh, file = "Revaskularisering_sh.RData")

  Revaskularisering_bo <- read.table('E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Hjerteinfarkt/Resultater/Revaskularisering_BO.csv',
             sep = ';', header = T, encoding = 'native', strip.white=TRUE)
  Revaskularisering_bo <- Revaskularisering_bo[, -c(2)]
  Revaskularisering_bo$AldKjGr <- Revaskularisering_bo$ald_gr +Revaskularisering_bo$ErMann*max(Revaskularisering_bo$ald_gr)
  Revaskularisering_bo <- Revaskularisering_bo[, -c(3,4)]
  names(Revaskularisering_bo)[c(2,5)] <- c('bohf', 'N')
  save(Revaskularisering_bo, file = "Revaskularisering_bo.RData")

  ############ Hjerneslag ############################################################
  ###################################################################################


  Hjerneslag_behandlet_slagenhet_bo <- read.table('E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Hjerneslag/Resultater/Beh_slagenhet_BO.csv',
                                                  sep = ';', header = T, encoding = 'native')

  Hjerneslag_behandlet_slagenhet_bo <- Hjerneslag_behandlet_slagenhet_bo[, -2]
  Hjerneslag_behandlet_slagenhet_bo$AldKjGr <- Hjerneslag_behandlet_slagenhet_bo$ald_gr +
    Hjerneslag_behandlet_slagenhet_bo$ErMann*max(Hjerneslag_behandlet_slagenhet_bo$ald_gr)
  Hjerneslag_behandlet_slagenhet_bo <- Hjerneslag_behandlet_slagenhet_bo[, -c(3,4)]
  names(Hjerneslag_behandlet_slagenhet_bo)[c(2,4)] <- c('bohf', 'N')
  save(Hjerneslag_behandlet_slagenhet_bo, file = "Hjerneslag_behandlet_slagenhet_bo.RData")

  Hjerneslag_behandlet_slagenhet_sh <- read.table('E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Hjerneslag/Resultater/Beh_slagenhet_BEH.csv', sep = ';', header = T, encoding = 'native')
  save(Hjerneslag_behandlet_slagenhet_sh, file = "Hjerneslag_behandlet_slagenhet_sh.RData")

  Hjerneslag_tromsbolyse_bo <- read.table('E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Hjerneslag/Resultater/Trombolyse_BO.csv', sep = ';', header = T, encoding = 'native')

  Hjerneslag_tromsbolyse_bo <- Hjerneslag_tromsbolyse_bo[, -2]
  Hjerneslag_tromsbolyse_bo$AldKjGr <- Hjerneslag_tromsbolyse_bo$ald_gr +
    Hjerneslag_tromsbolyse_bo$ErMann*max(Hjerneslag_tromsbolyse_bo$ald_gr)
  Hjerneslag_tromsbolyse_bo <- Hjerneslag_tromsbolyse_bo[, -c(3,4)]
  names(Hjerneslag_tromsbolyse_bo)[c(2,4)] <- c('bohf', 'N')
  save(Hjerneslag_tromsbolyse_bo, file = "Hjerneslag_tromsbolyse_bo.RData")

  Hjerneslag_tromsbolyse_sh <- read.table('E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Hjerneslag/Resultater/Trombolyse_BEH.csv', sep = ';', header = T, encoding = 'native')
  save(Hjerneslag_tromsbolyse_sh, file = "Hjerneslag_tromsbolyse_sh.RData")

  setwd(currentDir)
}




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



