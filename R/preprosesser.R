#' Les og preprosesser data for fremstilling i figur
#'
#' Denne funksjonenen laster dataen fra forskjellige kilder og
#' omformer den til formen forventet av funksjonene i IndikatoR
#'
#' @return En rekke datasett organisert som forventet av funksjonene i IndikatoR
#' @export
indikatorLastdataOgPreprosesser <- function()
{
  currentDir <- getwd()
  setwd('E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/R-kode/indikatoR/data/')

  # Standard sykehusnavn
  sykehusnavn <- read.table('E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/R-kode/indikatoR/doc/sykehusnavn.csv', header=TRUE, sep=";", encoding = 'UFT-8')
  names(sykehusnavn) <- c("BehSh_nr","BehSh_lang", "BehSh_kort")

  # Standard BOHF-navn
  # I bohf_gammel er OUS erstattet med Oslo (Lovisenberg og Diakonhjemmet mangler)
  bohf_navn <- read.table('E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/R-kode/indikatoR/doc/bohf_navn.csv', header=TRUE, sep=";", encoding = 'UFT-8')
  names(bohf_navn) <- c("bohf_nr","bohf_gammel", "bohf_navn")

  #############################################################################################
  ## Brystkreft - brystbevarende  #####################################################

  raatall <- read.table('E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Brystkreft/Resultater/Tilpassede_datasett_KT/Brystbevarende_sh_v2.csv', header=TRUE, sep=";", encoding = 'UFT-8')
  raatall <- merge(raatall, sykehusnavn_lang[,c('BehSh_lang', 'BehSh_kort')],
                                   by.x = c("Behandlende.sykehus"), by.y = c("BehSh_lang"), all.x = TRUE, all.y = FALSE)
  raatall$Behandlende.sykehus <- raatall$BehSh_kort
  raatall <- raatall[ , -which(names(raatall)=="BehSh_kort")]

  AntTilfeller <- raatall[, 2:4]
  AntTotalt <- raatall[, 5:7]
  colnames(AntTilfeller) <- c('2013','2014','2015')
  rownames(AntTilfeller) <- as.character(raatall$Behandlende.sykehus)
  colnames(AntTotalt) <- c('2013','2014','2015')
  rownames(AntTotalt) <- as.character(raatall$Behandlende.sykehus)
  Brystbevarende_sh_v2 <- list(AntTilfeller=AntTilfeller, AntTotalt=AntTotalt)
  save(Brystbevarende_sh_v2, file = "Brystbevarende_sh_v2.RData")


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

  raatall <- read.table('E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Brystkreft/Resultater/Tilpassede_datasett_KT/Ki67_sh_v2.csv', header=TRUE, sep=";", encoding = 'UFT-8')
  raatall <- merge(raatall, sykehusnavn_lang[,c('BehSh_lang', 'BehSh_kort')],
                   by.x = c("Behandlende.sykehus"), by.y = c("BehSh_lang"), all.x = TRUE, all.y = FALSE)
  raatall$Behandlende.sykehus <- raatall$BehSh_kort
  raatall <- raatall[ , -which(names(raatall)=="BehSh_kort")]
  BrystKi67sh2015_v2 <- raatall[ , c(1, 4,7,10,13)]
  names(BrystKi67sh2015_v2) <- c('Behandlende sykehus', 'Lav <15%', 'Intermediær 15-30%', 'Høy >30%', 'Ukjent')
  save(BrystKi67sh2015_v2, file = "BrystKi672015_sh_v2.RData")


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
  raatall <- read.table('E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Brystkreft/Resultater/Tilpassede_datasett_KT/BrystkreftHistologi_sh_v2.csv', header=TRUE, sep=";", encoding = 'UFT-8')
  raatall <- merge(raatall, sykehusnavn_lang[,c('BehSh_lang', 'BehSh_kort')],
                   by.x = c("Behandlende.sykehus"), by.y = c("BehSh_lang"), all.x = TRUE, all.y = FALSE)
  raatall$Behandlende.sykehus <- raatall$BehSh_kort
  raatall <- raatall[ , -which(names(raatall)=="BehSh_kort")]
  BrystkreftHistologi_sh_v2 <- raatall[ , c(1, 4,7,10,13)]
  names(BrystkreftHistologi_sh_v2) <- c('Behandlende sykehus', 'Grad 1', 'Grad 2', 'Grad 3', 'Ukjent')
  save(BrystkreftHistologi_sh_v2, file = "BrystkreftHistologi_sh_v2.RData")

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

  Innb2015aldkj <- read.table('E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/R-kode/indikatoR/doc/Innbyggere2015aldkj.csv', sep = ';', header = T, encoding = 'UTF-8')
  save(Innb2015aldkj, file = "Innb2015aldkj.RData")


  ##############################################################################################
  ################  Hoftebrudd #########################################################

  ### Ferdig justerte tall ######################################

  Hoftebrudd_Preoperativ_liggetid_bo_justert <- read.table('E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Hoftebrudd/Resultater/Preoperativ_liggetid.csv', sep = ';', header = T, encoding = 'native')
  save(Hoftebrudd_Preoperativ_liggetid_bo_justert, file = "Hoftebrudd_Preoperativ_liggetid_bo_justert.RData")

  Hoftebrudd_Preoperativ_liggetid_sh_v2 <- read.table('E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Hoftebrudd/Resultater/Preoperativ_liggetid_Behsh_justert.csv', sep = ';', header = T, encoding = 'native')
  Hoftebrudd_Preoperativ_liggetid_sh_v2 <- Hoftebrudd_Preoperativ_liggetid_sh_v2[, c(2:8,1)]
  Hoftebrudd_Preoperativ_liggetid_sh_v2 <- merge(Hoftebrudd_Preoperativ_liggetid_sh_v2, sykehusnavn[,c('BehSh_nr', 'BehSh_kort')],
                    by.x = c("behsh"), by.y = c("BehSh_nr"), all.x = TRUE, all.y = FALSE)
  Hoftebrudd_Preoperativ_liggetid_sh_v2$behsh_txt <- Hoftebrudd_Preoperativ_liggetid_sh_v2$BehSh_kort
  Hoftebrudd_Preoperativ_liggetid_sh_v2 <- Hoftebrudd_Preoperativ_liggetid_sh_v2[ , -which(names(Hoftebrudd_Preoperativ_liggetid_sh_v2)=="BehSh_kort")]

  Hoftebrudd_Preoperativ_liggetid_sh_v2$behsh <- Hoftebrudd_Preoperativ_liggetid_sh_v2$behsh_txt
  Hoftebrudd_Preoperativ_liggetid_sh_v2 <- Hoftebrudd_Preoperativ_liggetid_sh_v2[ , -which(names(Hoftebrudd_Preoperativ_liggetid_sh_v2)=="behsh_txt")]
  save(Hoftebrudd_Preoperativ_liggetid_sh_v2, file = "Hoftebrudd_Preoperativ_liggetid_sh_v2.RData")

  Hoftebrudd_Produksjon_sh_v2 <- read.table('E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Hoftebrudd/Resultater/Hoftebruddteknikk_behsh_v2.csv',
                                         sep = ';', header = T, encoding = 'native', strip.white=TRUE)

  Hoftebrudd_Produksjon_sh_v2 <- merge(Hoftebrudd_Produksjon_sh_v2, sykehusnavn[,c('BehSh_nr', 'BehSh_kort')],
                                                 by.x = c("BehSh"), by.y = c("BehSh_nr"), all.x = TRUE, all.y = FALSE)
  Hoftebrudd_Produksjon_sh_v2$BehSh_txt <- Hoftebrudd_Produksjon_sh_v2$BehSh_kort
  Hoftebrudd_Produksjon_sh_v2 <- Hoftebrudd_Produksjon_sh_v2[ , -which(names(Hoftebrudd_Produksjon_sh_v2)=="BehSh_kort")]

  Hoftebrudd_Produksjon_sh_v2 <- Hoftebrudd_Produksjon_sh_v2[, -1]

  names(Hoftebrudd_Produksjon_sh_v2)[names(Hoftebrudd_Produksjon_sh_v2)=='BehSh_txt'] <- 'Behandlende.sykehus'
  save(Hoftebrudd_Produksjon_sh_v2, file = "Hoftebrudd_Produksjon_sh_v2.RData")

  Hoftebrudd_Produksjon_bo <- read.table('E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Hoftebrudd/Resultater/Hoftebruddteknikk.csv',
                                         sep = ';', header = T, encoding = 'native', strip.white=TRUE)
  save(Hoftebrudd_Produksjon_bo, file = "Hoftebrudd_Produksjon_bo.RData")

  ##########      UNDER ARBEID      #####################################################################
  ##########      UNDER ARBEID      #####################################################################
  ##########      UNDER ARBEID      #####################################################################
  ##########      UNDER ARBEID      #####################################################################

  Hoftebrudd_Produksjon_bo_aldkjgr <- read.table('E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Hoftebrudd/Resultater/Hoftebrudd_BO_teknikk_raadata_alt.csv',
                                                 sep = ';', header = T, encoding = 'native', strip.white=TRUE)
  Hoftebrudd_Produksjon_bo_aldkjgr <- Hoftebrudd_Produksjon_bo_aldkjgr[,-7]
  Hoftebrudd_Produksjon_bo_aldkjgr$bohf <- as.character(Hoftebrudd_Produksjon_bo_aldkjgr$bohf_txt)
  Hoftebrudd_Produksjon_bo_aldkjgr$bohf[Hoftebrudd_Produksjon_bo_aldkjgr$bohf=='Landet'] <- 'Norge'
  Hoftebrudd_Produksjon_bo_aldkjgr$AldKjGr <- Hoftebrudd_Produksjon_bo_aldkjgr$alder +
    Hoftebrudd_Produksjon_bo_aldkjgr$ermann*max(Hoftebrudd_Produksjon_bo_aldkjgr$alder)
  Hoftebrudd_Produksjon_bo_aldkjgr <- Hoftebrudd_Produksjon_bo_aldkjgr[,-c(1, 3:4)]
  Hoftebrudd_Produksjon_bo_aldkjgr2015 <- Hoftebrudd_Produksjon_bo_aldkjgr[Hoftebrudd_Produksjon_bo_aldkjgr$aar==2015, ]
  Hoftebrudd_Produksjon_bo_aldkjgr2015 <- Hoftebrudd_Produksjon_bo_aldkjgr2015[, -1]
  Hoftebrudd_Produksjon_bo_aldkjgr2015 <- tidyr::spread(Hoftebrudd_Produksjon_bo_aldkjgr2015, 'teknikk', 'Antall')

  tmp <- Hoftebrudd_Produksjon_bo_aldkjgr2015
  tmp[is.na(tmp)] <- 0

  vekt <- tmp[tmp$bohf=='Norge', 6]/sum(tmp[tmp$bohf=='Norge', 6])
  vektFrame <- data.frame('AldKjGr'=sort(unique(tmp$AldKjGr)), 'vekt'=vekt)

  tmp[, 3:5] <- tmp[, 3:5]/tmp$Totalt
  tmp[which(is.nan(tmp[, 3])),3:5] <- 0
  tmp <- merge(tmp, vektFrame, by='AldKjGr', all.x=T)
  tmp[, c(3:5)] <- tmp[, c(3:5)] * tmp$vekt
  Hoftebrudd_andeler_2015_bo_justert <- aggregate(tmp[, c(3:6)], by=list(bohf=tmp$bohf), sum)
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

  ############ Korrigerte tall!!!!!!!!!!!!!!!
  Hoftebrudd_Produksjon_bo_aldkjgr_korr <- read.table('E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Hoftebrudd/Resultater/Hoftebrudd_BOHF_teknikk.csv',
                                                 sep = ';', header = T, encoding = 'native', strip.white=TRUE)
  names(Hoftebrudd_Produksjon_bo_aldkjgr_korr)[names(Hoftebrudd_Produksjon_bo_aldkjgr_korr)=='Bohf'] <- 'bohf'
  Hoftebrudd_Produksjon_bo_aldkjgr_korr <- merge(Hoftebrudd_Produksjon_bo_aldkjgr_korr, bohf_navn[,c('bohf_nr', 'bohf_gammel')],
                                                 by.x = c("bohf"), by.y = c("bohf_nr"), all.x = TRUE, all.y = FALSE)
  Hoftebrudd_Produksjon_bo_aldkjgr_korr$bohf <- Hoftebrudd_Produksjon_bo_aldkjgr_korr$bohf_gammel
  Hoftebrudd_Produksjon_bo_aldkjgr_korr <- Hoftebrudd_Produksjon_bo_aldkjgr_korr[ , -which(names(Hoftebrudd_Produksjon_bo_aldkjgr_korr)=="bohf_gammel")]
  Hoftebrudd_Produksjon_bo_aldkjgr_korr$Teknikk <- Hoftebrudd_Produksjon_bo_aldkjgr_korr$teknikk_txt
  Hoftebrudd_Produksjon_bo_aldkjgr_korr <- Hoftebrudd_Produksjon_bo_aldkjgr_korr[, -which(names(Hoftebrudd_Produksjon_bo_aldkjgr_korr)
                                                                                          %in% c('bohf_txt', 'teknikk_txt'))]
  Hoftebrudd_Produksjon_bo_aldkjgr_korr$AldKjGr <- Hoftebrudd_Produksjon_bo_aldkjgr_korr$aldergr +
  Hoftebrudd_Produksjon_bo_aldkjgr_korr$Ermann*max(Hoftebrudd_Produksjon_bo_aldkjgr_korr$aldergr)
  Hoftebrudd_Produksjon_bo_aldkjgr_korr <- Hoftebrudd_Produksjon_bo_aldkjgr_korr[ , -which(names(Hoftebrudd_Produksjon_bo_aldkjgr_korr)
                                                                                  %in% c('Ermann', 'aldergr'))]
  Norge <- aggregate(Hoftebrudd_Produksjon_bo_aldkjgr_korr$Antall,
                     by=list(Teknikk=Hoftebrudd_Produksjon_bo_aldkjgr_korr$Teknikk,
                             aar=Hoftebrudd_Produksjon_bo_aldkjgr_korr$aar, AldKjGr=Hoftebrudd_Produksjon_bo_aldkjgr_korr$AldKjGr), sum, na.rm=T)
  Norge$bohf <- 'Norge'
  names(Norge)[which(names(Norge)=='x')] <- 'Antall'

  Hoftebrudd_Produksjon_bo_aldkjgr_korr <- Hoftebrudd_Produksjon_bo_aldkjgr_korr[, c("aar", "Teknikk", "Antall", "bohf", "AldKjGr")]
  Norge <- Norge[, c("aar", "Teknikk", "Antall", "bohf", "AldKjGr")]
  Hoftebrudd_Produksjon_bo_aldkjgr_korr <- rbind(Hoftebrudd_Produksjon_bo_aldkjgr_korr, Norge)

  Hoftebrudd_Produksjon_bo_aldkjgr2015_v2 <- Hoftebrudd_Produksjon_bo_aldkjgr_korr[Hoftebrudd_Produksjon_bo_aldkjgr_korr$aar==2015, ]
  Hoftebrudd_Produksjon_bo_aldkjgr2015_v2 <- Hoftebrudd_Produksjon_bo_aldkjgr2015_v2[, -1]
  Hoftebrudd_Produksjon_bo_aldkjgr2015_v2 <- tidyr::spread(Hoftebrudd_Produksjon_bo_aldkjgr2015_v2, 'Teknikk', 'Antall')
  Hoftebrudd_Produksjon_bo_aldkjgr2015_v2[is.na(Hoftebrudd_Produksjon_bo_aldkjgr2015_v2)] <- 0
  Hoftebrudd_Produksjon_bo_aldkjgr2015_v2$Totalt <- rowSums(Hoftebrudd_Produksjon_bo_aldkjgr2015_v2[, 3:5])

  tmp <- Hoftebrudd_Produksjon_bo_aldkjgr2015_v2

  vekt <- tmp[tmp$bohf=='Norge', 6]/sum(tmp[tmp$bohf=='Norge', 6])
  vektFrame <- data.frame('AldKjGr'=sort(unique(tmp$AldKjGr)), 'vekt'=vekt)

  tmp[, 3:5] <- tmp[, 3:5]/tmp$Totalt
  tmp[which(is.nan(tmp[, 3])),3:5] <- 0
  tmp <- merge(tmp, vektFrame, by='AldKjGr', all.x=T)
  tmp[, c(3:5)] <- tmp[, c(3:5)] * tmp$vekt
  Hoftebrudd_andeler_2015_bo_justert_v2 <- aggregate(tmp[, c(3:6)], by=list(bohf=tmp$bohf), sum)
  Hoftebrudd_andeler_2015_bo_justert_v2[,2:4] <- Hoftebrudd_andeler_2015_bo_justert_v2[,2:4]/rowSums(Hoftebrudd_andeler_2015_bo_justert_v2[,2:4])
  rownames(Hoftebrudd_andeler_2015_bo_justert_v2) <- Hoftebrudd_andeler_2015_bo_justert_v2$bohf
  Hoftebrudd_andeler_2015_bo_justert_v2 <- Hoftebrudd_andeler_2015_bo_justert_v2[, -1]
  names(Hoftebrudd_andeler_2015_bo_justert_v2)[names(Hoftebrudd_andeler_2015_bo_justert_v2)=='Totalt'] <- 'N'
  names(Hoftebrudd_andeler_2015_bo_justert_v2)[names(Hoftebrudd_andeler_2015_bo_justert_v2)=='Pinning'] <- 'To skruer eller pinner'
  names(Hoftebrudd_andeler_2015_bo_justert_v2)[names(Hoftebrudd_andeler_2015_bo_justert_v2)=='Hemipro'] <- 'Hemiprotese'
  names(Hoftebrudd_andeler_2015_bo_justert_v2)[names(Hoftebrudd_andeler_2015_bo_justert_v2)=='Totalpr'] <- 'Totalprotese'
  Hoftebrudd_andeler_2015_bo_justert_v2 <- Hoftebrudd_andeler_2015_bo_justert_v2[, c(2,1,3,4)]
  save(Hoftebrudd_andeler_2015_bo_justert_v2, file = "Hoftebrudd_andeler_2015_bo_justert_v2.RData")

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
  Hofteproteser_rater <- merge(Hofteproteser_rater, bohf_navn[,c('bohf_nr', 'bohf_gammel')],
                                                 by.x = c("BoHF"), by.y = c("bohf_nr"), all.x = TRUE, all.y = FALSE)
  Hofteproteser_rater$BoHF_txt <- Hofteproteser_rater$bohf_gammel
  Hofteproteser_rater <- Hofteproteser_rater[ , -which(names(Hofteproteser_rater)=="bohf_gammel")]
  Hofteproteser_rater <- Hofteproteser_rater[ , -which(names(Hofteproteser_rater)=="BoHF")]
  Hofteproteser_rater <- Hofteproteser_rater[, c(1:3,5,4)]
  names(Hofteproteser_rater) <- c('aar', 'bohf', 'andel', 'antall', 'N')
  save(Hofteproteser_rater, file = "Hofteproteser_rater.RData")

  Kneproteser_rater <- read.table('E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Proteser/Resultater/Rater_kneproteser.csv',
                                    sep = ';', header = T, encoding = 'native', strip.white=TRUE)
  Kneproteser_rater <- merge(Kneproteser_rater, bohf_navn[,c('bohf_nr', 'bohf_gammel')],
                               by.x = c("BoHF"), by.y = c("bohf_nr"), all.x = TRUE, all.y = FALSE)
  Kneproteser_rater$BoHF_txt <- Kneproteser_rater$bohf_gammel
  Kneproteser_rater <- Kneproteser_rater[ , -which(names(Kneproteser_rater)=="bohf_gammel")]
  Kneproteser_rater <- Kneproteser_rater[ , -which(names(Kneproteser_rater)=="BoHF")]
  Kneproteser_rater <- Kneproteser_rater[, c(1:3,5,4)]
  names(Kneproteser_rater) <- c('aar', 'bohf', 'andel', 'antall', 'N')
  save(Kneproteser_rater, file = "Kneproteser_rater.RData")

  ## NKR rater ###############
  Prolapskirurgi_rater <- read.table('E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Rygg/NPR/Rater_Prolapskirurgi.csv',
                                  sep = ';', header = T, encoding = 'native', strip.white=TRUE)

  Prolapskirurgi_rater <- Prolapskirurgi_rater[, -2]
  Prolapskirurgi_rater <- Prolapskirurgi_rater[, c(1:3,5,4)]
  names(Prolapskirurgi_rater) <- c('aar', 'bohf', 'andel', 'antall', 'N')
  save(Prolapskirurgi_rater, file = "Prolapskirurgi_rater.RData")


  ############ Hjerteinfarkt ############################################################
  ###################################################################################


  Angio_sh_v2 <- read.table('E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Hjerteinfarkt/Resultater/Angio_BEH_v2.csv',
                         sep = ';', header = T, encoding = 'native', strip.white=TRUE)
  Angio_sh_v2 <- merge(Angio_sh_v2, sykehusnavn[,c('BehSh_nr', 'BehSh_kort')],
                                       by.x = c("BehSh"), by.y = c("BehSh_nr"), all.x = TRUE, all.y = FALSE)
  Angio_sh_v2$Sykehus <- Angio_sh_v2$BehSh_kort
  Angio_sh_v2 <- Angio_sh_v2[ , -which(names(Angio_sh_v2)=="BehSh_kort")]
  Angio_sh_v2 <- Angio_sh_v2[, -which(names(Angio_sh_v2)=="BehSh")]
  save(Angio_sh_v2, file = "Angio_sh_v2.RData")

  Angio_bo <- read.table('E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Hjerteinfarkt/Resultater/Angio_BO.csv',
             sep = ';', header = T, encoding = 'native', strip.white=TRUE)
  Angio_bo <- Angio_bo[, -c(2,8)]
  Angio_bo$AldKjGr <- Angio_bo$ald_gr +Angio_bo$ErMann*max(Angio_bo$ald_gr)
  Angio_bo <- Angio_bo[, -c(3,4)]
  names(Angio_bo)[c(2,5)] <- c('bohf', 'N')
  save(Angio_bo, file = "Angio_bo.RData")

  Revaskularisering_sh_v2 <- read.table('E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Hjerteinfarkt/Resultater/Revaskularisering_BEH_v2.csv',
                                     sep = ';', header = T, encoding = 'native', strip.white=TRUE)
  Revaskularisering_sh_v2 <- merge(Revaskularisering_sh_v2, sykehusnavn[,c('BehSh_nr', 'BehSh_kort')],
                       by.x = c("BehSh"), by.y = c("BehSh_nr"), all.x = TRUE, all.y = FALSE)
  Revaskularisering_sh_v2$Sykehus <- Revaskularisering_sh_v2$BehSh_kort
  Revaskularisering_sh_v2 <- Revaskularisering_sh_v2[ , -which(names(Revaskularisering_sh_v2)=="BehSh_kort")]
  Revaskularisering_sh_v2 <- Revaskularisering_sh_v2[, -which(names(Revaskularisering_sh_v2)=="BehSh")]
  save(Revaskularisering_sh_v2, file = "Revaskularisering_sh_v2.RData")

  Revaskularisering_bo <- read.table('E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Hjerteinfarkt/Resultater/Revaskularisering_BO.csv',
             sep = ';', header = T, encoding = 'native', strip.white=TRUE)
  Revaskularisering_bo <- merge(Revaskularisering_bo, bohf_navn[,c('bohf_nr', 'bohf_gammel')],
                             by.x = c("BoHF"), by.y = c("bohf_nr"), all.x = TRUE, all.y = FALSE)
  Revaskularisering_bo$BoHF_txt <- Revaskularisering_bo$bohf_gammel
  Revaskularisering_bo <- Revaskularisering_bo[ , -which(names(Revaskularisering_bo)=="bohf_gammel")]
  Revaskularisering_bo <- Revaskularisering_bo[ , -which(names(Revaskularisering_bo)=="BoHF")]
  Revaskularisering_bo$AldKjGr <- Revaskularisering_bo$ald_gr +Revaskularisering_bo$ErMann*max(Revaskularisering_bo$ald_gr)
  Revaskularisering_bo <- Revaskularisering_bo[ , -which(names(Revaskularisering_bo)=="ald_gr")]
  Revaskularisering_bo <- Revaskularisering_bo[ , -which(names(Revaskularisering_bo)=="ErMann")]
  names(Revaskularisering_bo)[c(2,5)] <- c('bohf', 'N')
  save(Revaskularisering_bo, file = "Revaskularisering_bo.RData")

  ############ Hjerneslag ############################################################
  ###################################################################################


  Hjerneslag_behandlet_slagenhet_bo <- read.table('E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Hjerneslag/Resultater/Beh_slagenhet_BO.csv',
                                                  sep = ';', header = T, encoding = 'native')
  Hjerneslag_behandlet_slagenhet_bo <- merge(Hjerneslag_behandlet_slagenhet_bo, bohf_navn[,c('bohf_nr', 'bohf_gammel')],
                               by.x = c("BoHF"), by.y = c("bohf_nr"), all.x = TRUE, all.y = FALSE)
  Hjerneslag_behandlet_slagenhet_bo$BoHF_txt <- Hjerneslag_behandlet_slagenhet_bo$bohf_gammel
  Hjerneslag_behandlet_slagenhet_bo <- Hjerneslag_behandlet_slagenhet_bo[ , -which(names(Hjerneslag_behandlet_slagenhet_bo)=="bohf_gammel")]
  Hjerneslag_behandlet_slagenhet_bo <- Hjerneslag_behandlet_slagenhet_bo[ , -which(names(Hjerneslag_behandlet_slagenhet_bo)=="BoHF")]
  Hjerneslag_behandlet_slagenhet_bo$AldKjGr <- Hjerneslag_behandlet_slagenhet_bo$ald_gr +
    Hjerneslag_behandlet_slagenhet_bo$ErMann*max(Hjerneslag_behandlet_slagenhet_bo$ald_gr)
  Hjerneslag_behandlet_slagenhet_bo <- Hjerneslag_behandlet_slagenhet_bo[, -c(3,4)]
  names(Hjerneslag_behandlet_slagenhet_bo)[c(2,4)] <- c('bohf', 'N')
  save(Hjerneslag_behandlet_slagenhet_bo, file = "Hjerneslag_behandlet_slagenhet_bo.RData")

  Hjerneslag_behandlet_slagenhet_sh_v2 <- read.table('E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Hjerneslag/Resultater/Beh_slagenhet_BEH_V2.csv', sep = ';', header = T, encoding = 'native')
  Hjerneslag_behandlet_slagenhet_sh_v2 <- merge(Hjerneslag_behandlet_slagenhet_sh_v2, sykehusnavn[,c('BehSh_nr', 'BehSh_kort')],
                                   by.x = c("BehSh"), by.y = c("BehSh_nr"), all.x = TRUE, all.y = FALSE)
  Hjerneslag_behandlet_slagenhet_sh_v2$Helseenhet <- Hjerneslag_behandlet_slagenhet_sh_v2$BehSh_kort
  Hjerneslag_behandlet_slagenhet_sh_v2 <- Hjerneslag_behandlet_slagenhet_sh_v2[ , -which(names(Hjerneslag_behandlet_slagenhet_sh_v2)=="BehSh_kort")]
  Hjerneslag_behandlet_slagenhet_sh_v2 <- Hjerneslag_behandlet_slagenhet_sh_v2[, -which(names(Hjerneslag_behandlet_slagenhet_sh_v2)=="BehSh")]
  save(Hjerneslag_behandlet_slagenhet_sh_v2, file = "Hjerneslag_behandlet_slagenhet_sh_v2.RData")

  Hjerneslag_tromsbolyse_bo <- read.table('E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Hjerneslag/Resultater/Trombolyse_BO.csv', sep = ';', header = T, encoding = 'native')

  Hjerneslag_tromsbolyse_bo <- Hjerneslag_tromsbolyse_bo[, -2]
  Hjerneslag_tromsbolyse_bo$AldKjGr <- Hjerneslag_tromsbolyse_bo$ald_gr +
    Hjerneslag_tromsbolyse_bo$ErMann*max(Hjerneslag_tromsbolyse_bo$ald_gr)
  Hjerneslag_tromsbolyse_bo <- Hjerneslag_tromsbolyse_bo[, -c(3,4)]
  names(Hjerneslag_tromsbolyse_bo)[c(2,4)] <- c('bohf', 'N')
  save(Hjerneslag_tromsbolyse_bo, file = "Hjerneslag_tromsbolyse_bo.RData")

  Hjerneslag_tromsbolyse_sh_v2 <- read.table('E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Hjerneslag/Resultater/Trombolyse_BEH_v2.csv', sep = ';', header = T, encoding = 'native')
  Hjerneslag_tromsbolyse_sh_v2 <- merge(Hjerneslag_tromsbolyse_sh_v2, sykehusnavn[,c('BehSh_nr', 'BehSh_kort')],
                                                by.x = c("BehSh"), by.y = c("BehSh_nr"), all.x = TRUE, all.y = FALSE)
  Hjerneslag_tromsbolyse_sh_v2$Helseenhet <- Hjerneslag_tromsbolyse_sh_v2$BehSh_kort
  Hjerneslag_tromsbolyse_sh_v2 <- Hjerneslag_tromsbolyse_sh_v2[ , -which(names(Hjerneslag_tromsbolyse_sh_v2)=="BehSh_kort")]
  Hjerneslag_tromsbolyse_sh_v2 <- Hjerneslag_tromsbolyse_sh_v2[, -which(names(Hjerneslag_tromsbolyse_sh_v2)=="BehSh")]
  save(Hjerneslag_tromsbolyse_sh_v2, file = "Hjerneslag_tromsbolyse_sh_v2.RData")


  setwd(currentDir)
}
