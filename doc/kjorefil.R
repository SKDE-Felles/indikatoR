rm(list = ls())
library(indikatoR)
setwd('E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/R-kode/indikatoR/doc/')

hjemkatalog <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/R-kode/indikatoR/doc/'
figurkatalog <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/'


skrivSKDEdisk <- F
skrivSKDEdiskPNG <- T

terskel_sh <- 10
terskel_hf <- 10
terskel_bo1 <- 15
terskel_bo2 <- 40
terskel_bo3 <- 30


############  Brystkreft - brystbevarende  ###################################################

figurnavn <- 'Brystbevarende_sh'
outfile <- paste0(hjemkatalog,'figurer/',figurnavn,'.pdf')
if (skrivSKDEdisk) {outfile <- paste0(figurkatalog,'PDF/',figurnavn,'.pdf')}
if (skrivSKDEdiskPNG) {outfile <- paste0(figurkatalog,'png/',figurnavn,'.png')}
tittel=c('Andel med brystbevarende kirurgi for tumorstørrelse 0-30 mm,', 'pr. behandlende sykehus')
AntTilfeller=Brystbevarende_sh_v2$AntTilfeller
N=Brystbevarende_sh_v2$AntTotalt
decreasing=F
terskel=terskel_sh
indikatorFigAndelGrVar(AntTilfeller=AntTilfeller, N=N, outfile=outfile, tittel=tittel, sideTxt='Behandlende sykehus',
                       skriftStr=1, width=600, height=600, decreasing=decreasing,
                       terskel=terskel, minstekrav = 70, maal = 80, legPlass='top')

### Aldersjustering
AntTilfeller=Brystbevarende_bo_aldersgr$AntTilfeller
N=Brystbevarende_bo_aldersgr$AntTotalt
terskel=terskel_bo1
figurnavn <- 'Brystbevarende_bo_justert'
outfile <- paste0(hjemkatalog,'figurer/',figurnavn,'.pdf')
if (skrivSKDEdisk) {outfile <- paste0(figurkatalog,'PDF/',figurnavn,'.pdf')}
if (skrivSKDEdiskPNG) {outfile <- paste0(figurkatalog,'png/',figurnavn,'.png')}
tittel=c('Andel med brystbevarende kirurgi for tumorstørrelse 0-30 mm (aldersjustert),', 'pr. boområde/opptaksområde')

indikatorFigAndelGrVar_justert(AntTilfeller=AntTilfeller, N=N, outfile=outfile, tittel=tittel, #skriftStr=1.1,
                               width=600, height=600, decreasing=decreasing, terskel=terskel,
                               minstekrav = 70, maal = 80)


###############################################################################
##########  Brystkreft Ki 67  #######################################################

# load("C:/GIT/indikatoR/data/BrystKi672015_sh.RData")

Antall <- BrystKi67sh2015_v2
tittel=c('Fordeling av Ki67 proliferasjonsrate 2015,', 'pr. behandlende sykehus')
inkl_ukjent=F
figurnavn <- 'TestresultaterKi67_sh'
outfile <- paste0(hjemkatalog,'figurer/',figurnavn,'.pdf')
if (skrivSKDEdisk) {outfile <- paste0(figurkatalog,'PDF/',figurnavn,'.pdf')}
if (skrivSKDEdiskPNG) {outfile <- paste0(figurkatalog,'png/',figurnavn,'.png')}
terskel <- terskel_sh
indikatorFigAndelStabelGrVar_mUkjent(Antall=Antall, outfile, tittel=tittel, sideTxt='Behandlende sykehus', skriftStr=1.1,
                                     inkl_ukjent=inkl_ukjent, width=600, height=600, terskel=terskel)

tittel=c('Fordeling av Ki67 proliferasjonsrate 2015 (aldersjustert),', ' pr. boområde/opptaksområde')
figurnavn <- 'TestresultaterKi67_bo_justert'
outfile <- paste0(hjemkatalog,'figurer/',figurnavn,'.pdf')
if (skrivSKDEdisk) {outfile <- paste0(figurkatalog,'PDF/',figurnavn,'.pdf')}
if (skrivSKDEdiskPNG) {outfile <- paste0(figurkatalog,'png/',figurnavn,'.png')}
Antall <- BrystKi67Bo2015_aldersgr
terskel <- terskel_bo1
indikatorFigAndelStabelGrVar_justert(Antall=Antall, outfile=outfile, tittel=tittel,
                                     width=600, height=600, terskel=terskel)

###############################################################################
##########  Brystkreft histologi  #######################################################

Antall <- BrystkreftHistologi_sh_v2
tittel=c('Fordeling av histologisk grad (Nottingham) 2015,', ' pr. behandlende sykehus')
figurnavn <- 'HistologiskGrad(Nottingham)_sh'
outfile <- paste0(hjemkatalog,'figurer/',figurnavn,'.pdf')
if (skrivSKDEdisk) {outfile <- paste0(figurkatalog,'PDF/',figurnavn,'.pdf')}
if (skrivSKDEdiskPNG) {outfile <- paste0(figurkatalog,'png/',figurnavn,'.png')}
terskel <- terskel_sh
indikatorFigAndelStabelGrVar_mUkjent(Antall=Antall, outfile, tittel=tittel, sideTxt='Behandlende sykehus', inkl_ukjent=F, terskel=terskel, skriftStr = 1.1)


tittel=c('Fordeling av histologisk grad (Nottingham) 2015 (aldersjustert),', ' pr. boområde/opptaksområde')
figurnavn <- 'HistologiskGrad(Nottingham)_bo_justert'
outfile <- paste0(hjemkatalog,'figurer/',figurnavn,'.pdf')
if (skrivSKDEdisk) {outfile <- paste0(figurkatalog,'PDF/',figurnavn,'.pdf')}
if (skrivSKDEdiskPNG) {outfile <- paste0(figurkatalog,'png/',figurnavn,'.png')}
Antall <- BrystkreftHistologi_bo2015_aldersgr
terskel <- terskel_bo1
indikatorFigAndelStabelGrVar_justert(Antall=Antall, outfile=outfile, tittel=tittel,
                                     width=600, height=600, terskel=terskel)




################################################################################
################### The hips don't lie ###################################

decreasing=F
Andeler <- Hoftebrudd_Preoperativ_liggetid_bo_justert[, -c(3,5,7,8)]
names(Andeler)[names(Andeler)=='andel24'] <- 'andel'
names(Andeler)[names(Andeler)=='bohf_txt'] <- 'bohf'
figurnavn <- 'HofteOp24_bo'
outfile <- paste0(hjemkatalog,'figurer/',figurnavn,'.pdf')
if (skrivSKDEdisk) {outfile <- paste0(figurkatalog,'PDF/',figurnavn,'.pdf')}
if (skrivSKDEdiskPNG) {outfile <- paste0(figurkatalog,'png/',figurnavn,'.png')}
tittel <- c('Andel hoftebrudd operert innen 24 timer (alders- og kjønnsjustert),', 'pr. boområde/opptaksområde')

terskel=terskel_bo2
indikatorFigAndelGrVar_preberegnet(Andeler=Andeler, outfile=outfile, tittel=tittel, width=600, height=600,
                                   til100=F, decreasing=decreasing, terskel=terskel, minstekrav=NA, maal=NA)


Andeler <- Hoftebrudd_Preoperativ_liggetid_bo_justert[, -c(3,4,7,8)]
names(Andeler)[names(Andeler)=='andel48'] <- 'andel'
names(Andeler)[names(Andeler)=='bohf_txt'] <- 'bohf'
figurnavn <- 'HofteOp48_bo'
outfile <- paste0(hjemkatalog,'figurer/',figurnavn,'.pdf')
if (skrivSKDEdisk) {outfile <- paste0(figurkatalog,'PDF/',figurnavn,'.pdf')}
if (skrivSKDEdiskPNG) {outfile <- paste0(figurkatalog,'png/',figurnavn,'.png')}
tittel <- c('Andel hoftebrudd operert innen 48 timer (alders- og kjønnsjustert),', 'pr. boområde/opptaksområde')

indikatorFigAndelGrVar_preberegnet(Andeler=Andeler, outfile=outfile, tittel=tittel, width=600, height=600,
                                   decreasing=decreasing, terskel=terskel, minstekrav=NA, maal=NA)


# load("C:/GIT/indikatoR/data/Hoftebrudd_Preoperativ_liggetid_sh.RData")

Andeler <- Hoftebrudd_Preoperativ_liggetid_sh_v2[ , -c(3,4,7)]
Andeler <- Andeler[, c(1,2,4,3)]
names(Andeler)[names(Andeler)=='andel24'] <- 'andel'
names(Andeler)[names(Andeler)=='Antall'] <- 'antall'
names(Andeler)[names(Andeler)=='behsh'] <- 'bohf'
figurnavn <- 'HofteOp24_sh'
outfile <- paste0(hjemkatalog,'figurer/',figurnavn,'.pdf')
if (skrivSKDEdisk) {outfile <- paste0(figurkatalog,'PDF/',figurnavn,'.pdf')}
if (skrivSKDEdiskPNG) {outfile <- paste0(figurkatalog,'png/',figurnavn,'.png')}
tittel <- 'Andel operert innen 24 timer, pr. behandlende sykehus'
terskel <- terskel_sh
indikatorFigAndelGrVar_preberegnet(Andeler=Andeler, outfile=outfile, tittel=tittel, width=600, height=800,
                                   decreasing=decreasing, terskel=terskel, minstekrav=NA, maal=NA,
                                   skriftStr=1, pktStr=1.4, sideTxt='Behandlende sykehus')

Andeler <- Hoftebrudd_Preoperativ_liggetid_sh_v2[ , -c(3,4,6)]
Andeler <- Andeler[, c(1,2,4,3)]
names(Andeler)[names(Andeler)=='andel48'] <- 'andel'
names(Andeler)[names(Andeler)=='Antall'] <- 'antall'
names(Andeler)[names(Andeler)=='behsh'] <- 'bohf'
figurnavn <- 'HofteOp48_sh'
outfile <- paste0(hjemkatalog,'figurer/',figurnavn,'.pdf')
if (skrivSKDEdisk) {outfile <- paste0(figurkatalog,'PDF/',figurnavn,'.pdf')}
if (skrivSKDEdiskPNG) {outfile <- paste0(figurkatalog,'png/',figurnavn,'.png')}
tittel <- 'Andel operert innen 48 timer, pr. behandlende sykehus'

indikatorFigAndelGrVar_preberegnet(Andeler=Andeler, outfile=outfile, tittel=tittel, width=600, height=800,
                                   decreasing=decreasing, terskel=terskel, minstekrav=NA, maal=NA,
                                   skriftStr=1, pktStr=1.4, sideTxt='Behandlende sykehus')



###### Produksjon

#Antall <- Hoftebrudd_Produksjon_sh_v2[ , -c(3,4,5)]
#names(Antall)[names(Antall)=='Totalt.antall'] <- 'antall'
#names(Antall)[names(Antall)=='Behandlende.sykehus'] <- 'bohf'
#figurnavn <- 'Hoftebrudd_produksjon_sh'
#outfile <- paste0(hjemkatalog,'figurer/',figurnavn,'.pdf')
#if (skrivSKDEdisk) {outfile <- paste0(figurkatalog,'PDF/',figurnavn,'.pdf')}
#if (skrivSKDEdiskPNG) {outfile <- paste0(figurkatalog,'png/',figurnavn,'.png')}
#tittel <- c('Antall hoftebrudd meldt til Nasjonalt Hoftebruddregister,', 'pr. behandlende sykehus')
#pktStr <- 1
#skriftStr <- 0.8
#indikatorFigAntallGrVar(Antall=Antall, outfile=outfile, tittel=tittel, width=800, height=700,
#                                    decreasing=F, xtekst ='Antall', sideTxt='Behandlende sykehus',
#                                    skriftStr=skriftStr, pktStr=pktStr)



##### Type behandling

Andeler <- Hoftebrudd_andeler_2015_bo_justert_v2
tittel <- 'Fordeling av operasjonsmetoder ved hoftebrudd 2015, boområde, justert'
figurnavn <- 'Hofte_operasjonsmetode_bo_justert'
outfile <- paste0(hjemkatalog,'figurer/',figurnavn,'.pdf')
if (skrivSKDEdisk) {outfile <- paste0(figurkatalog,'PDF/',figurnavn,'.pdf')}
if (skrivSKDEdiskPNG) {outfile <- paste0(figurkatalog,'png/',figurnavn,'.png')}
terskel <- terskel_bo2
decreasing <- TRUE
indikatorFigAndelStabelGrVar(Andeler=Andeler, outfile=outfile, tittel=tittel, sideTxt = 'Boområde/opptaksområde',
                             terskel = terskel, decreasing=decreasing)


Andeler <- Hoftebrudd_Produksjon_sh_v2
Andeler <- Andeler[Andeler$aar==2015, ]
Andeler <- Andeler[, -2]
rownames(Andeler) <- Andeler$Behandlende.sykehus
Andeler <- Andeler[, -1]
names(Andeler)[names(Andeler)=='Totalt.antall'] <- 'N'
names(Andeler)[names(Andeler)=='Pinning'] <- 'To skruer eller pinner'
Andeler[,1:3] <- Andeler[,1:3]/rowSums(Andeler[,1:3])
Andeler <- Andeler[Andeler$N > 0, ]
skriftStr <- 1
tittel <- 'Fordeling av operasjonsmetoder ved lårhalsbrudd 2015, pr. behandlende sykehus'
figurnavn <- 'Hofte_operasjonsmetode_sh'
outfile <- paste0(hjemkatalog,'figurer/',figurnavn,'.pdf')
if (skrivSKDEdisk) {outfile <- paste0(figurkatalog,'PDF/',figurnavn,'.pdf')}
if (skrivSKDEdiskPNG) {outfile <- paste0(figurkatalog,'png/',figurnavn,'.png')}
terskel <- terskel_sh
decreasing <- TRUE
indikatorFigAndelStabelGrVar(Andeler=Andeler, outfile=outfile, terskel=terskel, tittel=tittel,
                             decreasing=decreasing, skriftStr=skriftStr, sideTxt = 'Behandlende sykehus')




#Andeler <- Hoftebrudd_rater
#figurnavn <- 'Hoftebrudd_rater_bo'
#outfile <- paste0(hjemkatalog,'figurer/',figurnavn,'.pdf')
#if (skrivSKDEdisk) {outfile <- paste0(figurkatalog,'PDF/',figurnavn,'.pdf')}
#if (skrivSKDEdiskPNG) {outfile <- paste0(figurkatalog,'png/',figurnavn,'.png')}
#tittel <- c('Antall lårhalsbrudd pr. 1000 innbyggere (alders- og kjønnsjustert),', 'pr. boområde/opptaksområde')
#decreasing=F
#terskel=30
#til100 = F
#terskel <- terskel_bo2
#indikatorFigRaterGrVar(Andeler=Andeler, outfile=outfile, tittel=tittel, width=600, height=600, til100=til100,
#                      decreasing=decreasing, terskel=terskel, minstekrav=NA, maal=NA, xtekst ='Antall pr. 1000 innbyggere')


############## Proteser ###########################################
######################################################################

Andeler <- Hofteproteser_rater
figurnavn <- 'Hofteproteser_rater_bo'
outfile <- paste0(hjemkatalog,'figurer/',figurnavn,'.pdf')
if (skrivSKDEdisk) {outfile <- paste0(figurkatalog,'PDF/',figurnavn,'.pdf')}
if (skrivSKDEdiskPNG) {outfile <- paste0(figurkatalog,'png/',figurnavn,'.png')}
tittel <- 'Rater hofteproteser etter boområde'
decreasing=F
terskel=30
til100 = F
terskel <- terskel_bo2
indikatorFigRaterGrVar(Andeler=Andeler, outfile=outfile, tittel=tittel, width=600, height=600, til100=til100,
                                   decreasing=decreasing, terskel=terskel, minstekrav=NA, maal=NA,
                              xtekst ='Antall pr. 1000 innbyggere')

Andeler <- Kneproteser_rater
figurnavn <- 'Kneproteser_rater_bo'
outfile <- paste0(hjemkatalog,'figurer/',figurnavn,'.pdf')
if (skrivSKDEdisk) {outfile <- paste0(figurkatalog,'PDF/',figurnavn,'.pdf')}
if (skrivSKDEdiskPNG) {outfile <- paste0(figurkatalog,'png/',figurnavn,'.png')}
tittel <- 'Rater kneproteser etter boområde'
decreasing=F
terskel=30
til100 = F
skriftStr=1.3
pktStr=1.5

indikatorFigRaterGrVar(Andeler=Andeler, outfile=outfile, tittel=tittel, width=600, height=600, til100=til100,
                       decreasing=decreasing, terskel=terskel, minstekrav=NA, maal=NA,
                       xtekst ='Antall pr. 1000 innbyggere')

## Produksjon

#Antall <- Hofteprotese_Produksjon_sh[, c(2,1,6)]
#names(Antall) <- c('bohf', 'aar', 'antall')
#figurnavn <- 'Hofteprotese_produksjon_sh'
#outfile <- paste0(hjemkatalog,'figurer/',figurnavn,'.pdf')
#if (skrivSKDEdisk) {outfile <- paste0(figurkatalog,'PDF/',figurnavn,'.pdf')}
#if (skrivSKDEdiskPNG) {outfile <- paste0(figurkatalog,'png/',figurnavn,'.png')}
#tittel <- c('Antall hofteproteser meldt til Leddproteseregisteret', 'i 2013-15, per sykehus')
#pktStr <- 0.8
#skriftStr <- 0.8
# terskel <- terskel_sh
#indikatorFigAntallGrVar(Antall=Antall, outfile=outfile, tittel=tittel, width=600, height=600,
#                        decreasing=F, xtekst ='Antall', sideTxt='Sykehus',
#                        skriftStr=skriftStr, pktStr=pktStr)

Antall <- Kneprotese_Produksjon_sh[, c(2,1,6)]
names(Antall) <- c('bohf', 'aar', 'antall')
figurnavn <- 'Kne_produksjon_sh'
outfile <- paste0(hjemkatalog,'figurer/',figurnavn,'.pdf')
if (skrivSKDEdisk) {outfile <- paste0(figurkatalog,'PDF/',figurnavn,'.pdf')}
if (skrivSKDEdiskPNG) {outfile <- paste0(figurkatalog,'png/',figurnavn,'.png')}
tittel <- c('Antall kneproteser meldt til Leddproteseregisteret', 'i 2013-15, per sykehus')
indikatorFigAntallGrVar(Antall=Antall, outfile=outfile, tittel=tittel, width=800, height=700,
                        decreasing=F, xtekst ='Antall', sideTxt='Sykehus',
                        skriftStr=skriftStr, pktStr=pktStr)



############ Hjerteinfarkt ############################################################
###################################################################################

## Angio

Antall <- Angio_bo[, -4]
names(Antall)[3] <- 'Antall'
tittel <- 'Andel med angio innen 72 timer, pr. boområde, justert'
figurnavn <- 'Angio_U72_bo_justert'
outfile <- paste0(hjemkatalog,'figurer/',figurnavn,'.pdf')
if (skrivSKDEdisk) {outfile <- paste0(figurkatalog,'PDF/',figurnavn,'.pdf')}
if (skrivSKDEdiskPNG) {outfile <- paste0(figurkatalog,'png/',figurnavn,'.png')}
terskel <- terskel_bo3
indikatorFigAndelGrVar_aldKjJust(Antall=Antall, outfile=outfile, tittel=tittel, width=600, height=600,
                                             decreasing=F, terskel=terskel, minstekrav = 50,
                                             maal = 80, til100=F, legPlass='top')

AntTilfeller <- tidyr::spread(Angio_sh_v2[,1:3], 'Aar', 'under72t')
rownames(AntTilfeller) <- AntTilfeller$Sykehus
AntTilfeller <- AntTilfeller[, -1]
N <- tidyr::spread(Angio_sh_v2[,c(1,2,6)], 'Aar', 'Nevner')
rownames(N) <- N$Sykehus
N <- N[, -1]
N[is.na(N)] <- 0
tittel <- 'Andel med angio innen 72 timer, pr. sykehus'
figurnavn <- 'Angio_U72_sh'
outfile <- paste0(hjemkatalog,'figurer/',figurnavn,'.pdf')
if (skrivSKDEdisk) {outfile <- paste0(figurkatalog,'PDF/',figurnavn,'.pdf')}
if (skrivSKDEdiskPNG) {outfile <- paste0(figurkatalog,'png/',figurnavn,'.png')}
terskel <- terskel_sh
indikatorFigAndelGrVar(AntTilfeller=AntTilfeller, N=N, outfile=outfile, tittel=tittel,
                       sideTxt='Behandlende sykehus', minstekrav = 50, maal=80,width=600, height=800,
                       decreasing=decreasing, terskel=terskel, skriftStr=1, pktStr=1.4, legPlass='top')

## Revaskularisering

Antall <- Revaskularisering_bo[, -4]
names(Antall)[3] <- 'Antall'
tittel <- 'Trombolyse/angiografi/PCI innen 30 min., pr. boområde, justert'
figurnavn <- 'Revaskularisering_bo_justert'
outfile <- paste0(hjemkatalog,'figurer/',figurnavn,'.pdf')
if (skrivSKDEdisk) {outfile <- paste0(figurkatalog,'PDF/',figurnavn,'.pdf')}
if (skrivSKDEdiskPNG) {outfile <- paste0(figurkatalog,'png/',figurnavn,'.png')}
terskel <- terskel_bo3
indikatorFigAndelGrVar_aldKjJust_1aar(Antall=Antall, outfile=outfile, tittel=tittel, width=600, height=600,
                                 decreasing=F, terskel=terskel, minstekrav = 50,
                                 maal = 80, til100=F)

Antall <- Revaskularisering_sh_v2[, -4]
names(Antall) <- c('aar', 'bohf', 'Antall', 'N')
tittel <- 'Trombolyse/angiografi/PCI innen 30 min., pr. sykehus'
figurnavn <- 'Revaskularisering_sh'
outfile <- paste0(hjemkatalog,'figurer/',figurnavn,'.pdf')
if (skrivSKDEdisk) {outfile <- paste0(figurkatalog,'PDF/',figurnavn,'.pdf')}
if (skrivSKDEdiskPNG) {outfile <- paste0(figurkatalog,'png/',figurnavn,'.png')}
terskel <- terskel_sh
indikatorFigAndelGrVar_1aar(Antall, outfile, tittel, width=600, height=800,
                            decreasing=F, terskel=terskel, minstekrav = 50, minstekravTxt = 'Moderat',
                            maal = 80, maalTxt='Høy', til100=F, skriftStr=1)

############ Hjerneslag ############################################################
###################################################################################

Antall <- Hjerneslag_behandlet_slagenhet_bo

names(Antall)[3] <- 'Antall'
tittel <- 'Andel behandlet i slagenhet, pr. boområde, justert'
figurnavn <- 'BehSlagenhet_bo_justert'
outfile <- paste0(hjemkatalog,'figurer/',figurnavn,'.pdf')
if (skrivSKDEdisk) {outfile <- paste0(figurkatalog,'PDF/',figurnavn,'.pdf')}
if (skrivSKDEdiskPNG) {outfile <- paste0(figurkatalog,'png/',figurnavn,'.png')}
terskel <- terskel_bo3
indikatorFigAndelGrVar_aldKjJust(Antall=Antall, outfile=outfile, tittel=tittel, width=600, height=600,
                                 decreasing=F, terskel=terskel, minstekrav = 80, minstekravTxt = 'Moderat',
                                 maal = 90, maalTxt='Høy', til100=FALSE)


AntTilfeller <- tidyr::spread(Hjerneslag_behandlet_slagenhet_sh_v2[,1:3], 'Aar', 'TellerC')
rownames(AntTilfeller) <- AntTilfeller$Helseenhet
AntTilfeller <- AntTilfeller[, -1]
N <- tidyr::spread(Hjerneslag_behandlet_slagenhet_sh_v2[,c(1,2,4)], 'Aar', 'NevnerC')
rownames(N) <- N$Helseenhet
N <- N[, -1]
N[is.na(N)] <- 0
tittel <- 'Andel behandlet i slagenhet, pr. sykehus'
figurnavn <- 'BehSlagenhet_sh'
outfile <- paste0(hjemkatalog,'figurer/',figurnavn,'.pdf')
if (skrivSKDEdisk) {outfile <- paste0(figurkatalog,'PDF/',figurnavn,'.pdf')}
if (skrivSKDEdiskPNG) {outfile <- paste0(figurkatalog,'png/',figurnavn,'.png')}
terskel <- terskel_sh
indikatorFigAndelGrVar(AntTilfeller=AntTilfeller, N=N, outfile=outfile, tittel=tittel, sideTxt='Behandlende sykehus',
                       minstekrav = 80, maal = 90, skriftStr=1, pktStr=1,
                       width=600, height=800, decreasing=decreasing, terskel=terskel,
                       minstekravTxt = 'Moderat', maalTxt='Høy')


Antall <- Hjerneslag_tromsbolyse_bo
Antall$TellerE[is.na(Antall$TellerE)] <- 0

names(Antall)[3] <- 'Antall'
tittel <- 'Andel med trombolyse innen 40 min., pr. boområde, justert'
figurnavn <- 'Trombolyse_bo_justert'
outfile <- paste0(hjemkatalog,'figurer/',figurnavn,'.pdf')
if (skrivSKDEdisk) {outfile <- paste0(figurkatalog,'PDF/',figurnavn,'.pdf')}
if (skrivSKDEdiskPNG) {outfile <- paste0(figurkatalog,'png/',figurnavn,'.png')}
terskel <- terskel_bo3
indikatorFigAndelGrVar_aldKjJust(Antall=Antall, outfile=outfile, tittel=tittel, width=600, height=600,
                                 decreasing=F, terskel=terskel, minstekrav = 30, minstekravTxt = 'Moderat',
                                 maal = 50, maalTxt='Høy', til100=FALSE, legPlass='top') #


AntTilfeller <- tidyr::spread(Hjerneslag_tromsbolyse_sh_v2[,1:3], 'Aar', 'TellerE')
rownames(AntTilfeller) <- AntTilfeller$Helseenhet
AntTilfeller <- AntTilfeller[, -1]
N <- tidyr::spread(Hjerneslag_tromsbolyse_sh_v2[,c(1,2,4)], 'Aar', 'NevnerE')
rownames(N) <- N$Helseenhet
N <- N[, -1]
N[is.na(N)] <- 0
tittel <- 'Andel med trombolyse innen 40 min., pr. sykehus'
figurnavn <- 'Trombolyse_sh'
outfile <- paste0(hjemkatalog,'figurer/',figurnavn,'.pdf')
if (skrivSKDEdisk) {outfile <- paste0(figurkatalog,'PDF/',figurnavn,'.pdf')}
if (skrivSKDEdiskPNG) {outfile <- paste0(figurkatalog,'png/',figurnavn,'.png')}
terskel <- terskel_sh
indikatorFigAndelGrVar(AntTilfeller=AntTilfeller, N=N, outfile=outfile, tittel=tittel, sideTxt='Behandlende sykehus',
                       width=600, height=800, decreasing=F, terskel=terskel,
                       skriftStr=1, pktStr=1, minstekrav = 30, minstekravTxt = 'Moderat',
                       maal = 50, maalTxt='Høy', legPlass='top')


################# Prolapsrater ############################################
#############################################################

Andeler <- Prolapskirurgi_rater
figurnavn <- 'Prolapskirurgi_rater'
outfile <- paste0(hjemkatalog,'figurer/',figurnavn,'.pdf')
if (skrivSKDEdisk) {outfile <- paste0(figurkatalog,'PDF/',figurnavn,'.pdf')}
if (skrivSKDEdiskPNG) {outfile <- paste0(figurkatalog,'png/',figurnavn,'.png')}
tittel <- 'Rater prolapskirurgi etter boområde'
terskel <- terskel_bo3
indikatorFigRaterGrVar(Andeler=Andeler, outfile=outfile, tittel=tittel, width=600, height=600, til100=til100,
                       decreasing=T, terskel=terskel, minstekrav=NA, maal=NA,
                       xtekst ='Antall pr. 1000 innbyggere')



 Andeler <- Hoftebrudd_Preoperativ_liggetid_hf[ , -c(3,4,7)]
 Andeler <- Andeler[, c(1,2,4,3)]
 names(Andeler)[names(Andeler)=='andel24'] <- 'andel'
 names(Andeler)[names(Andeler)=='Antall'] <- 'antall'
 names(Andeler)[names(Andeler)=='behhf'] <- 'bohf'
 outfile <- 'C:/GIT/indikatoR/doc/figurer/HofteOp24_hf.pdf'
 if (skrivSKDEdisk) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PDF/HofteOp24_hf.pdf'}
 if (skrivSKDEdiskPNG) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PNG/HofteOp24_hf.png'}
 tittel <- 'Andel operert innen 24 timer, pr. helseforetak'
 skriftStr <- 1.0
 pktStr <- 1.0

# indikatorFigAndelGrVar_preberegnet(Andeler=Andeler, outfile=outfile, tittel=tittel, width=width, height=height,
#                                    decreasing=decreasing, terskel=terskel, minstekrav=minstekrav, maal=maal,
#                                    skriftStr=skriftStr, pktStr=pktStr, sideTxt='Helseforetak')

 Andeler <- Hoftebrudd_Preoperativ_liggetid_hf[ , -c(3,4,6)]
 Andeler <- Andeler[, c(1,2,4,3)]
 names(Andeler)[names(Andeler)=='andel48'] <- 'andel'
 names(Andeler)[names(Andeler)=='Antall'] <- 'antall'
 names(Andeler)[names(Andeler)=='behhf'] <- 'bohf'
 outfile <- 'C:/GIT/indikatoR/doc/figurer/HofteOp48_hf.pdf'
 if (skrivSKDEdisk) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PDF/HofteOp48_hf.pdf'}
 if (skrivSKDEdiskPNG) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PNG/HofteOp48_hf.png'}
 tittel <- 'Andel operert innen 48 timer, pr. helseforetak'

# indikatorFigAndelGrVar_preberegnet(Andeler=Andeler, outfile=outfile, tittel=tittel, width=width, height=height,
#                                    decreasing=decreasing, terskel=terskel, minstekrav=minstekrav, maal=maal,
#                                    skriftStr=1, pktStr=1.4, sideTxt='Helseforetak')

# Antall <- Hoftebrudd_Produksjon_bo[ , -c(1,4,5,6)]
# names(Antall)[names(Antall)=='n'] <- 'antall'
# names(Antall)[names(Antall)=='Boomraade'] <- 'bohf'
# outfile <- 'C:/GIT/indikatoR/doc/figurer/Hoftebrudd_produksjon_bo.pdf'
# if (skrivSKDEdisk) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PDF/Hoftebrudd_produksjon_bo.pdf'}
# if (skrivSKDEdiskPNG) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PNG/Hoftebrudd_produksjon_bo.png'}
# tittel <- c('Antall hoftebrudd meldt til Nasjonalt Hoftebruddregister', 'i 2013-15, per boområde')
#
# indikatorFigAntallGrVar(Antall=Antall, outfile=outfile, tittel=tittel, width=600, height=600,
#                         decreasing=F, xtekst ='Antall',
#                         skriftStr=1, pktStr=1.3)

# Andeler <- Hoftebrudd_Produksjon_hf
# Andeler <- Andeler[Andeler$aar==2015, ]
# Andeler <- Andeler[, -2]
# rownames(Andeler) <- Andeler$Behandlende.sykehus
# Andeler <- Andeler[, -1]
# names(Andeler)[names(Andeler)=='Totalt.antall'] <- 'N'
# names(Andeler)[names(Andeler)=='Pinning'] <- 'To skruer eller pinner'
# Andeler[,1:3] <- Andeler[,1:3]/rowSums(Andeler[,1:3])
# Andeler <- Andeler[Andeler$N > 0, ]
# tittel <- 'Operasjonsmetoder ved lårhalsbrudd 2015, HF'
# outfile <- 'C:/GIT/indikatoR/doc/figurer/Hofte_operasjonsmetode_hf.pdf'
# if (skrivSKDEdisk) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PDF/Hofte_operasjonsmetode_hf.pdf'}
# if (skrivSKDEdiskPNG) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PNG/Hofte_operasjonsmetode_hf.png'}
#
# indikatorFigAndelStabelGrVar(Andeler=Andeler, outfile=outfile, tittel=tittel, skriftStr=1, sideTxt = 'Helseforetak')
#

# AntTilfeller <- tidyr::spread(Hjerneslag_behandlet_slagenhet_hf[,1:3], 'Aar', 'TellerC')
# rownames(AntTilfeller) <- AntTilfeller$helseenhet
# AntTilfeller <- AntTilfeller[, -1]
# N <- tidyr::spread(Hjerneslag_behandlet_slagenhet_hf[,c(1,2,4)], 'Aar', 'NevnerC')
# rownames(N) <- N$helseenhet
# N <- N[, -1]
# N[is.na(N)] <- 0
# tittel <- 'Andel behandlet i slagenhet, pr. helseforetak'
# outfile <- 'C:/GIT/indikatoR/doc/figurer/BehSlagenhet_hf.pdf'
# if (skrivSKDEdisk) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PDF/BehSlagenhet_hf.pdf'}
# if (skrivSKDEdiskPNG) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PNG/BehSlagenhet_hf.png'}
#
# indikatorFigAndelGrVar(AntTilfeller=AntTilfeller, N=N, outfile=outfile, tittel=tittel, sideTxt='Helseforetak', minstekrav = 80, maal = 90,
#                        width=width, height=height, decreasing=decreasing, terskel=terskel, skriftStr=0.8, pktStr=1,
#                        minstekravTxt = 'Moderat', maalTxt='Høy')
# AntTilfeller <- tidyr::spread(Hjerneslag_tromsbolyse_hf[,1:3], 'Aar', 'TellerE')
# rownames(AntTilfeller) <- AntTilfeller$helseenhet
# AntTilfeller <- AntTilfeller[, -1]
# N <- tidyr::spread(Hjerneslag_tromsbolyse_hf[,c(1,2,4)], 'Aar', 'NevnerE')
# rownames(N) <- N$helseenhet
# N <- N[, -1]
# N[is.na(N)] <- 0
# tittel <- 'Andel med trombolyse innen 40 min., pr. helseforetak'
# outfile <- 'C:/GIT/indikatoR/doc/figurer/Trombolyse_hf.pdf'
# if (skrivSKDEdisk) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PDF/Trombolyse_hf.pdf'}
# if (skrivSKDEdiskPNG) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PNG/Trombolyse_hf.png'}
#
# indikatorFigAndelGrVar(AntTilfeller=AntTilfeller, N=N, outfile=outfile, tittel=tittel, sideTxt='Helseforetak',
#                        width=width, height=height, decreasing=decreasing, terskel=terskel, skriftStr=0.8, pktStr=1,
#                        minstekrav = 30, minstekravTxt = 'Moderat', maal = 50, maalTxt='Høy', legPlass='nede')

