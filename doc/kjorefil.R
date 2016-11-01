setwd('C:/GIT/indikatoR/doc/')
rm(list = ls())
library(indikatoR)

skrivSKDEdiskPNG <- T

terskel_sh <- 10
terskel_hf <- 10
terskel_bo1 <- 15
terskel_bo2 <- 40
terskel_bo3 <- 30


############  Brystkreft - brystbevarende  ###################################################

outfile='C:/GIT/indikatoR/doc/figurer/Brystbevarende_sh.pdf'
if (skrivSKDEdisk) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PDF/Brystbevarende_sh.pdf'}
if (skrivSKDEdiskPNG) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PNG/Brystbevarende_sh.png'}
tittel=c('Andel med brystbevarende kirurgi for tumorstørrelse 0-30 mm', 'pr. sykehus')
width=800
height=700
AntTilfeller=Brystbevarende_sh$AntTilfeller
N=Brystbevarende_sh$AntTotalt
decreasing=F
terskel=terskel_sh

indikatorFigAndelGrVar(AntTilfeller=AntTilfeller, N=N, outfile=outfile, tittel=tittel, sideTxt='Sykehus',
                       width=width, height=height, decreasing=decreasing, terskel=terskel, minstekrav = 70, maal = 80, legPlass='nede')

### Aldersjustering
AntTilfeller=Brystbevarende_bo_aldersgr$AntTilfeller
N=Brystbevarende_bo_aldersgr$AntTotalt
terskel=terskel_bo1
outfile='C:/GIT/indikatoR/doc/figurer/Brystbevarende_bo_justert.pdf'
if (skrivSKDEdisk) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PDF/Brystbevarende_bo_justert.pdf'}
if (skrivSKDEdiskPNG) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PNG/Brystbevarende_bo_justert.png'}
tittel=c('Andel med brystbevarende kirurgi for tumorstørrelse 0-30 mm', 'pr. boområde, aldersjustert')

indikatorFigAndelGrVar_justert(AntTilfeller=AntTilfeller, N=N, outfile=outfile, tittel=tittel,
                               width=width, height=height, decreasing=decreasing, terskel=terskel,
                               minstekrav = 70, maal = 80)


###############################################################################
##########  Brystkreft Ki 67  #######################################################

# load("C:/GIT/indikatoR/data/BrystKi672015_sh.RData")

Antall <- BrystKi67sh2015
tittel='Ki67 proliferasjonsrate, pr. sykehus'
inkl_ukjent=F
width=800
height=700
outfile='C:/GIT/indikatoR/doc/figurer/TestresultaterKi67_sh.pdf'
if (skrivSKDEdisk) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PDF/TestresultaterKi67_sh.pdf'}
if (skrivSKDEdiskPNG) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PNG/Brystbevarende_bo_justert.png'}
terskel <- terskel_sh
indikatorFigAndelStabelGrVar_mUkjent(Antall=Antall, outfile, tittel=tittel, sideTxt='Sykehus',
                                     inkl_ukjent=inkl_ukjent, width=width, height=height, terskel=terskel)

tittel='Ki67 proliferasjonsrate, pr. boområde, justert'
width=800
height=700
outfile <- 'TestresultaterKi67_bo_justert.png'
if (skrivSKDEdisk) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PDF/TestresultaterKi67_bo_justert.png'}
if (skrivSKDEdiskPNG) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PNG/Brystbevarende_bo_justert.png'}
Antall <- BrystKi67Bo2015_aldersgr
terskel <- terskel_bo1
indikatorFigAndelStabelGrVar_justert(Antall=Antall, outfile=outfile, tittel=tittel,
                                     width=800, height=700, terskel=terskel)

###############################################################################
##########  Brystkreft histologi  #######################################################

Antall <- BrystkreftHistologi_sh
tittel='Histologisk Grad (Nottingham), pr. sykehus'
outfile='C:/GIT/indikatoR/doc/figurer/HistologiskGrad(Nottingham)_sh.pdf'
if (skrivSKDEdisk) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PDF/HistologiskGrad(Nottingham)_sh.pdf'}
if (skrivSKDEdiskPNG) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PNG/Brystbevarende_bo_justert.png'}
terskel <- terskel_sh
indikatorFigAndelStabelGrVar_mUkjent(Antall=Antall, outfile, tittel=tittel, sideTxt='Sykehus', inkl_ukjent=F, terskel=terskel)


tittel='Histologisk Grad (Nottingham), pr. boområde, justert'
outfile <- 'C:/GIT/indikatoR/doc/figurer/HistologiskGrad(Nottingham)_bo_justert.pdf'
if (skrivSKDEdisk) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PDF/HistologiskGrad(Nottingham)_bo_justert.pdf'}
if (skrivSKDEdiskPNG) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PNG/Brystbevarende_bo_justert.png'}
Antall <- BrystkreftHistologi_bo2015_aldersgr
terskel <- terskel_bo1
indikatorFigAndelStabelGrVar_justert(Antall=Antall, outfile=outfile, tittel=tittel,
                                     width=800, height=700, terskel=terskel)




################################################################################
################### The hips don't lie ###################################

width=800
height=700
decreasing=F
minstekrav = NA
maal = NA
Andeler <- Hoftebrudd_Preoperativ_liggetid_bo_justert[, -c(3,5,7,8)]
names(Andeler)[names(Andeler)=='andel24'] <- 'andel'
names(Andeler)[names(Andeler)=='bohf_txt'] <- 'bohf'
outfile <- 'C:/GIT/indikatoR/doc/figurer/HofteOp24_bo.pdf'
if (skrivSKDEdisk) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PDF/HofteOp24_bo.pdf'}
if (skrivSKDEdiskPNG) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PNG/Brystbevarende_bo_justert.png'}
tittel <- 'Andel operert innen 24 timer, pr. boområde, justert'

terskel=terskel_bo2
indikatorFigAndelGrVar_preberegnet(Andeler=Andeler, outfile=outfile, tittel=tittel, width=width, height=height, til100=F,
                                               decreasing=decreasing, terskel=terskel, minstekrav=minstekrav, maal=maal)


Andeler <- Hoftebrudd_Preoperativ_liggetid_bo_justert[, -c(3,4,7,8)]
names(Andeler)[names(Andeler)=='andel48'] <- 'andel'
names(Andeler)[names(Andeler)=='bohf_txt'] <- 'bohf'
outfile <- 'C:/GIT/indikatoR/doc/figurer/HofteOp48_bo.pdf'
if (skrivSKDEdisk) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PDF/HofteOp48_bo.pdf'}
if (skrivSKDEdiskPNG) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PNG/Brystbevarende_bo_justert.png'}
tittel <- 'Andel operert innen 48 timer, pr. boområde, justert'

indikatorFigAndelGrVar_preberegnet(Andeler=Andeler, outfile=outfile, tittel=tittel, width=width, height=height,
                                   decreasing=decreasing, terskel=terskel, minstekrav=minstekrav, maal=maal)


# load("C:/GIT/indikatoR/data/Hoftebrudd_Preoperativ_liggetid_sh.RData")

Andeler <- Hoftebrudd_Preoperativ_liggetid_sh_v2[ , -c(3,4,7)]
Andeler <- Andeler[, c(1,2,4,3)]
names(Andeler)[names(Andeler)=='andel24'] <- 'andel'
names(Andeler)[names(Andeler)=='Antall'] <- 'antall'
names(Andeler)[names(Andeler)=='behsh'] <- 'bohf'
outfile <- 'C:/GIT/indikatoR/doc/figurer/HofteOp24_sh.pdf'
if (skrivSKDEdisk) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PDF/HofteOp24_sh.pdf'}
if (skrivSKDEdiskPNG) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PNG/Brystbevarende_bo_justert.png'}
tittel <- 'Andel operert innen 24 timer, pr. sykehus'
skriftStr <- 1.0
pktStr <- 1.0
terskel <- terskel_sh
indikatorFigAndelGrVar_preberegnet(Andeler=Andeler, outfile=outfile, tittel=tittel, width=width, height=height,
                                   decreasing=decreasing, terskel=terskel, minstekrav=minstekrav, maal=maal,
                                   skriftStr=skriftStr, pktStr=pktStr, sideTxt='Sykehus')

Andeler <- Hoftebrudd_Preoperativ_liggetid_sh_v2[ , -c(3,4,6)]
Andeler <- Andeler[, c(1,2,4,3)]
names(Andeler)[names(Andeler)=='andel48'] <- 'andel'
names(Andeler)[names(Andeler)=='Antall'] <- 'antall'
names(Andeler)[names(Andeler)=='behsh'] <- 'bohf'
outfile <- 'C:/GIT/indikatoR/doc/figurer/HofteOp48_sh.pdf'
if (skrivSKDEdisk) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PDF/HofteOp48_sh.pdf'}
if (skrivSKDEdiskPNG) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PNG/Brystbevarende_bo_justert.png'}
tittel <- 'Andel operert innen 48 timer, pr. sykehus'
skriftStr <- 1.0
pktStr <- 1.0

indikatorFigAndelGrVar_preberegnet(Andeler=Andeler, outfile=outfile, tittel=tittel, width=width, height=height,
                                   decreasing=decreasing, terskel=terskel, minstekrav=minstekrav, maal=maal,
                                   skriftStr=skriftStr, pktStr=pktStr, sideTxt='Sykehus')


Andeler <- Hoftebrudd_Preoperativ_liggetid_hf[ , -c(3,4,7)]
Andeler <- Andeler[, c(1,2,4,3)]
names(Andeler)[names(Andeler)=='andel24'] <- 'andel'
names(Andeler)[names(Andeler)=='Antall'] <- 'antall'
names(Andeler)[names(Andeler)=='behhf'] <- 'bohf'
outfile <- 'C:/GIT/indikatoR/doc/figurer/HofteOp24_hf.pdf'
if (skrivSKDEdisk) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PDF/HofteOp24_hf.pdf'}
if (skrivSKDEdiskPNG) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PNG/Brystbevarende_bo_justert.png'}
tittel <- 'Andel operert innen 24 timer, pr. helseforetak'
skriftStr <- 1.0
pktStr <- 1.0

indikatorFigAndelGrVar_preberegnet(Andeler=Andeler, outfile=outfile, tittel=tittel, width=width, height=height,
                                   decreasing=decreasing, terskel=terskel, minstekrav=minstekrav, maal=maal,
                                   skriftStr=skriftStr, pktStr=pktStr, sideTxt='Helseforetak')

Andeler <- Hoftebrudd_Preoperativ_liggetid_hf[ , -c(3,4,6)]
Andeler <- Andeler[, c(1,2,4,3)]
names(Andeler)[names(Andeler)=='andel48'] <- 'andel'
names(Andeler)[names(Andeler)=='Antall'] <- 'antall'
names(Andeler)[names(Andeler)=='behhf'] <- 'bohf'
outfile <- 'C:/GIT/indikatoR/doc/figurer/HofteOp48_hf.pdf'
if (skrivSKDEdisk) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PDF/HofteOp48_hf.pdf'}
if (skrivSKDEdiskPNG) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PNG/Brystbevarende_bo_justert.png'}
tittel <- 'Andel operert innen 48 timer, pr. helseforetak'
skriftStr <- 1.0
pktStr <- 1.0

indikatorFigAndelGrVar_preberegnet(Andeler=Andeler, outfile=outfile, tittel=tittel, width=width, height=height,
                                   decreasing=decreasing, terskel=terskel, minstekrav=minstekrav, maal=maal,
                                   skriftStr=skriftStr, pktStr=pktStr, sideTxt='Helseforetak')

###### Produksjon

# Antall <- Hoftebrudd_Produksjon_sh[ , -c(3,4,5)]
Antall <- Hoftebrudd_Produksjon_sh_v2[ , -c(3,4,5)]
names(Antall)[names(Antall)=='Totalt.antall'] <- 'antall'
names(Antall)[names(Antall)=='Behandlende.sykehus'] <- 'bohf'
outfile <- 'C:/GIT/indikatoR/doc/figurer/Hoftebrudd_produksjon_sh.pdf'
if (skrivSKDEdisk) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PDF/Hoftebrudd_produksjon_sh.pdf'}
if (skrivSKDEdiskPNG) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PNG/Brystbevarende_bo_justert.png'}
tittel <- c('Antall hoftebrudd meldt til Nasjonalt Hoftebruddregister', 'i 2013-15, per sykehus')
pktStr <- 1
skriftStr <- 1

indikatorFigAntallGrVar(Antall=Antall, outfile=outfile, tittel=tittel, width=800, height=700,
                                    decreasing=F, xtekst ='Antall', sideTxt='Sykehus',
                                    skriftStr=skriftStr, pktStr=pktStr)


# load("C:/GIT/indikatoR/data/Hoftebrudd_Produksjon_bo.RData")
Antall <- Hoftebrudd_Produksjon_bo[ , -c(1,4,5,6)]
names(Antall)[names(Antall)=='n'] <- 'antall'
names(Antall)[names(Antall)=='Boomraade'] <- 'bohf'
outfile <- 'C:/GIT/indikatoR/doc/figurer/Hoftebrudd_produksjon_bo.pdf'
if (skrivSKDEdisk) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PDF/Hoftebrudd_produksjon_bo.pdf'}
if (skrivSKDEdiskPNG) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PNG/Brystbevarende_bo_justert.png'}
tittel <- c('Antall hoftebrudd meldt til Nasjonalt Hoftebruddregister', 'i 2013-15, per boområde')
pktStr <- 1.3
skriftStr <- 1.3

indikatorFigAntallGrVar(Antall=Antall, outfile=outfile, tittel=tittel, width=800, height=700,
                        decreasing=F, xtekst ='Antall',
                        skriftStr=skriftStr, pktStr=pktStr)

##### Type behandling

Andeler <- Hoftebrudd_andeler_2015_bo_justert
tittel <- 'Operasjonsmetoder ved lårhalsbrudd 2015, boområde, justert'
outfile <- 'C:/GIT/indikatoR/doc/figurer/Hofte_operasjonsmetode_bo_justert.pdf'
if (skrivSKDEdisk) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PDF/Hofte_operasjonsmetode_bo_justert.pdf'}
if (skrivSKDEdiskPNG) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PNG/Brystbevarende_bo_justert.png'}
terskel <- terskel_bo2
indikatorFigAndelStabelGrVar(Andeler=Andeler, outfile=outfile, tittel=tittel, sideTxt = 'Boområde/opptaksområde', terskel = terskel)


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
tittel <- 'Operasjonsmetoder ved lårhalsbrudd 2015, sykehus'
outfile <- 'C:/GIT/indikatoR/doc/figurer/Hofte_operasjonsmetode_sh.pdf'
if (skrivSKDEdisk) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PDF/Hofte_operasjonsmetode_sh.pdf'}
if (skrivSKDEdiskPNG) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PNG/Brystbevarende_bo_justert.png'}
terskel <- terskel_sh
indikatorFigAndelStabelGrVar(Andeler=Andeler, outfile=outfile, tittel=tittel, skriftStr=skriftStr, sideTxt = 'Sykehus')


Andeler <- Hoftebrudd_Produksjon_hf
Andeler <- Andeler[Andeler$aar==2015, ]
Andeler <- Andeler[, -2]
rownames(Andeler) <- Andeler$Behandlende.sykehus
Andeler <- Andeler[, -1]
names(Andeler)[names(Andeler)=='Totalt.antall'] <- 'N'
names(Andeler)[names(Andeler)=='Pinning'] <- 'To skruer eller pinner'
Andeler[,1:3] <- Andeler[,1:3]/rowSums(Andeler[,1:3])
Andeler <- Andeler[Andeler$N > 0, ]
skriftStr <- 1
tittel <- 'Operasjonsmetoder ved lårhalsbrudd 2015, HF'
outfile <- 'C:/GIT/indikatoR/doc/figurer/Hofte_operasjonsmetode_hf.pdf'
if (skrivSKDEdisk) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PDF/Hofte_operasjonsmetode_hf.pdf'}
if (skrivSKDEdiskPNG) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PNG/Brystbevarende_bo_justert.png'}

indikatorFigAndelStabelGrVar(Andeler=Andeler, outfile=outfile, tittel=tittel, skriftStr=skriftStr, sideTxt = 'Helseforetak')


Andeler <- Hoftebrudd_rater
outfile <- 'C:/GIT/indikatoR/doc/figurer/Hoftebrudd_rater_bo.pdf'
if (skrivSKDEdisk) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PDF/Hoftebrudd_rater_bo.pdf'}
if (skrivSKDEdiskPNG) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PNG/Brystbevarende_bo_justert.png'}
tittel <- 'Rater hoftebrudd etter boområde'
width=800
height=700
decreasing=F
terskel=30
minstekrav = NA
maal = NA
til100 = F
skriftStr=1.3
pktStr=1.5
terskel <- terskel_bo2
indikatorFigRaterGrVar(Andeler=Andeler, outfile=outfile, tittel=tittel, width=width, height=height, til100=til100,
                       decreasing=decreasing, terskel=terskel, minstekrav=minstekrav, maal=maal, xtekst ='Rater pr. 1000 innbyggere')


############## Proteser ###########################################
######################################################################

Andeler <- Hofteproteser_rater
outfile <- 'C:/GIT/indikatoR/doc/figurer/Hofteproteser_rater_bo.pdf'
if (skrivSKDEdisk) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PDF/Hofteproteser_rater_bo.pdf'}
if (skrivSKDEdiskPNG) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PNG/Brystbevarende_bo_justert.png'}
tittel <- 'Rater hofteproteser etter boområde'
width=800
height=700
decreasing=F
terskel=30
minstekrav = NA
maal = NA
til100 = F
skriftStr=1.3
pktStr=1.5
terskel <- terskel_bo2
indikatorFigRaterGrVar(Andeler=Andeler, outfile=outfile, tittel=tittel, width=width, height=height, til100=til100,
                                   decreasing=decreasing, terskel=terskel, minstekrav=minstekrav, maal=maal, xtekst ='Rater pr. 1000 innbyggere')

Andeler <- Kneproteser_rater
outfile <- 'C:/GIT/indikatoR/doc/figurer/Kneproteser_rater_bo.pdf'
if (skrivSKDEdisk) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PDF/Kneproteser_rater_bo.pdf'}
if (skrivSKDEdiskPNG) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PNG/Brystbevarende_bo_justert.png'}
tittel <- 'Rater kneproteser etter boområde'
width=800
height=700
decreasing=F
terskel=30
minstekrav = NA
maal = NA
til100 = F
skriftStr=1.3
pktStr=1.5

indikatorFigRaterGrVar(Andeler=Andeler, outfile=outfile, tittel=tittel, width=width, height=height, til100=til100,
                       decreasing=decreasing, terskel=terskel, minstekrav=minstekrav, maal=maal, xtekst ='Rater pr. 1000 innbyggere')

## Produksjon

Antall <- Hofteprotese_Produksjon_sh[, c(2,1,6)]
names(Antall) <- c('bohf', 'aar', 'antall')
outfile <- 'C:/GIT/indikatoR/doc/figurer/Hofteprotese_produksjon_sh.pdf'
if (skrivSKDEdisk) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PDF/Hofteprotese_produksjon_sh.pdf'}
if (skrivSKDEdiskPNG) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PNG/Brystbevarende_bo_justert.png'}
tittel <- c('Antall hofteproteser meldt til Leddproteseregisteret', 'i 2013-15, per sykehus')
pktStr <- 0.8
skriftStr <- 0.8
# terskel <- terskel_sh
indikatorFigAntallGrVar(Antall=Antall, outfile=outfile, tittel=tittel, width=800, height=700,
                        decreasing=F, xtekst ='Antall', sideTxt='Sykehus',
                        skriftStr=skriftStr, pktStr=pktStr)

Antall <- Kneprotese_Produksjon_sh[, c(2,1,6)]
names(Antall) <- c('bohf', 'aar', 'antall')
outfile <- 'C:/GIT/indikatoR/doc/figurer/Kne_produksjon_sh.pdf'
if (skrivSKDEdisk) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PDF/Kne_produksjon_sh.pdf'}
if (skrivSKDEdiskPNG) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PNG/Brystbevarende_bo_justert.png'}
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
outfile <- 'C:/GIT/indikatoR/doc/figurer/Angio_U72_bo_justert.pdf'
if (skrivSKDEdisk) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PDF/Angio_U72_bo_justert.pdf'}
if (skrivSKDEdiskPNG) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PNG/Brystbevarende_bo_justert.png'}
terskel <- terskel_bo3
indikatorFigAndelGrVar_aldKjJust(Antall=Antall, outfile=outfile, tittel=tittel, width=800, height=700,
                                             decreasing=F, terskel=terskel, minstekrav = 50,
                                             maal = 80, til100=T, legPlass='nede')

# Antall <- Angio_bo
# Antall$Antall <- rowSums(Antall[,3:4])
# Antall <- Antall[, -c(3,4)]
# Antall <- Antall[, c(1,2,5,3,4)]
# tittel <- 'Andel med angio, pr. boområde, justert'
# outfile <- 'C:/GIT/indikatoR/doc/figurer/Angio_totalt_bo_justert.pdf'
# if (skrivSKDEdisk) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PDF/Angio_totalt_bo_justert.pdf'}
#
# indikatorFigAndelGrVar_aldKjJust(Antall=Antall, outfile=outfile, tittel=tittel, width=800, height=700,
#                                  decreasing=F, terskel=terskel, minstekrav = NA,
#                                  maal = NA, til100=F)

AntTilfeller <- tidyr::spread(Angio_sh_v2[,1:3], 'Aar', 'under72t')
rownames(AntTilfeller) <- AntTilfeller$Sykehus
AntTilfeller <- AntTilfeller[, -1]
N <- tidyr::spread(Angio_sh_v2[,c(1,2,6)], 'Aar', 'Nevner')
rownames(N) <- N$Sykehus
N <- N[, -1]
N[is.na(N)] <- 0
tittel <- 'Andel med angio innen 72 timer, pr. sykehus'
outfile <- 'C:/GIT/indikatoR/doc/figurer/Angio_U72_sh.pdf'
if (skrivSKDEdisk) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PDF/Angio_U72_sh.pdf'}
if (skrivSKDEdiskPNG) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PNG/Brystbevarende_bo_justert.png'}
terskel <- terskel_sh
indikatorFigAndelGrVar(AntTilfeller=AntTilfeller, N=N, outfile=outfile, tittel=tittel, sideTxt='Sykehus', minstekrav = 50, maal=80,
                       width=width, height=height, decreasing=decreasing, terskel=terskel, skriftStr=0.8, pktStr=1, legPlass='nede')

# AntTilfeller <- Angio_sh_v2
# AntTilfeller$AntTot <- rowSums(AntTilfeller[, c('under72t', 'over72t')])
# AntTilfeller <- tidyr::spread(AntTilfeller[, c('Aar', 'Sykehus', 'AntTot')], 'Aar', 'AntTot')
# rownames(AntTilfeller) <- AntTilfeller$Sykehus
# AntTilfeller <- AntTilfeller[, -1]
# tittel <- 'Andel med angio, pr. sykehus'
# outfile <- 'C:/GIT/indikatoR/doc/figurer/Angio_totalt_sh.pdf'
# if (skrivSKDEdisk) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PDF/Angio_totalt_sh.pdf'}
#
# indikatorFigAndelGrVar(AntTilfeller=AntTilfeller, N=N, outfile=outfile, tittel=tittel, sideTxt='Sykehus',
#                        width=width, height=height, decreasing=decreasing, terskel=terskel, skriftStr=0.8, pktStr=1)


# AntTilfeller <- tidyr::spread(Angio_hf[,1:3], 'Aar', 'under72t')
# rownames(AntTilfeller) <- AntTilfeller$sykehus
# AntTilfeller <- AntTilfeller[, -1]
# N <- tidyr::spread(Angio_hf[,c(1,2,6)], 'Aar', 'Nevner')
# rownames(N) <- N$sykehus
# N <- N[, -1]
# N[is.na(N)] <- 0
# tittel <- 'Andel med angio innen 72 timer, pr. helseforetak'
# outfile <- 'C:/GIT/indikatoR/doc/figurer/Angio_U72_hf.pdf'
# if (skrivSKDEdisk) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PDF/Angio_U72_hf.pdf'}
#
# indikatorFigAndelGrVar(AntTilfeller=AntTilfeller, N=N, outfile=outfile, tittel=tittel, sideTxt='Helseforetak',
#                        width=width, height=height, decreasing=decreasing, terskel=terskel, skriftStr=0.8, pktStr=1)

# AntTilfeller <- Angio_hf
# AntTilfeller$AntTot <- rowSums(AntTilfeller[, c('under72t', 'over72t')])
# AntTilfeller <- tidyr::spread(AntTilfeller[, c('Aar', 'sykehus', 'AntTot')], 'Aar', 'AntTot')
# rownames(AntTilfeller) <- AntTilfeller$sykehus
# AntTilfeller <- AntTilfeller[, -1]
# tittel <- 'Andel med angio, pr. helseforetak'
# outfile <- 'C:/GIT/indikatoR/doc/figurer/Angio_totalt_hf.pdf'
# if (skrivSKDEdisk) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PDF/Angio_totalt_hf.pdf'}
#
# indikatorFigAndelGrVar(AntTilfeller=AntTilfeller, N=N, outfile=outfile, tittel=tittel, sideTxt='Helseforetak',
#                        width=width, height=height, decreasing=decreasing, terskel=terskel, skriftStr=0.8, pktStr=1)

## Revaskularisering

Antall <- Revaskularisering_bo[, -4]
names(Antall)[3] <- 'Antall'
tittel <- 'Revaskularisering under 30 min., pr. boområde, justert'
outfile <- 'C:/GIT/indikatoR/doc/figurer/Revaskularisering_bo_justert.pdf'
if (skrivSKDEdisk) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PDF/Revaskularisering_bo_justert.pdf'}
if (skrivSKDEdiskPNG) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PNG/Brystbevarende_bo_justert.png'}
terskel <- terskel_bo3
indikatorFigAndelGrVar_aldKjJust_1aar(Antall=Antall, outfile=outfile, tittel=tittel, width=800, height=700,
                                 decreasing=F, terskel=terskel, minstekrav = 50,
                                 maal = 80, til100=T)

Antall <- Revaskularisering_sh_v2[, -4]
names(Antall) <- c('aar', 'bohf', 'Antall', 'N')
tittel <- 'Revaskularisering under 30 min., pr. sykehus'
outfile <- 'C:/GIT/indikatoR/doc/figurer/Revaskularisering_sh.pdf'
if (skrivSKDEdisk) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PDF/Revaskularisering_sh.pdf'}
if (skrivSKDEdiskPNG) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PNG/Brystbevarende_bo_justert.png'}
terskel <- terskel_sh
indikatorFigAndelGrVar_1aar(Antall, outfile, tittel, width=600, height=700,
                            decreasing=F, terskel=terskel, minstekrav = 50, minstekravTxt = 'Moderat=',
                            maal = 80, maalTxt='Høy=', til100=T, skriftStr=0.8)

# Antall <- Revaskularisering_hf[, -4]
# names(Antall) <- c('aar', 'bohf', 'Antall', 'N')
# tittel <- 'Revaskularisering under 30 min., pr. helseforetak'
# outfile <- 'C:/GIT/indikatoR/doc/figurer/Revaskularisering_hf.pdf'
# if (skrivSKDEdisk) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PDF/Revaskularisering_hf.pdf'}
#
# indikatorFigAndelGrVar_1aar(Antall, outfile, tittel, width=600, height=700, sideTxt = 'Helseforetak',
#                             decreasing=F, terskel=terskel, minstekrav = NA, minstekravTxt = 'Moderat=',
#                             maal = NA, maalTxt='Høy=', til100=FALSE, skriftStr=0.8)

############ Hjerneslag ############################################################
###################################################################################

# load("C:/GIT/indikatoR/data/Hjerneslag_behandlet_slagenhet_bo.RData")


Antall <- Hjerneslag_behandlet_slagenhet_bo

names(Antall)[3] <- 'Antall'
tittel <- 'Andel behandlet i slagenhet, pr. boområde, justert'
outfile <- 'C:/GIT/indikatoR/doc/figurer/BehSlagenhet_bo_justert.pdf'
if (skrivSKDEdisk) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PDF/BehSlagenhet_bo_justert.pdf'}
if (skrivSKDEdiskPNG) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PNG/Brystbevarende_bo_justert.png'}
terskel <- terskel_bo3
indikatorFigAndelGrVar_aldKjJust(Antall=Antall, outfile=outfile, tittel=tittel, width=800, height=700,
                                 decreasing=F, terskel=terskel, minstekrav = 80, minstekravTxt = 'Moderat=',
                                 maal = 90, maalTxt='Høy=', til100=FALSE)


AntTilfeller <- tidyr::spread(Hjerneslag_behandlet_slagenhet_sh_v2[,1:3], 'Aar', 'TellerC')
rownames(AntTilfeller) <- AntTilfeller$Helseenhet
AntTilfeller <- AntTilfeller[, -1]
N <- tidyr::spread(Hjerneslag_behandlet_slagenhet_sh_v2[,c(1,2,4)], 'Aar', 'NevnerC')
rownames(N) <- N$Helseenhet
N <- N[, -1]
N[is.na(N)] <- 0
tittel <- 'Andel behandlet i slagenhet, pr. sykehus'
outfile <- 'C:/GIT/indikatoR/doc/figurer/BehSlagenhet_sh.pdf'
if (skrivSKDEdisk) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PDF/BehSlagenhet_sh.pdf'}
if (skrivSKDEdiskPNG) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PNG/Brystbevarende_bo_justert.png'}
terskel <- terskel_sh
indikatorFigAndelGrVar(AntTilfeller=AntTilfeller, N=N, outfile=outfile, tittel=tittel, sideTxt='Sykehus', minstekrav = 80, maal = 90,
                       width=width, height=height, decreasing=decreasing, terskel=terskel, skriftStr=0.8, pktStr=1,
                       minstekravTxt = 'Moderat=', maalTxt='Høy=')

AntTilfeller <- tidyr::spread(Hjerneslag_behandlet_slagenhet_hf[,1:3], 'Aar', 'TellerC')
rownames(AntTilfeller) <- AntTilfeller$helseenhet
AntTilfeller <- AntTilfeller[, -1]
N <- tidyr::spread(Hjerneslag_behandlet_slagenhet_hf[,c(1,2,4)], 'Aar', 'NevnerC')
rownames(N) <- N$helseenhet
N <- N[, -1]
N[is.na(N)] <- 0
tittel <- 'Andel behandlet i slagenhet, pr. helseforetak'
outfile <- 'C:/GIT/indikatoR/doc/figurer/BehSlagenhet_hf.pdf'
if (skrivSKDEdisk) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PDF/BehSlagenhet_hf.pdf'}
if (skrivSKDEdiskPNG) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PNG/Brystbevarende_bo_justert.png'}

indikatorFigAndelGrVar(AntTilfeller=AntTilfeller, N=N, outfile=outfile, tittel=tittel, sideTxt='Helseforetak', minstekrav = 80, maal = 90,
                       width=width, height=height, decreasing=decreasing, terskel=terskel, skriftStr=0.8, pktStr=1,
                       minstekravTxt = 'Moderat=', maalTxt='Høy=')



Antall <- Hjerneslag_tromsbolyse_bo
Antall$TellerE[is.na(Antall$TellerE)] <- 0

names(Antall)[3] <- 'Antall'
tittel <- 'Andel med trombolyse innen 40 min., pr. boområde, justert'
outfile <- 'C:/GIT/indikatoR/doc/figurer/Trombolyse_bo_justert.pdf'
if (skrivSKDEdisk) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PDF/Trombolyse_bo_justert.pdf'}
if (skrivSKDEdiskPNG) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PNG/Brystbevarende_bo_justert.png'}
terskel <- terskel_bo3
indikatorFigAndelGrVar_aldKjJust(Antall=Antall, outfile=outfile, tittel=tittel, width=800, height=700,
                                 decreasing=F, terskel=terskel, minstekrav = 30, minstekravTxt = 'Moderat=',
                                 maal = 50, maalTxt='Høy=', til100=FALSE, legPlass='nede') #

AntTilfeller <- tidyr::spread(Hjerneslag_tromsbolyse_sh_v2[,1:3], 'Aar', 'TellerE')
rownames(AntTilfeller) <- AntTilfeller$Helseenhet
AntTilfeller <- AntTilfeller[, -1]
N <- tidyr::spread(Hjerneslag_tromsbolyse_sh_v2[,c(1,2,4)], 'Aar', 'NevnerE')
rownames(N) <- N$Helseenhet
N <- N[, -1]
N[is.na(N)] <- 0
tittel <- 'Andel med trombolyse innen 40 min., pr. sykehus'
outfile <- 'C:/GIT/indikatoR/doc/figurer/Trombolyse_sh.pdf'
if (skrivSKDEdisk) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PDF/Trombolyse_sh.pdf'}
if (skrivSKDEdiskPNG) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PNG/Brystbevarende_bo_justert.png'}
terskel <- terskel_sh
indikatorFigAndelGrVar(AntTilfeller=AntTilfeller, N=N, outfile=outfile, tittel=tittel, sideTxt='Sykehus',
                       width=width, height=height, decreasing=decreasing, terskel=terskel, skriftStr=0.8, pktStr=1,
                       minstekrav = 30, minstekravTxt = 'Moderat=', maal = 50, maalTxt='Høy=', legPlass='nede')

AntTilfeller <- tidyr::spread(Hjerneslag_tromsbolyse_hf[,1:3], 'Aar', 'TellerE')
rownames(AntTilfeller) <- AntTilfeller$helseenhet
AntTilfeller <- AntTilfeller[, -1]
N <- tidyr::spread(Hjerneslag_tromsbolyse_hf[,c(1,2,4)], 'Aar', 'NevnerE')
rownames(N) <- N$helseenhet
N <- N[, -1]
N[is.na(N)] <- 0
tittel <- 'Andel med trombolyse innen 40 min., pr. helseforetak'
outfile <- 'C:/GIT/indikatoR/doc/figurer/Trombolyse_hf.pdf'
if (skrivSKDEdisk) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PDF/Trombolyse_hf.pdf'}
if (skrivSKDEdiskPNG) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PNG/Brystbevarende_bo_justert.png'}

indikatorFigAndelGrVar(AntTilfeller=AntTilfeller, N=N, outfile=outfile, tittel=tittel, sideTxt='Helseforetak',
                       width=width, height=height, decreasing=decreasing, terskel=terskel, skriftStr=0.8, pktStr=1,
                       minstekrav = 30, minstekravTxt = 'Moderat=', maal = 50, maalTxt='Høy=', legPlass='nede')


################# Prolapsrater ############################################
#############################################################


Andeler <- Prolapskirurgi_rater
outfile <- 'C:/GIT/indikatoR/doc/figurer/Prolapskirurgi_rater.pdf'
if (skrivSKDEdisk) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PDF/Prolapskirurgi_rater.pdf'}
if (skrivSKDEdiskPNG) {outfile <- 'E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/PNG/Brystbevarende_bo_justert.png'}
tittel <- 'Rater prolapskirurgi etter boområde'
# width=800
# height=700
# decreasing=F
# terskel=30
# minstekrav = NA
# maal = NA
# til100 = F
# skriftStr=1.3
# pktStr=1.5
terskel <- terskel_bo3
indikatorFigRaterGrVar(Andeler=Andeler, outfile=outfile, tittel=tittel, width=width, height=height, til100=til100,
                       decreasing=decreasing, terskel=terskel, minstekrav=minstekrav, maal=maal, xtekst ='Rater pr. 1000 innbyggere')
