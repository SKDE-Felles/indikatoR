setwd('C:/GIT/indikatoR/doc/')
rm(list = ls())
library(indikatoR)


############  Brystkreft - brystbevarende  ###################################################

# load("C:/GIT/indikatoR/data/Brystbevarende_sh.RData")

outfile='C:/GIT/indikatoR/doc/figurer/Brystbevarende_sh.pdf'
tittel=c('Andel med brystbevarende kirurgi for tumorstørrelse 0-30 mm', 'pr. sykehus')
width=800
height=700
AntTilfeller=Brystbevarende_sh$AntTilfeller
N=Brystbevarende_sh$AntTotalt
decreasing=F
terskel=30

indikatorFigAndelGrVar(AntTilfeller=AntTilfeller, N=N, outfile=outfile, tittel=tittel, sideTxt='Sykehus',
                       width=width, height=height, decreasing=decreasing, terskel=terskel, minstekrav = 70, maal = 80, legPlass='nede')


# load("C:/GIT/indikatoR/data/Brystbevarende_bo.RData")

outfile='C:/GIT/indikatoR/doc/figurer/Brystbevarende_bo_ujustert.pdf'
tittel=c('Andel med brystbevarende kirurgi for tumorstørrelse 0-30 mm', 'pr. boområde, ujustert')
width=800
height=700
AntTilfeller=Brystbevarende_bo$AntTilfeller
N=Brystbevarende_bo$AntTotalt
decreasing=F

indikatorFigAndelGrVar(AntTilfeller=AntTilfeller, N=N, outfile=outfile, tittel=tittel,
                       width=width, height=height, decreasing=decreasing, terskel=terskel, minstekrav = 70, maal = 80, legPlass='nede')

### Aldersjustering
# load("C:/GIT/indikatoR/data/Brystbevarende_bo_aldersgr.RData")
AntTilfeller=Brystbevarende_bo_aldersgr$AntTilfeller
N=Brystbevarende_bo_aldersgr$AntTotalt
terskel=30
tertiler = c(-1,56,66,140)
outfile='C:/GIT/indikatoR/doc/figurer/Brystbevarende_bo_justert.pdf'
tittel=c('Andel med brystbevarende kirurgi for tumorstørrelse 0-30 mm', 'pr. boområde, aldersjustert')
justeringLand=F

indikatorFigAndelGrVar_justert(AntTilfeller=AntTilfeller, N=N, outfile=outfile, tittel=tittel,
                               width=width, height=height, decreasing=decreasing, terskel=terskel,
                               minstekrav = 70, maal = 80, tertiler=tertiler, justeringLand=justeringLand)


###############################################################################
##########  Brystkreft Ki 67  #######################################################

# load("C:/GIT/indikatoR/data/BrystKi672015_sh.RData")

Antall <- BrystKi67sh2015
tittel='Ki67 proliferasjonsrate, pr. sykehus'
inkl_ukjent=F
width=800
height=700
outfile='C:/GIT/indikatoR/doc/figurer/TestresultaterKi67_sh.pdf'
indikatorFigAndelStabelGrVar_mUkjent(Antall=Antall, outfile, tittel=tittel, sideTxt='Sykehus',
                                     inkl_ukjent=inkl_ukjent, width=width, height=height, terskel=30)

# load("C:/GIT/indikatoR/data/BrystKi672015_bo.RData")
Antall <- BrystKi67Bo2015
tittel='Ki67 proliferasjonsrate, pr. boområde, ujustert'
outfile='C:/GIT/indikatoR/doc/figurer/TestresultaterKi67_bo.pdf'
indikatorFigAndelStabelGrVar_mUkjent(Antall=Antall, outfile=outfile, tittel=tittel, inkl_ukjent=F)

# load("C:/GIT/indikatoR/data/BrystKi67Bo2015_aldersgr.RData")
tertiler <- c(-1,56,67,140)
tittel='Ki67 proliferasjonsrate, pr. boområde, justert'
width=800
height=700
outfile <- 'TestresultaterKi67_bo_justert.png'

Antall <- BrystKi67Bo2015_aldersgr
indikatorFigAndelStabelGrVar_justert(Antall=Antall, outfile=outfile, tittel=tittel,
                                     width=800, height=700, tertiler = tertiler, terskel=30)

###############################################################################
##########  Brystkreft histologi  #######################################################

# load("C:/GIT/indikatoR/data/BrystkreftHistologi_sh.RData")
Antall <- BrystkreftHistologi_sh
tittel='Histologisk Grad (Nottingham), pr. sykehus'
outfile='C:/GIT/indikatoR/doc/figurer/HistologiskGrad(Nottingham)_sh.pdf'

indikatorFigAndelStabelGrVar_mUkjent(Antall=Antall, outfile, tittel=tittel, sideTxt='Sykehus', inkl_ukjent=F)


# load("C:/GIT/indikatoR/data/BrystkreftHistologi_bo.RData")
Antall <- BrystkreftHistologi_bo
tittel='Histologisk Grad (Nottingham), pr. boområde, ujustert'
outfile='C:/GIT/indikatoR/doc/figurer/HistologiskGrad(Nottingham)_bo_ujustert.pdf'

indikatorFigAndelStabelGrVar_mUkjent(Antall=Antall, outfile=outfile, tittel=tittel, inkl_ukjent=F)

## justert

# load("C:/GIT/indikatoR/data/BrystkreftHistologi_bo2015_aldersgr.RData")
tertiler <- c(-1,54,66,140)
tittel='Histologisk Grad (Nottingham), pr. boområde, justert'
width=800
height=700
outfile <- 'C:/GIT/indikatoR/doc/figurer/HistologiskGrad(Nottingham)_bo_justert.pdf'

Antall <- BrystkreftHistologi_bo2015_aldersgr

indikatorFigAndelStabelGrVar_justert(Antall=Antall, outfile=outfile, tittel=tittel,
                                     width=800, height=700, tertiler = tertiler)




################################################################################
################### The hips don't lie ###################################

# load("C:/GIT/indikatoR/data/Hoftebrudd_Preoperativ_liggetid_bo_justert.RData")

Andeler <- Hoftebrudd_Preoperativ_liggetid_bo_justert[, -c(4,6,7)]
names(Andeler)[names(Andeler)=='andel24'] <- 'andel'
outfile <- 'C:/GIT/indikatoR/doc/figurer/HofteOp24_bo.pdf'
tittel <- 'Andel operert innen 24 timer, pr. boområde, justert'
width=800
height=700
decreasing=F
terskel=30
minstekrav = NA
maal = NA

indikatorFigAndelGrVar_preberegnet(Andeler=Andeler, outfile=outfile, tittel=tittel, width=width, height=height, til100=F,
                                               decreasing=decreasing, terskel=terskel, minstekrav=minstekrav, maal=maal)


Andeler <- Hoftebrudd_Preoperativ_liggetid_bo_justert[, -c(3,6,7)]
names(Andeler)[names(Andeler)=='andel48'] <- 'andel'
outfile <- 'C:/GIT/indikatoR/doc/figurer/HofteOp48_bo.pdf'
tittel <- 'Andel operert innen 48 timer, pr. boområde, justert'

indikatorFigAndelGrVar_preberegnet(Andeler=Andeler, outfile=outfile, tittel=tittel, width=width, height=height,
                                   decreasing=decreasing, terskel=terskel, minstekrav=minstekrav, maal=maal)


# load("C:/GIT/indikatoR/data/Hoftebrudd_Preoperativ_liggetid_sh.RData")

Andeler <- Hoftebrudd_Preoperativ_liggetid_sh[ , -c(3,4,7)]
Andeler <- Andeler[, c(1,2,4,3)]
names(Andeler)[names(Andeler)=='andel24'] <- 'andel'
names(Andeler)[names(Andeler)=='Antall'] <- 'antall'
names(Andeler)[names(Andeler)=='behsh'] <- 'bohf'
outfile <- 'C:/GIT/indikatoR/doc/figurer/HofteOp24_sh.pdf'
tittel <- 'Andel operert innen 24 timer, pr. sykehus'
skriftStr <- 1.0
pktStr <- 1.0

indikatorFigAndelGrVar_preberegnet(Andeler=Andeler, outfile=outfile, tittel=tittel, width=width, height=height,
                                   decreasing=decreasing, terskel=terskel, minstekrav=minstekrav, maal=maal,
                                   skriftStr=skriftStr, pktStr=pktStr, sideTxt='Sykehus')

Andeler <- Hoftebrudd_Preoperativ_liggetid_sh[ , -c(3,4,6)]
Andeler <- Andeler[, c(1,2,4,3)]
names(Andeler)[names(Andeler)=='andel48'] <- 'andel'
names(Andeler)[names(Andeler)=='Antall'] <- 'antall'
names(Andeler)[names(Andeler)=='behsh'] <- 'bohf'
outfile <- 'C:/GIT/indikatoR/doc/figurer/HofteOp48_sh.pdf'
tittel <- 'Andel operert innen 48 timer, pr. sykehus'
skriftStr <- 1.0
pktStr <- 1.0

indikatorFigAndelGrVar_preberegnet(Andeler=Andeler, outfile=outfile, tittel=tittel, width=width, height=height,
                                   decreasing=decreasing, terskel=terskel, minstekrav=minstekrav, maal=maal,
                                   skriftStr=skriftStr, pktStr=pktStr, sideTxt='Sykehus')


###### Produksjon

# load("C:/GIT/indikatoR/data/Hoftebrudd_Produksjon_sh.RData")

Antall <- Hoftebrudd_Produksjon_sh[ , -c(3,4,5)]
names(Antall)[names(Antall)=='Totalt.antall'] <- 'antall'
names(Antall)[names(Antall)=='Behandlende.sykehus'] <- 'bohf'
outfile <- 'C:/GIT/indikatoR/doc/figurer/Hoftebrudd_produksjon_sh.pdf'
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
tittel <- c('Antall hoftebrudd meldt til Nasjonalt Hoftebruddregister', 'i 2013-15, per boområde')
pktStr <- 1.3
skriftStr <- 1.3

indikatorFigAntallGrVar(Antall=Antall, outfile=outfile, tittel=tittel, width=800, height=700,
                        decreasing=F, xtekst ='Antall',
                        skriftStr=skriftStr, pktStr=pktStr)

##### Type behandling

Andeler <- Hoftebrudd_Produksjon_bo[ , -1]
Andeler <- Andeler[Andeler$aar==2015, ]
Andeler <- Andeler[, -2]
rownames(Andeler) <- Andeler$Boomraade
Andeler <- Andeler[, -1]
names(Andeler)[names(Andeler)=='n'] <- 'N'
names(Andeler)[names(Andeler)=='Pinning'] <- 'To skruer eller pinner'

tittel <- 'Operasjonsmetoder ved lårhalsbrudd 2015, boområde, ujustert'
outfile <- 'C:/GIT/indikatoR/doc/figurer/Hofte_operasjonsmetode_bo_ujustert.pdf'
indikatorFigAndelStabelGrVar(Andeler=Andeler, outfile=outfile, tittel=tittel)


Andeler <- Hoftebrudd_andeler_2015_bo_justert
rownames(Andeler)[rownames(Andeler)=='Norge'] <- 'Norge HF-omr'
rownames(Andeler) <- substr(rownames(Andeler),1,nchar(rownames(Andeler))-7)

tittel <- 'Operasjonsmetoder ved lårhalsbrudd 2015, boområde, justert'
outfile <- 'C:/GIT/indikatoR/doc/figurer/Hofte_operasjonsmetode_bo_justert.pdf'

indikatorFigAndelStabelGrVar(Andeler=Andeler, outfile=outfile, tittel=tittel)



# load("C:/GIT/indikatoR/data/Hoftebrudd_Produksjon_sh.RData")

Andeler <- Hoftebrudd_Produksjon_sh
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
outfile <- 'C:/GIT/indikatoR/doc/figurer/Hofte_operasjonsmetode_sh_ujustert.pdf'

indikatorFigAndelStabelGrVar(Andeler=Andeler, outfile=outfile, tittel=tittel, skriftStr=skriftStr, sideTxt = 'Sykehus')

Andeler <- Hoftebrudd_rater
outfile <- 'C:/GIT/indikatoR/doc/figurer/Hoftebrudd_rater_bo.pdf'
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

indikatorFigRaterGrVar(Andeler=Andeler, outfile=outfile, tittel=tittel, width=width, height=height, til100=til100,
                       decreasing=decreasing, terskel=terskel, minstekrav=minstekrav, maal=maal, xtekst ='Rater pr. 1000 innbyggere')


############## Proteser ###########################################
######################################################################

# load("C:/GIT/indikatoR/data/Hofteproteser_rater.RData")

Andeler <- Hofteproteser_rater
outfile <- 'C:/GIT/indikatoR/doc/figurer/Hofteproteser_rater_bo.pdf'
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

indikatorFigRaterGrVar(Andeler=Andeler, outfile=outfile, tittel=tittel, width=width, height=height, til100=til100,
                                   decreasing=decreasing, terskel=terskel, minstekrav=minstekrav, maal=maal, xtekst ='Rater pr. 1000 innbyggere')

# load("C:/GIT/indikatoR/data/Kneproteser_rater.RData")

Andeler <- Kneproteser_rater
outfile <- 'C:/GIT/indikatoR/doc/figurer/Kneproteser_rater_bo.pdf'
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
tittel <- c('Antall hofteproteser meldt til Leddproteseregisteret', 'i 2013-15, per sykehus')
pktStr <- 0.8
skriftStr <- 0.8
indikatorFigAntallGrVar(Antall=Antall, outfile=outfile, tittel=tittel, width=800, height=700,
                        decreasing=F, xtekst ='Antall', sideTxt='Sykehus',
                        skriftStr=skriftStr, pktStr=pktStr)

Antall <- Kneprotese_Produksjon_sh[, c(2,1,6)]
names(Antall) <- c('bohf', 'aar', 'antall')
outfile <- 'C:/GIT/indikatoR/doc/figurer/Kne_produksjon_sh.pdf'
tittel <- c('Antall kneproteser meldt til Leddproteseregisteret', 'i 2013-15, per sykehus')
indikatorFigAntallGrVar(Antall=Antall, outfile=outfile, tittel=tittel, width=800, height=700,
                        decreasing=F, xtekst ='Antall', sideTxt='Sykehus',
                        skriftStr=skriftStr, pktStr=pktStr)



############ Hjerteinfarkt ############################################################
###################################################################################

## Angio

# load("C:/GIT/indikatoR/data/Angio_bo.RData")

Antall <- Angio_bo[, -4]
names(Antall)[3] <- 'Antall'
tittel <- 'Andel med angio innen 72 timer, pr. boområde, justert'
outfile <- 'C:/GIT/indikatoR/doc/figurer/Angio_U72_bo_justert.pdf'


indikatorFigAndelGrVar_aldKjJust(Antall=Antall, outfile=outfile, tittel=tittel, width=800, height=700,
                                             decreasing=F, terskel=30, minstekrav = NA,
                                             maal = NA, til100=FALSE)


aux <- aggregate(Antall[, c('Antall', 'N')], by=list(aar=Antall$aar, bohf=Antall$bohf), sum)
AntTilfeller <- tidyr::spread(aux[,-4], 'aar', 'Antall')
N <- tidyr::spread(aux[,-3], 'aar', 'N')
rownames(AntTilfeller) <- AntTilfeller$bohf
rownames(N) <- N$bohf
AntTilfeller <- AntTilfeller[,-1]
N <- N[,-1]
tittel <- 'Andel med angio innen 72 timer, pr. boområde, ujustert'
outfile <- 'C:/GIT/indikatoR/doc/figurer/Angio_U72_bo_ujustert.pdf'

indikatorFigAndelGrVar(AntTilfeller=AntTilfeller, N=N, outfile=outfile, tittel=tittel,
                       width=width, height=height, decreasing=F, terskel=30)


Antall <- Angio_bo
Antall$Antall <- rowSums(Antall[,3:4])
Antall <- Antall[, -c(3,4)]
Antall <- Antall[, c(1,2,5,3,4)]
tittel <- 'Andel med angio, pr. boområde, justert'
outfile <- 'C:/GIT/indikatoR/doc/figurer/Angio_totalt_bo_justert.pdf'

indikatorFigAndelGrVar_aldKjJust(Antall=Antall, outfile=outfile, tittel=tittel, width=800, height=700,
                                 decreasing=F, terskel=30, minstekrav = NA,
                                 maal = NA, til100=FALSE)

aux <- aggregate(Antall[, c('Antall', 'N')], by=list(aar=Antall$aar, bohf=Antall$bohf), sum)
AntTilfeller <- tidyr::spread(aux[,-4], 'aar', 'Antall')
rownames(AntTilfeller) <- AntTilfeller$bohf
AntTilfeller <- AntTilfeller[,-1]
tittel <- 'Andel med angio, pr. boområde, ujustert'
outfile <- 'C:/GIT/indikatoR/doc/figurer/Angio_totalt_bo_ujustert.pdf'

indikatorFigAndelGrVar(AntTilfeller=AntTilfeller, N=N, outfile=outfile, tittel=tittel,
                       width=width, height=height, decreasing=F, terskel=30)


# load("C:/GIT/indikatoR/data/Angio_sh.RData")

AntTilfeller <- tidyr::spread(Angio_sh[,1:3], 'Aar', 'under72t')
rownames(AntTilfeller) <- AntTilfeller$Sykehus
AntTilfeller <- AntTilfeller[, -1]
N <- tidyr::spread(Angio_sh[,c(1,2,6)], 'Aar', 'Nevner')
rownames(N) <- N$Sykehus
N <- N[, -1]
N[is.na(N)] <- 0
tittel <- 'Andel med angio innen 72 timer, pr. sykehus'
outfile <- 'C:/GIT/indikatoR/doc/figurer/Angio_U72_sh.pdf'

indikatorFigAndelGrVar(AntTilfeller=AntTilfeller, N=N, outfile=outfile, tittel=tittel, sideTxt='Sykehus',
                       width=width, height=height, decreasing=decreasing, terskel=terskel, skriftStr=0.8, pktStr=1)

AntTilfeller <- Angio_sh
AntTilfeller$AntTot <- rowSums(AntTilfeller[, c('under72t', 'over72t')])
AntTilfeller <- tidyr::spread(AntTilfeller[, c('Aar', 'Sykehus', 'AntTot')], 'Aar', 'AntTot')
rownames(AntTilfeller) <- AntTilfeller$Sykehus
AntTilfeller <- AntTilfeller[, -1]
tittel <- 'Andel med angio, pr. sykehus'
outfile <- 'C:/GIT/indikatoR/doc/figurer/Angio_totalt_sh.pdf'

indikatorFigAndelGrVar(AntTilfeller=AntTilfeller, N=N, outfile=outfile, tittel=tittel, sideTxt='Sykehus',
                       width=width, height=height, decreasing=decreasing, terskel=terskel, skriftStr=0.8, pktStr=1)

## Revaskularisering

# load("C:/GIT/indikatoR/data/Revaskularisering_bo.RData")

Antall <- Revaskularisering_bo[, -4]
names(Antall)[3] <- 'Antall'
tittel <- 'Revaskularisering under 30 min., pr. boområde, justert'
outfile <- 'C:/GIT/indikatoR/doc/figurer/Revaskularisering_bo_justert.pdf'

indikatorFigAndelGrVar_aldKjJust_1aar(Antall=Antall, outfile=outfile, tittel=tittel, width=800, height=700,
                                 decreasing=F, terskel=30, minstekrav = NA,
                                 maal = NA, til100=F)

aux <- aggregate(Antall[, c('Antall', 'N')], by=list(aar=Antall$aar, bohf=Antall$bohf), sum)
AntTilfeller <- tidyr::spread(aux[,-4], 'aar', 'Antall')
N <- tidyr::spread(aux[,-3], 'aar', 'N')
rownames(AntTilfeller) <- AntTilfeller$bohf
rownames(N) <- N$bohf
AntTilfeller <- AntTilfeller[,-1]
N <- N[,-1]


# load("C:/GIT/indikatoR/data/Revaskularisering_sh.RData")

Antall <- Revaskularisering_sh[, -4]
names(Antall) <- c('aar', 'bohf', 'Antall', 'N')
tittel <- 'Revaskularisering under 30 min., pr. sykehus'
outfile <- 'C:/GIT/indikatoR/doc/figurer/Revaskularisering_sh.pdf'

indikatorFigAndelGrVar_1aar(Antall, outfile, tittel, width=600, height=700,
                            decreasing=F, terskel=30, minstekrav = NA,
                            maal = NA, til100=FALSE, skriftStr=0.8)


############ Hjerneslag ############################################################
###################################################################################

# load("C:/GIT/indikatoR/data/Hjerneslag_behandlet_slagenhet_bo.RData")


Antall <- Hjerneslag_behandlet_slagenhet_bo

names(Antall)[3] <- 'Antall'
tittel <- 'Andel behandlet i slagenhet, pr. boområde, justert'
outfile <- 'C:/GIT/indikatoR/doc/figurer/BehSlagenhet_bo_justert.pdf'

indikatorFigAndelGrVar_aldKjJust(Antall=Antall, outfile=outfile, tittel=tittel, width=800, height=700,
                                 decreasing=F, terskel=30, minstekrav = NA,
                                 maal = NA, til100=FALSE)


# load("C:/GIT/indikatoR/data/Hjerneslag_behandlet_slagenhet_sh.RData")

AntTilfeller <- tidyr::spread(Hjerneslag_behandlet_slagenhet_sh[,1:3], 'Aar', 'TellerC')
rownames(AntTilfeller) <- AntTilfeller$Helseenhet
AntTilfeller <- AntTilfeller[, -1]
N <- tidyr::spread(Hjerneslag_behandlet_slagenhet_sh[,c(1,2,4)], 'Aar', 'NevnerC')
rownames(N) <- N$Helseenhet
N <- N[, -1]
N[is.na(N)] <- 0
tittel <- 'Andel behandlet i slagenhet, pr. sykehus'
outfile <- 'C:/GIT/indikatoR/doc/figurer/BehSlagenhet_sh.pdf'

indikatorFigAndelGrVar(AntTilfeller=AntTilfeller, N=N, outfile=outfile, tittel=tittel, sideTxt='Sykehus',
                       width=width, height=height, decreasing=decreasing, terskel=terskel, skriftStr=0.8, pktStr=1)




Antall <- Hjerneslag_tromsbolyse_bo
Antall$TellerE[is.na(Antall$TellerE)] <- 0

names(Antall)[3] <- 'Antall'
tittel <- 'Andel med trombolyse .... , pr. boområde, justert'
outfile <- 'C:/GIT/indikatoR/doc/figurer/Trombolyse_bo_justert.pdf'

indikatorFigAndelGrVar_aldKjJust(Antall=Antall, outfile=outfile, tittel=tittel, width=800, height=700,
                                 decreasing=F, terskel=30, minstekrav = NA,
                                 maal = NA, til100=FALSE)

AntTilfeller <- tidyr::spread(Hjerneslag_tromsbolyse_sh[,1:3], 'Aar', 'TellerE')
rownames(AntTilfeller) <- AntTilfeller$Helseenhet
AntTilfeller <- AntTilfeller[, -1]
N <- tidyr::spread(Hjerneslag_tromsbolyse_sh[,c(1,2,4)], 'Aar', 'NevnerE')
rownames(N) <- N$Helseenhet
N <- N[, -1]
N[is.na(N)] <- 0
tittel <- 'Andel med trombolyse .... , pr. sykehus'
outfile <- 'C:/GIT/indikatoR/doc/figurer/Trombolyse_sh.pdf'

indikatorFigAndelGrVar(AntTilfeller=AntTilfeller, N=N, outfile=outfile, tittel=tittel, sideTxt='Sykehus',
                       width=width, height=height, decreasing=decreasing, terskel=terskel, skriftStr=0.8, pktStr=1)


################# Prolapsrater ############################################
#############################################################


Andeler <- Prolapskirurgi_rater
outfile <- 'C:/GIT/indikatoR/doc/figurer/Prolapskirurgi_rater.pdf'
tittel <- 'Rater prolapskirurgi etter boområde'
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

################# DEBUG ZONE BELOW ############################################


# AntTilfeller_test <- AntTilfeller
#
# AntTilfeller_test[, c('2015gr1', '2015gr2', '2015gr3')] <- Antall[,2:4]
# AntTilfeller_test[, -which(names(AntTilfeller_test) %in% c('2015gr1', '2015gr2', '2015gr3'))] <- NA
#
# N_test <- N
# N_test[, -which(names(AntTilfeller_test) %in% c('2015gr1', '2015gr2', '2015gr3'))] <- NA
# N_test[, '2015gr1'] <- rowSums(Antall[, c(2,5,8)])
# N_test[, '2015gr2'] <- rowSums(Antall[, c(3,6,9)])
# N_test[, '2015gr3'] <- rowSums(Antall[, c(4,7,10)])
#
# outfile='test1.png'
# tittel=c('Dette er en test', 'pr. boområde, aldersjustert (register)')
# justeringLand=F
#
# indikatorFigAndelGrVar_justert(AntTilfeller=AntTilfeller_test, N=N_test, outfile=outfile, tittel=tittel,
#                                width=width, height=height, decreasing=decreasing, terskel=terskel,
#                                minstekrav = NA, maal = NA, tertiler=tertiler, justeringLand=justeringLand)
# AntTilfeller_ujust <- AntTilfeller_test[, 1:3]
# AntTilfeller_ujust[, 3] <- rowSums(AntTilfeller_test[,c(3,6,9)])
# N_ujust <- N_test[, 1:3]
# N_ujust[, 3] <- rowSums(N_test[,c(3,6,9)])
# outfile='test2.png'
# tittel=c('Dette er en test', 'pr. boområde, ujustert (register)')
#
# indikatorFigAndelGrVar(AntTilfeller=AntTilfeller_ujust, N=N_ujust, outfile=outfile, tittel=tittel,
#                        width=width, height=height, decreasing=decreasing, terskel=terskel, minstekrav = NA, maal = NA)
#
#
# AntTilfeller_test[, c('2015gr1', '2015gr2', '2015gr3')] <- Antall[,5:7]
#
# outfile='test3.png'
# tittel=c('Dette er en test', 'pr. boområde, aldersjustert (register)')
# indikatorFigAndelGrVar_justert(AntTilfeller=AntTilfeller_test, N=N_test, outfile=outfile, tittel=tittel,
#                                width=width, height=height, decreasing=decreasing, terskel=terskel,
#                                minstekrav = NA, maal = NA, tertiler=tertiler, justeringLand=justeringLand)
#
# AntTilfeller_ujust[, 3] <- rowSums(AntTilfeller_test[,c(3,6,9)])
# # N_ujust <- N_test[, 1:3]
# # N_ujust[, 3] <- rowSums(N_test[,c(3,6,9)])
# outfile='test4.png'
# tittel=c('Dette er en test', 'pr. boområde, ujustert (register)')
#
# indikatorFigAndelGrVar(AntTilfeller=AntTilfeller_ujust, N=N_ujust, outfile=outfile, tittel=tittel,
#                        width=width, height=height, decreasing=decreasing, terskel=terskel, minstekrav = NA, maal = NA)
#
#
# AntTilfeller_test[, c('2015gr1', '2015gr2', '2015gr3')] <- Antall[,8:10]
#
# outfile='test5.png'
# tittel=c('Dette er en test', 'pr. boområde, aldersjustert (register)')
# indikatorFigAndelGrVar_justert(AntTilfeller=AntTilfeller_test, N=N_test, outfile=outfile, tittel=tittel,
#                                width=width, height=height, decreasing=decreasing, terskel=terskel,
#                                minstekrav = NA, maal = NA, tertiler=tertiler, justeringLand=justeringLand)
#
# AntTilfeller_ujust[, 3] <- rowSums(AntTilfeller_test[,c(3,6,9)])
# # N_ujust <- N_test[, 1:3]
# # N_ujust[, 3] <- rowSums(N_test[,c(3,6,9)])
# outfile='test6.png'
# tittel=c('Dette er en test', 'pr. boområde, ujustert (register)')
#
# indikatorFigAndelGrVar(AntTilfeller=AntTilfeller_ujust, N=N_ujust, outfile=outfile, tittel=tittel,
#                        width=width, height=height, decreasing=decreasing, terskel=terskel, minstekrav = NA, maal = NA)
#
#

# aux <- Antall[,2:10]
# aux[aux<5] <- 9999
# apply(aux, 1, max)


#
# aux <- cbind(rowSums(Antall[,2:4]), rowSums(Antall[,5:7]), rowSums(Antall[,8:10]))
#
# rownames(aux) <- as.character(Antall[,1])
#
# aux2<- aux/rowSums(aux)*100
#
# # BrystKi67Bo2015_aldersgr
#
# Innb2015aldkj <- Innb2015aldkj[Innb2015aldkj$ErMann==0, ]
# Innb2015aldkj$aldergr <- cut(Innb2015aldkj$Alder, breaks=tertiler, labels = FALSE)
# p <- tapply(Innb2015aldkj$AntInnb, Innb2015aldkj$aldergr, sum)
# p <- p/sum(p)
#
# andeler_ujust_gr1 <- BrystKi67Bo2015_aldersgr[ , c(2, 5, 8)]/rowSums(BrystKi67Bo2015_aldersgr[ , c(2, 5, 8)])
# andeler_ujust_gr2 <- BrystKi67Bo2015_aldersgr[ , c(3, 6, 9)]/rowSums(BrystKi67Bo2015_aldersgr[ , c(3, 6, 9)])
# andeler_ujust_gr3 <- BrystKi67Bo2015_aldersgr[ , c(4, 7, 10)]/rowSums(BrystKi67Bo2015_aldersgr[ , c(4, 7, 10)])
#
# r_gr1 <- rowSums(andeler_ujust_gr1*t(matrix(p, nrow = length(p), ncol = dim(andeler_ujust_gr1)[1])))
#
# andeler_lav <- data.frame(andeler_ujust_gr1[,1], andeler_ujust_gr2[,1], andeler_ujust_gr3[,1])
# andeler_intermediaer <- data.frame(andeler_ujust_gr1[,2], andeler_ujust_gr2[,2], andeler_ujust_gr3[,2])
# andeler_hoy <- data.frame(andeler_ujust_gr1[,3], andeler_ujust_gr2[,3], andeler_ujust_gr3[,3])
#
# andeler_justert <- andeler_lav*p[1] + andeler_intermediaer*p[2] + andeler_hoy*p[3]
#
# andeler_juks <- andeler_justert / cbind(rowSums(andeler_justert), rowSums(andeler_justert), rowSums(andeler_justert))
#
#

###############################################################################
##########  Brystkreft aksille  #######################################################
### Behandler
# load("C:/GIT/indikatoR/data/BrystAksille_sh.RData")
#
# outfile='BrystAksille_sh.png'
# tittel=c('Andel aksilledisseksjoner, 10 eller flere lymfeknuter', 'pr. sykehus')
#
# AntTilfeller=BrystAksille_sh$AntTilfeller
# N=BrystAksille_sh$AntTotalt
# decreasing=F
# terskel=30
#
# width=800
# height=700
#
# indikatorFigAndelGrVar(AntTilfeller=AntTilfeller, N=N, outfile=outfile, tittel=tittel,
#                        width=width, height=height, decreasing=decreasing, terskel=terskel, minstekrav = 85, maal = 98)
#
# ### Boområder
# load("C:/GIT/indikatoR/data/BrystAksille_bo.RData")
#
# outfile='BrystAksille_bo.png'
# tittel=c('Andel aksilledisseksjoner, 10 eller flere lymfeknuter', 'pr. boområde')
# AntTilfeller=BrystAksille_bo$AntTilfeller
# N=BrystAksille_bo$AntTotalt
# decreasing=F
# terskel=10
# width=800
# height=700
#
# indikatorFigAndelGrVar(AntTilfeller=AntTilfeller, N=N, outfile=outfile, tittel=tittel,
#                        width=width, height=height, decreasing=decreasing, terskel=terskel, minstekrav = 85, maal = 98)
#
# ### Aldersjustering
#
# load("C:/GIT/indikatoR/data/BrystAksille_bo_aldersgr.RData")
#
# outfile='BrystAksille_bo_justert_register.png'
# tittel=c('Andel aksilledisseksjoner, 10 eller flere lymfeknuter', 'pr. boområde, aldersjustert (register)')
# AntTilfeller=BrystAksille_bo_aldersgr$AntTilfeller
# N=BrystAksille_bo_aldersgr$AntTotalt
# terskel=10
# tertiler = c(-1,51,66,140)
# justeringLand=F
#
# indikatorFigAndelGrVar_justert(AntTilfeller=AntTilfeller, N=N, outfile=outfile, tittel=tittel,
#                        width=width, height=height, decreasing=decreasing, terskel=terskel,
#                        minstekrav = 85, maal = 98, tertiler=tertiler, justeringLand=justeringLand)
#
# outfile='BrystAksille_bo_justert_landet.png'
# tittel=c('Andel aksilledisseksjoner, 10 eller flere lymfeknuter', 'pr. boområde, aldersjustert (landet)')
# justeringLand=T
#
# indikatorFigAndelGrVar_justert(AntTilfeller=AntTilfeller, N=N, outfile=outfile, tittel=tittel,
#                                width=width, height=height, decreasing=decreasing, terskel=terskel,
#                                minstekrav = 85, maal = 98, tertiler=tertiler, justeringLand=justeringLand)
#
##### Type behandling protese
#
# Andeler <- Hofteprotese_Produksjon_sh[, -(3:5)]
# Andeler <- Andeler[Andeler$aar==2015, ]
# rownames(Andeler) <- Andeler$behsh
# Andeler <- Andeler[, -(1:2)]
# Andeler <- Andeler[, c(2:4, 1)]
# names(Andeler) <- c('Usementert', 'Sementert', 'Hybrid', 'N')
# Andeler[is.na(Andeler)] <- 0
#
# tittel <- 'Festemetoder (??) for hofteproteser 2015, pr. sykehus'
# outfile <- 'Hofte_feste_sh.png'
#
# indikatorFigAndelStabelGrVar(Andeler=Andeler, outfile=outfile, tittel=tittel, skriftStr=0.8, sideTxt = 'Sykehus')
#
#
