#' Plot alder og kjønnsjusterte andeler/andeler i angitt format
#'
#' Denne funksjonen tar som input en dataramme med andeler over 3 år,
#' der radnavn angir grupperingsvariabel og kolonnenavn år. Funksjonen
#' returnerer et søyleplot hvor søylene representerer sist år, fyllt sirkel er året
#' før og åpen sirkel to år før
#'
#' @param andeler En dataramme med andeler/andeler i spesifisert form
#' @param outfile Angir filnavn og format på figuren som returneres,
#' @param N En vektor/matrise med N for ratene
#' @return Et plot av andeler over tre år
#'
#' @export
#'
indikatorFigAndelGrVar_justert <- function(AntTilfeller, N, outfile, tittel, width=800, height=700,
                                           decreasing=F, terskel=30, minstekrav = NA, sideTxt='Boområde/opptaksområde',
                                           maal = NA, tertiler = c(-1,51,66,140), justeringLand=T)
  {
  AntTilfeller[is.na(AntTilfeller)] <- 0
  # N[is.na(N)] <- 0

  if (justeringLand) {
    # Innb2015aldkj <- read.table('C:/GIT/indikatoR/doc/Innbyggere2015aldkj.csv', sep = ';', header = T, encoding = 'UTF-8')
    Innb2015aldkj <- Innb2015aldkj[Innb2015aldkj$ErMann==0, ]
    Innb2015aldkj$aldergr <- cut(Innb2015aldkj$Alder, breaks=tertiler, labels = FALSE)
    vekt <- tapply(Innb2015aldkj$AntInnb, Innb2015aldkj$aldergr, sum)
    vekt <- vekt/sum(vekt)
  } else {
    ### Registerbefolkning #######
    vekt <- as.vector( N['Norge', c(3,6,9)]/ rowSums(N['Norge', c(3,6,9)]))
    vekt <- as.numeric(as.character(vekt))

  }

  andeler_ujust <- AntTilfeller/N
  andeler_ujust[is.na(andeler_ujust)] <- 0
  N[is.na(N)] <- 0

  r1 <- rowSums(andeler_ujust[, c("2013gr1", "2013gr2", "2013gr3")] * t(matrix(vekt, nrow = length(vekt), ncol = dim(andeler_ujust)[1])))
  r2 <- rowSums(andeler_ujust[, c("2014gr1", "2014gr2", "2014gr3")]*t(matrix(vekt, nrow = length(vekt), ncol = dim(andeler_ujust)[1])))
  r3 <- rowSums(andeler_ujust[, c("2015gr1", "2015gr2", "2015gr3")]*t(matrix(vekt, nrow = length(vekt), ncol = dim(andeler_ujust)[1])))
  n1 <- rowSums(N[, c("2013gr1", "2013gr2", "2013gr3")])
  N <- data.frame('2013'=rowSums(N[, c("2013gr1", "2013gr2", "2013gr3")]), '2014'=rowSums(N[, c("2014gr1", "2014gr2", "2014gr3")]),
                  '2015'=rowSums(N[, c("2015gr1", "2015gr2", "2015gr3")]))

  andeler <- data.frame('2013' = r1, '2014' = r2, '2015' = r3) *100

  # terskel <- 10
  andeler[N < terskel] <- NA

  if (decreasing){
    rekkefolge <- order(andeler[,3], decreasing = decreasing)
  } else {
    rekkefolge <- order(andeler[,3], decreasing = decreasing, na.last = F)
  }
  andeler <- andeler[rekkefolge, ]
  N <- N[rekkefolge, ]
  andeler[N[,3]<terskel, 1:2] <- NA
  # pst_txt <- paste0(round(andeler[,3]), '%')
  pst_txt <- paste0(sprintf('%.1f', andeler[, 3]), '%')
  pst_txt[is.na(andeler[,3])] <- paste0('N<', terskel, ' siste år')

  # N[N<terskel] <- paste0('<', terskel)


  FigTypUt <- rapbase::figtype(outfile='', width=width, height=height, pointsizePDF=11, fargepalett='BlaaOff')
  farger <- FigTypUt$farger
  soyleFarger <- rep(farger[3], length(andeler[,3]))
  soyleFarger[which(rownames(andeler)=='Norge')] <- farger[4]
  # if (outfile == '') {windows(width = width, height = height)}
  windows(width = width, height = height)

  oldpar_mar <- par()$mar
  oldpar_fig <- par()$fig

  cexgr <- 1.3

  vmarg <- max(0, strwidth(rownames(andeler), units='figure', cex=cexgr)*0.8)
  # hmarg <- max(0, 3*strwidth(max(N), units='figure', cex=cexgr)*0.7)
  par('fig'=c(vmarg, 1, 0, 1))
  par('mar'=c(5.1, 4.1, 4.1, 9.1))

  ypos <- barplot( t(andeler[,3]), beside=T, las=1,
                   main = tittel, font.main=1, cex.main=1.3,
                   # xlim=c(0,max(andeler, na.rm = T)*1.1),
                   xlim=c(0,100),
                   names.arg=rep('',dim(andeler)[1]),
                   horiz=T, axes=F, space=c(0,0.3),
                   col=soyleFarger, border=NA, xlab = 'Andel %') # '#96BBE7'
  #   ypos <- barplot(t(as.matrix(andeler)), horiz=T, beside=FALSE, border=NA, main=tittel,
  #                   names.arg=rep('',dim(andeler)[1]), font.main=1, cex.main=1.3, xlab='Andel %',
  #                   las=1, col=farger[c(1,3,4)])
  ypos <- as.vector(ypos)
  if (!is.na(minstekrav)) {
    lines(x=rep(minstekrav, 2), y=c(-1, max(ypos)+diff(ypos)[1]), col=farger[2], lwd=2)
    barplot( t(andeler[,3]), beside=T, las=1,
             main = tittel, font.main=1, cex.main=1.3,
             # xlim=c(0,max(andeler, na.rm = T)*1.1),
             xlim=c(0,100),
             names.arg=rep('',dim(andeler)[1]),
             horiz=T, axes=F, space=c(0,0.3),
             col=soyleFarger, border=NA, xlab = 'Andel %', add=TRUE)
    par(xpd=TRUE)
    text(x=minstekrav, y=max(ypos)+diff(ypos)[1], labels = paste0('Min=',minstekrav,'%'), pos = 3, cex=0.7)
    par(xpd=FALSE)
  }
  if (!is.na(maal)) {
    lines(x=rep(maal, 2), y=c(-1, max(ypos)+diff(ypos)[1]), col=farger[2], lwd=2)
    barplot( t(andeler[,3]), beside=T, las=1,
             main = tittel, font.main=1, cex.main=1.3,
             # xlim=c(0,max(andeler, na.rm = T)*1.1),
             xlim=c(0,100),
             names.arg=rep('',dim(andeler)[1]),
             horiz=T, axes=F, space=c(0,0.3),
             col=soyleFarger, border=NA, xlab = 'Andel %', add=TRUE)
    par(xpd=TRUE)
    text(x=maal, y=max(ypos)+diff(ypos)[1], labels = paste0('Mål=',maal,'%'), pos = 3, cex=0.7)
    par(xpd=FALSE)
  }
  axis(1,cex.axis=0.9)
  mtext( rownames(andeler), side=2, line=0.2, las=1, at=ypos, col=1, cex=cexgr)
  mtext( c(N[,1], 2013), side=4, line=2.5, las=1, at=c(ypos, max(ypos)+diff(ypos)[1]), col=1, cex=cexgr, adj = 1)
  mtext( c(N[,2], 2014), side=4, line=5.5, las=1, at=c(ypos, max(ypos)+diff(ypos)[1]), col=1, cex=cexgr, adj = 1)
  mtext( c(N[,3], 2015), side=4, line=8.5, las=1, at=c(ypos, max(ypos)+diff(ypos)[1]), col=1, cex=cexgr, adj = 1)
  mtext( 'N', side=4, line=5.5, las=1, at=max(ypos)+2*diff(ypos)[1], col=1, cex=cexgr, adj = 1)
  mtext(text = sideTxt, side=2, line=10.5, las=0, col=1, cex=cexgr)
  points(y=ypos, x=andeler[,1],cex=1.5) #'#4D4D4D'
  points(y=ypos, x=andeler[,2],cex=1.5,pch= 19)
  # text(x=0, y=ypos,andeler[,3], cex=0.75,pos=4)
  text(x=0, y=ypos, labels = pst_txt, cex=0.75,pos=4)
  # legend('bottomright',c('2013','2014'), pch=c(1,19),bty='n',cex=1.5)

  #   legend('bottomright', xjust=1, cex=1.2, bty='o', bg='white', box.col='white',
  #          lwd=c(NA,NA,NA,2), pch=c(1,19,15,NA), pt.cex=c(1,1,2,1), col=c('black','black',farger[3],farger[1]),
  #          legend=c('2013','2014', '2015', 'Mål') )
  #   legend('bottomright', xjust=1, cex=1.2, bty='o', bg='white', box.col='white',
  #          lwd=c(NA,NA,NA), pch=c(1,19,15), pt.cex=c(1,1,2), col=c('black','black',farger[3]),
  #          legend=c('2013','2014', '2015') )
  legend(x=82, y=ypos[2]+1,xjust=0, cex=1.2, bty='o', bg='white', box.col='white',
         lwd=c(NA,NA,NA), pch=c(1,19,15), pt.cex=c(1,1,2), col=c('black','black',farger[3]),
         legend=c('2013','2014', '2015') )

  par('mar'= oldpar_mar)
  par('fig'= oldpar_fig)

  # if (outfile != '') {dev.off()}

  if (outfile != '') {savePlot(outfile, type=substr(outfile, nchar(outfile)-2, nchar(outfile)))}

}
