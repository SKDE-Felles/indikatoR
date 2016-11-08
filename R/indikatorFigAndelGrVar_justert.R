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
indikatorFigAndelGrVar_justert <- function(AntTilfeller, N, outfile, tittel, width=800, height=700, skriftStr=1.3,
                                           decreasing=F, terskel=30, minstekrav = NA, sideTxt='Boområde/opptaksområde',
                                           maal = NA)
  {
  AntTilfeller[is.na(AntTilfeller)] <- 0
  # N[is.na(N)] <- 0

    vekt <- as.vector( N['Norge', c(3,6,9)]/ rowSums(N['Norge', c(3,6,9)]))
    vekt <- as.numeric(as.character(vekt))

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

  andeler[N < terskel] <- NA

  if (decreasing){
    rekkefolge <- order(andeler[,3], decreasing = decreasing)
  } else {
    rekkefolge <- order(andeler[,3], decreasing = decreasing, na.last = F)
  }
  andeler <- andeler[rekkefolge, ]
  N <- N[rekkefolge, ]
  andeler[N[,3]<terskel, 1:2] <- NA
  pst_txt <- paste0(sprintf('%.0f', andeler[, 3]), ' %')
  pst_txt[is.na(andeler[,3])] <- paste0('N<', terskel, ' siste år')

  FigTypUt <- rapbase::figtype(outfile='', width=width, height=height, pointsizePDF=11, fargepalett='BlaaOff')
  farger <- FigTypUt$farger
  soyleFarger <- rep(farger[3], length(andeler[,3]))
  soyleFarger[which(rownames(andeler)=='Norge')] <- farger[4]
  windows(width = width, height = height)

  oldpar_mar <- par()$mar
  oldpar_fig <- par()$fig
  oldpar_oma <- par()$oma

  cexgr <- skriftStr

  vmarg <- max(0, strwidth(rownames(andeler), units='figure', cex=cexgr)*0.8)
  par('fig'=c(vmarg, 1, 0, 1))
  par('mar'=c(5.1, 4.1, 4.1, 9.1))
  par('oma'=c(0,2,0,0))

  ypos <- barplot( t(andeler[,3]), beside=T, las=1,
                   main = tittel, font.main=1, cex.main=1.3,
                   # xlim=c(0,max(andeler, na.rm = T)*1.1),
                   xlim=c(0,100),
                   names.arg=rep('',dim(andeler)[1]),
                   horiz=T, axes=F, space=c(0,0.3),
                   col=soyleFarger, border=NA, xlab = 'Andel (%)') # '#96BBE7'
  ypos <- as.vector(ypos)
  yposOver <- max(ypos)+0.5*diff(ypos)[1]
  if (!is.na(minstekrav)) {
    lines(x=rep(minstekrav, 2), y=c(-1, yposOver), col=farger[2], lwd=2)
    barplot( t(andeler[,3]), beside=T, las=1,
             main = tittel, font.main=1, cex.main=1.3,
             # xlim=c(0,max(andeler, na.rm = T)*1.1),
             xlim=c(0,100),
             names.arg=rep('',dim(andeler)[1]),
             horiz=T, axes=F, space=c(0,0.3),
             col=soyleFarger, border=NA, xlab = 'Andel (%)', add=TRUE)
    par(xpd=TRUE)
    text(x=minstekrav, y=yposOver, labels = 'Min', pos = 3, cex=0.9) #paste0('Min.=',minstekrav,'%')
    par(xpd=FALSE)
  }
  if (!is.na(maal)) {
    lines(x=rep(maal, 2), y=c(-1, yposOver), col=farger[2], lwd=2)
    barplot( t(andeler[,3]), beside=T, las=1,
             main = tittel, font.main=1, cex.main=1.3,
             # xlim=c(0,max(andeler, na.rm = T)*1.1),
             xlim=c(0,100),
             names.arg=rep('',dim(andeler)[1]),
             horiz=T, axes=F, space=c(0,0.3),
             col=soyleFarger, border=NA, xlab = 'Andel (%)', add=TRUE)
    par(xpd=TRUE)
    text(x=maal, y=yposOver, labels = 'Mål', pos = 3, cex=0.9) #paste0('Mål=',maal,'%'),
    par(xpd=FALSE)
  }
  axis(1,cex.axis=0.9)
  mtext( rownames(andeler), side=2, line=0.2, las=1, at=ypos, col=1, cex=cexgr)
  mtext( c(N[,1], 2013), side=4, line=2.5, las=1, at=c(ypos, max(ypos)+diff(ypos)[1]), col=1, cex=cexgr, adj = 1)
  mtext( c(N[,2], 2014), side=4, line=5.5, las=1, at=c(ypos, max(ypos)+diff(ypos)[1]), col=1, cex=cexgr, adj = 1)
  mtext( c(N[,3], 2015), side=4, line=8.5, las=1, at=c(ypos, max(ypos)+diff(ypos)[1]), col=1, cex=cexgr, adj = 1)
  mtext( 'N', side=4, line=5.5, las=1, at=max(ypos)+2*diff(ypos)[1], col=1, cex=cexgr, adj = 1)
  # mtext(text = sideTxt, side=2, line=10.5, las=0, col=1, cex=cexgr)
  mtext(sideTxt, WEST<-2, line=0.4, cex=cexgr, col="black", outer=TRUE)
  points(y=ypos, x=andeler[,1],cex=1.5) #'#4D4D4D'
  points(y=ypos, x=andeler[,2],cex=1.5,pch= 19)
  text(x=0, y=ypos, labels = pst_txt, cex=0.9,pos=4)
  legend(0, yposOver, yjust=0, xpd=TRUE, cex=0.9, ncol=3, bty='o', bg='white', box.col='white',
         lwd=c(NA,NA,NA), pch=c(1,19,15), pt.cex=c(1.2,1.2,1.8), col=c('black','black',farger[3]),
         legend=c('2013','2014', '2015') )


  par('mar'= oldpar_mar)
  par('fig'= oldpar_fig)
  par('oma'= oldpar_oma)

  if (outfile != '') {savePlot(outfile, type=substr(outfile, nchar(outfile)-2, nchar(outfile)))}

}
