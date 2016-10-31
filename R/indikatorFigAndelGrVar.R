#' Plot andeler/andeler i angitt format
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
indikatorFigAndelGrVar <- function(AntTilfeller, N, outfile, tittel, width=800, height=700, sideTxt='Boområde/opptaksområde',
                                   decreasing=F, terskel=30, minstekrav = NA, maal = NA, skriftStr=1.3, pktStr=1.5, legPlass='top',
                                   minstekravTxt='Min=', maalTxt='Mål=')
{
  andeler <- AntTilfeller/N * 100

  # terskel <- 10
  andeler[N < terskel] <- NA

  if (decreasing){
    rekkefolge <- order(andeler[, dim(andeler)[2]], decreasing = decreasing)
  } else {
    rekkefolge <- order(andeler[, dim(andeler)[2]], decreasing = decreasing, na.last = F)
  }
  andeler <- andeler[rekkefolge, ]
  N <- N[rekkefolge, ]
  andeler[N[, dim(andeler)[2]]<terskel, 1:2] <- NA
  pst_txt <- paste0(sprintf('%.1f', andeler[, dim(andeler)[2]]), '%')
  pst_txt[is.na(andeler[, dim(andeler)[2]])] <- paste0('N<', terskel, ' siste år')

  FigTypUt <- rapbase::figtype(outfile='', width=width, height=height, pointsizePDF=11, fargepalett='BlaaOff')
  farger <- FigTypUt$farger
  soyleFarger <- rep(farger[3], length(andeler[,dim(andeler)[2]]))
  soyleFarger[which(rownames(andeler)=='Norge')] <- farger[4]
  # if (outfile == '') {windows(width = width, height = height)}
  windows(width = width, height = height)

  oldpar_mar <- par()$mar
  oldpar_fig <- par()$fig
  oldpar_oma <- par()$oma

  cexgr <- skriftStr

  vmarg <- max(0, strwidth(rownames(andeler), units='figure', cex=cexgr)*0.8)
  # hmarg <- max(0, 3*strwidth(max(N), units='figure', cex=cexgr)*0.7)
  par('fig'=c(vmarg, 1, 0, 1))
  par('mar'=c(5.1, 4.1, 4.1, 9.1))
  par('oma'=c(0,2,0,0))

  ypos <- barplot( t(andeler[,dim(andeler)[2]]), beside=T, las=1,
                   main = tittel, font.main=1, cex.main=1.3,
                   # xlim=c(0,max(andeler, na.rm = T)*1.1),
                   xlim=c(0,100),
                   names.arg=rep('',dim(andeler)[1]),
                   horiz=T, axes=F, space=c(0,0.3),
                   col=soyleFarger, border=NA, xlab = 'Andel (%)') # '#96BBE7'
  #   ypos <- barplot(t(as.matrix(andeler)), horiz=T, beside=FALSE, border=NA, main=tittel,
  #                   names.arg=rep('',dim(andeler)[1]), font.main=1, cex.main=1.3, xlab='Andel %',
  #                   las=1, col=farger[c(1,3,4)])
  ypos <- as.vector(ypos)
  if (!is.na(minstekrav)) {
    lines(x=rep(minstekrav, 2), y=c(-1, max(ypos)+diff(ypos)[1]), col=farger[2], lwd=2)
    barplot( t(andeler[,dim(andeler)[2]]), beside=T, las=1,
             main = tittel, font.main=1, cex.main=1.3,
             # xlim=c(0,max(andeler, na.rm = T)*1.1),
             xlim=c(0,100),
             names.arg=rep('',dim(andeler)[1]),
             horiz=T, axes=F, space=c(0,0.3),
             col=soyleFarger, border=NA, xlab = 'Andel (%)', add=TRUE)
    par(xpd=TRUE)
    text(x=minstekrav, y=max(ypos)+diff(ypos)[1], labels = paste0(minstekravTxt, minstekrav,'%'), pos = 3, cex=0.7)
    par(xpd=FALSE)
  }
  if (!is.na(maal)) {
    lines(x=rep(maal, 2), y=c(-1, max(ypos)+diff(ypos)[1]), col=farger[2], lwd=2)
    barplot( t(andeler[, dim(andeler)[2]]), beside=T, las=1,
             main = tittel, font.main=1, cex.main=1.3,
             # xlim=c(0,max(andeler, na.rm = T)*1.1),
             xlim=c(0,100),
             names.arg=rep('',dim(andeler)[1]),
             horiz=T, axes=F, space=c(0,0.3),
             col=soyleFarger, border=NA, xlab = 'Andel (%)', add=TRUE)
    par(xpd=TRUE)
    text(x=maal, y=max(ypos)+diff(ypos)[1], labels = paste0(maalTxt,maal,'%'), pos = 3, cex=0.7)
    par(xpd=FALSE)
  }
  axis(1,cex.axis=0.9)
  mtext( rownames(andeler), side=2, line=0.2, las=1, at=ypos, col=1, cex=cexgr)

  if (dim(andeler)[2]==2) {
    mtext( c(N[,1], 2014), side=4, line=2.5, las=1, at=c(ypos, max(ypos)+diff(ypos)[1]), col=1, cex=cexgr, adj = 1)
    mtext( c(N[,2], 2015), side=4, line=5.5, las=1, at=c(ypos, max(ypos)+diff(ypos)[1]), col=1, cex=cexgr, adj = 1)
    mtext( 'N', side=4, line=4.0, las=1, at=max(ypos)+2*diff(ypos)[1], col=1, cex=cexgr, adj = 1)
    points(y=ypos, x=andeler[,1],cex=pktStr, pch= 19)
    text(x=0, y=ypos, labels = pst_txt, cex=0.75,pos=4)
    # mtext( 'Boområde/opptaksområde', side=2, line=9.5, las=0, col=1, cex=cexgr)
    mtext(sideTxt, WEST<-2, line=0.4, cex=cexgr, col="black", outer=TRUE)
    if (legPlass=='nede'){
      legend('bottomright', cex=1.2, bty='n', #bg='white', box.col='white',
             lwd=c(NA,NA), pch=c(19,15), pt.cex=c(1,2), col=c('black',farger[3]),
             legend=c('2014', '2015'), ncol = 1)
    } else {
      par(xpd=TRUE)
      legend('top', inset=c(vmarg,-.025), cex=1.2, bty='n', #bg='white', box.col='white',
             lwd=c(NA,NA), pch=c(19,15), pt.cex=c(1,2), col=c('black',farger[3]),
             legend=c('2014', '2015'), ncol = 3)
      par(xpd=FALSE)
    }


  } else {
    mtext( c(N[,1], 2013), side=4, line=2.5, las=1, at=c(ypos, max(ypos)+diff(ypos)[1]), col=1, cex=cexgr, adj = 1)
    mtext( c(N[,2], 2014), side=4, line=5.5, las=1, at=c(ypos, max(ypos)+diff(ypos)[1]), col=1, cex=cexgr, adj = 1)
    mtext( c(N[,3], 2015), side=4, line=8.5, las=1, at=c(ypos, max(ypos)+diff(ypos)[1]), col=1, cex=cexgr, adj = 1)
    mtext( 'N', side=4, line=5.5, las=1, at=max(ypos)+2*diff(ypos)[1], col=1, cex=cexgr, adj = 1)
    # mtext(text = sideTxt, side=2, line=9.5, las=0, col=1, cex=cexgr)
    mtext(sideTxt, WEST<-2, line=0.4, cex=cexgr, col="black", outer=TRUE)
    points(y=ypos, x=andeler[,1],cex=pktStr) #'#4D4D4D'
    points(y=ypos, x=andeler[,2],cex=pktStr,pch= 19)
    text(x=0, y=ypos, labels = pst_txt, cex=0.75,pos=4)
    if (legPlass=='nede'){
      legend(x=82, y=ypos[2]+1,xjust=0, cex=1.2, bty='o', bg='white', box.col='white',
             lwd=c(NA,NA,NA), pch=c(1,19,15), pt.cex=c(1,1,2), col=c('black','black',farger[3]),
             legend=c('2013','2014', '2015') )
    } else {
      par(xpd=TRUE)
      legend('top', inset=c(vmarg,-.025), cex=1.2, bty='n', #bg='white', box.col='white',
             lwd=c(NA,NA,NA), pch=c(1,19,15), pt.cex=c(1,1,2), col=c('black','black',farger[3]),
             legend=c('2013','2014', '2015'), ncol = 3)
      par(xpd=FALSE)
    }
  }


  par('mar'= oldpar_mar)
  par('fig'= oldpar_fig)
  par('oma'= oldpar_oma)
  # if (outfile != '') {dev.off()}

  if (outfile != '') {savePlot(outfile, type=substr(outfile, nchar(outfile)-2, nchar(outfile)))}

}
