#' Plot andeler/rater i stabelplot
#'
#' Denne funksjonen tar som input en dataramme med andeler av tre kategorier
#' som til sammen summerer til 100%
#'
#' @param Andeler En dataramme med rater/andeler i spesifisert form
#' @param outfile Angir filnavn og format på figuren som returneres
#' @param tittel tittel på figuren
#'
#' @return Et plot av rater over tre år
#'
#' @export
#'
indikatorFigAndelStabelGrVar <- function(Andeler, outfile, tittel, skriftStr=1.3, sideTxt='Boområde/opptaksområde', terskel=30)
  {

  Andeler[Andeler$N < terskel, 1:3] <- NA

  rekkefolge <- order(Andeler[,1], decreasing = F, na.last = F)
  Andeler <- Andeler[rekkefolge, ]

  andeler <- Andeler[,-4]*100
  N <- Andeler[, 4]

  radnavn <- rownames(andeler)

  FigTypUt <- rapbase::figtype(outfile='', width=width, height=height, pointsizePDF=11, fargepalett='BlaaOff')
  farger <- FigTypUt$farger
  # if (outfile == '') {windows(width = width, height = height)}
  windows(width = width, height = height)
  oldpar_mar <- par()$mar
  oldpar_fig <- par()$fig

  cexgr <- skriftStr

  vmarg <- max(0, strwidth(radnavn, units='figure', cex=cexgr)*0.8)
  # hmarg <- max(0, 3*strwidth(max(N), units='figure', cex=cexgr)*0.7)
  par('fig'=c(vmarg, 1, 0, 1))
  par('mar'=c(5.1, 4.1, 4.1, 3.1))

  ypos <- barplot(t(as.matrix(andeler)), horiz=T, beside=FALSE, border=NA, main=tittel,
                  names.arg=rep('',dim(andeler)[1]), font.main=1, cex.main=1.3, xlab='Andel %',
                  las=1, col=farger[c(1,3,4)])

  ypos <- as.vector(ypos)
  mtext( radnavn, side=2, line=0.2, las=1, at=ypos, col=1, cex=cexgr)
  mtext( c(N, 'N'), side=4, line=2.5, las=1, at=c(ypos, max(ypos)+diff(ypos)[1]), col=1, cex=cexgr, adj = 1)
  if (sideTxt=='Sykehus'){
    mtext(text = sideTxt, side=2, line=15.5, las=0, col=1, cex=cexgr)
  } else {
    mtext(text = sideTxt, side=2, line=13.5, las=0, col=1, cex=cexgr)
  }
#   mtext( c(paste0(round(rowSums(Antall[rekkefolg, 11:13])/N_tot*100, 0), '%'), 'Ukjent'), side=4, line=6.0, las=1,
#          at=c(ypos, max(ypos)+diff(ypos)[1]), col=1, cex=cexgr, adj = 1)
#   text(x=andeler[,1], y=ypos, labels = paste0(round(andeler[,1]), '%'), cex=0.85, pos=2, col='white')
#   text(x=(andeler[,1]+andeler[,2]), y=ypos, labels = paste0(round(andeler[,2]), '%'), cex=0.85, pos=2)
  text(x=rep(100, length(andeler[3])), y=ypos, labels = paste0(round(andeler[,3]), '%'), cex=0.85, pos=2)

  text(x=0, y=ypos, labels = paste0(round(andeler[,1]), '%'), cex=0.85, pos=4, col='white')
  text(x=(andeler[,1]+andeler[,2]/2), y=ypos, labels = paste0(round(andeler[,2]), '%'), cex=0.85, pos=2)

  par(xpd=TRUE)
  legend('top', inset=c(vmarg,-.03), names(andeler), fill = farger[c(1,3,4)], ncol = 3, border = farger[c(1,3,4)], bty = 'n', cex = 0.8)

  par('mar'= oldpar_mar)
  par('fig'= oldpar_fig)
  par(xpd=FALSE)

  # if (outfile != '') {dev.off()}

  if (outfile != '') {savePlot(outfile, type='png')}



}




