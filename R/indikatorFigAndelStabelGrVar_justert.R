#' Plot andeler/rater i stabelplot
#'
#' Denne funksjonen tar som input en dataramme med andeler av tre kategorier
#' som til sammen summerer til 100%
#'
#' @param plotdata En dataramme med rater/andeler i spesifisert form
#' @param outfile Angir filnavn og format på figuren som returneres,
#'
#' @return Et plot av rater over tre år
#' @export
#'
indikatorFigAndelStabelGrVar_justert <- function(Antall, outfile='', tittel='Tittel', sideTxt='Boområde/opptaksområde', terskel=30,
                                                 width=800, height=700, tertiler = c(-1,51,66,140))
  {

  Antall[is.na(Antall)] <- 0
  p <- cbind(rowSums(Antall[dim(Antall[1])[1] , c(2, 5, 8)]),
             rowSums(Antall[dim(Antall[1])[1] , c(3, 6, 9)]),
             rowSums(Antall[dim(Antall[1])[1] , c(4, 7, 10)]))
  p <- p/sum(p)

  andeler_ujust_gr1 <- Antall[ , c(2, 5, 8)]/rowSums(Antall[ , c(2, 5, 8)])
  andeler_ujust_gr2 <- Antall[ , c(3, 6, 9)]/rowSums(Antall[ , c(3, 6, 9)])
  andeler_ujust_gr3 <- Antall[ , c(4, 7, 10)]/rowSums(Antall[ , c(4, 7, 10)])

  andeler_justert <- andeler_ujust_gr1*p[1] + andeler_ujust_gr2*p[2] + andeler_ujust_gr3*p[3]
  andeler <- andeler_justert *100

  rownames(andeler) <- Antall[,1]
  N <- rowSums(Antall[, 2:10])
  N_tot <- rowSums(Antall[, 2:13])
  andeler[which(N<terskel), ] <- NA
  rekkefolg <- order(andeler[,3], decreasing = T, na.last = F)
  # rekkefolg <- order(andeler[,3], decreasing = T)
  andeler <- andeler[rekkefolg, ]
  N <- N[rekkefolg]
  N_tot <- N_tot[rekkefolg]
  # radnavn <- paste0(rownames(andeler), ' (N=', N, ')')
  radnavn <- rownames(andeler)
  names(andeler) <- substr(names(andeler),1,nchar(names(andeler))-8)

  FigTypUt <- rapbase::figtype(outfile='', width=width, height=height, pointsizePDF=11, fargepalett='BlaaOff')
  farger <- FigTypUt$farger
  # if (outfile == '') {windows(width = width, height = height)}
  windows(width = width, height = height)
  oldpar_mar <- par()$mar
  oldpar_fig <- par()$fig

  cexgr <- 1.3

  vmarg <- max(0, strwidth(radnavn, units='figure', cex=cexgr)*0.8)
  # hmarg <- max(0, 3*strwidth(max(N), units='figure', cex=cexgr)*0.7)
  par('fig'=c(vmarg, 1, 0, 1))
  par('mar'=c(5.1, 4.1, 4.1, 6.1))

  ypos <- barplot(t(as.matrix(andeler)), horiz=T, beside=FALSE, border=NA, main=tittel,
                  names.arg=rep('',dim(andeler)[1]), font.main=1, cex.main=1.3, xlab='Andel (%)',
                  las=1, col=farger[c(1,3,4)])

  ypos <- as.vector(ypos)
  mtext( radnavn, side=2, line=0.2, las=1, at=ypos, col=1, cex=cexgr)
  mtext( c(N, 'N'), side=4, line=2.5, las=1, at=c(ypos, max(ypos)+diff(ypos)[1]), col=1, cex=cexgr, adj = 1)
  mtext( c(paste0(round(rowSums(Antall[rekkefolg, 11:13])/N_tot*100, 0), '%'), 'Ukjent'), side=4, line=6.0, las=1,
         at=c(ypos, max(ypos)+diff(ypos)[1]), col=1, cex=cexgr, adj = 1)
  mtext(text = sideTxt, side=2, line=10.5, las=0, col=1, cex=cexgr)
  text(x=andeler[,1], y=ypos, labels = paste0(round(andeler[,1]), '%'), cex=0.85, pos=2, col='white')
  text(x=(andeler[,1]+andeler[,2]), y=ypos, labels = paste0(round(andeler[,2]), '%'), cex=0.85, pos=2)
  text(x=rep(100, length(andeler[3])), y=ypos, labels = paste0(round(andeler[,3]), '%'), cex=0.85, pos=2)
  if (length(which(is.na(andeler[,1]))) > 0){
    text(x=0, y=ypos[1:length(which(is.na(andeler[,1])))], labels = paste0('N < ', terskel), cex=0.85, pos=4)
  }

  par(xpd=TRUE)
  legend('top', inset=c(vmarg,-.03), names(andeler), fill = farger[c(1,3,4)], ncol = 3, border = farger[c(1,3,4)], bty = 'n', cex = 0.8)

  par('mar'= oldpar_mar)
  par('fig'= oldpar_fig)
  par(xpd=FALSE)

  # if (outfile != '') {dev.off()}

  if (outfile != '') {savePlot(outfile, type=substr(outfile, nchar(outfile)-2, nchar(outfile)))}



}




