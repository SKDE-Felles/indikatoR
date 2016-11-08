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
indikatorFigAndelStabelGrVar_mUkjent <- function(Antall, outfile, tittel, inkl_ukjent=F, sideTxt='Boområde/opptaksområde',
                                                 width=800, height=700, terskel=30, skriftStr=1.3) {


  Antall[is.na(Antall)] <- 0

  if (inkl_ukjent) {
    andeler <- Antall[, 2:5]/rowSums(Antall[, 2:5])*100
    # rownames(andeler) <- Antall$`Behandlende sykehus`
    rownames(andeler) <- Antall[,1]
    rekkefolg <- order(andeler[,2])
    andeler <- andeler[rekkefolg, ]
    N <- rowSums(Antall[, 2:5])[rekkefolg]
    radnavn <- paste0(rownames(andeler), ' (N=', N, ')')

    FigTypUt <- rapbase::figtype(outfile='', width=8, height=7, pointsizePDF=11, fargepalett='BlaaOff')
    farger <- FigTypUt$farger
    # if (outfile == '') {windows(width = width, height = height)}
    windows(width = width, height = height)
    oldpar_mar <- par()$mar
    oldpar_fig <- par()$fig

    cexgr <- 0.8
    vmarg <- max(0, strwidth(radnavn, units='figure', cex=cexgr)*0.7)
    par('fig'=c(vmarg, 1, 0, 1))
    # par('mar'=c(5.1, 4.1, 4.1, 6.1))

    ypos <- barplot(t(as.matrix(andeler)), horiz=T, beside=FALSE, border=NA, main=tittel,
                    names.arg=rep('',dim(andeler)[1]), font.main=1, cex.main=1, xlab='Andel %',
                    las=1, col=farger[1:4])

    ypos <- as.vector(ypos)
    mtext(radnavn , side=2, line=0.2, las=1, at=ypos, col=1, cex=cexgr)
    par(xpd=TRUE)
    legend('top', inset=c(vmarg,-.03), names(andeler), fill = farger[1:4], ncol = 4, border = farger[1:4],
           bty = 'n', cex = 0.9)

    par('mar'= oldpar_mar)
    par('fig'= oldpar_fig)
    par(xpd=FALSE)

  } else {
  andeler <- Antall[, 2:4]/rowSums(Antall[, 2:4])*100
  rownames(andeler) <- Antall[,1]
  N <- rowSums(Antall[, 2:4])
  N_tot <- rowSums(Antall[, 2:5])
  andeler[which(N<terskel), ] <- NA
  rekkefolg <- order(andeler[,3], decreasing = T, na.last = F)
  andeler <- andeler[rekkefolg, ]
  N <- N[rekkefolg]
  N_tot <- N_tot[rekkefolg]
  radnavn <- rownames(andeler)

  FigTypUt <- rapbase::figtype(outfile='', width=width, height=height, pointsizePDF=11, fargepalett='BlaaOff')
  farger <- FigTypUt$farger
  windows(width = width, height = height)
  oldpar_mar <- par()$mar
  oldpar_fig <- par()$fig
  oldpar_oma <- par()$oma

  cexgr <- skriftStr

  vmarg <- max(0, strwidth(radnavn, units='figure', cex=cexgr)*0.8)
  par('fig'=c(vmarg, 1, 0, 1))
  par('mar'=c(5.1, 4.1, 4.1, 6.1))
  par('oma'=c(0,2,0,0))

  ypos <- barplot(t(as.matrix(andeler)), horiz=T, beside=FALSE, border=NA, main=tittel,
                  names.arg=rep('',dim(andeler)[1]), font.main=1, cex.main=1.3, xlab='Andel (%)',
                  las=1, col=farger[c(1,3,4)])

  ypos <- as.vector(ypos)
  mtext( radnavn, side=2, line=0.2, las=1, at=ypos, col=1, cex=cexgr)
  mtext( c(N, 'N'), side=4, line=2.5, las=1, at=c(ypos, max(ypos)+diff(ypos)[1]), col=1, cex=cexgr, adj = 1)
  mtext( c(paste0(round(Antall[rekkefolg, 5]/N_tot*100, 0), ' %'), 'Ukjent'), side=4, line=6.0, las=1,
         at=c(ypos, max(ypos)+diff(ypos)[1]), col=1, cex=cexgr, adj = 1)
  # mtext(text = sideTxt, side=2, line=10.5, las=0, col=1, cex=cexgr)
  mtext(sideTxt, WEST<-2, line=0.4, cex=cexgr, col="black", outer=TRUE)
  text(x=andeler[,1], y=ypos, labels = paste0(round(andeler[,1]), ' %'), cex=0.9, pos=2, col='white')
  text(x=(andeler[,1]+andeler[,2]), y=ypos, labels = paste0(round(andeler[,2]), ' %'), cex=0.9, pos=2)
  text(x=rep(100, length(andeler[3])), y=ypos, labels = paste0(round(andeler[,3]), ' %'), cex=0.9, pos=2)
  if (length(which(is.na(andeler[,1]))) > 0){
    text(x=0, y=ypos[1:length(which(is.na(andeler[,1])))], labels = paste0('N < ', terskel), cex=0.9, pos=4)
  }
  par(xpd=TRUE)
  legend('top', inset=c(vmarg,-.03), names(andeler), fill = farger[c(1,3,4)], ncol = 3, border = farger[c(1,3,4)],
         bty = 'n', cex = 0.9)
  par(xpd=FALSE)

  par('mar'= oldpar_mar)
  par('fig'= oldpar_fig)
  par('oma'= oldpar_oma)

  }

  if (outfile != '') {savePlot(outfile, type=substr(outfile, nchar(outfile)-2, nchar(outfile)))}



}




