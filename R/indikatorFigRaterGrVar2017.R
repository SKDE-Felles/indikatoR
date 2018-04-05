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
indikatorFigRaterGrVar2017 <- function(Andeler, outfile, tittel, width=600, height=600,
                                   decreasing=F, terskel=0, minstekrav = NA, maal = NA, xtekst ='Andel %',
                                   til100 = F, skriftStr=1.3, pktStr=1.5)
  {
  aarstall <- sort(unique(Andeler$aar))

  N <- tidyr::spread(Andeler[, -c(3,5)], 'aar', 'antall')
  rownames(N) <- as.character(N$bohf)
  N <- N[, 2:4]
  N[is.na(N)] <- 0

  andeler <- tidyr::spread(Andeler[, -c(4,5)], 'aar', 'andel')
  rownames(andeler) <- as.character(andeler$bohf)
  andeler <- andeler[, 2:4]

  andeler[N < terskel] <- NA

  if (decreasing){
    rekkefolge <- order(andeler[,3], decreasing = decreasing)
  } else {
    rekkefolge <- order(andeler[,3], decreasing = decreasing, na.last = F)
  }
  andeler <- andeler[rekkefolge, ]
  N <- N[rekkefolge, ]
  andeler[N[,3]<terskel, 1:2] <- NA
  pst_txt <- sprintf('%.0f', andeler[,3])
  pst_txt[is.na(andeler[,3])] <- paste0('N<', terskel, ' siste år')

  FigTypUt <- rapbase::figtype(outfile='', width=width, height=height, pointsizePDF=11, fargepalett='BlaaOff')
  farger <- FigTypUt$farger
  soyleFarger <- rep(farger[3], length(andeler[,3]))
  soyleFarger[which(rownames(andeler)=='Norge')] <- farger[4]
  windows(width = width, height = height)

  oldpar_mar <- par()$mar
  oldpar_fig <- par()$fig

  cexgr <- skriftStr

  if (til100) {xmax <- 100
  } else {
    xmax <- max(andeler, na.rm = T)*1.1
  }

  vmarg <- max(0, strwidth(rownames(andeler), units='figure', cex=cexgr)*0.8)
  par('fig'=c(vmarg, 1, 0, 1))
  par('mar'=c(5.1, 4.1, 4.1, 9.1))

  ypos <- barplot( t(andeler[,3]), beside=T, las=1,
                   main = tittel, font.main=1, cex.main=1.3,
                   # xlim=c(0,max(andeler, na.rm = T)*1.1),
                   xlim=c(0,xmax),
                   names.arg=rep('',dim(andeler)[1]),
                   horiz=T, axes=F, space=c(0,0.3),
                   col=soyleFarger, border=NA, xlab = xtekst) # '#96BBE7'
  ypos <- as.vector(ypos)
  if (!is.na(minstekrav)) {
    lines(x=rep(minstekrav, 2), y=c(-1, max(ypos)+diff(ypos)[1]), col=farger[2], lwd=2)
    barplot( t(andeler[,3]), beside=T, las=1,
             main = tittel, font.main=1, cex.main=1.3,
             #xlim=c(0,xmax),
             names.arg=rep('',dim(andeler)[1]),
             horiz=T, axes=F, space=c(0,0.3),
             col=soyleFarger, border=NA, add=TRUE)
    par(xpd=TRUE)
    text(x=minstekrav, y=max(ypos)+diff(ypos)[1], labels = paste0('Min=',minstekrav,'%'), pos = 3, cex=0.7)
    par(xpd=FALSE)
  }
  if (!is.na(maal)) {
    lines(x=rep(maal, 2), y=c(-1, max(ypos)+diff(ypos)[1]), col=farger[2], lwd=2)
    barplot( t(andeler[,3]), beside=T, las=1,
             main = tittel, font.main=1, cex.main=1.3,
             # xlim=c(0,max(andeler, na.rm = T)*1.1),
             xlim=c(0,xmax),
             names.arg=rep('',dim(andeler)[1]),
             horiz=T, axes=F, space=c(0,0.3),
             col=soyleFarger, border=NA, xlab = 'Andel %', add=TRUE)
    par(xpd=TRUE)
    text(x=maal, y=max(ypos)+diff(ypos)[1], labels = paste0('Mål=',maal,'%'), pos = 3, cex=0.7)
    par(xpd=FALSE)
  }
  axis(1,cex.axis=0.9)
  mtext( rownames(andeler), side=2, line=0.2, las=1, at=ypos, col=1, cex=cexgr)
  mtext( c(N[,1], as.character(aarstall[1])), side=4, line=2.5, las=1, at=c(ypos, max(ypos)+diff(ypos)[1]), col=1, cex=cexgr, adj = 1)
  mtext( c(N[,2], as.character(aarstall[2])), side=4, line=5.5, las=1, at=c(ypos, max(ypos)+diff(ypos)[1]), col=1, cex=cexgr, adj = 1)
  mtext( c(N[,3], as.character(aarstall[3])), side=4, line=8.5, las=1, at=c(ypos, max(ypos)+diff(ypos)[1]), col=1, cex=cexgr, adj = 1)
  mtext( 'Boområde/opptaksområde', side=2, line=9.5, las=0, col=1, cex=cexgr)#, outer=TRUE)#, adj = 1)
   mtext( 'N', side=4, line=5.5, las=1, at=max(ypos)+1.8*diff(ypos)[1], col=1, cex=cexgr, adj = 1)
  points(y=ypos, x=andeler[,1],cex=pktStr) #'#4D4D4D'
  points(y=ypos, x=andeler[,2],cex=pktStr,pch= 19)
  text(x=0, y=ypos, labels = pst_txt, cex=0.8,pos=4)
  par(xpd=TRUE)
  legend('top', inset=c(vmarg,-.03), cex=0.9, bty='n', # bg='white', box.col='white',
         lwd=c(NA,NA,NA), pch=c(1,19,15), pt.cex=c(1.2,1.2,1.8), col=c('black','black',farger[3]),
         legend=aarstall, ncol = 3)
  par(xpd=FALSE)

#   legend(x=82, y=ypos[2]+1,xjust=0, cex=1.2, bty='o', bg='white', box.col='white',
#          lwd=c(NA,NA,NA), pch=c(1,19,15), pt.cex=c(1,1,2), col=c('black','black',farger[3]),
#          legend=c('2013','2014', '2015') )

  par('mar'= oldpar_mar)
  par('fig'= oldpar_fig)

  if (outfile != '') {savePlot(outfile, type=substr(outfile, nchar(outfile)-2, nchar(outfile)))}








}
