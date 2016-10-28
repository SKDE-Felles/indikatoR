#' Plot antall i angitt format
#'
#' Denne funksjonen tar som input en dataramme med antall.Funksjonen
#' returnerer et søyleplot hvor søylene representerer sist år, fyllt sirkel er året
#' før og åpen sirkel to år før
#'
#' @param Antall En dataramme med antall i spesifisert form
#' @param outfile Angir filnavn og format på figuren som returneres,
#' @return Et plot av antall over tre år
#'
#' @export
#'
indikatorFigAntallGrVar <- function(Antall, outfile, tittel, width=800, height=700, sideTxt='Boområde/opptaksområde',
                                    decreasing=F, xtekst ='Antall', skriftStr=1.3, pktStr=1)
{


  Antall <- tidyr::spread(Antall, 'aar', 'antall')
  rownames(Antall) <- as.character(Antall$bohf)
  Antall <- Antall[, -1]
  Antall <- Antall[-which(rownames(Antall)=='Norge'), ]

  Antall[is.na(Antall)] <- 0

  if (decreasing){
    rekkefolge <- order(Antall[,3], decreasing = decreasing)
  } else {
    rekkefolge <- order(Antall[,3], decreasing = decreasing, na.last = F)
  }
  Antall <- Antall[rekkefolge, ]

  FigTypUt <- rapbase::figtype(outfile='', width=width, height=height, pointsizePDF=11, fargepalett='BlaaOff')
  farger <- FigTypUt$farger
  soyleFarger <- rep(farger[3], length(Antall[,3]))
  soyleFarger[which(rownames(Antall)=='Norge')] <- farger[4]
  windows(width = width, height = height)

  oldpar_mar <- par()$mar
  oldpar_fig <- par()$fig

  cexgr <- skriftStr
  xmax <- max(Antall, na.rm = T)*1.1

  vmarg <- max(0, strwidth(rownames(Antall), units='figure', cex=cexgr)*0.8)
  par('fig'=c(vmarg, 1, 0, 1))
  par('mar'=c(10.1, 4.1, 4.1, 9.1))

  ypos <- barplot( t(Antall[,3]), beside=T, las=1,
                   main = tittel, font.main=1, cex.main=1.3,
                   # xlim=c(0,max(Antall, na.rm = T)*1.1),
                   xlim=c(0,xmax),
                   names.arg=rep('',dim(Antall)[1]),
                   horiz=T, axes=F, space=c(0,0.3),
                   col=soyleFarger, border=NA, xlab = xtekst) # '#96BBE7'
  ypos <- as.vector(ypos)


  axis(1,cex.axis=0.9)
  mtext( rownames(Antall), side=2, line=0.2, las=1, at=ypos, col=1, cex=cexgr)
  mtext( c(Antall[,1], 2013), side=4, line=2.5, las=1, at=c(ypos, max(ypos)+diff(ypos)[1]), col=1, cex=cexgr, adj = 1)
  mtext( c(Antall[,2], 2014), side=4, line=5.5, las=1, at=c(ypos, max(ypos)+diff(ypos)[1]), col=1, cex=cexgr, adj = 1)
  mtext( c(Antall[,3], 2015), side=4, line=8.5, las=1, at=c(ypos, max(ypos)+diff(ypos)[1]), col=1, cex=cexgr, adj = 1)
  mtext( 'N', side=4, line=5.5, las=1, at=max(ypos)+2*diff(ypos)[1], col=1, cex=cexgr, adj = 1)
  if (sideTxt=='Sykehus'){
    mtext(text = sideTxt, side=2, line=15, las=0, col=1, cex=cexgr)

  } else {
    mtext(text = sideTxt, side=2, line=13.7, las=0, col=1, cex=cexgr)
  }

  points(y=ypos, x=Antall[,1],cex= pktStr) #'#4D4D4D'
  points(y=ypos, x=Antall[,2],cex= pktStr,pch= 19)
  # text(x=0, y=ypos, labels = pst_txt, cex=0.75,pos=4)
  par(xpd=TRUE)
  legend('top', inset=c(vmarg,-.025), cex=1.2, bty='n', #bg='white', box.col='white',
         lwd=c(NA,NA,NA), pch=c(1,19,15), pt.cex=c(1,1,2), col=c('black','black',farger[3]),
         legend=c('2013','2014', '2015'), ncol = 3)
  par(xpd=FALSE)

  #   legend(x=82, y=ypos[2]+1,xjust=0, cex=1.2, bty='o', bg='white', box.col='white',
  #          lwd=c(NA,NA,NA), pch=c(1,19,15), pt.cex=c(1,1,2), col=c('black','black',farger[3]),
  #          legend=c('2013','2014', '2015') )


  par('mar'= oldpar_mar)
  par('fig'= oldpar_fig)

  if (outfile != '') {savePlot(outfile, type='pdf')}
















}
