#' Plot alder og kjønnsjusterte andeler/andeler i angitt format
#'
#' Denne funksjonen tar som input en dataramme med andeler over 3 år,
#' der radnavn angir grupperingsvariabel og kolonnenavn år. Funksjonen
#' returnerer et søyleplot hvor søylene representerer sist år, fyllt sirkel er året
#' før og åpen sirkel to år før
#'
#' @param AntTilfeller En dataramme med antall i spesifisert form
#' @param outfile Angir filnavn og format på figuren som returneres,
#' @param N En dataramme med nevneren i andelsberegningen
#' @param marker En vektor med navn på enheter som skal markeres med stjerne
#'
#' @return Et plot av andeler over tre år
#'
#' @export
#'
indikatorFigAndelGrVar_aldKjJust <-
                    function(Antall, outfile, tittel, width=800, height=700, decreasing=F, terskel=30,
                             minstekrav = NA, minstekravTxt='Min', sideTxt ='Boområde/opptaksområde',
                             maal = NA, maalTxt='Mål',  til100=FALSE, legPlass='top', marker=NA)
  {

  cexgr <- 1

  vekt <- tapply(Antall$N[which(Antall$aar==2015)], Antall$AldKjGr[which(Antall$aar==2015)], sum)
  vekt <- vekt/sum(vekt)

  N <- aggregate(Antall[, c('N')], by=list(aar=Antall$aar, bohf=Antall$bohf), sum)
  N <- tidyr::spread(N, 'aar', 'x')
  rownames(N) <- as.character(N$bohf)
  N <- as.data.frame(N)
  N <- N[, -1]

  aux <- aggregate(Antall[, c('Antall', 'N')], by=list(aar=Antall$aar, bohf=Antall$bohf, AldKjGr=Antall$AldKjGr), sum)

  vektFrame <- data.frame('AldKjGr'=sort(unique(Antall$AldKjGr)), 'vekt'=vekt)

  tmp <-merge(aux, vektFrame, by='AldKjGr', all.x=T)
  tmp$andel_ujust <- tmp$Antall/tmp$N
  tmp$andel_just <- tmp$andel_ujust * tmp$vekt

  andeler <- aggregate(tmp[, c('andel_just')], by=list(aar=tmp$aar, bohf=tmp$bohf), sum)

  andeler <- tidyr::spread(andeler, 'aar', 'x')
  rownames(andeler) <- as.character(andeler$bohf)
  andeler <- andeler[, -1]*100
  andeler[N < terskel] <- NA

  if (decreasing){
    rekkefolge <- order(andeler[, dim(andeler)[2]], decreasing = decreasing)
  } else {
    rekkefolge <- order(andeler[, dim(andeler)[2]], decreasing = decreasing, na.last = F)
  }
  andeler <- andeler[rekkefolge, ]
  N <- N[rekkefolge, ]
  andeler[N[, dim(andeler)[2]]<terskel, -dim(andeler)[2]] <- NA
  pst_txt <- paste0(sprintf('%.0f', andeler[, dim(andeler)[2]]), ' %')
  pst_txt[is.na(andeler[, dim(andeler)[2]])] <- paste0('N<', terskel, ' siste år')

  FigTypUt <- rapbase::figtype(outfile='', width=width, height=height, pointsizePDF=11, fargepalett='BlaaOff')
  farger <- FigTypUt$farger
  soyleFarger <- rep(farger[3], length(andeler[,dim(andeler)[2]]))
  soyleFarger[which(rownames(andeler)=='Norge')] <- farger[4]
  if (!is.na(marker[1])) {
    rownames(andeler)[which(rownames(andeler) %in% marker)] <-
      paste0(rownames(andeler)[which(rownames(andeler) %in% marker)], '*')
  }

  windows(width = width, height = height)

  oldpar_mar <- par()$mar
  oldpar_fig <- par()$fig
  oldpar_oma <- par()$oma

  if (til100) {xmax <- 100
  } else {
    # xmax <- max(andeler, na.rm = T)*1.1
    xmax <- ceiling(max(c(1.05*maal, max(andeler, na.rm = TRUE)))/10)*10
  }

  vmarg <- max(0, strwidth(rownames(andeler), units='figure', cex=cexgr)*0.8)
  par('fig'=c(vmarg, 1, 0, 1))
  par('mar'=c(5.1, 4.1, 5.1, 9.1))
  par('oma'=c(0,2,0,0))

  ypos <- barplot( t(andeler[,dim(andeler)[2]]), beside=T, las=1,
                   xlim=c(0,xmax),
                   names.arg=rep('',dim(andeler)[1]),
                   horiz=T, axes=F, space=c(0,0.3),
                   col=soyleFarger, border=NA, xlab = 'Andel (%)') # '#96BBE7'
  ypos <- as.numeric(ypos) #as.vector(ypos)
  yposOver <- max(ypos) + 0.4*diff(ypos)[1]

    if (!is.na(minstekrav)) {
    lines(x=rep(minstekrav, 2), y=c(-1, yposOver), col=farger[2], lwd=2)
    barplot( t(andeler[,dim(andeler)[2]]), beside=T, las=1,
            xlim=c(0,xmax),
             names.arg=rep('',dim(andeler)[1]),
             horiz=T, axes=F, space=c(0,0.3),
             col=soyleFarger, border=NA, xlab = 'Andel (%)', add=TRUE)
    par(xpd=TRUE)
    text(x=minstekrav, y=yposOver, labels = minstekravTxt, #paste0(minstekravTxt, minstekrav,'%'),
         pos = 3, cex=0.9)
    par(xpd=FALSE)
  }
  if (!is.na(maal)) {
    lines(x=rep(maal, 2), y=c(-1, yposOver), col=farger[2], lwd=2)
    barplot( t(andeler[,dim(andeler)[2]]), beside=T, las=1,
             xlim=c(0,xmax),
             names.arg=rep('',dim(andeler)[1]),
             horiz=T, axes=F, space=c(0,0.3),
             col=soyleFarger, border=NA, xlab = 'Andel (%)', add=TRUE)
    par(xpd=TRUE)
    text(x=maal, y=yposOver, labels = maalTxt, #paste0(maalTxt,maal,'%'),
         pos = 3, cex=0.9)
    par(xpd=FALSE)
  }

  axis(1, cex.axis=0.9)
  mtext( rownames(andeler), side=2, line=0.2, las=1, at=ypos, col=1, cex=cexgr)
  title(tittel, line=3) #outer=TRUE)


  if (dim(andeler)[2]==2) {
    mtext( c(N[,1], 2014), side=4, line=2.5, las=1, at=c(ypos, max(ypos)+diff(ypos)[1]), col=1, cex=cexgr, adj = 1)
    mtext( c(N[,2], 2015), side=4, line=5.5, las=1, at=c(ypos, max(ypos)+diff(ypos)[1]), col=1, cex=cexgr, adj = 1)
    mtext( 'N', side=4, line=4.0, las=1, at=max(ypos)+2*diff(ypos)[1], col=1, cex=cexgr, adj = 1)
    par(xpd=TRUE)
    points(y=ypos, x=andeler[,1],cex=1.5, pch= 19)
    par(xpd=FALSE)
    if (legPlass=='nede') {
      legend('bottomright', cex=0.9, bty='n',
             lwd=c(NA,NA), pch=c(19,15), pt.cex=c(1.2,1.8), col=c('black',farger[3]),
             legend=c('2014', '2015'), ncol = 1)}
    if (legPlass=='top'){
       legend(0, yposOver+ diff(ypos)[1], yjust=0, xpd=TRUE, cex=0.9, bty='n', #bg='white', box.col='white',y=max(ypos),
             lwd=c(NA,NA), pch=c(19,15), pt.cex=c(1.2,1.8), col=c('black',farger[3]),
             legend=c('2014', '2015'), ncol = dim(andeler)[2])}

  } else {
    mtext( c(N[,1], 2013), side=4, line=2.5, las=1, at=c(ypos, max(ypos)+diff(ypos)[1]), col=1, cex=cexgr, adj = 1)
    mtext( c(N[,2], 2014), side=4, line=5.5, las=1, at=c(ypos, max(ypos)+diff(ypos)[1]), col=1, cex=cexgr, adj = 1)
    mtext( c(N[,3], 2015), side=4, line=8.5, las=1, at=c(ypos, max(ypos)+diff(ypos)[1]), col=1, cex=cexgr, adj = 1)
    mtext( 'N', side=4, line=5.5, las=1, at=max(ypos)+2*diff(ypos)[1], col=1, cex=cexgr, adj = 1)
    par(xpd=TRUE)
    points(y=ypos, x=andeler[,1],cex=1.5)
    points(y=ypos, x=andeler[,2],cex=1.5,pch= 19)
    par(xpd=FALSE)
    if (legPlass=='nede') {
       legend('bottomright', cex=1.2, bty='n',
             lwd=c(NA,NA,NA), pch=c(1,19,15), pt.cex=c(1,1,2), col=c('black','black',farger[3]),
             legend=c('2013','2014', '2015'), ncol = 1)}
    if (legPlass=='top'){
         legend(0, yposOver+ diff(ypos)[1], yjust=0, xpd=TRUE, cex=0.9, bty='n', #bg='white', box.col='white',y=max(ypos),
               lwd=c(NA,NA,NA), pch=c(1,19,15), pt.cex=c(1.2,1.2,1.8), col=c('black', 'black',farger[3]),
               legend=c('2013', '2014', '2015'), ncol = dim(andeler)[2])}
  }

  text(x=0, y=ypos, labels = pst_txt, cex=0.9, pos=4)
  mtext(sideTxt, WEST<-2, line=0.4, cex=cexgr, col="black", outer=TRUE)

  par('mar'= oldpar_mar)
  par('fig'= oldpar_fig)
  par('oma'= oldpar_oma)

  if (outfile != '') {savePlot(outfile, type=substr(outfile, nchar(outfile)-2, nchar(outfile)))}

}
