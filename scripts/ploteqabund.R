.getColor <- function(col,trans){
  
  # trans - transparency fraction [0, 1]
  
  tmp <- col2rgb(col)
  rgb(tmp[1,], tmp[2,], tmp[3,], maxColorValue = 255, 
      alpha = 255*trans, names = paste(col,trans,sep='_'))
}

.getPlotLayout <- function(np){
  
  # np - no. plots
  
  if(np == 1)return( c(1,1) )
  if(np == 2)return( c(1,2) )
  if(np == 3)return( c(1,3) )
  if(np <= 4)return( c(2,2) )
  if(np <= 6)return( c(2,3) )
  if(np <= 9)return( c(3,3) )
  if(np <= 12)return( c(3,4) )
  if(np <= 16)return( c(4,4) )
  if(np <= 20)return( c(4,5) )
  if(np <= 25)return( c(5,5) )
  if(np <= 25)return( c(5,6) )
  return( c(6,6) )
}

#dotted line=mean, dark shade=68.2%, light shade=95% CI

.shadeInterval <- function(xvalues,loHi,col='grey',PLOT  = TRUE, add  = TRUE,
                           xlab=' ',ylab=' ', xlim = NULL, ylim = NULL, 
                           LOG = FALSE, trans = .5){
  
  tmp <- smooth.na(xvalues,loHi)
  
  xvalues <- tmp[,1]
  loHi <- tmp[,-1]
  
  xbound <- c(xvalues,rev(xvalues))
  ybound <- c(loHi[,1],rev(loHi[,2]))
  if(is.null(ylim))ylim <- range(as.numeric(loHi))
  if(is.null(xlim))xlim <- range(xvalues)
  
  if(!add){
    if(!LOG)plot(NULL, xlim = xlim, ylim=ylim, 
                 xlab=xlab, ylab=ylab)
    if(LOG)suppressWarnings( plot(NULL,  xlim = xlim, ylim=ylim, 
                                  xlab=xlab, ylab=ylab, log='y') )
  }
  if(PLOT)polygon(xbound,ybound, border=NA,col=.getColor(col, trans))
  
  invisible(cbind(xbound,ybound))
  
}
.outFile <- function(outFolder=character(0),file){
  paste(outFolder,file,sep='/')
}

.plotLabel <- function(label,location='topleft',cex=1.3,font=1,
                       above=F,below=F,bg=NULL){
  
  if(above){
    adj <- 0
    if(location == 'topright')adj=1
    title(label,adj=adj, font.main = font, font.lab =font)
    return()
  }
  if(below){
    adj <- 0
    if(location == 'bottomright')adj=1
    mtext(label,side=1,adj=adj, outer=F,font.main = font, font.lab =font,cex=cex)
    return()
  }
  
  if(is.null(bg)){
    tmp <- legend(location,legend=' ',bty='n')
  } else {
    tmp <- legend(location,legend=label,bg=bg,border=bg,text.col=bg,bty='o')
  }
  
  xt <- tmp$rect$left # + tmp$rect$w
  yt <- tmp$text$y
  
  pos <- 4
  tmp <- grep('right',location)
  if(length(tmp) > 0)pos <- 2
  
  XX <- par()$xlog
  YY <- par()$ylog
  
  if(XX)xt <- 10^xt
  if(YY)yt <- 10^yt
  
  text(xt,yt,label,cex=cex,font=font,pos=pos)
}

.getBin <- function(xx, yy, minbin = 5, length = 15){
  
  # aggregates feathery tails of distribution
  
  xi  <- xr  <- seq(min(xx, na.rm=T), max(xx, na.rm=T), length = length)
  xc  <- findInterval(xx, xi, all.inside = T)
  tab <- table(xc)
  
  while( tab[1] < (minbin/2) ){
    xi <- xi[-1]
    xc  <- findInterval(xx, xi, all.inside = T)
    tab <- table(xc)
  }
  nt <- length(tab)
  nx <- length(xi)
  while( tab[nt] < (minbin/2) ){
    xi <- xi[-nx]
    xc  <- findInterval(xx, xi, all.inside = T)
    tab <- table(xc)
    nx <- length(xi)
    nt <- length(tab)
  }
  
  # interior
  while(min(tab) < minbin){
    length <- length - 1
    xi  <- seq(min(xi, na.rm=T), max(xi, na.rm=T), length = length)
    xc  <- findInterval(xx, xi, all.inside = T)
    tab <- table(xc)
    nx <- length(xi)
    nt <- length(tab)
  }
  xi[1]  <- xr[1]
  xi[nx] <- xr[length(xr)]
  
  xmids <- (xi[1:(nx-1)] + xi[2:nx])/2
  
  list(xseq = xi, xmids = xmids, xbin = xc)
}

smooth.na <- function(x,y){   
  
  #remove missing values
  #x is the index
  #y is a matrix with rows indexed by x
  
  if(!is.matrix(y))y <- matrix(y,ncol=1)
  
  wy <- which(!is.finite(y),arr.ind   = TRUE)
  if(length(wy) == 0)return(cbind(x,y))
  wy <- unique(wy[,1])
  ynew <- y[-wy,]
  xnew <- x[-wy]
  
  cbind(xnew,ynew)
}


  outFolder=outFolder
  ccMu <- wstar$ccMu#[,notOther]
  ccSd <- wstar$ccSd#[,notOther]
  ccx  <- wstar$x
  ccx  <- ccx[, !colnames(ccx) %in% attributes(ccx)$factors, drop=F]
  ccx  <- ccx[,-1, drop=F]
  
  np <- ncol(ccMu)
  
  npage <- 1
  o   <- 1:np
  if(np > 16){
    npage <- ceiling(np/16)
    np    <- 16
  }
  
  mfrow <- .getPlotLayout(np)
  
  nbin <- 12
  ylimit <- c(0, max(ccMu) )
  
 
  for(m in 1:ncol(ccx)){   # loop over predictors
    
    xm  <- colnames(ccx)[m]
    xx  <- ccx[,m]
    atx <- quantile(xx,seq(0, 1, length=nbin))
    
    xlimit <- c( sum( c(.7, .3)*atx[1:2] ), sum( c(.3, .7)*atx[(nbin-1):nbin] ) )
    
    k   <- 0
    add <- F
    o   <- 1:np
    o   <- o[o <= 16]
    
    for(p in 1:npage){
      
      file <- paste('equilAbund_', xm, '_', p,'.pdf',sep='')
      
      #if(SAVEPLOTS)
        pdf( file=.outFile(outFolder,file) )
      
      npp <- ncol(ccMu) - k
      if(npp > np)npp <- np
      mfrow <- .getPlotLayout(np)
      par(mfrow=mfrow, bty='n', omi=c(.3,.3,0,0), mar=c(3,3,2,1)) #, 
#          tcl= tcl, mgp=mgp)
      
      for(j in o){
        
        yy  <- ccMu[,j]
        
        minbin <- 5
        xmids  <- 0
        while( length(xmids) == 1){
          minbin <- minbin - 1
          tt  <- .getBin(xx, yy, minbin = minbin)
          xbin <- tt$xbin
          xmids <- tt$xmids
        }
        
        c95 <- tapply( yy, list(bin = xbin), quantile, pnorm( c(-1.96, -1, 1, 1.96) ))
        ci <- matrix( unlist( c95 ), ncol = 4, byrow = T )
        rownames(ci) <- names(c95)
        
        xmids <- xmids[ as.numeric(names(c95)) ]
        
        .shadeInterval( xmids, loHi = ci[,c(1, 4)],col=specColor[j],PLOT=T,add=F,
                        xlab=' ',ylab=' ', xlim = xlimit,  
                        LOG=F, trans = .3)
        .shadeInterval( xmids, loHi = ci[,c(2, 3)],col=specColor[j], PLOT=T, add=T, 
                        trans = .3)
        mu <- tapply( yy, xbin, mean )
        lines(xmids,  mu, lty=2, lwd=2, col = specColor[j])
        
        
        k <- k + 1
        if(k > 26)k <- 1
        
        lab <- colnames(ccMu)[j]
        
        .plotLabel( lab,above=T )
      }
      mtext(xm, 1, outer=T)
      mtext('Equilibrium abundance', 2, outer=T)
      
      #if(!SAVEPLOTS){
        readline('equilibrium abundance -- return to continue ')
      #} else {
        dev.off()
  #    }
   #   o <- o + 16
  #    o <- o[o <= SO]
    }
  }
#}
