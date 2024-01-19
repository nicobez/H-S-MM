##############
# Loading adds on functions
##############

################
funcAcf <- function(x,code,i.state){
  # this function computes the covariance for 1D processes with gaps (NA)
  # Output : the covariance + the number of obs available for the computation
  # pairs are selected if :
  #   the obs belong to the same code.trajectory
  #   the obs belong to the same sequence i.e. bit between two series of NA
  #   the two obs are not NA
  # x is a vector alternating data and NA
  xx <- !is.na(x) # logical ; T for active data ; F for inactive
  tmp <- rle(xx) 
  seq.nb <- length(tmp$lengths) # nb of sequence of i.state and non-i.state
  seq.id <- rep(1:seq.nb,tmp$lengths) # vector of seq number ; same size than x
  seq.size.i.state <- tmp$lengths[tmp$values] # keeps only the sequences associated to T, i.e. not NA values
  tmp <- table(seq.size.i.state)
  tmp2 <- as.numeric(attributes(tmp)$dimnames$seq.size.i.state)
  # lag.max <- max(tmp2[tmp>1]) # largest sequence observed at least twice ; above this max, computation concerns data from different sequences
  # correction 2023/11/024
  lag.max <- max(tmp2)-2 # largest sequence observed at least twice ; above this max, computation concerns data from different sequences
  n <- length(x)
  res.cov <- NULL #rep(NA,lag.max+1) # "+1" to get h=0 in the output
  res.n <- res.cov
  condition <- T ; i <- 0
  while(condition){
  #for(i in 0:lag.max){ #i <- 1 # i = 0 to get h=0 in the output
    sel <- seq.id[1:(n-i)]==seq.id[(1+i):n] & 
      code[1:(n-i)]==code[(1+i):n] &
      !is.na(x[1:(n-i)]*x[(1+i):n]) # size n-i 
    if(sum(sel)>=2){
    i.1 <- (1:(n-i))[sel] # index of active first points
    i.2 <- i.1+i # index of active second points (they are necessarily ok)
    res.cov[i+1] <- mean(x[i.1]*x[i.2])-mean(x[i.1])*mean(x[i.2])
    res.n[i+1] <- sum(sel)
    i <- i+1} else condition <- F
  }
  list(cov=res.cov,n=res.n)
}

################
funcLogLikelihood <- function( s, Z,type="poisson"){
  # (marginal) Log-Likelyhood of shifted data for Poisson or NegBinom 
  # If the variance and the mean of the shifted data are not consistent with a NegBinom, the LL is -Inf
  N    <- length(Z)
  Z <- Z-s
  Zm <- mean(Z)
  Zvar <- var(Z)
  if(type=="poisson")
    res <- N  * (Zm) * ( 1-log(Zm)) - sum(log(gamma(Z+1)))
  if(type=="nbinom"){
    n <- Zm^2/(Zvar-Zm)
    p <- Zm/Zvar
    res <- -Inf
    if(Zvar > Zm) {
      N0 <- sum(Z==0)
      res <- N0 + sum(log(1/(Z[Z>0]*beta(n,Z[Z>0]))))+N*n*log(p)+log(1-p)*N*Zm
    }
  }
  res   
}

################
funcShiftMle <- function(Z,type="poisson"){
  # ML estimation of the shift of Poisson or NegBinom 
  res <- which.max(sapply(0:min(Z), function(s_){funclvrais_marginale(s_,Z,type=type) }))
  (0:min(Z))[res]
}

################
funcNp2mv <- function(n,p){
  # transforms the (n,p)=(size,prob) values of a NegBinom into the corresponding (mean,variance)
  m <- n*(1-p)/p
  v <- n*(1-p)/p^2
  c(m,v)
}

################
funcMv2np <- function(m,v){
  # transforms the (mean,variance) values of a NegBinom into the corresponding (n,p)=(size,prob) 
  n <- m^2/(v-m)
  p <- m/v
  c(n,p)
}

################
funcHellingerGeomSNB <- function(m,v,shift){
  x <- 0:floor(10*m)
  p <- 1/m ; q <- 1-p
  Gi <- q^x*p
  #Gi <- dgeom(x,prob=1/(1+m)) # WARNING rgeom(x,p) generates 0 ! and mean = 1/p
  tmp <- funcMv2np(m-shift,v)
  NBi <- c(rep(0,shift),dnbinom(x[1:(length(x)-shift)],size=tmp[1],prob=tmp[2]))
  sqrt(1-sum(sqrt(Gi*NBi)))
}

################
funcSseNB <- function(v,x){
  # sum of square errors between empirical proportions and nbinom pdf of parameters v=c(size,prob)  
  #sum((dnbinom(x[,1],size=v[1],prob=v[2])-x[,2])^2)
  if (v[2] <0) cat(v,"\n")
  sum((pnbinom(x[,1],size=v[1],prob=v[2])-cumsum(x[,2]))^2)
  
}
################
funcSseNB2 <- function(v,x){
  # sum of square errors between empirical proportions and nbinom pdf of parameters v=c(size,prob)  
  #sum((dnbinom(x[,1],size=v[1],prob=v[2])-x[,2])^2)
  sum((pnbinom(x[,1]-v[3],size=v[1],prob=v[2])-cumsum(x[,2]))^2)
}

################
funcEstNB <- function(d){
  # Inference of shifted.nbinom parameters by least squares.
  # For each possible shift value, SSE is computed and the least one is selected.
  # The optim requires initial values. To avoid problems for nbinom when the empiriacl mean is larger than the variance,
  # initial values are rather set to :
  #   size = n ~ m²/var instead of m²/(var-m) that can be < 0 when var < m
  #   prob = p = n/(m+n) that corresponds to the theoretical values that naturally lies in [0,1]
  # Input : d is the vector of the sojourn times
  d1 <- sort(unique(d))
  p1 <- sapply(d1, function(i){sum(d==i)})/length(d)
  tmp <- which.min(sapply(0:min(d1), function(s){optim(c((mean(d)-s)^2/funcvar(d),1/(1+funcVar(d)/(mean(d)-s))),funcSseNB,x=cbind(d1-s,p1),lower=c(0,0),upper=c(500,1),method="L-BFGS-B")$value}))
  s <- (0:min(d1))[tmp]
  res <- optim(c((mean(d)-s)^2/funcVar(d),1/(1+funcVar(d)/(mean(d)-s))),
               funcSseNB,x=cbind(d1-s,p1))$par
  list(size=res[1],prob=res[2],shift=s)
}

################
funcSsePois <- function(v,x){
  # sum of square errors between empirical proportions and poisson pdf of parameters v=lambda  
  #sum((dpois(x[,1],lambda=v[1])-x[,2])^2)
  sum((ppois(x[,1],lambda=v[1])-cumsum(x[,2]))^2)
  
}

################
funcEstPois <- function(d){
  # Inference of shifted.nbinom parameters by least squares.
  # For each possible shift value, SSE is computed and the least one is selected.
  # The optim is initialised to the empirical mean.
  # Input : d is the vector of the sojourn times
  d1 <- sort(unique(d))
  p1 <- sapply(d1, function(i){sum(d==i)})/length(d)
  tmp <- which.min(sapply(0:min(d1), function(s){optim(mean(d)-s,funcSsePois,x=cbind(d1-s,p1),method="Brent",lower=0,upper=mean(d))$value}))
  s <- (0:min(d1))[tmp]
  res <- optim(mean(d)-s,funcSsePois,x=cbind(d1-s,p1),method="Brent",lower=0,upper=mean(d))$par
  list(lambda=res[1],shift=s)
}

################
funcVar <- function(x,na.rm=T){
  # Computing proper empirical variance (the R function var() provides an estimated variance in 1/(n-1))
  mean(x^2,na.rm=na.rm)-mean(x,na.rm=na.rm)^2
}

################
funcCov <- function(x,y,na.rm=T){
  # Computing proper empirical covariance (the R function cov() provides an estimated covariance in 1/(n-1))
  mean(x*y,na.rm=na.rm)-mean(x,na.rm=na.rm)*mean(y,na.rm=na.rm)
}

################
funcAccuracy <- function(M) {
  # Accuracy associated to a confusion matrix
  (M[1,1]+M[2,2])/sum(M)
}

################
funcOdd <- function(x) {
  # Provides TRUE if x is odd
  x%%2 == 1
}

################
funcLast <- function(x){
  x[length(x)]
}

################
funcTau <- function(x){
  # Overlapping of speeds' Gaussian pdfs between states (one value per speed variable) 
  # x is an array with dim= 4*5*2 of which we use [1:2,1:2,]
  res <- c(NA,NA)
  for(j in 1:2){
    m1 <- x[j,1,1]
    s1 <- sqrt(x[j,2,1])
    m2 <- x[j,1,2]
    s2 <- sqrt(x[j,2,2])
    #x.inter <- (m1*s2+m2*s1)/(s1+s2)
    b <- 2*(m2*s1^2 - m1*s2^2)
    a <- s2^2-s1^2
    c <- m1^2*s2^2-m2^2*s1^2-2*s1^2*s2^2*(log(s2)-log(s1))
    delta <- b^2 -4*a*c
    x1 <- (-b+sqrt(delta))/(2*a)
    x2 <- (-b-sqrt(delta))/(2*a)
    if(m1==m2){
      temp1 <- sort(c(x1,x2))
      x1 <- temp1[1] ; x2 <- temp1[2] # make sure x1 < x2
      if(s2 > s1){
        tau <- pnorm(x2,m2,s2)-pnorm(x1,m2,s2)+(1-pnorm(x2,m1,s1)+pnorm(x1,m1,s1))
      } else {
        tau <- pnorm(x2,m1,s1)-pnorm(x1,m1,s1)+(1-pnorm(x2,m2,s2)+pnorm(x1,m2,s2))
      }
    }
    if(m1<m2){
      x.inter <- (x1 > m1 & x1 < m2)*x1 + (x2 > m1 & x2 < m2)*x2 
      tau <- 1-pnorm(x.inter,mean=m1,sd=s1)+pnorm(x.inter,mean=m2,sd=s2)
    } else {
      x.inter <- (x1 > m2 & x1 < m1)*x1 + (x2 > m2 & x2 < m1)*x2
      tau <- 1-pnorm(x.inter,mean=m2,sd=s2)+pnorm(x.inter,mean=m1,sd=s1)
    }
    res[j] <- tau
  } 
  res
}

################
funcCurvedarrow <- function(from, to, lwd=2, lty=1, lcol="black", arr.col=lcol, 
                          arr.pos=0.5, curve=1, dr=0.01, endhead=FALSE, segment = c(0,1), ...)   {
  # copy from diagram package
  dpos  <- to-from
  angle <- atan(dpos[2]/dpos[1])*180/pi         # angle between both
  if (is.nan(angle)) return
  mid   <- 0.5*(to+from)                        # midpoint of ellipsoid arrow
  dst   <- dist(rbind(to, from))                # distance from-to
  ry    <- curve*dst                            # small radius of ellepsoid
  aFrom<-0                                      # angle to and from
  aTo<-pi
  if ( from[1] <= to[1]) {
    aFrom <- pi
    aTo <- 2*pi
  }
  
  if (segment [1] != 0)
    From <- segment[1] * aTo + (1-segment[1]) * aFrom
  else
    From <- aFrom
  
  if (segment [2] != 1)
    To <- segment[2] * aTo + (1-segment[2]) * aFrom
  else
    To <- aTo
  
  meanpi <- arr.pos * aTo + (1-arr.pos) * aFrom
  if (endhead) To <- meanpi
  
  
  plotellipse(rx=dst/2,  ry=ry, mid=mid, angle=angle, from = From, to = To,
              lwd=lwd, lty=lty, lcol=lcol)
  ell <- getellipse(rx=dst/2, ry=ry, mid=mid, angle=angle,
                    from=1.001*meanpi, to=0.999*meanpi, dr= 0.002)       #Changed from -0.002
  Arrows(ell[1,1], ell[1,2], ell[nrow(ell),1], ell[nrow(ell),2],
         code=1, lcol=lcol, arr.col=arr.col, ...)
  curvedarrow <- c(ell[nrow(ell),1], ell[nrow(ell),2])
}


################
funcUpperHalfCircle <- function(x,y,r=0.01,nsteps=1000,...){  
  temp <- par('usr')
  rx <- (temp[2]-temp[1])*r
  ry <- (temp[4]-temp[3])*r
  rs <- seq(0,pi,len=nsteps) 
  for(i in 1:length(x)){
    xc <- x[i]+rx*cos(rs) 
    yc <- y[i]+ry*sin(rs) 
    polygon(xc,yc,...) 
  }
} 

################
funcLowerHalfCircle <- function(x,y,r=0.01,nsteps=1000,...){ 
  temp <- par('usr')
  rx <- (temp[2]-temp[1])*r
  ry <- (temp[4]-temp[3])*r
  rs <- seq(0,pi,len=nsteps) 
  for(i in 1:length(x)){
    xc <- x[i]-rx*cos(rs) 
    yc <- y[i]-ry*sin(rs) 
    polygon(xc,yc,...)
  }
} 

################
funcCircle <- function(x,y,r=0.01,nsteps=1000,...){ 
  temp <- par('usr')
  rx <- (temp[2]-temp[1])*r
  ry <- (temp[4]-temp[3])*r
  rs <- seq(0,2*pi,len=nsteps) 
  for(i in 1:length(x)){
    xc <- x[i]-rx*cos(rs) 
    yc <- y[i]-ry*sin(rs) 
    polygon(xc,yc,...)
  }
} 

