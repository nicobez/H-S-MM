##########################################
# Proportions in state 1 & 2
##########################################
piModel <- array(NA,dim=c(nVessel,4,2),dimnames=list(listVessel,c("d1","d2","d3","d4"),c("State 1","State 2")))
for(i.v in 1:nVessel){
  for(i.state in 1:2){
    for(i.d in 1:4) piModel[i.v,i.d,i.state] <- parameters[i.v,i.d,3,1,i.state]/sum(parameters[i.v,i.d,3,1,])
  }
}

rm(i.v,i.state,i.d)

##########################################
# Accuracy
##########################################
for(i.method in 1:2){
  for(i.AR in 1:2){
    for(i.size in 1:2){ #c(50,250)){
      # i.method <- 1 ; i.AR <- 1 
      method <- listMethod[i.method]
      AR <- listAR[i.AR]
      size <- listSize[i.size]
      temp1 <- get(paste0("confusion",size,method,AR))
      temp2 <- apply(temp1,1:5,"funcAccuracy")
      assign(paste0("accuracy",size,method,AR),apply(temp2,2:5,"mean",na.rm=T))
      assign(paste0("accuracyQ25",size,method,AR),apply(temp2,2:5,"quantile",probs=0.25,na.rm=T))
      assign(paste0("accuracyQ75",size,method,AR),apply(temp2,2:5,"quantile",probs=0.75,na.rm=T))
      assign(paste0("accuracyQ10",size,method,AR),apply(temp2,2:5,"quantile",probs=0.10,na.rm=T))
      assign(paste0("accuracyQ90",size,method,AR),apply(temp2,2:5,"quantile",probs=0.90,na.rm=T))
      }
  }
}

rm(i.method,i.AR,i.size,method,AR,size,temp1,temp2)

##########################################
# Combining output into a single object where the diagonal corresponds to MSA and extra diagonal to MRA
##########################################
temp <- array(dim=c(nVessel,4,4,4),
            dimnames=list(listVessel,
                          c("d1","d2","d3","d4"),
                          c("HMM x AR0","HMM x AR1","HSMM x AR0","HSMM x AR1"),
                          c("HMM x AR0","HMM x AR1","HSMM x AR0","HSMM x AR1")))
accuracy250 <- temp
lossAccuracy250 <- temp
accuracy50 <- temp

i.method <- c(1,1,2,2) ; i.AR <- c(1,2,1,2)
for(i in 1:4){
  method.ref <- listMethod[i.method[i]]
  AR.ref <- listAR[i.AR[i]] 
  m.250 <- get(paste0("accuracy",250,method.ref,AR.ref))
  m.50 <- get(paste0("accuracy",50,method.ref,AR.ref))
  for(j in 1:4){
    method <- listMethod[i.method[j]]
    AR <- listAR[i.AR[j]]
    accuracy250[,,i,j] <- m.250[,1:4,i.method[j],i.AR[j]]
    lossAccuracy250[,,i,j] <- 100*(m.250[,1:4,i.method[j],i.AR[j]]/m.250[,1:4,i.method[i],i.AR[i]]-1)
    accuracy50[,,i,j] <- m.50[,1:4,i.method[j],i.AR[j]]
  }
}
rm(temp,i,j,m.250,m.50,method.ref,AR.ref,method,AR,i.method,i.AR)


##########################################
# Theoretical accuracy without hypothesis about the state process, that is under mixed model
##########################################
dVModel <- array(NA,dim=c(nVessel,4,3),dimnames=list(listVessel,c("d1","d2","d3","d4"),c("Vp","Vr","mean")))
delta <- 0.1
x <- seq(-20,20,delta)
y <- seq(-20,20,delta)
grid <- expand.grid(x,y)

for(i.v in 1:nVessel){
  for(i.d in 1:4){
    tmp <- parameters[i.v,i.d,,,]
    for(i.var in 1:2){
      p1 <- dnorm(x=x,mean=tmp[i.var,1,1],sd=sqrt(tmp[i.var,2,1]))
      p2 <- dnorm(x=x,mean=tmp[i.var,1,2],sd=sqrt(tmp[i.var,2,2]))
      dVModel[i.v,i.d,i.var] <- piModel[i.v,i.d,1]*delta*sum(sapply(1:length(x), function(i) ifelse(p1[i]>p2[i],p1[i],0))) +
        piModel[i.v,i.d,2]*delta*sum(sapply(1:length(x), function(i) ifelse(p2[i]>p1[i],p2[i],0)))
    }
    p1 <- dmvnorm(x=grid,mean=parameters[i.v,i.d,1:2,1,1],sigma=diag(parameters[i.v,i.d,1:2,2,1]))
    p2 <- dmvnorm(x=grid,mean=parameters[i.v,i.d,1:2,1,2],sigma=diag(parameters[i.v,i.d,1:2,2,2]))
    p1Cut <- p1
    p2Cut <- p2
    p1Cut[p1<p2] <- 0 ; p2Cut[p2<p1] <- 0
    dVModel[i.v,i.d,3] <- piModel[i.v,i.d,1]*delta^2*sum(p1Cut) +
      piModel[i.v,i.d,2]*delta^2*sum(p2Cut)
  }
}

rm(delta,x,y,grid,i.v,i.d,i.var,tmp,p1,p2,p1Cut,p2Cut)

##########################################
# Empirical accuracy without hypothesis about the state process 
##########################################
dVEmpirical <- array(dim=c(2,4,3),dimnames=list(listVessel,c("d1","d2","d3","d4"),c("Vp","Vr","mean")))
delta <- 0.5
for(i.vessel in 1:2){
  for(i.degrad in 1:4){ # i.vessel = 1 ; i.degrad = 4
    pi1 <- piModel[i.vessel,i.degrad,1]
    pi2 <- piModel[i.vessel,i.degrad,2]
    data <- get(paste0("vessel",i.vessel,"deg",i.degrad))
    breaks <- seq(floor(min(c(data$vp,data$vr))),ceiling(max(c(data$vp,data$vr))),delta)
    vp1 <- cut(data$vp[data$state==1],breaks=breaks)
    vr1<- cut(data$vr[data$state==1],breaks=breaks)
    vp2 <- cut(data$vp[data$state==2],breaks=breaks)
    vr2<- cut(data$vr[data$state==2],breaks=breaks)
    #
    pp1 <- proportions(table(vp1)) ; pp2 <- proportions(table(vp2))
    pr1 <- proportions(table(vr1)) ; pr2 <- proportions(table(vr2))
    p1 <- proportions(table(vp1,vr1))
    p2 <- proportions(table(vp2,vr2))
    #
    dVEmpirical[i.vessel,i.degrad,1] <- pi1 * sum(sapply(1:length(pp1),function(i) ifelse(pp1[i]>pp2[i],pp1[i],0))) +
      pi2 * sum(sapply(1:length(pp2), function(i) ifelse(pp2[i]>pp1[i],pp2[i],0)))  
    #
    dVEmpirical[i.vessel,i.degrad,2] <- pi1 * sum(sapply(1:length(pr1),function(i) ifelse(pr1[i]>pr2[i],pr1[i],0))) +
      pi2 * sum(sapply(1:length(pr2), function(i) ifelse(pr2[i]>pr1[i],pr2[i],0)))
    #
    dVEmpirical[i.vessel,i.degrad,3] <- pi1 * sum(sapply(1:length(p1),function(i) ifelse(p1[i]>p2[i],p1[i],0))) +
      pi2 * sum(sapply(1:length(p2), function(i) ifelse(p2[i]>p1[i],p2[i],0)))
  }
}
rm(delta,i.vessel,i.degrad,data,breaks,vp1,vp2,vr1,vr2,pp1,pp2,pr1,pr2,p1,p2,pi1,pi2)

##########################################
# Hellinger distances between geometric and shifted NB pdfs
##########################################
hellingerDist <- array(NA,dim=c(nVessel,4,3),
                      dimnames=list(listVessel,c("d1","d2","d3","d4"),c("state 1","state 2","mean")))
  
for(i.vessel in 1:nVessel){
  for(i.state in 1:2){
    for(i.degrad in 1:4) hellingerDist[i.vessel,i.degrad,i.state] <- funcHellingerGeomSNB(parameters[i.vessel,i.degrad,3,1,i.state],
                                                                    parameters[i.vessel,i.degrad,3,2,i.state],
                                                                    parameters[i.vessel,i.degrad,3,5,i.state])
  }
}

for(i.vessel in 1:nVessel){
    for(i.degrad in 1:4) {
      pi1 <- piModel[i.vessel,i.degrad,1]
      pi2 <- piModel[i.vessel,i.degrad,2]
      hellingerDist[i.vessel,i.degrad,3] <- pi1*hellingerDist[i.vessel,i.degrad,1] + pi2*hellingerDist[i.vessel,i.degrad,2]
  }
}

rm(i.vessel,i.state,i.degrad,pi1,pi2)

##########################################
# Hellinger distances between empirical frequencies and shifted NB pdfs per state and for the two states combined
##########################################
hellingerDistPropSNB <- array(NA,dim=c(nVessel,4,3),
                              dimnames=list(listVessel,c("d1","d2","d3","d4"),c("state 1","state 2","mean")))
for(i.vessel in 1:2){
  for(i.degrad in 1:4){ # i.vessel = 1 ; i.degrad = 4
    pi1 <- piModel[i.vessel,i.degrad,1]
    pi2 <- piModel[i.vessel,i.degrad,2]
    #
    data <- get(paste0("vessel",i.vessel,"deg",i.degrad))
    temp <- rle(data$state)
    breaks <- seq(0,2*max(temp$lengths),1)
    for(i.state in 1:2){ #i.state=2
      temp1 <- temp$lengths[temp$values==i.state] 
      p <- sapply(breaks, function(i) sum(temp1==i))/length(temp1)
      #
      m <- parameters[i.vessel,i.degrad,3,1,i.state]
      v <- parameters[i.vessel,i.degrad,3,2,i.state]
      shift <- parameters[i.vessel,i.degrad,3,5,i.state]
      temp2 <- funcMv2np(m-shift,v)
      NB <- c(rep(0,shift),dnbinom(breaks[1:(length(breaks)-shift)],size=temp2[1],prob=temp2[2]))
      hellingerDistPropSNB[i.vessel,i.degrad,i.state] <- sqrt(1-sum(sqrt(p*NB)))
    }
  }
}
for(i.vessel in 1:nVessel){
  for(i.degrad in 1:4) {
    pi1 <- piModel[i.vessel,i.degrad,1]
    pi2 <- piModel[i.vessel,i.degrad,2]
    hellingerDistPropSNB[i.vessel,i.degrad,3] <- pi1*hellingerDistPropSNB[i.vessel,i.degrad,1] + pi2*hellingerDistPropSNB[i.vessel,i.degrad,2]
  }
}

rm(pi1,pi2,temp2,shift,v,m,p,temp1,breaks,temp,data,i.vessel,i.degrad,i.state,NB)

##########################################
# Hellinger distances between empirical frequencies and geometric pdfs per state and for the two states combined
##########################################
hellingerDistPropGeom <- array(NA,dim=c(nVessel,4,3),
                              dimnames=list(listVessel,c("d1","d2","d3","d4"),
                                            c("state 1","state 2","mean")))
for(i.vessel in 1:2){
  for(i.degrad in 1:4){ # i.vessel = 1 ; i.degrad = 4
    pi1 <- piModel[i.vessel,i.degrad,1]
    pi2 <- piModel[i.vessel,i.degrad,2]
    #
    data <- get(paste0("vessel",i.vessel,"deg",i.degrad))
    temp <- rle(data$state)
    breaks <- seq(0,2*max(temp$lengths),1)
    for(i.state in 1:2){ #i.state=2
      temp1 <- temp$lengths[temp$values==i.state] 
      p <- sapply(breaks, function(i) sum(temp1==i))/length(temp1)
      m <- parameters[i.vessel,i.degrad,3,1,i.state]
      Geom <- dgeom(breaks,1-1/m)
      hellingerDistPropGeom[i.vessel,i.degrad,i.state] <- sqrt(1-sum(sqrt(p*Geom)))
    }
  }
}
for(i.vessel in 1:nVessel){
  for(i.degrad in 1:4) {
    pi1 <- piModel[i.vessel,i.degrad,1]
    pi2 <- piModel[i.vessel,i.degrad,2]
    hellingerDistPropGeom[i.vessel,i.degrad,3] <- pi1*hellingerDistPropGeom[i.vessel,i.degrad,1] 
    + pi2*hellingerDistPropGeom[i.vessel,i.degrad,2]
  }
}

rm(pi1,pi2,m,p,temp1,breaks,temp,data,i.vessel,i.degrad,i.state,Geom)

##########################################
# Estimating the order of the AR process of the empirical speed variables
##########################################
orderAR <- array(dim=c(2,4,2,2),dimnames=list(listVessel,c("d1","d2","d3","d4"),c("Vp","Vr"),c("State1","State2")))

nlag=7
for(i.vessel in 1:2){
  for(i.degrad in 1:4){ #i.vessel = 1 ; i.degrad = 1
    vessel <- get(paste0("vessel",i.vessel,"deg",i.degrad))
    for(i.var in 1:2){ #i.var = 1
      if(i.var==1) y <- vessel$vp
      if(i.var==2) y <- vessel$vr
      state <- vessel$state
      code <- vessel$code.traj
      for(i.state in 1:2){ #i.state = 1
        yy <- y
        yy[vessel$state!=i.state] <- NA
        n <- sum(!is.na(yy))
        yyy <- !is.na(yy)
        res.cov <- funcAcf(yy,code,i.state)
        # les coef de correlation partielles sont estimés comme indiqué dans le cours de TS de V. Monbet
        # Les matrices de var-covar sont alimentées avec les cov empiriques et NON avec les valeurs de modele de cov
        res.r <- rep(NA,length(res.cov$cov)-1)
        for(i.k in 1:(length(res.cov$cov)-1)){ #i.k <- 2
          Cij <-matrix(nrow=i.k,ncol=i.k)
          Ci0 <- res.cov$cov[2:(i.k+1)] # c(C(1),...,C(k))
          for(i in 1:i.k){
            for(j in 1:i.k){
              Cij[i,j] <- res.cov$cov[abs(i-j)+1]
            }
          }
          res.r[i.k] <- (solve(Cij)%*%Ci0)[i.k]
        }
        tmp <- rle(yyy)
        seq.size <- tmp$lengths[tmp$values] # one keep only the sequence assciated to T, i.e. not NA values
        seq.size <- table(seq.size)
        sequence.size <- as.numeric(attributes(seq.size)$dimnames$seq.size) # available sequence size
        sequence.nb <- as.vector(seq.size) # nb of sequences of each available size
        test.n <- rev(cumsum(rev(sequence.nb)))
        test.n[1] <- n # nb of sequence of a given size and above
        # the first value corresponds to sequence of at least one observation
        #  and is thus equal to the number of observations and not to the nb of sequences
        test.n <- rep(test.n,c(1,diff(sequence.size))) # completion based on the fact that if no sequence of size 8 exists, the larger sequences can be used
        rank <- 0:(length(test.n)-1)
        #tmp <- pacf(yy,lag.max=nlag+1,plot=F,na.action=na.pass)
        # A sequence of N obs allows fitting an AR(N-1)
        nlag <- max(rank)
        res <- pacf(yy,lag.max=nlag,plot=F,na.action=na.pass)
        
        xplot=rank[-1]
        yplot=as.vector(res$acf) # pacf does not provides output for distance 0 ; starts at 1 for AR1 based on 2 consecutive data
        
        qf.ref <- qf(0.9,1,test.n[-1]-rank[-1]-2) # quantile 90% de la distribution de Fisher n-k-2
        r.ref <- sqrt(qf.ref/(test.n[-1]-2+qf.ref-rank[-1])) # valeur de rho associée
        sel <- abs(yplot) > r.ref # detection de l'appartenance à l'intervalle de confiance à 90% sous H0
        # sel <- as.logical(cumprod(sel)) # mise à F le reste de la serie une foisle premier F rencontré
        res <- max(c(0,xplot)[c(T,sel)],na.rm=T) # rang de la première fois où on entre dans l'IC - 1
        orderAR[i.vessel,i.degrad,i.var,i.state] <- res
      }
    }
  }
}

rm(res,qf.ref,r.ref,sel,xplot,yplot,nlag,rank,test.n,sequence.nb,sequence.size,seq.size,
   tmp,res.r,Cij,Ci0,i.k,res.cov,i,j,y,yy,yyy,n,code,state,vessel,i.vessel,i.degrad,i.var,i.state)


##########################################
# Integral Ranges for the AR1 process
##########################################

integralRange <- array(NA,dim=c(nVessel,4,3,3),
                       dimnames=list(listVessel,c("d1","d2","d3","d4"),
                                     c("Vp","Vr","mean"),
                                     c("state 1","state 2","mean")))

for(i.vessel in 1:nVessel){
  for(i.degrad in 1:4) {
    for(i.var in 1:2){
      for(i.state in 1:2){
        ro <- parameters[i.vessel,i.degrad,i.var,3,i.state]
        integralRange[i.vessel,i.degrad,i.var,i.state] <- sum(ro^(0:20))
      }
    }
  }
}

for(i.vessel in 1:nVessel){
  for(i.degrad in 1:4) {
    integralRange[i.vessel,i.degrad,3,1] <- mean(integralRange[i.vessel,i.degrad,1:2,1])
    integralRange[i.vessel,i.degrad,3,2] <- mean(integralRange[i.vessel,i.degrad,1:2,2])
    pi1 <- piModel[i.vessel,i.degrad,1]
    pi2 <- piModel[i.vessel,i.degrad,2]
    integralRange[i.vessel,i.degrad,3,3] <- pi1*integralRange[i.vessel,i.degrad,3,1] + pi2*integralRange[i.vessel,i.degrad,3,2]
  }
}

rm(i.vessel,i.state,i.degrad,i.var,pi1,pi2,ro)
    
##########################################
# Integral Ranges for the AR1 process relative to the sojourn time duration
##########################################

temp <- array(NA,dim=c(nVessel,4,3,3),
                       dimnames=list(listVessel,c("d1","d2","d3","d4"),
                                     c("Vp","Vr","mean"),
                                     c("state 1","state 2","mean")))
for(i.vessel in 1:nVessel){
  for(i.degrad in 1:4) {
    for(i.var in 1:2){
      for(i.state in 1:2){
        ro <- parameters[i.vessel,i.degrad,i.var,3,i.state]
        temp[i.vessel,i.degrad,i.var,i.state] <- (sum(ro^(0:20))-1)/parameters[i.vessel,i.degrad,3,1,i.state]
      }
    }
  }
}

for(i.vessel in 1:nVessel){
  for(i.degrad in 1:4) {
    temp[i.vessel,i.degrad,3,1] <- mean(temp[i.vessel,i.degrad,1:2,1])
    temp[i.vessel,i.degrad,3,2] <- mean(temp[i.vessel,i.degrad,1:2,2])
    pi1 <- piModel[i.vessel,i.degrad,1]
    pi2 <- piModel[i.vessel,i.degrad,2]
    temp[i.vessel,i.degrad,3,3] <- pi1*temp[i.vessel,i.degrad,3,1] + pi2*temp[i.vessel,i.degrad,3,2]
  }
}

#integralRangeRelativeToDuration <- temp
arDist <- temp

rm(i.vessel,i.state,i.degrad,i.var,pi1,pi2,temp,ro)

