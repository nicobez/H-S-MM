orderAR <- array(dim=c(2,4,2,2),dimnames=list(listVessel,c("d1","d2","d3","d4"),c("Vp","Vr"),c("State1","State2")))

nlag=8
my.cex <- 1
plot(c(0,1.05*(nlag)),c(-0.15,1),type="n",xaxs="i",yaxs="i",las=1,
     xlab="order",ylab="Coefficient of partial autocorrelation")
abline(h=0)
for(i.vessel in 1:2){
  for(i.degrad in 1:4){ #i.vessel = 2 ; i.degrad = 1
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
        # partial correlation are estimated according to the pdf of V. Monbet p25
        # var-covar matrices are build from the empirical covariances and NOT from a covariance model
        res.r <- rep(NA,length(res.cov$cov)-1)
        for(i.k in 1:(length(res.cov$cov)-1)){ #i.k <- 2
          Cij <-matrix(nrow=i.k,ncol=i.k)
          Ci0 <- res.cov$cov[2:(i.k+1)] # c(C(1),...,C(k))
          for(i in 1:i.k){
            for(j in 1:i.k) Cij[i,j] <- res.cov$cov[abs(i-j)+1]
          }
          res.r[i.k] <- (solve(Cij)%*%Ci0)[i.k]
        }
        effectiveN <- res.cov$n[-1]
        rank <- 1:length(effectiveN)
        # selection of acceptable situations ie n-k-2 > 0 AND n > 50 pairs of observations
        nTest <- effectiveN-rank-2
        sel <- nTest > 0 & res.cov$n[-1] > 50 
        rank <- rank[sel]
        effectiveN <- effectiveN[sel]
        res.r <- res.r[sel]
        nTest <- nTest[sel]
        # relation between the square of a Student and a Fisher : T^2(n-k-2) = F(1,n-k-2)
        qf.ref <- qf(0.9,1,nTest) # quantile 90% of the Fisher n-k-2
        r.ref <- sqrt(qf.ref/(qf.ref+nTest)) # r.ref = partial r associated to the 90% limit
        sel <- abs(res.r) > r.ref # position of the empirical value wrt r.ref
        AR.order <- max(c(0,rank)[c(T,as.logical(cumprod(sel)))]) # rank of the of the first time in the conf interval
        if(AR.order > 0){
          points(rank[1:AR.order],res.r[1:AR.order],pch=myPch[i.var,i.state],col=vesselCol[i.vessel])
          lines(rank[1:AR.order],res.r[1:AR.order],lty=2, col=vesselCol[i.vessel],type="b",pch="")
        }
        # symbols(1:length(res.r),res.r,circle=res.cov$n[-1],inches=0.25,add=T)
        orderAR[i.vessel,i.degrad,i.var,i.state] <- AR.order
      }
    }
  }
}

legendV1V2(0.1,0.775,3,0.975,cex=0.8,model=F)


###########################################

my.cex <- 1
par(new=T)
par(mai=c(4.25,4.25,0.9,0.5))

plot(c(0,25),c(0,8.5),type="n",yaxs="i",xaxs="i",xaxt="n",yaxt="n",las=1,
     xlab="",ylab="",
     cex.lab=my.cex,cex.axis=my.cex)
mtext(at=seq(0,20,5),text=seq(0,20,5),side=1,line=0.3,cex=my.cex*0.75)
mtext(at=0:8,text=0:8,side=2,line=0.3,cex=my.cex*c(1,2,rep(1,6))*0.75,las=1)
mtext(at=12.5,text="Resolution",side=1,line=1,cex=my.cex*0.75)
mtext(at=12.5,text="(mean duration of state 2 in number of time steps)",side=1,line=1.5,cex=my.cex*0.75)
mtext(at=4,text="Most likely order of the AR process",side=2,line=1,cex=my.cex*0.75,las=0)

for(i.v in 1:nVessel){
  for(i.var in 1:2){
    for(i.state in 1:2) {# i.v <- 1 ; i.var <- 1 ; i.state <- 1
      #points(parameters[i.v,,3,1,2]+runif(4,-0.2,0.2),orderAR[i.v,,i.var,i.state]+runif(4,-0.1,0.1),col=vesselCol[i.v],
      #       pch=myPch[i.var,i.state],cex=1.3)#,type="b")
      points(parameters[i.v,,3,1,2]+ifelse(i.v==1,-0.2,0.2),orderAR[i.v,,i.var,i.state]+0.1,col=vesselCol[i.v],
             pch=myPch[i.var,i.state],cex=1)#,type="b")
      lines(parameters[i.v,,3,1,2]+ifelse(i.v==1,-0.2,0.2),orderAR[i.v,,i.var,i.state]+0.1,col=vesselCol[i.v],
            pch="",type="b",lty=2)
    }
  }
}
abline(h=0:8,lty=c(1,1,rep(3,7)),lwd=c(1,2,rep(1,8)))
#legendV1V2(1,5.9,10,7.7)

par(mai= c(1.02,0.82,0.82,0.42))

#polygon(c(0,25,25,0,0),c(-0.1,-0.1,1.1,1.1,-0.1),col=rgb(0,0,0,0.15))

dev.print(device = png, file = "Result/figurePartialRoAndOrderAR.png",width=600,height=600)

rm(i.v,i.var,i.state,my.cex,sel,orderAR,AR.order,nlag,i.vessel,i.degrad,vessel,state,code,y,yy,yyy,n,
   res.cov,res.r,i.k,Cij,Ci0,i,effectiveN,rank,r.ref,qf.ref,j)

