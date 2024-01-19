# Preparing outputs for real data estimation (HSMM-AR1)
accuracyHSMMAR1Real <- apply(resReal,1:4,"funcAccuracy")[,,2,2]


m <- matrix(1+c(0,0,1,1,1,1,0,0,3:6,2,7:11,2,12:16,2,17:21,2,22:32),7,6,byrow=T)

layout(m, widths=c(1,1, rep(4,4)),heights=c(1,1,rep(4,4),0.5))
#layout.show(32)
par(mai=rep(0.075,4))

i.method <- c(1,1,2,2) ; i.AR <- c(1,2,1,2)

# screen 1
plot(0,0,bty="n",type="n",xaxt="n",yaxt="n")
funcCurvedarrow(from=c(0.03,-1.7),to=c(1.97,0.007),
              curve = -0.5, lwd=3,
              arr.pos = 0.7,arr.type = "curved", arr.col = "grey")

# screen 2 and 3
plot(0,0,bty="n",type="n",xaxt="n",yaxt="n")
text(0,0,"Estimation model",lwd=2,cex=1.5)
# screen 2
plot(0,0,bty="n",type="n",xaxt="n",yaxt="n")
text(0,0,"Simulation model",lwd=2,cex=1.5,srt=90)

# screen 4:7
for(i in 1:4){
  method <- listMethod[i.method[i]]
  AR <- listAR[i.AR[i]]
  plot(0,0,bty="n",type="n",xaxt="n",yaxt="n")
  text(0,0,paste(method,AR,sep=","),lwd=2,cex=1.25)
}

# screen 8:27
xmax<- 25
for(i in 1:4){
  method <- listMethod[i.method[i]]
  AR <- listAR[i.AR[i]]
  plot(0,0,bty="n",type="n",xaxt="n",yaxt="n")
  text(-0.2,0,paste(method,AR,sep=","),lwd=2,cex=1.25,srt=90)
  #
  for(j in 1:4){
    plot(c(0,xmax),c(0.65,1),type="n",yaxs="i",xaxs="i",las=1,xlab="", ylab="",xaxt="n",yaxt="n")
    if(i != j) rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = rgb(0,0,0,0.15))
    abline(h=seq(0.6,1,0.1),lty=3)
    if(i==j) text(20,0.67,paste0("MSA(",i,",",j,")"),cex=1.25) else text(20,0.67,paste0("MRA(",i,",",j,")"),cex=1.25)
    for(i.size in 250){#c(50,250)){
      m <- get(paste0("accuracy",i.size,method,AR))
      q25 <- get(paste0("accuracyQ25",i.size,method,AR))
      q75 <- get(paste0("accuracyQ75",i.size,method,AR))
      q10 <- get(paste0("accuracyQ10",i.size,method,AR))
      q90 <- get(paste0("accuracyQ90",i.size,method,AR))    
      if(j==1) mtext(side=2,at=seq(0.7,1,0.1),text=as.character(seq(0.7,1,0.1)),las=1,line=0.25,cex=0.75)
      if(i==4) mtext(side=1,at=seq(5,xmax,5),text=as.character(seq(5,xmax,5)),line=0.25,cex=0.75)
      for(i.vessel in 1:nVessel){
        x <- parameters[i.vessel,,3,1,2]
        xp <- c(x,x[4:1]) # mu Vp 1
        yp <- c(q25[i.vessel,1:4,i.method[j],i.AR[j]],q75[i.vessel,4:1,i.method[j],i.AR[j]])
        polygon(xp,yp,col=vesselBg[i.vessel],lty=i.vessel,border=vesselCol[i.vessel])
        yp <- c(q10[i.vessel,1:4,i.method[j],i.AR[j]],q90[i.vessel,4:1,i.method[j],i.AR[j]])
        polygon(xp,yp,lty=1,border=vesselCol[i.vessel])
        points(x,dVModel[i.vessel,,3],col=vesselCol[i.vessel])
        lines(x,dVModel[i.vessel,,3],col=vesselCol[i.vessel],lwd=2)
        if(i==4 & j==4) {
          lines(x,accuracyHSMMAR1Real[i.vessel,],col=vesselCol[i.vessel],lty=2,lwd=2)
          points(x,accuracyHSMMAR1Real[i.vessel,],col=vesselCol[i.vessel])
        }  
      }
    }
  }
} 

#screen 28:32
for(i in 1:6)   plot(0,0,bty="n",type="n",xaxt="n",yaxt="n")

# Reset graphical environment
par(defPar)

# Save
dev.print(device = png, file = "Result/accuracyMsaMra250.png", width=600,height=600*1.25)
#dev.print(device = postscript, file = "Result/accuracyMsaMra250.eps",width=450,height=600,horizontal = FALSE)

# Clean
rm(m,i,j,i.method,i.size,i.vessel,i.AR,method,AR,xmax,q10,q25,q75,q90,x,xp,yp,accuracyHSMMAR1Real)




