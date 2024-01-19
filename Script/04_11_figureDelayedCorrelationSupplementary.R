i.degrad <- c(3,4)
#def.par <- par(no.readonly = TRUE)

m <- matrix(c(0,0,1,2,3,4,9,10,3,5,11,12,6,7,13,14,6,8,15,16), 5,4,byrow=T)
layout(m, widths=c(0.5,0.5,3,3),heights=c(1,rep(4,4)))
# layout.show(16)
par(mai=rep(0.075,4))
# screen 1, 2
plot(0,0,bty="n",type="n",xaxt="n",yaxt="n")
text(0,0,"Vessel 1",cex=1.25,col=vesselCol[1])
plot(0,0,bty="n",type="n",xaxt="n",yaxt="n")
text(0,0,"Vessel 2",cex=1.25,col=vesselCol[2])
# screen 3
plot(0,0,bty="n",type="n",xaxt="n",yaxt="n")
text(-0.2,0,"Vp",lwd=2,cex=1.25,srt=0)
# screen 4
plot(0,0,bty="n",type="n",xaxt="n",yaxt="n")
text(-0.2,0,"State1",lwd=2,cex=1.25,srt=0)
# screen 5
plot(0,0,bty="n",type="n",xaxt="n",yaxt="n")
text(-0.2,0,"State2",lwd=2,cex=1.25,srt=0)
# screen 6
plot(0,0,bty="n",type="n",xaxt="n",yaxt="n")
text(-0.2,0,"Vr",lwd=2,cex=1.25,srt=0)
# screen 7
plot(0,0,bty="n",type="n",xaxt="n",yaxt="n")
text(-0.2,0,"State1",lwd=2,cex=1.25,srt=0)
# screen 8
plot(0,0,bty="n",type="n",xaxt="n",yaxt="n")
text(-0.2,0,"State2",lwd=2,cex=1.25,srt=0)

for(i.var in 1:2){
  for(i.state in 1:2){
    for(i.vessel in 1:2){
      #for(i.degrad in 1:4){
      # i.vessel <- 2 ; i.state <- 2
      gps.deg <- get(paste0("vessel",i.vessel,"deg",i.degrad[i.vessel]))
      if(i.var==1) x <- gps.deg$vp[gps.deg$state==i.state]
      if(i.var==2) x <- gps.deg$vr[gps.deg$state==i.state]
      x2 <- x[-1]
      x1 <- x[-length(x)]
      traj <- gps.deg$code.traj[gps.deg$state==i.state]
      sel <- traj[-1]-traj[-length(traj)]==0
      if(i.var==1 & i.vessel==1 & i.state==2) sel <- sel & x2 < 4 & x1 < 4
      #plot(x2[sel],x1[sel],pch=20,col=rgb(0,0,0,0.25),xlab="",ylab="",xaxt="n",yaxt="n")
      plot(x2[sel],x1[sel],pch=myPch[i.var,i.state],col=vesselCol[i.vessel],xlab="",ylab="",xaxt="n",yaxt="n")
      abline(v=0,h=0,lty=2)
      
      #cat(funcVar(lsfit(x2[sel],x1[sel],intercept=F)$residuals)/funcVar(x1[sel]), "\n")
      #cat(round(lsfit(x2[sel],x1[sel],intercept=F)$coefficients^2*funcVar(x2[sel])/funcVar(x1[sel])*100,0),"\n")
      if(anova(lm(x1[sel]~x2[sel]))$`Pr(>F)`[1] < 0.05){
        abline(lsfit(x2[sel],x1[sel],intercept=F),col=vesselCol[i.vessel],lwd=2)
        text(par('usr')[1]+1,par('usr')[3]+1,pos=4,cex=1.5,
             paste0("R² = ",round(lsfit(x2[sel],x1[sel],intercept=F)$coefficients^2*funcVar(x2[sel])/funcVar(x1[sel])*100,0),"%"))
      }
    }
  }
}
par(defPar)

# Cleaning
rm(i.degrad,m,i.var,i.state,i.vessel,gps.deg,x1,x2,traj,sel)

dev.print(device = png, file = "Result/figureDelayedCorrelation.png",width=600,height=600)

