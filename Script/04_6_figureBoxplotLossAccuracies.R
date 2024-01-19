
MA.250.df <- as.data.frame.table(lossAccuracy250)
names(MA.250.df) <- c("vessel","degrad","model.ref","model.est","accuracy")

temp1 <- grep("HMM",MA.250.df$model.ref)
markov.ref <- rep(2,128) ; markov.ref[temp1] <- 1
temp1 <- grep("HMM",MA.250.df$model.est)
markov.est <- rep(2,128) ; markov.est[temp1] <- 1
temp1 <- grep("AR0",MA.250.df$model.ref)
ar.ref <- rep(2,128) ; ar.ref[temp1] <- 1
temp1 <- grep("AR0",MA.250.df$model.est)
ar.est <- rep(2,128) ; ar.est[temp1] <- 1
MA.250.df <- cbind(MA.250.df, markov.ref,markov.est,ar.ref,ar.est)
MA.250.df$delta.markov <- markov.ref - markov.est
MA.250.df$delta.ar <- ar.ref - ar.est

m <- matrix(1:16, 4,4,byrow=T)
layout(m, widths=c(1, rep(3,3)),heights=c(1,rep(3,3)))  #;layout.show(20)
par(mai=rep(0.025,4))

delta <- c(-1,0,1)  
# 1
plot(0,0,bty="n",type="n",xaxt="n",yaxt="n")
# 2:4
plot(0,0,bty="n",type="n",xaxt="n",yaxt="n") ; text(0,0,"AR0 => AR1",lwd=2,cex=1.25)
plot(0,0,bty="n",type="n",xaxt="n",yaxt="n") ; text(0,0,"same AR",lwd=2,cex=1.25)
plot(0,0,bty="n",type="n",xaxt="n",yaxt="n") ; text(0,0,"AR1 => AR0",lwd=2,cex=1.25)
#
tempY <- range(MA.250.df$accuracy)+c(-0.5,0.5)
for(i in 1:3){
  plot(0,0,bty="n",type="n",xaxt="n",yaxt="n")
  if (i==1) text(0,0,"HMM => HSMM",lwd=2,cex=1.25,srt=90)
  if (i==2) text(0,0,"same Markov property",lwd=2,cex=1.25,srt=90)
  if (i==3) text(0,0,"HSMM => HMM",lwd=2,cex=1.25,srt=90)
  for(j in 1:3){
    plot(c(0,1),tempY,yaxs="i",bty="n",xaxt="n",yaxt="n",type="n",xlab="",ylab="")
    if(!(i==2 & j==2)) {
      tempA <- MA.250.df$accuracy[MA.250.df$delta.ar==delta[j] & MA.250.df$delta.markov==delta[i]]
      tempN <- length(tempA)
      tempVessel <- MA.250.df$vessel[MA.250.df$delta.ar==delta[j] & MA.250.df$delta.markov==delta[i]]
      tempDegrad <- MA.250.df$degrad[MA.250.df$delta.ar==delta[j] & MA.250.df$delta.markov==delta[i]]
      #boxplot(tempAccuracy,at=0.5,add=T,frame.plot=F,xaxt="n",yaxt="n",outcex=1.5,outpch=20)
      points(rep(0.5,tempN)+runif(tempN,-0.05,0.05),tempA,col=vesselCol[tempVessel],cex=2)
      polygon(c(0.4,0.4,0.6,0.6,0.4),
              c(max(tempA)+0.5,min(tempA)-0.5,min(tempA)-0.5,max(tempA)+0.5,max(tempA)+0.5),
              lty=2)
      points(0.6,mean(tempA),pch=20,cex=3,col=1)
      points(0.6,mean(tempA[tempVessel==listVessel[1]]),pch=20,col=vesselCol[1],cex=3)
      points(0.6,mean(tempA[tempVessel==listVessel[2]]),pch=20,col=vesselCol[2],cex=3)
      abline(h=0)
      #if(i==2 & j==3){
      #  arrows(0.4,min(tempA)-0.5,0,min(tempA)-0.5,lty=1,length=0.1)
      #  i1 <- match(tempVessel,listVessel)
      #  i2 <- match(tempDegrad,c("d1","d2","d3","d4"))
      #  tempArDist <-sapply(1:16, function(i) arDist[i1[i],i2[i],3,3])
      #  points(0.4-2*tempArDist,tempA,col=vesselCol[tempVessel],cex=2)
      #}
      if(j==1){
        points(rep(0,5),seq(0,-0.2,-0.05)*100,type="b",pch="-")
        text(rep(0.1,4),seq(-0.05,-0.2,-0.05)*100,seq(-0.05,-0.2,-0.05)*100,cex=1.5)
      }
      if(j==3){
        points(rep(1,5),seq(0,-0.2,-0.05)*100,type="b",pch="-")
        text(rep(0.9,4),seq(-0.05,-0.2,-0.05)*100,seq(-0.05,-0.2,-0.05)*100,cex=1.5)}
    }
  }
}

par(defPar)

# Save
dev.print(device = png, file = "Result/accuracyMra250.png", width=500,height=650)

mean(MA.250.df$accuracy[!(MA.250.df$delta.markov==0 & MA.250.df$delta.ar==0)])
mean(MA.250.df$accuracy[MA.250.df$delta.markov==1 & MA.250.df$delta.ar==1])

#Cleaning
rm(MA.250.df,temp1,markov.est,markov.ref,ar.est,ar.ref,m,delta,tempY,i,j,tempA,tempN,tempVessel,tempDegrad)
