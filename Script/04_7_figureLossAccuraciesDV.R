#####################
# Details when same markov but with AR1 ==> AR0
#####################
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

temp <- MA.250.df[MA.250.df$delta.ar==1 & MA.250.df$delta.markov==0,]
i1 <- match(temp$vessel,listVessel)
i2 <- match(temp$degrad,c("d1","d2","d3","d4"))

temp$D <-sapply(1:16, function(i) arDist[i1[i],i2[i],3,3])

plot(temp$D,temp$accuracy,col=vesselCol[i1],xlim=c(0,0.16),xaxs="i",xlab=TeX("$d_V$"),
     ylim=c(-10.5,0),yaxs="i",ylab="Relative loss of accuracy",pch=20,cex=3,type="n")
for(i in 1:16) funcUpperHalfCircle(temp$D[i],temp$accuracy[i],col=vesselCol[i1[i]])
funcLowerHalfCircle(temp$D,temp$accuracy,col="white")
lines(temp$D[c(1,3,5,7)],temp$accuracy[c(1,3,5,7)],col=vesselCol[1],type="b",pch="")
lines(temp$D[c(1,3,5,7)+1],temp$accuracy[c(1,3,5,7)+1],col=vesselCol[2],type="b",pch="")
lines(temp$D[c(1,3,5,7)+8],temp$accuracy[c(1,3,5,7)+8],col=vesselCol[1],type="b",pch="")
lines(temp$D[c(1,3,5,7)+8+1],temp$accuracy[c(1,3,5,7)+8+1],col=vesselCol[2],type="b",pch="")
abline(h=seq(-10,-2,2),lty=2)
abline(lsfit(temp$D,temp$accuracy),lty=1)
text(temp$D[7],temp$accuracy[7],'HMM',col=vesselCol[1],pos=4)
text(temp$D[8],temp$accuracy[8],'HMM',col=vesselCol[2],pos=4)
text(temp$D[15],temp$accuracy[15],'HSMM',col=vesselCol[1],pos=4)
text(temp$D[16],temp$accuracy[16],'HSMM',col=vesselCol[2],pos=4)

dev.print(device = png, file = "Result/lossAccuracyDv.png", width=500,height=500)

#lsfit(temp$D,temp$accuracy)$coef

rm(MA.250.df,temp1,markov.est,markov.ref,ar.ref,ar.est,temp,i1,i2,i)


