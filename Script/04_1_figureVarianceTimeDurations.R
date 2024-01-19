m <- as.vector(parameters[,,3,1,])
vsnb <- as.vector(parameters[,,3,2,])

pss <- 1-1/m
vgeom <- m^2*pss

plot(log10(vsnb),log10(vgeom),col=vesselCol[c(1,2)],
     xlab=TeX("$log_{10}(Var(T_{sNB}))$"),ylab=TeX("$log_{10}(Var(T_{geom}))$"),
     xlim=c(0,4.2),ylim=c(0,4.2),xaxs="i",yaxs="i",pch=rep(myPch[3,1:2],rep(8,2)))
abline(0,1,lty=2)

dev.print(device = png, file = "Result/varianceTimeDurations.png",width=600,height=600)


plot(log10(sqrt(vsnb)),log10(sqrt(vgeom)),col=vesselCol[c(1,2)],
     xlab=TeX("Standard deviation of $T_{sNB}$ (log10)"),ylab="",
     xlim=c(0,2.25),ylim=c(0,2.25),
     xaxs="i",yaxs="i",pch=rep(myPch[3,1:2],rep(8,2)))
mtext(text=TeX("Standard deviation of $T_{geom}$ (log10)"),at=1.125,side=2, line=2.5,las=0,cex=1.2)
abline(0,1,lty=2)

dev.print(device = png, file = "Result/stdTimeDurations.png",width=600,height=600)

rm(m,vsnb,pss,vgeom)
