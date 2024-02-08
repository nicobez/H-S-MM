# Remark :
# the shift is very well estimated (bias=0 var=0) EXCEPT for HSMM-AR0 and HSMM-AR1, vessel2, state2
# and is not represented 

# Cosmetics
var.names <- c(TeX("$E[v_{1,1}]$"),TeX("$\\sigma[v_{1,1}]$"),TeX("$\\tau_{1,1}$"),
               TeX("$E[v_{1,2}]$"),TeX("$\\sigma[v_{1,2}]$"),TeX("$\\tau_{1,2}$"),
               TeX("$E[v_{2,1}]$"),TeX("$\\sigma[v_{2,1}]$"),TeX("$\\tau_{2,1}$"),
               TeX("$E[v_{2,2}]$"),TeX("$\\sigma[v_{2,2}]$"),TeX("$\\tau_{2,2}$"),
               TeX("$E[T_{1}]$"),TeX("$\\sigma[T_{1}]$"),
               TeX("$E[T_{2}]$"),TeX("$\\sigma[T_{2}]$"))

################## y-axis is common to all the degradation for a given vessel, variable, state and statistic
layout(matrix(1+c(0,rep(1,4),rep(2,4),2+(1:(17*9))),1+17,9,byrow = TRUE),widths=c(1,2,rep(1.5,3),2,rep(1.5,3)))
#layout.show(3+17*9)

par(mar=rep(0.02,4))
plot(c(0.5,3),c(0,1),type="n",xaxt="n",yaxt="n")

plot(c(0,1),c(0,1),type="n",xaxt="n",yaxt="n")
text(0.5,0.65,"Settings 1",col=vesselCol[1])
par(new=T)
plot(c(0,27),c(0,1),type="n",xaxt="n",yaxt="n",xaxs="i")
arrows(2,0.2,26,0.2,length=0.1)
points(parameters[1,,3,1,2],rep(0.2,4),pch=20,cex=2,col=vesselCol[1])
text(25,0.5,"resolution")
temp <- 27/6.5*c(1,2.75,4.25,5.75)
segments(parameters[1,4,3,1,2],0.2,temp[1],0,col=vesselCol[1])
segments(parameters[1,3,3,1,2],0.2,temp[2],0,col=vesselCol[1])
segments(parameters[1,2,3,1,2],0.2,temp[3],0,col=vesselCol[1])
segments(parameters[1,1,3,1,2],0.2,temp[4],0,col=vesselCol[1])

plot(c(0,1),c(0,1),type="n",xaxt="n",yaxt="n")
text(0.5,0.65,"Settings 2",col=vesselCol[2])
par(new=T)
plot(c(0,27),c(0,1),type="n",xaxt="n",yaxt="n",xaxs="i")
arrows(2,0.2,26,0.2,length=0.1)
points(parameters[2,,3,1,2],rep(0.2,4),pch=20,cex=2,col=vesselCol[2])
text(25,0.5,"resolution")
temp <- 27/6.5*c(1,2.75,4.25,5.75)
segments(parameters[2,4,3,1,2],0.2,temp[1],0,col=vesselCol[2])
segments(parameters[2,3,3,1,2],0.2,temp[2],0,col=vesselCol[2])
segments(parameters[2,2,3,1,2],0.2,temp[3],0,col=vesselCol[2])
segments(parameters[2,1,3,1,2],0.2,temp[4],0,col=vesselCol[2])

myAT=c(1,1.5,2,2.5)
ii=0
for(i.var in 1:3){
  for(i.state in 1:2){
    if(i.var==3) iStat=c(1,2) else iStat=(1:3)
    for(i.stat in iStat){
      ii=ii+1
      plot(c(0,1),c(0,1),type="n",xaxt="n",yaxt="n")
      text(0.5,0.5,var.names[ii],col=1)
      for(i.v in 1:2){
        for(i.d in 4:1){
          temp <- thetaMsa250[,i.v,,1:2,1:2,i.var,i.stat,i.state]
          if(i.stat==2) temp=sqrt(temp)
          yplot <- as.vector(quantile(temp,probs=c(0.025,0.975),na.rm=T))
          plot(c(ifelse(i.d==4,-0.5,0.5),3),yplot,
               bty="n", type="n",xaxt="n",yaxt="n",xaxs="i")
          if(i.d==4) mtext(side=2,cex=0.5,line=-1.5,las=1,
                           text=round(yplot[1]+c(0.25,0.5,0.75)*diff(yplot),1),
                           at=yplot[1]+c(0.25,0.5,0.75)*diff(yplot),
                           col=vesselCol[i.v])
          for(i.model in 1:2){
            for(i.AR in 1:2){
              temp <- thetaMsa250[,i.v,i.d,i.model,i.AR,i.var,i.stat,i.state]
              if(i.stat==2) temp=sqrt(temp)
              if(sum(is.na(temp))!=100){
                boxplot(temp,at=myAT[2*i.model+i.AR-2],
                        add=T,yaxt="n",col=vesselCol[i.v])#,border=vesselCol[i.v])
                #points(myAT[2*i.model+i.AR-2],mean(temp),pch=20,col=1,cex=2)
              }
              if(i.stat==2) segments(0.25,sqrt(parameters[i.v,i.d,i.var,i.stat,i.state]),
                                     3,sqrt(parameters[i.v,i.d,i.var,i.stat,i.state]),col=vesselCol[i.v]) 
              else segments(0.25,parameters[i.v,i.d,i.var,i.stat,i.state],3,
                            parameters[i.v,i.d,i.var,i.stat,i.state],col=vesselCol[i.v])
              if(i.d==4) abline(v=0.25,col=vesselCol[i.v])
            }
          }
        }
      }
    }
  }
}
plot(c(0.5,3),c(0,1),type="n",xaxt="n",yaxt="n")
for(iVessel in 1:2){
  for(i in 1:4){
    plot(c(ifelse(i==1,-0.5,0.5),3),c(0,1),type="n",xaxt="n",yaxt="n",xaxs="i")
    text(1.0,0.5,"HMM-AR0",srt=90,cex=0.75,col=vesselCol[iVessel])
    text(1.5,0.5,"HMM-AR1",srt=90,cex=0.75,col=vesselCol[iVessel])
    text(2.0,0.5,"HSMM-AR0",srt=90,cex=0.75,col=vesselCol[iVessel])
    text(2.5,0.5,"HSMM-AR1",srt=90,cex=0.75,col=vesselCol[iVessel])
  }
}


dev.print(device = png, file = "Result/boxplotMsa250MixingDegradation.png",width=21*20,height=29.7*20)
dev.print(device = postscript, file = "Result/boxplotMsa250MixingDegradation.eps",width=450,height=600,horizontal = FALSE)
dev.copy2eps(device = eps, file = "Result/boxplotMsa250MixingDegradation.epsf",width=21*40,height=29.7*40)


#Cleaning
rm(var.names,temp,myAT,ii,i.var,i.state,i.stat,i.v,i.d,yplot,i.AR,i.model,iStat)

