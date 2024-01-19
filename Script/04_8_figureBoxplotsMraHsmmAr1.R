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

################## y-axis is specific to each degradation for a given vessel, variable, state and statistic

layout(matrix(1+c(0,rep(1,4),rep(2,4),2+(1:(16*9))),1+16,9,byrow = TRUE),widths=c(1,rep(2,4),rep(2,4)))
#layout.show(3+16*9)

par(mar=rep(0,4))
plot(c(0.5,3),c(0,1),type="n",xaxt="n",yaxt="n")
text(1.0,0.5,"HMM-AR0",srt=90,cex=0.75)
text(1.5,0.5,"HMM-AR1",srt=90,cex=0.75)
text(2.0,0.5,"HSMM-AR0",srt=90,cex=0.75)
text(2.5,0.5,"HSMM-AR1",srt=90,cex=0.75)

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
          temp <- thetaMra250HSMMAR1[,i.v,i.d,1:2,1:2,i.var,i.stat,i.state]
          if(i.stat==2) temp=sqrt(temp)
          yplot <- as.vector(quantile(temp,probs=c(0.025,0.975),na.rm=T))
          plot(c(-0.5,3),yplot,bty="n", type="n",xaxt="n",yaxt="n",xaxs="i")
          mtext(side=2,cex=0.5,line=-1.5,las=1,
                           text=round(yplot[1]+c(0.25,0.5,0.75)*diff(yplot),1),
                           at=yplot[1]+c(0.25,0.5,0.75)*diff(yplot),
                           col=vesselCol[i.v])
          for(i.model in 1:2){
            for(i.AR in 1:2){
              temp <- thetaMra250HSMMAR1[,i.v,i.d,i.model,i.AR,i.var,i.stat,i.state]
              if(i.stat==2) temp=sqrt(temp)
              if(sum(is.na(temp))!=100){
                boxplot(temp,at=myAT[2*i.model+i.AR-2],
                        add=T,yaxt="n",col=ifelse(i.model==2&i.AR==2,"white",vesselCol[i.v]))#,border=vesselCol[i.v])
                #points(myAT[2*i.model+i.AR-2],mean(temp),pch=20,col=1,cex=2)
              }
              if(i.stat==2) segments(0.25,sqrt(parameters[i.v,i.d,i.var,i.stat,i.state]),
                                     3,sqrt(parameters[i.v,i.d,i.var,i.stat,i.state]),col=vesselCol[i.v]) 
              else segments(0.25,parameters[i.v,i.d,i.var,i.stat,i.state],3,
                            parameters[i.v,i.d,i.var,i.stat,i.state],col=vesselCol[i.v])
              abline(v=0.25,col=vesselCol[i.v])
            }
          }
        }
      }
    }
  }
}

dev.print(device = png, file = "Result/boxplotMRA250HsmmAr1ByDegradation.png",width=450,height=600)
dev.print(device = postscript, file = "Result/boxplotMRA250HsmmAr1ByDegradation.eps",width=450,height=600,horizontal = FALSE)

rm(var.names,temp,myAT,ii,i.var,i.state,i.stat,yplot,i.v,i.d,i.model,i.AR,iStat)

