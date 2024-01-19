my.cex <- 1
plot(c(0,30),c(-0.15,0.8),type="n",yaxs="i",xaxs="i",xaxt="n",las=1,
     xlab="",ylab="Autocorrelation coefficients",cex.lab=my.cex,cex.axis=my.cex)

#rasterImage(graphicalCodesV1V2,0.5,0.63,12,0.78)

mtext(at=seq(0,20,5),text=seq(0,20,5),side=1,line=1,cex=my.cex)
mtext(at=12.5,text="Resolution",side=1,line=2,cex=my.cex)
mtext(at=12.5,text="(mean duration of state 2 in number of time steps)",side=1,line=3,cex=my.cex)

for(i.v in 1:nVessel){
  lines(parameters[i.v,,3,1,2],parameters[i.v,,1,3,1],col=vesselCol[i.v],type="b",pch=0,cex=1.0)
  lines(parameters[i.v,,3,1,2],parameters[i.v,,1,3,2],col=vesselCol[i.v],type="b",pch=15,cex=1.0)
  lines(parameters[i.v,,3,1,2],parameters[i.v,,2,3,1],col=vesselCol[i.v],type="b",pch=2,cex=1.0)
  lines(parameters[i.v,,3,1,2],parameters[i.v,,2,3,2],col=vesselCol[i.v],type="b",pch=17,cex=1.0)
}
abline(h=0,lty=2)
segments(25,-0.8,25,0.8) ; segments(30,-0.8,30,0.7)
for(i.v in 1:nVessel){
  points(27.5+(i.v-1)*0,mean(parameters[i.v,,1,3,1]),pch=0,cex=1.0,col=vesselCol[i.v])
  points(27.5+(i.v-1)*0,mean(parameters[i.v,,1,3,2]),pch=15,cex=1.0,col=vesselCol[i.v])
  points(27.5+(i.v-1)*0,mean(parameters[i.v,,2,3,1]),pch=2,cex=1.0,col=vesselCol[i.v])
  points(27.5+(i.v-1)*0,mean(parameters[i.v,,2,3,2]),pch=17,cex=1.0,col=vesselCol[i.v])
}
# legend(x=c(2,8),y=c(0.6,0.8),bty="n",
#        pch=c(0,2),legend=c("v1-state1","v2-state1"),lty=c(0,0),pt.cex=c(my.cex,my.cex),cex=my.cex)
# legend(x=c(10,15),y=c(0.6,0.8),bty="n",
#        pch=c(15,17),legend=c("v1-state2","v2-state2"),lty=c(0,0),pt.cex=c(my.cex,my.cex),cex=my.cex)
# #legend(horiz = T, x=c(1,20),y=c(0.7,0.7),text.col=vesselCol,legend=c("vessel 1","vessel 2"),bty="n",cex=my.cex)
#text(x=27.5,y=0.7,"vessel 1",cex=my.cex,col=vesselCol[1])
#text(x=32.5,y=0.7,"vessel 2",cex=my.cex,col=vesselCol[2])
text(27.5,0.75,"Averages",cex=my.cex)

legendV1V2(0.5,0.63,12,0.78,cex=1.25)


dev.print(device = png, file = "Result/autocorrelation.png",width=600,height=600)

# Cleaning
rm(my.cex,i.v)
