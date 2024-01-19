layout(matrix(c(1,1,1,1,7,7,7,7,
                2,2,3,3,8,8,9,9,
                4,4,4,6,10,10,10,12,
                5,5,5,0,11,11,11,0,
                0,13,13,13,13,14,14,14),5,8,byrow = TRUE), 
       heights = c(3,1,3,1,4))

#layout(matrix(c(1,1,1,1,7,7,7,7,2,2,3,3,8,8,9,9,5,5,5,0,11,11,11,0,4,4,4,6,10,10,10,12,0,0,13,13,13,13,0,0),5,8,byrow = TRUE), 
#       heights = c(3,1,1,3,4))
par(cex=1)

vmin <- -12 #min(c(vessel1deg3$vp,vessel1deg3$vr,vessel2deg4$vp,vessel2deg4$vr))
vmax <- 13.5 #max(c(vessel1deg3$vp,vessel1deg3$vr,vessel2deg4$vp,vessel2deg4$vr))
delta <- 0.5

texT <- c(TeX("$T_1$"),TeX("$T_2$"))

for(i.v in 1:2){ #i.v <- 1
  # Trajectories
  par(mar = c(2,2,0,0.25))
  if(i.v==1) i.degrad <- 3 # deg3 = 1 hour = VMS for vessel 1
  if(i.v==2) i.degrad <- 4 # deg4 = 1 hour = VMS for vessel 2
  attach(get(paste0("vessel",i.v,"deg",i.degrad)))
  x <- lon 
  y <- lat
  plot(x,y, type="l",lty=3,xlab="",ylab="",xaxt="n",yaxt="n",fg=vesselCol[i.v])
  #mtext("Easting (degree)",side=1,line=1,cex.lab=0.75,col=vesselCol[i.v])
  #if(i.v==1) mtext("Northing (degree)",side=2,line=1.75,las=0,col=vesselCol[i.v])
  axis(1,line=-0.85,cex=0.5,tick=F,col.axis=vesselCol[i.v])
  axis(2,line=-0.85,cex=0.5,las=1,tick=F,col.axis=vesselCol[i.v]) 
  for(i.state in 1:2)
    points(x[state==i.state],y[state==i.state], pch=21,col=vesselCol[i.v],bg=ifelse(i.state==1,"white",vesselBg[i.v]))
  legend(ifelse(i.v==1,"topright","topleft"),horiz = F, legend=c("State 1","State 2"),text.col=vesselCol[i.v],
         pch=c(21,21),col=rep(vesselCol[i.v],2),pt.bg=c("white",vesselBg[i.v]),bty="n")
  legend(ifelse(i.v==1,"toplef","topright"),horiz = F, legend=paste0("N = ",length(x)," "),text.col=vesselCol[i.v],bty="n")
  # Sojourn time 
  state2 <- NULL ;  state1 <- NULL
  for(i in 1:max(code.traj)){
    #i <-20
    tmp <- state[code.traj==i]
    tmp2 <- abs(diff(tmp))
    if(sum(tmp2==1)==0) ### no fishing during the trip
      state1 <- c(state1, length(tmp2)+1)
    if(sum(tmp2==1)>0){
      tmp2 <- c(1,tmp2)
      if(funcLast(tmp2)==0) tmp2[length(tmp)] <- 1
      tmp2 <- diff(which(tmp2==1))
      if(tmp[1]==1){ # the time series starts by state1
        state1 <- c(state1,tmp2[seq(1,length(tmp2),2)])
        state2 <- c(state2,tmp2[seq(2,length(tmp2),2)])
      }
      if(tmp[1]==2){ # the time series starts by state2
        state1 <- c(state1,tmp2[seq(2,length(tmp2),2)])
        state2 <- c(state2,tmp2[seq(1,length(tmp2),2)])
      }
    }
  }
  # temp <- rle(state) donne des r?sultats l?g?rement diff?remment li?s aux sauts de mar?es
  xbreaks <- 0:ceiling(max(state1))
  hist1 <- hist(state1,breaks=xbreaks,plot=F)
  y1 <- hist1$density #hist1$counts/sum(hist1$counts)
  #
  xbreaks <- 0:ceiling(max(state2))
  hist2 <- hist(state2,breaks=xbreaks,plot=F)
  y2 <- hist2$density #hist2$counts/sum(hist2$counts)
  #
  par(mar = c(1,2,0,0))
  for(i.state in 1:2){
    temp <- get(paste0("state",i.state))
    xbreaks <- 0:ceiling(max(temp))
    temp.hist <- hist(temp,breaks=xbreaks,plot=F)
    temp.y <- temp.hist$density #hist1$counts/sum(hist1$counts)
    barplot(temp.y, axes = FALSE, ylim = c(0, max(y1,y2)), xlim=c(0,max(state1,state2)),
            space = 0,border=vesselCol[i.v],col=ifelse(i.state==1,"white",vesselBg[i.v]))#,add=T)
    if(i.v==1) mtext(text=seq(1,length(temp.y),by=3),side=1,line=0.3,at=seq(1,length(temp.y),by=3)-0.5,cex=0.75,col=vesselCol[i.v])
    if(i.v==2) mtext(text=seq(1,length(temp.y),by=2),side=1,line=0.3,at=seq(1,length(temp.y),by=2)-0.5,cex=0.75,col=vesselCol[i.v])
    #text(max(state1,state2)/2,max(y2,y1)/2,paste0("N = ",length(temp)),col=vesselCol[i.v])
    #text(max(state1,state2)/2,0.75*max(y2,y1),paste0("state ",i.state),col=vesselCol[i.v])
    text(0.75*max(state1,state2),0.75*max(y2,y1),texT[i.state],col=vesselCol[i.v])
    text(0.75*max(state1,state2),max(y2,y1)/2,paste0("N = ",length(temp)),col=vesselCol[i.v])
  }  
  # Correlation plot between V1 and V2
  par(mar = c(0,3,2,0))
  plot(vp,vr,type="n",las=1,xlim=c(vmin,vmax),ylim=c(vmin,vmax), xaxt="n",yaxt="n",fg=vesselCol[i.v])
  for(i.state in 1:2) points(vp[state==i.state],vr[state==i.state],
                             pch=21,col=vesselCol[i.v],bg=ifelse(i.state==1,"white",vesselBg[i.v]))
  mtext(TeX("$V_p$"),side=1,line=1.1,col=vesselCol[i.v])
  mtext(TeX("$V_r$"),side=2,line=1.1,srt=90,col=vesselCol[i.v])
  axis(1,line=-0.95,cex=0.5,tick=F,col.axis=vesselCol[i.v])
  axis(2,line=-0.85,cex=0.5,las=1,tick=F,col.axis=vesselCol[i.v]) 
  legend(ifelse(i.v==1,"topright","topleft"),horiz = F, legend=c("State 1","State 2"),text.col=vesselCol[i.v],
         pch=c(21,21),col=rep(vesselCol[i.v],2),pt.bg=c("white",vesselBg[i.v]),bty="n")
  abline(v=0,h=0,lty=2,col=vesselCol[i.v])
  #
  par(mar = c(0,3,1,1))
  #xbreaks <- seq(floor(min(vp)),ceiling(max(vp)),by=delta)
  xbreaks <- seq(vmin,vmax,by=delta)
  hist1 <- hist(vp[state==1],breaks=xbreaks,plot=F)
  hist2 <- hist(vp[state==2],breaks=xbreaks,plot=F)
  y1 <- hist1$density ; y2 <- hist2$density
  top <- 1.1*max(c(y1,y2))
  barplot(y1,border=vesselCol[i.v],space=0,col="white",ylim=c(-top,top),axes = FALSE)
  barplot(-y2,border=vesselCol[i.v],space=0,col=vesselBg[i.v],axes = FALSE,add=T)
  #
  par(mar = c(0,0,2,1))
  #xbreaks <- seq(floor(min(vr)),ceiling(max(vr)),by=delta)
  hist1 <- hist(vr[state==1],breaks=xbreaks,plot=F) ; hist2 <- hist(vr[state==2],breaks=xbreaks,plot=F)
  y1 <- hist1$density ; y2 <- hist2$density
  top <- 1.1*max(c(y1,y2))
  barplot(y1, axes = FALSE, xlim = c(-top, top), space = 0, border=vesselCol[i.v],col="white",horiz = T)
  barplot(-y2, axes = FALSE, space = 0, border=vesselCol[i.v],col=vesselBg[i.v],horiz=T,add=T)
  detach(2)
}
### Summary metrics
my.cex <- 1
par(mar = c(5,5,1,1))
plot(c(0,25),c(0,1),yaxs="i",xaxs="i",type="n",las=1,ylab="",xlab="Data resolution")
for(i.v in 1:2){
  funcUpperHalfCircle(parameters[i.v,,3,1,2],dVEmpirical[i.v,,3],r=0.02,col=vesselBg[i.v],border=vesselCol[i.v],lwd=0.5)
  funcLowerHalfCircle(parameters[i.v,,3,1,2],dVEmpirical[i.v,,3],r=0.02,col="white",border=vesselCol[i.v],lwd=0.5)
  lines(parameters[i.v,,3,1,2],dVEmpirical[i.v,,3],col=vesselCol[i.v],lty=2)
  text(parameters[i.v,,3,1,2],hellingerDistPropGeom[i.v,,3],col=vesselCol[i.v],"G")
  lines(parameters[i.v,,3,1,2],hellingerDistPropGeom[i.v,,3],col=vesselCol[i.v],type="b",pch="  ",lty=2)
  text(parameters[i.v,,3,1,2],hellingerDistPropSNB[i.v,,3],col=vesselCol[i.v],"sNB")
  lines(parameters[i.v,,3,1,2],hellingerDistPropSNB[i.v,,3],col=vesselCol[i.v],type="b",pch="  ",lty=2)
}

par(mar = c(5,0,1,0))
plot(c(0,1),c(0,1),yaxs="i",xaxs="i",xaxt="n",yaxt="n",type="n",xlab="",ylab="",bty="n")
text(0,0.6,"G : Hellinger distance between",pos=4,cex=0.8)
text(0.1,0.5,"empirical histogram and geometric pmf.",pos=4,cex=0.8)
text(0,0.3,"sNB : Hellinger distance between",pos=4,cex=0.8)
text(0.1,0.2,"empirical histogram and sNB pmf.",pos=4,cex=0.8)
funcUpperHalfCircle(0.05,0.8,r=0.02,col=1,border=1,lwd=0.5)
funcLowerHalfCircle(0.05,0.8,r=0.02,col="white",border=1,lwd=0.5)
text(0.05,0.8,TeX(paste0(" : Accuracy under mixed model (","$d_V$",").")),pos=4,cex=0.8)

par(defPar)

dev.print(device = png, file = "Result/Data.png",width=600,height=900)

# Clean
rm(i.v,i.degrad,i.state,x,y,state1,state2,tmp,tmp2,i,xbreaks,hist1,hist2,y1,y2,temp,temp.hist,temp.y,delta,vmin,vmax,top,texT,my.cex)

