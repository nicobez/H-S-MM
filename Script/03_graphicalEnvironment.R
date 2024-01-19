par(las=1,cex=1.25)
defPar <- par(no.readonly = TRUE)

# Setting the colour for the vessel
vesselCol <- c(myPalette[1],myPalette[16])
vesselBg <- NULL
for(i in 1:2){
  temp <- as.numeric(hex2RGB(vesselCol[i])@coords[1,])
  temp <- rgb(temp[1],temp[2],temp[3],alpha=0.5)
  vesselBg[i] <- temp
}
# Cleaning
rm(i,temp)

# Setting pch
myPch <- array(c(0,2,1,15,17,16),dim=c(3,2),dimnames=list(c("vp","vr","T"),c("State 1","State 2")))

# # Loading the table with the graphical codes
# graphicalCodesV1V2 <-readPNG("Result/graphicalCodesV1V2.png")
# graphicalCodesV1V2 <- readJPEG("Result/graphicalCodesV1V2.jpg")


legendV1V2 <- function(x0,y0,x1,y1,cex=1,model=T){
  dx <- (x1-x0)/5 ; dy <- (y1-y0)/4
  xc <- (x1+x0)/2 ; yc <- (y1+y0)/2
  polygon(c(xc+0.5*dx,xc+2.5*dx,xc+2.5*dx,xc+0.5*dx,xc+0.5*dx),c(yc,yc,yc+dy,yc+dy,yc),border=1,col="white")
  polygon(c(xc-2.5*dx,xc+2.5*dx,xc+2.5*dx,xc-2.5*dx,xc-2.5*dx),c(yc-2*dy,yc-2*dy,yc,yc,yc-2*dy),border=1,col="white")
  polygon(c(xc-1.5*dx,xc+0.5*dx,xc+0.5*dx,xc-1.5*dx,xc-1.5*dx),c(yc-2*dy,yc-2*dy,yc+2*dy,yc+2*dy,yc-2*dy),border=1,col="white")
  text(xc-0.5*dx,yc+1.5*dy,"State",col=1)
  text(xc-1*dx,yc+0.5*dy,"1",col=1)
  text(xc,yc+0.5*dy,"2",col=1)
  text(xc-2*dx,yc-0.5*dy,TeX("$v_{1}$"))
  text(xc-2*dx,yc-1.5*dy,TeX("$v_{2}$"))
  if(model){
    text(xc+1.5*dx,yc+0.5*dy,"Settings") 
    text(xc+dx,yc-dy,"1",col=vesselCol[1])
    text(xc+2*dx,yc-dy,"2",col=vesselCol[2])
    segments(xc+0.6*dx,yc-dy,xc+0.9*dx,yc-dy,col=vesselCol[1])
    segments(xc+1.1*dx,yc-dy,xc+1.4*dx,yc-dy,col=vesselCol[1])
    segments(xc+1.6*dx,yc-dy,xc+1.9*dx,yc-dy,col=vesselCol[2])
    segments(xc+2.1*dx,yc-dy,xc+2.4*dx,yc-dy,col=vesselCol[2])
  } else {
    text(xc+1.5*dx,yc+0.5*dy,"Vessels")
    text(xc+dx,yc-dy,"1",col=vesselCol[1])
    text(xc+2*dx,yc-dy,"2",col=vesselCol[2])
    segments(xc+0.6*dx,yc-dy,xc+0.9*dx,yc-dy,col=vesselCol[1],lty=2)
    segments(xc+1.1*dx,yc-dy,xc+1.4*dx,yc-dy,col=vesselCol[1],lty=2)
    segments(xc+1.6*dx,yc-dy,xc+1.9*dx,yc-dy,col=vesselCol[2],lty=2)
    segments(xc+2.1*dx,yc-dy,xc+2.4*dx,yc-dy,col=vesselCol[2],lty=2)
  } 
  points(xc-dx,yc-0.5*dy,pch=0,cex=cex)
  points(xc,yc-0.5*dy,pch=15,cex=cex)
  points(xc-1*dx,yc-1.5*dy,pch=2,cex=cex)
  points(xc,yc-1.5*dy,pch=17,cex=cex)
  segments(xc-2.5*dx,yc-dy,xc+0.5*dx,yc-dy)
  segments(xc-2.5*dx,yc,xc+2.5*dx,yc)
  segments(xc-1.5*dx,yc+dy,xc+0.5*dx,yc+dy)
  segments(xc-0.5*dx,yc-2*dy,xc-0.5*dx,yc+dy)
  segments(xc+1.5*dx,yc-2*dy,xc+1.5*dx,yc)
}

