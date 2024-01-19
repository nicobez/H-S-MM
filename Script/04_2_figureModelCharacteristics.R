
tempA=sqrt(dVModel[,,3])
tempA=(tempA-min(tempA))/(max(tempA)-min(tempA))

tempT=sqrt(parameters[,,3,1,])
tempT=(tempT-min(tempT))/(max(tempT)-min(tempT))


par(mai=c(1,1,0.2,0.2))
plot(c(0.15,0.55), c(0,0.155), type="n",xaxs="i",xlab=TeX("$d_T$"),yaxs="i",ylab="",las=1,cex.lab=1.5)
mtext(TeX("$d_{AR}$"),side=2,line=3,las=1,cex=1.5)
for(i.v in 1:2){
  for(i.d in 1:4){
    if(i.d <= 3) funcCurvedarrow(from=c(hellingerDist[i.v,i.d,3], arDist[i.v,i.d,3,3]),to=c(hellingerDist[i.v,i.d+1,3], arDist[i.v,i.d+1,3,3]),
                    curve = 0, lwd=2, lcol= vesselCol[i.v],
                    arr.pos = 0.5,arr.type = "curved", arr.col = vesselCol[i.v])
    funcCircle(hellingerDist[i.v,i.d,3],
           arDist[i.v,i.d,3,3],
           r=0.02*(1+tempA[i.v,i.d]),border=vesselCol[i.v],col="white")
    funcUpperHalfCircle(hellingerDist[i.v,i.d,3],
                    arDist[i.v,i.d,3,3],
                    #r=0.02*(1+tempA[i.v,i.d])*sqrt(piModel[i.v,i.d,2]),#/max(piModel[i.v,i.d,]),
                    r=0.02*(1+tempT[i.v,i.d,2])*sqrt(piModel[i.v,i.d,2]),#/max(piModel[i.v,i.d,]),
                    col=vesselCol[i.v],border=vesselCol[i.v])
    funcLowerHalfCircle(hellingerDist[i.v,i.d,3],
                    arDist[i.v,i.d,3,3],
                    #r=0.02*(1+tempA[i.v,i.d])*sqrt(piModel[i.v,i.d,1]),#/max(piModel[i.v,i.d,]),
                    r=0.02*(1+tempT[i.v,i.d,1])*sqrt(piModel[i.v,i.d,1]),#/max(piModel[i.v,i.d,]),
                    col="white",border=vesselCol[i.v])
  }
}

funcCircle(0.18,0.14,r=0.02)
text(0.2,0.14,TeX(paste0("Circle area proportional to the accuracy of mixed model (","$d_V$",")")),pos=4)
funcUpperHalfCircle(0.18, 0.13,r=0.02*sqrt(0.75),col="black")
funcLowerHalfCircle(0.18, 0.13,r=0.02*sqrt(0.25),col="white")
text(0.2,0.13,"Proportion of time spent in state 1 (white) and 2 (plain)",pos=4)
text(0.2,0.125,TeX("(e.g., $\\pi_1=0.25 ;\\pi_2=0.75)"),pos=4)
text(0.2,0.12,TeX("Radii are proportional to $E(T_1)$ and $E(T_2)$"),pos=4)
funcCurvedarrow(from=c(0.16,0.11),to=c(0.18,0.11),
              curve = 0, lwd=2,arr.pos = 0.6,arr.type = "curved")
text(0.2,0.11,"Reduction of resolution",pos=4)
text(0.45,0.08,"Settings 1", col=vesselCol[1])
text(0.3,0.018,"Settings 2", col=vesselCol[2],pos=4)

par(defPar)

rm(tempA,tempT,i.v,i.d)

dev.print(device = png, file = "Result/figureModelCharacteristics.png", width=600,height=600)
