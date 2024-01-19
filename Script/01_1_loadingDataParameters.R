# Settings -------------------------------------------
listVessel <- c(924675,"007")               
listDegrad <- rbind(c(1,2,4,8),c(1,3,6,12))
listMethod <- c("HMM","HSMM")
listAR <- c("AR0","AR1")
listSize <-c(50,250)
nSimu <- 100
nVessel <- length(listVessel)

#####################
# Raw data and parameters
#####################

# Loading raw data -------------------------------------------
# (15/11/2023) They have been centred for anonymity concerns
for(i.v in 1:2){
  for(i.d in 1:4){
    load(paste0("Data/vessel_",listVessel[i.v],"_degrad_",i.d,"_cleaned.RData"))
    assign(paste0("vessel",i.v,"deg",i.d,sep=""),dataVesselDeg)
  }
}
rm(dataVesselDeg,i.v,i.d)

# Loading model parameters -------------------------------------------
# Model parameters are fitted once for all on the basis of raw data.
# To speed up the process, they are uploaded without re-estimating them. 
# The code for the estimation can be obtained on demand. 
load("Data/parameters.nbinom")
parameters <- parameters.nbinom
rm(parameters.nbinom)



