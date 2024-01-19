# Full experimental design i.e. for 2 vessels -----------------------------
expDesign <- expand.grid(listVessel,listMethod,listAR,1:4)
dimnames(expDesign)[[2]] <- c("vessel","method","AR", "degrad")
expDesignSize <- dim(expDesign)[1]

#####################
# Simulation-Estimation : directories and file names 
#####################
# path for MSA and MRA for 250 alternations
pathMsa250 <- "Data/MSA_20201130"
pathMra250 <- "Data/MRA_20201130"
# path for MSA and MRA for 50 alternations
pathMsa50 <- "Data/MSA_20210222"
pathMra50 <- "Data/MRA_20210222"

listNomMsa250 <- list.files(path=pathMsa250,pattern="ResultatsMPI")
listNomMra250 <- list.files(path=pathMra250,pattern="ResultatsMPI")
listNomMsa50 <- list.files(path=pathMsa50,pattern="ResultatsMPI")
listNomMra50 <- list.files(path=pathMra50,pattern="ResultatsMPI")

#####################
# Simulation-Estimation : Parameters re-estimation (only for MSA x 250 alternations)
#####################
thetaMsa250=array(NA,dim=c(nSimu,nVessel,4,2,2,3,5,2),
              dimnames=list(paste0("simu",seq(1,nSimu)),
                            listVessel,
                            c("d1","d2","d3","d4"),
                            c("HMM","HSMM"),
                            c("AR0","AR1"),
                            c("vp","vr","duree"),
                            c("m","var","mu","eta","shift"),
                            c("s1","s2")))

# loading RData for the two vessels only and read the parameters re-estimations
for(i in (1:expDesignSize)){
  cat("i :",i,"\n")
  for(j in ((i-1)*nSimu+1):(i*nSimu)){
    i.simu <- j - (i-1)*nSimu
    i.vessel <- which(listVessel==as.character(expDesign[i,1]))      
    i.method <- (as.character(expDesign[i,2]) == "HMM") + 2*(as.character(expDesign[i,2]) == "HSMM")  #"HMM" ou "HSMM"
    method <-  as.character(expDesign[i,2])
    i.AR <- (as.character(expDesign[i,3]) == "AR0") + 2*(as.character(expDesign[i,3]) == "AR1")      # "AR0" ou "AR1"
    AR <- as.character(expDesign[i,3])
    i.degrad <- expDesign[i,4]
    
    temp0 <- paste0("ResultatsMPI_MSA-nbinom.",i.vessel,".",i.method,".",i.AR,".",i.degrad,"_exp",i,"_simu",i.simu,".RData")
    if(temp0 %in% listNomMsa250){
      load(file=paste(pathMsa250,temp0,sep="/"))
      thetaMsa250[i.simu,i.vessel,i.degrad,i.method,i.AR,,,] <- res.AS.EM.Q.temp$theta 
      #msa250$confusion[i.simu,i.vessel,i.degrad,i.method,i.AR,,] <- res.AS.EM.Q.temp$confusion.ML
    }
  }
}

#####################
# Simulation-Estimation : Confusion of state re-estimation
#####################
confusion <- array(NA,dim=c(nSimu,nVessel,4,2,2,2,2),
                      dimnames=list(paste0("simu",seq(1,nSimu)),
                                    listVessel,
                                    c("d1","d2","d3","d4"),
                                    c("HMM","HSMM"),
                                    c("AR0","AR1"),
                                    c("yhat1","yhat2"),
                                    c("state1","state2")))
confusion250HMMAR0 <- confusion ### robustesse quand model0=HMMxAR0
confusion250HMMAR1 <- confusion ### robustesse quand model0=HMMxAR1
confusion250HSMMAR0 <- confusion ### robustesse quand model0=HSMMxAR0
confusion250HSMMAR1 <- confusion ### robustesse quand model0=HSMMxAR1
confusion50HMMAR0 <- confusion ### idem pour chaines courte
confusion50HMMAR1 <- confusion 
confusion50HSMMAR0 <- confusion 
confusion50HSMMAR1 <- confusion

# loading RData corresponding to the confusion matrix for the two vessels only long sequences
for(i in 1:expDesignSize){
  for(j in ((i-1)*nSimu+1):(i*nSimu)){ # j in 1:3200
    i.simu <- j - (i-1)*nSimu # i.simu in 1:100
    i.vessel <- which(listVessel==as.character(expDesign[i,1]))      
    i.method <- (as.character(expDesign[i,2]) == "HMM") + 2*(as.character(expDesign[i,2]) == "HSMM")  #"HMM" ou "HSMM"
    method <-  as.character(expDesign[i,2])
    i.AR <- (as.character(expDesign[i,3]) == "AR0") + 2*(as.character(expDesign[i,3]) == "AR1")      # "AR0" ou "AR1"
    AR <- as.character(expDesign[i,3])
    i.degrad <- expDesign[i,4]
    #
    i.othermethod <- (as.character(expDesign[i,2]) != "HMM") + 2*(as.character(expDesign[i,2]) != "HSMM")  #"HMM" ou "HSMM"
    i.otherAR <- (as.character(expDesign[i,3]) != "AR0") + 2*(as.character(expDesign[i,3]) != "AR1")      # "AR0" ou "AR1"
    #
    for(i.length in c("50","250")){
      temp1 <- get(paste0("confusion",i.length,method,AR))
      #
      temp0 <- paste0("ResultatsMPI_MRA-nbinom._exp",i,"_simu",j,"_othermethod",i.method,"_otherAR",i.otherAR,".RData")
      if(temp0 %in% get(paste0("listNomMra",i.length))){
        load(file=paste(get(paste0("pathMra",i.length)),temp0,sep="/"))
        temp1[i.simu,i.vessel,i.degrad,i.method,i.otherAR,,] <- res.AS.EM.Q.temp$confusion.ML
      }
      #
      temp0 <- paste0("ResultatsMPI_MRA-nbinom._exp",i,"_simu",j,"_othermethod",i.othermethod,"_otherAR",i.otherAR,".RData")
      if(temp0 %in% get(paste0("listNomMra",i.length))){
        load(file=paste(get(paste0("pathMra",i.length)),temp0,sep="/"))
        temp1[i.simu,i.vessel,i.degrad,i.othermethod,i.otherAR,,] <- res.AS.EM.Q.temp$confusion.ML
      }
      #
      temp0 <- paste0("ResultatsMPI_MRA-nbinom._exp",i,"_simu",j,"_othermethod",i.othermethod,"_otherAR",i.AR,".RData")
      if(temp0 %in% get(paste0("listNomMra",i.length))){
        load(file=paste(get(paste0("pathMra",i.length)),temp0,sep="/"))
        temp1[i.simu,i.vessel,i.degrad,i.othermethod,i.AR,,] <- res.AS.EM.Q.temp$confusion.ML 
      }
      #
      temp0 <- paste0("ResultatsMPI_MSA-nbinom.",i.vessel,".",i.method,".",i.AR,".",i.degrad,"_exp",i,"_simu",i.simu,".RData")
      if(temp0 %in% get(paste0("listNomMsa",i.length))){
        load(file=paste(get(paste0("pathMsa",i.length)),temp0,sep="/"))
        temp1[i.simu,i.vessel,i.degrad,i.method,i.AR,,] <- res.AS.EM.Q.temp$confusion.ML 
      }
      #
      assign(paste0("confusion",i.length,method,AR),temp1)
    }
  }
}

# cleaning workspace
rm(res.AS.EM.Q.temp,i,j,i.simu,i.vessel,i.method,method,i.length,i.AR,AR,i.degrad,temp0,temp1,i.otherAR,i.othermethod,confusion)

#####################
# Simulation-Estimation : parameters re-estimation in MRA case study with HSMM-AR1 as simulation model 
#####################
thetaMra250HSMMAR1 <- array(NA,dim=c(nSimu,nVessel,4,2,2,3,5,2),
                   dimnames=list(paste0("simu",seq(1,nSimu)),
                                 listVessel,
                                 c("d1","d2","d3","d4"),
                                 c("HMM","HSMM"),
                                 c("AR0","AR1"),
                                 c("vp","vr","duree"),
                                 c("m","var","mu","eta","shift"),
                                 c("s1","s2")))

thetaMra250HMMAR0 <- thetaMra250HSMMAR1
thetaMra250HMMAR1 <- thetaMra250HSMMAR1
thetaMra250HSMMAR0 <- thetaMra250HSMMAR1

# loading RData corresponding to the confusion matrix for the two vessels only long sequences
for(i in 1:expDesignSize){
  for(j in ((i-1)*nSimu+1):(i*nSimu)){ # j in 1:3200
    i.simu <- j - (i-1)*nSimu # i.simu in 1:100
    i.vessel <- which(listVessel==as.character(expDesign[i,1]))      
    i.method <- (as.character(expDesign[i,2]) == "HMM") + 2*(as.character(expDesign[i,2]) == "HSMM")  #"HMM" ou "HSMM"
    method <-  as.character(expDesign[i,2])
    i.AR <- (as.character(expDesign[i,3]) == "AR0") + 2*(as.character(expDesign[i,3]) == "AR1")      # "AR0" ou "AR1"
    AR <- as.character(expDesign[i,3])
    i.degrad <- expDesign[i,4]
    #
    i.othermethod <- (as.character(expDesign[i,2]) != "HMM") + 2*(as.character(expDesign[i,2]) != "HSMM")  #"HMM" ou "HSMM"
    i.otherAR <- (as.character(expDesign[i,3]) != "AR0") + 2*(as.character(expDesign[i,3]) != "AR1")      # "AR0" ou "AR1"
    #
    i.length="250" #for(i.length in c("50","250")){
    temp1 <- get(paste0("thetaMra",i.length,method,AR))
    #
    temp0 <- paste0("ResultatsMPI_MRA-nbinom._exp",i,"_simu",j,"_othermethod",i.method,"_otherAR",i.otherAR,".RData")
    if(temp0 %in% get(paste0("listNomMra",i.length))){
      load(file=paste(get(paste0("pathMra",i.length)),temp0,sep="/"))
      temp1[i.simu,i.vessel,i.degrad,i.method,i.otherAR,,,] <- res.AS.EM.Q.temp$theta
    }
    #
    temp0 <- paste0("ResultatsMPI_MRA-nbinom._exp",i,"_simu",j,"_othermethod",i.othermethod,"_otherAR",i.otherAR,".RData")
    if(temp0 %in% get(paste0("listNomMra",i.length))){
      load(file=paste(get(paste0("pathMra",i.length)),temp0,sep="/"))
      temp1[i.simu,i.vessel,i.degrad,i.othermethod,i.otherAR,,,] <- res.AS.EM.Q.temp$theta
    }
    #
    temp0 <- paste0("ResultatsMPI_MRA-nbinom._exp",i,"_simu",j,"_othermethod",i.othermethod,"_otherAR",i.AR,".RData")
    if(temp0 %in% get(paste0("listNomMra",i.length))){
      load(file=paste(get(paste0("pathMra",i.length)),temp0,sep="/"))
      temp1[i.simu,i.vessel,i.degrad,i.othermethod,i.AR,,,] <- res.AS.EM.Q.temp$theta 
    }
    #
    temp0 <- paste0("ResultatsMPI_MSA-nbinom.",i.vessel,".",i.method,".",i.AR,".",i.degrad,"_exp",i,"_simu",i.simu,".RData")
    if(temp0 %in% get(paste0("listNomMsa",i.length))){
      load(file=paste(get(paste0("pathMsa",i.length)),temp0,sep="/"))
      temp1[i.simu,i.vessel,i.degrad,i.method,i.AR,,,] <- res.AS.EM.Q.temp$theta 
    }
    #
    assign(paste0("thetaMra",i.length,method,AR),temp1)
    #}
  }
}

# cleaning workspace
rm(res.AS.EM.Q.temp,i,j,i.simu,i.vessel,i.length,i.method,method,i.AR,AR,i.degrad,temp0,temp1,i.otherAR,i.othermethod)

rm(pathMsa50,pathMsa250,pathMra50,pathMra250,listNomMsa250,listNomMsa50,listNomMra250,listNomMra50)
