#############
#
# Make file 
# author : nicolas.bez@ird.fr
#
# january 2024
# 
# **********************************************************************
#   Relationship between notation in mathematical equations and in R code
# **********************************************************************
#   Equation          | R
#   =================================
#   tau (coef AR1)    | mu (coef AR1)
#   nu                | eta
#   mu                | m
#   ---------------------------------
#   mu=nu/(1-tau)     | m=eta/(1-mu)
#
# **********************************************************************
#   Degradation
# **********************************************************************
#   d1:d4 in R objects is reverse wrt to resolution (d1 High Def ==> d4 low def)
#
#############

rm(list=ls())

source("Script/99_addsOnFunctions.R")

source("Script/00_1_libraries.R")

source("Script/01_1_loadingDataParameters.R")
source("Script/01_2_loadingSimulationsOutputs.R")
source("Script/01_3_loadingRealCaseEstimations.R")

source("Script/02_analysis.R")

source("Script/03_graphicalEnvironment.R")

source("Script/04_1_figureVarianceTimeDurations.R")
source("Script/04_2_figureModelCharacteristics.R")
source("Script/04_3_figureAutocorrelations.R")
source("Script/04_4_figureBoxplotsMsa.R")
source("Script/04_5_figureAccuraciesMsaMra250.R")
source("Script/04_6_figureBoxplotLossAccuracies.R")
source("Script/04_7_figureLossAccuraciesDV.R")
source("Script/04_8_figureBoxplotsMraHsmmAr1.R")
source("Script/04_9_figureData.R")
source("Script/04_10_figurePartialRoAndOrderAR.R")
source("Script/04_11_figureDelayedCorrelationSupplementary.R")













