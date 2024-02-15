# H-S-MM
Scripts for reproducing all the figures in the paper on HMM and HSMM

The outputs of the MSA and the MRA experiments are first loaded and used for analyses and figures.

MSA = Model Skill Assessement, i.e. estimation model = simulation model

MRA = Model Robustness Assessment, i.e. estimation != simulation model

Experiments for MSA can be reproduced from ... However, the outputs have been stored in the Data directory to enable direct analyses. 

Experiments for MRA was done with a remote supercalculator and have been stored in the Data directory to enable direct analyses.

# Relationship between notation in mathematical equations and in R code
 
Equation          | R

tau (coef AR1)    | mu (coef AR1)

nu                | eta

mu                | m

mu=nu/(1-tau)     | m=eta/(1-mu)

 **********************************************************************
   Degradation
 **********************************************************************
   d1:d4 in R objects is reverse wrt to resolution (d1 High Def ==> d4 low def)

