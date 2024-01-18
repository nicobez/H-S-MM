# H-S-MM
Script for the paper on HMM and HSMM


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
