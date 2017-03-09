library(MVA)
library(sem)
library(foreign)
#fuente: http://rstudio-pubs-static.s3.amazonaws.com/11011_caa8639a12cb488aba3df58c696626bb.html
#In this model, I am determining whether AIDS-related stigma, religiosity, and adherence 
#to the traditional male role norm of antifemininity lead to perceptions of two types of threat 
#in response to gay men. I am also testing whether these perceived threats, in turn, lead to intoxicated 
#and non-intoxicated aggression toward sexual minorities.

corr.diss <- matrix(c(1, 0, 0, 0, 0, 0, 0, 0.876, 1, 0, 0, 0, 0, 0, 0.318, 0.32, 
                      1, 0, 0, 0, 0, 0.358, 0.333, 0.84, 1, 0, 0, 0, 0.245, 0.264, 0.566, 0.521, 
                      1, 0, 0, 0.267, 0.254, 0.549, 0.506, 0.365, 1, 0, 0.058, 0.074, 0.074, 0.301, 
                      0.52, 0.149, 1), ncol = 7, byrow = T)

rownames(corr.diss) <- colnames(corr.diss) <- c("IntAgg", "NonIntAgg", "RealThr", 
                                                "SymbThr", "ARStigma", "MRNaf", "Relig")

corr.diss

ram.diss <- matrix(c(
  # path                        parameter   start-value
  "RealThr   ->  IntAgg",     "b11",      NA,
  "SymbThr   ->  IntAgg",     "b12",      NA,
  "RealThr   ->  NonIntAgg",  "b21",      NA,
  "SymbThr   ->  NonIntAgg",  "b22",      NA,
  "ARStigma  ->  RealThr",    "a11",      NA,
  "Relig     ->  RealThr",    "a12",      NA,
  "MRNaf     ->  RealThr",    "a13",      NA,
  "ARStigma  ->  SymbThr",    "a21",      NA,
  "Relig     ->  SymbThr",    "a22",      NA,
  "MRNaf     ->  SymbThr",    "a23",      NA,
  "RealThr  <->  SymbThr",    "c11",      NA,
  "IntAgg <-> NonIntAgg",     "c21",      NA,     
  "RealThr  <->  RealThr",    "d11",      NA,
  "SymbThr  <->  SymbThr",    "d21",      NA,
  "IntAgg   <->  IntAgg",     "d31",      NA,
  "NonIntAgg <-> NonIntAgg",  "d41",      NA),
  ncol=3, byrow=T)

sem.diss <- sem(ram.diss, corr.diss, N = 212, fixed.x = c("ARStigma", "Relig", 
                                                          "MRNaf"))
sem.diss
summary(sem.diss)
pathDiagram(sem.diss)