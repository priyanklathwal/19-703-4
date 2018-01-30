##################################################################################
---
title: "Homework 1 Solution"
author: "Priyank Lathwal"
date: "Sunday, January 28, 2018"
output: pdf_document
---
##################################################################################

##################################################################################


SituationB <- function(pval){
  
  #Simulation with 30 observations
  Liking1 <- rnorm(30, 0, 1) # Generate thirty observations of Liking1 with mean = 0, var = 1
  Liking2 <- rnorm(30, 0, 1) # Generate thirty observations of Liking2 with mean = 0, var = 1
 
  #Two-Sample t-tests
  t.test1 <- t.test(Liking1[1:20], Liking2[1:20], var.equal = TRUE) # Two-sample t-test for checking statistical significance for first 20 observations
  t.test2 <- t.test(Liking1, Liking2, var.equal = TRUE) # Two-sample t-test for checking statistical significance for 30 observations
  #signif = 1 if at least one test gives p < pval, 0 otherwise
  signif <- ifelse(t.test1$p.value < pval  | #p value for first 20 observations
                     t.test2$p.value < pval, #p value for 30 observations
                   1, 0)
  return(signif) #value returned by the function
}

replicates <- replicate(15000, SituationB(.05))
SitBSim <- mean(replicates)
