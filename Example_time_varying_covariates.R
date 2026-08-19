library("tmle")
library("tmle3")
library("sl3")
library("SuperLearner")
library("survival")
library("survminer")
library("gtsummary")
library("LtAtStructuR")
library("ggplot2")
library("tidyverse")

source("NICE/helper_functions.R")

set.seed(2025)

## Set the number of intervals
K = 10

## Generate data
ObsData <- generateData2(n=10000, ntime = K)

## Arrange data in the suitable to ltmle form
col.seq = c("L","A",paste0("C1"),paste0("L1"),paste0("Y1"))
for (i in 2:K) col.seq = c(col.seq,c(paste0("C",i),paste0("L",i),paste0("Y",i)))
dat.ltmle = ObsData[,col.seq]

## Convert censoring indicator to factor variable with levels "censored"/"uncensored"
for (i in 1:K) dat.ltmle[,paste0("C",i)] = BinaryToCensoring(is.censored=dat.ltmle[,paste0("C",i)])

## Estimate survival probabilities at each time point t=1:K and store the results
for (i in 1:K) assign(paste0("result",i), ltmle(dat.ltmle[,col.seq[c(1,2,3:(2+3*i))]],
                                                Anodes = "A",
                                                Cnodes = paste0("C",1:i),
                                                Ynodes = paste0("Y",1:i),
                                                Lnodes = paste0("L",1:i),
                                                #SL.library = list(Q = SL.lib, g = SL.lib)
                                                survivalOutcome = TRUE,
                                                abar=list(1,0)))

# one can use a Super Learner, by specifying the library, e.g. SL.lib <- c("SL.glm", "SL.stepAIC", "SL.nnet", "SL.gam", "SL.glmnet")
# and providing it as an input to ltmle for argument SL.library

## Create a dataframe with estimated survival probabilities and 95% confidence intervals
surv_proba = data.frame(A=0, proba = 1)
for (i in 1:K) surv_proba = rbind(surv_proba, c(0,1 - summary(get(paste0("result",i)))$effect.measures$control$estimate))
surv_proba = rbind(surv_proba,c(1,1))
for (i in 1:K) surv_proba = rbind(surv_proba, c(1,1 - summary(get(paste0("result",i)))$effect.measures$treatment$estimate))
surv_proba$LB = 1
surv_proba$UB = 1
for (i in 1:K) surv_proba[i+1,3:4] = c(surv_proba$proba[i+1] - qnorm(0.975)*summary(get(paste0("result",i)))$effect.measures$control$std.dev,
                                     surv_proba$proba[i+1] + qnorm(0.975)*summary(get(paste0("result",i)))$effect.measures$control$std.dev)
for (i in 1:K) surv_proba[K+i+2,3:4] = c(surv_proba$proba[K+i+2] - qnorm(0.975)*summary(get(paste0("result",i)))$effect.measures$treatment$std.dev,
                                     surv_proba$proba[K+i+2] + qnorm(0.975)*summary(get(paste0("result",i)))$effect.measures$treatment$std.dev)

## Plot the estimated survival curves with confidence intervals
ggplot(data=surv_proba,aes(x=rep(c(0,1:K),2),y=proba, col=as.character(A), fill=as.character(A))) +    
  geom_line(lwd=1) + 
  geom_ribbon(aes(ymin = LB, ymax = UB),alpha = 0.2,linetype=2) + 
  ylim(c(0,1)) + 
  scale_colour_discrete(name="A", labels=c("0", "1")) +
  scale_fill_discrete(name="A", labels=c("0", "1")) +
  xlab("time") +
  ylab("Survival probability")


## Compute estimates of Risk Ratio and CIs
RR <- data.frame(time = 1, 
                 "estimated RR" = summary(get(paste0("result",i)))$effect.measures$RR$estimate,
                 "CI, lower bound" = summary(get(paste0("result",i)))$effect.measures$RR$CI[1],
                 "CI, upper bound" = summary(get(paste0("result",i)))$effect.measures$RR$CI[2])
for (i in 2:K) RR = rbind(RR, c(i,
                                "estimated RR" = summary(get(paste0("result",i)))$effect.measures$RR$estimate,
                                "CI, lower bound" = summary(get(paste0("result",i)))$effect.measures$RR$CI[1],
                                "CI, upper bound" = summary(get(paste0("result",i)))$effect.measures$RR$CI[2]))

RR
