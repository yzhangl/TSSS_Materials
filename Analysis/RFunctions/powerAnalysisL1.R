powerAnalysis.L1Effect<-function(model,effect,sampleSize,clusterSize,pL12,newClusterSizes,newSampleSizes){
  
## model: a lme4 model object
## effect: a string for the name of the effect to be analyzed
## sampleSize: number of subjects in the pilot study
## clusterSize: number of trials per condition in the pilot study
## pL12: number of interaction between within-subject and between-subject variables
## newClusterSizes: a list of new numbers of trials per condition
## newSampleSizes: a list of new numbers of subjects

  require(lme4)
  require(dplyr)
  require(pwr)
  fixed.effect <- abs(fixef(model)[[effect]])
  random.var <- as.data.frame(VarCorr(model)) %>% filter(is.na(var2),var1==effect)
  random.var <- random.var$vcov
  t.value <- coef(summary(model))[effect,'t value']
  se.effect <- fixed.effect/t.value
  var.ratio <- (se.effect * se.effect * sampleSize - random.var) * clusterSize
  pwrt.df <- data.frame(clusterSize=c(),sampleSize=c(),power=c(),d=c())
  for(newClusterSize in newClusterSizes)
  {
    new.se <- sqrt((random.var + var.ratio/newClusterSize)/sampleSize)
    
    new.t <- fixed.effect/new.se
    new.d <- new.t/sqrt(sampleSize - pL12)
    pwrt <- pwr.t.test(d=new.d,n=newSampleSizes,sig.level=.05,type="one.sample",alternative="two.sided")
    temp<-data.frame(clusterSize=newClusterSize,
                     sampleSize=pwrt$n,
                     power=pwrt$power,
                     d=new.d)
    pwrt.df<-rbind(pwrt.df,temp)
  }
  return(pwrt.df)
}