set.seed(20)
library(data.table);library(readxl);library(foreign);library(MASS);library(Hmisc);library(lme4);library(lmerTest)
mydata <- fread('~/Proteomics/imput.t.csv')

results <- data.table()

for (i in 8:1689){
  tryCatch({
  id = colnames(mydata)[i]
  
  mydata$male <- as.character(mydata$male)
  mydata$batch <- as.factor(mydata$batch)
  mydata$batch <- relevel(mydata$batch, ref='original')
  
  fit <- lmerTest::lmer(formula(paste0("amd2_person ~ ",id," + age + male + (1 | TIME_POINT)")), data=mydata,
                        control=lmerControl(check.nobs.vs.nRE="warning"))
  
  summary(fit)
  
  ORs <- exp(summary(fit)$coefficients[id,1] + qnorm(c(0.025,0.5,0.975)) * summary(fit)$coefficients[id,3])
  
  myresults <- data.table(
    id       = id,
    estimate = signif(coef(summary(fit))[id, "Estimate"], digits = 4),
    OR = signif(ORs[2], digits = 4),
    CIlow = signif(ORs[1], digits=4),
    CIhigh = signif(ORs[3], digits=4),
    pvalue   = signif(coef(summary(fit))[id, "Pr(>|t|)"], digits = 4),
    age_p = signif(coef(summary(fit))['age', "Pr(>|t|)"], digits = 4),
    sex_p = signif(coef(summary(fit))['male1', "Pr(>|t|)"], digits = 4)
  )
  
  print(paste0(i, '(',id,')'))
  
  results <- rbind(myresults, results)
  }, error=function(msg){
    return(NA)
  })
}
