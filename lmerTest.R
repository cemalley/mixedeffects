library(lmerTest)
library(data.table)
library(readxl)
library(sjPlot)
data <- as.data.table(read_excel('~/AREDS2/NEI_Chew_Cirrus_CC_FD_Statistics_Summary.xls'))

sev <- fread('~/AREDS2/base_scale.csv')
sev[Scale0==88,Scale0:=NA]
sev[,Sev:=ifelse(Scale0 == 1, 'none', Scale0)] # no controls present here.
sev[,Sev:=ifelse(Scale0 %in% (2:4), 'early',Sev)]
sev[,Sev:=ifelse(Scale0 %in% (5:8), 'int',Sev)]
sev[,Sev:=ifelse(Scale0 %in% (9:11), 'late',Sev)]
sev$PATID <- as.character(sev$PATID)
table(sev$Scale0, sev$Sev)

data[,EYE:=tstrsplit(Eye,'_')[3]]
data[,EYE:=ifelse(EYE=='OD','R','L')]
data[,PATID:=tstrsplit(Eye,'_')[2]]
data[,PATID:=as.character(as.integer(PATID) )]
data <- merge(data, sev, by=c('EYE','PATID'), all.x=T, all.y=F)
names(data)[4:8] <- c('FD_mean_perc','FD_mean_size','FD_median_size','FD_max_size','FD_95th_size')
table(data$PATID)# 5 people only have one eye of data here. the rest have both eyes.

demog <- fread('~/AREDS2/demog.csv')
demog$PATID <- as.character(demog$PATID)
data <- merge(data, demog, by='PATID')
data$treat <- as.character(data$treat)
data$male <- as.character(data$male)
data$edu <- as.character(data$edu)
data$smkever <- as.character(data$smkever)

anova(lm(FD_mean_perc ~ age, data=data)) # age is not significant!

anova(lm(FD_mean_perc ~ male, data=data)) #
anova(lm(FD_mean_perc ~ treat, data=data)) #
anova(lm(FD_mean_perc ~ edu, data=data)) #
anova(lm(FD_mean_perc ~ smkever, data=data)) # smoking is significant

# model------

#1. 
fit <- lmerTest::lmer(FD_mean_perc ~ Sev + age + male + smkever + (1|PATID), data=data)


#2. 
fit <- lmerTest::lmer(FD_mean_size ~ Sev + age + male + smkever + (1|PATID), data=data)

#3.
fit <- lmerTest::lmer(FD_median_size ~ Sev + age + male + smkever + (1|PATID), data=data)


#4. 
fit <- lmerTest::lmer(FD_max_size ~ Sev + age + male + smkever + (1|PATID), data=data)

#5. 
fit <- lmerTest::lmer(FD_95th_size ~ Sev + age + male + smkever + (1|PATID), data=data)


summary(fit)

conf_intervals <- confint(fit, method = "profile", parm = "beta_", level = 0.95)

conf_intervals


