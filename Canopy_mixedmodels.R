#MSH Canopy Cover study data for Mixed Model statistics
#10 reaches: 5 streams x 2 canopy types
#Excel sheet "habitat"
#2018 light (lux) data, n=28, 3/reach (30 loggers, 2 failed NA's)
#2018 water quality & canopy cover data, n=30 (DO,Cond,pH,CDOM,CC)
#Excel sheet "widths"
#2018 bankfull & wetted widths, n=60, 6/reach
#Excel sheet "canvas-season" 
#2019 canvas strips/season, n=200, 10/reach, 2 seasons
#Excel sheet "canvas-location" 
#2019 canvas strips/location, n=151, 10/reach/Stream & 5/reach/Riparian

setwd("C:/Users/sclaeson/Documents/MSH research/PP streams 2018-2019/CanopyCover paper/Stats_MM")
getwd()

library(readxl)
library(car) # Anova p-values
library(MASS)
library(effects) # Model plot all effects
library(moments) # skewness
library(lme4) # mixed models
library(multcomp) # multi-contrasts adj p-values
library(gmodels) # 'estimable' contrasts raw p-values

PPdata <- read_xlsx("Canopy_data_SC.xlsx", sheet="habitat", na="na")
PPdata$Stream = factor(PPdata$Stream, levels=c("Camp-E","Clear","Forsyth","Geo-W","Redrock"))
PPdata$Canopy = factor(PPdata$Canopy, levels=c("Open","Closed"))
PPdata$Dummy = factor(PPdata$Dummy)
PPdata$Replicate = factor(PPdata$Replicate)
str(PPdata)

#subset canvas data
PPdata = subset(PPdata, Season=="Summer")
PPdata = subset(PPdata, Lcoation=="Riparian")

PPdata$metric = PPdata$Lux.davg
PPdata$metric.ln = log(PPdata$metric) #ln, CANNOT have zeros (CC, tslossd)
PPdata$metric.as = asin(sqrt(PPdata$metric))
#arcsine square-root, ok with zeros, data must be btwn 0-1 (CC, tslossd.p)


qqPlot(PPdata$metric)
qqPlot(PPdata$metric.ln)
qqPlot(PPdata$metric.as)
shapiro.test(PPdata$metric) #test of homogeneous variances
shapiro.test(PPdata$metric.ln)
shapiro.test(PPdata$metric.as)
hist(PPdata$metric)
hist(PPdata$metric.ln)
hist(PPdata$metric.as)
skewness(PPdata$metric, na.rm=TRUE)
skewness(PPdata$metric.ln, na.rm=TRUE) # <1 for normal data
boxplot(metric~Canopy*Stream, data=PPdata)


#Linear mixed model with within reach Replicates as random
lmm1 = lmer(metric.ln ~ Canopy*Stream + (1|Replicate), data=PPdata)
Anova(lmm1)
summary(lmm1)
plot(allEffects(lmm1))
plot(lmm1) #residuals

# Tukey pairwise comparisons, p-values adj for multiple comparisons
lmm1 = lmer(metric.ln ~ Canopy+Stream + (1|Replicate), data=PPdata)
tuk.s = glht(lmm1, linfct=mcp(Stream="Tukey"))
summary(tuk.s)

# Dummy = Canopy*Stream
lmm.d = lmer(metric.ln ~ Dummy + (1|Replicate), data=PPdata)
plot(allEffects(lmm.d))
tuk.d = glht(lmm.d, linfct=mcp(Dummy="Tukey"))
summary(tuk.d) # p-values adj for multiple comparisons but more than desired

# Specific contrast tests, coef only correct with NO intercept model
SC.tests = rbind("Camp-E open-clsd" =c(1,-1,0,0,0,0,0,0,0,0), 
             "Clear open-clsd"     =c(0,0,1,-1,0,0,0,0,0,0), 
             "Forsyth open-clsd"   =c(0,0,0,0,1,-1,0,0,0,0), 
             "Geo-W open-clsd"     =c(0,0,0,0,0,0,1,-1,0,0), 
             "Redrock open-clsd"   =c(0,0,0,0,0,0,0,0,1,-1))

# LMM model with NO intercept
lmm.noi = lmer(metric.ln ~ Dummy-1 + (1|Replicate), data=PPdata)
# Specific contrasts - p-values are adj for multiple comparisons
SC.mcp = glht(lmm.noi, linfct=SC.tests)
summary(SC.mcp)

#Generalized Linear mixed model
# family=gaussian(link="log") for ln-transformed means
# family=gaussian(link="identity") for untransformed normal data (e.g. Wetted)
#   This is exact same as LMM
# family=poisson(link="log") for count data (e.g. Lux ?)

#Logistic regression for % data, response=cbind(#successes,#failures)
glmm.1 = glmer(cbind(PPdata$CC.p, PPdata$CC.a) ~ Canopy+Stream + (1|Replicate),
                family=binomial, data=PPdata)

glmm1 = glmer(metric ~ Canopy+Stream + (1|Replicate), 
              family=gaussian(link="log"), data=PPdata)
Anova(glmm1)
summary(glmm1)
plot(allEffects(glmm1))
plot(glmm1) #residuals

# Tukey pairwise comparisons, p-values adj for multiple comparisons
tuk = glht(glmm.1, linfct=mcp(Stream="Tukey"))
summary(tuk)

#Generalized - negative binomial distribution
nb1 = glmer.nb(metric ~ Canopy+Stream + (1|Replicate), data=PPdata)
Anova(nb1)
summary(nb1)
plot(allEffects(nb1))
plot(nb1) #residuals

#Linear model pseudo-replication of within reach replicates
LM1=lm(metric.ln~Canopy+Stream, data=PPdata)
Anova(LM1) #type="II" default
summary(LM1)
plot(allEffects(LM1))
par(mfrow=c(2,2))
plot(LM1)  #Diagnostic plots
dev.off()


rm(list=ls())
