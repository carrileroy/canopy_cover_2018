#MSH Canopy study - 2018 benthic invert and periphyton data
#Analyzed using 2-way Permutative ANOVAs - we tried paired t-tests (didn't work) and we didn't want to use 1-way only. 
#Based on no replication across all Stream*Canopy sites, we didn't include the interaction term (not enough degrees of freedom)
# data in PPdata.csv
#
if(!require(readxl)){install.packages("readxl")}
if(!require(car)){install.packages("car")}
if(!require(effects)){install.packages("effects")}
if(!require(dplyr)){install.packages("dplyr")}
if(!require(lmPerm)){install.packages("lmPerm")}
if(!require(agricolae)){install.packages("agricolae")}
#
library(readxl)
library(car) 
library(effects)
library(dplyr)
library(lmPerm)   #permutative analyses
library(agricolae) #HSD.test
#

str(PPdata)  #n=10 records

##################
# Permutative 2-way ANOVAs - no interactions (not enough replication!)
PPdata$Stream = factor(PPdata$Stream,
                     levels=unique(PPdata$Stream))
PPdata$Canopy = factor(PPdata$Canopy,
                  levels=unique(PPdata$Canopy))
#
# Set a random seed
set.seed(1431)
#
# Chla: Run a permutative 2-way ANOVA for %C (Yvar~Xvar1*Xvar2, data=YourDataSet, perm="Prob")
fit6 <- aovp(Chla~Stream+Canopy, data=PPdata, perm="Prob")
anova(fit6)
# n.s.: Canopy: F(1,4) = 3.61, p = 0.1677; Stream: F(4,4) = 3.39, p = 0.1131; 
#
# Invert.rich: Run a permutative 2-way ANOVA for %C (Yvar~Xvar1*Xvar2, data=YourDataSet, perm="Prob")
fit7 <- aovp(Invert.rich~Stream+Canopy, data=PPdata, perm="Prob")
anova(fit7)
# Stream is significant here - F(4,4) = 17.67, p = 0.0132, not Canopy (F(1,4) = 1.18, p = 0.3317)
TukeyHSD(fit7, which = "Stream")
#Geo-W not equal Camp-E; Redrock not equal Camp-E; Geo-W not equal Clear; Geo-W not equal Forsyth.
HSD.test(fit7, "Stream", group=TRUE)
out1 <- HSD.test(fit7, "Stream", alpha = 0.05, group = TRUE, main = NULL, unbalanced = TRUE, console = TRUE)
#Geo-W          26.5      a
#Camp-E         26.0      a
#Clear          21.5     ab
#Forsyth        20.0      b
#Redrock        19.0      b
#
# Invert.den: Run a permutative 2-way ANOVA for %C (Yvar~Xvar1*Xvar2, data=YourDataSet, perm="Prob")
fit8 <- aovp(Invert.den~Stream+Canopy, data=PPdata, perm="Prob")
anova(fit8)
# Stream is significant here - F(4,4) = 14.07, p = 0.0274, not Canopy (F(1,4) = 0.00, p = 0.999)
TukeyHSD(fit8, which = "Stream")
#Forsyth not equal Clear; Geo-W not equal Clear; Redrock not equal Clear
HSD.test(fit8, "Stream", group=TRUE)
out1 <- HSD.test(fit8, "Stream", alpha = 0.05, group = TRUE, main = NULL, unbalanced = TRUE, console = TRUE)
#Redrock    12535.0      a
#Forsyth     5164.0      b
#Camp-E      4658.5      b
#Geo-W       3938.5      b
#Clear        960.5      b
#
# Insect.bio: Run a permutative 2-way ANOVA for %C (Yvar~Xvar1*Xvar2, data=YourDataSet, perm="Prob")
fit9 <- aovp(Insect.bio~Stream+Canopy, data=PPdata, perm="Prob")
anova(fit9)
# n.s. Stream: F(4,4) = 3.54, p = 0.1226; Canopy: F(1,4) = 2.14, p = 0.1812
#
# Peri.rich: Run a permutative 2-way ANOVA for %C (Yvar~Xvar1*Xvar2, data=YourDataSet, perm="Prob")
fit10 <- aovp(Peri.rich~Stream+Canopy, data=PPdata, perm="Prob")
anova(fit10)
# n.s. Canopy: F(1,4) = 1.47, p = 0.3706; Stream: F(4,4) = 3.42, p = 0.0918; 
#
# Peri.Biovol.ln: Run a permutative 2-way ANOVA for %C (Yvar~Xvar1*Xvar2, data=YourDataSet, perm="Prob")
fit11 <- aovp(Peri.Biovol.ln~Stream+Canopy, data=PPdata, perm="Prob")
anova(fit11)
# Stream is significant here - F(4,4) = 7.26, p = 0.0466, not Canopy: F(1,4) = 2.02, p = 0.2362
TukeyHSD(fit11, which = "Stream")
HSD.test(fit11, "Stream", group=TRUE)
out1 <- HSD.test(fit11, "Stream", alpha = 0.05, group = TRUE, main = NULL, unbalanced = TRUE, console = TRUE)
#Camp-E          21.845      a
#Forsyth         21.010      a
#Clear           18.035      a
#Geo-W           12.245      a
#Redrock         11.615      a
#
# metric_ln: Run a permutative 2-way ANOVA for %C (Yvar~Xvar1*Xvar2, data=YourDataSet, perm="Prob")
fit12 <- aovp(metric_ln~Stream+Canopy, data=PPdata, perm="Prob")
anova(fit12)
# Stream is significant here - F(4,4) = 11.08, p = 0.0258, not Canopy (F(1,4) = 3.28, p = 0.1336)
TukeyHSD(fit12, which = "Stream")
HSD.test(fit12, "Stream", group=TRUE)
out1 <- HSD.test(fit12, "Stream", alpha = 0.05, group = TRUE, main = NULL, unbalanced = TRUE, console = TRUE)
#Forsyth  6.835361      a
#Redrock  6.621398      a
#Camp-E   6.465524     ab
#Geo-W    5.640295     ab
#Clear    5.153843      b
#
# SHpct.bio: Run a permutative 2-way ANOVA for %C (Yvar~Xvar1*Xvar2, data=YourDataSet, perm="Prob")
fit13 <- aovp(SHpct.bio~Stream+Canopy, data=PPdata, perm="Prob")
anova(fit13)
# n.s.
#
#######################
#Parametric tests: old code - not used. 
#
PPdata <- read_xlsx("InvertPeriphytonData.xlsx", sheet="Sites")
PPdata$Stream = factor(PPdata$Stream, levels=c("Camp-E","Clear","Geo-W","Forsyth","Redrock"))
PPdata$Canopy = factor(PPdata$Canopy, levels=unique(PPdata$Canopy))
str(PPdata)  #n=10 records
#
PPdata$metric = PPdata$Insect.bio
PPdata$metric_ln = log(PPdata$metric)
#
boxplot(metric~Canopy*Stream, data=PPdata)
boxplot(metric~Canopy, data=PPdata)
boxplot(metric~Stream, data=PPdata)
hist(PPdata$metric)
hist(PPdata$metric_ln)
qqPlot(PPdata$metric)
qqPlot(PPdata$metric_ln)
shapiro.test(PPdata$metric) # p<0.05 NOT normal
shapiro.test(PPdata$metric_ln)
#
#Invert.rich, Peri.rich = normal
#Invert.den, Insect.bio = normal, but ln-transformed is better
#Peri.biovol, Chla = NOT normal, ln-transformed normal
#
#
#Permutative 1-way ANOVA
lmpfit <- lmp(metric ~ Canopy+Stream, data=PPdata, perm="Prob")
summary(lmpfit) #overall model p-value and r-squared, only available with lmp command
Anova(lmpfit)
apfit <- aovp(metric ~ Canopy+Stream, data=PPdata, perm="Prob")
Anova(apfit)
summary(apfit) #same as 'anova(apfit)'
TukeyHSD(apfit, "Stream") #only available with aovp command
#
#
#Standard 1-way ANOVA - no replication for interaction
#aov or lm = exact same 2-way ANOVA/linear model
afit = aov(metric_ln ~ Canopy+Stream, data=PPdata)
Anova(afit) #default is Type II sum of squares
TukeyHSD(afit, "Stream") #only works with aov command (not lm)
fitlm = lm(metric_ln ~ Canopy+Stream, data=PPdata)
summary(fitlm) #overall model p-value and r-squared, only with lm command
plot(allEffects(afit))
par(mfrow=c(2,2))
plot(afit)  #Diagnostic plots
dev.off()
#
#######################
#Paired T-tests for canopy type differences WITHIN stream (metric)
#Sample size, mean, and standard deviation for each Canopy type
PPdata %>%
  group_by(Canopy) %>%
  summarise(
    count = n(),
    mean = mean(metric),
    sd = sd(metric)
  )
#Calculate metric differences between Open minus Closed Canopies
differences <- with(PPdata, metric[Canopy=="Open"] - metric[Canopy=="Closed"])
#Test *differences* for normal distribution
shapiro.test(differences)
#Paired T-test
t.test(metric ~ Canopy, data=PPdata, paired=TRUE)
#
#
#Paired T-test for canopy type differences WITHIN stream (chl-a)
#Sample size, mean, and standard deviation for each Canopy type
PPdata %>%
  group_by(Canopy) %>%
  summarise(
    count = n(),
    mean = mean(Chla),
    sd = sd(Chla)
  )
#Calculate Chla differences between Open minus Closed Canopies
differences <- with(PPdata, Chla[Canopy=="Open"] - Chla[Canopy=="Closed"])
#Test *differences* for normal distribution
shapiro.test(differences)
#Paired T-test
t.test(Chla ~ Canopy, data=PPdata, paired=TRUE)
#
#
#Paired T-test for canopy type differences WITHIN stream (Peri.Biovol.ln)
#Sample size, mean, and standard deviation for each Canopy type
PPdata %>%
  group_by(Canopy) %>%
  summarise(
    count = n(),
    mean = mean(Peri.Biovol.ln),
    sd = sd(Peri.Biovol.ln)
  )
#Calculate Peri.Biovol.ln differences between Open minus Closed Canopies
differences <- with(PPdata, Peri.Biovol.ln[Canopy=="Open"] - Peri.Biovol.ln[Canopy=="Closed"])
#Test *differences* for normal distribution
shapiro.test(differences)
#Paired T-test
t.test(Peri.Biovol.ln ~ Canopy, data=PPdata, paired=TRUE)
#
#
#Paired T-test for canopy type differences WITHIN stream (Invert.rich)
#Sample size, mean, and standard deviation for each Canopy type
PPdata %>%
  group_by(Canopy) %>%
  summarise(
    count = n(),
    mean = mean(Invert.rich),
    sd = sd(Invert.rich)
  )
#Calculate Invert.rich differences between Open minus Closed Canopies
differences <- with(PPdata, Invert.rich[Canopy=="Open"] - Invert.rich[Canopy=="Closed"])
#Test *differences* for normal distribution
shapiro.test(differences)
#Paired T-test
t.test(Invert.rich ~ Canopy, data=PPdata, paired=TRUE)
#
#
#Paired T-test for canopy type differences WITHIN stream (Invert.den)
#Sample size, mean, and standard deviation for each Canopy type
PPdata %>%
  group_by(Canopy) %>%
  summarise(
    count = n(),
    mean = mean(Invert.den),
    sd = sd(Invert.den)
  )
#Calculate Invert.den differences between Open minus Closed Canopies
differences <- with(PPdata, Invert.den[Canopy=="Open"] - Invert.den[Canopy=="Closed"])
#Test *differences* for normal distribution
shapiro.test(differences)
#Paired T-test
t.test(Invert.den ~ Canopy, data=PPdata, paired=TRUE)
#
#
#Paired T-test for canopy type differences WITHIN stream (Insect.bio)
#Sample size, mean, and standard deviation for each Canopy type
PPdata %>%
  group_by(Canopy) %>%
  summarise(
    count = n(),
    mean = mean(Insect.bio),
    sd = sd(Insect.bio)
  )
#Calculate Insect.bio differences between Open minus Closed Canopies
differences <- with(PPdata, Insect.bio[Canopy=="Open"] - Insect.bio[Canopy=="Closed"])
#Test *differences* for normal distribution
shapiro.test(differences)
#Paired T-test
t.test(Insect.bio ~ Canopy, data=PPdata, paired=TRUE)
#
#
#Paired T-test for canopy type differences WITHIN stream (Peri.rich)
#Sample size, mean, and standard deviation for each Canopy type
PPdata %>%
  group_by(Canopy) %>%
  summarise(
    count = n(),
    mean = mean(Peri.rich),
    sd = sd(Peri.rich)
  )
#Calculate Peri.rich differences between Open minus Closed Canopies
differences <- with(PPdata, Peri.rich[Canopy=="Open"] - Peri.rich[Canopy=="Closed"])
#Test *differences* for normal distribution
shapiro.test(differences)
#Paired T-test
t.test(Peri.rich ~ Canopy, data=PPdata, paired=TRUE)