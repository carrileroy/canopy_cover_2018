# Carri J. LeRoy, 8-13-20, 3-30-23
#
# 2018 canopy cover paper: Paired whisker plots - Figure 2
# data in light.csv
#
# install required packages for blocked ANOVA
if(!require(lmPerm)){install.packages("lmPerm")}
if(!require(psych)){install.packages("psych")}
if(!require(FSA)){install.packages("FSA")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(car)){install.packages("car")}
if(!require(multcompView)){install.packages("multcompView")}
if(!require(lsmeans)){install.packages("lsmeans")}
if(!require(tidyr)){install.packages("tidyr")}
if(!require(dplyr)){install.packages("dplyr")}
if(!require(magrittr)){install.packages("magrittr")}
if(!require(gridExtra)){install.packages("gridExtra")}
if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(readxl)){install.packages("readxl")}
if(!require(devtools)) install.packages("devtools")
if(!require(ggpubr)) install.packages("ggpubr")
# Call the packages
library(lmPerm)           # permutative analyses
library(psych)            # stats for psych research (useful)
library(FSA)              # Fisheries stock assessment methods
library(multcompView)     # Visualizations of paired comparisons
library(lsmeans)          # least-squared means
library(tidyr)          	# data re-shaping
library(ggplot2)        	# plotting & data
library(dplyr)          	# data manipulation
library(magrittr)       	# pipe operator
library(gridExtra)     	  # provides side-by-side plotting
library(car)     		      # companion to applied regression
library(tidyverse)        # tidyverse
library(readxl)           # reads Excel files into R
library(devtools)
library(ggpubr)
#
#
# Starting with variable Lux
#
#  Order factors by the order in data frame
#  Otherwise, R will alphabetize them
#
light$stream = factor(light$stream,
                       levels=unique(light$stream))
light$site = factor(light$site,
                     levels=unique(light$site))
light$dummy = factor(light$dummy, 
                     levels=unique(light$dummy))

#Check data structure: R needs to treat your grouping variables as "factors" not "numbers"
str(light)
#
#

# Now, create a nice box and whisker plot for Lux: 
##
## New plot 3-29-23 based on Shannon's updated LMM analysis
##
fig2 <- ggboxplot(light, x = "stream", y = "lux", fill = "site") +
  scale_fill_manual(values = c("white", "gray50")) +
  labs(x = "Stream", 
       y = "Mean daily light intensity (lux)") +
  ylim(0, 85000) +
  theme(legend.position = c(0.15, 0.95)) +
  theme(axis.text.x = element_text(angle=90)) +
  theme(text = element_text(size = 15))+
  theme(axis.line = element_line(size=0.5, colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank()) +
  theme(legend.title=element_blank()) +
  theme(legend.background = element_rect(fill="white", size=0.5)) +
  theme(legend.text = element_text(colour="black", size = 15)) +
  geom_text(size=6, x=1.0, y=63000, label="*") +
  geom_text(size=6, x=2.0, y=47000, label="*") +
  geom_text(size=6, x=3.0, y=80000, label="*") +
  geom_text(size=6, x=4.0, y=65000, label="*") +
  geom_text(size=6, x=5.0, y=62000, label="*")
fig2
#
ggsave("Fig2-final.pdf", width=18.2, height=15.12, units="cm", dpi=600)
ggsave(fig2, file="Fig2.eps", width=18.2, height=15.12, units="cm", dpi=600, device="eps")
#
###############################
# OLD PLOTS: 
fig2 <- ggboxplot(light, x = "stream", y = "lux", fill = "site") +
  scale_fill_manual(values = c("white", "gray50")) +
  labs(x = "Stream", 
       y = "Mean daily light intensity (lux)") +
  ylim(0, 85000) +
  theme(legend.position = c(0.15, 0.95)) +
  theme(axis.text.x = element_text(angle=90)) +
  theme(text = element_text(size = 15))+
  theme(axis.line = element_line(size=0.5, colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank()) +
  theme(legend.title=element_blank()) +
  theme(legend.background = element_rect(fill="white", size=0.5)) +
  theme(legend.text = element_text(colour="black", size = 15)) +
  geom_text(size=4, x=0.8, y=63000, label="ab") +
  geom_text(size=4, x=1.2, y=33000, label="cd") +
  geom_text(size=4, x=1.8, y=47000, label="bc") +
  geom_text(size=4, x=2.2, y=22000, label="d") +
  geom_text(size=4, x=2.8, y=80000, label="a") +
  geom_text(size=4, x=3.2, y=15000, label="d") +
  geom_text(size=4, x=3.8, y=65000, label="a") +
  geom_text(size=4, x=4.2, y=12000, label="d") +
  geom_text(size=4, x=4.8, y=62000, label="ab") +
  geom_text(size=4, x=5.2, y=15000, label="d")
fig2
#
ggsave("Fig2-update.pdf", width=18.2, height=15.12, units="cm", dpi=600)
ggsave(fig2, file="Fig2.eps", width=18.2, height=15.12, units="cm", dpi=600, device="eps")
#
#
#
## Old plots - when it was 2 panels (lux and air temp)
plotA <- ggboxplot(light, x = "stream", y = "lux", fill = "site") +
  scale_fill_manual(values = c("yellow1", "green3")) +
  labs(tag = "A", x = element_blank(), 
       y = "Mean daily light intensity (Lux)") +
  theme(legend.position = c(0.12, 0.9)) +
  theme(text = element_text(size = 20))+
  theme(axis.line = element_line(size=0.5, colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank(), axis.text.x = element_blank()) +
  theme(legend.title=element_blank()) +
  theme(legend.background = element_rect(fill="white", size=0.5)) +
  theme(legend.text = element_text(colour="black", size = 20))
plotA
#
# 

###############################################
# OLD CODE: 
#Run 2-way Permutative ANOVA:
fit15 <- aovp(lux ~ stream*site, data=light)
summary(fit15)

# ANOVA: F(9,17) = 	310.53
#               Df   R Sum Sq  R    Mean Sq     Iter    F-ratio     Pr(Prob)    
# stream        4     7.3471e+08    1.8368e+08  5000    4.23        0.0142 *  
#  site         1     1.3090e+10    1.3090e+10  5000    301.67      <2e-16 ***
#  stream:site  4     8.0360e+08    2.0090e+08  5000    4.63        0.0124 *  
#  Residuals    17    7.3766e+08    4.3392e+07                  
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Run Tukey HSD on results
TukeyHSD(fit15, which = "stream:site")

# Then run it using a dummy variable to force the Tukey test: 
fit16 <- aovp(lux ~ dummy, data = light)
summary(fit16)
#
# TukeyHSD
TukeyHSD(fit16)
#
HSD.test(fit16, "dummy", group=TRUE)
out1 <- HSD.test(fit16, "dummy", alpha = 0.05, group = TRUE, main = NULL, unbalanced = TRUE, console = TRUE)

#lux groups	
#1  56461.000     ab		Camp-E	Open	summer
#2  17774.000     cd		Camp-E	Closed	summer
#3  36792.000     bc		Geo-W	Open	summer
#4   9812.000      d		Geo-W	Closed	summer
#5  65339.000      a		Clear	Open	summer
#6   7756.333      d		Clear	Closed	summer
#7  58712.500      a		Forsyth	Open	summer
#8   6478.000      d		Forsyth	Closed	summer
#9  53980.500     ab		Redrock	Open	summer
#10  5460.000      d		Redrock	Closed	summer



#moving on to Temp: variable = air.temp
#
light$stream = factor(light$stream,
                      levels=unique(light$stream))
light$site = factor(light$site,
                    levels=unique(light$site))
#Check data structure: R needs to treat your grouping variables as "factors" not "numbers"
str(light)
# Testing for homogeneity of variances:
leveneTest(air.temp ~ site, light, center=mean)
# Testing for normality: first, subset the dataset for just one treatment (open)
open.C <- subset(light, site=="Open")
# then run Shapiro-Wilks test: Open sites pass normality test!
shapiro.test(open.C$air.temp)
# Now, repeat for closed: Closed sites also pass normality test!
closed.C <- subset(light, site=="Closed")
shapiro.test(closed.C$air.temp)
# Ln-transformed air.temp in Excel, now Shapiro-Wilks' test for closed: 
closed.C <- subset(light, site=="Closed")
shapiro.test(closed.C$ln.temp)
#
#Run 2-way ANOVA:
res.aov3 <- aov(ln.temp ~ stream * site, data = light)
summary(res.aov3)
# For unbalanced designs, this one is better - type III: I went with this one...
my_anova <- aov(ln.temp ~ stream * site, data = light)
Anova(my_anova, type = "III")
TukeyHSD(my_anova, which = "stream:site")
# Now, create a nice box and whisker plot for Air temp: 
plotB <- ggboxplot(light, x = "stream", y = "air.temp", fill = "site") +
  scale_fill_manual(values = c("yellow1", "green3")) +
  labs(tag = "B", x = "Stream", 
       y = expression("Mean daily air temperature (C)")) +
  theme(legend.position = c(0.12, 0.9)) +
  theme(text = element_text(size = 20))+
  theme(axis.line = element_line(size=0.5, colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank()) +
  theme(legend.title=element_blank()) +
  theme(legend.background = element_rect(fill="white", size=0.5)) +
  theme(legend.text = element_text(colour="black", size = 20))
plotB
#
# Arrange plot A and B in one column:
gA <- ggplotGrob(plotA)
gB <- ggplotGrob(plotB)
grid::grid.newpage()
grid::grid.draw(rbind(gA, gB))
#
# Export as pdf (portrait)
#
# Old tests for normality and equality of variance
# Testing for homogeneity of variances:
leveneTest(lux ~ site, light, center=mean)
# Oops - not equal variances, need to try ln-transformation
# Testing for normality: first, subset the dataset for just one treatment (open)
open.lux <- subset(light, site=="Open")
# then run Shapiro-Wilks test: Open sites pass normality test!
shapiro.test(open.lux$lux)
# Now, repeat for closed: Closed sites DO NOT pass normality test...
closed.lux <- subset(light, site=="Closed")
shapiro.test(closed.lux$lux)
# Ln-transformed lux in Excel, now re-run Levene's test: Good!
leveneTest(ln.lux ~ site, light, center=mean)
# Re-run Shapiro-Wilk's test: Good!
closed.lux <- subset(light, site=="Closed")
shapiro.test(closed.lux$ln.lux)
#Run 2-way ANOVA:
res.aov3 <- aov(ln.lux ~ stream*site, data=light)
summary(res.aov3)
# For unbalanced designs, this one is better - type III: I went with this one...
my_anova <- aov(ln.lux ~ stream * site, data = light)
Anova(my_anova, type = "III")