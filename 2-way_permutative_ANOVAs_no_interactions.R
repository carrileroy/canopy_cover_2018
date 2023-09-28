# MSH Canopy study - 2018 Table 1 physiochemical data analysis
# Analyzed using 2-way Permutative ANOVAs - we tried paired t-tests (didn't work) and we didn't want to use 1-way only. 
# First set of analyses based on no replication across all Stream*Canopy sites (averaged across all replicate values), we didn't include the interaction term (not enough degrees of freedom)
# data in table1ave.csv [but none of these analyses were significant - not enough statistical power]
#
# Next set of analyses based on whatever replication we had (n = 3 or n = 6 mostly); data in table1ave.csv and table1.csv
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
#################
# First set of analyses based on average values across all sites (n = 2 per stream, n = 5 per canopy type)
#################
PPdata <- table1ave
#
#
# Permutative 2-way ANOVAs - no interactions (not enough replication!)
PPdata$stream = factor(PPdata$stream,
                     levels=unique(PPdata$stream))
PPdata$canopy = factor(PPdata$canopy,
                  levels=unique(PPdata$canopy))
#
# Set a random seed
set.seed(1431)
#
# % CC: Run a permutative 2-way ANOVA (Yvar~Xvar1*Xvar2, data=YourDataSet, perm="Prob")
fit1 <- aovp(CC~stream+canopy, data=PPdata, perm="Prob")
anova(fit1)
# n.s.
#
# Light - see separate analysis, measured every 15 minutes over 1 week
# Discharge (cms): Only n = 1 per stream, can't run a permutative 2-way ANOVA
#
# Bankfull width (m)
fit2 <- aovp(bankfull~stream+canopy, data=PPdata, perm="Prob")
anova(fit2)
# n.s.

# Wetted width (m)
fit3 <- aovp(wetted~stream+canopy, data=PPdata, perm="Prob")
anova(fit3)
# n.s.

# Slope %: Only n = 1 per stream, can't run a permutative 2-way ANOVA
# D50: Only n = 1 per stream, can't run a permutative 2-way ANOVA
# Water temp 2018 - see separate analysis, mesured hourly over whole study period
# Water temp 2019 - see separate analysis, mesured hourly over whole study period

# % Dry 2018 (# dry days / # of days in study)
fit4 <- aovp(dry18~stream+canopy, data=PPdata, perm="Prob")
anova(fit4)
# n.s.

# % Dry 2019 (# dry days / # of days in study)
fit5 <- aovp(dry19~stream+canopy, data=PPdata, perm="Prob")
anova(fit5)
# n.s.

# Dissolved oxygen (mg / L)
fit6 <- aovp(DO~stream+canopy, data=PPdata, perm="Prob")
anova(fit6)
# n.s.

# Conductivity (uS / cm)
fit7 <- aovp(cond~stream+canopy, data=PPdata, perm="Prob")
anova(fit7)
# n.s.

# pH
fit8 <- aovp(pH~stream+canopy, data=PPdata, perm="Prob")
anova(fit8)
# n.s.

# CDOM
fit9 <- aovp(CDOM~stream+canopy, data=PPdata, perm="Prob")
anova(fit9)
# n.s.
#################
# Second set of analyses based on all measurements per reach - only those variables that had replication
#################

PPdata <- table1
#
#
# Permutative 2-way ANOVAs - no interactions (not enough replication!)
PPdata$stream = factor(PPdata$stream,
                       levels=unique(PPdata$stream))
PPdata$canopy = factor(PPdata$canopy,
                       levels=unique(PPdata$canopy))
#
# Set a random seed
set.seed(1431)
#
# % CC: (n = 3) Run a permutative 2-way ANOVA (Yvar~Xvar1*Xvar2, data=YourDataSet, perm="Prob")
fit10 <- aovp(CC~stream+canopy, data=PPdata, perm="Prob")
anova(fit10)
# significant effect of canopy type (p < 0.0001), not stream
TukeyHSD(fit10, which = "canopy")
HSD.test(fit10, "canopy", group=TRUE)
out1 <- HSD.test(fit10, "canopy", alpha = 0.05, group = TRUE, main = NULL, unbalanced = TRUE, console = TRUE)
#
#Analysis of Variance Table							
#Response: CC							
#Df R 	Sum Sq R 	Mean Sq	Iter	Pr(Prob)     	F-ratios: Overall 	F(5,24) =	503.1276596
#stream     4      	670	167	2582	0.1491	1.776595745		
#canopy     1    	47127	47127	5000	<2e-16 ***	501.3510638		
#Residuals 24     	2256	94					
#---							
#  CC higher at closed canopy sites							
#Closed 	90.57333      a						
#Open   	11.30400      b						

#
# Light - see separate analysis, measured every 15 minutes over 1 week
# Discharge (cms): Only n = 1 per stream, can't run a permutative 2-way ANOVA
#
# Bankfull width (m) (n = 6) Run a permutative 2-way ANOVA (Yvar~Xvar1*Xvar2, data=YourDataSet, perm="Prob")
fit11 <- aovp(bankfull~stream+canopy, data=PPdata, perm="Prob")
anova(fit11)
# significant effect of canopy type (p < 0.0001), and stream (p < 0.0001)
TukeyHSD(fit11, which = "canopy")
HSD.test(fit11, "canopy", group=TRUE)
out2 <- HSD.test(fit11, "canopy", alpha = 0.05, group = TRUE, main = NULL, unbalanced = TRUE, console = TRUE)
TukeyHSD(fit11, which = "stream")
HSD.test(fit11, "stream", group=TRUE)
out2 <- HSD.test(fit11, "stream", alpha = 0.05, group = TRUE, main = NULL, unbalanced = TRUE, console = TRUE)
#
#Analysis of Variance Table							
#Response: bankfull							
#Df R 	Sum Sq R 	Mean Sq	Iter	Pr(Prob)    	F-ratios: Overall 	F(5,54) =	83.62909261
#stream     4   	126.5	31.626	5000	< 2.2e-16 ***	14.79232928		
#canopy     1   	147.17	147.173	5000	< 2.2e-16 ***	68.83676333		
#Residuals 54   	115.43	2.138					

#BF width greater at open canopy sites		
#Open   	5.616333      a	
#Closed 	2.484000      b	

#BF width:	across streams	
#Clear     	5.845833      a	
#Forsyth   	4.582500      a	
#Geo-West  	4.228333      a	
#Red Rock  	4.186667      a	
#Camp-East 	1.407500      b	


# Wetted width (m) (n = 6) Run a permutative 2-way ANOVA (Yvar~Xvar1*Xvar2, data=YourDataSet, perm="Prob")
fit12 <- aovp(wetted~stream+canopy, data=PPdata, perm="Prob")
anova(fit12)
# significant effect of stream (p < 0.0001), not canopy type
TukeyHSD(fit12, which = "stream")
HSD.test(fit12, "stream", group=TRUE)
out3 <- HSD.test(fit12, "stream", alpha = 0.05, group = TRUE, main = NULL, unbalanced = TRUE, console = TRUE)
#
#Analysis of Variance Table							
#Response: wetted							
#Df R 	Sum Sq R 	Mean Sq	 Iter 	Pr(Prob)    	F-ratios: Overall 	 F(5,54) =	9.550380518
#stream     4   	6.2066	1.55165	5000	<2e-16 ***	9.446879756		
#canopy     1   	0.017	0.017	51	0.7255	0.103500761		
#Residuals 54   	8.8693	0.16425					
#---							
#  Wetted width:							
#  Forsyth   	1.7116667      a						
# Red Rock  	1.3658333     ab						
# Clear     	1.3458333     ab						
# Camp-East 	1.2358333      b						
# Geo-West  	0.7166667      c						


# Slope %: Only n = 1 per stream, can't run a permutative 2-way ANOVA
# D50: Only n = 1 per stream, can't run a permutative 2-way ANOVA
# Water temp 2018 - see separate analysis, mesured hourly over whole study period
# Water temp 2019 - see separate analysis, mesured hourly over whole study period
# % Dry 2018 (# dry days / # of days in study), no replicate measures
# % Dry 2019 (# dry days / # of days in study), no replicate measures

# Dissolved oxygen (mg / L) (n = 3) Run a permutative 2-way ANOVA (Yvar~Xvar1*Xvar2, data=YourDataSet, perm="Prob")
fit13<- aovp(DO~stream+canopy, data=PPdata, perm="Prob")
anova(fit13)
# significant effect of stream (p < 0.0001), not canopy type
TukeyHSD(fit13, which = "stream")
HSD.test(fit13, "stream", group=TRUE)
out4 <- HSD.test(fit13, "stream", alpha = 0.05, group = TRUE, main = NULL, unbalanced = TRUE, console = TRUE)
#
#Analysis of Variance Table							
#Response: DO							
#Df R 	Sum Sq R 	Mean Sq 	Iter 	Pr(Prob)    	F-ratios: Overall 	F(5,24) =	399.380137
#stream     4   	46.489	11.6223	5000	<2e-16 ***	398.0239726		
#canopy     1    	0.04	0.0396	305	0.2492	1.356164384		
#Residuals 24    	0.7	0.0292					
#---							
#  DO:							
#  Red Rock  	12.033333      a						
# Forsyth   	11.820000      a						
# Geo-West   	9.706667      b						
# Camp-East  	9.655000      b						
# Clear      	8.973333      c						

# Conductivity (uS / cm) (n = 3) Run a permutative 2-way ANOVA (Yvar~Xvar1*Xvar2, data=YourDataSet, perm="Prob")
fit14 <- aovp(cond~stream+canopy, data=PPdata, perm="Prob")
anova(fit14)
# significant effect of stream (p < 0.0001), not canopy type
TukeyHSD(fit14, which = "stream")
HSD.test(fit14, "stream", group=TRUE)
out5 <- HSD.test(fit14, "stream", alpha = 0.05, group = TRUE, main = NULL, unbalanced = TRUE, console = TRUE)
#
#Analysis of Variance Table							
#Response: cond							
#Df R 	Sum Sq R 	Mean Sq Iter 		Pr(Prob)    	F-ratios: Overall 	F(5,24) =	480.2671233
#stream     4   	278637	69659	5000	< 2e-16 ***	477.1164384		
#canopy     1      	460	460	1523	0.06172	3.150684932		
#Residuals 24     	3509	146					
#---							
#  Conductivity:							
#  Clear     	289.03333      a						
# Geo-West  	215.98333      b						
# Camp-East 	164.03333      c						
# Forsyth    	53.11667      d						
# Red Rock   	35.18333      d						

# pH (n = 3) Run a permutative 2-way ANOVA (Yvar~Xvar1*Xvar2, data=YourDataSet, perm="Prob")
fit15 <- aovp(pH~stream+canopy, data=PPdata, perm="Prob")
anova(fit15)
# significant effect of stream (p < 0.0001), not canopy type
TukeyHSD(fit15, which = "stream")
HSD.test(fit15, "stream", group=TRUE)
out6 <- HSD.test(fit15, "stream", alpha = 0.05, group = TRUE, main = NULL, unbalanced = TRUE, console = TRUE)
#
#Analysis of Variance Table							
#Response: pH							
#Df R 	Sum Sq R 	Mean Sq Iter 		Pr(Prob)    	F-ratios: Overall 		12.76183383
#stream     4  	1.04171	0.260428	5000	<2e-16 ***	10.77573651		
#canopy     1  	0.048	0.048	684	0.1287	1.986097319		
#Residuals 24  	0.58003	0.024168					
#---							
#  pH;							
#Clear     	7.488333      a						
#Camp-East 	7.393333      a						
#Geo-West  	7.303333      a						
#Forsyth   	7.270000      a						
#Red Rock  	6.938333      b						

# CDOM (n = 3) Run a permutative 2-way ANOVA (Yvar~Xvar1*Xvar2, data=YourDataSet, perm="Prob")
fit16 <- aovp(CDOM~stream+canopy, data=PPdata, perm="Prob")
anova(fit16)
# significant effect of stream (p < 0.0001), not canopy type
TukeyHSD(fit16, which = "stream")
HSD.test(fit16, "stream", group=TRUE)
out7 <- HSD.test(fit16, "stream", alpha = 0.05, group = TRUE, main = NULL, unbalanced = TRUE, console = TRUE)
#
#Analysis of Variance Table							
#
#Response: CDOM							
#Df R 	Sum Sq R 	Mean Sq Iter 		Pr(Prob)    	F-ratios: Overall 	F(5,24) =	91.4558734
#stream     4   	6009.6	1502.41	5000	<2e-16 ***	91.44309191		
#canopy     1      	0.2	0.21	51	0.9608	0.012781497		
#Residuals 24    	394.3	16.43					
#---							
#  CDOM							
#Camp-East 	40.000000      a						
#Geo-West  	29.666667      b						
#Clear     	13.500000      c						
#Red Rock   	4.583333      d						
#Forsyth    	4.500000      d						

###############
# Bankfull - with interactions? 
##############
PPdata$dummy = factor(PPdata$dummy,
                       levels=unique(PPdata$dummy))
# Bankfull width (m) (n = 6) Run a permutative 2-way ANOVA (Yvar~Xvar1*Xvar2, data=YourDataSet, perm="Prob")
fit17 <- aovp(bankfull~stream*canopy, data=PPdata, perm="Prob")
anova(fit17)
# significant effect of canopy type (p < 0.0001), and stream (p < 0.0001)
fit18 <- aovp(bankfull~dummy, data=PPdata, perm="Prob")
anova(fit18)
TukeyHSD(fit18, which = "dummy")
HSD.test(fit18, "dummy", group=TRUE)
out2 <- HSD.test(fit18, "dummy", alpha = 0.05, group = TRUE, main = NULL, unbalanced = TRUE, console = TRUE)

#### Because Bankfull width is significant for both factors, ran with interaction: 							
#Analysis of Variance Table							
#Response: bankfull							
#Df R 	Sum Sq R 	Mean Sq	 Iter  	Pr(Prob)    			
#stream         4  	126.502	31.626	5000	< 2.2e-16 ***			
#  canopy         1  	147.173	147.173	5000	< 2.2e-16 ***			
#  stream:canopy  4   	82.169	20.542	5000	< 2.2e-16 ***			
#  Residuals     50   	33.258	0.665		

#bankfull groups	
#4  9.600000      a	clear-open
#6  5.933333      b	forsyth-open
#8  5.691667      b	geo-w-open
#10 5.175000      b	redrock-open
#5  3.231667      c	forsyth-closed
#9  3.198333      c	redrock-closed
#7  2.765000      c	geo-w-closed
#3  2.091667     cd	clear-closed
#2  1.681667     cd	camp-e open
#1  1.133333      d	camp-e closed

