# Carri J. LeRoy, 10-26-22, 3-30-23
#
# 2018 canopy cover paper: Canvas strips final 3 figures, 5A, 5B, 5C. 
# Data in canvas.csv and rip.riparian.csv
# Update R and RStudio: 
install.packages("installr")
library(installr)
updateR()

###############
#Figures 5a, 5b, 5c
###############
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
if(!require(devtools)){install.packages("devtools")}
if(!require(ggpubr)){install.packages("ggpubr")}
#
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
# Make sure your grouping variables are classified as factors, not characters and not numbers
# This also orders your factors as they are in the data table, otherwise R with alphabetize them. 
canvas$stream = factor(canvas$stream,
                       levels=unique(canvas$stream))
canvas$site = factor(canvas$site,
                     levels=unique(canvas$site))
rip.riparian$stream = factor(rip.riparian$stream,
                             levels=unique(rip.riparian$stream))
rip.riparian$site = factor(rip.riparian$site,
                           levels=unique(rip.riparian$site))
#
#Now make a plot in grayscale - success!
#
#
plotA <- ggboxplot(canvas, x = "stream", y = "SH2.TSloss.d", fill = "site") +
  scale_fill_manual(values = c("white", "gray50")) +
  labs(tag = "a", x = "Stream", 
       y = "%Tensile strength loss / day") +
  ylim(0, 3) +
  theme(legend.position = c(0.25, 0.9)) +
  theme(axis.text.x = element_text(angle=90)) +
  theme(text = element_text(size = 10))+
  theme(axis.line = element_line(size=0.5, colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank()) +
  theme(legend.title=element_blank()) +
  theme(legend.background = element_rect(fill="white", size=0.5)) +
  theme(legend.text = element_text(colour="black", size = 10)) +
  geom_text(size=4, x=2.0, y=1.6, label="*") +
  geom_text(size=4, x=3.0, y=1.5, label="*") +
  geom_text(size=4, x=4.0, y=2.9, label="*") +
  geom_text(size=4, x=5.0, y=2.0, label="*")
plotA
#
#
plotB <- ggboxplot(canvas, x = "stream", y = "FH2.TSloss.d", fill = "site") +
  scale_fill_manual(values = c("White", "gray50")) +
  labs(tag = "b", x = "Stream", 
       y = element_blank()) +
  ylim(0, 3) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle=90)) +
  theme(text = element_text(size = 10))+
  theme(axis.line = element_line(size=0.5, colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank(),
        axis.text.y = element_blank()) +
  theme(legend.title=element_blank()) +
  theme(legend.background = element_rect(fill="white", size=0.5)) +
  theme(legend.text = element_text(colour="black", size = 10)) +
  geom_text(size=4, x=3.0, y=0.6, label="*")
plotB
#
#
plotC <- ggboxplot(rip.riparian, x = "stream", y = "TSloss.d", fill = "site") +
  scale_fill_manual(values = c("white", "gray50")) +
  labs(tag = "c", x = "Stream", 
       y = element_blank()) +
  ylim(0, 3) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle=90)) +
  theme(text = element_text(size = 10))+
  theme(axis.line = element_line(size=0.5, colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank(),
        axis.text.y = element_blank()) +
  theme(legend.title=element_blank()) +
  theme(legend.background = element_rect(fill="white", size=0.5)) +
  theme(legend.text = element_text(colour="black", size = 10)) +
  geom_text(size=4, x=1.0, y=1.8, label="*") +
  geom_text(size=4, x=2.0, y=1.7, label="*") +
  geom_text(size=4, x=3.0, y=2.8, label="*") +
  geom_text(size=4, x=5.0, y=1.2, label="*")
plotC
#
#
#
# Arrange plot A B and C in one row:
gA <- ggplotGrob(plotA)
gB <- ggplotGrob(plotB)
gC <- ggplotGrob(plotC)
grid.arrange(gA, gB, gC, ncol=3)
#
g <- arrangeGrob(gA, gB, gC, ncol=3)
ggsave(file="Fig5-final.pdf", g, width=24, height=10, units="cm", dpi=600)
ggsave(g, file="Fig5.eps", width=24, height=10, units="cm", dpi=600, device="eps")

