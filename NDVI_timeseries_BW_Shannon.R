# NDVI (avg of 3 samples/site/year) for MSH Canopy paper
# NDVI data from 1984-2019

setwd("C:/Users/sclaeson/Documents/MSH research/PP streams 2018-2019/CanopyCover paper/Stats")
getwd()

if(!require(ggplot2)){install.packages("ggplot2")}
library(ggplot2)
library(readxl)

dta = read_excel("NDVIavg_Canopy.xlsx", sheet="NDVI")
dta$Canopy = factor(dta$Canopy, levels=c("Closed","Open"))
dta$Stream = factor(dta$Stream, levels=c("Camp-E","Geo-W","Clear","Forsyth","Redrock"))

## from Carri:
#dta<-read.csv("A:\\mtsthelens\\Canopy2018\\MSH_NDVIpolygonresults.csv")
#dta$stream<-rep(c("Clear", "Camp-E", "Forsyth", "Geo-W", "Redrock"), each=72)
#dta$Canopy<-rep(c("Closed", "Open"), each=36)
#cols<-c("black", "gray70")

boxplot(NDVIavg~Site, data=dta, las=2, xlab="", cex.axis=0.8)

ggplot(dta, aes(x=Year, y=NDVIavg))+
  geom_line(aes(group=Canopy, color=Canopy), size=1)+
  facet_wrap(.~Stream)+
  geom_abline(slope=0, intercept=0.3, color="black")+
  theme_bw()+scale_color_manual(values=c("black","grey70"))+
  theme(legend.position = c(0.1, 0.9), legend.title = element_blank(), legend.background = element_blank(), 
        legend.text = element_text(colour="black", size=12)) +
  theme(panel.grid.minor = element_blank()) +
  theme(strip.text = element_text(size=11), strip.background = element_rect(fill="grey90")) +
  theme(text = element_text(size = 12)) +
  theme(axis.text.x = element_text(colour="black", size=10, angle=45, vjust=0.5), 
        axis.text.y = element_text(colour="black", size=10)) +
  labs(x="Year", y="mean NDVI") +
  scale_y_continuous(breaks=seq(0,1,by=0.2)) +
  xlim(1980, 2020) 

ggsave("NDVIavg_Canopy.jpeg", width=16, height=10, units="cm", dpi=600) #default dpi=300
ggsave("NDVIavg_Canopy.pdf", width=16, height=10, units="cm", dpi=600)

#extra ggplot code
theme(panel.grid.major = element_blank()) 
theme(strip.background = element_rect(fill="white"))
theme(axis.text.x = element_text(angle=90))
