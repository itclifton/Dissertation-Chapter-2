setwd("~/Desktop/Research/Dissertation Code and Data/Dissertation-Chapter-2")

library(ggplot2)
library(cowplot)
st.err = function(x) {sd(x)/sqrt(length(x))}

## Data Management ----
# Raw Data
data1<-read.csv("AaronTselect.csv")
# Calculate Tsel as mean of each individual's nine Tsel measurements
Tsel<-aggregate(Tselect~id, mean, data=data1)
data2<-merge(data1,Tsel, by="id")
data3<-aggregate(cbind(svl,mass,Ctmax,Ctmin,Tselect.y)~id+sex+Sex, max, data=data2)
data3.1=subset(data3, id!="1901" & id!="1910") # 1901 is a different population and 1910 is a major outlier for CTmin. 
# adding a CTbreadth column
data3.1$Ctbreadth<-data3.1$Ctmax-data3.1$Ctmin 
data3.1<-data3.1[, -2]

## Summaries and Statistics ----
mean(data3.1$Ctmax)
st.err(data3.1$Ctmax)
mean(data3.1$Tselect.y)
st.err(data3.1$Tselect.y)
mean(data3.1$Ctmin)
st.err(data3.1$Ctmin)
mean(data3.1$Ctbreadth)
st.err(data3.1$Ctbreadth)

# Linear Models
plot(Ctmax~log(svl), data=data3.1)
abline(lm(Ctmax~log(svl), data=data3.1))
summary(lm(Ctmax~log(svl), data=data3.1))

plot(Tselect.y~log(svl), data=data3.1)
abline(lm(Tselect.y~log(svl), data=data3.1))
summary(lm(Tselect.y~log(svl), data=data3.1))

plot(Ctmin~log(svl), data=data3.1)
abline(lm(Ctmin~log(svl), data=data3.1))
summary(lm(Ctmin~log(svl), data=data3.1))

plot(Ctbreadth~log(svl), data=data3.1)
abline(lm(Ctbreadth~log(svl), data=data3.1))
summary(lm(Ctbreadth~log(svl), data=data3.1))

## Figures ----
# CTmax
r1=ggplot(data=data3.1, aes(x=log(svl), y=Ctmax))+
  geom_point(data=data3.1, colour="#898989")+
  geom_smooth(method='lm', colour="black")+
  theme_bw()+
  theme(panel.grid.major=element_line(colour="#FFFFFF"),panel.grid.minor=element_line(colour="#FFFFFF"))+
  theme(axis.text.y=element_text(size=12))+
  theme(axis.text.x=element_text(size=12))+
  theme(axis.title.y=element_text(size=14, face="bold"))+
  theme(axis.title.x=element_text(size=14, face="bold"))+
  ylab("CTmax (°C)")+
  xlab("ln(SVL)")
# Tsel
r2=ggplot(data=data3.1, aes(x=log(svl), y=Tselect.y))+
  geom_point(data=data3.1, colour="#898989")+
  geom_smooth(method='lm', colour="black")+
  theme_bw()+
  theme(panel.grid.major=element_line(colour="#FFFFFF"),panel.grid.minor=element_line(colour="#FFFFFF"))+
  theme(axis.text.y=element_text(size=12))+
  theme(axis.text.x=element_text(size=12))+
  theme(axis.title.y=element_text(size=14, face="bold"))+
  theme(axis.title.x=element_text(size=14, face="bold"))+
  ylab("Tsel (°C)")+
  xlab("ln(SVL)")
# CTmin
r3=ggplot(data=data3.1, aes(x=log(svl), y=Ctmin))+
  geom_point(data=data3.1, colour="#898989")+
  geom_smooth(method='lm', colour="black")+
  theme_bw()+
  theme(panel.grid.major=element_line(colour="#FFFFFF"),panel.grid.minor=element_line(colour="#FFFFFF"))+
  theme(axis.text.y=element_text(size=12))+
  theme(axis.text.x=element_text(size=12))+
  theme(axis.title.y=element_text(size=14, face="bold"))+
  theme(axis.title.x=element_text(size=14, face="bold"))+
  ylab("CTmin (°C)")+
  xlab("ln(SVL)")
# Tbreadth
r4=ggplot(data=data3.1, aes(x=log(svl), y=Ctbreadth))+
  geom_point(data=data3.1, colour="#898989")+
  geom_smooth(method='lm', colour="black")+
  theme_bw()+
  theme(panel.grid.major=element_line(colour="#FFFFFF"),panel.grid.minor=element_line(colour="#FFFFFF"))+
  theme(axis.text.y=element_text(size=12))+
  theme(axis.text.x=element_text(size=12))+
  theme(axis.title.y=element_text(size=14, face="bold"))+
  theme(axis.title.x=element_text(size=14, face="bold"))+
  ylab("Tbreadth (°C)")+
  xlab("ln(SVL)")

Fig1=plot_grid(r1,r2,r3,r4,
               labels = "AUTO", ncol = 2, nrow = 2)
#save_plot("Fig1_ln.tiff", Regs, base_height=10, dpi=300)



