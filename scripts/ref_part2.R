##libraries
library(R2jags)
library(reshape2)
library(ggplot2)
library(RColorBrewer)

##clean up
rm(list = ls())

##load results of step 1
load("ref.Rdata")

##reconstitute data for model 6
data1<-data[,c(2:4, 7, 9, 12, 14, 23:24, 27)]
data1$inflation_2015<-ifelse(data1$cod_dpto==11, mean(subset(data1, cod_dpto==25)$inflation_2015), data1$inflation_2015)
data1<-na.omit(data1)
y<-data1$per_si
x1<-data1$desp
x2<-data1$vict
x3<-data1$supp
N<-length(x1)
depto<-as.factor(as.numeric(data1$cod_dpto))
J<-length(levels(depto))
x4<-aggregate(unemployment_2015 ~ cod_dpto, data=data1, mean)$unemployment_2015
x5<-aggregate(gdpg ~ cod_dpto, data=data1, mean)$gdpg
x6<-aggregate(inflation_2015 ~ cod_dpto, data=data1, mean)$inflation_2015

##attach best model
attach.jags(ref.6)

##compute parameter values
a.single<-rep(NA,J)
a.single<-apply(a,2,median)
mu.single<-apply(mu.a,2,median)
b1.single<-median(b1)
b2.single<-median(b2)
b3.single<-median(b3)
b4.single<-median(b4)
b5.single<-median(b5)
b6.single<-median(b6)

##compute predictive values mpios
y.hat.single<-a.single[depto]+b1.single*x1+b2.single*x2+b3.single*x3

##compute residuals mpios
y.resid.single<-y-y.hat.single

##plot residuals mpios
pdf("municipal_residual.pdf")
plot(y.hat.single,y.resid.single)
dev.off()

##compute values at depto level
mu.med.single<-median(mu.a)
a.hat.single<-mu.med.single+b4.single*x4+b5.single*x5+b6.single*x6

##compute residuals depto
a.resid.single<-a.single-a.hat.single

##plot depto residuals
pdf("departamento_residual.pdf")
plot(a.hat.single,a.resid.single)
dev.off()

##get parameter quantiles
quant1<-quantile(b1,c(0.025, 0.5, 0.975))
quant2<-quantile(b2,c(0.025, 0.5, 0.975))
quant3<-quantile(b3,c(0.025, 0.5, 0.975))
quant4<-quantile(b4,c(0.025, 0.5, 0.975))
quant5<-quantile(b5,c(0.025, 0.5, 0.975))
quant6<-quantile(b6,c(0.025, 0.5, 0.975))

##detach
detach.jags()

##calculate predictions
max_y<- mean(a.single) + b1.single*max(x1) + b2.single*median(x2) + b3.single*median(x3) 
min_y<- mean(a.single) + b1.single*min(x1) + b2.single*median(x2) + b3.single*median(x3)

##make prediction dataframe based on displacement
predframe<-as.data.frame(cbind((data1$desp*quant1[1]+mean(a.single) + b2.single*median(x2) + b3.single*median(x3)), (data1$desp*quant1[2]+mean(a.single)+ b2.single*median(x2) + b3.single*median(x3)), (data1$desp*quant1[3]+mean(a.single)+ b2.single*median(x2) + b3.single*median(x3)), data1$desp, data1$DEPARTAMENTO))

##rename columns
colnames(predframe)<-c("lwr", "per_si","upr", "desp", "DEPARTAMENTO")

##include other values
predframe$lwr<-as.numeric(as.character(predframe$lwr))
predframe$per_si<-as.numeric(as.character(predframe$per_si))
predframe$upr<-as.numeric(as.character(predframe$upr))

##displacement is predictor
predframe$desp<-as.numeric(as.character(predframe$desp))

##count colors
colourCount <- length(unique(data1$DEPARTAMENTO))

##make palette
getPalette <- colorRampPalette(brewer.pal(9, "Set1"))

##plot displacement
desp_plot<-qplot(desp, per_si, data=data1, colour=DEPARTAMENTO)+theme_bw()+ xlab("Displaced arrivals (log10 scaled by 2005 population)")+ylab("Percent support for peace")+scale_colour_manual(values = getPalette(colourCount))+ geom_segment(aes(x = min(x1), y = min_y,  xend = max(x1), yend = max_y), colour="grey50") + geom_ribbon(data=predframe, aes(ymin=lwr, ymax=upr), alpha=0.3,  fill="lightgrey", colour="grey")

##print displacement plot
ggsave("desp_plot.pdf", h=5, w=8)

##calculate predictions
max_y<- mean(a.single) + b1.single*median(x1) + b2.single*max(x2) + b3.single*median(x3) 
min_y<- mean(a.single) + b1.single*median(x1) + b2.single*min(x2) + b3.single*median(x3)

##make prediction dataframe based on victims
predframe<-as.data.frame(cbind((data1$vict*quant2[1]+mean(a.single) + b1.single*median(x1) + b3.single*median(x3)), (data1$vict*quant2[2]+mean(a.single) + b1.single*median(x1) + b3.single*median(x3)), (data1$vict*quant2[3]+mean(a.single) + b1.single*median(x1) + b3.single*median(x3)), data1$vict, data1$DEPARTAMENTO))

##rename columns
colnames(predframe)<-c("lwr", "per_si","upr", "vict", "DEPARTAMENTO")
predframe$lwr<-as.numeric(as.character(predframe$lwr))
predframe$per_si<-as.numeric(as.character(predframe$per_si))
predframe$upr<-as.numeric(as.character(predframe$upr))
predframe$vict<-as.numeric(as.character(predframe$vict))

##plot victims
vict_plot<-qplot(vict, per_si, data=data1, colour=DEPARTAMENTO)+theme_bw()+ xlab("Victims of violence (log10 scaled by 2005 population)")+ylab("Percent support for peace")+scale_colour_manual(values = getPalette(colourCount))+ geom_segment(aes(x = min(x2), y = min_y,  xend = max(x2), yend = max_y), colour="grey50") + geom_ribbon(data=predframe, aes(ymin=lwr, ymax=upr), alpha=0.3,  fill="lightgrey", colour="grey")

##print victims plot
ggsave("vict_plot.pdf", h=5, w=8)

##calculate predictions
min_y<- mean(a.single) + b1.single*median(x1) + b2.single*median(x2) + b3.single*max(x3) 
max_y<- mean(a.single) + b1.single*median(x1) + b2.single*median(x2) + b3.single*min(x3)

##make prediction dataframe based on supporrt
predframe<-as.data.frame(cbind((data1$supp*quant3[1]+mean(a.single) + b1.single*median(x1) + b2.single*median(x2)), (data1$supp*quant3[2]+mean(a.single) + b1.single*median(x1) + b2.single*median(x2)), (data1$supp*quant3[3]+mean(a.single) + b1.single*median(x1) + b2.single*median(x2)), data1$supp, data1$DEPARTAMENTO))

##rename columns
colnames(predframe)<-c("lwr", "per_si","upr", "supp", "DEPARTAMENTO")
predframe$lwr<-as.numeric(as.character(predframe$lwr))
predframe$per_si<-as.numeric(as.character(predframe$per_si))
predframe$upr<-as.numeric(as.character(predframe$upr))
predframe$supp<-as.numeric(as.character(predframe$supp))

##plot support
supp_plot<-qplot(supp, per_si, data=data1, colour=DEPARTAMENTO)+theme_bw()+ xlab("Percent support for Centro Democratico in 2014")+ylab("Percent support for peace")+scale_colour_manual(values = getPalette(colourCount))+ geom_segment(aes(x = min(x3), y = max_y,  xend = max(x3), yend = min_y), colour="grey50") + geom_ribbon(data=predframe, aes(ymin=lwr, ymax=upr), alpha=0.3,  fill="lightgrey", colour="grey")

##print the plot
ggsave("supp_plot.pdf", h=5, w=8)

##calculate predictions deptos
max_y<- mu.single + b4.single*min(x4) + b5.single*median(x5) + b6.single*median(x6)
min_y<- mu.single + b4.single*max(x4) + b5.single*median(x5) + b6.single*median(x6)
data1<-data1[with(data1, order(cod_dpto)), ]
DEPARTAMENTO<-as.character(unique(data1$DEPARTAMENTO))

##make prediction dataframe based on unemployment
predframe<-as.data.frame(cbind((x4*quant4[1]+mu.single+ b5.single*median(x5) + b6.single*median(x6)), (x4*quant4[2]+mu.single+ b5.single*median(x5) + b6.single*median(x6)), (x4*quant4[3]+mu.single+ b5.single*median(x5) + b6.single*median(x6)), x4, DEPARTAMENTO))

##rename columns
colnames(predframe)<-c("lwr", "per_si","upr", "unemployment_2015", "DEPARTAMENTO")
predframe$lwr<-as.numeric(as.character(predframe$lwr))
predframe$per_si<-as.numeric(as.character(predframe$per_si))
predframe$upr<-as.numeric(as.character(predframe$upr))
predframe$unemployment_2015<-as.numeric(as.character(predframe$unemployment_2015))

##predict at depto level
y1<-a.single

##wrangle these data
dat4plo<-as.data.frame(cbind(x4,x5,x6,y1,DEPARTAMENTO))
dat4plo$y1<-as.numeric(as.character(dat4plo$y1))
dat4plo$x4<-as.numeric(as.character(dat4plo$x4))
dat4plo$x5<-as.numeric(as.character(dat4plo$x5))
dat4plo$x6<-as.numeric(as.character(dat4plo$x6))

##rename columns
colnames(dat4plo)[1:4]<-c("unemployment_2015", "gdpg", "inflation_2015", "per_si")

##plot unemployment
unem_plot<-qplot(unemployment_2015, per_si, data=dat4plo, colour=DEPARTAMENTO)+theme_bw()+ xlab("Unemployment rate 2015")+ylab("Quantitative estimate support for peace")+scale_colour_manual(values = getPalette(colourCount))+ geom_segment(aes(x = min(x4), y = max_y,  xend = max(x4), yend = min_y), colour="grey50")+ geom_ribbon(data=predframe, aes(ymin=lwr, ymax=upr), alpha=0.3,  fill="lightgrey", colour="grey")+ylim(40,130)

##print unemployment plot
ggsave("unem_plot.pdf", h=5, w=8)

##calculate predictions deptos
max_y<- mu.single + b4.single*median(x4) + b5.single*min(x5) + b6.single*median(x6)
min_y<- mu.single + b4.single*median(x4) + b5.single*max(x5) + b6.single*median(x6)

##make prediction dataframe based on gdp
predframe<-as.data.frame(cbind((x5*quant5[1]+mu.single+ b4.single*median(x4) + b6.single*median(x6)), (x5*quant5[2]+mu.single+ b4.single*median(x4) + b6.single*median(x6)), (x5*quant5[3]+mu.single+ b4.single*median(x4) + b6.single*median(x6)), x5, DEPARTAMENTO))

##rename columns
colnames(predframe)<-c("lwr", "per_si","upr", "gdpg", "DEPARTAMENTO")
predframe$lwr<-as.numeric(as.character(predframe$lwr))
predframe$per_si<-as.numeric(as.character(predframe$per_si))
predframe$upr<-as.numeric(as.character(predframe$upr))
predframe$gdpg<-as.numeric(as.character(predframe$gdpg))

##plot gdp
gdpg_plot<-qplot(gdpg, per_si, data=dat4plo, colour=DEPARTAMENTO)+theme_bw()+ xlab("GDP growth 2013-2014")+ylab("Quantitative estimate support for peace")+scale_colour_manual(values = getPalette(colourCount))+ geom_segment(aes(x = min(x5), y = max_y,  xend = max(x5), yend = min_y), colour="grey50")+ geom_ribbon(data=predframe, aes(ymin=lwr, ymax=upr), alpha=0.3,  fill="lightgrey", colour="grey")+ylim(40,130)

##print gdp plot
ggsave("gdpg_plot.pdf", h=5, w=8)

##calculate predictions deptos
max_y<- mu.single + b4.single*median(x4) + b5.single*median(x5) + b6.single*min(x6)
min_y<- mu.single + b4.single*median(x4) + b5.single*median(x5) + b6.single*max(x6)

##make prediction dataframe based on inflation
predframe<-as.data.frame(cbind((x6*quant6[1]+mu.single+ b4.single*median(x4) + b5.single*median(x5)), (x6*quant6[2]+mu.single+ b4.single*median(x4) + b5.single*median(x5)), (x6*quant6[3]+mu.single+ b4.single*median(x4) + b5.single*median(x5)), x6, DEPARTAMENTO))

##rename columns
colnames(predframe)<-c("lwr", "per_si","upr", "inflation_2015", "DEPARTAMENTO")
predframe$lwr<-as.numeric(as.character(predframe$lwr))
predframe$per_si<-as.numeric(as.character(predframe$per_si))
predframe$upr<-as.numeric(as.character(predframe$upr))
predframe$inflation_2015<-as.numeric(as.character(predframe$inflation_2015))

##plot inflation
infl_plot<-qplot(inflation_2015, per_si, data=dat4plo, colour=DEPARTAMENTO)+theme_bw()+ xlab("Inflation 2015")+ylab("Quantitative estimate support for peace")+scale_colour_manual(values = getPalette(colourCount))+ geom_segment(aes(x = min(x6), y = max_y,  xend = max(x6), yend = min_y), colour="grey50")+ geom_ribbon(data=predframe, aes(ymin=lwr, ymax=upr), alpha=0.3,  fill="lightgrey", colour="grey")+ylim(40,130)

##print inflation plot
ggsave("infl_plot.pdf", h=5, w=8)

##save
save.image("ref.Rdata")