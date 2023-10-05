##libraries
library(R2jags)
library(reshape2)
library(ggplot2)
library(RColorBrewer)

##clear out workspace
rm(list = ls())

##get data
unem<-read.csv("unemployment_2015.csv")
infl<-read.csv("Inflation_2015.csv")
econ<-read.csv("econ.csv")
desp<-read.csv("desplazadas.csv")
persi<-read.csv("Plebiscito.csv")
pres<-read.csv("pres2014.csv")

##collapse two codes
infl$statecode<-ifelse(infl$statecode==11,25, infl$statecode)

##merge referendum and displaced
persi<-merge(persi, desp, by.y="CODIGO.DANE", by.x="Id_mun", all.x=T, all.y=T)

##create a link file
link<-econ[,c(1:5, 9:10)]
link<-subset(link, year==2014|year==2013)

##melt 
link<-melt(link, id=c("cod_dpto", "cod_mpio", "year"), measure.vars= c("pib_dpto_const_base2000"))

##cast
link<-dcast( cod_dpto+cod_mpio~year, data=link)
link$gdpg<-100*((link$"2014"/link$"2013")-1)

##process presidential election adata
p2<-subset(pres, NOMBRE_CANDIDATO=="\xeeSCAR IV\xe7N ZULUAGA")
p2<-aggregate(VOTOS ~ MUNICIPIO + DEPARTAMENTO, data=p2, sum)
p1<-aggregate(VOTOS ~ MUNICIPIO + DEPARTAMENTO, data=pres, sum)
p3<-merge(p1, p2, by.x=c("MUNICIPIO", "DEPARTAMENTO"), by.y=c("MUNICIPIO", "DEPARTAMENTO") )
p3$supp<-p3$VOTOS.y*100/p3$VOTOS.x

##clean up departamento designations
p3$DEPARTAMENTO<-ifelse(p3$DEPARTAMENTO=="BOGOTA D.C.", "BOGOTA, D.C.", as.character(p3$DEPARTAMENTO))
p3$DEPARTAMENTO<-ifelse(p3$DEPARTAMENTO=="NORTE DE SAN", "NORTE DE SANTANDER", as.character(p3$DEPARTAMENTO))
p3$DEPARTAMENTO<-ifelse(p3$DEPARTAMENTO=="SAN ANDRES", "ARCHIPIELAGO DE SAN ANDRES, PROVIDENCIA Y SANTA CATALINA", as.character(p3$DEPARTAMENTO))
p3$DEPARTAMENTO<-ifelse(p3$DEPARTAMENTO=="VALLE", "VALLE DEL CAUCA", as.character(p3$DEPARTAMENTO))
p3$DEPARTAMENTO<-as.factor(p3$DEPARTAMENTO)

##get rid of outside Col vote
p3<-subset(p3, DEPARTAMENTO !="CONSULADOS")
p3$MUNICIPIO<-droplevels(p3)$MUNICIPIO

##export for manual clean up
write.csv(p3, "p3_orig.csv")

##import manually cleaned
p3<-read.csv("p3_edi.csv")

##clean up names
p3$MUNICIPIO<-gsub("\xe3", "Ñ", p3$MUNICIPIO)
persi$MUNICIPIO<-gsub("\x84", "Ñ", persi$MUNICIPIO)
p3$MUNICIPIO<-as.factor(p3$MUNICIPIO)
persi$MUNICIPIO<-as.factor(persi$MUNICIPIO)

##clean up troublesome names
p3$MUNICIPIO<-gsub("ARROYO HONDO", "ARROYOHONDO", p3$MUNICIPIO)
p3$MUNICIPIO<-gsub("BOGOTA. D.C.", "BOGOTA, D.C.", p3$MUNICIPIO)
p3$MUNICIPIO<-gsub("BUGA", "GUADALAJARA DE BUGA", p3$MUNICIPIO)
p3$MUNICIPIO<-gsub("CARMEN DE VIBORAL", "EL CARMEN DE VIBORAL", p3$MUNICIPIO)
p3$MUNICIPIO<-gsub("CERRO DE SAN ANTONIO", "CERRO SAN ANTONIO", p3$MUNICIPIO)
p3$MUNICIPIO<-gsub("CHIVOLO", "CHIBOLO", p3$MUNICIPIO)
p3$MUNICIPIO<-gsub("DON MATIAS", "DONMATIAS", p3$MUNICIPIO)
p3$MUNICIPIO<-gsub("EL TABLON", "EL TABLON DE GOMEZ", p3$MUNICIPIO)
p3$MUNICIPIO<-gsub("GUADALAJARA DE BUGALAGRANDE", "BUGALAGRANDE", p3$MUNICIPIO)
p3$MUNICIPIO<-gsub("TOLUVIEJO", "TOLU VIEJO", p3$MUNICIPIO)
p3$MUNICIPIO<-gsub("TUMACO", "SAN ANDRES DE TUMACO", p3$MUNICIPIO)
p3$MUNICIPIO<-gsub("CUBARRAL", "SAN LUIS DE CUBARRAL", p3$MUNICIPIO)
p3$MUNICIPIO<-gsub("VILLA DE LEIVA", "VILLA DE LEYVA", p3$MUNICIPIO)
p3$MUNICIPIO<-gsub("VISTA HERMOSA", "VISTAHERMOSA", p3$MUNICIPIO)
p3$MUNICIPIO<-gsub("YONDO-CASABE", "YONDO", p3$MUNICIPIO)
p3$MUNICIPIO<-gsub("PUERTO NARE-LA MAGDALENA", "PUERTO NARE", p3$MUNICIPIO)
p3$MUNICIPIO<-gsub("MIRITI PARANA", "MIRITI - PARANA", p3$MUNICIPIO)
p3$MUNICIPIO<-gsub("TOLU", "SANTIAGO DE TOLU", p3$MUNICIPIO)
p3$MUNICIPIO<-gsub("UBATE", "VILLA DE SAN DIEGO DE UBATE", p3$MUNICIPIO)
p3$MUNICIPIO<-gsub("MARIQUITA", "SAN SEBASTIAN DE MARIQUITA", p3$MUNICIPIO)
p3$MUNICIPIO<-gsub("RIOVIEJO", "RIO VIEJO", p3$MUNICIPIO)
p3$MUNICIPIO<-gsub("SAN ANDRES DE SOTAVENTO", "SAN ANDRES SOTAVENTO", p3$MUNICIPIO)
p3$MUNICIPIO<-gsub("SANTIAGO DE TOLU VIEJO", "TOLU VIEJO", p3$MUNICIPIO)
p3$MUNICIPIO<-gsub("SAN JUAN DE RIOSECO", "SAN JUAN DE RIO SECO", p3$MUNICIPIO)
p3$MUNICIPIO<-gsub("SINCE", "SAN LUIS DE SINCE", p3$MUNICIPIO)
p3$MUNICIPIO<-gsub("SAN LUIS DE SINCELEJO", "SINCELEJO", p3$MUNICIPIO)
p3$MUNICIPIO<-gsub("ANTIOQUIA", "SANTAFE DE ANTIOQUIA", p3$MUNICIPIO)
p3$MUNICIPIO<-gsub("SAN MARTIN DE LOS LLANOS", "SAN MARTIN", p3$MUNICIPIO)
p3$MUNICIPIO<-gsub("MANAURE BALCON DEL CESAR", "MANAURE", p3$MUNICIPIO)

##make into factor
p3$MUNICIPIO<-as.factor(p3$MUNICIPIO)

##merge link and unemployment data then merege osme more
data<-merge(link, unem, by.x="cod_dpto", by.y="statecode", all.x=T)
data<-merge(data, infl, by.x="cod_dpto", by.y="statecode", all.x=T)
data<-merge(data, persi, by.x="cod_mpio", by.y="Id_mun", all.x=T, all.y=T)

##generate displacement & victimstatistic
data$desp<-log10((data$PERSONAS.RECIBIDAS+1)/data$censo)
data$vict<-log10((data$victim+1)/data$censo)

##clean up p3 this time departamento
p3$DEPARTAMENTO <-gsub("\xe3", "Ñ", p3$DEPARTAMENTO)
data$DEPARTAMENTO <-gsub("\x84", "Ñ", data$DEPARTAMENTO)

##merge data with presidential votes
data<-merge(data, p3, by.x=c("MUNICIPIO", "DEPARTAMENTO"), by.y=c("MUNICIPIO", "DEPARTAMENTO"), all.x=T)
data$X<-NULL

##get the columns 
data1<-data[,c(2:4, 7, 9, 12, 14, 23:24, 27)]

##collapse those two codes
data1$inflation_2015<-ifelse(data1$cod_dpto==11, mean(subset(data1, cod_dpto==25)$inflation_2015), data1$inflation_2015)

##write out the data for the model
write.csv(data1, "variables_used.csv")

##delete missing data rows
data1<-na.omit(data1)

##linear  odel
m<-lm(per_si~desp + vict + unemployment_2015 + gdpg + inflation_2015 +supp, data=data1)

##name variable for Jags
y<-data1$per_si
x1<-data1$desp
x2<-data1$vict
x3<-data1$supp
N<-length(x1)

##these variables observed at departamento level
depto<-as.factor(as.numeric(data1$cod_dpto))
J<-length(levels(depto))
x4<-aggregate(unemployment_2015 ~ cod_dpto, data=data1, mean)$unemployment_2015
x5<-aggregate(gdpg ~ cod_dpto, data=data1, mean)$gdpg
x6<-aggregate(inflation_2015 ~ cod_dpto, data=data1, mean)$inflation_2015

##data for model
ref6.data<-list("N", "y", "J", "depto", "x1", "x2", "x3", "x4", "x5", "x6")

##starting values
ref6.inits<-function(){list(a=rnorm(J), sigma.y=runif(1), mu.a=rnorm(1), sigma.a=runif(1), b1=rnorm(1), b2=rnorm(1), b3=rnorm(1), b4=rnorm(1), b5=rnorm(1), b6=rnorm(1))}

##model parameters
ref6.parameters<-c("a", "b1", "b2", "b3", "b4", "b5", "b6","sigma.y", "e.y", "mu.a", "sigma.a", "e.a")

##run jags model
ref.6<-jags(ref6.data, ref6.inits, ref6.parameters, "model6.txt", n.chains=4, n.iter=5000)

##plot model output
pdf("model6.pdf")
plot(ref.6)
dev.off()

##attach
attach.jags(ref.6)

##calculate R^2 at various levels
rsquared.y6<-1-mean(apply(e.y,1,var))/var(na.omit(y))
rsquared.a6<-1-mean(apply(e.a,1,var))/mean(apply(a,1,var))

##calculate pooling
lambda.y6<-1-var(apply (e.y, 2, mean))/mean(apply (e.y, 1, var))
lambda.a6<-1- var(apply (e.a, 2, mean))/mean(apply(e.a, 1,var))

##detach
detach.jags()

##another model
data1<-data[,c(2:4, 7, 9, 14, 23:24, 27)]
data1<-na.omit(data1)

##variables
y<-data1$per_si
x1<-data1$desp
x2<-data1$vict
x3<-data1$supp
N<-length(x1)
depto<-as.factor(as.numeric(data1$cod_dpto))
J<-length(levels(depto))
x4<-aggregate(unemployment_2015 ~ cod_dpto, data=data1, mean)$unemployment_2015
x5<-aggregate(gdpg ~ cod_dpto, data=data1, mean)$gdpg

##jagas vars
ref5.data<-list("N", "y", "J", "depto", "x1", "x2", "x3", "x4", "x5")

##starting values
ref5.inits<-function(){list(a=rnorm(J), sigma.y=runif(1), mu.a=rnorm(1), sigma.a=runif(1), b1=rnorm(1), b2=rnorm(1), b3=rnorm(1), b4=rnorm(1), b5=rnorm(1))}

##parameters
ref5.parameters<-c("a", "b1", "b2", "b3", "b4", "b5", "sigma.y", "e.y", "mu.a", "sigma.a", "e.a")

##run  model
ref.5<-jags(ref5.data, ref5.inits, ref5.parameters, "model5.txt", n.chains=4, n.iter=5000)

##print model
pdf("model5.pdf")
plot(ref.5)
dev.off()

##attach
attach.jags(ref.5)

##calculate R^2 at various levels
rsquared.y5<-1-mean(apply(e.y,1,var))/var(na.omit(y))
rsquared.a5<-1-mean(apply(e.a,1,var))/mean(apply(a,1,var))
##calculate pooling
lambda.y5<-1-var(apply (e.y, 2, mean))/mean(apply (e.y, 1, var))
lambda.a5<-1- var(apply (e.a, 2, mean))/mean(apply(e.a, 1,var))

##detach
detach.jags()

##another model
data1<-data[,c(2:4, 7, 9, 14, 24, 27)]
data1<-na.omit(data1)

##variables
y<-data1$per_si
x2<-data1$vict
x3<-data1$supp
N<-length(x1)
depto<-as.factor(as.numeric(data1$cod_dpto))
J<-length(levels(depto))
head(data1)
x4<-aggregate(unemployment_2015 ~ cod_dpto, data=data1, mean)$unemployment_2015
x5<-aggregate(gdpg ~ cod_dpto, data=data1, mean)$gdpg

##jagas vars
ref4.data<-list("N", "y", "J", "depto", "x2", "x3", "x4", "x5")

##starting values
ref4.inits<-function(){list(a=rnorm(J), sigma.y=runif(1), mu.a=rnorm(1), sigma.a=runif(1),  b2=rnorm(1), b3=rnorm(1), b4=rnorm(1), b5=rnorm(1))}

##parameters
ref4.parameters<-c("a", "b2", "b3", "b4", "b5", "sigma.y", "e.y", "mu.a", "sigma.a", "e.a")

##run  model
ref.4<-jags(ref4.data, ref4.inits, ref4.parameters, "model4.txt", n.chains=4, n.iter=5000)

##print model
pdf("model4.pdf")
plot(ref.4)
dev.off()

##attach
attach.jags(ref.4)

##calculate R^2 at various levels
rsquared.y4<-1-mean(apply(e.y,1,var))/var(na.omit(y))
rsquared.a4<-1-mean(apply(e.a,1,var))/mean(apply(a,1,var))

##calculate pooling
lambda.y4<-1-var(apply (e.y, 2, mean))/mean(apply (e.y, 1, var))
lambda.a4<-1- var(apply (e.a, 2, mean))/mean(apply(e.a, 1,var))

##detach
detach.jags()

##print out models
sink("results_models.txt")
print(ref.4)
print(ref.5)
print(ref.6)
sink()

##print out R^2
sink("model_r2.txt")
print(round(c(rsquared.y4, rsquared.a4),2))
print(round(c(lambda.y4, lambda.a4),2))
print(round(c(rsquared.y5, rsquared.a5),2))
print(round(c(lambda.y5, lambda.a5),2))
print(round(c(rsquared.y6, rsquared.a6),2))
print(round(c(lambda.y6, lambda.a6),2))
sink()

##save
save.image("ref.Rdata")