##################### plot the difference in greenspace cover distribution by deprivation level in the 3 cities

library(ggplot2)
library(sf)
load("C:/Users/David/Documents/Facebook COVID/berlin_corona_FB_BI_Aug_2021.Rdata")

##Berlin
ber.quad<-ber.pop.bi[!duplicated(ber.pop.bi$quadkey),]
rm(ber.pop.bi)
ber.quad$MSS.f<-factor(ceiling(ber.quad$MSS))

coverage.dist.berlin<-ggplot(ber.quad, aes(x=log10(coverage_area+0.000001),fill=MSS.f))+
geom_density(alpha=0.5)#,adjust=2) #+ here change legend title and match colours to other graphs
coverage.dist.berlin
##more zeros in 1 more coverage in non-zero tile in 4

coverage.dist.berlin<-ggplot(subset(ber.quad,coverage_area>0), aes(x=coverage_area,fill=MSS.f))+
geom_density(alpha=0.5)#,adjust=2) #+ here change legend title and match colours to other graphs
coverage.dist.berlin


#Paris
load("C:/Users/David/Documents/Facebook COVID/paris_corona_FB_periph_Aug_2021.Rdata")
par.quad<-par.pop.periph[!duplicated(par.pop.periph$quadkey),]
rm(par.pop.periph)
hist(par.quad$FDep)
par.quad$FDep.f<-"less than -1"
par.quad$FDep.f[par.quad$FDep>=(-1) & par.quad$FDep<1]<-"-1 to 1"
par.quad$FDep.f[par.quad$FDep>=(1)]<-"more than 1"
par.quad$FDep.f<-factor(par.quad$FDep.f,levels=c("less than -1","-1 to 1","more than 1"))


coverage.dist.paris<-ggplot(par.quad, aes(x=log10(coverage_area+0.000001),fill=FDep.f))+
geom_density(alpha=0.5,adjust=1.5) #+ here change legend title and match colours to other graphs
coverage.dist.paris


coverage.dist.paris<-ggplot(subset(par.quad,coverage_area>0), aes(x=coverage_area,fill=FDep.f))+
geom_density(alpha=0.5,adjust=2) #+ here change legend title and match colours to other graphs
coverage.dist.paris

#London
load("C:/Users/David/Documents/Facebook COVID/london_map.Rdata")
lon.quant<-as.numeric(quantile(london.map$IMD, c(.2,.4,.6,.8)))
london.map$IMD.f<-"<20%"
london.map$IMD.f[london.map$IMD>lon.quant[1] & london.map$IMD<=lon.quant[2]]<-"20-40%"
london.map$IMD.f[london.map$IMD>lon.quant[2] & london.map$IMD<=lon.quant[3]]<-"40-60%"
london.map$IMD.f[london.map$IMD>lon.quant[3] & london.map$IMD<=lon.quant[4]]<-"60-80%"
london.map$IMD.f[london.map$IMD>lon.quant[4]]<-">80%"
london.map$IMD.f<-factor(london.map$IMD.f,levels=c("<20%","20-40%","40-60%","60-80%",">80%"))


coverage.dist.london<-ggplot(london.map, aes(x=log10(coverage_area+0.000001),fill=IMD.f))+
geom_density(alpha=0.3) #+ here change legend title and match colours to other graphs
coverage.dist.london


coverage.dist.london<-ggplot(subset(london.map,coverage_area>0), aes(x=coverage_area,fill=IMD.f))+
geom_density(alpha=0.5,adjust=2) #+ here change legend title and match colours to other graphs
coverage.dist.london

windows()
coverage.dist.paris
windows()
coverage.dist.berlin



coverage.dist.london<-ggplot(london.map, aes(IMD.f,log10(coverage_area),fill=IMD.f))+
geom_violin()
coverage.dist.london

coverage.dist.berlin<-ggplot(ber.quad, aes(MSS.f,log10(coverage_area),fill=MSS.f))+
geom_violin()
coverage.dist.berlin


