########### combine movement analyses across the 3 cities
# Aug 2022
library(ggplot2)
library(lme4)
library(ggeffects)
library(car)
################################

load("F:/COVID cities/Facebook COVID/berlin_mvt.Rdata")
load("F:/COVID cities/Facebook COVID/Twitter/tweet.volume.df.all.Rdata")

ber.mov$repeats<-tweet.vol.ber$repeats[match(as.Date(ber.mov$dates),tweet.vol.ber$date)]
rm(tweet.vol.lon,tweet.vol.ber,tweet.vol.par)

ber.mov.08<-subset(ber.mov,time=="08:00")
ber.mov.08$start_MSS.f<-factor(ceiling(ber.mov.08$start_MSS))
ber.mov.08$end_MSS.f<-factor(ceiling(ber.mov.08$end_MSS))
berlin.map.district$MSS.f<-factor(ceiling(berlin.map.district$MSS))

ber.mov.08.lockdown<-subset(ber.mov.08,!is.na(start_MSS)&!is.na(end_MSS)&lockdown==2)

threshold<-as.numeric(quantile(berlin.map.district$coverage_area[berlin.map.district$MSS.f=="4"],na.rm=T,.75))

ber.mov.08.lockdown.deprive<-subset(ber.mov.08.lockdown,end_MSS.f=="4"&end_greenspace_coverage>threshold)
ber.mov.08.lockdown.deprive<-subset(ber.mov.08.lockdown.deprive,repeats!="4")
ber.mov.08.lockdown.deprive$repeats<-factor(ber.mov.08.lockdown.deprive$repeats)

ber.mov.08.lockdown.deprive.all<-subset(ber.mov.08.lockdown,end_MSS.f=="4")
ber.mov.08.lockdown.deprive.all<-subset(ber.mov.08.lockdown.deprive.all,repeats!="4")
ber.mov.08.lockdown.deprive.all$repeats<-factor(ber.mov.08.lockdown.deprive.all$repeats)

#lme.cover1.1.berlin<-lmer(z_score~start_MSS.f*weekend+(1|dated)+(1|start_quadkeys),data=ber.mov.08.lockdown.deprive,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
lme.cover1.1.berlin<-lmer(z_score~start_MSS.f+(1|dated)+(1|start_quadkeys),data=ber.mov.08.lockdown.deprive,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
lme.cover1.2.berlin<-lmer(z_score~start_MSS.f+(1|dated)+(1|start_quadkeys),data=ber.mov.08.lockdown.deprive.all,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))

Berlin.mvt.effect<-ggpredict(lme.cover1.2.berlin,terms=c("start_MSS.f"))
plot(Berlin.mvt.effect)

Anova(lme.cover1.1.berlin)
summary(lme.cover1.1.berlin)

#,"weekend"

######
load("F:/COVID cities/Facebook COVID/paris_mvt.Rdata")
load("F:/COVID cities/Facebook COVID/Twitter/tweet.volume.df.all.Rdata")

par.mov$repeats<-tweet.vol.par$repeats[match(as.Date(par.mov$dates),tweet.vol.par$date)]
rm(tweet.vol.lon,tweet.vol.ber,tweet.vol.par)


par.mov.08<-subset(par.mov,time=="08:00")

hist(gardens.quadkey.df.map$FDep)

par.mov.08$start_FDep.f<-"deprived"
par.mov.08$start_FDep.f[par.mov.08$start_FDep>(-2) & par.mov.08$start_FDep<=(-.5)]<-"deprived"
par.mov.08$start_FDep.f[par.mov.08$start_FDep>(-.5) & par.mov.08$start_FDep<=(.5)]<-"average"
par.mov.08$start_FDep.f[par.mov.08$start_FDep>(.5)]<-"affluent"
par.mov.08$start_FDep.f<-factor(par.mov.08$start_FDep.f)

par.mov.08$end_FDep.f<-"deprived"
par.mov.08$end_FDep.f[par.mov.08$end_FDep>(-2) & par.mov.08$end_FDep<=(-.5)]<-"deprived"
par.mov.08$end_FDep.f[par.mov.08$end_FDep>(-.5) & par.mov.08$end_FDep<=(.5)]<-"average"
par.mov.08$end_FDep.f[par.mov.08$end_FDep>(.5)]<-"affluent"
par.mov.08$end_FDep.f<-factor(par.mov.08$end_FDep.f)


par.mov.08.lockdown<-subset(par.mov.08,!is.na(start_FDep)&!is.na(end_FDep)&lockdown==2)

threshold.par<-as.numeric(quantile(gardens.quadkey.df.map$coverage_area[gardens.quadkey.df.map$FDep<(-.5)],na.rm=T,.75))

par.mov.08.lockdown.deprive<-subset(par.mov.08.lockdown,end_FDep.f=="deprived"&end_greenspace_coverage>threshold.par)

par.mov.08.lockdown.deprive<-subset(par.mov.08.lockdown,end_FDep.f=="deprived")

lme.cover1.1.paris<-lmer(z_score~start_FDep.f*weekend*repeats*end_greenspace_coverage+(1|dated)+(1|start_quadkeys),data=par.mov.08.lockdown.deprive,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
Paris.mvt.effect<-ggpredict(lme.cover1.1.paris,terms=c("start_FDep.f","weekend ","repeats","end_greenspace_coverage [0,.5,1]"))
plot(Paris.mvt.effect)

par.mov.08.lockdown<-subset(par.mov.08.lockdown,repeats!="1")
lme.cover1.1.paris<-lmer(z_score~start_FDep*end_FDep*weekend*end_greenspace_coverage+(1|dated)+(1|start_quadkeys),data=par.mov.08.lockdown.deprive,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
Paris.mvt.effect<-ggpredict(lme.cover1.1.paris,terms=c("end_FDep [-2,0,2]","start_FDep [-2,0,2]","weekend [yes]","end_greenspace_coverage [0,.5,1]"))
plot(Paris.mvt.effect)


### affluent
threshold.par<-as.numeric(quantile(gardens.quadkey.df.map$coverage_area[gardens.quadkey.df.map$FDep>(.5)],na.rm=T,.9))

par.mov.08.lockdown.affluent<-subset(par.mov.08.lockdown,end_FDep.f=="affluent"&repeats!="1"&end_greenspace_coverage>threshold.par)
par.mov.08.lockdown.affluent<-subset(par.mov.08.lockdown,end_FDep.f=="affluent"&repeats!="1")

lme.cover1.1.paris<-lmer(z_score~start_FDep.f*weekend+(1|dated)+(1|start_quadkeys),data=par.mov.08.lockdown.affluent,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
Paris.mvt.effect<-ggpredict(lme.cover1.1.paris,terms=c("start_FDep.f","weekend"))
plot(Paris.mvt.effect)

lme.cover1.1.paris<-lmer(z_score~start_FDep.f*end_greenspace_coverage*weekend+(1|dated)+(1|start_quadkeys),data=par.mov.08.lockdown.affluent,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
Paris.mvt.effect<-ggpredict(lme.cover1.1.paris,terms=c("start_FDep.f","weekend","end_greenspace_coverage [0,0.5,1]"))
plot(Paris.mvt.effect)

par.mov.08.lockdown.r<-subset(par.mov.08.lockdown,repeats!="1")
lme.cover1.1.paris<-lmer(z_score~start_FDep.f*end_FDep.f*end_greenspace_coverage*weekend+(1|dated)+(1|start_quadkeys),data=par.mov.08.lockdown.r,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
Paris.mvt.effect<-ggpredict(lme.cover1.1.paris,terms=c("end_FDep.f","start_FDep.f","weekend","end_greenspace_coverage [1]"))
plot(Paris.mvt.effect)


ber.mov.08.lockdown.all<-subset(ber.mov.08.lockdown,repeats!="4")
lme.cover1.1.berlin<-lmer(z_score~start_MSS.f*end_MSS.f*end_greenspace_coverage*weekend+(1|dated)+(1|start_quadkeys),data=ber.mov.08.lockdown.all,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
Berlin.mvt.effect<-ggpredict(lme.cover1.1.berlin,terms=c("end_MSS.f","start_MSS.f","weekend","end_greenspace_coverage [1]"))
plot(Berlin.mvt.effect)

