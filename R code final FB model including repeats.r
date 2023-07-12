
library(splines)
library(lme4)

load("C:/Users/David/Documents/Facebook COVID/london_corona_FB_M25_Aug_2021.Rdata")
lon.pop.M25<-lon.pop.M25[,c(1,2,12,19:29,31,32)]
lon.pop.M25$lockdown[is.na(lon.pop.M25$lockdown)]<-0 #the NA for unreported last 2-3 days when downloaded which were confirmed as 0s on the ordinal scale since
load("C:/Users/David/Documents/Facebook COVID/Twitter/tweet.volume.df.all.Rdata")

lon.pop.M25$repeats<-tweet.vol.lon$repeats[match(lon.pop.M25$dates,tweet.vol.lon$date)]
rm(tweet.vol.lon,tweet.vol.ber,tweet.vol.par)
lon.pop.M25$repeats<-as.numeric(lon.pop.M25$repeats)
lon.pop.M25$repeats[lon.pop.M25$lockdown=="0"]<-lon.pop.M25$repeats[lon.pop.M25$lockdown=="0"]-1 #we start in March
lon.pop.M25$repeats<-factor(lon.pop.M25$repeats)
table(lon.pop.M25$lockdown,lon.pop.M25$repeats)

lon.pop.M25.sub<-subset(lon.pop.M25,repeats!="4")
lon.pop.M25.sub$repeats<-factor(as.numeric(lon.pop.M25.sub$repeats))
rm(lon.pop.M25)
gc()

lon.pop.M25.sub$dated<-factor(lon.pop.M25.sub$dates)

save(lon.pop.M25.sub,file="C:/Users/David/Documents/Facebook COVID/london_corona_FB_M25_sub_Aug_2021.Rdata")

load("C:/Users/David/Documents/Facebook COVID/london_corona_FB_M25_sub_Aug_2021.Rdata")
library(splines)
library(lme4)


tic<-Sys.time()
lon.lme.final<-lmer(clipped_z_score~lockdown*time*weekend*ns(IMD,df=2)*ns(coverage_area,df=2)+ (1|dated) + (1|quadkeys) + (1|lockdown:repeats) ,data=lon.pop.M25.sub,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
Sys.time()-tic

#######################################################################################
##one for each time instead
lon.pop.M25.sub<-subset(lon.pop.M25.sub,time=="0800")

tic<-Sys.time()
lon.lme.final<-lmer(clipped_z_score~lockdown*repeats*weekend*ns(IMD,df=2)*ns(coverage_area,df=2)+ (1|dated) + (1|quadkeys) ,data=lon.pop.M25.sub,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
Sys.time()-tic
#AIC=17485693
london.final.effect<-ggpredict(lon.lme.final,terms=c("coverage_area [all]","IMD [6.5, 14.5,20.25,35.5]","lockdown","weekend"),condition=c(repeats="3"))
 png(file="C:/Users/David/Documents/Facebook COVID/London_lockdown_main_final_0800_with_repeats_3.png", width=60,height=42,units="cm",res=200)
 plot(london.final.effect)
 dev.off()

tic<-Sys.time()
lon.lme.final<-lmer(clipped_z_score~lockdown*weekend*ns(IMD,df=2)*ns(coverage_area,df=2)+ (1|dated) + (1|quadkeys) ,data=lon.pop.M25.sub,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
Sys.time()-tic
AIC(lon.lme.final)
#AIC=17528699

##no lockdown, no repeats effects run on thinlinc
#AIC=17575342

tic<-Sys.time()
lon.lme.final<-lmer(clipped_z_score~lockdown*repeats*weekend*IMD*ns(coverage_area,df=2)+ (1|dated) + (1|quadkeys) ,data=lon.pop.M25.sub,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
Sys.time()-tic
AIC(lon.lme.final)
#AIC=17488689


tic<-Sys.time()
lon.lme.final<-lmer(clipped_z_score~lockdown*repeats*weekend*IMD*ns(coverage_area,df=4)+ (1|dated) + (1|quadkeys) ,data=lon.pop.M25.sub,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
Sys.time()-tic
AIC(lon.lme.final)
#AIC=17447622 #so when we give "wriggle room" on the relationship the same number of df is "better" spent on coverage rather than spread on IMD and coverage

london.final.effect<-ggpredict(lon.lme.final,terms=c("coverage_area [all]","IMD [6.5, 17.5,35.5]","lockdown","weekend"),condition=c(repeats="1"))
 png(file="C:/Users/David/Documents/Facebook COVID/London_lockdown_main_final_0800_with_repeats_1.png", width=60,height=42,units="cm",res=200)
 plot(london.final.effect)
 dev.off()
london.final.effect<-ggpredict(lon.lme.final,terms=c("coverage_area [all]","IMD [6.5, 17.5,35.5]","lockdown","weekend"),condition=c(repeats="2"))
 png(file="C:/Users/David/Documents/Facebook COVID/London_lockdown_main_final_0800_with_repeats_2.png", width=60,height=42,units="cm",res=200)
 plot(london.final.effect)
 dev.off()
london.final.effect<-ggpredict(lon.lme.final,terms=c("coverage_area [all]","IMD [6.5, 17.5,35.5]","lockdown","weekend"),condition=c(repeats="3"))
 png(file="C:/Users/David/Documents/Facebook COVID/London_lockdown_main_final_0800_with_repeats_3.png", width=60,height=42,units="cm",res=200)
 plot(london.final.effect)
 dev.off()

########
#### while it may be supported by model selection (but be careful spline curviness selection by AIC 
### 1. there is no real support for a non-linear response on IMD - no real data support (CI breadth) and no expected functional response
### this seems mainly driven by extremal situation, ie the response get exarcebated when we get to 1% deprived and 99% affluent
### 2. there is no real support for beyond quadratic response to coverage. the quadratic is expected from the difference in coverage 
### distribution with IMD but beyond quadratic not really (and no real support for it from data either (large CI)). ns allows for assymetric 'quadratic' which is good

#####################################################################################################################################
####################################################################################################################################
#linear model selection
library(splines)
library(lme4)

load("C:/Users/David/Documents/Facebook COVID/london_corona_FB_M25_sub_Aug_2021.Rdata")
lon.pop.M25.sub$IMDs<-lon.pop.M25.sub$IMD/100
formula<-c("clipped_z_score~lockdown",
"clipped_z_score~lockdown*repeats",
"clipped_z_score~lockdown*weekend",
"clipped_z_score~lockdown*repeats*weekend",
"clipped_z_score~lockdown*IMDs",
"clipped_z_score~lockdown*repeats*IMDs",
"clipped_z_score~lockdown*weekend*IMDs",
"clipped_z_score~lockdown*repeats*weekend*IMDs",
"clipped_z_score~lockdown*coverage_area",
"clipped_z_score~lockdown*repeats*coverage_area",
"clipped_z_score~lockdown*weekend*coverage_area",
"clipped_z_score~lockdown*repeats*weekend*coverage_area",
"clipped_z_score~lockdown*IMDs*coverage_area",
"clipped_z_score~lockdown*repeats*IMDs*coverage_area",
"clipped_z_score~lockdown*weekend*IMDs*coverage_area",
"clipped_z_score~lockdown*repeats*weekend*IMDs*coverage_area",
"clipped_z_score~lockdown*time",
"clipped_z_score~lockdown*repeats*time",
"clipped_z_score~lockdown*weekend*time",
"clipped_z_score~lockdown*repeats*weekend*time",
"clipped_z_score~lockdown*IMDs*time",
"clipped_z_score~lockdown*repeats*IMDs*time",
"clipped_z_score~lockdown*weekend*IMDs*time",
"clipped_z_score~lockdown*repeats*weekend*IMDs*time",
"clipped_z_score~lockdown*coverage_area*time",
"clipped_z_score~lockdown*repeats*coverage_area*time",
"clipped_z_score~lockdown*weekend*coverage_area*time",
"clipped_z_score~lockdown*repeats*weekend*coverage_area*time",
"clipped_z_score~lockdown*IMDs*coverage_area*time",
"clipped_z_score~lockdown*repeats*IMDs*coverage_area*time",
"clipped_z_score~lockdown*weekend*IMDs*coverage_area*time",
"clipped_z_score~lockdown*repeats*weekend*time*IMDs*coverage_area"
)

AIC.df<-data.frame(model=formula, AIC=NA, time=0)
for (i in 31:length(formula)) {
print(formula[i])
flush.console()
tic<-Sys.time()
lon.lme.final<-lmer(as.formula(paste0(formula[i],"+ (1|dated) + (1|quadkeys)")) ,data=lon.pop.M25.sub,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
AIC.df$time[i]<-Sys.time()-tic
AIC.df$AIC[i]<-AIC(lon.lme.final)
rm(lon.lme.final)
gc()
}

save(AIC.df,formula,file="C:/Users/David/Documents/Facebook COVID/modelselection_London_FB.Rdata")

lon.lme.final<-lmer(as.formula(paste0(formula[i],"+ (1|dated) + (1|quadkeys)")) ,data=lon.pop.M25.sub,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
save(lon.lme.final,file="C:/Users/David/Documents/Facebook COVID/London_FB_LINEAR_final_model_32.Rdata")

##########################################################################################################
##########################################################################################################
##########################################################################################################
##########################################################################################################
## addition 22 sept CONTRAST 0800-0000
##########################################################################################################
##########################################################################################################
load("C:/Users/David/Documents/Facebook COVID/london_corona_FB_M25_sub_Aug_2021.Rdata")



london_contrast0800<-subset(lon.pop.M25.sub,time=="0800")
london_contrast0000<-subset(lon.pop.M25.sub,time=="0000")
london_contrast<-merge(london_contrast0800,london_contrast0000,by.x=c("quadkeys","dated"),by.y=c("quadkeys","dated"),all.x=TRUE)
london_contrast$z_score<-london_contrast$clipped_z_score.x-london_contrast$clipped_z_score.y

lon.M25.cont<-london_contrast[,c(1,2,5,33,8,10,12,13,16,17)]
names(lon.M25.cont)<-c("quadkeys","dated","z08","z_score","coverage_area","dates","lockdown","IMD","weekend","repeats")
lon.M25.cont$IMDs<-lon.M25.cont$IMD/100

rm(lon.pop.M25.sub,london_contrast,london_contrast0000,london_contrast0800)
gc()

library(lme4)

formula<-c("z_score~lockdown",
"z_score~lockdown*repeats",
"z_score~lockdown*weekend",
"z_score~lockdown*repeats*weekend",
"z_score~lockdown*IMDs",
"z_score~lockdown*repeats*IMDs",
"z_score~lockdown*weekend*IMDs",
"z_score~lockdown*repeats*weekend*IMDs",
"z_score~lockdown*coverage_area",
"z_score~lockdown*repeats*coverage_area",
"z_score~lockdown*weekend*coverage_area",
"z_score~lockdown*repeats*weekend*coverage_area",
"z_score~lockdown*IMDs*coverage_area",
"z_score~lockdown*repeats*IMDs*coverage_area",
"z_score~lockdown*weekend*IMDs*coverage_area",
"z_score~lockdown*repeats*weekend*IMDs*coverage_area"
)

AIC.df.c<-data.frame(model=formula, AIC=NA, time=0)
for (i in 1:length(formula)) {
print(formula[i])
flush.console()
tic<-Sys.time()
lon.lme.final<-lmer(as.formula(paste0(formula[i],"+ (1|dated) + (1|quadkeys)")) ,data=lon.M25.cont,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
AIC.df.c$time[i]<-Sys.time()-tic
AIC.df.c$AIC[i]<-AIC(lon.lme.final)
rm(lon.lme.final)
gc()
}
AIC.df.c
save(AIC.df.c,formula,file="C:/Users/David/Documents/Facebook COVID/modelselection_London_CONTRAST_FB.Rdata")


###########################################################################################

formula<-c("z08~lockdown",
"z08~lockdown*repeats",
"z08~lockdown*weekend",
"z08~lockdown*repeats*weekend",
"z08~lockdown*IMDs",
"z08~lockdown*repeats*IMDs",
"z08~lockdown*weekend*IMDs",
"z08~lockdown*repeats*weekend*IMDs",
"z08~lockdown*coverage_area",
"z08~lockdown*repeats*coverage_area",
"z08~lockdown*weekend*coverage_area",
"z08~lockdown*repeats*weekend*coverage_area",
"z08~lockdown*IMDs*coverage_area",
"z08~lockdown*repeats*IMDs*coverage_area",
"z08~lockdown*weekend*IMDs*coverage_area",
"z08~lockdown*repeats*weekend*IMDs*coverage_area"
)

AIC.df.c<-data.frame(model=formula, AIC=NA, time=0)
for (i in 1:length(formula)) {
print(formula[i])
flush.console()
tic<-Sys.time()
lon.lme.final<-lmer(as.formula(paste0(formula[i],"+ (1|dated) + (1|quadkeys)")) ,data=lon.M25.cont,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
AIC.df.c$time[i]<-Sys.time()-tic
AIC.df.c$AIC[i]<-AIC(lon.lme.final)
rm(lon.lme.final)
gc()
}
AIC.df.c
save(AIC.df.c,formula,file="C:/Users/David/Documents/Facebook COVID/modelselection_London_0800_FB.Rdata")




###########################################################################################

i=16
lon.lme.final<-lmer(as.formula(paste0(formula[i],"+ (1|dated) + (1|quadkeys)")) ,data=lon.M25.cont,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))


save(lon.lme.final,lon.M25.cont,file="C:/Users/David/Documents/Facebook COVID/London_FB_LINEAR_CONTRAST_final_model_32.Rdata")

#######MANUSCRIPT figure LONDON CONTRAST LOCKDOWN
load("C:/Users/David/Documents/Facebook COVID/London_FB_LINEAR_CONTRAST_final_model_32.Rdata")
lon.final.effect<-ggpredict(lon.lme.final,terms=c("coverage_area [all]","IMDs [0.065, 0.175,0.355]","repeats","weekend"),condition=c(lockdown="2"))

gg.london.0<-plot(lon.final.effect,limits=c(-1,3.5),show.y.title=FALSE,show.x.title=FALSE,show.title=FALSE)
gg.london.0[[1]]$labels$y<-"Predicted difference in Z-score (Z08-Z00)"
gg.london.0$labels$x<-"Proportion of tile covered by greenspace"
gg.london.0[[1]]$labels$subtitle<-"London - week days"
gg.london.0[[2]]$labels$subtitle<-"London - weekend"

library(grid)
library(gridExtra)
library(ggplotify)
#######MANUSCRIPT figure LONDON CONTRAST LOCKDOWN
tiff(file="C:/Users/David/OneDrive - Danmarks Tekniske Universitet/Facebook Greenspace ms/Figure_london_Z_lockdownonly_CONTRAST_repeats.tiff", width=40,height=20,units="cm",res=200)
gg.london.0
dev.off()




save(gg.london.0,file="C:/Users/David/OneDrive - Danmarks Tekniske Universitet/Facebook Greenspace ms/london_GROB_lockdown_wk.Rdata")

###############################################################################################################################################
#################################################################################################################################################
####### continuation
gg.london.2<-plot(lon.final.effect,limits=c(-1,3.5),show.y.title=FALSE,show.x.title=FALSE,show.title=FALSE)
gg.london.2[[1]]$labels$y<-"Predicted difference in Z-score (Z08-Z00)"
gg.london.2$labels$x<-"Proportion of tile covered by greenspace"
gg.london.2[[1]]$labels$title<-"London - require not leaving house with exceptions"
gg.london.2[[1]]$labels$subtitle<-"week days"
gg.london.2[[2]]$labels$subtitle<-"weekend"


lon.final.effect1<-ggpredict(lon.lme.final,terms=c("coverage_area [all]","IMDs [0.065, 0.175,0.355]","repeats","weekend"),condition=c(lockdown="1"))
gg.london.1<-plot(lon.final.effect1,limits=c(-1,3.5),show.y.title=FALSE,show.x.title=FALSE,show.title=FALSE)
gg.london.1[[1]]$labels$y<-"Predicted difference in Z-score (Z08-Z00)"
gg.london.1$labels$x<-"Proportion of tile covered by greenspace"
gg.london.1[[1]]$labels$title<-"London - recommend not leaving house"
gg.london.1[[1]]$labels$subtitle<-"week days"
gg.london.1[[2]]$labels$subtitle<-"weekend"


lon.final.effect0<-ggpredict(lon.lme.final,terms=c("coverage_area [all]","IMDs [0.065, 0.175,0.355]","repeats","weekend"),condition=c(lockdown="0"))
gg.london.0<-plot(lon.final.effect0,limits=c(-1,3.5),show.y.title=FALSE,show.x.title=FALSE,show.title=FALSE)
gg.london.0[[1]]$labels$y<-"Predicted difference in Z-score (Z08-Z00)"
gg.london.0$labels$x<-"Proportion of tile covered by greenspace"
gg.london.0[[1]]$labels$title<-"London - no restrictions"
gg.london.0[[1]]$labels$subtitle<-"week days"
gg.london.0[[2]]$labels$subtitle<-"weekend"

###### MANUSCRIPT FIGURE LONDON 0800 ALL outcomes

library(grid)
library(gridExtra)
library(ggplotify)
tiff(file="C:/Users/David/OneDrive - Danmarks Tekniske Universitet/Facebook Greenspace ms/Figure_london_Z_ALL_CONTRAST_repeats.tiff", width=40,height=40,units="cm",res=200)
grid.arrange(as.grob(gg.london.0),as.grob(gg.london.1),as.grob(gg.london.2),nrow=3,heights=c(10,10,10))
dev.off()





###############################################################################################################################################
#################################################################################################################################################

###############################################################################################################################################
################################################################################################################################################

lon.final.effect<-ggpredict(lon.lme.final,terms=c("coverage_area [all]","IMDs [0.065, 0.175,0.355]","lockdown","weekend"),condition=c(repeats="1"))
 png(file="C:/Users/David/Documents/Facebook COVID/London_lockdown_main_final_CONTRAST_repeat1.png", width=60,height=42,units="cm",res=200)
 plot(lon.final.effect,limits=c(-1,3.5))
 
 dev.off()

lon.final.effect<-ggpredict(lon.lme.final,terms=c("coverage_area [all]","IMDs [0.065, 0.175,0.355]","lockdown","weekend"),condition=c(repeats="2"))
 png(file="C:/Users/David/Documents/Facebook COVID/London_lockdown_main_final_CONTRAST_repeat2.png", width=60,height=42,units="cm",res=200)
 plot(lon.final.effect,limits=c(-2,2))
 dev.off()

##########################################################################################################
##########################################################################################################
##########################################################################################################
##########################################################################################################


##########################################################################################################
##########################################################################################################
library(lme4)

load("C:/Users/David/Documents/Facebook COVID/london_corona_FB_M25_sub_Aug_2021.Rdata")
lon.pop.M25.sub$IMDs<-lon.pop.M25.sub$IMD/100
lon.pop.M25.sub<-subset(lon.pop.M25.sub,time=="0800")
tic<-Sys.time()
lon.lme.final<-lmer(clipped_z_score~lockdown*repeats*weekend*IMDs*coverage_area+ (1|dated) + (1|quadkeys) ,data=lon.pop.M25.sub,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
Sys.time()-tic

save(lon.lme.final,lon.pop.M25.sub,file="C:/Users/David/Documents/Facebook COVID/london_corona_FB_model_linear_0800.Rdata")

rm(lon.pop.M25.sub,lon.lme.final)
gc()

####################################################################################################################
#######0800
load("C:/Users/David/Documents/Facebook COVID/london_corona_FB_model_linear_0800.Rdata")


lon.final.effect<-ggpredict(lon.lme.final,terms=c("coverage_area [all]","IMDs [0.065, 0.175,0.355]","repeats","weekend"),condition=c(lockdown="2"))

gg.london.0<-plot(lon.final.effect,limits=c(-2,3.5),show.y.title=FALSE,show.x.title=FALSE,show.title=FALSE)
gg.london.0[[1]]$labels$y<-"Predicted Z-score (Z08)"
gg.london.0$labels$x<-"Proportion of tile covered by greenspace"
gg.london.0[[1]]$labels$subtitle<-"London - week days"
gg.london.0[[2]]$labels$subtitle<-"London - weekend"

#######MANUSCRIPT figure LONDON 0800 LOCKDOWN
tiff(file="C:/Users/David/OneDrive - Danmarks Tekniske Universitet/Facebook Greenspace ms/Figure_london_Z_lockdownonly_0800_repeats.tiff", width=40,height=20,units="cm",res=200)
gg.london.0
dev.off()

save(gg.london.0,file="C:/Users/David/OneDrive - Danmarks Tekniske Universitet/Facebook Greenspace ms/london_GROB_lockdown_wk_0800.Rdata")


###continuation
lon.final.effect2<-ggpredict(lon.lme.final,terms=c("coverage_area [all]","IMDs [0.065, 0.175,0.355]","repeats","weekend"),condition=c(lockdown="2"))

gg.london.2<-plot(lon.final.effect2,limits=c(-2.5,3.5),show.y.title=FALSE,show.x.title=FALSE,show.title=FALSE)
gg.london.2[[1]]$labels$y<-"Predicted Z-score (Z08)"
gg.london.2$labels$x<-"Proportion of tile covered by greenspace"
gg.london.2[[1]]$labels$title<-"London - require not leaving house with exceptions"
gg.london.2[[1]]$labels$subtitle<-"week days"
gg.london.2[[2]]$labels$subtitle<-"weekend"


lon.final.effect1<-ggpredict(lon.lme.final,terms=c("coverage_area [all]","IMDs [0.065, 0.175,0.355]","repeats","weekend"),condition=c(lockdown="1"))
gg.london.1<-plot(lon.final.effect1,limits=c(-2.5,3.5),show.y.title=FALSE,show.x.title=FALSE,show.title=FALSE)
gg.london.1[[1]]$labels$y<-"Predicted Z-score (Z08)"
gg.london.1$labels$x<-"Proportion of tile covered by greenspace"
gg.london.1[[1]]$labels$title<-"London - recommend not leaving house"
gg.london.1[[1]]$labels$subtitle<-"week days"
gg.london.1[[2]]$labels$subtitle<-"weekend"


lon.final.effect0<-ggpredict(lon.lme.final,terms=c("coverage_area [all]","IMDs [0.065, 0.175,0.355]","repeats","weekend"),condition=c(lockdown="0"))
gg.london.0<-plot(lon.final.effect0,limits=c(-2.5,3.5),show.y.title=FALSE,show.x.title=FALSE,show.title=FALSE)
gg.london.0[[1]]$labels$y<-"Predicted Z-score (Z08)"
gg.london.0$labels$x<-"Proportion of tile covered by greenspace"
gg.london.0[[1]]$labels$title<-"London - no restrictions"
gg.london.0[[1]]$labels$subtitle<-"week days"
gg.london.0[[2]]$labels$subtitle<-"weekend"

###### MANUSCRIPT FIGURE LONDON 0800 ALL outcomes

library(grid)
library(gridExtra)
library(ggplotify)
tiff(file="C:/Users/David/OneDrive - Danmarks Tekniske Universitet/Facebook Greenspace ms/Figure_london_Z_ALL_0800_repeats.tiff", width=40,height=40,units="cm",res=200)
grid.arrange(as.grob(gg.london.0),as.grob(gg.london.1),as.grob(gg.london.2),nrow=3,heights=c(10,10,10))
dev.off()



##########################################################################################################################
###1600
load("C:/Users/David/Documents/Facebook COVID/london_corona_FB_M25_sub_Aug_2021.Rdata")
lon.pop.M25.sub$IMDs<-lon.pop.M25.sub$IMD/100

lon.pop.M25.sub<-subset(lon.pop.M25.sub,time=="1600")
tic<-Sys.time()
lon.lme.final<-lmer(clipped_z_score~lockdown*repeats*weekend*IMDs*coverage_area+ (1|dated) + (1|quadkeys) ,data=lon.pop.M25.sub,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
Sys.time()-tic

save(lon.lme.final,lon.pop.M25.sub,file="C:/Users/David/Documents/Facebook COVID/london_corona_FB_model_linear_1600.Rdata")

rm(lon.pop.M25.sub,lon.lme.final)
gc()




###0000
library(lme4)
load("C:/Users/David/Documents/Facebook COVID/london_corona_FB_M25_sub_Aug_2021.Rdata")
lon.pop.M25.sub$IMDs<-lon.pop.M25.sub$IMD/100

lon.pop.M25.sub<-subset(lon.pop.M25.sub,time=="0000")
tic<-Sys.time()
lon.lme.final<-lmer(clipped_z_score~lockdown*repeats*weekend*IMDs*coverage_area+ (1|dated) + (1|quadkeys) ,data=lon.pop.M25.sub,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
Sys.time()-tic

save(lon.lme.final,lon.pop.M25.sub,file="C:/Users/David/Documents/Facebook COVID/london_corona_FB_model_linear_0000.Rdata")

rm(lon.pop.M25.sub,lon.lme.final)
gc()

#####################################################################################################
#####################################################################################################

###0800
load("C:/Users/David/Documents/Facebook COVID/london_corona_FB_M25_sub_Aug_2021.Rdata")

lon.pop.M25.sub<-subset(lon.pop.M25.sub,time=="0800")
tic<-Sys.time()
lon.lme.final<-lmer(clipped_z_score~lockdown*repeats*weekend*IMD*ns(coverage_area,df=2)+ (1|dated) + (1|quadkeys) ,data=lon.pop.M25.sub,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
Sys.time()-tic

save(lon.lme.final,lon.pop.M25.sub,file="C:/Users/David/Documents/Facebook COVID/london_corona_FB_model_0800.Rdata")

rm(lon.pop.M25.sub,lon.lme.final)
gc()

###1600
load("C:/Users/David/Documents/Facebook COVID/london_corona_FB_M25_sub_Aug_2021.Rdata")

lon.pop.M25.sub<-subset(lon.pop.M25.sub,time=="1600")
tic<-Sys.time()
lon.lme.final<-lmer(clipped_z_score~lockdown*repeats*weekend*IMD*ns(coverage_area,df=2)+ (1|dated) + (1|quadkeys) ,data=lon.pop.M25.sub,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
Sys.time()-tic

save(lon.lme.final,lon.pop.M25.sub,file="C:/Users/David/Documents/Facebook COVID/london_corona_FB_model_1600.Rdata")

rm(lon.pop.M25.sub,lon.lme.final)
gc()




###0000
load("C:/Users/David/Documents/Facebook COVID/london_corona_FB_M25_sub_Aug_2021.Rdata")

lon.pop.M25.sub<-subset(lon.pop.M25.sub,time=="0000")
tic<-Sys.time()
lon.lme.final<-lmer(clipped_z_score~lockdown*repeats*weekend*IMD*ns(coverage_area,df=2)+ (1|dated) + (1|quadkeys) ,data=lon.pop.M25.sub,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
Sys.time()-tic

save(lon.lme.final,lon.pop.M25.sub,file="C:/Users/David/Documents/Facebook COVID/london_corona_FB_model_0000.Rdata")

rm(lon.pop.M25.sub,lon.lme.final)
gc()


library(ggeffects)
##plot 0000
load("C:/Users/David/Documents/Facebook COVID/london_corona_FB_model_0000.Rdata")

london.final.effect<-ggpredict(lon.lme.final,terms=c("coverage_area [all]","IMD [6.5, 17.5,35.5]","lockdown","weekend"),condition=c(repeats="1"))
 png(file="C:/Users/David/Documents/Facebook COVID/London_lockdown_main_final_0000_with_repeats_1.png", width=60,height=42,units="cm",res=200)
 plot(london.final.effect)
 dev.off()
london.final.effect<-ggpredict(lon.lme.final,terms=c("coverage_area [all]","IMD [6.5, 17.5,35.5]","lockdown","weekend"),condition=c(repeats="2"))
 png(file="C:/Users/David/Documents/Facebook COVID/London_lockdown_main_final_0000_with_repeats_2.png", width=60,height=42,units="cm",res=200)
 plot(london.final.effect)
 dev.off()
london.final.effect<-ggpredict(lon.lme.final,terms=c("coverage_area [all]","IMD [6.5, 17.5,35.5]","lockdown","weekend"),condition=c(repeats="3"))
 png(file="C:/Users/David/Documents/Facebook COVID/London_lockdown_main_final_0000_with_repeats_3.png", width=60,height=42,units="cm",res=200)
 plot(london.final.effect)
 dev.off()

####
#the nighttime behaviour does not change with weekend and weekdays within IMD 
# affluent area low greenspace area are less used when no restriction and those leafy places are used more
# this discrepancy (slope) changes with lockdown
# no real changes for deprived areas
#the pattern changed for the 2nd and 3rd waves
#massive exodus for all IMD except for places with high greenspace

####################################################################################################################################

rm(london.final.effect,lon.pop.M25.sub,lon.lme.final)
gc()
load("C:/Users/David/Documents/Facebook COVID/london_corona_FB_model_0800.Rdata")
london.final.effect<-ggpredict(lon.lme.final,terms=c("coverage_area [all]","lockdown","IMD [6.5, 17.5,35.5]","weekend"))
 png(file="C:/Users/David/Documents/Facebook COVID/London_lockdown_0800.png", width=60,height=42,units="cm",res=200)
 plot(london.final.effect)
 dev.off()
load("C:/Users/David/Documents/Facebook COVID/london_corona_FB_model_1600.Rdata")
london.final.effect<-ggpredict(lon.lme.final,terms=c("coverage_area [all]","lockdown","IMD [6.5, 17.5,35.5]","weekend"))
 png(file="C:/Users/David/Documents/Facebook COVID/London_lockdown_1600.png", width=60,height=42,units="cm",res=200)
 plot(london.final.effect)
 dev.off()
load("C:/Users/David/Documents/Facebook COVID/london_corona_FB_model_0000.Rdata")
london.final.effect<-ggpredict(lon.lme.final,terms=c("coverage_area [all]","lockdown","IMD [6.5, 17.5,35.5]","weekend"))
 png(file="C:/Users/David/Documents/Facebook COVID/London_lockdown_0000.png", width=60,height=42,units="cm",res=200)
 plot(london.final.effect)
 dev.off()

##############################################################################################
##############################################################################################
##############################################################################################
###FINAL PLOTS

library(ggeffects)
library(ggplot2)

load("C:/Users/David/Documents/Facebook COVID/london_corona_FB_M25_sub_Aug_2021.Rdata")
lon.pop.M25.sub$IMDs<-lon.pop.M25.sub$IMD/100
lon.pop.M25.sub<-subset(lon.pop.M25.sub,time=="0800")

load("C:/Users/David/Documents/Facebook COVID/london_corona_FB_model_linear_0800.Rdata")
london.final.effect<-ggpredict(lon.lme.final,terms=c("coverage_area [all]","lockdown","IMDs [0.065, 0.175,0.355]","repeats"),condition=c(weekend="yes"))
 png(file="C:/Users/David/Documents/Facebook COVID/London_lockdown_linear_wkend_0800.png", width=60,height=42,units="cm",res=200)
 plot(london.final.effect,limits=c(-4,3))
 dev.off()

london.final.effect<-ggpredict(lon.lme.final,terms=c("coverage_area [all]","lockdown","IMDs [0.065, 0.175,0.355]","repeats"),condition=c(weekend="no"))
 png(file="C:/Users/David/Documents/Facebook COVID/London_lockdown_linear_wk_0800.png", width=60,height=42,units="cm",res=200)
 plot(london.final.effect,limits=c(-4,3))
 dev.off()
 
############## 
load("C:/Users/David/Documents/Facebook COVID/london_corona_FB_M25_sub_Aug_2021.Rdata")
lon.pop.M25.sub$IMDs<-lon.pop.M25.sub$IMD/100
lon.pop.M25.sub<-subset(lon.pop.M25.sub,time=="1600")
 
load("C:/Users/David/Documents/Facebook COVID/london_corona_FB_model_linear_1600.Rdata")
london.final.effect<-ggpredict(lon.lme.final,terms=c("coverage_area [all]","lockdown","IMDs [0.065, 0.175,0.355]","repeats"),condition=c(weekend="yes"))
 png(file="C:/Users/David/Documents/Facebook COVID/London_lockdown_linear_wkend_1600.png", width=60,height=42,units="cm",res=200)
 plot(london.final.effect,limits=c(-4,3))
 dev.off()
london.final.effect<-ggpredict(lon.lme.final,terms=c("coverage_area [all]","lockdown","IMDs [0.065, 0.175,0.355]","repeats"),condition=c(weekend="no"))
 png(file="C:/Users/David/Documents/Facebook COVID/London_lockdown_linear_wk_1600.png", width=60,height=42,units="cm",res=200)
 plot(london.final.effect,limits=c(-4,3))
 dev.off()


#######
load("C:/Users/David/Documents/Facebook COVID/london_corona_FB_M25_sub_Aug_2021.Rdata")
lon.pop.M25.sub$IMDs<-lon.pop.M25.sub$IMD/100
lon.pop.M25.sub<-subset(lon.pop.M25.sub,time=="0000")

load("C:/Users/David/Documents/Facebook COVID/london_corona_FB_model_linear_0000.Rdata")
london.final.effect<-ggpredict(lon.lme.final,terms=c("coverage_area [all]","lockdown","IMDs [0.065, 0.175,0.355]","repeats"),condition=c(weekend="yes"))
 png(file="C:/Users/David/Documents/Facebook COVID/London_lockdown_linear_wkend_0000.png", width=60,height=42,units="cm",res=200)
 plot(london.final.effect,limits=c(-4,3))
 dev.off()
london.final.effect<-ggpredict(lon.lme.final,terms=c("coverage_area [all]","lockdown","IMDs [0.065, 0.175,0.355]","repeats"),condition=c(weekend="no"))
 png(file="C:/Users/David/Documents/Facebook COVID/London_lockdown_linear_wk_0000.png", width=60,height=42,units="cm",res=200)
 plot(london.final.effect,limits=c(-4,3))
 dev.off()

##FINAL PLOTS
##############################################################################################
##############################################################################################
##############################################################################################
##############################################################################################

#################################################################################################

#######################
#### greenspace distribution #within M25 there is no difference and no covariance, noice
load("C:/Users/David/Documents/Facebook COVID/london_corona_FB_M25_Aug_2021.Rdata")
lon.pop.M25<-lon.pop.M25[,c(1,2,12,19:29,31,32)]
quadkeys.u<-unique(lon.pop.M25$quadkeys)
quadkey.df<-lon.pop.M25[match(quadkeys.u,lon.pop.M25$quadkeys),]

rm(lon.pop.M25)
gc()
library(hdrcde)
IMD.quant<-quantile(quadkey.df$IMD,c(0.05,.2,.4,.6,.8,.95))

plot(cde(quadkey.df$coverage_area[quadkey.df$IMD<IMD.quant[2]],quadkey.df$gardens[quadkey.df$IMD<IMD.quant[2]],nymargin=100))  
windows()
plot(cde(quadkey.df$coverage_area[quadkey.df$IMD>IMD.quant[5]],quadkey.df$gardens[quadkey.df$IMD>IMD.quant[5]],nymargin=100))  

hist(quadkey.df$coverage_area[quadkey.df$IMD>IMD.quant[5]],100)
windows()
hist(quadkey.df$coverage_area[quadkey.df$IMD<IMD.quant[2]],100)

hist(quadkey.df$IMD)

cov(quadkey.df$IMD,quadkey.df$coverage_area) #nice

quadkey.df$IMD.5<-"0-20"
IMD.quant<-quantile(quadkey.df$IMD,c(.2,.4,.6,.8))
quadkey.df$IMD.5[quadkey.df$IMD>IMD.quant[1]&quadkey.df$IMD<=IMD.quant[2]]<-"20-40"
quadkey.df$IMD.5[quadkey.df$IMD>IMD.quant[2]&quadkey.df$IMD<=IMD.quant[3]]<-"40-60"
quadkey.df$IMD.5[quadkey.df$IMD>IMD.quant[3]&quadkey.df$IMD<=IMD.quant[4]]<-"60-80"
quadkey.df$IMD.5[quadkey.df$IMD>IMD.quant[4]]<-"80-100"
quadkey.df$IMD.5<-factor(quadkey.df$IMD.5)

plot(cde(quadkey.df$coverage_area[quadkey.df$IMD<IMD.quant[1]],quadkey.df$gardens[quadkey.df$IMD<IMD.quant[1]],nxmargin=100))  
windows()
plot(cde(quadkey.df$coverage_area[quadkey.df$IMD>IMD.quant[4]],quadkey.df$gardens[quadkey.df$IMD>IMD.quant[4]],nxmargin=100))  

quantile(quadkey.df$coverage_area[quadkey.df$IMD.5=="0-20"],c(.025,.1,.25,.5,.75,.9,.975,1))

quantile(quadkey.df$coverage_area[quadkey.df$IMD.5=="20-40"],c(.025,.1,.25,.5,.75,.9,.975,1))

quantile(quadkey.df$coverage_area[quadkey.df$IMD.5=="40-60"],c(.025,.1,.25,.5,.75,.9,.975,1))

quantile(quadkey.df$coverage_area[quadkey.df$IMD.5=="60-80"],c(.025,.1,.25,.5,.75,.9,.975,1))

quantile(quadkey.df$coverage_area[quadkey.df$IMD.5=="80-100"],c(.025,.1,.25,.5,.75,.9,.975,1))

library(ggplot2)
ggplot(quadkey.df, aes(IMD.5,coverage_area)) + geom_violin()

ggplot(quadkey.df[quadkey.df$coverage_area>.5,], aes(IMD.5,gardens)) + geom_violin()


ggplot(quadkey.df[quadkey.df$IMD.5=="0-20",], aes(coverage_area,gardens)) + 
	geom_hex() +
	scale_fill_viridis_c() +
	geom_point(shape = '.', col = 'white')
	
windows()
  
ggplot(quadkey.df[quadkey.df$IMD.5=="80-100",], aes(coverage_area,gardens)) + 
	geom_hex() +
	scale_fill_viridis_c() +
	geom_point(shape = '.', col = 'white')
#######################################################################################################


##plot 0800
load("C:/Users/David/Documents/Facebook COVID/london_corona_FB_model_0800.Rdata")

london.final.effect<-ggpredict(lon.lme.final,terms=c("coverage_area [all]","IMD [6.5, 17.5,35.5]","lockdown","weekend"),condition=c(repeats="1"))
 png(file="C:/Users/David/Documents/Facebook COVID/London_lockdown_main_final_0800_with_repeats_1.png", width=60,height=42,units="cm",res=200)
 plot(london.final.effect)
 dev.off()
london.final.effect<-ggpredict(lon.lme.final,terms=c("coverage_area [all]","IMD [6.5, 17.5,35.5]","lockdown","weekend"),condition=c(repeats="2"))
 png(file="C:/Users/David/Documents/Facebook COVID/London_lockdown_main_final_0800_with_repeats_2.png", width=60,height=42,units="cm",res=200)
 plot(london.final.effect)
 dev.off()
london.final.effect<-ggpredict(lon.lme.final,terms=c("coverage_area [all]","IMD [6.5, 17.5,35.5]","lockdown","weekend"),condition=c(repeats="3"))
 png(file="C:/Users/David/Documents/Facebook COVID/London_lockdown_main_final_0800_with_repeats_3.png", width=60,height=42,units="cm",res=200)
 plot(london.final.effect)
 dev.off()


rm(london.final.effect,lon.pop.M25.sub,lon.lme.final)
gc()



##plot 1600
load("C:/Users/David/Documents/Facebook COVID/london_corona_FB_model_1600.Rdata")

london.final.effect<-ggpredict(lon.lme.final,terms=c("coverage_area [all]","IMD [6.5, 17.5,35.5]","lockdown","weekend"),condition=c(repeats="1"))
 png(file="C:/Users/David/Documents/Facebook COVID/London_lockdown_main_final_1600_with_repeats_1.png", width=60,height=42,units="cm",res=200)
 plot(london.final.effect)
 dev.off()
london.final.effect<-ggpredict(lon.lme.final,terms=c("coverage_area [all]","IMD [6.5, 17.5,35.5]","lockdown","weekend"),condition=c(repeats="2"))
 png(file="C:/Users/David/Documents/Facebook COVID/London_lockdown_main_final_1600_with_repeats_2.png", width=60,height=42,units="cm",res=200)
 plot(london.final.effect)
 dev.off()
london.final.effect<-ggpredict(lon.lme.final,terms=c("coverage_area [all]","IMD [6.5, 17.5,35.5]","lockdown","weekend"),condition=c(repeats="3"))
 png(file="C:/Users/David/Documents/Facebook COVID/London_lockdown_main_final_1600_with_repeats_3.png", width=60,height=42,units="cm",res=200)
 plot(london.final.effect)
 dev.off()


rm(london.final.effect,lon.pop.M25.sub,lon.lme.final)
gc()


