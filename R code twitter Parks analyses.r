####################################
###### twitter parks analysis
library(stm)
library(glmmTMB)
library(ggeffects)

load("C:/Users/David/Documents/Facebook COVID/Twitter/BerlinParks.Rdata")
load("C:/Users/David/Documents/Facebook COVID/Twitter/ParisParks.Rdata")
load("C:/Users/David/Documents/Facebook COVID/Twitter/LondonParks.Rdata")


table(tweets$lang)

table(tweets_berlin_all$lang)

table(tweets_paris_all$lang)


lockdown<-read.csv("C:/Users/David/Documents/Facebook COVID/Google trends/OxCGRT_latest_Aug_30_2021.txt",header=T)

lockdown$date.pos<-as.Date(as.character(lockdown$Date),"%Y%m%d")

London<-lockdown$C6_Stay_at_home_requirements[lockdown$RegionName=="England"]
Paris<-lockdown$C6_Stay_at_home_requirements[lockdown$CountryCode=="FRA"]
Berlin<-lockdown$C6_Stay_at_home_requirements[lockdown$CountryCode=="DEU"]

lockdown.dat<-data.frame(date=c(lockdown$date.pos[lockdown$RegionName=="England"],lockdown$date.pos[lockdown$RegionName=="FRA"],lockdown$date.pos[lockdown$RegionName=="DEU"]),region=rep(c("London","Paris","Berlin"),each=length(London)),lockdown=c(London,Paris,Berlin))

lockdown.dat<-lockdown.dat[-which(lockdown.dat$date=="2021-08-31"),] #matching tweet query
lockdown.dat<-lockdown.dat[-which(lockdown.dat$date=="2021-08-30"),] #matching tweet query
which(is.na(lockdown.dat$lockdown))

lockdown.dat$lockdown[606:607]<-0 #a posteriori London
lockdown.dat$lockdown[1214]<-0 #a posteriori Paris
lockdown.dat$lockdown[1820:1821]<-2 #a posteriori Berlin

##let's restrict to native language
#Paris: 3369 fr tweet
#Berlin: 1707 de tweet
#London: 94470 en tweets

paris.tweet<-data.frame(id=seq(1,dim(tweets_paris_all)[1],1),date=as.Date(tweets_paris_all$created_at),lang=tweets_paris_all$lang,like=tweets_paris_all$public_metrics$like_count,text=tweets_paris_all$text,FP=NA)

for (i in 1:dim(tweets_paris_all)[1]) {

paris.tweet$FP[i]<-any(c(grepl("Princ",tweets_paris_all$entities$annotations[[i]]$normalized_text,ignore.case=TRUE),grepl("Princ",tweets_paris_all$text[i],ignore.case=TRUE)))
#some Parc des Princes still sneaked in
}

paris.tweet<-subset(paris.tweet,FP==FALSE)
paris.tweet<-subset(paris.tweet,lang=="fr")

paris.tweet$lockdown<-lockdown.dat$lockdown[lockdown.dat$region=="Paris"][match(paris.tweet$date,lockdown.dat$date[lockdown.dat$region=="Paris"])]
paris.tweet$lockdown<-factor(paris.tweet$lockdown)

library(textclean)
library(stm)
library(stringr)

lesmois<-c("ème","janvier","février","fevrier","mars","avril","mai","juin","juillet","aout","août","septembre","octobre","novembre","décembre","decembre","Vient de publier une photo")
text<-paris.tweet$text
for (i in 1:length(lesmois)) {
text<-str_replace_all(text,c(lesmois[i]),"")

}


paris.tweet.text<-textProcessor(text,metadata=paris.tweet[,c(1,2,4,7)],removenumbers=TRUE,striphtml=TRUE,language="french")

paris.mat<-prepDocuments(paris.tweet.text$documents,paris.tweet.text$vocab,paris.tweet.text$meta,lower.thresh=10,upper.thresh=1000)

# paris.k<-searchK(paris.mat$documents,paris.mat$vocab,prevalence=~lockdown,data=paris.mat$meta,K=c(10,12,14,15,16,18),init.type="Spectral")

# plot(paris.k)

paris.stm<-stm(paris.mat$documents,paris.mat$vocab,prevalence=~lockdown,data=paris.mat$meta,K=0,init.type="Spectral") #32

prep <- estimateEffect(1:39 ~ lockdown, paris.stm,meta = paris.mat$meta, uncertainty = "Global")

plot(prep, covariate = "lockdown", topics = c(22,25,17,4),model = paris.stm, labeltype="lift", method = "pointestimate",n=5)


plot(prep, covariate = "lockdown", topics = c(1:39),model = paris.stm, cov.value1="2",cov.value2="1", method = "difference")

labelTopics(paris.stm,c(22,25,17,4))

cloud(paris.stm,22,threh=.75)

library(stminsights)

paris.ef<-get_effects(prep,variable="lockdown",type="pointestimate")

library(dplyr)
library(ggplot2)

# ggplot(filter(paris.ef,topic%in%c(22,25,17,4)), aes(x=topic,y=proportion,color=value))+
# geom_errorbar(aes(ymin=lower,ymax=upper))+
# geom_point(size=2)+
# theme_classic()+
# labs(x="tweet topic",y="topic prevalence")


#this is good:
paris.plot<-ggplot(paris.ef, aes(x=topic,y=proportion,color=value))+
geom_errorbar(aes(ymin=lower,ymax=upper))+
geom_point(size=2)+
theme_classic()+
labs(x="tweet topic",y="topic prevalence",color="mobility restriction")+
theme(legend.position=c(.6,.8))

#cloud just goes to wordcloud::wordcloud
source("C:/Users/David/Documents/Facebook COVID/cloud_multitopic.r")

g0<-cloud_multitopic(paris.stm,topic=c(1,25,37),max.words=30)+ggtitle("0: no measures")+theme(plot.title = element_text(size=8)) # lockdown 0
g1<-cloud_multitopic(paris.stm,topic=c(14,16,17,31,4,5,8),max.words=30)+ggtitle("1: recommend not leaving house")+theme(plot.title = element_text(size=8))  # lockdown 1
g2<-cloud_multitopic(paris.stm,topic=c(10,19,21,22,39),max.words=30)+ggtitle("2: require not leaving house with exceptions")+theme(plot.title = element_text(size=8))  # lockdown 2

library(grid)
library(gridExtra)

layout<-rbind(	c(1,2,3),
				c(4,4,4))
				
#####################MANUSCRIPT FIGURE paris stm################################################
tiff(file="C:/Users/David/OneDrive - Danmarks Tekniske Universitet/Facebook Greenspace ms/Figure_Paris_STM.tiff",width=20,height=15,res=200,units="cm")
grid.arrange(g0,g1,g2,paris.plot,heights=c(5,10),layout_matrix=layout)
dev.off()

###########################################################################################################################################################
###########################################################################################################################################################

###########################################################################################################################################################
###########################################################################################################################################################
###Berlin STM


berlin.tweet<-data.frame(id=seq(1,dim(tweets_berlin_all)[1],1),date=as.Date(tweets_berlin_all$created_at),lang=tweets_berlin_all$lang,like=tweets_berlin_all$public_metrics$like_count,text=tweets_berlin_all$text,FP=NA)

berlin.tweet<-subset(berlin.tweet,lang=="de")

berlin.tweet$lockdown<-lockdown.dat$lockdown[lockdown.dat$region=="Berlin"][match(berlin.tweet$date,lockdown.dat$date[lockdown.dat$region=="Berlin"])]
berlin.tweet$lockdown<-factor(berlin.tweet$lockdown)

library(textclean)
library(stm)
library(stringr)

lesmois<-c("Januar", "Februar", "März", "April", "Mai", "Juni", "Juli", "August", "September", "Oktober", "November","Dezember","Gerade ein Foto gepostet")
text<-berlin.tweet$text
for (i in 1:length(lesmois)) {
text<-str_replace_all(text,c(lesmois[i]),"")

}


berlin.tweet.text<-textProcessor(text,metadata=berlin.tweet[,c(1,2,4,7)],removenumbers=TRUE,striphtml=TRUE,language="german")

berlin.mat<-prepDocuments(berlin.tweet.text$documents,berlin.tweet.text$vocab,berlin.tweet.text$meta,lower.thresh=10,upper.thresh=600)

berlin.stm<-stm(berlin.mat$documents,berlin.mat$vocab,prevalence=~lockdown,data=berlin.mat$meta,K=0,init.type="Spectral") #32

prep <- estimateEffect(1:28 ~ lockdown, berlin.stm,meta = berlin.mat$meta, uncertainty = "Global")

library(stminsights)

berlin.ef<-get_effects(prep,variable="lockdown",type="pointestimate")

library(dplyr)
library(ggplot2)

#this is good:
berlin.plot<-ggplot(berlin.ef, aes(x=topic,y=proportion,color=value))+
geom_errorbar(aes(ymin=lower,ymax=upper))+
geom_point(size=2)+
theme_classic()+
labs(x="tweet topic",y="topic prevalence",color="mobility restriction")+
theme(legend.position=c(.6,.8))

#cloud just goes to wordcloud::wordcloud
source("C:/Users/David/Documents/Facebook COVID/cloud_multitopic.r")

g0<-cloud_multitopic(berlin.stm,topic=c(13,23),max.words=30)+ggtitle("0: no measures")+theme(plot.title = element_text(size=8)) # lockdown 0
g1<-cloud_multitopic(berlin.stm,topic=c(20,6),max.words=30)+ggtitle("1: recommend not leaving house")+theme(plot.title = element_text(size=8))  # lockdown 1
g2<-cloud_multitopic(berlin.stm,topic=c(15,18,22,27,5),max.words=30)+ggtitle("2: require not leaving house with exceptions")+theme(plot.title = element_text(size=8))  # lockdown 2

library(grid)
library(gridExtra)

layout<-rbind(	c(1,2,3),
				c(4,4,4))
				
#####################MANUSCRIPT FIGURE berlin stm################################################
tiff(file="C:/Users/David/OneDrive - Danmarks Tekniske Universitet/Facebook Greenspace ms/Figure_berlin_STM.tiff",width=20,height=15,res=200,units="cm")
grid.arrange(g0,g1,g2,berlin.plot,heights=c(5,10),layout_matrix=layout)
dev.off()

###########################################################################################################################################################
###########################################################################################################################################################
###########################################################################################################################################################
###########################################################################################################################################################

###########################################################################################################################################################
###########################################################################################################################################################
###london STM


london.tweet<-data.frame(id=seq(1,dim(tweets)[1],1),date=as.Date(tweets$created_at),lang=tweets$lang,like=tweets$public_metrics$like_count,text=tweets$text,FP=NA)

london.tweet<-subset(london.tweet,lang=="en")

london.tweet$lockdown<-lockdown.dat$lockdown[lockdown.dat$region=="London"][match(london.tweet$date,lockdown.dat$date[lockdown.dat$region=="London"])]
london.tweet$lockdown<-factor(london.tweet$lockdown)

library(textclean)
library(stm)
library(stringr)

lesmois<-c("january", "february", "march", "april", "may", "june", "july", "august", "september", "october", "november","december")
text<-london.tweet$text
for (i in 1:length(lesmois)) {
text<-str_replace_all(text,c(lesmois[i]),"")

}


london.tweet.text<-textProcessor(text,metadata=london.tweet[,c(1,2,4,7)],removenumbers=TRUE,striphtml=TRUE,language="english")
length(text)

london.mat<-prepDocuments(london.tweet.text$documents,london.tweet.text$vocab,london.tweet.text$meta,lower.thresh=10,upper.thresh=30000)

london.stm<-stm(london.mat$documents,london.mat$vocab,prevalence=~lockdown,data=london.mat$meta,K=0,init.type="Spectral") #32

save(paris.stm,london.stm,berlin.stm,file="C:/Users/David/Documents/Facebook COVID/STM_all_cities.Rdata")

prep <- estimateEffect(1:57 ~ lockdown, london.stm,meta = london.mat$meta, uncertainty = "Global")

library(stminsights)

london.ef<-get_effects(prep,variable="lockdown",type="pointestimate")

library(dplyr)
library(ggplot2)

#this is good:
london.plot<-ggplot(london.ef, aes(x=topic,y=proportion,color=value))+
geom_errorbar(aes(ymin=lower,ymax=upper))+
geom_point(size=2)+
theme_classic()+
labs(x="tweet topic",y="topic prevalence",color="mobility restriction")+
theme(legend.position=c(.6,.8))

#cloud just goes to wordcloud::wordcloud lets change that
source("C:/Users/David/Documents/Facebook COVID/cloud_multitopic.r")

g0<-cloud_multitopic(london.stm,topic=c(25,29,41,48,49,5,55),max.words=30)+ggtitle("0: no measures")+theme(plot.title = element_text(size=8)) # lockdown 0
g1<-cloud_multitopic(london.stm,topic=c(34,36,43,45,46,54,9),max.words=30)+ggtitle("1: recommend not leaving house")+theme(plot.title = element_text(size=8))  # lockdown 1
g2<-cloud_multitopic(london.stm,topic=c(14,15,19,2,26,28,35,44,52,6),max.words=30)+ggtitle("2: require not leaving house with exceptions")+theme(plot.title = element_text(size=8))  # lockdown 2

library(grid)
library(gridExtra)

layout<-rbind(	c(1,2,3),
				c(4,4,4))
				
#####################MANUSCRIPT FIGURE london stm################################################
tiff(file="C:/Users/David/OneDrive - Danmarks Tekniske Universitet/Facebook Greenspace ms/Figure_london_STM.tiff",width=30,height=20,res=200,units="cm")
grid.arrange(g0,g1,g2,london.plot,heights=c(5,10),layout_matrix=layout)
dev.off()

###########################################################################################################################################################
###########################################################################################################################################################
###########################################################################################################################################################
###########################################################################################################################################################




#tweets$entities$annotations #df $type
dates.lon<-as.Date(tweets$created_at)
date.t.df.lon<-as.data.frame(table(dates.lon))
date.t.df.lon$dates<-as.Date(date.t.df.lon$dates)

dates.ber<-as.Date(tweets_berlin_all$created_at)
date.t.df.ber<-as.data.frame(table(dates.ber))
date.t.df.ber$dates<-as.Date(date.t.df.ber$dates)

#dates.par<-as.Date(tweets_paris_all$created_at)
#we need to get ride of the false positive of the Parc des Princes

paris.tweet<-data.frame(id=seq(1,dim(tweets_paris_all)[1],1),date=as.Date(tweets_paris_all$created_at),lang=tweets_paris_all$lang,like=tweets_paris_all$public_metrics$like_count,text=tweets_paris_all$text,FP=NA)

for (i in 1:dim(tweets_paris_all)[1]) {

paris.tweet$FP[i]<-any(c(grepl("Princ",tweets_paris_all$entities$annotations[[i]]$normalized_text,ignore.case=TRUE),grepl("Princ",tweets_paris_all$text[i],ignore.case=TRUE)))
#some Parc des Princes still sneaked in
}

paris.tweet<-subset(paris.tweet,FP==FALSE)


date.t.df.par<-as.data.frame(table(paris.tweet$date))
names(date.t.df.par)[1]<-"dates"
date.t.df.par$dates<-as.Date(date.t.df.par$dates)


tweet.vol.lon<-merge(lockdown.dat[lockdown.dat$region=="London",],date.t.df.lon,by.x="date",by.y="dates",all.x=TRUE,all.y=TRUE)
tweet.vol.par<-merge(lockdown.dat[lockdown.dat$region=="Paris",],date.t.df.par,by.x="date",by.y="dates",all.x=TRUE,all.y=TRUE)
tweet.vol.ber<-merge(lockdown.dat[lockdown.dat$region=="Berlin",],date.t.df.ber,by.x="date",by.y="dates",all.x=TRUE,all.y=TRUE)

tweet.vol.ber$Freq[which(is.na(tweet.vol.ber$Freq))]<-0
tweet.vol.lon$Freq[which(is.na(tweet.vol.lon$Freq))]<-0
tweet.vol.par$Freq[which(is.na(tweet.vol.par$Freq))]<-0

tweet.vol.lon$lockdown.phase<-factor(tweet.vol.lon$lockdown)
tweet.vol.ber$lockdown.phase<-factor(tweet.vol.ber$lockdown)
tweet.vol.par$lockdown.phase<-factor(tweet.vol.par$lockdown)

ml<-1
nl<-1
pl<-1

mb<-1
nb<-1
pb<-1

mp<-1
np<-1
pp<-1

tweet.vol.lon$repeats<-1
tweet.vol.par$repeats<-1
tweet.vol.ber$repeats<-1


for (i in 2:dim(tweet.vol.lon)[1]) {

####lon
if (tweet.vol.lon$lockdown.phase[i]!=tweet.vol.lon$lockdown.phase[i-1]) {

if (tweet.vol.lon$lockdown.phase[i-1]=="0") {ml<-ml+1}
if (tweet.vol.lon$lockdown.phase[i-1]=="1") {nl<-nl+1}
if (tweet.vol.lon$lockdown.phase[i-1]=="2") {pl<-pl+1}

}

if (tweet.vol.lon$lockdown.phase[i]=="0") {tweet.vol.lon$repeats[i]<-ml}
if (tweet.vol.lon$lockdown.phase[i]=="1") {tweet.vol.lon$repeats[i]<-nl}
if (tweet.vol.lon$lockdown.phase[i]=="2") {tweet.vol.lon$repeats[i]<-pl}

###########

####par
if (tweet.vol.par$lockdown.phase[i]!=tweet.vol.par$lockdown.phase[i-1]) {

if (tweet.vol.par$lockdown.phase[i-1]=="0") {mp<-mp+1}
if (tweet.vol.par$lockdown.phase[i-1]=="1") {np<-np+1}
if (tweet.vol.par$lockdown.phase[i-1]=="2") {pp<-pp+1}

}

if (tweet.vol.par$lockdown.phase[i]=="0") {tweet.vol.par$repeats[i]<-mp}
if (tweet.vol.par$lockdown.phase[i]=="1") {tweet.vol.par$repeats[i]<-np}
if (tweet.vol.par$lockdown.phase[i]=="2") {tweet.vol.par$repeats[i]<-pp}

###########

####ber
if (tweet.vol.ber$lockdown.phase[i]!=tweet.vol.ber$lockdown.phase[i-1]) {

if (tweet.vol.ber$lockdown.phase[i-1]=="0") {mb<-mb+1}
if (tweet.vol.ber$lockdown.phase[i-1]=="1") {nb<-nb+1}
if (tweet.vol.ber$lockdown.phase[i-1]=="2") {pb<-pb+1}

}

if (tweet.vol.ber$lockdown.phase[i]=="0") {tweet.vol.ber$repeats[i]<-mb}
if (tweet.vol.ber$lockdown.phase[i]=="1") {tweet.vol.ber$repeats[i]<-nb}
if (tweet.vol.ber$lockdown.phase[i]=="2") {tweet.vol.ber$repeats[i]<-pb}

###########

 
}

# plot((tweet.vol$C6_Stay_at_home_requirements)~tweet.vol$date.pos,ylim=c(0,4))
# lines(tweet.vol$repeats~tweet.vol$date.pos)

tweet.vol.lon$phase<-1
tweet.vol.par$phase<-1
tweet.vol.ber$phase<-1

for (i in 2:dim(tweet.vol.lon)[1]) {

####lon
if (tweet.vol.lon$lockdown.phase[i]!=tweet.vol.lon$lockdown.phase[i-1]) {
tweet.vol.lon$phase[i]<-tweet.vol.lon$phase[i-1]+1} else {
tweet.vol.lon$phase[i]<-tweet.vol.lon$phase[i-1]
}
######

####ber
if (tweet.vol.ber$lockdown.phase[i]!=tweet.vol.ber$lockdown.phase[i-1]) {
tweet.vol.ber$phase[i]<-tweet.vol.ber$phase[i-1]+1} else {
tweet.vol.ber$phase[i]<-tweet.vol.ber$phase[i-1]
}
######
####par
if (tweet.vol.par$lockdown.phase[i]!=tweet.vol.par$lockdown.phase[i-1]) {
tweet.vol.par$phase[i]<-tweet.vol.par$phase[i-1]+1} else {
tweet.vol.par$phase[i]<-tweet.vol.par$phase[i-1]
}
######

}
tweet.vol.par$phase<-factor(tweet.vol.par$phase)
tweet.vol.lon$phase<-factor(tweet.vol.lon$phase)
tweet.vol.ber$phase<-factor(tweet.vol.ber$phase)


tweet.vol.lon$repeats.cum<-as.numeric(tweet.vol.lon$repeats)
tweet.vol.par$repeats.cum<-as.numeric(tweet.vol.par$repeats)
tweet.vol.ber$repeats.cum<-as.numeric(tweet.vol.ber$repeats)

for (i in 2:dim(tweet.vol.lon)[1]) {

###par
if (tweet.vol.par$lockdown.phase[i]!=tweet.vol.par$lockdown.phase[i-1]) {
tweet.vol.par$repeats.cum[i]<-tweet.vol.par$repeats.cum[i-1]+1
}
#######

###ber
if (tweet.vol.ber$lockdown.phase[i]!=tweet.vol.ber$lockdown.phase[i-1]) {
tweet.vol.ber$repeats.cum[i]<-tweet.vol.ber$repeats.cum[i-1]+1
}
#######

###lon
if (tweet.vol.lon$lockdown.phase[i]!=tweet.vol.lon$lockdown.phase[i-1]) {
tweet.vol.lon$repeats.cum[i]<-tweet.vol.lon$repeats.cum[i-1]+1
}
#######


}
tweet.vol.lon$repeats.cum<-factor(tweet.vol.lon$repeats.cum)
tweet.vol.par$repeats.cum<-factor(tweet.vol.par$repeats.cum)
tweet.vol.ber$repeats.cum<-factor(tweet.vol.ber$repeats.cum)


tweet.vol.lon$fake<-months(tweet.vol.lon$date)
tweet.vol.par$fake<-months(tweet.vol.par$date)
tweet.vol.ber$fake<-months(tweet.vol.ber$date)

tweet.vol.lon$all<-factor(paste(tweet.vol.lon$lockdown.phase,tweet.vol.lon$repeats,sep="."))
tweet.vol.par$all<-factor(paste(tweet.vol.par$lockdown.phase,tweet.vol.par$repeats,sep="."))
tweet.vol.ber$all<-factor(paste(tweet.vol.ber$lockdown.phase,tweet.vol.ber$repeats,sep="."))

library(glmmTMB)

tweet.vol.lon$date.f<-numFactor(as.numeric(tweet.vol.lon$date))
tweet.vol.lon$repeats<-factor(tweet.vol.lon$repeats)

tweet.vol.ber$date.f<-numFactor(as.numeric(tweet.vol.ber$date))
tweet.vol.ber$repeats<-factor(tweet.vol.ber$repeats)

tweet.vol.par$date.f<-numFactor(as.numeric(tweet.vol.par$date))
tweet.vol.par$repeats<-factor(tweet.vol.par$repeats)

tweet.vol.lon$group<-1
tweet.vol.par$group<-1
tweet.vol.ber$group<-1


save(tweet.vol.ber,tweet.vol.par,tweet.vol.lon,file="C:/Users/David/Documents/Facebook COVID/Twitter/tweet.volume.df.all.Rdata")

tweet.count<-rbind(tweet.vol.par,tweet.vol.ber[,c(1:3,5:13)],tweet.vol.lon[,c(1:3,5:13)])

tweet.count$region<-factor(tweet.count$region)

library(ggeffects)
######################
#london
vol.glm.0<-glmmTMB(Freq~lockdown.phase+ar1(date.f+0|group),data=tweet.vol.lon,nbinom2)
vol.glm.1<-glmmTMB(Freq~lockdown.phase,data=tweet.vol.lon,nbinom2)

table(tweet.vol.lon$lockdown.phase,tweet.vol.lon$repeats)

vol.glm.00<-glmmTMB(Freq~1+ar1(date.f+0|group),data=subset(tweet.vol.lon,repeats!="4"),nbinom2)
vol.glm.0<-glmmTMB(Freq~lockdown.phase*repeats+ar1(date.f+0|group),data=subset(tweet.vol.lon,repeats!="4"),nbinom2)
vol.glm.1<-glmmTMB(Freq~lockdown.phase+repeats+ar1(date.f+0|group),data=subset(tweet.vol.lon,repeats!="4"),nbinom2)
vol.glm.2<-glmmTMB(Freq~lockdown.phase+ar1(date.f+0|group),data=subset(tweet.vol.lon,repeats!="4"),nbinom2)
AIC(vol.glm.00,vol.glm.0,vol.glm.1,vol.glm.2)

summary(vol.glm.0)
summary(vol.glm.1)

plot(ggpredict(vol.glm.0,c("repeats","lockdown.phase")))

theme_set(theme_classic())
gg.tweet.count.lon<-plot(ggpredict(vol.glm.0,c("repeats","lockdown.phase")),use.theme=FALSE)+
labs(
x="Mobility restriction wave",
y="Predicted daily tweet numbers about Parks in London",
title="",
color="mobility restriction"
) +
theme(legend.position=c(.7,.8))



# > summary(vol.glm.0)
 # Family: nbinom2  ( log )
# Formula:          Freq ~ lockdown.phase * repeats + ar1(date.f + 0 | group)
# Data: subset(tweet.vol.lon, repeats != "4")

     # AIC      BIC   logLik deviance df.resid 
  # 4553.3   4602.3  -2264.7   4529.3      425 

# Random effects:

# Conditional model:
 # Groups Name          Variance Std.Dev. Corr      
 # group  date.f(18262) 0.07303  0.2702   0.48 (ar1)
# Number of obs: 437, groups:  group, 1

# Dispersion parameter for nbinom2 family (): 1.15e+04 

# Conditional model:
                         # Estimate Std. Error z value Pr(>|z|)    
# (Intercept)               4.57946    0.05184   88.34  < 2e-16 ***
# lockdown.phase1           1.13476    0.09252   12.27  < 2e-16 ***
# lockdown.phase2           0.87909    0.08134   10.81  < 2e-16 ***
# repeats2                  0.88880    0.12200    7.29 3.22e-13 ***
# repeats3                  0.68634    0.07420    9.25  < 2e-16 ***
# lockdown.phase1:repeats2 -1.16309    0.16382   -7.10 1.25e-12 ***
# lockdown.phase2:repeats2 -1.13712    0.16099   -7.06 1.63e-12 ***
# lockdown.phase1:repeats3 -1.19420    0.13780   -8.67  < 2e-16 ***
# lockdown.phase2:repeats3 -1.13282    0.10757  -10.53  < 2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# > 



#########################
#paris
table(tweet.vol.par$lockdown.phase,tweet.vol.par$repeats)
vol.glm.00<-glmmTMB(Freq~1+ar1(date.f+0|group),data=subset(tweet.vol.par,lockdown.phase!="1"&repeats!="3"),nbinom2)
vol.glm.0<-glmmTMB(Freq~lockdown.phase*repeats+ar1(date.f+0|group),data=subset(tweet.vol.par,lockdown.phase!="1"&repeats!="3"),nbinom2)
vol.glm.1<-glmmTMB(Freq~lockdown.phase+repeats+ar1(date.f+0|group),data=subset(tweet.vol.par,lockdown.phase!="1"&repeats!="3"),nbinom2)
vol.glm.2<-glmmTMB(Freq~lockdown.phase+ar1(date.f+0|group),data=subset(tweet.vol.par,lockdown.phase!="1"&repeats!="3"),nbinom2)


theme_set(theme_classic())
gg.tweet.count.par<-plot(ggpredict(vol.glm.0,c("repeats","lockdown.phase")),use.theme=FALSE)+
labs(
x="Mobility restriction wave",
y="Predicted daily tweet numbers about Parks in Paris",
title="",
color="mobility restriction"
) +
theme(legend.position=c(.6,.8))


summary(vol.glm.0)

vol.glm.2b<-glmmTMB(Freq~lockdown.phase+ar1(date.f+0|group),data=tweet.vol.par,nbinom2)
##there is no support for a change in volume with lockdown phase

plot(ggpredict(vol.glm.2b,c("lockdown.phase")))


vol.glm.00<-glmmTMB(Freq~1+ar1(date.f+0|group),data=subset(tweet.vol.par,repeats=="1"),nbinom2)
vol.glm.2<-glmmTMB(Freq~lockdown.phase+ar1(date.f+0|group),data=subset(tweet.vol.par,repeats=="1"),nbinom2)
AIC(vol.glm.0,vol.glm.1,vol.glm.2,vol.glm.3,vol.glm.00)

plot(ggpredict(vol.glm.2,c("lockdown.phase")))

#########################
#berlin
table(tweet.vol.ber$lockdown.phase,tweet.vol.ber$repeats)
vol.glm.2b<-glmmTMB(Freq~lockdown.phase+ar1(date.f+0|group),data=tweet.vol.ber,nbinom2)
vol.glm.2c<-glmmTMB(Freq~lockdown.phase+ar1(date.f+0|group),data=subset(tweet.vol.ber,repeats=="1"),nbinom2)


vol.glm.00<-glmmTMB(Freq~1+ar1(date.f+0|group),data=subset(tweet.vol.ber,repeats!="3"&repeats!="4"),nbinom2)
vol.glm.0<-glmmTMB(Freq~lockdown.phase*repeats+ar1(date.f+0|group),data=subset(tweet.vol.ber,repeats!="3"&repeats!="4"),nbinom2)
vol.glm.1<-glmmTMB(Freq~lockdown.phase+repeats+ar1(date.f+0|group),data=subset(tweet.vol.ber,repeats!="3"&repeats!="4"),nbinom2)
vol.glm.2<-glmmTMB(Freq~lockdown.phase+ar1(date.f+0|group),data=subset(tweet.vol.ber,repeats!="3"&repeats!="4"),nbinom2)
AIC(vol.glm.0,vol.glm.1,vol.glm.2,vol.glm.00)

plot(ggpredict(vol.glm.0,c("repeats","lockdown.phase")))


theme_set(theme_classic())
gg.tweet.count.ber<-plot(ggpredict(vol.glm.0,c("repeats","lockdown.phase")),use.theme=FALSE)+
labs(
x="Mobility restriction wave",
y="Predicted daily tweet numbers about Parks in Berlin",
title="",
color="mobility restriction"
) +
theme(legend.position=c(.6,.8))


#
vol.glm.00<-glmmTMB(Freq~1+ar1(date.f+0|group),data=subset(tweet.vol.ber,lockdown.phase!="0"),nbinom2)
vol.glm.0<-glmmTMB(Freq~lockdown.phase*repeats+ar1(date.f+0|group),data=subset(tweet.vol.ber,lockdown.phase!="0"),nbinom2)
vol.glm.1<-glmmTMB(Freq~lockdown.phase+repeats+ar1(date.f+0|group),data=subset(tweet.vol.ber,lockdown.phase!="0"),nbinom2)
vol.glm.2<-glmmTMB(Freq~lockdown.phase+ar1(date.f+0|group),data=subset(tweet.vol.ber,lockdown.phase!="0"),nbinom2)
AIC(vol.glm.0,vol.glm.1,vol.glm.2,vol.glm.00)

##per phase
vol.glm.00<-glmmTMB(Freq~1+ar1(date.f+0|group),data=subset(tweet.vol.ber,lockdown.phase=="0"),nbinom2)
vol.glm.0<-glmmTMB(Freq~repeats+ar1(date.f+0|group),data=subset(tweet.vol.ber,lockdown.phase=="0"),nbinom2)
AIC(vol.glm.0,vol.glm.00)


vol.glm.00<-glmmTMB(Freq~1+ar1(date.f+0|group),data=subset(tweet.vol.ber,lockdown.phase=="2"),nbinom2)
vol.glm.0<-glmmTMB(Freq~repeats+ar1(date.f+0|group),data=subset(tweet.vol.ber,lockdown.phase=="2"),nbinom2)
AIC(vol.glm.0,vol.glm.00)


#################################

library(gridExtra)
library(grid)

#####################MANUSCRIPT FIGURE GOOGLE SEARCH################################################
tiff(file="C:/Users/David/OneDrive - Danmarks Tekniske Universitet/Facebook Greenspace ms/Figure twitter volume.tiff",width=42,height=16,res=200,units="cm")
grid.arrange(textGrob("(a)",hjust=10),textGrob("(b)",hjust=10),textGrob("(c)",hjust=10),gg.tweet.count.lon,gg.tweet.count.par,gg.tweet.count.ber,nrow=2,ncol=3,heights=c(.6,10),widths=c(7,7,7))
dev.off()

#######################################################################################################
#######################################################################################################
#######################################################################################################
### do tweets get more likes during a particular phase?

london.like<-london.mat$meta
london.like$topic<-apply(london.stm$theta,1,which.max)

lon.lik.glm0<-glmmTMB(like~1+(1|topic),data=london.like,nbinom2)
lon.lik.glm<-glmmTMB(like~lockdown+(1|topic),data=london.like,nbinom2)
AIC(lon.lik.glm0,lon.lik.glm)

#######################################################################################################
paris.like<-paris.mat$meta
paris.like$topic<-apply(paris.stm$theta,1,which.max)

par.lik.glm0<-glmmTMB(like~1+(1|topic),data=paris.like,nbinom2)
par.lik.glm<-glmmTMB(like~lockdown+(1|topic),data=paris.like,nbinom2)
AIC(par.lik.glm0,par.lik.glm)

#######################################################################################################
berlin.like<-berlin.mat$meta
berlin.like$topic<-apply(berlin.stm$theta,1,which.max)

ber.lik.glm0<-glmmTMB(like~1+(1|topic),data=berlin.like,nbinom2)
ber.lik.glm<-glmmTMB(like~lockdown+(1|topic),data=berlin.like,nbinom2)
anova(ber.lik.glm)

AIC(ber.lik.glm0,ber.lik.glm,ber.lik.glm1)




#######################################################################################################
#######################################################################################################
#######################################################################################################


theme_set(theme_classic())
gg.tweet.like.ber<-plot(ggpredict(ber.lik.glm,c("lockdown")),use.theme=FALSE)+
labs(
x="Mobility restriction",
y="Predicted average likes per tweet",
title=""
) 


theme_set(theme_classic())
gg.tweet.like.par<-plot(ggpredict(par.lik.glm,c("lockdown")),use.theme=FALSE)+
labs(
x="Mobility restriction",
y="Predicted average likes per tweet",
title=""
) 


theme_set(theme_classic())
gg.tweet.like.lon<-plot(ggpredict(lon.lik.glm,c("lockdown")),use.theme=FALSE)+
labs(
x="Mobility restriction",
y="Predicted average likes per tweet",
title=""
) 


#####################MANUSCRIPT FIGURE like twitter################################################
tiff(file="C:/Users/David/OneDrive - Danmarks Tekniske Universitet/Facebook Greenspace ms/Figure twitter likes.tiff",width=42,height=16,res=200,units="cm")
grid.arrange(textGrob("(a)",hjust=10),textGrob("(b)",hjust=10),textGrob("(c)",hjust=10),gg.tweet.like.lon,gg.tweet.like.par,gg.tweet.like.ber,nrow=2,ncol=3,heights=c(.6,10),widths=c(7,7,7))
dev.off()

# > summary(lon.lik.glm)
 # Family: nbinom2  ( log )
# Formula:          like ~ lockdown + (1 | topic)
# Data: london.like

      # AIC       BIC    logLik  deviance  df.resid 
 # 388653.6  388700.9 -194321.8  388643.6     94387 

# Random effects:

# Conditional model:
 # Groups Name        Variance Std.Dev.
 # topic  (Intercept) 0.8948   0.9459  
# Number of obs: 94392, groups:  topic, 56

# Dispersion parameter for nbinom2 family (): 0.159 

# Conditional model:
            # Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  1.66741    0.12758  13.069  < 2e-16 ***
# lockdown1   -0.35647    0.02195 -16.236  < 2e-16 ***
# lockdown2    0.11640    0.02028   5.738 9.57e-09 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# > 

# > summary(ber.lik.glm)
 # Family: nbinom2  ( log )
# Formula:          like ~ lockdown + (1 | topic)
# Data: berlin.like

     # AIC      BIC   logLik deviance df.resid 
  # 7293.4   7320.1  -3641.7   7283.4     1543 

# Random effects:

# Conditional model:
 # Groups Name        Variance Std.Dev.
 # topic  (Intercept) 1.947    1.396   
# Number of obs: 1548, groups:  topic, 23

# Dispersion parameter for nbinom2 family (): 0.206 

# Conditional model:
            # Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  1.52403    0.33305   4.576 4.74e-06 ***
# lockdown1   -0.04117    0.19039  -0.216   0.8288    
# lockdown2    0.41734    0.16515   2.527   0.0115 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# > 


# > summary(par.lik.glm)
 # Family: nbinom2  ( log )
# Formula:          like ~ lockdown + (1 | topic)
# Data: paris.like

     # AIC      BIC   logLik deviance df.resid 
 # 11492.0  11521.9  -5741.0  11482.0     2923 

# Random effects:

# Conditional model:
 # Groups Name        Variance Std.Dev.
 # topic  (Intercept) 1.384    1.177   
# Number of obs: 2928, groups:  topic, 36

# Dispersion parameter for nbinom2 family (): 0.131 

# Conditional model:
            # Estimate Std. Error z value Pr(>|z|)    
# (Intercept)   1.4451     0.2350   6.150 7.75e-10 ***
# lockdown1     0.4340     0.2128   2.040   0.0413 *  
# lockdown2     0.6557     0.1220   5.376 7.63e-08 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# > 



