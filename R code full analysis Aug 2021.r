#######################################################################################
##### R code covid interventions and greenspace use
######################################################################################

#OxGRT codebook is at: https://github.com/OxCGRT/covid-policy-tracker/blob/master/documentation/codebook.md

#we are after C6 - stay at home
lockdown<-read.csv("C:/Users/David/Documents/Facebook COVID/lockdown_indicators/OxCGRT_latest_Aug_30_2021.txt",header=T)

#C6
# 0 - no measures
# 1 - recommend not leaving house
# 2 - require not leaving house with exceptions for daily exercise, grocery shopping, and 'essential' trips
# 3 - require not leaving house with minimal exceptions (eg allowed to leave once a week, or only one person can leave at a time, etc)
# Blank - no data

lockdown.cb<-read.csv("C:/Users/David/Documents/Facebook COVID/lockdown_indicators/OxCGRT_latest_combined_Aug_30_2021.txt",header=T)

hist(lockdown.cb$C6_combined_numeric)
hist(lockdown$C6_Stay_at_home_requirements)

table(lockdown$Jurisdiction[lockdown$CountryCode=="GBR"])

table(lockdown$RegionName[lockdown$CountryCode=="GBR"])

lockdown[lockdown$RegionName=="England",]$C6_Stay_at_home_requirements
lockdown[lockdown$RegionName=="Scotland",]$C6_Stay_at_home_requirements
lockdown[lockdown$RegionName=="Scotland",]$Date

lockdown$date.pos<-as.Date(as.character(lockdown$Date),"%Y%m%d")

lockdown[lockdown$RegionName=="England",c(17,52)]

#plot(lockdown[lockdown$RegionName=="England",]$C6_Stay_at_home_requirements,lockdown[lockdown$RegionName=="Scotland",]$C6_Stay_at_home_requirements,type='l')

plot(lockdown[lockdown$RegionName=="England",]$C6_Stay_at_home_requirements~lockdown[lockdown$RegionName=="England",]$date.pos,type='l')

table(lockdown$RegionName[lockdown$CountryCode=="USA"])


#######################################################################################
# ok so we have a preliminary model fit which shows that during the first lockdown the use of greenspace increased in London but that increase was dependent on 
#week day, time of the day and the level of deprivation of the tile

#####################################################################################
### we then have twitter data showing that covid associated tweet were more positive when talking about nature

# we then need to replicate to other cities and multiple lockdowns
# we then need to sample some of those greenspaces and look at the conversation about them during lockdowns and outside lockdowns

#we need to find cities where BOTH greenspace AND deprivation index data exist. That's hard. 
#CONCLUSION #1: nations/cities need to collect greenspace and deprivation index data!


################################################################################################################################
#let's replicate for London

###first let's stich let's pay attention to the difference in file size after API transition - we might have extra columns or extra tiles to deal with

lockdown.london<-lockdown[lockdown$RegionName=="England",c(17,52)]
library(sf)
deprivation_folder<-"C:/Users/David/Documents/deprivation_indices"
greenspace_folder<-"C:/Users/David/Documents/greenspace"
lon.pop.files<-list.files("C:/Users/David/Documents/Facebook COVID/Facebook London",full.names=TRUE)

UKgreenspace<-st_read(paste0(greenspace_folder,"/GB-greenspace/data/GB_GreenspaceSite.shp"))


lon.pop<-read.csv(lon.pop.files[1],header=T)

#let's make a bbox of the city to retain only greenspaces that will be there
london.bbox<-st_as_sf(lon.pop,coords=c("lon","lat"),crs=4326)
#london.bbox<-st_as_sf(read.csv(lon.pop.files[1],header=T),coords=c("lon","lat"),crs=4326)

london.bbox.uk<-st_transform(london.bbox,27700)  #27700
london.bbox.uk.buf<-st_buffer(london.bbox.uk,1000)
obj<-st_as_sfc(st_bbox(london.bbox.uk.buf))

rm(london.bbox,london.bbox.uk,london.bbox.uk.buf)

###########
UKgreenspace<-st_transform(UKgreenspace,4326)
 lsoa.simp<-st_combine(LSOA)
 lsoa.simp<-st_transform(lsoa.simp,4326)
london.gardens<-st_intersection(UKgreenspace,lsoa.simp)

london.gardens<-st_intersection(UKgreenspace,obj)
rm(UKgreenspace)

###########
earth_circumference_m<-6378137*2*pi              
earth_circumference<-360 
##################


lon.pop$tile_size<-(cos(lon.pop$lat * pi/180) * earth_circumference)/(2^(nchar(as.character(lon.pop$quadkey))))
lon.pop$lon.top<-lon.pop$lon+(lon.pop$tile_size/2)
lon.pop$lon.bot<-lon.pop$lon-(lon.pop$tile_size/2)
lon.pop$lat.top<-lon.pop$lat+(lon.pop$tile_size/2)
lon.pop$lat.bot<-lon.pop$lat-(lon.pop$tile_size/2)

lon.pop$gardens<-0
lon.pop$coverage<-0


gardens.quadkey<-list()
gardens.quadkey.df<-data.frame(quadkey=0,gardens=0,coverage=0)


##############################################
#### inner file loop
#############################################
for (i in 1:dim(lon.pop)[1]) {

try.sf<-st_sfc(st_polygon(list(rbind(c(lon.pop$lon.top[i],lon.pop$lat.top[i]),c(lon.pop$lon.bot[i],lon.pop$lat.top[i]),c(lon.pop$lon.bot[i],lon.pop$lat.bot[i]),c(lon.pop$lon.top[i],lon.pop$lat.bot[i]),c(lon.pop$lon.top[i],lon.pop$lat.top[i])))))
st_crs(try.sf)<-4326
try.sf.uk<-st_transform(try.sf,27700)  #27700
try.inter<-st_intersection(london.gardens,try.sf.uk)

if (length(try.inter$id)>0) {
lon.pop$gardens[i]<-length(try.inter$id)
lon.pop$coverage[i]<-as.numeric(sum(st_area(try.inter)))
gardens.quadkey[[paste(lon.pop$quadkey[i])]]<-data.frame(name=as.array(as.character(try.inter$id)),area=as.numeric(st_area(try.inter))) #let's keep area here too for when we subset some greenspace categories away
gardens.quadkey.df<-rbind(gardens.quadkey.df,data.frame(quadkey=lon.pop$quadkey[i],gardens=length(try.inter$id),coverage=as.numeric(sum(st_area(try.inter)))))

#print(i)
#flush.console()

}  else {

gardens.quadkey.df<-rbind(gardens.quadkey.df,data.frame(quadkey=lon.pop$quadkey[i],gardens=0,coverage=0))

}
}
lon.pop$coverage_area<-lon.pop$coverage/(lon.pop$tile_size*earth_circumference_m/360)^2

gardens.quadkey.df<-gardens.quadkey.df[-1,]

#######################################################
########inter-file loop starts here

for (j in 1369:length(lon.pop.files)) {


lon.pop.it<-read.csv(lon.pop.files[j],header=T)

lon.pop.it$tile_size<-(cos(lon.pop.it$lat * pi/180) * earth_circumference)/(2^(nchar(as.character(lon.pop.it$quadkey))))
lon.pop.it$lon.top<-lon.pop.it$lon+(lon.pop.it$tile_size/2)
lon.pop.it$lon.bot<-lon.pop.it$lon-(lon.pop.it$tile_size/2)
lon.pop.it$lat.top<-lon.pop.it$lat+(lon.pop.it$tile_size/2)
lon.pop.it$lat.bot<-lon.pop.it$lat-(lon.pop.it$tile_size/2)

lon.pop.it$gardens<-0
lon.pop.it$coverage<-0


for (i in 1:dim(lon.pop.it)[1]) {  #I know it does not look it, but practically it is more time efficient

#######
##### let's first compare the quadkeys to the preexisting quadkeys
existing.quadkey<-which(lon.pop.it$quadkey[i]==gardens.quadkey.df$quadkey,arr.ind=TRUE)
if (length(existing.quadkey)>0) {

lon.pop.it$gardens[i]<-gardens.quadkey.df$gardens[existing.quadkey]
lon.pop.it$coverage[i]<-gardens.quadkey.df$coverage[existing.quadkey]

} else {

try.sf<-st_sfc(st_polygon(list(rbind(c(lon.pop.it$lon.top[i],lon.pop.it$lat.top[i]),c(lon.pop.it$lon.bot[i],lon.pop.it$lat.top[i]),c(lon.pop.it$lon.bot[i],lon.pop.it$lat.bot[i]),c(lon.pop.it$lon.top[i],lon.pop.it$lat.bot[i]),c(lon.pop.it$lon.top[i],lon.pop.it$lat.top[i])))))
st_crs(try.sf)<-4326
try.sf.uk<-st_transform(try.sf,27700)  #27700
try.inter<-st_intersection(london.gardens,try.sf.uk)

ggplot()+geom_sf(data=Parisgreenspace,aes(fill=categorie))+geom_sf(data=try.sf)

if (length(try.inter$id)>0) {
lon.pop.it$gardens[i]<-length(try.inter$id)
lon.pop.it$coverage[i]<-as.numeric(sum(st_area(try.inter)))
gardens.quadkey[[paste(lon.pop.it$quadkey[i])]]<-data.frame(name=as.array(as.character(try.inter$id)),area=as.numeric(st_area(try.inter))) #let's keep area here too for when we subset some greenspace categories away
gardens.quadkey.df<-rbind(gardens.quadkey.df,data.frame(quadkey=lon.pop.it$quadkey[i],gardens=length(try.inter$id),coverage=as.numeric(sum(st_area(try.inter)))))

}  else {

gardens.quadkey.df<-rbind(gardens.quadkey.df,data.frame(quadkey=lon.pop.it$quadkey[i],gardens=0,coverage=0))

} #ifelse intersect

} #ifelse preexisting

} #iteration

lon.pop.it$coverage_area<-lon.pop.it$coverage/(lon.pop.it$tile_size*earth_circumference_m/360)^2

lon.pop<-rbind(lon.pop,lon.pop.it)

print(j)
flush.console()

}


save(lockdown.london,london.gardens,lon.pop,gardens.quadkey.df,gardens.quadkey,file="C:/Users/David/Documents/Facebook COVID/london_corona_greenspace_Aug_2021.Rdata")


lon.pop$dated<-factor(substr(lon.pop$date_time,1,10))
lon.pop$time<-factor(substr(lon.pop$date_time,12,15))

lon.pop$dates<-as.POSIXct(lon.pop$dated, format="%Y-%m-%d")

save(lockdown.london,london.gardens,lon.pop,gardens.quadkey.df,gardens.quadkey,file="C:/Users/David/Documents/Facebook COVID/london_corona_greenspace_Aug_2021.Rdata")


##we need to QC what's going on here. for this first pass we just redress (0.1% quadkeys)
lon.pop$coverage_area[lon.pop$coverage_area>1]<-1

save(lockdown.london,london.gardens,lon.pop,gardens.quadkey.df,gardens.quadkey,file="C:/Users/David/Documents/Facebook COVID/london_corona_greenspace_Aug_2021.Rdata")




lon.pop$nature.deprivation<-"poor"
lon.pop$nature.deprivation[lon.pop$coverage_area>.66]<-"rich"
lon.pop$nature.deprivation[lon.pop$coverage_area<.25&lon.pop$coverage_area>.05]<-"jewels"
lon.pop$nature.deprivation[lon.pop$coverage_area>=.25&lon.pop$coverage_area<=.66]<-"middle"
lon.pop$nature.deprivation<-factor(lon.pop$nature.deprivation)

times<-unique(lon.pop$time)
lon.pop$time[lon.pop$time==times[1]]<-times[4]  #dealing with date time format change
lon.pop$time[lon.pop$time==times[2]]<-times[5]  #dealing with date time format change
lon.pop$time[lon.pop$time==times[3]]<-times[6]  #dealing with date time format change
lon.pop$time<-factor(lon.pop$time)
lon.pop$dates<-as.Date(lon.pop$dates)
save(lockdown.london,london.gardens,lon.pop,gardens.quadkey.df,gardens.quadkey,file="C:/Users/David/Documents/Facebook COVID/london_corona_greenspace_Aug_2021.Rdata")

lon.pop$lockdown<-0
lon.pop$lockdown<-lockdown.london$C6_Stay_at_home_requirements[match(lon.pop$dates,lockdown.london$date.pos)] #much faster than merge
lon.pop$lockdown<-factor(lon.pop$lockdown)

save(lon.pop,file="C:/Users/David/Documents/Facebook COVID/london_corona_FB_Aug_2021.Rdata")

#######################################################################################
library(sf)
load("C:/Users/David/Documents/Facebook COVID/london_corona_FB_Aug_2021.Rdata")

LSOA<-st_read("C:/Users/David/Documents/deprivation_indices/london/statistical-gis-boundaries-london/ESRI/LSOA_2011_London_gen_MHW.shp")
london.imd.2019<-read.csv("C:/Users/David/Documents/deprivation_indices/london/London.IMD.2019.csv",header=TRUE)

LSOA$IMD<-london.imd.2019$IMD[match(LSOA$LSOA11CD,london.imd.2019$LSOA.code)]

plot(LSOA["IMD"])

lon.pop$IMD<-NA

quadkey.imd<-data.frame(quadkey=unique(lon.pop$quadkey),IMD=NA)

iter<-match(quadkey.imd$quadkey,lon.pop$quadkey)

m<-1
for (i in iter) {

try.sf<-st_sfc(st_polygon(list(rbind(c(lon.pop$lon.top[i],lon.pop$lat.top[i]),c(lon.pop$lon.bot[i],lon.pop$lat.top[i]),c(lon.pop$lon.bot[i],lon.pop$lat.bot[i]),c(lon.pop$lon.top[i],lon.pop$lat.bot[i]),c(lon.pop$lon.top[i],lon.pop$lat.top[i])))))
st_crs(try.sf)<-4326
try.sf.uk<-st_transform(try.sf,st_crs(LSOA)) #UK grid
try.inter<-st_intersection(LSOA,try.sf.uk)

if (length(try.inter$LSOA11CD)>0) {
	quadkey.imd$IMD[m]<-median(try.inter$IMD) #LSOA$IMD does not contain any NAs so if the intersection length >0 we are safe to take the median without removing NAs
}


print(i)
flush.console()
m<-m+1

} 

quadkey.imd$lat<-lon.pop$lat[iter]
quadkey.imd$lon<-lon.pop$lon[iter]
#quadkey.imd.map<-st_as_sf(quadkey.imd,coords=c("lon","lat"),crs=4326)

save(quadkey.imd,file="C:/Users/David/Documents/deprivation_indices/london/london_corona_IMD_quadkey.Rdata")

min(quadkey.imd$lat)
min(quadkey.imd$lon)
max(quadkey.imd$lat)
max(quadkey.imd$lon)

min(quadkey.imd$lat[!is.na(quadkey.imd$IMD)])
min(quadkey.imd$lon[!is.na(quadkey.imd$IMD)])
max(quadkey.imd$lat[!is.na(quadkey.imd$IMD)])
max(quadkey.imd$lon[!is.na(quadkey.imd$IMD)])

#that's essentially inside the M25

##### the FB mask for London is way larger than London proper, hence lots of NAs

##### the FB mask for London is way larger than London proper, hence lots of NAs


lon.pop$IMD<-quadkey.imd$IMD[match(lon.pop$quadkey,quadkey.imd$quadkey)]

table(lon.pop$nature.deprivation[!is.na(lon.pop$IMD)])
hist(lon.pop$coverage_area[!is.na(lon.pop$IMD)])
#we still have full coverage of the greenspace coverage area range in the London 'proper' set which also provides a more 'homogeneous' urban life
# parameter landscape than accounting for the suburbs
#also when we account for the suburbs, suddenly some 'green space' may not be accounted as greenspaces because of their ownership status
#so we keep as is (that is the London city limits defined for IMD)

save(lon.pop,file="C:/Users/David/Documents/Facebook COVID/london_corona_FB_Aug_2021.Rdata")

lon.pop.M25<-lon.pop[!is.na(lon.pop$IMD),]

save(lon.pop.M25,file="C:/Users/David/Documents/Facebook COVID/london_corona_FB_M25_Aug_2021.Rdata")

###############################################################################################
### let the fun begin
###############################################################################################


load("C:/Users/David/Documents/Facebook COVID/london_corona_FB_M25_Aug_2021.Rdata")

###let's add weather
#weather data was downloaded from meteostat the station ID used is in the name of the file
#total_prcp is prcp + snow

lon.weather<-read.csv("C:/Users/David/Documents/Facebook COVID/weather/meteostat_London_weather_covid_GB-QK9E.csv",sep=";",header=TRUE)
lon.weather$date<-as.Date(lon.weather$date,"%d-%m-%Y")

lon.pop.M25$temp<-lon.weather$tavg[match(lon.pop.M25$dates,lon.weather$date)]
lon.pop.M25$prcp<-lon.weather$total_prcp[match(lon.pop.M25$dates,lon.weather$date)]


lon.pop.M25$weekday<-weekdays(lon.pop.M25$dates)
lon.pop.M25$weekend<-"no"
lon.pop.M25$weekend[lon.pop.M25$weekday=="Saturday"]<-"yes"
lon.pop.M25$weekend[lon.pop.M25$weekday=="Sunday"]<-"yes"
lon.pop.M25$weekend<-factor(lon.pop.M25$weekend)
lon.pop.M25$weekday<-factor(lon.pop.M25$weekday)

save(lon.pop.M25,file="C:/Users/David/Documents/Facebook COVID/london_corona_FB_M25_Aug_2021.Rdata")

#a wee bit of exploration

hist(lon.pop.M25$clipped_z_score)
hist(lon.pop.M25$temp)

par(mfrow=c(2,1))
hist(lon.pop.M25$clipped_z_score[lon.pop.M25$temp>15])
hist(lon.pop.M25$clipped_z_score[lon.pop.M25$temp<11])

#well this is interesting, temp not really associated to z score

par(mfrow=c(3,1))
hist(lon.pop.M25$clipped_z_score[lon.pop.M25$lockdown==0])
hist(lon.pop.M25$clipped_z_score[lon.pop.M25$lockdown==1])
hist(lon.pop.M25$clipped_z_score[lon.pop.M25$lockdown==2])

#obvious effect on the tails

par(mfrow=c(2,1))
hist(lon.pop.M25$clipped_z_score[lon.pop.M25$weekend=="yes"])
hist(lon.pop.M25$clipped_z_score[lon.pop.M25$weekend=="no"])


par(mfrow=c(3,1))
hist(lon.pop.M25$clipped_z_score[lon.pop.M25$time=="0000"])
hist(lon.pop.M25$clipped_z_score[lon.pop.M25$time=="0800"])
hist(lon.pop.M25$clipped_z_score[lon.pop.M25$time=="1600"])

lon.pop.M25$quadkeys<-factor(lon.pop.M25$quadkey)

save(lon.pop.M25,file="C:/Users/David/Documents/Facebook COVID/london_corona_FB_M25_Aug_2021.Rdata")


######################################################################################################################################################
load("C:/Users/David/Documents/Facebook COVID/london_corona_FB_M25_Aug_2021.Rdata")
lon.pop.M25<-lon.pop.M25[,c(1,2,12,19:29,31,32)]
lon.pop.M25$lockdown[is.na(lon.pop.M25$lockdown)]<-0 #the NA for unreported last 2-3 days when downloaded which were confirmed as 0s on the ordinal scale since

library(lme4)
library(splines)
library(glmmTMB)

nct<-parallel::detectCores()/2 #let's try the parallel optimisation

#so of course the limitation becomes memory, as we push the data.frame to multiple virtual sessions essentially. 
#given the temporary memory burden of data + model fit is about 25Gb, on this machine we are really parallelising over 2 threads at once at most


# n = 17,586,960 observations
# lon.lme10<-glmmTMB(clipped_z_score~ns(IMD,df=4)*time*weekend*ns(coverage_area,df=4)+ (1|dated) + (1|quadkeys),data=lon.pop.M25)
# #a big no no for memory use, let see whether we really need this level of details in the effect (aka asymmetric non-linearity)

# lon.lme10b<-glmmTMB(clipped_z_score~IMD*time*weekend*coverage_area+ (1|dated) + (1|quadkeys),data=lon.pop.M25)
# lon.lme11<-glmmTMB(clipped_z_score~lockdown*IMD*time*weekend*coverage_area+ (1|dated) + (1|quadkeys),data=lon.pop.M25)

tic<-Sys.time()
#lon.lme12<-glmmTMB(clipped_z_score~lockdown*IMD*time*coverage_area+ (1|dated) + (1|quadkeys),data=lon.pop.M25,control=glmmTMBControl(parallel=2,optCtrl=list(iter.max=600,eval.max=600),optimizer=optim,optArgs=list(method="BFGS")))
lon.lme12<-glmmTMB(clipped_z_score~lockdown*IMD*time*coverage_area+ (1|dated) + (1|quadkeys),data=lon.pop.M25,control=glmmTMBControl(optCtrl=list(iter.max=1000,eval.max=1000)))
toc<-Sys.time()
toc-tic

#AIC 57895405
#false convergence. is it a flase positive? let's try another optimiser, if we get false converence as well, it should be false positive
lon.lme12b<-glmmTMB(clipped_z_score~lockdown*IMD*time*coverage_area+ (1|dated) + (1|quadkeys),data=lon.pop.M25,control=glmmTMBControl(optCtrl=list(iter.max=1000,eval.max=1000),optimizer=optim,optArgs=list(method="BFGS")))

lon.lme12c<-glmmTMB(clipped_z_score~lockdown*IMD*coverage_area+ (1|dated) + (1|quadkeys) + (1|time),data=lon.pop.M25)

tic<-Sys.time()
lon.lme12d<-glmmTMB(clipped_z_score~lockdown*IMD*coverage_area*weekend+ (1|dated) + (1|quadkeys) + (1|time),data=lon.pop.M25)
toc<-Sys.time()
toc-tic

tic<-Sys.time()
#lon.lme12e<-glmmTMB(clipped_z_score~lockdown*time*weekend*ns(IMD,df=2)*ns(coverage_area,df=2)+ (1|dated) + (1|quadkeys),data=lon.pop.M25,control=glmmTMBControl(optCtrl=list(iter.max=700,eval.max=700)))
lon.lme12e<-lmer(clipped_z_score~lockdown*time*weekend*IMD*coverage_area+ (1|dated) + (1|quadkeys),data=lon.pop.M25,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
#that's the ticket
#AIC 57849520

toc<-Sys.time()
toc-tic

library(effects)
png(file="C:/Users/David/Documents/Facebook COVID/London_coverageimd_2_wkend_0800.png", width=60,height=42,units="cm",res=200)
plot(Effect("coverage_area:IMD",lon.lme12e, fixed.predictors=list(given.values=c(lockdown2=1,weekendyes=1,time0800=1))))
#plot(allEffects(lon.lme10,xlevels=list(IMD=c(8.6, 15.9,24,44))),main="") #10-20, 40, 60, 80-100
dev.off()

library(ggplot2)
library(ggeffects)

wkend0800_2<-ggpredict(lon.lme12e,terms=c("coverage_area","IMD [8.6, 15.9,24,44]"),condition=c(lockdown="2",weekend="yes",time="0800"))

png(file="C:/Users/David/Documents/Facebook COVID/London_coverageimd_2_wkend_0800.png", width=60,height=42,units="cm",res=200)
plot(wkend0800_2)
#plot(allEffects(lon.lme10,xlevels=list(IMD=c(8.6, 15.9,24,44))),main="") #10-20, 40, 60, 80-100
dev.off()

rm(wkend0800_2)
gc()
wk0800_2<-ggpredict(lon.lme12e,terms=c("coverage_area","IMD [8.6, 15.9,24,44]"),condition=c(lockdown="2",weekend="no",time="0800"))

png(file="C:/Users/David/Documents/Facebook COVID/London_coverageimd_2_wk_0800.png", width=60,height=42,units="cm",res=200)
plot(wk0800_2)
#plot(allEffects(lon.lme10,xlevels=list(IMD=c(8.6, 15.9,24,44))),main="") #10-20, 40, 60, 80-100
dev.off()


rm(wk0800_2)
gc()
wk0800_1<-ggpredict(lon.lme12e,terms=c("coverage_area","IMD [8.6, 15.9,24,44]"),condition=c(lockdown="1",weekend="no",time="0800"))

png(file="C:/Users/David/Documents/Facebook COVID/London_coverageimd_1_wk_0800.png", width=60,height=42,units="cm",res=200)
plot(wk0800_1)
#plot(allEffects(lon.lme10,xlevels=list(IMD=c(8.6, 15.9,24,44))),main="") #10-20, 40, 60, 80-100
dev.off()


rm(wk0800_1)
gc()
wkend0800_1<-ggpredict(lon.lme12e,terms=c("coverage_area","IMD [8.6, 15.9,24,44]"),condition=c(lockdown="1",weekend="yes",time="0800"))

png(file="C:/Users/David/Documents/Facebook COVID/London_coverageimd_1_wkend_0800.png", width=60,height=42,units="cm",res=200)
plot(wkend0800_1)
#plot(allEffects(lon.lme10,xlevels=list(IMD=c(8.6, 15.9,24,44))),main="") #10-20, 40, 60, 80-100
dev.off()


rm(wkend0800_1)
gc()
wkend0800_0<-ggpredict(lon.lme12e,terms=c("coverage_area","IMD [8.6, 15.9,24,44]"),condition=c(lockdown="0",weekend="yes",time="0800"))

png(file="C:/Users/David/Documents/Facebook COVID/London_coverageimd_0_wkend_0800.png", width=60,height=42,units="cm",res=200)
plot(wkend0800_0)
#plot(allEffects(lon.lme10,xlevels=list(IMD=c(8.6, 15.9,24,44))),main="") #10-20, 40, 60, 80-100
dev.off()

rm(wkend0800_0)
gc()
wk0800_0<-ggpredict(lon.lme12e,terms=c("coverage_area","IMD [8.6, 15.9,24,44]"),condition=c(lockdown="0",weekend="no",time="0800"))

png(file="C:/Users/David/Documents/Facebook COVID/London_coverageimd_0_wk_0800.png", width=60,height=42,units="cm",res=200)
plot(wk0800_0)
#plot(allEffects(lon.lme10,xlevels=list(IMD=c(8.6, 15.9,24,44))),main="") #10-20, 40, 60, 80-100
dev.off()

#gc() don't foget

save(lon.lme12e,file="C:/Users/David/Documents/Facebook COVID/lon.lme12e.Rdata")

#############################################################################################################################################
###attempting to capture non-linearity
load("C:/Users/David/Documents/Facebook COVID/london_corona_FB_M25_Aug_2021.Rdata")
lon.pop.M25<-lon.pop.M25[,c(1,2,12,19:29,31,32)]
lon.pop.M25$lockdown[is.na(lon.pop.M25$lockdown)]<-0 #the NA for unreported last 2-3 days when downloaded which were confirmed as 0s on the ordinal scale since

library(splines)
library(lme4)

tic<-Sys.time()
#lon.lme12e<-glmmTMB(clipped_z_score~lockdown*time*weekend*ns(IMD,df=2)*ns(coverage_area,df=2)+ (1|dated) + (1|quadkeys),data=lon.pop.M25,control=glmmTMBControl(optCtrl=list(iter.max=700,eval.max=700)))
#lon.lme12ns<-lmer(clipped_z_score~lockdown*time*weekend*IMD*ns(coverage_area,df=2)+ (1|dated) + (1|quadkeys),data=lon.pop.M25,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
#AIC 57779601
#lon.lme12nogreen<-lmer(clipped_z_score~lockdown*time*weekend*IMD+ (1|dated) + (1|quadkeys),data=lon.pop.M25,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
#AIC 57895418
#lon.lme12poly3<-lmer(clipped_z_score~lockdown*time*weekend*IMD*ns(coverage_area,4)+ (1|dated) + (1|quadkeys),data=lon.pop.M25,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))

toc<-Sys.time()
toc-tic


tic<-Sys.time()
lon.lme12e_2<-lmer(clipped_z_score~time*weekend*IMD*ns(coverage_area,df=4)+ (1|dated) + (1|quadkeys),data=subset(lon.pop.M25,lockdown=="2"),control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
toc<-Sys.time()
toc-tic
AIC(lon.lme12e_2)
#save(lon.lme12e_2,file="C:/Users/David/Documents/Facebook COVID/lon.lme12e_2iv.Rdata")
rm(lon.lme12e_2)
gc()

tic<-Sys.time()
lon.lme12e_2<-lmer(clipped_z_score~time*weekend*IMD*ns(coverage_area,df=2)+ (1|dated) + (1|quadkeys),data=subset(lon.pop.M25,lockdown=="2"),control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
toc<-Sys.time()
toc-tic
AIC(lon.lme12e_2)
#save(lon.lme12e_2,file="C:/Users/David/Documents/Facebook COVID/lon.lme12e_2ii.Rdata")
rm(lon.lme12e_2)
gc()


tic<-Sys.time()
lon.lme12e_2<-lmer(clipped_z_score~time*weekend*IMD*ns(coverage_area,df=3)+ (1|dated) + (1|quadkeys),data=subset(lon.pop.M25,lockdown=="2"),control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
toc<-Sys.time()
toc-tic
AIC(lon.lme12e_2)
#save(lon.lme12e_2,file="C:/Users/David/Documents/Facebook COVID/lon.lme12e_2iii.Rdata")
rm(lon.lme12e_2)
gc()

#poly 4 AIC 18425940
#poly 2 AIC 18446733
#poly 3 AIC 18434703
#ns 4   AIC 18409705
#ns 3   AIC 18418808
#ns 2   AIC 18442911

lon.lme.final<-lmer(clipped_z_score~lockdown*time*weekend*IMD*ns(coverage_area,df=2)+ (1|dated) + (1|quadkeys),data=lon.pop.M25,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
AIC(lon.lme.final)
#AIC 57651171
rm(lon.lme.final)
gc()

lon.lme.final<-lmer(clipped_z_score~time*weekend*IMD*ns(coverage_area,df=2)+ (1|dated) + (1|quadkeys),data=lon.pop.M25,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
AIC(lon.lme.final)
#AIC 58414701
rm(lon.lme.final)
gc()

lon.lme.final<-lmer(clipped_z_score~time*weekend*IMD+ (1|dated) + (1|quadkeys),data=lon.pop.M25,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
AIC(lon.lme.final)
#AIC 58474799
rm(lon.lme.final)
gc()

lon.lme.final<-lmer(clipped_z_score~lockdown*time*weekend*IMD+ (1|dated) + (1|quadkeys),data=lon.pop.M25,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
AIC(lon.lme.final)
#AIC 57766347
rm(lon.lme.final)
gc()

lon.lme.final<-lmer(clipped_z_score~lockdown*time*weekend*ns(coverage_area,df=2)+ (1|dated) + (1|quadkeys),data=lon.pop.M25,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
AIC(lon.lme.final)
#AIC 57885758
rm(lon.lme.final)
gc()

lon.lme.final<-lmer(clipped_z_score~time*weekend*ns(coverage_area,df=2)+ (1|dated) + (1|quadkeys),data=lon.pop.M25,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
AIC(lon.lme.final)
#AIC 58577672
rm(lon.lme.final)
gc()


lon.lme.final<-lmer(clipped_z_score~lockdown*time*weekend+ (1|dated) + (1|quadkeys),data=lon.pop.M25,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
AIC(lon.lme.final)
#AIC 57958173
rm(lon.lme.final)
gc()

lon.lme.final<-lmer(clipped_z_score~time*weekend+ (1|dated) + (1|quadkeys),data=lon.pop.M25,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
AIC(lon.lme.final)
#AIC 58615306
rm(lon.lme.final)
gc()

tic<-Sys.time()
lon.lme.final<-lmer(clipped_z_score~lockdown*time*weekend*IMD*ns(coverage_area,df=2)+ (1|dated) + (1|quadkeys),data=lon.pop.M25,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
toc<-Sys.time()
toc-tic

save(lon.lme.final,file="C:/Users/David/Documents/Facebook COVID/London_FB_final_model.Rdata")

load("C:/Users/David/Documents/Facebook COVID/london_corona_FB_M25_Aug_2021.Rdata")
lon.pop.M25<-lon.pop.M25[,c(1,2,12,19:29,31,32)]
lon.pop.M25$lockdown[is.na(lon.pop.M25$lockdown)]<-0 #the NA for unreported last 2-3 days when downloaded which were confirmed as 0s on the ordinal scale since

load("C:/Users/David/Documents/Facebook COVID/London_FB_final_model.Rdata")
library(ggeffects)
library(splines)

london.final.effect<-ggpredict(lon.lme.final,terms=c("coverage_area [all]","IMD [8.6, 15.9,24,44]","lockdown"),condition=c(weekend="yes",time="0800"))
 png(file="C:/Users/David/Documents/Facebook COVID/London_lockdown_main_final.png", width=60,height=42,units="cm",res=200)
 plot(london.final.effect)
 dev.off()



london.final.effect<-ggpredict(lon.lme.final,terms=c("coverage_area [all]","IMD [8.6, 15.9,24,44]","lockdown"),condition=c(weekend="no",time="0800"))
 png(file="C:/Users/David/Documents/Facebook COVID/London_lockdown_main_final_wk_0800.png", width=60,height=42,units="cm",res=200)
 plot(london.final.effect)
 dev.off()

london.final.effect<-ggpredict(lon.lme.final,terms=c("coverage_area [all]","IMD [8.6, 15.9,24,44]","lockdown"),condition=c(weekend="yes",time="0000"))
 png(file="C:/Users/David/Documents/Facebook COVID/London_lockdown_main_final_wkend_0000.png", width=60,height=42,units="cm",res=200)
 plot(london.final.effect)
 dev.off()

gc()

london.final.effect<-ggpredict(lon.lme.final,terms=c("coverage_area [all]","IMD [8.6, 15.9,24,44]","lockdown"),condition=c(weekend="no",time="0000"))
 png(file="C:/Users/David/Documents/Facebook COVID/London_lockdown_main_final_wk_0000.png", width=60,height=42,units="cm",res=200)
 plot(london.final.effect)
 dev.off()

gc()


london.final.effect<-ggpredict(lon.lme.final,terms=c("coverage_area [all]","IMD [8.6, 15.9,24,44]","lockdown"),condition=c(weekend="yes",time="1600"))
 png(file="C:/Users/David/Documents/Facebook COVID/London_lockdown_main_final_wkend_1600.png", width=60,height=42,units="cm",res=200)
 plot(london.final.effect)
 dev.off()

gc()

london.final.effect<-ggpredict(lon.lme.final,terms=c("coverage_area [all]","IMD [8.6, 15.9,24,44]","lockdown"),condition=c(weekend="no",time="1600"))
 png(file="C:/Users/David/Documents/Facebook COVID/London_lockdown_main_final_wk_1600.png", width=60,height=42,units="cm",res=200)
 plot(london.final.effect)
 dev.off()

gc()

london.final.effect<-ggpredict(lon.lme.final,terms=c("coverage_area [all]","IMD [8.6, 15.9,24,44]","lockdown"))
 png(file="C:/Users/David/Documents/Facebook COVID/London_lockdown_main_final_average.png", width=60,height=42,units="cm",res=200)
 plot(london.final.effect)
 dev.off()


lon.lme.final<-lmer(clipped_z_score~lockdown*time*weekend*ns(IMD,df=2)*ns(coverage_area,df=2)+ (1|dated) + (1|quadkeys),data=lon.pop.M25,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
AIC(lon.lme.final)

# lock2.imd<-ggpredict(lon.lme12e_2,terms=c("IMD [all]","coverage_area [0,.25,.5,.75,.9]","weekend","time"))

# png(file="C:/Users/David/Documents/Facebook COVID/London_lockdown2.imd.png", width=60,height=42,units="cm",res=200)
# plot(lock2.imd)
# #plot(allEffects(lon.lme10,xlevels=list(IMD=c(8.6, 15.9,24,44))),main="") #10-20, 40, 60, 80-100
# dev.off()



tic<-Sys.time()
lon.lme12e_1<-lmer(clipped_z_score~time*weekend*IMD*ns(coverage_area,df=4)+ (1|dated) + (1|quadkeys),data=subset(lon.pop.M25,lockdown=="1"),control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
toc<-Sys.time()
toc-tic

save(lon.lme12e_1,file="C:/Users/David/Documents/Facebook COVID/lon.lme12e_1i.Rdata")

rm(lon.lme12e_1)
gc()


tic<-Sys.time()
lon.lme12e_0<-lmer(clipped_z_score~time*weekend*IMD*ns(coverage_area,df=4)+ (1|dated) + (1|quadkeys),data=subset(lon.pop.M25,lockdown=="0"),control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
toc<-Sys.time()
toc-tic

save(lon.lme12e_0,file="C:/Users/David/Documents/Facebook COVID/lon.lme12e_0i.Rdata")

rm(lon.lme12e_0)
gc()


library(ggeffects)

wkend0800_2<-ggpredict(lon.lme12ns,terms=c("coverage_area","IMD [8.6, 15.9,24,44]"),condition=c(lockdown="2",weekend="yes",time="0800"))

png(file="C:/Users/David/Documents/Facebook COVID/London_ns_coverageimd_2_wkend_0800.png", width=60,height=42,units="cm",res=200)
plot(wkend0800_2)
#plot(allEffects(lon.lme10,xlevels=list(IMD=c(8.6, 15.9,24,44))),main="") #10-20, 40, 60, 80-100
dev.off()

rm(wkend0800_2)
gc()
wk0800_2<-ggpredict(lon.lme12ns,terms=c("coverage_area","IMD [8.6, 15.9,24,44]"),condition=c(lockdown="2",weekend="no",time="0800"))

png(file="C:/Users/David/Documents/Facebook COVID/London_ns_coverageimd_2_wk_0800.png", width=60,height=42,units="cm",res=200)
plot(wk0800_2)
#plot(allEffects(lon.lme10,xlevels=list(IMD=c(8.6, 15.9,24,44))),main="") #10-20, 40, 60, 80-100
dev.off()


rm(wk0800_2)
gc()
wk0800_1<-ggpredict(lon.lme12ns,terms=c("coverage_area","IMD [8.6, 15.9,24,44]"),condition=c(lockdown="1",weekend="no",time="0800"))

png(file="C:/Users/David/Documents/Facebook COVID/London_ns_coverageimd_1_wk_0800.png", width=60,height=42,units="cm",res=200)
plot(wk0800_1)
#plot(allEffects(lon.lme10,xlevels=list(IMD=c(8.6, 15.9,24,44))),main="") #10-20, 40, 60, 80-100
dev.off()


rm(wk0800_1)
gc()
wkend0800_1<-ggpredict(lon.lme12ns,terms=c("coverage_area","IMD [8.6, 15.9,24,44]"),condition=c(lockdown="1",weekend="yes",time="0800"))

png(file="C:/Users/David/Documents/Facebook COVID/London_ns_coverageimd_1_wkend_0800.png", width=60,height=42,units="cm",res=200)
plot(wkend0800_1)
#plot(allEffects(lon.lme10,xlevels=list(IMD=c(8.6, 15.9,24,44))),main="") #10-20, 40, 60, 80-100
dev.off()


rm(wkend0800_1)
gc()
wkend0800_0<-ggpredict(lon.lme12ns,terms=c("coverage_area","IMD [8.6, 15.9,24,44]"),condition=c(lockdown="0",weekend="yes",time="0800"))

png(file="C:/Users/David/Documents/Facebook COVID/London_ns_coverageimd_0_wkend_0800.png", width=60,height=42,units="cm",res=200)
plot(wkend0800_0)
#plot(allEffects(lon.lme10,xlevels=list(IMD=c(8.6, 15.9,24,44))),main="") #10-20, 40, 60, 80-100
dev.off()

rm(wkend0800_0)
gc()
wk0800_0<-ggpredict(lon.lme12ns,terms=c("coverage_area","IMD [8.6, 15.9,24,44]"),condition=c(lockdown="0",weekend="no",time="0800"))

png(file="C:/Users/David/Documents/Facebook COVID/London_ns_coverageimd_0_wk_0800.png", width=60,height=42,units="cm",res=200)
plot(wk0800_0)
#plot(allEffects(lon.lme10,xlevels=list(IMD=c(8.6, 15.9,24,44))),main="") #10-20, 40, 60, 80-100
dev.off()




















#######################################################################################################################################
png(file="C:/Users/David/Documents/Facebook COVID/fittedIMDxgreenspacemodel_quintile.png", width=60,height=42,units="cm",res=200)
plot(allEffects(lon.lme12,xlevels=list(IMD=c(8.6, 15.9,24,44))),main="") #10-20, 40, 60, 80-100
dev.off()




#lon.lme10b 58454533 failed convergence when linear weekend effect not clear

#library(nlme)
lon.lme0<-lmer(clipped_z_score~ns(coverage_area,df=3)*time*weekend+ (1|quadkey),data=lon.pop.M25)
lon.lme1<-lmer(clipped_z_score~ns(coverage_area,df=3)*time*weekend+ (1|dated) + (1|quadkey),data=lon.pop.M25)
lon.lme2<-lmer(clipped_z_score~ns(coverage_area,df=3)*time*weekend+ (1|dated) + (1|quadkey) +(1|time),data=lon.pop.M25)

#looks like lme1 is better

#let's move to fixed effects
lon.lme1<-lmer(clipped_z_score~ns(coverage_area,df=3)*time*weekend+ (1|dated) + (1|quadkey),data=lon.pop.M25,REML=FALSE)
lon.lme2<-lmer(clipped_z_score~ns(coverage_area,df=4)*time*weekend+ (1|dated) + (1|quadkey),data=lon.pop.M25,REML=FALSE)
AIC(lon.lme1,lon.lme2)
#tried df=5 and worsens so looks like df 4 describes the curves

lon.lme3<-lmer(clipped_z_score~ns(coverage_area,df=4)*weekend +time+ (1|dated) + (1|quadkey),data=lon.pop.M25,REML=FALSE)
lon.lme4<-lmer(clipped_z_score~ns(coverage_area,df=4)+time+weekend+ (1|dated) + (1|quadkey),data=lon.pop.M25,REML=FALSE)
AIC(lon.lme2,lon.lme3,lon.lme4)

lon.lme5<-lmer(clipped_z_score~ns(IMD,df=2)*time*weekend+ (1|dated) + (1|quadkey),data=lon.pop.M25,REML=FALSE)
lon.lme6<-lmer(clipped_z_score~ns(IMD,df=3)*time*weekend+ (1|dated) + (1|quadkey),data=lon.pop.M25,REML=FALSE)
lon.lme7<-lmer(clipped_z_score~ns(IMD,df=4)*time*weekend+ (1|dated) + (1|quadkey),data=lon.pop.M25,REML=FALSE)
lon.lme8<-lmer(clipped_z_score~IMD*time*weekend+ (1|dated) + (1|quadkey),data=lon.pop.M25,REML=FALSE)
AIC(lon.lme2,lon.lme5,lon.lme6,lon.lme7,lon.lme8)



lon.lme9<-lmer(clipped_z_score~ns(IMD,df=4)*time*weekend+ns(coverage_area,df=4)*time*weekend+ (1|dated) + (1|quadkey),data=lon.pop.M25,REML=FALSE)
AIC(lon.lme9)


lon.lme10<-lmer(clipped_z_score~ns(IMD,df=4)*time*weekend*ns(coverage_area,df=4)+ (1|dated) + (1|quadkey),data=lon.pop.M25)
AIC(lon.lme10)


###########################################################################################
##########################################################################################
##################################################################
#####twitter sampling



library(academictwitteR)


###########################################################################################
##########################################################################################
###additional function


ls_files <- function(data_path, pattern) {
   ## parse and bind
   files <-
     list.files(
       path = file.path(data_path),
       pattern = pattern,
       recursive = T,
       include.dirs = T,
       full.names = T
     )
   
   return(files)
 }


bind_tweets <- function(data_path, user = FALSE) {
   if(user) {
    files <- ls_files(data_path, "^users_")
   } else {
     files <- ls_files(data_path, "^data_")
   }
   json.df.all <- data.frame()
   for (i in seq_along(files)) {
     filename = files[[i]]
     json.df <- jsonlite::read_json(filename, simplifyVector = TRUE)
     if (user) {
       json.df <- json.df$users
     }
     json.df.all <- dplyr::bind_rows(json.df.all, json.df)
   return(json.df.all)
 }}

###############################################################################################
###############################################################################################



setwd("C:/Users/David/Documents/Facebook COVID/Twitter/")

Sys.setenv(TWITTER_BEARER="INSERT_YOURS_HERE")

get_bearer() # it checks out

start<-"2020-01-01T00:00:00Z"

#end<-"2021-04-05T00:00:00Z" #not sure what I was thinking
end<-"2021-08-30T00:00:00Z"

# tweets2 <-
  # get_all_tweets(
    # query=c("park","garden"),
    # start,
    # end,
	# n=1000000, #for trials restrict collection
    # data_path = "JanMar/" ,
	# country="GB",
	# place="london",
	# is_retweet=FALSE,
	# is_reply=FALSE,
	# is_quote=FALSE,
	# remove_promoted=TRUE, #these ensure we are only collecting original tweets

  # )
# # 48,487 tweets in total for this period and location-bound 
# #51,126 tweets in total for this period and location-bound starting Apr 5th

#load("C:/Users/David/Documents/Facebook COVID/LondonParks.Rdata")

#tweets<-dplyr::bind_rows(tweets2,tweets)

#save(tweets,file="C:/Users/David/Documents/Facebook COVID/LondonParks.Rdata")


tweets_london <-
  get_all_tweets(
    query=c("park"),
    start,
    end,
	n=1000000, #for trials restrict collection
    data_path = "London/" ,
	country="GB",
	place="london",
	is_retweet=FALSE,
	is_reply=FALSE,
	is_quote=FALSE,
	remove_promoted=TRUE, #these ensure we are only collecting original tweets

  )


save(tweets_london,file="C:/Users/David/Documents/Facebook COVID/Twitter/LondonParks_only.Rdata")


tweets_berlin <-
  get_all_tweets(
    query=c("park"),
    start,
    end,
	n=1000000, #for trials restrict collection
    data_path = "Berlin/" ,
	country="DE",
	place="berlin",
	is_retweet=FALSE,
	is_reply=FALSE,
	is_quote=FALSE,
	remove_promoted=TRUE, #these ensure we are only collecting original tweets

  )


save(tweets_berlin,file="C:/Users/David/Documents/Facebook COVID/Twitter/BerlinParks_only.Rdata")


tweets_paris <-
  get_all_tweets(
    query=c("parc"),
	exclude=c("foot","PSG","Parc Des Princes","disney"),
    start,
    end,
	n=1000000, #for trials restrict collection
    data_path = "Paris/" ,
	country="FR",
	place="paris",
	is_retweet=FALSE,
	is_reply=FALSE,
	is_quote=FALSE,
	remove_promoted=TRUE, #these ensure we are only collecting original tweets

  )


save(tweets_paris,file="C:/Users/David/Documents/Facebook COVID/Twitter/ParisParks_only.Rdata")


tweets_paris_jardin <-
  get_all_tweets(
    query=c("jardin","square"),
	#exclude=c("foot","PSG","Parc Des Princes","disney"),
    start,
    end,
	n=1000000, #for trials restrict collection
    data_path = "Paris_all/" ,
	country="FR",
	place="paris",
	is_retweet=FALSE,
	is_reply=FALSE,
	is_quote=FALSE,
	remove_promoted=TRUE, #these ensure we are only collecting original tweets

  )

load("C:/Users/David/Documents/Facebook COVID/Twitter/ParisParks_only.Rdata")

tweets_paris_all<-dplyr::bind_rows(tweets_paris,tweets_paris_jardin)

save(tweets_paris_all,file="C:/Users/David/Documents/Facebook COVID/Twitter/ParisParks.Rdata")

tweets_berlin_garten <-
  get_all_tweets(
    query=c("gärten","garten","Südgelände","suedgelaende","sudgelande"),
    start,
    end,
	n=1000000, #for trials restrict collection
    data_path = "Berlin_all/" ,
	country="DE",
	place="berlin",
	is_retweet=FALSE,
	is_reply=FALSE,
	is_quote=FALSE,
	remove_promoted=TRUE, #these ensure we are only collecting original tweets

  )

load("C:/Users/David/Documents/Facebook COVID/Twitter/BerlinParks_only.Rdata")

tweets_berlin_all<-dplyr::bind_rows(tweets_berlin,tweets_berlin_garten)


save(tweets_berlin_all,file="C:/Users/David/Documents/Facebook COVID/Twitter/BerlinParks.Rdata")



##############################################################################################################
### harvest finished
##############################################################################################################

library(glmmTMB)
library(ggeffects)

load("C:/Users/David/Documents/Facebook COVID/Twitter/BerlinParks.Rdata")
load("C:/Users/David/Documents/Facebook COVID/Twitter/ParisParks.Rdata")
load("C:/Users/David/Documents/Facebook COVID/Twitter/LondonParks.Rdata")


# lockdown<-read.csv("C:/Users/David/Documents/Facebook COVID/lockdown_indicators/OxCGRT_latest_Aug_30_2021.txt",header=T)
# lockdown$date.pos<-as.Date(as.character(lockdown$Date),"%Y%m%d")

# lockdown.london<-lockdown[lockdown$RegionName=="England",c(17,52)]

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



tweets$entities$annotations #df $type
dates.lon<-as.Date(tweets$created_at)
date.t.df.lon<-as.data.frame(table(dates.lon))
date.t.df.lon$dates<-as.Date(date.t.df.lon$dates)

dates.ber<-as.Date(tweets_berlin_all$created_at)
date.t.df.ber<-as.data.frame(table(dates.ber))
date.t.df.ber$dates<-as.Date(date.t.df.ber$dates)

dates.par<-as.Date(tweets_paris_all$created_at)
date.t.df.par<-as.data.frame(table(dates.par))
date.t.df.par$dates<-as.Date(date.t.df.par$dates)


tweet.vol.lon<-merge(lockdown.dat[lockdown.dat$region=="London",],date.t.df.lon,by.x="date",by.y="dates",all.x=TRUE,all.y=TRUE)
tweet.vol.par<-merge(lockdown.dat[lockdown.dat$region=="Paris",],date.t.df.par,by.x="date",by.y="dates",all.x=TRUE,all.y=TRUE)
tweet.vol.ber<-merge(lockdown.dat[lockdown.dat$region=="Berlin",],date.t.df.ber,by.x="date",by.y="dates",all.x=TRUE,all.y=TRUE)

tweet.vol.ber$Freq[which(is.na(tweet.vol.ber$Freq))]<-0
tweet.vol.lon$Freq[which(is.na(tweet.vol.lon$Freq))]<-0
tweet.vol.par$Freq[which(is.na(tweet.vol.par$Freq))]<-0

plot(tweet.vol.par$Freq~tweet.vol.par$date)
lines((tweet.vol.par$lockdown*10)~tweet.vol.par$date)

# plot(log10(tweet.vol$Freq)~tweet.vol$date.pos)
# lines((tweet.vol$C6_Stay_at_home_requirements+1)~tweet.vol$date.pos)

# plot((tweet.vol$C6_Stay_at_home_requirements)~tweet.vol$date.pos)

# tweet.vol$C6_Stay_at_home_requirements[is.na(tweet.vol$C6_Stay_at_home_requirements)]<-0
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

load("C:/Users/David/Documents/Facebook COVID/Twitter/tweet.volume.df.all.Rdata")
#####################################################################################################################################
######london
vol.glm.lon.0<-glmmTMB(Freq~lockdown.phase+(1|lockdown.phase:repeats)+ar1(date.f+0|lockdown.phase:repeats),data=tweet.vol.lon,nbinom2)
vol.glm.lon.0b<-glmmTMB(Freq~lockdown.phase+(1|lockdown.phase:repeats)+ar1(date.f+0|group),data=tweet.vol.lon,nbinom2)
vol.glm.lon.1<-glmmTMB(Freq~lockdown.phase+(1|repeats.cum)+ar1(date.f+0|repeats.cum),data=tweet.vol.lon,nbinom2)
vol.glm.lon.2<-glmmTMB(Freq~lockdown.phase*repeats+(1|repeats.cum)+ar1(date.f+0|repeats.cum),data=tweet.vol.lon,nbinom2)
vol.glm.lon.3<-glmmTMB(Freq~1+(1|lockdown.phase:repeats)+ar1(date.f+0|lockdown.phase:repeats),data=tweet.vol.lon,nbinom2)
vol.glm.lon.4<-glmmTMB(Freq~1+(1|repeats.cum)+ar1(date.f+0|repeats.cum),data=tweet.vol.lon,nbinom2)
vol.glm.lon.5<-glmmTMB(Freq~1*repeats+(1|repeats.cum)+ar1(date.f+0|repeats.cum),data=tweet.vol.lon,nbinom2)
vol.glm.lon.6<-glmmTMB(Freq~lockdown.phase*repeats+ar1(date.f+0|fake),data=tweet.vol.lon,nbinom2)
vol.glm.lon.10<-glmmTMB(Freq~1+ar1(date.f+0|phase),data=tweet.vol.lon,nbinom2)
vol.glm.lon.8<-glmmTMB(Freq~lockdown.phase+ar1(date.f+0|phase),data=tweet.vol.lon,nbinom2)
vol.glm.lon.9<-glmmTMB(Freq~lockdown.phase*repeats+ar1(date.f+0|phase),data=tweet.vol.lon,nbinom2)
vol.glm.lon.9b<-glmmTMB(Freq~lockdown.phase*repeats+ar1(date.f+0|group),data=tweet.vol.lon,nbinom2)
vol.glm.lon.7<-glmmTMB(Freq~phase+ar1(date.f+0|phase),data=tweet.vol.lon,nbinom2)
vol.glm.lon.11<-glmmTMB(Freq~lockdown.phase+repeats+ar1(date.f+0|phase),data=tweet.vol.lon,nbinom2)
vol.glm.lon.12<-glmmTMB(Freq~lockdown.phase+(1|phase)+ar1(date.f+0|phase),data=tweet.vol.lon,nbinom2)
vol.glm.lon.13<-glmmTMB(Freq~lockdown.phase+(1|repeats.cum)+ar1(date.f+0|phase),data=tweet.vol.lon,nbinom2)
vol.glm.lon.14<-glmmTMB(Freq~lockdown.phase+(1|repeats)+ar1(date.f+0|phase),data=tweet.vol.lon,nbinom2)
AIC(vol.glm.lon.0,vol.glm.lon.1,vol.glm.lon.2,vol.glm.lon.3,vol.glm.lon.4,vol.glm.lon.5,vol.glm.lon.6,vol.glm.lon.7,vol.glm.lon.8,vol.glm.lon.9,vol.glm.lon.10,vol.glm.lon.11,vol.glm.lon.12,vol.glm.lon.13,vol.glm.lon.14)

##################################
#### added 25 Mar 2022, fixed date NAs
tweet.vol.lon$date.22<-as.numeric(tweet.vol.lon$date)-min(as.numeric(tweet.vol.lon$date))+1

tweet.vol.lon$date.f<-factor(tweet.vol.lon$date.22)

vol.glm.lon.00f<-glmmTMB(Freq~1+ar1(date.f+0|group),data=subset(tweet.vol.lon,repeats!="4"),nbinom2)
vol.glm.lon.0f<-glmmTMB(Freq~lockdown.phase+ar1(date.f+0|group),data=subset(tweet.vol.lon,repeats!="4"),nbinom2)
vol.glm.lon.1f<-glmmTMB(Freq~repeats+ar1(date.f+0|group),data=subset(tweet.vol.lon,repeats!="4"),nbinom2)
vol.glm.lon.2f<-glmmTMB(Freq~lockdown.phase+repeats+ar1(date.f+0|group),data=subset(tweet.vol.lon,repeats!="4"),nbinom2)
vol.glm.lon.3f<-glmmTMB(Freq~lockdown.phase*repeats+ar1(date.f+0|group),data=subset(tweet.vol.lon,repeats!="4"),nbinom2)
AIC(vol.glm.lon.00f,vol.glm.lon.0f,vol.glm.lon.1f,vol.glm.lon.2f,vol.glm.lon.3f)
                # df      AIC
# vol.glm.lon.00f  4 4591.193
# vol.glm.lon.0f   6 4592.390
# vol.glm.lon.1f   6 4594.563
# vol.glm.lon.2f   8 4596.318
# vol.glm.lon.3f  12 4553.300

Anova(vol.glm.lon.3f)

plot(ggpredict(vol.glm.lon.3f,c("lockdown.phase","repeats")))

################################################################################################################################################

#####################################################################################################################################
######paris
vol.glm.par.0<-glmmTMB(Freq~lockdown.phase+(1|lockdown.phase:repeats)+ar1(date.f+0|lockdown.phase:repeats),data=tweet.vol.par,nbinom2)
vol.glm.par.1<-glmmTMB(Freq~lockdown.phase+(1|repeats.cum)+ar1(date.f+0|repeats.cum),data=tweet.vol.par,nbinom2)
vol.glm.par.2<-glmmTMB(Freq~lockdown.phase*repeats+(1|repeats.cum)+ar1(date.f+0|repeats.cum),data=tweet.vol.par,nbinom2)

vol.glm.par.3<-glmmTMB(Freq~1+(1|lockdown.phase:repeats)+ar1(date.f+0|lockdown.phase:repeats),data=tweet.vol.par,nbinom2)
vol.glm.par.4<-glmmTMB(Freq~1+(1|repeats.cum)+ar1(date.f+0|repeats.cum),data=tweet.vol.par,nbinom2)
vol.glm.par.5<-glmmTMB(Freq~1*repeats+(1|repeats.cum)+ar1(date.f+0|repeats.cum),data=tweet.vol.par,nbinom2)


vol.glm.par.6<-glmmTMB(Freq~lockdown.phase*repeats+ar1(date.f+0|fake),data=tweet.vol.par,nbinom2)

vol.glm.par.10<-glmmTMB(Freq~1+ar1(date.f+0|phase),data=tweet.vol.par,nbinom2)
vol.glm.par.8<-glmmTMB(Freq~lockdown.phase+ar1(date.f+0|phase),data=tweet.vol.par,nbinom2)
vol.glm.par.9<-glmmTMB(Freq~lockdown.phase*repeats+ar1(date.f+0|phase),data=tweet.vol.par,nbinom2)
vol.glm.par.7<-glmmTMB(Freq~phase+ar1(date.f+0|phase),data=tweet.vol.par,nbinom2)

vol.glm.par.11<-glmmTMB(Freq~lockdown.phase+repeats+ar1(date.f+0|phase),data=tweet.vol.par,nbinom2)
vol.glm.par.12<-glmmTMB(Freq~lockdown.phase+(1|phase)+ar1(date.f+0|phase),data=tweet.vol.par,nbinom2)
vol.glm.par.13<-glmmTMB(Freq~lockdown.phase+(1|repeats.cum)+ar1(date.f+0|phase),data=tweet.vol.par,nbinom2)
vol.glm.par.14<-glmmTMB(Freq~lockdown.phase+(1|repeats)+ar1(date.f+0|phase),data=tweet.vol.par,nbinom2)

AIC(vol.glm.par.0,vol.glm.par.1,vol.glm.par.2,vol.glm.par.3,vol.glm.par.4,vol.glm.par.5,vol.glm.par.6,vol.glm.par.7,vol.glm.par.8,vol.glm.par.9,vol.glm.par.10,vol.glm.par.11,vol.glm.par.12,vol.glm.par.13,vol.glm.par.14)

################################################################################################################################################
vol.glm.par.00f<-glmmTMB(Freq~1+ar1(date.f+0|group),data=subset(tweet.vol.par,repeats!="3" &lockdown.phase!="1"),nbinom2)
vol.glm.par.0f<-glmmTMB(Freq~lockdown.phase+ar1(date.f+0|group),data=subset(tweet.vol.par,repeats!="3"&lockdown.phase!="1"),nbinom2)
vol.glm.par.1f<-glmmTMB(Freq~repeats+ar1(date.f+0|group),data=subset(tweet.vol.par,repeats!="3"&lockdown.phase!="1"),nbinom2)
vol.glm.par.2f<-glmmTMB(Freq~lockdown.phase+repeats+ar1(date.f+0|group),data=subset(tweet.vol.par,repeats!="3"&lockdown.phase!="1"),nbinom2)
vol.glm.par.3f<-glmmTMB(Freq~lockdown.phase*repeats+ar1(date.f+0|group),data=subset(tweet.vol.par,repeats!="3"&lockdown.phase!="1"),nbinom2)
AIC(vol.glm.par.00f,vol.glm.par.0f,vol.glm.par.1f,vol.glm.par.2f,vol.glm.par.3f)


#####################################################################################################################################
######berlin
vol.glm.ber.0<-glmmTMB(Freq~lockdown.phase+(1|lockdown.phase:repeats)+ar1(date.f+0|lockdown.phase:repeats),data=tweet.vol.ber,nbinom2)
vol.glm.ber.1<-glmmTMB(Freq~lockdown.phase+(1|repeats.cum)+ar1(date.f+0|repeats.cum),data=tweet.vol.ber,nbinom2)
vol.glm.ber.2<-glmmTMB(Freq~lockdown.phase*repeats+(1|repeats.cum)+ar1(date.f+0|repeats.cum),data=tweet.vol.ber,nbinom2)

vol.glm.ber.3<-glmmTMB(Freq~1+(1|lockdown.phase:repeats)+ar1(date.f+0|lockdown.phase:repeats),data=tweet.vol.ber,nbinom2)
vol.glm.ber.4<-glmmTMB(Freq~1+(1|repeats.cum)+ar1(date.f+0|repeats.cum),data=tweet.vol.ber,nbinom2)
vol.glm.ber.5<-glmmTMB(Freq~1*repeats+(1|repeats.cum)+ar1(date.f+0|repeats.cum),data=tweet.vol.ber,nbinom2)


vol.glm.ber.6<-glmmTMB(Freq~lockdown.phase*repeats+ar1(date.f+0|fake),data=tweet.vol.ber,nbinom2)

vol.glm.ber.10<-glmmTMB(Freq~1+ar1(date.f+0|phase),data=tweet.vol.ber,nbinom2)
vol.glm.ber.8<-glmmTMB(Freq~lockdown.phase+ar1(date.f+0|phase),data=tweet.vol.ber,nbinom2)
vol.glm.ber.9<-glmmTMB(Freq~lockdown.phase*repeats+ar1(date.f+0|phase),data=tweet.vol.ber,nbinom2)
vol.glm.ber.7<-glmmTMB(Freq~phase+ar1(date.f+0|phase),data=tweet.vol.ber,nbinom2)

vol.glm.ber.11<-glmmTMB(Freq~lockdown.phase+repeats+ar1(date.f+0|phase),data=tweet.vol.ber,nbinom2)
vol.glm.ber.12<-glmmTMB(Freq~lockdown.phase+(1|phase)+ar1(date.f+0|phase),data=tweet.vol.ber,nbinom2)
vol.glm.ber.13<-glmmTMB(Freq~lockdown.phase+(1|repeats.cum)+ar1(date.f+0|phase),data=tweet.vol.ber,nbinom2)
vol.glm.ber.14<-glmmTMB(Freq~lockdown.phase+(1|repeats)+ar1(date.f+0|phase),data=tweet.vol.ber,nbinom2)

AIC(vol.glm.ber.0,vol.glm.ber.1,vol.glm.ber.2,vol.glm.ber.3,vol.glm.ber.4,vol.glm.ber.5,vol.glm.ber.6,vol.glm.ber.7,vol.glm.ber.8,vol.glm.ber.9,vol.glm.ber.10,vol.glm.ber.11,vol.glm.ber.12,vol.glm.ber.13,vol.glm.ber.14)

################################################################################################################################################
vol.glm.ber.00f<-glmmTMB(Freq~1+ar1(date.f+0|group),data=subset(tweet.vol.ber,repeats!="3"),nbinom2)
vol.glm.ber.0f<-glmmTMB(Freq~lockdown.phase+ar1(date.f+0|group),data=subset(tweet.vol.ber,repeats!="3"),nbinom2)
vol.glm.ber.1f<-glmmTMB(Freq~repeats+ar1(date.f+0|group),data=subset(tweet.vol.ber,repeats!="3"),nbinom2)
vol.glm.ber.2f<-glmmTMB(Freq~lockdown.phase+repeats+ar1(date.f+0|group),data=subset(tweet.vol.ber,repeats!="3"),nbinom2)
vol.glm.ber.3f<-glmmTMB(Freq~lockdown.phase*repeats+ar1(date.f+0|group),data=subset(tweet.vol.ber,repeats!="3"),nbinom2)
AIC(vol.glm.ber.00f,vol.glm.ber.0f,vol.glm.ber.1f,vol.glm.ber.2f,vol.glm.ber.3f)

################################################################################################################################################


plot(ggpredict(vol.glm.lon.9b,c("lockdown.phase","repeats")))
plot(ggpredict(vol.glm.ber.7))

# library(ggeffects)

# plot(ggpredict(vol.glm7))

# windows()
# plot(as.numeric(tweet.vol$phase)~tweet.vol$date.pos,col=tweet.vol$lockdown.phase)



###we need to replicate for berlin and paris

#######################################################################################################################

berlin.weather<-read.csv("C:/Users/David/OneDrive - Danmarks Tekniske Universitet/weather/meteostat_Berlin_weather_covid_DE-RIXV.csv",header=T,sep=";")
paris.weather<-read.csv("C:/Users/David/OneDrive - Danmarks Tekniske Universitet/weather/meteostat_Paris_weather_covid_FR-4FGP.csv",header=T,sep=";")
london.weather<-read.csv("C:/Users/David/OneDrive - Danmarks Tekniske Universitet/weather/meteostat_London_weather_covid_GB-QK9E.csv",header=T,sep=";")
berlin.weather$date<-as.Date(berlin.weather$date,"%d-%m-%Y")
paris.weather$date<-as.Date(paris.weather$date,"%d-%m-%Y")
london.weather$date<-as.Date(london.weather$date,"%d-%m-%Y")



tweet.vol.lon<-merge(tweet.vol.lon,london.weather,by.x="date",by.y="date",all.x=TRUE)
tweet.vol.par<-merge(tweet.vol.par,paris.weather,by.x="date",by.y="date",all.x=TRUE)
tweet.vol.ber<-merge(tweet.vol.ber,berlin.weather,by.x="date",by.y="date",all.x=TRUE)

###########################################################################################################################
####partial out the effect of temperature


#####################################################################################################################################
######london
vol.glm.lon.0<-glmmTMB(Freq~lockdown.phase+(1|lockdown.phase:repeats)+ar1(date.f+0|group),data=tweet.vol.lon,nbinom2)
vol.glm.lon.0b<-glmmTMB(Freq~tavg+lockdown.phase+(1|lockdown.phase:repeats)+ar1(date.f+0|group),data=tweet.vol.lon,nbinom2)

vol.glm.lon.0bd<-glmmTMB(Freq~tavg*lockdown.phase+(1|lockdown.phase:repeats)+ar1(date.f+0|group),data=tweet.vol.lon,nbinom2)
vol.glm.lon.0bcd<-glmmTMB(Freq~tavg*lockdown.phase+(1|lockdown.phase:repeats)+ar1(date.f+0|lockdown.phase:repeats),data=tweet.vol.lon,nbinom2)
vol.glm.lon.3bcd<-glmmTMB(Freq~tavg*(1|lockdown.phase:repeats)+ar1(date.f+0|lockdown.phase:repeats),data=tweet.vol.lon,nbinom2)
vol.glm.lon.6bd<-glmmTMB(Freq~tavg*lockdown.phase*repeats+ar1(date.f+0|group),data=tweet.vol.lon,nbinom2)
vol.glm.lon.2bd<-glmmTMB(Freq~tavg*lockdown.phase+repeats+ar1(date.f+0|group),data=tweet.vol.lon,nbinom2)
vol.glm.lon.7bd<-glmmTMB(Freq~tavg*phase+ar1(date.f+0|group),data=tweet.vol.lon,nbinom2)

vol.glm.lon.3<-glmmTMB(Freq~1+(1|lockdown.phase:repeats)+ar1(date.f+0|group),data=tweet.vol.lon,nbinom2)
vol.glm.lon.3b<-glmmTMB(Freq~tavg+(1|lockdown.phase:repeats)+ar1(date.f+0|group),data=tweet.vol.lon,nbinom2)

vol.glm.lon.0c<-glmmTMB(Freq~lockdown.phase+(1|lockdown.phase:repeats)+ar1(date.f+0|lockdown.phase:repeats),data=tweet.vol.lon,nbinom2)
vol.glm.lon.0bc<-glmmTMB(Freq~tavg+lockdown.phase+(1|lockdown.phase:repeats)+ar1(date.f+0|lockdown.phase:repeats),data=tweet.vol.lon,nbinom2)
vol.glm.lon.3c<-glmmTMB(Freq~1+(1|lockdown.phase:repeats)+ar1(date.f+0|lockdown.phase:repeats),data=tweet.vol.lon,nbinom2)
vol.glm.lon.3bc<-glmmTMB(Freq~tavg+(1|lockdown.phase:repeats)+ar1(date.f+0|lockdown.phase:repeats),data=tweet.vol.lon,nbinom2)

vol.glm.lon.6<-glmmTMB(Freq~lockdown.phase*repeats+ar1(date.f+0|group),data=tweet.vol.lon,nbinom2)

###pb with 2 unblanced by repeats (no 4s)
vol.glm.lon.6.sub<-glmmTMB(Freq~lockdown.phase*repeats+ar1(date.f+0|group),data=subset(tweet.vol.lon,repeats!="4"),nbinom2)


###
vol.glm.lon.6b<-glmmTMB(Freq~tavg+lockdown.phase*repeats+ar1(date.f+0|group),data=tweet.vol.lon,nbinom2)
vol.glm.lon.2<-glmmTMB(Freq~lockdown.phase+repeats+ar1(date.f+0|group),data=tweet.vol.lon,nbinom2)
vol.glm.lon.2b<-glmmTMB(Freq~tavg+lockdown.phase+repeats+ar1(date.f+0|group),data=tweet.vol.lon,nbinom2)
vol.glm.lon.7<-glmmTMB(Freq~phase+ar1(date.f+0|group),data=tweet.vol.lon,nbinom2)
vol.glm.lon.7b<-glmmTMB(Freq~tavg+phase+ar1(date.f+0|group),data=tweet.vol.lon,nbinom2)
AIC(vol.glm.lon.0bd,vol.glm.lon.0bcd,vol.glm.lon.3bcd,vol.glm.lon.6bd,vol.glm.lon.2bd,vol.glm.lon.7bd,vol.glm.lon.0,vol.glm.lon.0b,vol.glm.lon.3,vol.glm.lon.3b, vol.glm.lon.0c,vol.glm.lon.0bc,vol.glm.lon.3c,vol.glm.lon.3bc, vol.glm.lon.6,vol.glm.lon.6b,vol.glm.lon.2,vol.glm.lon.2b,vol.glm.lon.7,vol.glm.lon.7b)

################################################################################################################################################

#####################################################################################################################################
######paris
vol.glm.par.0<-glmmTMB(Freq~lockdown.phase+(1|lockdown.phase:repeats)+ar1(date.f+0|group),data=tweet.vol.par,nbinom2)
vol.glm.par.0b<-glmmTMB(Freq~tavg+lockdown.phase+(1|lockdown.phase:repeats)+ar1(date.f+0|group),data=tweet.vol.par,nbinom2)
vol.glm.par.3<-glmmTMB(Freq~1+(1|lockdown.phase:repeats)+ar1(date.f+0|group),data=tweet.vol.par,nbinom2)
vol.glm.par.3b<-glmmTMB(Freq~tavg+(1|lockdown.phase:repeats)+ar1(date.f+0|group),data=tweet.vol.par,nbinom2)

vol.glm.par.0bd<-glmmTMB(Freq~tavg*lockdown.phase+(1|lockdown.phase:repeats)+ar1(date.f+0|group),data=tweet.vol.par,nbinom2)
vol.glm.par.0bcd<-glmmTMB(Freq~tavg*lockdown.phase+(1|lockdown.phase:repeats)+ar1(date.f+0|lockdown.phase:repeats),data=tweet.vol.par,nbinom2)
vol.glm.par.3bcd<-glmmTMB(Freq~tavg*(1|lockdown.phase:repeats)+ar1(date.f+0|lockdown.phase:repeats),data=tweet.vol.par,nbinom2)
vol.glm.par.6bd<-glmmTMB(Freq~tavg*lockdown.phase*repeats+ar1(date.f+0|group),data=tweet.vol.par,nbinom2)
vol.glm.par.2bd<-glmmTMB(Freq~tavg*lockdown.phase+repeats+ar1(date.f+0|group),data=tweet.vol.par,nbinom2)
vol.glm.par.7bd<-glmmTMB(Freq~tavg*phase+ar1(date.f+0|group),data=tweet.vol.par,nbinom2)


vol.glm.par.0c<-glmmTMB(Freq~lockdown.phase+(1|lockdown.phase:repeats)+ar1(date.f+0|lockdown.phase:repeats),data=tweet.vol.par,nbinom2)
vol.glm.par.0bc<-glmmTMB(Freq~tavg+lockdown.phase+(1|lockdown.phase:repeats)+ar1(date.f+0|lockdown.phase:repeats),data=tweet.vol.par,nbinom2)
vol.glm.par.3c<-glmmTMB(Freq~1+(1|lockdown.phase:repeats)+ar1(date.f+0|lockdown.phase:repeats),data=tweet.vol.par,nbinom2)
vol.glm.par.3bc<-glmmTMB(Freq~tavg+(1|lockdown.phase:repeats)+ar1(date.f+0|lockdown.phase:repeats),data=tweet.vol.par,nbinom2)


vol.glm.par.6<-glmmTMB(Freq~lockdown.phase*repeats+ar1(date.f+0|group),data=tweet.vol.par,nbinom2)
vol.glm.par.6b<-glmmTMB(Freq~tavg+lockdown.phase*repeats+ar1(date.f+0|group),data=tweet.vol.par,nbinom2)
vol.glm.par.2<-glmmTMB(Freq~lockdown.phase+repeats+ar1(date.f+0|group),data=tweet.vol.par,nbinom2)
vol.glm.par.2b<-glmmTMB(Freq~tavg+lockdown.phase+repeats+ar1(date.f+0|group),data=tweet.vol.par,nbinom2)
vol.glm.par.7<-glmmTMB(Freq~phase+ar1(date.f+0|group),data=tweet.vol.par,nbinom2)
vol.glm.par.7b<-glmmTMB(Freq~tavg+phase+ar1(date.f+0|group),data=tweet.vol.par,nbinom2)
AIC(vol.glm.par.0bd,vol.glm.par.0bcd,vol.glm.par.3bcd,vol.glm.par.6bd,vol.glm.par.2bd,vol.glm.par.7bd,vol.glm.par.0,vol.glm.par.0b,vol.glm.par.3,vol.glm.par.3b, vol.glm.par.0c,vol.glm.par.0bc,vol.glm.par.3c,vol.glm.par.3bc, vol.glm.par.6,vol.glm.par.6b,vol.glm.par.2,vol.glm.par.2b,vol.glm.par.7,vol.glm.par.7b)

################################################################################################################################################


#####################################################################################################################################
######berlin
vol.glm.ber.0<-glmmTMB(Freq~lockdown.phase+(1|lockdown.phase:repeats)+ar1(date.f+0|group),data=tweet.vol.ber,nbinom2)
vol.glm.ber.0b<-glmmTMB(Freq~tavg+lockdown.phase+(1|lockdown.phase:repeats)+ar1(date.f+0|group),data=tweet.vol.ber,nbinom2)
vol.glm.ber.3<-glmmTMB(Freq~1+(1|lockdown.phase:repeats)+ar1(date.f+0|group),data=tweet.vol.ber,nbinom2)
vol.glm.ber.3b<-glmmTMB(Freq~tavg+(1|lockdown.phase:repeats)+ar1(date.f+0|group),data=tweet.vol.ber,nbinom2)


vol.glm.ber.0bd<-glmmTMB(Freq~tavg*lockdown.phase+(1|lockdown.phase:repeats)+ar1(date.f+0|group),data=tweet.vol.ber,nbinom2)
vol.glm.ber.0bcd<-glmmTMB(Freq~tavg*lockdown.phase+(1|lockdown.phase:repeats)+ar1(date.f+0|lockdown.phase:repeats),data=tweet.vol.ber,nbinom2)
vol.glm.ber.3bcd<-glmmTMB(Freq~tavg*(1|lockdown.phase:repeats)+ar1(date.f+0|lockdown.phase:repeats),data=tweet.vol.ber,nbinom2)
vol.glm.ber.6bd<-glmmTMB(Freq~tavg*lockdown.phase*repeats+ar1(date.f+0|group),data=tweet.vol.ber,nbinom2)
vol.glm.ber.2bd<-glmmTMB(Freq~tavg*lockdown.phase+repeats+ar1(date.f+0|group),data=tweet.vol.ber,nbinom2)
vol.glm.ber.7bd<-glmmTMB(Freq~tavg*phase+ar1(date.f+0|group),data=tweet.vol.ber,nbinom2)


vol.glm.ber.0c<-glmmTMB(Freq~lockdown.phase+(1|lockdown.phase:repeats)+ar1(date.f+0|lockdown.phase:repeats),data=tweet.vol.ber,nbinom2)
vol.glm.ber.0bc<-glmmTMB(Freq~tavg+lockdown.phase+(1|lockdown.phase:repeats)+ar1(date.f+0|lockdown.phase:repeats),data=tweet.vol.ber,nbinom2)
vol.glm.ber.3c<-glmmTMB(Freq~1+(1|lockdown.phase:repeats)+ar1(date.f+0|lockdown.phase:repeats),data=tweet.vol.ber,nbinom2)
vol.glm.ber.3bc<-glmmTMB(Freq~tavg+(1|lockdown.phase:repeats)+ar1(date.f+0|lockdown.phase:repeats),data=tweet.vol.ber,nbinom2)

vol.glm.ber.6<-glmmTMB(Freq~lockdown.phase*repeats+ar1(date.f+0|group),data=tweet.vol.ber,nbinom2)
vol.glm.ber.6b<-glmmTMB(Freq~tavg+lockdown.phase*repeats+ar1(date.f+0|group),data=tweet.vol.ber,nbinom2)
vol.glm.ber.2<-glmmTMB(Freq~lockdown.phase+repeats+ar1(date.f+0|group),data=tweet.vol.ber,nbinom2)
vol.glm.ber.2b<-glmmTMB(Freq~tavg+lockdown.phase+repeats+ar1(date.f+0|group),data=tweet.vol.ber,nbinom2)
vol.glm.ber.7<-glmmTMB(Freq~phase+ar1(date.f+0|group),data=tweet.vol.ber,nbinom2)
vol.glm.ber.7b<-glmmTMB(Freq~tavg+phase+ar1(date.f+0|group),data=tweet.vol.ber,nbinom2)
AIC(vol.glm.ber.0bd,vol.glm.ber.0bcd,vol.glm.ber.3bcd,vol.glm.ber.6bd,vol.glm.ber.2bd,vol.glm.ber.7bd,vol.glm.ber.0,vol.glm.ber.0b,vol.glm.ber.3,vol.glm.ber.3b, vol.glm.ber.0c,vol.glm.ber.0bc,vol.glm.ber.3c,vol.glm.ber.3bc, vol.glm.ber.6,vol.glm.ber.6b,vol.glm.ber.2,vol.glm.ber.2b,vol.glm.ber.7,vol.glm.ber.7b)

################################################################################################################################################


plot(ggpredict(vol.glm.ber.2b,c("lockdown.phase","repeats","tavg")))
plot(ggpredict(vol.glm.par.2b,c("lockdown.phase","repeats","tavg")))
plot(ggpredict(vol.glm.lon.2b,c("lockdown.phase","repeats","tavg")))

plot(ggpredict(vol.glm.lon.6.sub,c("lockdown.phase","repeats")))

plot(ggpredict(vol.glm.par.3bc))

table(tweet.vol.par$lockdown.phase,tweet.vol.par$repeats)
table(tweet.vol.ber$lockdown.phase,tweet.vol.ber$repeats)

###we need to do model selection for london on the subset)

######london subset
vol.glom.lon.sub.0<-glmmTMB(Freq~lockdown.phase+(1|lockdown.phase:repeats)+ar1(date.f+0|group),data=subset(tweet.vol.lon,repeats!="4"),nbinom2)
vol.glom.lon.sub.0b<-glmmTMB(Freq~tavg+lockdown.phase+(1|lockdown.phase:repeats)+ar1(date.f+0|group),data=subset(tweet.vol.lon,repeats!="4"),nbinom2)

vol.glom.lon.sub.0bd<-glmmTMB(Freq~tavg*lockdown.phase+(1|lockdown.phase:repeats)+ar1(date.f+0|group),data=subset(tweet.vol.lon,repeats!="4"),nbinom2)
vol.glom.lon.sub.0bcd<-glmmTMB(Freq~tavg*lockdown.phase+(1|lockdown.phase:repeats)+ar1(date.f+0|lockdown.phase:repeats),data=subset(tweet.vol.lon,repeats!="4"),nbinom2)
vol.glom.lon.sub.3bcd<-glmmTMB(Freq~tavg*(1|lockdown.phase:repeats)+ar1(date.f+0|lockdown.phase:repeats),data=subset(tweet.vol.lon,repeats!="4"),nbinom2)
vol.glom.lon.sub.6bd<-glmmTMB(Freq~tavg*lockdown.phase*repeats+ar1(date.f+0|group),data=subset(tweet.vol.lon,repeats!="4"),nbinom2)
vol.glom.lon.sub.2bd<-glmmTMB(Freq~tavg*lockdown.phase+repeats+ar1(date.f+0|group),data=subset(tweet.vol.lon,repeats!="4"),nbinom2)
vol.glom.lon.sub.7bd<-glmmTMB(Freq~tavg*phase+ar1(date.f+0|group),data=subset(tweet.vol.lon,repeats!="4"),nbinom2)

vol.glom.lon.sub.3<-glmmTMB(Freq~1+(1|lockdown.phase:repeats)+ar1(date.f+0|group),data=subset(tweet.vol.lon,repeats!="4"),nbinom2)
vol.glom.lon.sub.3b<-glmmTMB(Freq~tavg+(1|lockdown.phase:repeats)+ar1(date.f+0|group),data=subset(tweet.vol.lon,repeats!="4"),nbinom2)

vol.glom.lon.sub.0c<-glmmTMB(Freq~lockdown.phase+(1|lockdown.phase:repeats)+ar1(date.f+0|lockdown.phase:repeats),data=subset(tweet.vol.lon,repeats!="4"),nbinom2)
vol.glom.lon.sub.0bc<-glmmTMB(Freq~tavg+lockdown.phase+(1|lockdown.phase:repeats)+ar1(date.f+0|lockdown.phase:repeats),data=subset(tweet.vol.lon,repeats!="4"),nbinom2)
vol.glom.lon.sub.3c<-glmmTMB(Freq~1+(1|lockdown.phase:repeats)+ar1(date.f+0|lockdown.phase:repeats),data=subset(tweet.vol.lon,repeats!="4"),nbinom2)
vol.glom.lon.sub.3bc<-glmmTMB(Freq~tavg+(1|lockdown.phase:repeats)+ar1(date.f+0|lockdown.phase:repeats),data=subset(tweet.vol.lon,repeats!="4"),nbinom2)

vol.glom.lon.sub.6<-glmmTMB(Freq~lockdown.phase*repeats+ar1(date.f+0|group),data=subset(tweet.vol.lon,repeats!="4"),nbinom2)
vol.glom.lon.sub.6b<-glmmTMB(Freq~tavg+lockdown.phase*repeats+ar1(date.f+0|group),data=subset(tweet.vol.lon,repeats!="4"),nbinom2)
vol.glom.lon.sub.2<-glmmTMB(Freq~lockdown.phase+repeats+ar1(date.f+0|group),data=subset(tweet.vol.lon,repeats!="4"),nbinom2)
vol.glom.lon.sub.2b<-glmmTMB(Freq~tavg+lockdown.phase+repeats+ar1(date.f+0|group),data=subset(tweet.vol.lon,repeats!="4"),nbinom2)
vol.glom.lon.sub.7<-glmmTMB(Freq~phase+ar1(date.f+0|group),data=subset(tweet.vol.lon,repeats!="4"),nbinom2)
vol.glom.lon.sub.7b<-glmmTMB(Freq~tavg+phase+ar1(date.f+0|group),data=subset(tweet.vol.lon,repeats!="4"),nbinom2)
AIC(vol.glom.lon.sub.0bd,vol.glom.lon.sub.0bcd,vol.glom.lon.sub.3bcd,vol.glom.lon.sub.6bd,vol.glom.lon.sub.2bd,vol.glom.lon.sub.7bd,vol.glom.lon.sub.0,vol.glom.lon.sub.0b,vol.glom.lon.sub.3,vol.glom.lon.sub.3b, vol.glom.lon.sub.0c,vol.glom.lon.sub.0bc,vol.glom.lon.sub.3c,vol.glom.lon.sub.3bc, vol.glom.lon.sub.6,vol.glom.lon.sub.6b,vol.glom.lon.sub.2,vol.glom.lon.sub.2b,vol.glom.lon.sub.7,vol.glom.lon.sub.7b)


#plot(ggpredict(vol.glom.lon.sub.6b,c("lockdown.phase","repeats","tavg")))

plot(ggpredict(vol.glom.lon.sub.6,c("lockdown.phase","repeats")))


###we need to do model selection for Berlin on the subset)

######Berlin subset
vol.glom.ber.sub.0<-glmmTMB(Freq~lockdown.phase+(1|lockdown.phase:repeats)+ar1(date.f+0|group),data=subset(tweet.vol.ber,repeats!="4"),nbinom2)
vol.glom.ber.sub.0b<-glmmTMB(Freq~tavg+lockdown.phase+(1|lockdown.phase:repeats)+ar1(date.f+0|group),data=subset(tweet.vol.ber,repeats!="4"),nbinom2)

vol.glom.ber.sub.0bd<-glmmTMB(Freq~tavg*lockdown.phase+(1|lockdown.phase:repeats)+ar1(date.f+0|group),data=subset(tweet.vol.ber,repeats!="4"),nbinom2)
vol.glom.ber.sub.0bcd<-glmmTMB(Freq~tavg*lockdown.phase+(1|lockdown.phase:repeats)+ar1(date.f+0|lockdown.phase:repeats),data=subset(tweet.vol.ber,repeats!="4"),nbinom2)
vol.glom.ber.sub.3bcd<-glmmTMB(Freq~tavg*(1|lockdown.phase:repeats)+ar1(date.f+0|lockdown.phase:repeats),data=subset(tweet.vol.ber,repeats!="4"),nbinom2)
vol.glom.ber.sub.6bd<-glmmTMB(Freq~tavg*lockdown.phase*repeats+ar1(date.f+0|group),data=subset(tweet.vol.ber,repeats!="4"),nbinom2)
vol.glom.ber.sub.2bd<-glmmTMB(Freq~tavg*lockdown.phase+repeats+ar1(date.f+0|group),data=subset(tweet.vol.ber,repeats!="4"),nbinom2)
vol.glom.ber.sub.7bd<-glmmTMB(Freq~tavg*phase+ar1(date.f+0|group),data=subset(tweet.vol.ber,repeats!="4"),nbinom2)

vol.glom.ber.sub.3<-glmmTMB(Freq~1+(1|lockdown.phase:repeats)+ar1(date.f+0|group),data=subset(tweet.vol.ber,repeats!="4"),nbinom2)
vol.glom.ber.sub.3b<-glmmTMB(Freq~tavg+(1|lockdown.phase:repeats)+ar1(date.f+0|group),data=subset(tweet.vol.ber,repeats!="4"),nbinom2)

vol.glom.ber.sub.0c<-glmmTMB(Freq~lockdown.phase+(1|lockdown.phase:repeats)+ar1(date.f+0|lockdown.phase:repeats),data=subset(tweet.vol.ber,repeats!="4"),nbinom2)
vol.glom.ber.sub.0bc<-glmmTMB(Freq~tavg+lockdown.phase+(1|lockdown.phase:repeats)+ar1(date.f+0|lockdown.phase:repeats),data=subset(tweet.vol.ber,repeats!="4"),nbinom2)
vol.glom.ber.sub.3c<-glmmTMB(Freq~1+(1|lockdown.phase:repeats)+ar1(date.f+0|lockdown.phase:repeats),data=subset(tweet.vol.ber,repeats!="4"),nbinom2)
vol.glom.ber.sub.3bc<-glmmTMB(Freq~tavg+(1|lockdown.phase:repeats)+ar1(date.f+0|lockdown.phase:repeats),data=subset(tweet.vol.ber,repeats!="4"),nbinom2)

vol.glom.ber.sub.6<-glmmTMB(Freq~lockdown.phase*repeats+ar1(date.f+0|group),data=subset(tweet.vol.ber,repeats!="4"),nbinom2)
vol.glom.ber.sub.6b<-glmmTMB(Freq~tavg+lockdown.phase*repeats+ar1(date.f+0|group),data=subset(tweet.vol.ber,repeats!="4"),nbinom2)
vol.glom.ber.sub.2<-glmmTMB(Freq~lockdown.phase+repeats+ar1(date.f+0|group),data=subset(tweet.vol.ber,repeats!="4"),nbinom2)
vol.glom.ber.sub.2b<-glmmTMB(Freq~tavg+lockdown.phase+repeats+ar1(date.f+0|group),data=subset(tweet.vol.ber,repeats!="4"),nbinom2)
vol.glom.ber.sub.7<-glmmTMB(Freq~phase+ar1(date.f+0|group),data=subset(tweet.vol.ber,repeats!="4"),nbinom2)
vol.glom.ber.sub.7b<-glmmTMB(Freq~tavg+phase+ar1(date.f+0|group),data=subset(tweet.vol.ber,repeats!="4"),nbinom2)
AIC(vol.glom.ber.sub.0bd,vol.glom.ber.sub.0bcd,vol.glom.ber.sub.3bcd,vol.glom.ber.sub.6bd,vol.glom.ber.sub.2bd,vol.glom.ber.sub.7bd,vol.glom.ber.sub.0,vol.glom.ber.sub.0b,vol.glom.ber.sub.3,vol.glom.ber.sub.3b, vol.glom.ber.sub.0c,vol.glom.ber.sub.0bc,vol.glom.ber.sub.3c,vol.glom.ber.sub.3bc, vol.glom.ber.sub.6,vol.glom.ber.sub.6b,vol.glom.ber.sub.2,vol.glom.ber.sub.2b,vol.glom.ber.sub.7,vol.glom.ber.sub.7b)

#plot(ggpredict(vol.glom.ber.sub.2b,c("lockdown.phase","repeats","tavg")))

plot(ggpredict(vol.glom.ber.sub.6,c("lockdown.phase","repeats")))

###we need to do model selection for Paris on the subset)

######Paris subset
vol.glom.par.sub.0<-glmmTMB(Freq~lockdown.phase+(1|lockdown.phase:repeats)+ar1(date.f+0|group),data=subset(tweet.vol.par,repeats!="3"&lockdown.phase!="1"),nbinom2)
vol.glom.par.sub.0b<-glmmTMB(Freq~tavg+lockdown.phase+(1|lockdown.phase:repeats)+ar1(date.f+0|group),data=subset(tweet.vol.par,repeats!="3"&lockdown.phase!="1"),nbinom2)

vol.glom.par.sub.0bd<-glmmTMB(Freq~tavg*lockdown.phase+(1|lockdown.phase:repeats)+ar1(date.f+0|group),data=subset(tweet.vol.par,repeats!="3"&lockdown.phase!="1"),nbinom2)
vol.glom.par.sub.0bcd<-glmmTMB(Freq~tavg*lockdown.phase+(1|lockdown.phase:repeats)+ar1(date.f+0|lockdown.phase:repeats),data=subset(tweet.vol.par,repeats!="3"&lockdown.phase!="1"),nbinom2)
vol.glom.par.sub.3bcd<-glmmTMB(Freq~tavg*(1|lockdown.phase:repeats)+ar1(date.f+0|lockdown.phase:repeats),data=subset(tweet.vol.par,repeats!="3"&lockdown.phase!="1"),nbinom2)
vol.glom.par.sub.6bd<-glmmTMB(Freq~tavg*lockdown.phase*repeats+ar1(date.f+0|group),data=subset(tweet.vol.par,repeats!="3"&lockdown.phase!="1"),nbinom2)
vol.glom.par.sub.2bd<-glmmTMB(Freq~tavg*lockdown.phase+repeats+ar1(date.f+0|group),data=subset(tweet.vol.par,repeats!="3"&lockdown.phase!="1"),nbinom2)
vol.glom.par.sub.7bd<-glmmTMB(Freq~tavg*phase+ar1(date.f+0|group),data=subset(tweet.vol.par,repeats!="3"&lockdown.phase!="1"),nbinom2)

vol.glom.par.sub.3<-glmmTMB(Freq~1+(1|lockdown.phase:repeats)+ar1(date.f+0|group),data=subset(tweet.vol.par,repeats!="3"&lockdown.phase!="1"),nbinom2)
vol.glom.par.sub.3b<-glmmTMB(Freq~tavg+(1|lockdown.phase:repeats)+ar1(date.f+0|group),data=subset(tweet.vol.par,repeats!="3"&lockdown.phase!="1"),nbinom2)

vol.glom.par.sub.0c<-glmmTMB(Freq~lockdown.phase+(1|lockdown.phase:repeats)+ar1(date.f+0|lockdown.phase:repeats),data=subset(tweet.vol.par,repeats!="3"&lockdown.phase!="1"),nbinom2)
vol.glom.par.sub.0bc<-glmmTMB(Freq~tavg+lockdown.phase+(1|lockdown.phase:repeats)+ar1(date.f+0|lockdown.phase:repeats),data=subset(tweet.vol.par,repeats!="3"&lockdown.phase!="1"),nbinom2)
vol.glom.par.sub.3c<-glmmTMB(Freq~1+(1|lockdown.phase:repeats)+ar1(date.f+0|lockdown.phase:repeats),data=subset(tweet.vol.par,repeats!="3"&lockdown.phase!="1"),nbinom2)
vol.glom.par.sub.3bc<-glmmTMB(Freq~tavg+(1|lockdown.phase:repeats)+ar1(date.f+0|lockdown.phase:repeats),data=subset(tweet.vol.par,repeats!="3"&lockdown.phase!="1"),nbinom2)

vol.glom.par.sub.6<-glmmTMB(Freq~lockdown.phase*repeats+ar1(date.f+0|group),data=subset(tweet.vol.par,repeats!="3"&lockdown.phase!="1"),nbinom2)
vol.glom.par.sub.6b<-glmmTMB(Freq~tavg+lockdown.phase*repeats+ar1(date.f+0|group),data=subset(tweet.vol.par,repeats!="3"&lockdown.phase!="1"),nbinom2)
vol.glom.par.sub.2<-glmmTMB(Freq~lockdown.phase+repeats+ar1(date.f+0|group),data=subset(tweet.vol.par,repeats!="3"&lockdown.phase!="1"),nbinom2)
vol.glom.par.sub.2b<-glmmTMB(Freq~tavg+lockdown.phase+repeats+ar1(date.f+0|group),data=subset(tweet.vol.par,repeats!="3"&lockdown.phase!="1"),nbinom2)
vol.glom.par.sub.7<-glmmTMB(Freq~phase+ar1(date.f+0|group),data=subset(tweet.vol.par,repeats!="3"&lockdown.phase!="1"),nbinom2)
vol.glom.par.sub.7b<-glmmTMB(Freq~tavg+phase+ar1(date.f+0|group),data=subset(tweet.vol.par,repeats!="3"&lockdown.phase!="1"),nbinom2)
AIC(vol.glom.par.sub.0bd,vol.glom.par.sub.0bcd,vol.glom.par.sub.3bcd,vol.glom.par.sub.6bd,vol.glom.par.sub.2bd,vol.glom.par.sub.7bd,vol.glom.par.sub.0,vol.glom.par.sub.0b,vol.glom.par.sub.3,vol.glom.par.sub.3b, vol.glom.par.sub.0c,vol.glom.par.sub.0bc,vol.glom.par.sub.3c,vol.glom.par.sub.3bc, vol.glom.par.sub.6,vol.glom.par.sub.6b,vol.glom.par.sub.2,vol.glom.par.sub.2b,vol.glom.par.sub.7,vol.glom.par.sub.7b)


plot(ggpredict(vol.glom.par.sub.2,c("repeats","lockdown.phase")))
