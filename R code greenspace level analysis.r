#########################R code
### re-intersect greenspace locations with tiles to find which tiles are in each greenspace#####then ask the question at the greenspace level: 
###tile nested in greenspace only for tiles belonging to greenspace

### we will then assemble the three cities in one data.frame

##Paris first, easier as smaller set
lockdown<-read.csv("C:/Users/David/Documents/Facebook COVID/lockdown_indicators/OxCGRT_latest_Aug_30_2021.txt",header=T)
lockdown$date.pos<-as.Date(as.character(lockdown$Date),"%Y%m%d")

table(lockdown$RegionName[lockdown$CountryCode=="FRA"])

lockdown.paris<-lockdown[lockdown$CountryCode=="FRA",c(17,52)]

lockdown.paris[is.na(lockdown.paris)]<-0 # posthoc confirmation

deprivation_folder<-"C:/Users/David/Documents/deprivation_indices"
greenspace_folder<-"C:/Users/David/Documents/greenspace"

library(sf)

Parisgreenspace<-st_read(paste0(greenspace_folder,"/Paris-greenspace/espaces_verts.shp"))
library(geojsonsf)

###########lat and lon quad formation
###########
earth_circumference_m<-6378137*2*pi              
earth_circumference<-360 
##################


# par.pop$tile_size_lat<-(cos(par.pop$lat * pi/180) * earth_circumference)/(2^(nchar(as.character(par.pop$quadkey))))
# par.pop$tile_size_lon<-(cos(par.pop$lon * pi/180) * earth_circumference)/(2^(nchar(as.character(par.pop$quadkey))))
# par.pop$lon.top<-par.pop$lon+(par.pop$tile_size_lon/2)
# par.pop$lon.bot<-par.pop$lon-(par.pop$tile_size_lon/2)
# par.pop$lat.top<-par.pop$lat+(par.pop$tile_size_lat/2)
# par.pop$lat.bot<-par.pop$lat-(par.pop$tile_size_lat/2)

load("C:/Users/David/Documents/Facebook COVID/paris_corona_greenspace_Aug_2021.Rdata")

green.quadkey<-gardens.quadkey.df[gardens.quadkey.df$coverage>0,]

green.quadkey$lat<-par.pop$lat[match(green.quadkey$quadkey,par.pop$quadkey)]
green.quadkey$lon<-par.pop$lon[match(green.quadkey$quadkey,par.pop$quadkey)]
green.quadkey$lat.top<-par.pop$lat.top[match(green.quadkey$quadkey,par.pop$quadkey)]
green.quadkey$lon.top<-par.pop$lon.top[match(green.quadkey$quadkey,par.pop$quadkey)]
green.quadkey$lat.bot<-par.pop$lat.bot[match(green.quadkey$quadkey,par.pop$quadkey)]
green.quadkey$lon.bot<-par.pop$lon.bot[match(green.quadkey$quadkey,par.pop$quadkey)]
green.quadkey$coverage_area<-par.pop$coverage_area[match(green.quadkey$quadkey,par.pop$quadkey)]
library(sf)

green.quadkey$polygon<-NA
for (i in 1:dim(green.quadkey)[1]) {
green.quadkey$polygon[i]<-st_sfc(st_polygon(list(rbind(c(green.quadkey$lon.top[i],green.quadkey$lat.top[i]),c(green.quadkey$lon.bot[i],green.quadkey$lat.top[i]),c(green.quadkey$lon.bot[i],green.quadkey$lat.bot[i]),c(green.quadkey$lon.top[i],green.quadkey$lat.bot[i]),c(green.quadkey$lon.top[i],green.quadkey$lat.top[i])))))

}


green.quadkey.map<-st_sf(green.quadkey,crs=4326)


gardens.quadkey.df$lat<-par.pop$lat[match(gardens.quadkey.df$quadkey,par.pop$quadkey)]
gardens.quadkey.df$lon<-par.pop$lon[match(gardens.quadkey.df$quadkey,par.pop$quadkey)]
gardens.quadkey.df$lat.top<-par.pop$lat.top[match(gardens.quadkey.df$quadkey,par.pop$quadkey)]
gardens.quadkey.df$lon.top<-par.pop$lon.top[match(gardens.quadkey.df$quadkey,par.pop$quadkey)]
gardens.quadkey.df$lat.bot<-par.pop$lat.bot[match(gardens.quadkey.df$quadkey,par.pop$quadkey)]
gardens.quadkey.df$lon.bot<-par.pop$lon.bot[match(gardens.quadkey.df$quadkey,par.pop$quadkey)]
gardens.quadkey.df$coverage_area<-par.pop$coverage_area[match(gardens.quadkey.df$quadkey,par.pop$quadkey)]
library(sf)

gardens.quadkey.df$polygon<-NA
for (i in 1:dim(gardens.quadkey.df)[1]) {
gardens.quadkey.df$polygon[i]<-st_sfc(st_polygon(list(rbind(c(gardens.quadkey.df$lon.top[i],gardens.quadkey.df$lat.top[i]),c(gardens.quadkey.df$lon.bot[i],gardens.quadkey.df$lat.top[i]),c(gardens.quadkey.df$lon.bot[i],gardens.quadkey.df$lat.bot[i]),c(gardens.quadkey.df$lon.top[i],gardens.quadkey.df$lat.bot[i]),c(gardens.quadkey.df$lon.top[i],gardens.quadkey.df$lat.top[i])))))

}


gardens.quadkey.df.map<-st_sf(gardens.quadkey.df,crs=4326)

Parisgreenspace.sub<-subset(Parisgreenspace,type_ev!="Cimetières")

for (i in 1:dim(gardens.quadkey.df.map)[1]) {

try.inter<-st_intersection(Parisgreenspace.sub,gardens.quadkey.df.map[i,])

if (length(try.inter$nom_ev)>0) {
gardens.quadkey.df.map$gardens[i]<-length(try.inter$nom_ev)
gardens.quadkey.df.map$coverage[i]<-as.numeric(sum(st_area(try.inter)))

}  else {

gardens.quadkey.df.map$gardens[i]<-0
gardens.quadkey.df.map$coverage[i]<-0
} #ifelse intersect
print(i)
flush.console()
}
gardens.quadkey.df.map$tile_size_lat<-par.pop$tile_size_lat[match(gardens.quadkey.df.map$quadkey,par.pop$quadkey)]
gardens.quadkey.df.map$tile_size_lon<-par.pop$tile_size_lon[match(gardens.quadkey.df.map$quadkey,par.pop$quadkey)]

gardens.quadkey.df.map$coverage_area<-gardens.quadkey.df.map$coverage/((gardens.quadkey.df.map$tile_size_lat*earth_circumference_m/360)*(gardens.quadkey.df.map$tile_size_lon*earth_circumference_m/360))

green.quadkey.map<-gardens.quadkey.df.map[gardens.quadkey.df.map$coverage_area>0,]

quantile(green.quadkey.map$coverage_area,.9)


###ok now we want to know about greenspace, so we have many tiles that contain multiple greenspace polygons because the tiles are much bigger than the greenspace
### those are useful in the previous analyses but does not tell us exactly about the greenspace use.
##so we are going to limit to 
plot(Parisgreenspace["type_ev"])
windows()
plot(green.quadkey.map["gardens"])
paris.map<-st_sf(Parisgreenspace,crs=4326)
plot(paris.map["type_ev"])
hist(green.quadkey.map$coverage_area)

plot(green.quadkey.map["gardens"][green.quadkey.map$coverage_area>quantile(green.quadkey.map$coverage_area,.9),])

#let's retain greenspace rich tiles, more than 90% quantile
paris.green.quadkey<-green.quadkey.map$quadkey[green.quadkey.map$coverage_area>quantile(green.quadkey$coverage_area,.9)]

Paris.green<-par.pop[!is.na(match(par.pop$quadkey,paris.green.quadkey)),]
Paris.green$city<-"Paris"

save(gardens.quadkey.df.map,Paris.green,file="C:/Users/David/Documents/Facebook COVID/greenspace_level_analyses/Paris_green_pop_df.Rdata")

##cut to the bbox of FDep


##############################################################################################################################
###############################################################################################################################
####BERLIN


library(sf)
library(geojsonsf)
Berlingreenspace<-geojson_sf(paste0(greenspace_folder,"/Berlingreenspace.geojson"))

Berlingreenspace.map<-st_sf(Berlingreenspace,crs=4326)

load("C:/Users/David/Documents/Facebook COVID/berlin_corona_greenspace_Aug_2021.Rdata")


earth_circumference_m<-6378137*2*pi              
earth_circumference<-360 
##################

gardens.quadkey.df$lat<-ber.pop$lat[match(gardens.quadkey.df$quadkey,ber.pop$quadkey)]
gardens.quadkey.df$lon<-ber.pop$lon[match(gardens.quadkey.df$quadkey,ber.pop$quadkey)]
gardens.quadkey.df$tile_size_lat<-(cos(gardens.quadkey.df$lat * pi/180) * earth_circumference)/(2^(nchar(as.character(gardens.quadkey.df$quadkey))))
gardens.quadkey.df$tile_size_lon<-(cos(gardens.quadkey.df$lon * pi/180) * earth_circumference)/(2^(nchar(as.character(gardens.quadkey.df$quadkey))))
gardens.quadkey.df$lon.top<-gardens.quadkey.df$lon+(gardens.quadkey.df$tile_size_lon/2)
gardens.quadkey.df$lon.bot<-gardens.quadkey.df$lon-(gardens.quadkey.df$tile_size_lon/2)
gardens.quadkey.df$lat.top<-gardens.quadkey.df$lat+(gardens.quadkey.df$tile_size_lat/2)
gardens.quadkey.df$lat.bot<-gardens.quadkey.df$lat-(gardens.quadkey.df$tile_size_lat/2)


gardens.quadkey.df$polygon<-NA
for (i in 1:dim(gardens.quadkey.df)[1]) {
gardens.quadkey.df$polygon[i]<-st_sfc(st_polygon(list(rbind(c(gardens.quadkey.df$lon.top[i],gardens.quadkey.df$lat.top[i]),c(gardens.quadkey.df$lon.bot[i],gardens.quadkey.df$lat.top[i]),c(gardens.quadkey.df$lon.bot[i],gardens.quadkey.df$lat.bot[i]),c(gardens.quadkey.df$lon.top[i],gardens.quadkey.df$lat.bot[i]),c(gardens.quadkey.df$lon.top[i],gardens.quadkey.df$lat.top[i])))))

}

gardens.quadkey.df.map<-st_sf(gardens.quadkey.df,crs=4326)


for (i in 1:dim(gardens.quadkey.df.map)[1]) {

try.inter<-st_intersection(Berlingreenspace,gardens.quadkey.df.map[i,])

if (length(try.inter$namenr)>0) {
gardens.quadkey.df.map$gardens[i]<-length(try.inter$namenr)
gardens.quadkey.df.map$coverage[i]<-as.numeric(sum(st_area(try.inter)))

}  else {

gardens.quadkey.df.map$gardens[i]<-0
gardens.quadkey.df.map$coverage[i]<-0
} #ifelse intersect

}

gardens.quadkey.df.map$coverage_area<-gardens.quadkey.df.map$coverage/((gardens.quadkey.df.map$tile_size_lat*earth_circumference_m/360)*(gardens.quadkey.df.map$tile_size_lon*earth_circumference_m/360))


green.quadkey<-gardens.quadkey.df.map[gardens.quadkey.df.map$coverage>0,]

berlin.green.quadkey<-green.quadkey$quadkey[green.quadkey$coverage_area>quantile(green.quadkey$coverage_area,.9)]

plot(green.quadkey["coverage_area"][green.quadkey$coverage_area>quantile(green.quadkey$coverage_area,.9),])


Berlin.green<-ber.pop[!is.na(match(ber.pop$quadkey,berlin.green.quadkey)),]
Berlin.green$city<-"Berlin"

save(gardens.quadkey.df.map,Berlin.green,file="C:/Users/David/Documents/Facebook COVID/greenspace_level_analyses/Berlin_green_pop_df.Rdata")

##################################################################################

##cut to the bbox of MSS
#this will be particularly important for London
##actually greenspace bbox might just do it


##############################################################################################################################
###############################################################################################################################
####LONDON



load("C:/Users/David/Documents/Facebook COVID/london_corona_greenspace_Aug_2021.Rdata")

lockdown<-read.csv("C:/Users/David/Documents/Facebook COVID/lockdown_indicators/OxCGRT_latest_Aug_30_2021.txt",header=T)

lockdown.london<-lockdown[lockdown$RegionName=="England",c(17,52)]
library(sf)
deprivation_folder<-"C:/Users/David/Documents/deprivation_indices"
greenspace_folder<-"C:/Users/David/Documents/greenspace"

UKgreenspace<-st_read(paste0(greenspace_folder,"/GB-greenspace/data/GB_GreenspaceSite.shp"))

UKgreenspace.map<-st_transform(UKgreenspace,crs=4326)

UKgreenspace.map<-subset(UKgreenspace.map,function.!="Cemetery")


#we need to clip to the M26 circle
LSOA<-st_read("C:/Users/David/Documents/deprivation_indices/london/statistical-gis-boundaries-london/ESRI/LSOA_2011_London_gen_MHW.shp")

LSOA.map<-st_transform(LSOA,crs=4326)

london_bound<-aggregate(LSOA.map,by=list(v=rep(1,nrow(LSOA.map))),FUN=max)

UKgreenspace.map.clip<-st_intersection(UKgreenspace.map,london_bound)




earth_circumference_m<-6378137*2*pi              
earth_circumference<-360 
##################

gardens.quadkey.df$lat<-lon.pop$lat[match(gardens.quadkey.df$quadkey,lon.pop$quadkey)]
gardens.quadkey.df$lon<-lon.pop$lon[match(gardens.quadkey.df$quadkey,lon.pop$quadkey)]
gardens.quadkey.df$tile_size_lat<-(cos(gardens.quadkey.df$lat * pi/180) * earth_circumference)/(2^(nchar(as.character(gardens.quadkey.df$quadkey))))
gardens.quadkey.df$tile_size_lon<-(cos(gardens.quadkey.df$lon * pi/180) * earth_circumference)/(2^(nchar(as.character(gardens.quadkey.df$quadkey))))
gardens.quadkey.df$lon.top<-gardens.quadkey.df$lon+(gardens.quadkey.df$tile_size_lon/2)
gardens.quadkey.df$lon.bot<-gardens.quadkey.df$lon-(gardens.quadkey.df$tile_size_lon/2)
gardens.quadkey.df$lat.top<-gardens.quadkey.df$lat+(gardens.quadkey.df$tile_size_lat/2)
gardens.quadkey.df$lat.bot<-gardens.quadkey.df$lat-(gardens.quadkey.df$tile_size_lat/2)


gardens.quadkey.df$polygon<-NA
for (i in 1:dim(gardens.quadkey.df)[1]) {
gardens.quadkey.df$polygon[i]<-st_sfc(st_polygon(list(rbind(c(gardens.quadkey.df$lon.top[i],gardens.quadkey.df$lat.top[i]),c(gardens.quadkey.df$lon.bot[i],gardens.quadkey.df$lat.top[i]),c(gardens.quadkey.df$lon.bot[i],gardens.quadkey.df$lat.bot[i]),c(gardens.quadkey.df$lon.top[i],gardens.quadkey.df$lat.bot[i]),c(gardens.quadkey.df$lon.top[i],gardens.quadkey.df$lat.top[i])))))

}

gardens.quadkey.df.map<-st_sf(gardens.quadkey.df,crs=4326)


gardens.quadkey.df.map.clip<-st_intersection(gardens.quadkey.df.map,london_bound)

#################
gardens.quadkey.df.map.clip<-gardens.quadkey.df.map.clip[,c(1:11,27)]


for (i in 1001:dim(gardens.quadkey.df.map.clip)[1]) {

try.inter<-st_intersection(UKgreenspace.map.clip,gardens.quadkey.df.map.clip[i,])

if (length(try.inter$id)>0) {
gardens.quadkey.df.map.clip$gardens[i]<-length(try.inter$id)
gardens.quadkey.df.map.clip$coverage[i]<-tryCatch(as.numeric(sum(st_area(try.inter))),error=function(e) gardens.quadkey.df.map.clip$coverage[i]) # the intersection broke some polygons

}  else {

gardens.quadkey.df.map.clip$gardens[i]<-0
gardens.quadkey.df.map.clip$coverage[i]<-0
} #ifelse intersect

print(i)
flush.console()
}

gardens.quadkey.df.map.clip$coverage_area<-gardens.quadkey.df.map.clip$coverage/((gardens.quadkey.df.map.clip$tile_size_lat*earth_circumference_m/360)*(gardens.quadkey.df.map.clip$tile_size_lon*earth_circumference_m/360))
gardens.quadkey.df.map.clip$coverage_area[which(gardens.quadkey.df.map.clip$coverage_area>1)]<-1

green.quadkey<-gardens.quadkey.df.map.clip[gardens.quadkey.df.map.clip$coverage>0,]

london.green.quadkey<-green.quadkey$quadkey[green.quadkey$coverage_area>quantile(green.quadkey$coverage_area,.9)]

plot(green.quadkey["coverage_area"][(green.quadkey$coverage_area>quantile(green.quadkey$coverage_area,.9))&(nchar(green.quadkey$quadkey)==16),])


London.green<-lon.pop[!is.na(match(lon.pop$quadkey,london.green.quadkey)),]
London.green$city<-"London"

save(gardens.quadkey.df.map.clip,London.green,file="C:/Users/David/Documents/Facebook COVID/greenspace_level_analyses/London_green_pop_df.Rdata")


##############################################################################################################################################################
##############################################################################################################################################################
##############################################################################################################################################################
##############################################################################################################################################################
##############################################################################################################################################################
##################bring it in together

library(sf)
library(lme4)
load("C:/Users/David/Documents/Facebook COVID/greenspace_level_analyses/London_green_pop_df.Rdata")
loondon.quadkey.df<-gardens.quadkey.df.map.clip
rm(gardens.quadkey.df.map.clip)
load("C:/Users/David/Documents/Facebook COVID/greenspace_level_analyses/Berlin_green_pop_df.Rdata")
berlin.quadkey.df<-gardens.quadkey.df.map
rm(gardens.quadkey.df.map)
load("C:/Users/David/Documents/Facebook COVID/greenspace_level_analyses/Paris_green_pop_df.Rdata")
paris.quadkey.df<-gardens.quadkey.df.map
rm(gardens.quadkey.df.map)

London.green$coverage_area<-loondon.quadkey.df$coverage_area[match(London.green$quadkey,loondon.quadkey.df$quadkey)]
Berlin.green$coverage_area<-berlin.quadkey.df$coverage_area[match(Berlin.green$quadkey,berlin.quadkey.df$quadkey)]
Paris.green$coverage_area<-paris.quadkey.df$coverage_area[match(Paris.green$quadkey,paris.quadkey.df$quadkey)]

##################
#missing london lockdown

lockdown<-read.csv("C:/Users/David/Documents/Facebook COVID/lockdown_indicators/OxCGRT_latest_Aug_30_2021.txt",header=T)
lockdown$date.pos<-as.Date(as.character(lockdown$Date),"%Y%m%d")
lockdown.london<-lockdown[lockdown$RegionName=="England",c(17,52)]

London.green$lockdown<-0
London.green$lockdown<-lockdown.london$C6_Stay_at_home_requirements[match(London.green$dates,lockdown.london$date.pos)] #much faster than merge
London.green$lockdown<-factor(London.green$lockdown)


all.green<-rbind(London.green[,c(1,2,3,12,19,20,21,22,23,24,27,26)],Berlin.green[,c(1,2,3,12,19,20,21,22,23,24,25,26)],Paris.green[,c(1,2,3,12,20,21,22,23,24,25,26,27)])

all.green$city<-factor(all.green$city)

all.green$weekday<-weekdays(all.green$dates)
all.green$weekend<-"no"
all.green$weekend[all.green$weekday=="Saturday"]<-"yes"
all.green$weekend[all.green$weekday=="Sunday"]<-"yes"
all.green$weekend<-factor(all.green$weekend)
all.green$weekday<-factor(all.green$weekday)

all.green$time[all.green$time=="00:0"]<-"0000"
all.green$time[all.green$time=="08:0"]<-"0800"
all.green$time[all.green$time=="16:0"]<-"1600"
all.green$time<-factor(as.character(all.green$time))
all.green$quadkeys<-factor(all.green$quadkey)

all.green.day<-subset(all.green,time=="0800")

library(lme4)

allcity.lme1<-lmer(clipped_z_score~lockdown*weekend*city + (1|city:dated) + (1|city:quadkeys),data=all.green.day,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))

library(ggplot2)
library(ggeffects)
plot(ggpredict(allcity.lme1,terms=c("lockdown","weekend","city")))

#############################################################################
######repeats


load("C:/Users/David/Documents/Facebook COVID/Twitter/tweet.volume.df.all.Rdata")
London.green$lockdown[is.na(London.green$lockdown)]<-unique(London.green$lockdown[London.green$dates==(unique(London.green$dates[is.na(London.green$lockdown)])-1)])
Paris.green$lockdown[is.na(Paris.green$lockdown)]<-unique(Paris.green$lockdown[Paris.green$dates==(unique(Paris.green$dates[is.na(Paris.green$lockdown)])-1)])
Berlin.green$lockdown[is.na(Berlin.green$lockdown)]<-unique(Berlin.green$lockdown[Berlin.green$dates==(unique(Berlin.green$dates[is.na(Berlin.green$lockdown)])-1)])

Paris.green$repeats<-tweet.vol.par$repeats[match(as.Date(Paris.green$dates),tweet.vol.par$date)]
Berlin.green$repeats<-tweet.vol.ber$repeats[match(as.Date(Berlin.green$dates),tweet.vol.ber$date)]
London.green$repeats<-tweet.vol.lon$repeats[match(as.Date(London.green$dates),tweet.vol.lon$date)]
rm(tweet.vol.lon,tweet.vol.ber,tweet.vol.par)

Paris.green$repeats<-as.numeric(Paris.green$repeats)
Paris.green$repeats[Paris.green$lockdown=="0"]<-Paris.green$repeats[Paris.green$lockdown=="0"]-1 #we start in March
Paris.green$repeats<-factor(Paris.green$repeats)

Berlin.green$repeats<-as.numeric(Berlin.green$repeats)
Berlin.green$repeats[Berlin.green$lockdown=="0"]<-Berlin.green$repeats[Berlin.green$lockdown=="0"]-1 #we start in March
Berlin.green$repeats[Berlin.green$lockdown=="1"]<-Berlin.green$repeats[Berlin.green$lockdown=="1"]-1 #we start in March
Berlin.green<-subset(Berlin.green,repeats!="4")
Berlin.green$repeats<-factor(Berlin.green$repeats)

London.green$repeats<-as.numeric(London.green$repeats)
London.green$repeats[London.green$lockdown=="0"]<-London.green$repeats[London.green$lockdown=="0"]-1 #we start in March
London.green<-subset(London.green,repeats!="4")
London.green$repeats<-factor(London.green$repeats)


all.green<-rbind(London.green[,c(1,2,3,12,19,20,21,22,23,24,27,26,28)],Berlin.green[,c(1,2,3,12,19,20,21,22,23,24,25,26,27)],Paris.green[,c(1,2,3,12,20,21,22,23,24,25,26,27,28)])

all.green$city<-factor(all.green$city)

all.green$weekday<-weekdays(all.green$dates)
all.green$weekend<-"no"
all.green$weekend[all.green$weekday=="Saturday"]<-"yes"
all.green$weekend[all.green$weekday=="Sunday"]<-"yes"
all.green$weekend<-factor(all.green$weekend)
all.green$weekday<-factor(all.green$weekday)

all.green$time[all.green$time=="00:0"]<-"0000"
all.green$time[all.green$time=="08:0"]<-"0800"
all.green$time[all.green$time=="16:0"]<-"1600"
all.green$time<-factor(as.character(all.green$time))
all.green$quadkeys<-factor(all.green$quadkey)

all.green.day<-subset(all.green,time=="0800")


allcity.lme1<-lmer(clipped_z_score~lockdown*weekend*city+ lockdown*city*repeats + (1|city:dated) + (1|city:quadkeys),data=all.green.day,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))

library(glmmTMB)
allcity.lme1<-glmmTMB(clipped_z_score~lockdown*weekend*city+ lockdown*city*repeats + (1|city:dated) + (1|city:quadkeys),data=all.green.day)


all.city.lockdown<-subset(all.green.day,lockdown=="2")
#contrast lockdown 1 and others
all.city.lockdown$repeats<-as.numeric(all.city.lockdown$repeats)
all.city.lockdown$repeats[all.city.lockdown$repeats==3]<-2
all.city.lockdown$repeats<-factor(all.city.lockdown$repeats)

allcity.lme1<-lmer(clipped_z_score~weekend  + (1|city:dated) + (1|city:quadkeys),data=all.city.lockdown,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
allcity.lme2<-lmer(clipped_z_score~weekend*city  + (1|city:dated) + (1|city:quadkeys),data=all.city.lockdown,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
allcity.lme3<-lmer(clipped_z_score~repeats*city  + (1|city:dated) + (1|city:quadkeys),data=all.city.lockdown,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
allcity.lme4<-lmer(clipped_z_score~weekend*city  + (1|city:dated) + (1|city:quadkeys),data=all.city.lockdown,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
allcity.lme5<-lmer(clipped_z_score~repeats*weekend*city  + (1|city:dated) + (1|city:quadkeys),data=all.city.lockdown,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
allcity.lme6<-lmer(clipped_z_score~repeats*city +weekend  + (1|city:dated) + (1|city:quadkeys),data=all.city.lockdown,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))

AIC(allcity.lme1,allcity.lme2,allcity.lme3,allcity.lme4,allcity.lme5,allcity.lme6)

             # df      AIC
# allcity.lme1  5 735986.0
# allcity.lme2  9 736000.0
# allcity.lme3  9 735766.1*
# allcity.lme4  9 736000.0
# allcity.lme5 15 735785.7
# allcity.lme6 10 735772.6
daic<-c(735986.0,736000.0,735766.1,735772.6,735785.7)-min(c(735986.0,736000.0,735766.1,735772.6,735785.7))
library(car)
Anova(allcity.lme3)

# Analysis of Deviance Table (Type II Wald chisquare tests)

# Response: clipped_z_score
                # Chisq Df Pr(>Chisq)    
# repeats      178.9834  1     <2e-16 ***
# city           0.3839  2     0.8253    
# repeats:city 115.5373  2     <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

summary(allcity.lme3)

# Random effects:
 # Groups        Name        Variance Std.Dev.
 # city:quadkeys (Intercept) 1.3384   1.1569  
 # city:dated    (Intercept) 0.1702   0.4126  
 # Residual                  1.2522   1.1190  
# Number of obs: 237261, groups:  city:quadkeys, 1312; city:dated, 639



plot(ggpredict(allcity.lme3,terms=c("repeats","city")))


pred.all.together<-ggpredict(allcity.lme3,terms=c("city","repeats"))
pred.plot<-plot(pred.all.together)+
			labs(color="lockdown wave",title="",tag="a.")+
			xlab("")+
			ylab("Predicted Z-score (Z08)")+
			theme_classic()+
			geom_hline(yintercept=0)+
			theme(legend.position=c(.8,.9))
			
			

# theme_set(theme_classic())
# plot1<-plot(pred.all.together,show.y.title=FALSE,show.x.title=FALSE,show.title=FALSE,use.theme=FALSE)
# #need to reverse the color scale as 1 is high is 4 is low in Berlin
# plot1$labels$y<-"Predicted Z-score (Z08)"
# plot1$labels$colour<-"wave"

unique.quad<-all.city.lockdown[!duplicated(all.city.lockdown$quadkey),]
unique.quad$size<-nchar(unique.quad$quadkey)

cover.rich.plot<-ggplot(unique.quad,aes(x=city,y=coverage_area))+
					geom_boxplot(outlier.shape = NA)+
					xlab("")+
					ylab("proportion of greenspace in tiles")+
					labs(tag="b.")+
					theme_classic()+
					theme(legend.position="none")
					


library(grid)
library(gridExtra)
grid.arrange(pred.plot,cover.rich.plot,ncol=2,nrow=1,widths=c(.75,.25))


tiff(file="C:/Users/David/OneDrive - Danmarks Tekniske Universitet/Facebook Greenspace ms/material for submission/Figure all cities greenspace rich tiles lockdown.tiff",width=20,height=10,res=200,units="cm")
grid.arrange(pred.plot,cover.rich.plot,ncol=2,nrow=1,widths=c(.6,.4))
dev.off()

