## assemble london movement data and check whether we have movement among or between IMD

library(sf)

lon.mov.files<-list.files("C:/Users/David/Documents/Facebook COVID/Facebook London/movement",full.names=TRUE)

#ParisIMD and Parisgreenspace are both crs 4326

lon.mov<-read.csv(lon.mov.files[1],header=T) #we are having some issues with 'duplicated rownames' from a misread of the linestring data

for (i in 2:length(lon.mov.files)) {
lon.it<-read.csv(lon.mov.files[i],header=T)
lon.mov<-rbind(lon.mov,lon.it)

}

save(lon.mov,file="C:/Users/David/Documents/Facebook COVID/london_mvt.Rdata")

lon.mov$dated<-factor(substr(lon.mov$date_time,1,10))
lon.mov$time<-factor(substr(lon.mov$date_time,12,16))
lon.mov<-subset(lon.mov,time=="08:00")

lon.mov$dates<-as.POSIXct(lon.mov$dated, format="%Y-%m-%d")
lon.mov$start_quadkeys<-factor(lon.mov$start_quadkey)

save(lon.mov,file="C:/Users/David/Documents/Facebook COVID/london_mvt08.Rdata")

###########
earth_circumference_m<-6378137*2*pi              
earth_circumference<-360 
##################


lon.mov$start_tile_size_lat<-(cos(lon.mov$start_lat * pi/180) * earth_circumference)/(2^(nchar(as.character(lon.mov$start_quadkey))))
lon.mov$start_tile_size_lon<-(cos(lon.mov$start_lon * pi/180) * earth_circumference)/(2^(nchar(as.character(lon.mov$start_quadkey))))
lon.mov$start_lon.top<-lon.mov$start_lon+(lon.mov$start_tile_size_lon/2)
lon.mov$start_lon.bot<-lon.mov$start_lon-(lon.mov$start_tile_size_lon/2)
lon.mov$start_lat.top<-lon.mov$start_lat+(lon.mov$start_tile_size_lat/2)
lon.mov$start_lat.bot<-lon.mov$start_lat-(lon.mov$start_tile_size_lat/2)


lon.mov$end_tile_size_lat<-(cos(lon.mov$end_lat * pi/180) * earth_circumference)/(2^(nchar(as.character(lon.mov$end_quadkey))))
lon.mov$end_tile_size_lon<-(cos(lon.mov$end_lon * pi/180) * earth_circumference)/(2^(nchar(as.character(lon.mov$end_quadkey))))
lon.mov$end_lon.top<-lon.mov$end_lon+(lon.mov$end_tile_size_lon/2)
lon.mov$end_lon.bot<-lon.mov$end_lon-(lon.mov$end_tile_size_lon/2)
lon.mov$end_lat.top<-lon.mov$end_lat+(lon.mov$end_tile_size_lat/2)
lon.mov$end_lat.bot<-lon.mov$end_lat-(lon.mov$end_tile_size_lat/2)



lockdown<-read.csv("C:/Users/David/Documents/Facebook COVID/lockdown_indicators/OxCGRT_latest_May_2022.txt",header=T)
lockdown$date.pos<-as.Date(as.character(lockdown$Date),"%Y%m%d")

table(lockdown$RegionName[lockdown$CountryCode=="GBR"])
table(lockdown$RegionName[lockdown$RegionName=="England"])

lockdown.london<-lockdown[lockdown$RegionName=="England",c(17,62)]

lockdown.london[is.na(lockdown.london)]<-0 # posthoc confirmation

deprivation_folder<-"C:/Users/David/Documents/deprivation_indices"
greenspace_folder<-"C:/Users/David/Documents/greenspace"

lon.mov$lockdown<-0
lon.mov$lockdown<-lockdown.london$C6_Stay.at.home.requirements[match(as.Date(lon.mov$dates),lockdown.london$date.pos)] #much faster than merge
lon.mov$lockdown<-factor(lon.mov$lockdown)


save(lon.mov,file="C:/Users/David/Documents/Facebook COVID/london_mvt08.Rdata")


gardens.quadkey.df<-data.frame(quadkey=unique(c(unique(lon.mov$start_quadkey),unique(lon.mov$end_quadkey))),gardens=NA,coverage=NA,IMD=NA)

gardens.quadkey.df$start_lon.top<-lon.mov$start_lon.top[match(gardens.quadkey.df$quadkey,lon.mov$start_quadkey)]
gardens.quadkey.df$start_lon.bot<-lon.mov$start_lon.bot[match(gardens.quadkey.df$quadkey,lon.mov$start_quadkey)]
gardens.quadkey.df$start_lat.top<-lon.mov$start_lat.top[match(gardens.quadkey.df$quadkey,lon.mov$start_quadkey)]
gardens.quadkey.df$start_lat.bot<-lon.mov$start_lat.bot[match(gardens.quadkey.df$quadkey,lon.mov$start_quadkey)]


gardens.quadkey.df$start_lon.top[is.na(gardens.quadkey.df$start_lon.top)]<-lon.mov$end_lon.top[match(gardens.quadkey.df$quadkey[is.na(gardens.quadkey.df$start_lon.top)],lon.mov$end_quadkey)]
gardens.quadkey.df$start_lon.bot[is.na(gardens.quadkey.df$start_lon.bot)]<-lon.mov$end_lon.bot[match(gardens.quadkey.df$quadkey[is.na(gardens.quadkey.df$start_lon.bot)],lon.mov$end_quadkey)]
gardens.quadkey.df$start_lat.top[is.na(gardens.quadkey.df$start_lat.top)]<-lon.mov$end_lat.top[match(gardens.quadkey.df$quadkey[is.na(gardens.quadkey.df$start_lat.top)],lon.mov$end_quadkey)]
gardens.quadkey.df$start_lat.bot[is.na(gardens.quadkey.df$start_lat.bot)]<-lon.mov$end_lat.bot[match(gardens.quadkey.df$quadkey[is.na(gardens.quadkey.df$start_lat.bot)],lon.mov$end_quadkey)]

gardens.quadkey.df$polygon<-NA
for (i in 1:dim(gardens.quadkey.df)[1]) {
gardens.quadkey.df$polygon[i]<-st_sfc(st_polygon(list(rbind(c(gardens.quadkey.df$start_lon.top[i],gardens.quadkey.df$start_lat.top[i]),c(gardens.quadkey.df$start_lon.bot[i],gardens.quadkey.df$start_lat.top[i]),c(gardens.quadkey.df$start_lon.bot[i],gardens.quadkey.df$start_lat.bot[i]),c(gardens.quadkey.df$start_lon.top[i],gardens.quadkey.df$start_lat.bot[i]),c(gardens.quadkey.df$start_lon.top[i],gardens.quadkey.df$start_lat.top[i])))))

}

gardens.quadkey.df.map<-st_sf(gardens.quadkey.df,crs=4326)

gardens.quadkey.df.map$coverage_area<-NA

load("C:/Users/David/Documents/Facebook COVID/london_map.Rdata")

london.map.district<-st_crop(gardens.quadkey.df.map,st_bbox(london.map))

#let's cut the rectangle to london's zone to remove lots of tiles

for (i in 1:dim(london.map.district)[1]) {
#match in ber.mov and get geometry
try.inter<-st_intersection(london.map,london.map.district[i,])

if (length(try.inter$quadkey)>0) {
london.map.district$gardens[i]<-sum(try.inter$gardens,na.rm=TRUE)
london.map.district$coverage[i]<-sum(try.inter$coverage,na.rm=TRUE)
london.map.district$IMD[i]<-median(try.inter$IMD,na.rm=TRUE)
london.map.district$coverage_area[i]<-london.map.district$coverage[i]/st_area(london.map.district[i,])
}
#calculate metrics
}
save(lon.mov,london.map.district,gardens.quadkey.df.map,file="C:/Users/David/Documents/Facebook COVID/london_mvt08.Rdata") 


iter_start<-match(lon.mov$start_quadkey,london.map.district$quadkey)
iter_end<-match(lon.mov$end_quadkey,london.map.district$quadkey)

lon.mov$start_greenspace_coverage<-london.map.district$coverage_area[iter_start]
lon.mov$end_greenspace_coverage<-london.map.district$coverage_area[iter_end]

lon.mov$start_IMD<-london.map.district$IMD[iter_start]
lon.mov$end_IMD<-london.map.district$IMD[iter_end]

lon.mov$weekday<-weekdays(lon.mov$dates)
lon.mov$weekend<-"no"
lon.mov$weekend[lon.mov$weekday=="Saturday"]<-"yes"
lon.mov$weekend[lon.mov$weekday=="Sunday"]<-"yes"
lon.mov$weekend<-factor(lon.mov$weekend)
lon.mov$weekday<-factor(lon.mov$weekday)
lon.mov$start_quadkeys<-factor(lon.mov$start_quadkey)
lon.mov$end_quadkeys<-factor(lon.mov$end_quadkey)


load("C:/Users/David/Documents/Facebook COVID/Twitter/tweet.volume.df.all.Rdata")

lon.mov$repeats<-tweet.vol.lon$repeats[match(as.Date(lon.mov$dates),tweet.vol.lon$date)]
rm(tweet.vol.lon,tweet.vol.ber,tweet.vol.par)

save(lon.mov,london.map.district,gardens.quadkey.df.map,file="C:/Users/David/Documents/Facebook COVID/london_mvt08.Rdata") 


lon.mov.08.lockdown<-subset(lon.mov,!is.na(start_IMD)&!is.na(end_IMD)&lockdown==2)
lon.mov.08.lockdown$repeats<-factor(lon.mov.08.lockdown$repeats)
lon.mov.08.lockdown$start_IMDs<-lon.mov.08.lockdown$start_IMD/100
lon.mov.08.lockdown$end_IMDs<-lon.mov.08.lockdown$end_IMD/100

library(lme4)
library(ggeffects)
library(car)

lon.mov.08.lockdown2<-subset(lon.mov.08.lockdown,repeats=="3")
lon.mov.08.lockdown2$end_greenspace_coverage[which(lon.mov.08.lockdown2$end_greenspace_coverage>1)]<-1
lon.mov.08.lockdown2$start_greenspace_coverage[which(lon.mov.08.lockdown2$start_greenspace_coverage>1)]<-1

lme.cover1<-lmer(z_score~start_greenspace_coverage*end_greenspace_coverage*weekend+(1|dated)+(1|start_quadkeys),data=lon.mov.08.lockdown2,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
lme.cover2<-lmer(z_score~start_IMDs*end_IMDs*weekend+(1|dated)+(1|start_quadkeys),data=lon.mov.08.lockdown2,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
lme.cover3<-lmer(z_score~start_IMDs*end_IMDs*end_greenspace_coverage*weekend+(1|dated)+(1|start_quadkeys),data=lon.mov.08.lockdown2,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
lme.cover4<-lmer(z_score~start_IMDs*start_greenspace_coverage*end_IMDs*end_greenspace_coverage*weekend+(1|dated)+(1|start_quadkeys),data=lon.mov.08.lockdown2,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
AIC(lme.cover1,lme.cover2,lme.cover3,lme.cover4)

Anova(lme.cover4)

quantile(lon.mov.08.lockdown2$end_greenspace_coverage)

lme.cover4b<-lmer(z_score~start_IMDs*start_greenspace_coverage*end_IMDs*end_greenspace_coverage+(1|dated)+(1|start_quadkeys),data=subset(lon.mov.08.lockdown2,weekend=="yes"),control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))

London.mvt.effect<-ggpredict(lme.cover4b,terms=c("end_IMDs [0.055,0.175,0.355]","start_IMDs [0.055,0.175,0.355]","start_greenspace_coverage [0]","end_greenspace_coverage [.5]"))
plot(London.mvt.effect)



lon.mov.08$start_IMD.f<-factor(ceiling(lon.mov.08$start_IMD))
lon.mov.08$end_IMD.f<-factor(ceiling(lon.mov.08$end_IMD))

