## assemble paris movement data and check whether we have movement among or between FID

# the resolution is 2 zoom level away from the population density data (we go from 400 x 400 m to 1600 x 1600 m)
# at that scale there is much less unusual movement, that's interesting in itself

par.mov.files<-list.files("C:/Users/David/Documents/Facebook COVID/Facebook Paris/movement",full.names=TRUE)

#ParisIMD and Parisgreenspace are both crs 4326

par.mov<-read.csv(par.mov.files[1],header=T) #we are having some issues with 'duplicated rownames' from a misread of the linestring data

for (i in 476:length(par.mov.files)) {
par.it<-read.csv(par.mov.files[i],header=T)
par.mov<-rbind(par.mov,par.it)

}

par.mov.files[i]

save(par.mov,file="C:/Users/David/Documents/Facebook COVID/paris_mvt.Rdata")

###########
earth_circumference_m<-6378137*2*pi              
earth_circumference<-360 
##################


par.mov$start_tile_size_lat<-(cos(par.mov$start_lat * pi/180) * earth_circumference)/(2^(nchar(as.character(par.mov$start_quadkey))))
par.mov$start_tile_size_lon<-(cos(par.mov$start_lon * pi/180) * earth_circumference)/(2^(nchar(as.character(par.mov$start_quadkey))))
par.mov$start_lon.top<-par.mov$start_lon+(par.mov$start_tile_size_lon/2)
par.mov$start_lon.bot<-par.mov$start_lon-(par.mov$start_tile_size_lon/2)
par.mov$start_lat.top<-par.mov$start_lat+(par.mov$start_tile_size_lat/2)
par.mov$start_lat.bot<-par.mov$start_lat-(par.mov$start_tile_size_lat/2)


par.mov$end_tile_size_lat<-(cos(par.mov$end_lat * pi/180) * earth_circumference)/(2^(nchar(as.character(par.mov$end_quadkey))))
par.mov$end_tile_size_lon<-(cos(par.mov$end_lon * pi/180) * earth_circumference)/(2^(nchar(as.character(par.mov$end_quadkey))))
par.mov$end_lon.top<-par.mov$end_lon+(par.mov$end_tile_size_lon/2)
par.mov$end_lon.bot<-par.mov$end_lon-(par.mov$end_tile_size_lon/2)
par.mov$end_lat.top<-par.mov$end_lat+(par.mov$end_tile_size_lat/2)
par.mov$end_lat.bot<-par.mov$end_lat-(par.mov$end_tile_size_lat/2)


par.mov$dated<-factor(substr(par.mov$date_time,1,10))
par.mov$time<-factor(substr(par.mov$date_time,12,16))

par.mov$dates<-as.POSIXct(par.mov$dated, format="%Y-%m-%d")


lockdown<-read.csv("C:/Users/David/Documents/Facebook COVID/lockdown_indicators/OxCGRT_latest_May_2022.txt",header=T)
lockdown$date.pos<-as.Date(as.character(lockdown$Date),"%Y%m%d")

table(lockdown$RegionName[lockdown$CountryCode=="FRA"])

lockdown.paris<-lockdown[lockdown$CountryCode=="FRA",c(17,62)]

lockdown.paris[is.na(lockdown.paris)]<-0 # posthoc confirmation

deprivation_folder<-"C:/Users/David/Documents/deprivation_indices"
greenspace_folder<-"C:/Users/David/Documents/greenspace"

par.mov$lockdown<-0
par.mov$lockdown<-lockdown.paris$C6_Stay.at.home.requirements[match(as.Date(par.mov$dates),lockdown.paris$date.pos)] #much faster than merge
par.mov$lockdown<-factor(par.mov$lockdown)


save(par.mov,file="C:/Users/David/Documents/Facebook COVID/paris_mvt.Rdata")


gardens.quadkey.df<-data.frame(quadkey=unique(c(unique(par.mov$start_quadkey),unique(par.mov$end_quadkey))),gardens=NA,coverage=NA,FDep=NA)

gardens.quadkey.df$start_lon.top<-par.mov$start_lon.top[match(gardens.quadkey.df$quadkey,par.mov$start_quadkey)]
gardens.quadkey.df$start_lon.bot<-par.mov$start_lon.bot[match(gardens.quadkey.df$quadkey,par.mov$start_quadkey)]
gardens.quadkey.df$start_lat.top<-par.mov$start_lat.top[match(gardens.quadkey.df$quadkey,par.mov$start_quadkey)]
gardens.quadkey.df$start_lat.bot<-par.mov$start_lat.bot[match(gardens.quadkey.df$quadkey,par.mov$start_quadkey)]


gardens.quadkey.df$start_lon.top[is.na(gardens.quadkey.df$start_lon.top)]<-par.mov$end_lon.top[match(gardens.quadkey.df$quadkey[is.na(gardens.quadkey.df$start_lon.top)],par.mov$end_quadkey)]
gardens.quadkey.df$start_lon.bot[is.na(gardens.quadkey.df$start_lon.bot)]<-par.mov$end_lon.bot[match(gardens.quadkey.df$quadkey[is.na(gardens.quadkey.df$start_lon.bot)],par.mov$end_quadkey)]
gardens.quadkey.df$start_lat.top[is.na(gardens.quadkey.df$start_lat.top)]<-par.mov$end_lat.top[match(gardens.quadkey.df$quadkey[is.na(gardens.quadkey.df$start_lat.top)],par.mov$end_quadkey)]
gardens.quadkey.df$start_lat.bot[is.na(gardens.quadkey.df$start_lat.bot)]<-par.mov$end_lat.bot[match(gardens.quadkey.df$quadkey[is.na(gardens.quadkey.df$start_lat.bot)],par.mov$end_quadkey)]

gardens.quadkey.df$polygon<-NA
for (i in 1:dim(gardens.quadkey.df)[1]) {
gardens.quadkey.df$polygon[i]<-st_sfc(st_polygon(list(rbind(c(gardens.quadkey.df$start_lon.top[i],gardens.quadkey.df$start_lat.top[i]),c(gardens.quadkey.df$start_lon.bot[i],gardens.quadkey.df$start_lat.top[i]),c(gardens.quadkey.df$start_lon.bot[i],gardens.quadkey.df$start_lat.bot[i]),c(gardens.quadkey.df$start_lon.top[i],gardens.quadkey.df$start_lat.bot[i]),c(gardens.quadkey.df$start_lon.top[i],gardens.quadkey.df$start_lat.top[i])))))

}

gardens.quadkey.df.map<-st_sf(gardens.quadkey.df,crs=4326)

gardens.quadkey.df.map$coverage_area<-NA

load("C:/Users/David/Documents/Facebook COVID/paris_map.Rdata")

for (i in 1:dim(gardens.quadkey.df.map)[1]) {
#match in ber.mov and get geometry
try.inter<-st_intersection(paris.map,gardens.quadkey.df.map[i,])

if (length(try.inter$quadkey)>0) {
gardens.quadkey.df.map$gardens[i]<-sum(try.inter$gardens,na.rm=TRUE)
gardens.quadkey.df.map$coverage[i]<-sum(try.inter$coverage,na.rm=TRUE)
gardens.quadkey.df.map$FDep[i]<-median(try.inter$FDep,na.rm=TRUE)
gardens.quadkey.df.map$coverage_area[i]<-gardens.quadkey.df.map$coverage[i]/st_area(gardens.quadkey.df.map[i,])
}
#calculate metrics
}
save(par.mov,gardens.quadkey.df.map,file="C:/Users/David/Documents/Facebook COVID/paris_mvt.Rdata") 


iter_start<-match(par.mov$start_quadkey,gardens.quadkey.df.map$quadkey)
iter_end<-match(par.mov$end_quadkey,gardens.quadkey.df.map$quadkey)

par.mov$start_greenspace_coverage<-gardens.quadkey.df.map$coverage_area[iter_start]
par.mov$end_greenspace_coverage<-gardens.quadkey.df.map$coverage_area[iter_end]

par.mov$start_FDep<-gardens.quadkey.df.map$FDep[iter_start]
par.mov$end_FDep<-gardens.quadkey.df.map$FDep[iter_end]

par.mov$weekday<-weekdays(par.mov$dates)
par.mov$weekend<-"no"
par.mov$weekend[par.mov$weekday=="Saturday"]<-"yes"
par.mov$weekend[par.mov$weekday=="Sunday"]<-"yes"
par.mov$weekend<-factor(par.mov$weekend)
par.mov$weekday<-factor(par.mov$weekday)
par.mov$start_quadkeys<-factor(par.mov$start_quadkey)
par.mov$end_quadkeys<-factor(par.mov$end_quadkey)


load("C:/Users/David/Documents/Facebook COVID/Twitter/tweet.volume.df.all.Rdata")

par.mov$repeats<-tweet.vol.par$repeats[match(as.Date(par.mov$dates),tweet.vol.par$date)]
rm(tweet.vol.lon,tweet.vol.ber,tweet.vol.par)

save(par.mov,gardens.quadkey.df.map,file="C:/Users/David/Documents/Facebook COVID/paris_mvt.Rdata") 


par.mov.08<-subset(par.mov,time=="08:00")
par.mov.08$start_FDep.f<-factor(ceiling(par.mov.08$start_FDep))
par.mov.08$end_FDep.f<-factor(ceiling(par.mov.08$end_FDep))


par.mov.08.lockdown<-subset(par.mov.08,!is.na(start_FDep)&!is.na(end_FDep)&lockdown==2)
par.mov.08.lockdown$repeats<-factor(par.mov.08.lockdown$repeats)

library(lme4)
lme.cover1<-lmer(z_score~start_greenspace_coverage*end_greenspace_coverage*weekend*repeats+(1|dated)+(1|start_quadkeys),data=par.mov.08.lockdown,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
lme.cover2<-lmer(z_score~start_FDep*end_FDep*weekend*repeats+(1|dated)+(1|start_quadkeys),data=par.mov.08.lockdown,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
lme.cover3<-lmer(z_score~start_FDep*end_FDep*end_greenspace_coverage*weekend*repeats+(1|dated)+(1|start_quadkeys),data=par.mov.08.lockdown,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
lme.cover4<-lmer(z_score~start_FDep*start_greenspace_coverage*end_FDep*end_greenspace_coverage*weekend*repeats+(1|dated)+(1|start_quadkeys),data=par.mov.08.lockdown,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
AIC(lme.cover1,lme.cover2,lme.cover3,lme.cover4)

library(ggeffects)


Paris.mvt.effect<-ggpredict(lme.cover4,terms=c("end_greenspace_coverage","start_FDep","weekend","repeats [2]"))
plot(Paris.mvt.effect)

threshold<-as.numeric(quantile(gardens.quadkey.df.map$coverage_area,na.rm=T,.5))
par.mov.08.lockdown.rich<-subset(par.mov.08.lockdown,end_greenspace_coverage>threshold&repeats=="2")
#let's look when we know parks were opened

lme.cover0<-lmer(z_score~start_FDep*end_FDep*weekend+(1|dated)+(1|start_quadkeys),data=par.mov.08.lockdown.rich,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
lme.cover01<-lmer(z_score~start_FDep*start_greenspace_coverage*end_FDep*weekend+(1|dated)+(1|start_quadkeys),data=par.mov.08.lockdown.rich,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))


Paris.mvt.effect<-ggpredict(lme.cover0,terms=c("end_FDep","start_FDep [-2,0,2]","weekend"))
plot(Paris.mvt.effect)


Paris.mvt.effect<-ggpredict(lme.cover01,terms=c("end_FDep","start_FDep [-2,0,2]","start_greenspace_coverage [0,0.05,0.1]","weekend [yes]"))
plot(Paris.mvt.effect)

par.mov.08.lockdown2<-subset(par.mov.08.lockdown,repeats=="2")
lme.cover1<-lmer(z_score~start_greenspace_coverage*end_greenspace_coverage*weekend+(1|dated)+(1|start_quadkeys),data=par.mov.08.lockdown2,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
lme.cover2<-lmer(z_score~start_FDep*end_FDep*weekend+(1|dated)+(1|start_quadkeys),data=par.mov.08.lockdown2,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
lme.cover3<-lmer(z_score~start_FDep*end_FDep*end_greenspace_coverage*weekend+(1|dated)+(1|start_quadkeys),data=par.mov.08.lockdown2,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
lme.cover4<-lmer(z_score~start_FDep*start_greenspace_coverage*end_FDep*end_greenspace_coverage*weekend+(1|dated)+(1|start_quadkeys),data=par.mov.08.lockdown2,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
AIC(lme.cover1,lme.cover2,lme.cover3,lme.cover4)

library(car)
Anova(lme.cover4)


Paris.mvt.effect<-ggpredict(lme.cover4,terms=c("end_FDep [-2,0,2]","start_FDep [-2,0,2]","start_greenspace_coverage [0]","end_greenspace_coverage [.3]"))
plot(Paris.mvt.effect)


library(ggplot2)
################################
#### keep that plot for the 3 cities to supplementary #need to do it at the population tile level across the 3 cities
gardens.quadkey.df.map$FDep.f<-"deprived"
gardens.quadkey.df.map$FDep.f[gardens.quadkey.df.map$FDep>(-2) & gardens.quadkey.df.map$FDep<=(-.5)]<-"challenged"
gardens.quadkey.df.map$FDep.f[gardens.quadkey.df.map$FDep>(-.5) & gardens.quadkey.df.map$FDep<=(.5)]<-"average"
gardens.quadkey.df.map$FDep.f[gardens.quadkey.df.map$FDep>(.5)]<-"affluent"
gardens.quadkey.df.map$FDep.f<-factor(gardens.quadkey.df.map$FDep.f)

gardens.quadkey.df.map$FDep.bin<-"deprived"
gardens.quadkey.df.map$FDep.bin[gardens.quadkey.df.map$FDep>=0]<-"affluent"
gardens.quadkey.df.map$FDep.bin<-factor(gardens.quadkey.df.map$FDep.bin)


coverage.dist.paris<-ggplot(gardens.quadkey.df.map, aes(x=coverage_area,fill=FDep.bin))+
geom_density(alpha=0.5,adjust=1.5) #+ here change legend title and match colours to other graphs


#reversed situation to Berlin

