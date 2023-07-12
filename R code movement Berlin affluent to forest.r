###############################################################
###### this is code to carry out the posthoc analysis
###### for Berlin trying to understand whether the difference in greenspace use
###### in neighbourhoods of different deprivation level compared to London and Paris
###### is associated with an overuse of 'greenspace' for which a deprivation level is not known
###### by people coming from affluent tiles
#################################################################

####let's have a look at what the movement data looks like


ber.mov.files<-list.files("C:/Users/David/Documents/Facebook COVID/Facebook Berlin/movement",full.names=TRUE)

#ParisIMD and Parisgreenspace are both crs 4326

ber.mov<-read.csv(ber.mov.files[1],header=T)

for (i in 2:length(ber.mov.files)) {
ber.it<-read.csv(ber.mov.files[i],header=T)
ber.mov<-rbind(ber.mov,ber.it)

}

save(ber.mov,file="C:/Users/David/Documents/Facebook COVID/berlin_mvt.Rdata")

###########
earth_circumference_m<-6378137*2*pi              
earth_circumference<-360 
##################


ber.mov$start_tile_size_lat<-(cos(ber.mov$start_lat * pi/180) * earth_circumference)/(2^(nchar(as.character(ber.mov$start_quadkey))))
ber.mov$start_tile_size_lon<-(cos(ber.mov$start_lon * pi/180) * earth_circumference)/(2^(nchar(as.character(ber.mov$start_quadkey))))
ber.mov$start_lon.top<-ber.mov$start_lon+(ber.mov$start_tile_size_lon/2)
ber.mov$start_lon.bot<-ber.mov$start_lon-(ber.mov$start_tile_size_lon/2)
ber.mov$start_lat.top<-ber.mov$start_lat+(ber.mov$start_tile_size_lat/2)
ber.mov$start_lat.bot<-ber.mov$start_lat-(ber.mov$start_tile_size_lat/2)


ber.mov$end_tile_size_lat<-(cos(ber.mov$end_lat * pi/180) * earth_circumference)/(2^(nchar(as.character(ber.mov$end_quadkey))))
ber.mov$end_tile_size_lon<-(cos(ber.mov$end_lon * pi/180) * earth_circumference)/(2^(nchar(as.character(ber.mov$end_quadkey))))
ber.mov$end_lon.top<-ber.mov$end_lon+(ber.mov$end_tile_size_lon/2)
ber.mov$end_lon.bot<-ber.mov$end_lon-(ber.mov$end_tile_size_lon/2)
ber.mov$end_lat.top<-ber.mov$end_lat+(ber.mov$end_tile_size_lat/2)
ber.mov$end_lat.bot<-ber.mov$end_lat-(ber.mov$end_tile_size_lat/2)


ber.mov$dated<-factor(substr(ber.mov$date_time,1,10))
ber.mov$time<-factor(substr(ber.mov$date_time,12,16))

ber.mov$dates<-as.POSIXct(ber.mov$dated, format="%Y-%m-%d")



lockdown<-read.csv("C:/Users/David/Documents/Facebook COVID/lockdown_indicators/OxCGRT_latest_Aug_30_2021.txt",header=T)
lockdown$date.pos<-as.Date(as.character(lockdown$Date),"%Y%m%d")

lockdown.berlin<-lockdown[lockdown$CountryCode=="DEU",c(17,52)]

lockdown.berlin[is.na(lockdown.berlin)]<-2 # posthoc confirmation

ber.mov$lockdown<-0
ber.mov$lockdown<-lockdown.berlin$C6_Stay_at_home_requirements[match(as.Date(ber.mov$dates),lockdown.berlin$date.pos)] #much faster than merge
ber.mov$lockdown<-factor(ber.mov$lockdown)


save(ber.mov,file="C:/Users/David/Documents/Facebook COVID/berlin_mvt.Rdata")

library(sf)

###############BERLIN
#load("C:/Users/David/Documents/Facebook COVID/berlin_corona_FB_Aug_2021.Rdata")
####  so the tile zoom level is different to ber.pop so we need to redo the sampling


ber.mov$gardens<-NA
ber.mov$coverage<-NA
ber.mov$MSS<-NA
gardens.quadkey.df.map$coverage_area<-NA

gardens.quadkey.df<-data.frame(quadkey=unique(c(unique(ber.mov$start_quadkey),unique(ber.mov$end_quadkey))),gardens=NA,coverage=NA,MSS=NA)

gardens.quadkey.df$start_lon.top<-ber.mov$start_lon.top[match(gardens.quadkey.df$quadkey,ber.mov$start_quadkey)]
gardens.quadkey.df$start_lon.bot<-ber.mov$start_lon.bot[match(gardens.quadkey.df$quadkey,ber.mov$start_quadkey)]
gardens.quadkey.df$start_lat.top<-ber.mov$start_lat.top[match(gardens.quadkey.df$quadkey,ber.mov$start_quadkey)]
gardens.quadkey.df$start_lat.bot<-ber.mov$start_lat.bot[match(gardens.quadkey.df$quadkey,ber.mov$start_quadkey)]


gardens.quadkey.df$start_lon.top[is.na(gardens.quadkey.df$start_lon.top)]<-ber.mov$end_lon.top[match(gardens.quadkey.df$quadkey[is.na(gardens.quadkey.df$start_lon.top)],ber.mov$end_quadkey)]
gardens.quadkey.df$start_lon.bot[is.na(gardens.quadkey.df$start_lon.bot)]<-ber.mov$end_lon.bot[match(gardens.quadkey.df$quadkey[is.na(gardens.quadkey.df$start_lon.bot)],ber.mov$end_quadkey)]
gardens.quadkey.df$start_lat.top[is.na(gardens.quadkey.df$start_lat.top)]<-ber.mov$end_lat.top[match(gardens.quadkey.df$quadkey[is.na(gardens.quadkey.df$start_lat.top)],ber.mov$end_quadkey)]
gardens.quadkey.df$start_lat.bot[is.na(gardens.quadkey.df$start_lat.bot)]<-ber.mov$end_lat.bot[match(gardens.quadkey.df$quadkey[is.na(gardens.quadkey.df$start_lat.bot)],ber.mov$end_quadkey)]


gardens.quadkey.df$polygon<-NA
for (i in 1:dim(gardens.quadkey.df)[1]) {
gardens.quadkey.df$polygon[i]<-st_sfc(st_polygon(list(rbind(c(gardens.quadkey.df$start_lon.top[i],gardens.quadkey.df$start_lat.top[i]),c(gardens.quadkey.df$start_lon.bot[i],gardens.quadkey.df$start_lat.top[i]),c(gardens.quadkey.df$start_lon.bot[i],gardens.quadkey.df$start_lat.bot[i]),c(gardens.quadkey.df$start_lon.top[i],gardens.quadkey.df$start_lat.bot[i]),c(gardens.quadkey.df$start_lon.top[i],gardens.quadkey.df$start_lat.top[i])))))

}

gardens.quadkey.df.map<-st_sf(gardens.quadkey.df,crs=4326)

load("C:/Users/David/Documents/Facebook COVID/berlin_map.Rdata")

for (i in 1:dim(gardens.quadkey.df.map)[1]) {
#match in ber.mov and get geometry
try.inter<-st_intersection(berlin.map,gardens.quadkey.df.map[i,])

if (length(try.inter$quadkey)>0) {
gardens.quadkey.df.map$gardens[i]<-sum(try.inter$gardens,na.rm=TRUE)
gardens.quadkey.df.map$coverage[i]<-sum(try.inter$coverage,na.rm=TRUE)
gardens.quadkey.df.map$MSS[i]<-median(try.inter$MSS,na.rm=TRUE)
gardens.quadkey.df.map$coverage_area[i]<-gardens.quadkey.df.map$coverage[i]/st_area(gardens.quadkey.df.map[i,])
}
#calculate metrics
}
save(ber.mov,gardens.quadkey.df.map,file="C:/Users/David/Documents/Facebook COVID/berlin_mvt.Rdata")

load("C:/Users/David/Documents/Facebook COVID/berlin_mvt.Rdata")
#######################################################################################################
#### we need to add forests which are not greenspace data

###################greenspace is far from simple.....
### thanks https://github.com/patperu/fisbroker_data 
###but really... what is so hard with releasing shp or kmz files??

library(httr)
library(sf)
library(dplyr)

get_X_Y_coordinates <- function(x) {
  sftype <- as.character(sf::st_geometry_type(x, by_geometry = FALSE))
  if(sftype == "POINT") {
    xy <- as.data.frame(sf::st_coordinates(x))
    dplyr::bind_cols(x, xy)
  } else {
    x
  }
}

sf_fisbroker <- function(url) {
    typenames <- basename(url)
    url <- httr::parse_url(url)
    url$query <- list(service = "wfs",
                      version = "2.0.0",
                      request = "GetFeature",
                      srsName = "EPSG:25833",
                      TYPENAMES = typenames)
    request <- httr::build_url(url)
    print(request)
    out <- sf::read_sf(request)
    out <- sf::st_transform(out, 4326)
    out <- get_X_Y_coordinates(out)
    return(out)
}

export_format <- c(
          "geojson", 
          "sqlite"
   )

sf_save <- function(z, fname) {
  ifelse(!dir.exists(fname), dir.create(fname), "Folder exists already")
  ff <- paste(file.path(fname, fname), export_format, sep = ".")
  purrr::walk(ff, ~{ sf::st_write(z, .x, delete_dsn = TRUE)})
  saveRDS(z, paste0(file.path(fname, fname), ".rds"))
}

z <- sf_fisbroker("https://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/wfs_forst_verwalt2014")

z <- z %>%
     mutate(RAUMID = stringr::str_sub(gml_id, 12, 19)) %>%
     select(gml_id, RAUMID, everything()) %>%
     arrange(RAUMID)

dplyr::glimpse(z)
sf_save(z, "Berlinforest")

gardens.quadkey.df.map$forest<-NA
gardens.quadkey.df.map$forest_area<-NA

for (i in 1:dim(gardens.quadkey.df.map)[1]) {
#match in ber.mov and get geometry
try.inter<-st_intersection(z,gardens.quadkey.df.map[i,])

if (length(try.inter$RAUMID)>0) {
gardens.quadkey.df.map$forest[i]<-sum(st_area(try.inter))
gardens.quadkey.df.map$forest_area[i]<-gardens.quadkey.df.map$forest[i]/st_area(gardens.quadkey.df.map[i,])
}
#calculate metrics
}

save(ber.mov,gardens.quadkey.df.map,file="C:/Users/David/Documents/Facebook COVID/berlin_mvt.Rdata")

iter_start<-match(ber.mov$start_quadkey,gardens.quadkey.df.map$quadkey)
iter_end<-match(ber.mov$end_quadkey,gardens.quadkey.df.map$quadkey)

ber.mov$start_forest_coverage<-gardens.quadkey.df.map$forest_area[iter_start]
ber.mov$end_forest_coverage<-gardens.quadkey.df.map$forest_area[iter_end]

ber.mov$start_greenspace_coverage<-gardens.quadkey.df.map$coverage_area[iter_start]
ber.mov$end_greenspace_coverage<-gardens.quadkey.df.map$coverage_area[iter_end]

ber.mov$start_MSS<-gardens.quadkey.df.map$MSS[iter_start]
ber.mov$end_MSS<-gardens.quadkey.df.map$MSS[iter_end]


save(ber.mov,gardens.quadkey.df.map,file="C:/Users/David/Documents/Facebook COVID/berlin_mvt.Rdata")


load(url("https://userpage.fu-berlin.de/soga/300/30100_data_sets/berlin_district.RData"))
plot(berlin.sf, main="")
berlin.cookie.cutter<-st_transform(berlin.sf,crs=4326)

berlin.map.district<-st_intersection(gardens.quadkey.df.map,berlin.cookie.cutter)



ber.mov$weekday<-weekdays(ber.mov$dates)
ber.mov$weekend<-"no"
ber.mov$weekend[ber.mov$weekday=="Saturday"]<-"yes"
ber.mov$weekend[ber.mov$weekday=="Sunday"]<-"yes"
ber.mov$weekend<-factor(ber.mov$weekend)
ber.mov$weekday<-factor(ber.mov$weekday)
ber.mov$start_quadkeys<-factor(ber.mov$start_quadkey)
ber.mov$end_quadkeys<-factor(ber.mov$end_quadkey)

save(ber.mov,gardens.quadkey.df.map,berlin.map.district,file="C:/Users/David/Documents/Facebook COVID/berlin_mvt.Rdata")

#########################################################################################################
##########################################################################################################
#########################################################################################################
##########################################################################################################
#### May 2022 insertion, are people from affluent area going to deprived neighbourhood greenspace
load("C:/Users/David/Documents/Facebook COVID/berlin_mvt.Rdata")
# path updated aug 2022 with new hdd
load("F:/COVID cities/Facebook COVID/berlin_mvt.Rdata")

load("C:/Users/David/Documents/Facebook COVID/Twitter/tweet.volume.df.all.Rdata")
# path updated aug 2022 with new hdd
load("F:/COVID cities/Facebook COVID/Twitter/tweet.volume.df.all.Rdata")

ber.mov$repeats<-tweet.vol.ber$repeats[match(as.Date(ber.mov$dates),tweet.vol.ber$date)]
rm(tweet.vol.lon,tweet.vol.ber,tweet.vol.par)

library(hdrcde)

plot(cde(berlin.map.district$coverage_area[!is.na(berlin.map.district$MSS)],berlin.map.district$MSS[!is.na(berlin.map.district$MSS)]))

quantile(berlin.map.district$coverage_area[berlin.map.district$MSS==1],c(.025,.05,.95,.975),na.rm=T)
quantile(berlin.map.district$coverage_area[berlin.map.district$MSS==2],c(.025,.05,.95,.975),na.rm=T)
quantile(berlin.map.district$coverage_area[berlin.map.district$MSS==3],c(.025,.05,.95,.975),na.rm=T)
quantile(berlin.map.district$coverage_area[berlin.map.district$MSS==4],c(.025,.05,.95,.975),na.rm=T)

ber.mov.08<-subset(ber.mov,time=="08:00")
ber.mov.08$start_MSS.f<-factor(ceiling(ber.mov.08$start_MSS))
ber.mov.08$end_MSS.f<-factor(ceiling(ber.mov.08$end_MSS))
dim(ber.mov.08)

table(ber.mov.08$start_MSS.f,ber.mov.08$end_MSS.f)/rowSums(table(ber.mov.08$start_MSS.f,ber.mov.08$end_MSS.f))

ber.mov.08.lockdown<-subset(ber.mov.08,!is.na(start_MSS)&!is.na(end_MSS)&lockdown==2)

###########################################################################################
#2nd wave
ber.mov.08.lockdown2<-subset(ber.mov.08.lockdown,repeats==2|repeats==3)

library(lme4)
lme.cover1<-lmer(z_score~start_greenspace_coverage*end_greenspace_coverage*weekend+(1|dated)+(1|start_quadkeys),data=ber.mov.08.lockdown2,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
lme.cover2<-lmer(z_score~start_MSS.f*end_MSS.f*weekend+(1|dated)+(1|start_quadkeys),data=ber.mov.08.lockdown2,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
lme.cover3<-lmer(z_score~start_MSS.f*end_MSS.f*end_greenspace_coverage*weekend+(1|dated)+(1|start_quadkeys),data=ber.mov.08.lockdown2,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
lme.cover4<-lmer(z_score~start_MSS.f*start_greenspace_coverage*end_MSS.f*end_greenspace_coverage*weekend+(1|dated)+(1|start_quadkeys),data=ber.mov.08.lockdown2,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
AIC(lme.cover1,lme.cover2,lme.cover3,lme.cover4)

library(ggeffects)
library(car)

quantile(ber.mov.08.lockdown2$end_greenspace_coverage)

Berlin.mvt.effect<-ggpredict(lme.cover4,terms=c("end_MSS.f","start_MSS.f","start_greenspace_coverage [0.06]","end_greenspace_coverage [.065]"))
plot(Berlin.mvt.effect)


###########################################################################################
library(lme4)
lme.cover1<-lmer(z_score~start_greenspace_coverage*end_greenspace_coverage*weekend+(1|dated)+(1|start_quadkeys),data=ber.mov.08.lockdown,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
lme.cover2<-lmer(z_score~start_MSS.f*end_MSS.f*weekend+(1|dated)+(1|start_quadkeys),data=ber.mov.08.lockdown,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
lme.cover3<-lmer(z_score~start_MSS.f*end_MSS.f*end_greenspace_coverage*weekend+(1|dated)+(1|start_quadkeys),data=ber.mov.08.lockdown,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
lme.cover4<-lmer(z_score~start_MSS.f*start_greenspace_coverage*end_MSS.f*end_greenspace_coverage*weekend+(1|dated)+(1|start_quadkeys),data=ber.mov.08.lockdown,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
AIC(lme.cover1,lme.cover2,lme.cover3,lme.cover4)

library(car)
Anova(lme.cover4)


library(ggeffects)

Berlin.mvt.effect<-ggpredict(lme.cover1,terms=c("end_greenspace_coverage [all]","start_greenspace_coverage [.013,.064,.11]","weekend"))
plot(Berlin.mvt.effect)

Berlin.mvt.effect<-ggpredict(lme.cover2,terms=c("start_MSS.f","end_MSS.f","weekend"))
plot(Berlin.mvt.effect)


Berlin.mvt.effect<-ggpredict(lme.cover3,terms=c("end_greenspace_coverage [all]","start_MSS.f","end_MSS.f [4]","weekend"))
plot(Berlin.mvt.effect)

#########################################################################################################
#let's focus on tiles with greenspace
threshold<-as.numeric(quantile(berlin.map.district$coverage_area,na.rm=T,.6))
ber.mov.08.lockdown.rich<-subset(ber.mov.08.lockdown,end_greenspace_coverage>threshold)
lme.cover0<-lmer(z_score~start_MSS.f*end_MSS.f*weekend+(1|dated)+(1|start_quadkeys),data=ber.mov.08.lockdown.rich,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
lme.cover01<-lmer(z_score~start_MSS.f*start_greenspace_coverage*end_MSS.f*weekend+(1|dated)+(1|start_quadkeys),data=ber.mov.08.lockdown.rich,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))


Berlin.mvt.effect<-ggpredict(lme.cover0,terms=c("end_MSS.f","start_MSS.f","weekend"))
plot(Berlin.mvt.effect)


Berlin.mvt.effect<-ggpredict(lme.cover01,terms=c("end_MSS.f","start_greenspace_coverage","weekend","start_MSS.f [1]"))
plot(Berlin.mvt.effect)

#########################################################################################################
#let's focus on deprived area
threshold<-as.numeric(quantile(berlin.map.district$coverage_area,na.rm=T,.5))

ber.mov.08.lockdown.deprive<-subset(ber.mov.08.lockdown,end_MSS.f=="4")

lme.cover1.1<-lmer(z_score~start_MSS.f*end_greenspace_coverage*weekend+(1|dated)+(1|start_quadkeys),data=ber.mov.08.lockdown.deprive,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
lme.cover1.2<-lmer(z_score~start_MSS.f*start_greenspace_coverage*end_greenspace_coverage*weekend+(1|dated)+(1|start_quadkeys),data=ber.mov.08.lockdown.deprive,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
lme.cover1.3<-lmer(z_score~start_MSS.f*weekend+(1|dated)+(1|start_quadkeys),data=ber.mov.08.lockdown.deprive,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
lme.cover1.4<-lmer(z_score~start_MSS.f*start_greenspace_coverage*end_greenspace_coverage+start_MSS.f*weekend+(1|dated)+(1|start_quadkeys),data=ber.mov.08.lockdown.deprive,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
lme.cover1.5<-lmer(z_score~start_MSS.f*start_greenspace_coverage*end_greenspace_coverage+start_MSS.f*end_greenspace_coverage*weekend+(1|dated)+(1|start_quadkeys),data=ber.mov.08.lockdown.deprive,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
lme.cover1.6<-lmer(z_score~start_greenspace_coverage*end_greenspace_coverage*weekend+start_MSS.f*end_greenspace_coverage*weekend+(1|dated)+(1|start_quadkeys),data=ber.mov.08.lockdown.deprive,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))

AIC(lme.cover1.1,lme.cover1.2,lme.cover1.3,lme.cover1.4,lme.cover1.5,lme.cover1.6)


Berlin.mvt.effect<-ggpredict(lme.cover1.2,terms=c("end_greenspace_coverage [0:0.05 by=0.001]","start_MSS.f","start_greenspace_coverage [.005,.03,.05]","weekend"))
plot(Berlin.mvt.effect)


Berlin.mvt.effect<-ggpredict(lme.cover1.1,terms=c("end_greenspace_coverage","start_MSS.f","weekend"))
plot(Berlin.mvt.effect)


#########################################################################################################
#########################################################################################################
#let's focus on deprived area greenspace rich tiles

library(ggplot2)
################################
#### keep that plot for the 3 cities to supplementary
berlin.map.district$MSS.f<-factor(ceiling(berlin.map.district$MSS))

coverage.dist.berlin<-ggplot(berlin.map.district, aes(x=coverage_area,fill=MSS.f))+
geom_density(alpha=0.5,adjust=1.5) #+ here change legend title and match colours to other graphs


threshold<-as.numeric(quantile(berlin.map.district$coverage_area[berlin.map.district$MSS.f=="4"],na.rm=T,.75))

ber.mov.08.lockdown.deprive<-subset(ber.mov.08.lockdown,end_MSS.f=="4"&end_greenspace_coverage>threshold)

lme.cover1.1<-lmer(z_score~start_MSS.f*weekend+(1|dated)+(1|start_quadkeys),data=ber.mov.08.lockdown.deprive,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
lme.cover1.2<-lmer(z_score~start_MSS.f*start_greenspace_coverage*weekend+(1|dated)+(1|start_quadkeys),data=ber.mov.08.lockdown.deprive,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
lme.cover1.3<-lmer(z_score~start_MSS.f*weekend+(1|dated)+(1|start_quadkeys),data=ber.mov.08.lockdown.deprive,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
lme.cover1.4<-lmer(z_score~start_MSS.f*start_greenspace_coverage+start_MSS.f*weekend+(1|dated)+(1|start_quadkeys),data=ber.mov.08.lockdown.deprive,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
lme.cover1.5<-lmer(z_score~start_MSS.f*start_greenspace_coverage+start_greenspace_coverage*weekend+(1|dated)+(1|start_quadkeys),data=ber.mov.08.lockdown.deprive,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
lme.cover1.6<-lmer(z_score~start_greenspace_coverage*weekend+start_MSS.f*weekend+(1|dated)+(1|start_quadkeys),data=ber.mov.08.lockdown.deprive,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
lme.cover1.7<-lmer(z_score~start_MSS.f*start_greenspace_coverage+(1|dated)+(1|start_quadkeys),data=ber.mov.08.lockdown.deprive,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
lme.cover1.8<-lmer(z_score~start_MSS.f*weekend+start_greenspace_coverage+(1|dated)+(1|start_quadkeys),data=ber.mov.08.lockdown.deprive,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
lme.cover1.9<-lmer(z_score~start_MSS.f+(1|dated)+(1|start_quadkeys),data=ber.mov.08.lockdown.deprive,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
AIC(lme.cover1.1,lme.cover1.2,lme.cover1.3,lme.cover1.4,lme.cover1.5,lme.cover1.6,lme.cover1.7,lme.cover1.8,lme.cover1.9)


Berlin.mvt.effect<-ggpredict(lme.cover1.2,terms=c("start_greenspace_coverage","start_MSS.f","weekend"))
plot(Berlin.mvt.effect)


Berlin.mvt.effect<-ggpredict(lme.cover1.1,terms=c("start_MSS.f","weekend"))
plot(Berlin.mvt.effect)

Berlin.mvt.effect<-ggpredict(lme.cover1.7,terms=c("start_MSS.f","start_greenspace_coverage [.005,.03,.05]"))
plot(Berlin.mvt.effect)


#########################################################################################################
########## added Aug 2022
#same for affluent

threshold.af<-as.numeric(quantile(berlin.map.district$coverage_area[berlin.map.district$MSS.f=="1"],na.rm=T,.75))

ber.mov.08.lockdown.aff<-subset(ber.mov.08.lockdown,end_MSS.f=="1"&end_greenspace_coverage>threshold.af)

lme.cover1.1<-lmer(z_score~start_MSS.f*weekend+(1|dated)+(1|start_quadkeys),data=ber.mov.08.lockdown.aff,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
Berlin.mvt.effect<-ggpredict(lme.cover1.1,terms=c("start_MSS.f","weekend"))
plot(Berlin.mvt.effect)


#### so we have cross MSS movement and the results is from the volume trade-off?
by(ber.mov.08.lockdown.aff$z_score,ber.mov.08.lockdown.aff$start_MSS.f,function(x) mean(x,na.rm=T))

##deprived to affluent
ber.mov.08.lockdown.aff$n_baseline[ber.mov.08.lockdown.aff$start_MSS.f=="4"]
#so numbers are too small but we can count edges?
length(unique(ber.mov.08.lockdown.aff$start_quadkeys[ber.mov.08.lockdown.aff$start_MSS.f=="4"]))
#4


##affuent to deprived
ber.mov.08.lockdown.deprive$n_baseline[ber.mov.08.lockdown.deprive$start_MSS.f=="1"]
#so numbers are too small but we can count edges?
length(unique(ber.mov.08.lockdown.deprive$start_quadkeys[ber.mov.08.lockdown.deprive$start_MSS.f=="1"]))
#7






#########################################################################################################
#########################################################################################################


#actually the level at odd with other cities is affluent areas what does end_MSS 1 look like?
ber.mov.08.lockdown.affluent<-subset(ber.mov.08.lockdown,end_MSS.f=="1")
lme.cover1.1<-lmer(z_score~start_MSS.f*end_greenspace_coverage*weekend+(1|dated)+(1|start_quadkeys),data=ber.mov.08.lockdown.affluent,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
lme.cover1.2<-lmer(z_score~start_MSS.f*start_greenspace_coverage*end_greenspace_coverage*weekend+(1|dated)+(1|start_quadkeys),data=ber.mov.08.lockdown.affluent,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
lme.cover1.3<-lmer(z_score~start_MSS.f*weekend+(1|dated)+(1|start_quadkeys),data=ber.mov.08.lockdown.affluent,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
lme.cover1.4<-lmer(z_score~start_MSS.f*start_greenspace_coverage*end_greenspace_coverage+start_MSS.f*weekend+(1|dated)+(1|start_quadkeys),data=ber.mov.08.lockdown.affluent,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
lme.cover1.5<-lmer(z_score~start_MSS.f*start_greenspace_coverage*end_greenspace_coverage+start_MSS.f*end_greenspace_coverage*weekend+(1|dated)+(1|start_quadkeys),data=ber.mov.08.lockdown.affluent,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
lme.cover1.6<-lmer(z_score~start_greenspace_coverage*end_greenspace_coverage*weekend+start_MSS.f*end_greenspace_coverage*weekend+(1|dated)+(1|start_quadkeys),data=ber.mov.08.lockdown.affluent,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))

lme.cover1.1b<-lmer(z_score~start_MSS.f*end_greenspace_coverage+(1|dated)+(1|start_quadkeys),data=ber.mov.08.lockdown.affluent,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))

AIC(lme.cover1.1,lme.cover1.2,lme.cover1.3,lme.cover1.4,lme.cover1.5,lme.cover1.6)


Berlin.mvt.effect<-ggpredict(lme.cover1.2,terms=c("end_greenspace_coverage [0:0.05 by=0.001]","start_MSS.f","start_greenspace_coverage [.005,.03,.05]","weekend"))
plot(Berlin.mvt.effect)

###hat's the question really
Berlin.mvt.effect<-ggpredict(lme.cover1.1b,terms=c("end_greenspace_coverage","start_MSS.f"))
plot(Berlin.mvt.effect)


### looks like we have a huge overrepresentation of "12021023322221" to "12021023322221"
berlin.map.district[berlin.map.district$quadkey=="12021023322221",]

#'deprived' tile, but very high coverage and complex coverage (53 'gardens')

hist(ber.mov.08.lockdown$z_score[ber.mov.08.lockdown$end_quadkey=="12021023322221"&ber.mov.08.lockdown$start_quadkey=="12021023322221"])
table(ber.mov.08.lockdown$start_MSS.f[ber.mov.08.lockdown$end_quadkey=="12021023322221"])
table(ber.mov.08.lockdown$start_MSS.f[ber.mov.08.lockdown$end_MSS.f=="4"])

##################################################
## who is coming there
ber.mov.08.lockdown.downtown<-subset(ber.mov.08.lockdown,end_quadkey=="12021023322221"&start_quadkey!="12021023322221")
lme.cover1.1d<-lmer(z_score~start_MSS.f*weekend+(1|dated)+(1|start_quadkeys),data=ber.mov.08.lockdown.downtown,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
lme.cover1.2d<-lmer(z_score~start_MSS.f*start_greenspace_coverage*weekend+(1|dated)+(1|start_quadkeys),data=ber.mov.08.lockdown.downtown,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
lme.cover1.3d<-lmer(z_score~start_greenspace_coverage*weekend+(1|dated)+(1|start_quadkeys),data=ber.mov.08.lockdown.downtown,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
lme.cover1.4d<-lmer(z_score~start_MSS.f+(1|dated)+(1|start_quadkeys),data=ber.mov.08.lockdown.downtown,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
lme.cover1.5d<-lmer(z_score~start_greenspace_coverage+(1|dated)+(1|start_quadkeys),data=ber.mov.08.lockdown.downtown,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
lme.cover1.6d<-lmer(z_score~start_MSS.f+start_greenspace_coverage*weekend+(1|dated)+(1|start_quadkeys),data=ber.mov.08.lockdown.downtown,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
lme.cover1.7d<-lmer(z_score~start_MSS.f*start_greenspace_coverage+start_greenspace_coverage*weekend+(1|dated)+(1|start_quadkeys),data=ber.mov.08.lockdown.downtown,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
lme.cover1.8d<-lmer(z_score~start_MSS.f*weekend+start_greenspace_coverage*weekend+(1|dated)+(1|start_quadkeys),data=ber.mov.08.lockdown.downtown,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))

AIC(lme.cover1.1d,lme.cover1.2d,lme.cover1.3d,lme.cover1.4d,lme.cover1.5d,lme.cover1.6d,lme.cover1.7d,lme.cover1.8d)


Berlin.mvt.effect<-ggpredict(lme.cover1.6d,terms=c("start_greenspace_coverage","start_MSS.f","weekend"))
plot(Berlin.mvt.effect)

################### that's the question really
Berlin.mvt.effect<-ggpredict(lme.cover1.1d,terms=c("start_MSS.f","weekend"))
plot(Berlin.mvt.effect)

by(ber.mov.08.lockdown.downtown$z_score[ber.mov.08.lockdown.downtown$start_quadkey=="12021023323303"],ber.mov.08.lockdown.downtown$weekend[ber.mov.08.lockdown.downtown$start_quadkey=="12021023323303"],mean)

boxplot(ber.mov.08.lockdown.downtown$z_score[ber.mov.08.lockdown.downtown$start_quadkey=="12021023323303"]~ber.mov.08.lockdown.downtown$weekday[ber.mov.08.lockdown.downtown$start_quadkey=="12021023323303"])

###################################affluent suburb quadkey going to inner city deprived quadkey, is this warping the model?
#greater on Sunday, but high throughout the week too

ber.mov.08.lockdown.downtown.dodge<-subset(ber.mov.08.lockdown.downtown,start_quadkey!="12021023323303")
lme.cover1.1d<-lmer(z_score~start_MSS.f*weekend+(1|dated)+(1|start_quadkeys),data=ber.mov.08.lockdown.downtown.dodge,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
lme.cover1.2d<-lmer(z_score~start_MSS.f*start_greenspace_coverage*weekend+(1|dated)+(1|start_quadkeys),data=ber.mov.08.lockdown.downtown.dodge,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
lme.cover1.3d<-lmer(z_score~start_greenspace_coverage*weekend+(1|dated)+(1|start_quadkeys),data=ber.mov.08.lockdown.downtown.dodge,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
lme.cover1.4d<-lmer(z_score~start_MSS.f+(1|dated)+(1|start_quadkeys),data=ber.mov.08.lockdown.downtown.dodge,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
lme.cover1.5d<-lmer(z_score~start_greenspace_coverage+(1|dated)+(1|start_quadkeys),data=ber.mov.08.lockdown.downtown.dodge,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
lme.cover1.6d<-lmer(z_score~start_MSS.f+start_greenspace_coverage*weekend+(1|dated)+(1|start_quadkeys),data=ber.mov.08.lockdown.downtown.dodge,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
lme.cover1.7d<-lmer(z_score~start_MSS.f*start_greenspace_coverage+start_greenspace_coverage*weekend+(1|dated)+(1|start_quadkeys),data=ber.mov.08.lockdown.downtown.dodge,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
lme.cover1.8d<-lmer(z_score~start_MSS.f*weekend+start_greenspace_coverage*weekend+(1|dated)+(1|start_quadkeys),data=ber.mov.08.lockdown.downtown.dodge,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))

AIC(lme.cover1.1d,lme.cover1.2d,lme.cover1.3d,lme.cover1.4d,lme.cover1.5d,lme.cover1.6d,lme.cover1.7d,lme.cover1.8d)

Berlin.mvt.effect<-ggpredict(lme.cover1.2d,terms=c("start_greenspace_coverage","start_MSS.f","weekend"))
plot(Berlin.mvt.effect)

#
Berlin.mvt.effect<-ggpredict(lme.cover1.1d,terms=c("start_MSS.f","weekend"))
plot(Berlin.mvt.effect)
#no, not a problem


#########################################################################################################
##########################################################################################################
#########################################################################################################
##########################################################################################################
##ok so do we have more travellers from affluent areas to forests during lockdown?
## during the day

ber.mov.08<-subset(ber.mov,time=="08:00")
ber.mov.08$start_MSS.f<-factor(ceiling(ber.mov.08$start_MSS))
ber.mov.08$end_MSS.f<-factor(ceiling(ber.mov.08$end_MSS))
dim(ber.mov.08)

table(ber.mov.08$start_MSS.f,ber.mov.08$end_MSS.f)/rowSums(table(ber.mov.08$start_MSS.f,ber.mov.08$end_MSS.f))


library(lme4)
#forest rich areas
ber.mov.08.rich.lockdown<-subset(ber.mov.08,end_forest_coverage>.6&lockdown==2)

lme.rich<-lmer(z_score~start_MSS.f*weekend+(1|dated)+(1|start_quadkeys),data=ber.mov.08.rich.lockdown,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))


library(ggeffects)

Berlin.forest.mvt.effect<-ggpredict(lme.rich,terms=c("start_MSS.f [1,2,3,4]","weekend"))
plot(Berlin.forest.mvt.effect)


# Random effects:
 # Groups         Name        Variance Std.Dev.
 # dated          (Intercept) 0.04193  0.2048  
 # start_quadkeys (Intercept) 1.89383  1.3762  
 # Residual                   2.30447  1.5180  
# Number of obs: 59867, groups:  dated, 524; start_quadkeys, 444

 # 232263.3


# lme.forest<-lmer(z_score~end_forest_coverage*start_MSS.f*weekend+(1|dated)+(1|start_quadkeys),data=ber.mov.08,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))

# lme.forest3<-lmer(z_score~end_forest_coverage*weekend+(1|dated)+(1|start_quadkeys),data=subset(ber.mov.08,is.na(start_MSS.f)==FALSE),control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))

# lme.forest4<-lmer(z_score~end_forest_coverage+(1|dated)+(1|start_quadkeys),data=subset(ber.mov.08,is.na(start_MSS.f)==FALSE),control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))

# AIC(lme.forest,lme.forest2,lme.forest3,lme.forest4)

ber.mov.08$wave<-"1"
ber.mov.08$wave[ber.mov.08$dates>"2020-09-01"]<-"2"
ber.mov.08$wave<-factor(ber.mov.08$wave)

lme.forest1<-lmer(z_score~end_forest_coverage +weekend+start_MSS.f+(1|dated)+(1|start_quadkeys),data=subset(ber.mov.08,lockdown==2&is.na(start_MSS.f)==FALSE),control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
lme.forest2<-lmer(z_score~end_forest_coverage*start_MSS.f*weekend+(1|dated)+(1|start_quadkeys),data=subset(ber.mov.08,lockdown==2&is.na(start_MSS.f)==FALSE),control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
lme.forest3<-lmer(z_score~end_forest_coverage*weekend+(1|dated)+(1|start_quadkeys),data=subset(ber.mov.08,lockdown==2&is.na(start_MSS.f)==FALSE),control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
lme.forest4<-lmer(z_score~end_forest_coverage+(1|dated)+(1|start_quadkeys),data=subset(ber.mov.08,lockdown==2&is.na(start_MSS.f)==FALSE),control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
lme.forest5<-lmer(z_score~end_forest_coverage*weekend+start_MSS.f+(1|dated)+(1|start_quadkeys),data=subset(ber.mov.08,lockdown==2&is.na(start_MSS.f)==FALSE),control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
lme.forest6<-lmer(z_score~end_forest_coverage*weekend+start_MSS.f*weekend+(1|dated)+(1|start_quadkeys),data=subset(ber.mov.08,lockdown==2&is.na(start_MSS.f)==FALSE),control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
lme.forest7<-lmer(z_score~end_forest_coverage*weekend+start_MSS.f*end_forest_coverage+(1|dated)+(1|start_quadkeys),data=subset(ber.mov.08,lockdown==2&is.na(start_MSS.f)==FALSE),control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
lme.forest8<-lmer(z_score~end_forest_coverage*weekend+start_MSS.f*end_forest_coverage+start_MSS.f*weekend+(1|dated)+(1|start_quadkeys),data=subset(ber.mov.08,lockdown==2&is.na(start_MSS.f)==FALSE),control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))


AIC(lme.forest1,lme.forest2,lme.forest3,lme.forest4,lme.forest5,lme.forest6,lme.forest7,lme.forest8)

lme.forest.w1<-lmer(z_score~end_forest_coverage*start_MSS.f*weekend+(1|dated)+(1|start_quadkeys),data=subset(ber.mov.08,lockdown==2&wave==2),control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))


library(ggeffects)
library(ggplot2)

Berlin.forest.mvt.effect<-ggpredict(lme.forest2,terms=c("end_forest_coverage[all]","start_MSS.f","weekend"))

Berlin.forest.mvt.effect$facet<-as.character(Berlin.forest.mvt.effect$facet)
Berlin.forest.mvt.effect$facet[Berlin.forest.mvt.effect$facet=="no"]<-"week days"
Berlin.forest.mvt.effect$facet[Berlin.forest.mvt.effect$facet=="yes"]<-"weekend"
Berlin.forest.mvt.effect$facet<-factor(Berlin.forest.mvt.effect$facet)



plot(Berlin.forest.mvt.effect,colors=c("#4daf4a","#984ea3","#377eb8","#e41a1c"),show.title = FALSE)+labs(colour="MSS")+xlab("proportion of tile forested")+ylab("Z08 of movement between tiles")




