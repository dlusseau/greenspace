###Berlin
# we replicate London's analyses in PARIS

#first we assign IMD and coverage_area and lockdown OxGRT to quadkeys


#OxGRT codebook is at: https://github.com/OxCGRT/covid-policy-tracker/blob/master/documentation/codebook.md

#we are after C6 - stay at home
lockdown<-read.csv("C:/Users/David/Documents/Facebook COVID/lockdown_indicators/OxCGRT_latest_Aug_30_2021.txt",header=T)
lockdown$date.pos<-as.Date(as.character(lockdown$Date),"%Y%m%d")

table(lockdown$RegionName[lockdown$CountryCode=="DEU"])

lockdown.berlin<-lockdown[lockdown$CountryCode=="DEU",c(17,52)]
lockdown.dk<-lockdown[lockdown$CountryCode=="DNK",c(17,52)]
#4 mar 2020 - 20 may 2021
as.Date(,"%Y-%m%-d")
lockdown.berlin[is.na(lockdown.berlin)]<-2 # posthoc confirmation


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



z <- sf_fisbroker("https://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s_gruenanlagenbestand")
z <- z %>%
     mutate(RAUMID = stringr::str_sub(gml_id, 12, 19)) %>%
     select(gml_id, RAUMID, everything()) %>%
     arrange(RAUMID)

dplyr::glimpse(z)
sf_save(z, "Berlingreenspace")

lor<-sf_fisbroker("https://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s_Indizes_MSS2019")
lor <- lor %>%
     mutate(RAUMID = stringr::str_sub(gml_id, 12, 19)) %>%
     select(gml_id, RAUMID, everything()) %>%
     arrange(RAUMID)

dplyr::glimpse(lor)
sf_save(lor, "BerlinIMD")

##btw, epsg:25833 ??????
#save to geojson and projects to epsg 4326; noice
#################################################

deprivation_folder<-"C:/Users/David/Documents/deprivation_indices"
greenspace_folder<-"C:/Users/David/Documents/greenspace/Berlingreenspace"

library(sf)
library(geojsonsf)
Berlingreenspace<-geojson_sf(paste0(greenspace_folder,"/Berlingreenspace.geojson"))
Encoding(Berlingreenspace$namenr) <- "UTF-8" 
Encoding(Berlingreenspace$objartname) <- "UTF-8" 
Encoding(Berlingreenspace$planname) <- "UTF-8" 

BerlinIMD<-geojson_sf(paste0(deprivation_folder,"/BerlinIMD/BerlinIMD.geojson"))

# SI_N_2019 frustratingly categorical and not in quintiles... we will deal with it as categorical post 
#in addition it is turned the wrong way around SI low is affluent area and SI high is deprived area
# finally .... 1 is high, 2 is MEDIUM :(, 3 is low, 4 is very low..... (now DEU might understand why others use quintiles)


ber.pop.files<-list.files("C:/Users/David/Documents/Facebook COVID/Facebook Berlin",full.names=TRUE)

#ParisIMD and Parisgreenspace are both crs 4326

ber.pop<-read.csv(ber.pop.files[1],header=T)

###########
earth_circumference_m<-6378137*2*pi              
earth_circumference<-360 
##################


ber.pop$tile_size<-(cos(ber.pop$lat * pi/180) * earth_circumference)/(2^(nchar(as.character(ber.pop$quadkey))))
ber.pop$lon.top<-ber.pop$lon+(ber.pop$tile_size/2)
ber.pop$lon.bot<-ber.pop$lon-(ber.pop$tile_size/2)
ber.pop$lat.top<-ber.pop$lat+(ber.pop$tile_size/2)
ber.pop$lat.bot<-ber.pop$lat-(ber.pop$tile_size/2)

ber.pop$gardens<-0
ber.pop$coverage<-0


gardens.quadkey<-list()
gardens.quadkey.df<-data.frame(quadkey=0,gardens=0,coverage=0)


##############################################
#### inner file loop
#############################################
for (i in 1:dim(ber.pop)[1]) {

try.sf<-st_sfc(st_polygon(list(rbind(c(ber.pop$lon.top[i],ber.pop$lat.top[i]),c(ber.pop$lon.bot[i],ber.pop$lat.top[i]),c(ber.pop$lon.bot[i],ber.pop$lat.bot[i]),c(ber.pop$lon.top[i],ber.pop$lat.bot[i]),c(ber.pop$lon.top[i],ber.pop$lat.top[i])))))
st_crs(try.sf)<-4326
try.inter<-st_intersection(Berlingreenspace,try.sf)

if (length(try.inter$namenr)>0) {
ber.pop$gardens[i]<-length(try.inter$namenr)
ber.pop$coverage[i]<-as.numeric(sum(st_area(try.inter)))
gardens.quadkey[[paste(ber.pop$quadkey[i])]]<-data.frame(name=as.array(as.character(try.inter$namenr)),area=as.numeric(st_area(try.inter))) #let's keep area here too for when we subset some greenspace categories away
gardens.quadkey.df<-rbind(gardens.quadkey.df,data.frame(quadkey=ber.pop$quadkey[i],gardens=length(try.inter$namenr),coverage=as.numeric(sum(st_area(try.inter)))))

#print(i)
#flush.console()

}  else {

gardens.quadkey.df<-rbind(gardens.quadkey.df,data.frame(quadkey=ber.pop$quadkey[i],gardens=0,coverage=0))

}
}
ber.pop$coverage_area<-ber.pop$coverage/(ber.pop$tile_size*earth_circumference_m/360)^2

gardens.quadkey.df<-gardens.quadkey.df[-1,]

#######################


#######################################################
########inter-file loop starts here

for (j in 2:length(ber.pop.files)) {


ber.pop.it<-read.csv(ber.pop.files[j],header=T)

ber.pop.it$tile_size<-(cos(ber.pop.it$lat * pi/180) * earth_circumference)/(2^(nchar(as.character(ber.pop.it$quadkey))))
ber.pop.it$lon.top<-ber.pop.it$lon+(ber.pop.it$tile_size/2)
ber.pop.it$lon.bot<-ber.pop.it$lon-(ber.pop.it$tile_size/2)
ber.pop.it$lat.top<-ber.pop.it$lat+(ber.pop.it$tile_size/2)
ber.pop.it$lat.bot<-ber.pop.it$lat-(ber.pop.it$tile_size/2)

ber.pop.it$gardens<-0
ber.pop.it$coverage<-0


for (i in 1:dim(ber.pop.it)[1]) {  #I know it does not look it, but practically it is more time efficient

#######
##### let's first compare the quadkeys to the preexisting quadkeys
existing.quadkey<-which(ber.pop.it$quadkey[i]==gardens.quadkey.df$quadkey,arr.ind=TRUE)
if (length(existing.quadkey)>0) {

ber.pop.it$gardens[i]<-gardens.quadkey.df$gardens[existing.quadkey]
ber.pop.it$coverage[i]<-gardens.quadkey.df$coverage[existing.quadkey]

} else {

try.sf<-st_sfc(st_polygon(list(rbind(c(ber.pop.it$lon.top[i],ber.pop.it$lat.top[i]),c(ber.pop.it$lon.bot[i],ber.pop.it$lat.top[i]),c(ber.pop.it$lon.bot[i],ber.pop.it$lat.bot[i]),c(ber.pop.it$lon.top[i],ber.pop.it$lat.bot[i]),c(ber.pop.it$lon.top[i],ber.pop.it$lat.top[i])))))
st_crs(try.sf)<-4326
try.inter<-st_intersection(Berlingreenspace,try.sf)

if (length(try.inter$namenr)>0) {
ber.pop.it$gardens[i]<-length(try.inter$namenr)
ber.pop.it$coverage[i]<-as.numeric(sum(st_area(try.inter)))
gardens.quadkey[[paste(ber.pop.it$quadkey[i])]]<-data.frame(name=as.array(as.character(try.inter$namenr)),area=as.numeric(st_area(try.inter))) #let's keep area here too for when we subset some greenspace categories away
gardens.quadkey.df<-rbind(gardens.quadkey.df,data.frame(quadkey=ber.pop.it$quadkey[i],gardens=length(try.inter$namenr),coverage=as.numeric(sum(st_area(try.inter)))))

}  else {

gardens.quadkey.df<-rbind(gardens.quadkey.df,data.frame(quadkey=ber.pop.it$quadkey[i],gardens=0,coverage=0))

} #ifelse intersect

} #ifelse preexisting

} #iteration

ber.pop.it$coverage_area<-ber.pop.it$coverage/(ber.pop.it$tile_size*earth_circumference_m/360)^2

ber.pop<-rbind(ber.pop,ber.pop.it)

print(j)
flush.console()

}


save(lockdown.berlin,BerlinIMD,Berlingreenspace,ber.pop,gardens.quadkey.df,gardens.quadkey,file="C:/Users/David/Documents/Facebook COVID/berlin_corona_greenspace_Aug_2021.Rdata")
###i overwrote paris :(

ber.pop$dated<-factor(substr(ber.pop$date_time,1,10))
ber.pop$time<-factor(substr(ber.pop$date_time,12,15))

ber.pop$dates<-as.POSIXct(ber.pop$dated, format="%Y-%m-%d")

save(lockdown.berlin,BerlinIMD,Berlingreenspace,ber.pop,gardens.quadkey.df,gardens.quadkey,file="C:/Users/David/Documents/Facebook COVID/berlin_corona_greenspace_Aug_2021.Rdata")


lockdown<-read.csv("C:/Users/David/Documents/Facebook COVID/lockdown_indicators/OxCGRT_latest_Aug_30_2021.txt",header=T)
lockdown$date.pos<-as.Date(as.character(lockdown$Date),"%Y%m%d")

lockdown.berlin<-lockdown[lockdown$CountryCode=="DEU",c(17,52)]

lockdown.berlin[is.na(lockdown.berlin)]<-2 # posthoc confirmation

ber.pop$lockdown<-0
ber.pop$lockdown<-lockdown.berlin$C6_Stay_at_home_requirements[match(as.Date(ber.pop$dates),lockdown.berlin$date.pos)] #much faster than merge
ber.pop$lockdown<-factor(ber.pop$lockdown)


save(lockdown.berlin,BerlinIMD,Berlingreenspace,ber.pop,gardens.quadkey.df,gardens.quadkey,file="C:/Users/David/Documents/Facebook COVID/berlin_corona_greenspace_Aug_2021.Rdata")



ber.pop$MSS<-NA

#########################

quadkey.imd<-data.frame(quadkey=unique(ber.pop$quadkey),MSS=NA)

iter<-match(quadkey.imd$quadkey,ber.pop$quadkey)

m<-1
for (i in iter) {

try.sf<-st_sfc(st_polygon(list(rbind(c(ber.pop$lon.top[i],ber.pop$lat.top[i]),c(ber.pop$lon.bot[i],ber.pop$lat.top[i]),c(ber.pop$lon.bot[i],ber.pop$lat.bot[i]),c(ber.pop$lon.top[i],ber.pop$lat.bot[i]),c(ber.pop$lon.top[i],ber.pop$lat.top[i])))))
st_crs(try.sf)<-4326
try.inter<-st_intersection(BerlinIMD,try.sf)

if (length(try.inter$RAUMID)>0) {
	quadkey.imd$MSS[m]<-median(try.inter$SI_N_2019) #LSOA$IMD does not contain any NAs so if the intersection length >0 we are safe to take the median without removing NAs
}


print(i)
flush.console()
m<-m+1

} 

quadkey.imd$lat<-ber.pop$lat[iter]
quadkey.imd$lon<-ber.pop$lon[iter]
#quadkey.imd.map<-st_as_sf(quadkey.imd,coords=c("lon","lat"),crs=4326)

save(quadkey.imd,file="C:/Users/David/Documents/deprivation_indices/london/berlin_corona_IMD_quadkey.Rdata")


#########################################################################
#### let's deal with median and this ordinal variable


ber.pop$MSS<-quadkey.imd$MSS[match(ber.pop$quadkey,quadkey.imd$quadkey)]


save(ber.pop,file="C:/Users/David/Documents/Facebook COVID/berlin_corona_FB_Aug_2021.Rdata")

ber.pop.bi<-ber.pop[!is.na(ber.pop$MSS),]

save(ber.pop.bi,file="C:/Users/David/Documents/Facebook COVID/berlin_corona_FB_BI_Aug_2021.Rdata")


#remember MSS is a 'status' measure 1 is high (affluent) 4 is low (deprived)
#let's treat it as continous variable in the first instance (after all it is ordinal and we get 1/2 measures from the median estimates, 
#in the SI definition, there is no indication of discontinuity between levels

cov(ber.pop.bi$MSS,ber.pop.bi$coverage_area)
#0.00587 noice

######################################################################################
load("C:/Users/David/Documents/Facebook COVID/berlin_corona_FB_BI_Aug_2021.Rdata")


ber.pop.bi$weekday<-weekdays(ber.pop.bi$dates)
ber.pop.bi$weekend<-"no"
ber.pop.bi$weekend[ber.pop.bi$weekday=="Saturday"]<-"yes"
ber.pop.bi$weekend[ber.pop.bi$weekday=="Sunday"]<-"yes"
ber.pop.bi$weekend<-factor(ber.pop.bi$weekend)
ber.pop.bi$weekday<-factor(ber.pop.bi$weekday)

ber.pop.bi$time[ber.pop.bi$time=="00:0"]<-"0000"
ber.pop.bi$time[ber.pop.bi$time=="08:0"]<-"0800"
ber.pop.bi$time[ber.pop.bi$time=="16:0"]<-"1600"
ber.pop.bi$time<-factor(as.character(ber.pop.bi$time))
ber.pop.bi$quadkeys<-factor(ber.pop.bi$quadkey)



load("C:/Users/David/Documents/Facebook COVID/Twitter/tweet.volume.df.all.Rdata")

ber.pop.bi$repeats<-tweet.vol.ber$repeats[match(as.Date(ber.pop.bi$dates),tweet.vol.ber$date)]
rm(tweet.vol.lon,tweet.vol.ber,tweet.vol.par)
table(ber.pop.bi$lockdown,ber.pop.bi$repeats)


ber.pop.bi$repeats<-as.numeric(ber.pop.bi$repeats)
ber.pop.bi$repeats[ber.pop.bi$lockdown=="0"]<-ber.pop.bi$repeats[ber.pop.bi$lockdown=="0"]-1 #we start in March
ber.pop.bi$repeats[ber.pop.bi$lockdown=="1"]<-ber.pop.bi$repeats[ber.pop.bi$lockdown=="1"]-1 #we start in March

ber.pop.bi$repeats<-factor(ber.pop.bi$repeats)

###we only have one sample of lockdown phase 1 so we subset to 0 and 2 only for this part of the model selection


save(ber.pop.bi,file="C:/Users/David/Documents/Facebook COVID/berlin_corona_FB_BI_Aug_2021.Rdata")


ber.pop.bi.day<-subset(ber.pop.bi,time=="0800")
rm(ber.pop.bi)
gc()
library(lme4)

formula<-c("clipped_z_score~lockdown",
"clipped_z_score~lockdown*weekend",
"clipped_z_score~lockdown*MSS",
"clipped_z_score~lockdown*weekend*MSS",
"clipped_z_score~lockdown*coverage_area",
"clipped_z_score~lockdown*weekend*coverage_area",
"clipped_z_score~lockdown*MSS*coverage_area",
"clipped_z_score~lockdown*weekend*MSS*coverage_area",
"clipped_z_score~lockdown +(1|lockdown:repeats)",
"clipped_z_score~lockdown*weekend+(1|lockdown:repeats)",
"clipped_z_score~lockdown*MSS+(1|lockdown:repeats)",
"clipped_z_score~lockdown*weekend*MSS+(1|lockdown:repeats)",
"clipped_z_score~lockdown*coverage_area+(1|lockdown:repeats)",
"clipped_z_score~lockdown*weekend*coverage_area+(1|lockdown:repeats)",
"clipped_z_score~lockdown*MSS*coverage_area+(1|lockdown:repeats)",
"clipped_z_score~lockdown*weekend*MSS*coverage_area+(1|lockdown:repeats)"
)


AIC.df.r<-data.frame(model=formula, AIC=NA, time=0)

for (i in 1:length(formula)) {
print(formula[i])
flush.console()
tic<-Sys.time()
ber.lme.final<-lmer(as.formula(paste0(formula[i],"+ (1|dated) + (1|quadkeys)")) ,data=ber.pop.bi.day,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
AIC.df.r$time[i]<-Sys.time()-tic
AIC.df.r$AIC[i]<-AIC(par.lme.final)
rm(par.lme.final)
gc()
}
AIC.df.r

save(AIC.df.r,formula,file="C:/Users/David/Documents/Facebook COVID/modelselection_Berlin_day_wrandom_repeats_FB.Rdata")

ber.lme.final<-lmer(as.formula(paste0(formula[i],"+ (1|dated) + (1|quadkeys)")) ,data=ber.pop.bi.day,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))



library(ggeffects)
##plot repeats 1 #FDep ticks are 10% 50% and 90%

Berlin.final.effect<-ggpredict(ber.lme.final,terms=c("coverage_area [all]","MSS [1,2,3,4]","lockdown","weekend"))
 png(file="C:/Users/David/Documents/Facebook COVID/Berlin_lockdown_main_final_0800_randomrepeat.png", width=60,height=42,units="cm",res=200)
 plot(Berlin.final.effect,limits=c(-4,3))
 dev.off()

######################################################################################################################################################
##### contrast

load("C:/Users/David/Documents/Facebook COVID/berlin_corona_FB_BI_Aug_2021.Rdata")
berlin_contrast0800<-subset(ber.pop.bi,time=="0800")
berlin_contrast0000<-subset(ber.pop.bi,time=="0000")
berlin_contrast<-merge(berlin_contrast0800,berlin_contrast0000,by.x=c("quadkeys","dated"),by.y=c("quadkeys","dated"),all.x=TRUE)
berlin_contrast$z_score<-berlin_contrast$clipped_z_score.x-berlin_contrast$clipped_z_score.y
ber.bi.cont<-berlin_contrast[,c(1,2,57,23,25,26,27,29)]
names(ber.bi.cont)<-c("quadkeys","dated","z_score","coverage_area","dates","lockdown","MSS","weekend")



load("C:/Users/David/Documents/Facebook COVID/Twitter/tweet.volume.df.all.Rdata")

ber.bi.cont$repeats<-tweet.vol.ber$repeats[match(as.Date(ber.bi.cont$dates),tweet.vol.ber$date)]
rm(tweet.vol.lon,tweet.vol.ber,tweet.vol.par)
table(ber.bi.cont$lockdown,ber.bi.cont$repeats)


ber.bi.cont$repeats<-as.numeric(ber.bi.cont$repeats)
ber.bi.cont$repeats[ber.bi.cont$lockdown=="0"]<-ber.bi.cont$repeats[ber.bi.cont$lockdown=="0"]-1 #we start in March
ber.bi.cont$repeats[ber.bi.cont$lockdown=="1"]<-ber.bi.cont$repeats[ber.bi.cont$lockdown=="1"]-1 #we start in March

ber.bi.cont$repeats<-factor(ber.bi.cont$repeats)


formula<-c("z_score~lockdown",
"z_score~lockdown*weekend",
"z_score~lockdown*MSS",
"z_score~lockdown*weekend*MSS",
"z_score~lockdown*coverage_area",
"z_score~lockdown*weekend*coverage_area",
"z_score~lockdown*MSS*coverage_area",
"z_score~lockdown*weekend*MSS*coverage_area",
"z_score~lockdown +(1|lockdown:repeats)",
"z_score~lockdown*weekend+(1|lockdown:repeats)",
"z_score~lockdown*MSS+(1|lockdown:repeats)",
"z_score~lockdown*weekend*MSS+(1|lockdown:repeats)",
"z_score~lockdown*coverage_area+(1|lockdown:repeats)",
"z_score~lockdown*weekend*coverage_area+(1|lockdown:repeats)",
"z_score~lockdown*MSS*coverage_area+(1|lockdown:repeats)",
"z_score~lockdown*weekend*MSS*coverage_area+(1|lockdown:repeats)"
)


AIC.df.c<-data.frame(model=formula, AIC=NA, time=0)

for (i in 1:length(formula)) {
print(formula[i])
flush.console()
tic<-Sys.time()
ber.lme.final<-lmer(as.formula(paste0(formula[i],"+ (1|dated) + (1|quadkeys)")) ,data=ber.bi.cont,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
AIC.df.c$time[i]<-Sys.time()-tic
AIC.df.c$AIC[i]<-AIC(ber.lme.final)
rm(ber.lme.final)
gc()
}
AIC.df.c
                                                                  # model      AIC      time
# 1                                                 clipped_z_score~lockdown 10988562  3.535201
# 2                                         clipped_z_score~lockdown*weekend 10988468  4.522551
# 3                                             clipped_z_score~lockdown*MSS 10980455  3.870851
# 4                                     clipped_z_score~lockdown*weekend*MSS 10971095  4.188819
# 5                                   clipped_z_score~lockdown*coverage_area 10988430  3.694836
# 6                           clipped_z_score~lockdown*weekend*coverage_area 10988338  4.174394
# 7                               clipped_z_score~lockdown*MSS*coverage_area 10979996  3.906533
# 8                       clipped_z_score~lockdown*weekend*MSS*coverage_area 10970608  4.854182
# 9                           clipped_z_score~lockdown +(1|lockdown:repeats) 10967884  9.944210
# 10                   clipped_z_score~lockdown*weekend+(1|lockdown:repeats) 10967752  7.206847
# 11                       clipped_z_score~lockdown*MSS+(1|lockdown:repeats) 10959779  9.122771
# 12               clipped_z_score~lockdown*weekend*MSS+(1|lockdown:repeats) 10950369 12.135927
# 13             clipped_z_score~lockdown*coverage_area+(1|lockdown:repeats) 10967753 11.504435
# 14     clipped_z_score~lockdown*weekend*coverage_area+(1|lockdown:repeats) 10967622 10.986751
# 15         clipped_z_score~lockdown*MSS*coverage_area+(1|lockdown:repeats) 10959317  9.834193
# 16 clipped_z_score~lockdown*weekend*MSS*coverage_area+(1|lockdown:repeats) 10949879 12.880459

save(AIC.df.r,formula,file="C:/Users/David/Documents/Facebook COVID/modelselection_Berlin_CONTRAST_wrandom_repeats_FB.Rdata")

i=16
ber.lme.final<-lmer(as.formula(paste0(formula[i],"+ (1|dated) + (1|quadkeys)")) ,data=ber.bi.cont,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))


library(ggeffects)
##plot repeats 1 #FDep ticks are 10% 50% and 90%

Berlin.final.effect<-ggpredict(ber.lme.final,terms=c("coverage_area [all]","MSS [1,2,3,4]","lockdown","weekend"))
 png(file="C:/Users/David/Documents/Facebook COVID/Berlin_lockdown_main_final_CONTRAST_randomrepeat.png", width=60,height=42,units="cm",res=200)
 plot(Berlin.final.effect,limits=c(-4,3))
 dev.off()

#################################################
####given we do not have a 0 repeats we tackle the questions in two steps:
# lockdown 1 and 2 repeats
#no repeats (subset repeat 1) 0 v 1 v 2

#let's start there.
ber.pop.bi.day

ber.pop.wave1<-subset(ber.pop.bi.day,repeats=="1")



formula<-c("clipped_z_score~lockdown",
"clipped_z_score~lockdown*weekend",
"clipped_z_score~lockdown*MSS",
"clipped_z_score~lockdown*weekend*MSS",
"clipped_z_score~lockdown*coverage_area",
"clipped_z_score~lockdown*weekend*coverage_area",
"clipped_z_score~lockdown*MSS*coverage_area",
"clipped_z_score~lockdown*weekend*MSS*coverage_area"
)


AIC.df.w<-data.frame(model=formula, AIC=NA, time=0)

for (i in 1:length(formula)) {
print(formula[i])
flush.console()
tic<-Sys.time()
ber.lme.final<-lmer(as.formula(paste0(formula[i],"+ (1|dated) + (1|quadkeys)")) ,data=ber.pop.wave1,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
AIC.df.w$time[i]<-Sys.time()-tic
AIC.df.w$AIC[i]<-AIC(ber.lme.final)
rm(ber.lme.final)
gc()
}
AIC.df.w

save(AIC.df.w,formula,file="C:/Users/David/Documents/Facebook COVID/modelselection_Berlin_day_wave1_FB.Rdata")

ber.lme.final<-lmer(as.formula(paste0(formula[i],"+ (1|dated) + (1|quadkeys)")) ,data=ber.pop.wave1,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))


Berlin.final.effect<-ggpredict(ber.lme.final,terms=c("coverage_area [all]","MSS [1,2,3,4]","lockdown","weekend"))
 png(file="C:/Users/David/Documents/Facebook COVID/Berlin_lockdown_main_final_WAVE1_day.png", width=60,height=42,units="cm",res=200)
 plot(Berlin.final.effect,limits=c(-4,3))
 dev.off()

##########################################
#### repeats

ber.pop.rep<-subset(ber.pop.bi.day,lockdown!="0")

ber.pop.rep$lockdown<-factor(ber.pop.rep$lockdown)

str(ber.pop.rep)

table(ber.pop.rep$lockdown,ber.pop.rep$repeats)
ber.pop.rep<-subset(ber.pop.rep,repeats!="4")
ber.pop.rep$repeats<-factor(ber.pop.rep$repeats)


formula<-c("clipped_z_score~lockdown",
"clipped_z_score~lockdown*repeats",
"clipped_z_score~lockdown*weekend",
"clipped_z_score~lockdown*repeats*weekend",
"clipped_z_score~lockdown*MSS",
"clipped_z_score~lockdown*repeats*MSS",
"clipped_z_score~lockdown*weekend*MSS",
"clipped_z_score~lockdown*repeats*weekend*MSS",
"clipped_z_score~lockdown*coverage_area",
"clipped_z_score~lockdown*repeats*coverage_area",
"clipped_z_score~lockdown*weekend*coverage_area",
"clipped_z_score~lockdown*repeats*weekend*coverage_area",
"clipped_z_score~lockdown*MSS*coverage_area",
"clipped_z_score~lockdown*repeats*MSS*coverage_area",
"clipped_z_score~lockdown*weekend*MSS*coverage_area",
"clipped_z_score~lockdown*repeats*weekend*MSS*coverage_area"

)


AIC.df.r<-data.frame(model=formula, AIC=NA, time=0)

for (i in 1:length(formula)) {
print(formula[i])
flush.console()
tic<-Sys.time()
ber.lme.final<-lmer(as.formula(paste0(formula[i],"+ (1|dated) + (1|quadkeys)")) ,data=ber.pop.rep,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
AIC.df.r$time[i]<-Sys.time()-tic
AIC.df.r$AIC[i]<-AIC(ber.lme.final)
rm(ber.lme.final)
gc()
}
AIC.df.r

save(AIC.df.r,formula,file="C:/Users/David/Documents/Facebook COVID/modelselection_Berlin_day_repeats_FB.Rdata")

ber.lme.final<-lmer(as.formula(paste0(formula[i],"+ (1|dated) + (1|quadkeys)")) ,data=ber.pop.rep,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))


Berlin.final.effect<-ggpredict(ber.lme.final,terms=c("coverage_area [all]","MSS [1,2,3,4]","repeats","weekend"),condition=c(lockdown="1"))
 png(file="C:/Users/David/Documents/Facebook COVID/Berlin_lockdown_main_final_REPEATS_day_lockdown1.png", width=60,height=42,units="cm",res=200)
 plot(Berlin.final.effect,limits=c(-4,3))
 dev.off()

Berlin.final.effect<-ggpredict(ber.lme.final,terms=c("coverage_area [all]","MSS [1,2,3,4]","repeats","weekend"),condition=c(lockdown="2"))
 png(file="C:/Users/David/Documents/Facebook COVID/Berlin_lockdown_main_final_REPEATS_day_lockdown2.png", width=60,height=42,units="cm",res=200)
 plot(Berlin.final.effect,limits=c(-4,3))
 dev.off()


##############################################################################################################################
################################################################################################################################
###################################################################################################################################
###################################################################################################################################
##### repeat these analyses with contrast

#ber.bi.cont

#################################################
####given we do not have a 0 repeats we tackle the questions in two steps:
# lockdown 1 and 2 repeats
#no repeats (subset repeat 1) 0 v 1 v 2

#let's start there.


ber.pop.wave1<-subset(ber.bi.cont,repeats=="1")



formula<-c("z_score~lockdown",
"z_score~lockdown*weekend",
"z_score~lockdown*MSS",
"z_score~lockdown*weekend*MSS",
"z_score~lockdown*coverage_area",
"z_score~lockdown*weekend*coverage_area",
"z_score~lockdown*MSS*coverage_area",
"z_score~lockdown*weekend*MSS*coverage_area"
)


AIC.df.w<-data.frame(model=formula, AIC=NA, time=0)

for (i in 1:length(formula)) {
print(formula[i])
flush.console()
tic<-Sys.time()
ber.lme.final<-lmer(as.formula(paste0(formula[i],"+ (1|dated) + (1|quadkeys)")) ,data=ber.pop.wave1,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
AIC.df.w$time[i]<-Sys.time()-tic
AIC.df.w$AIC[i]<-AIC(ber.lme.final)
rm(ber.lme.final)
gc()
}
AIC.df.w

save(AIC.df.w,formula,file="C:/Users/David/Documents/Facebook COVID/modelselection_Berlin_CONTRAST_wave1_FB.Rdata")

ber.lme.final<-lmer(as.formula(paste0(formula[i],"+ (1|dated) + (1|quadkeys)")) ,data=ber.pop.wave1,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))


Berlin.final.effect<-ggpredict(ber.lme.final,terms=c("coverage_area [all]","MSS [4,3,1]","lockdown","weekend"))
 png(file="C:/Users/David/Documents/Facebook COVID/Berlin_lockdown_main_final_WAVE1_CONTRAST.png", width=60,height=42,units="cm",res=200)
 plot(Berlin.final.effect,limits=c(-2,2))
 dev.off()

##########################################################################################################################################
#####MANUSCRIPT FIGURE BERLIN Z ONTRAST WAVE 1

gg.berlin<-plot(Berlin.final.effect,limits=c(-2,2),limit.range=FALSE,show.y.title=FALSE,show.x.title=FALSE,show.title=FALSE)
gg.berlin[[1]]$labels$y<-"Predicted Z-score (Z08-00)"
gg.berlin[[2]]$labels$x<-"Proportion of tile covered by greenspace"

tiff(file="C:/Users/David/OneDrive - Danmarks Tekniske Universitet/Facebook Greenspace ms/Figure_Berlin_Z_contrast_wave1.tiff", width=20,height=20,units="cm",res=200)
gg.berlin
dev.off()

##########################################
#### repeats

ber.pop.rep<-subset(ber.bi.cont,lockdown!="0")

ber.pop.rep$lockdown<-factor(ber.pop.rep$lockdown)

str(ber.pop.rep)

table(ber.pop.rep$lockdown,ber.pop.rep$repeats)
ber.pop.rep<-subset(ber.pop.rep,repeats!="4")
ber.pop.rep$repeats<-factor(ber.pop.rep$repeats)


formula<-c("z_score~lockdown",
"z_score~lockdown*repeats",
"z_score~lockdown*weekend",
"z_score~lockdown*repeats*weekend",
"z_score~lockdown*MSS",
"z_score~lockdown*repeats*MSS",
"z_score~lockdown*weekend*MSS",
"z_score~lockdown*repeats*weekend*MSS",
"z_score~lockdown*coverage_area",
"z_score~lockdown*repeats*coverage_area",
"z_score~lockdown*weekend*coverage_area",
"z_score~lockdown*repeats*weekend*coverage_area",
"z_score~lockdown*MSS*coverage_area",
"z_score~lockdown*repeats*MSS*coverage_area",
"z_score~lockdown*weekend*MSS*coverage_area",
"z_score~lockdown*repeats*weekend*MSS*coverage_area"

)


AIC.df.r<-data.frame(model=formula, AIC=NA, time=0)

for (i in 1:length(formula)) {
print(formula[i])
flush.console()
tic<-Sys.time()
ber.lme.final<-lmer(as.formula(paste0(formula[i],"+ (1|dated) + (1|quadkeys)")) ,data=ber.pop.rep,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
AIC.df.r$time[i]<-Sys.time()-tic
AIC.df.r$AIC[i]<-AIC(ber.lme.final)
rm(ber.lme.final)
gc()
}
AIC.df.r

save(AIC.df.r,formula,file="C:/Users/David/Documents/Facebook COVID/modelselection_Berlin_CONTRAST_repeats_FB.Rdata")

ber.lme.final<-lmer(as.formula(paste0(formula[i],"+ (1|dated) + (1|quadkeys)")) ,data=ber.pop.rep,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))


Berlin.final.effect<-ggpredict(ber.lme.final,terms=c("coverage_area [all]","MSS [1,2,3,4]","repeats","weekend"),condition=c(lockdown="1"))
Berlin.final.effect<-ggpredict(ber.lme.final,terms=c("coverage_area [all]","MSS [1,2,3,4]","repeats","weekend"),condition=c(lockdown="2"))


####################################################################################################################################################
#####################################################################################################################################################
Berlin.final.effect1<-ggpredict(ber.lme.final,terms=c("coverage_area [all]","MSS [4,3,1]","repeats","weekend"),condition=c(lockdown="1"))
Berlin.final.effect2<-ggpredict(ber.lme.final,terms=c("coverage_area [all]","MSS [4,3,1]","repeats","weekend"),condition=c(lockdown="2"))



gg.berlin.0<-plot(Berlin.final.effect1,limits=c(-1.25,2.75),colors= c("#4DAF4A","#377EB8","#E41A1C"),show.y.title=FALSE,show.x.title=FALSE,show.title=FALSE)
#need to reverse the color scale as 1 is high is 4 is low in Berlin

gg.berlin.0$labels$y<-"Predicted difference in Z-score (Z08-Z00)"
gg.berlin.0[[1]]$labels$x<-"Proportion of tile covered by greenspace"
gg.berlin.0[[1]]$labels$title<-"Berlin - contrasting day and night - recommend not leaving house with exceptions"
gg.berlin.0[[1]]$labels$subtitle<-"week days"
gg.berlin.0[[2]]$labels$subtitle<-"weekend"


gg.berlin.1<-plot(Berlin.final.effect2,limits=c(-1.25,2.75),colors= c("#4DAF4A","#377EB8","#E41A1C"),show.y.title=FALSE,show.x.title=FALSE,show.title=FALSE)
#need to reverse the color scale as 1 is high is 4 is low in Berlin
gg.berlin.1[[1]]$labels$y<-"Predicted difference in Z-score (Z08-Z00)"
gg.berlin.1$labels$x<-"Proportion of tile covered by greenspace"
gg.berlin.1[[1]]$labels$title<-"Berlin - contrasting day and night - require not leaving house with exceptions"
gg.berlin.1[[1]]$labels$subtitle<-"week days"
gg.berlin.1[[2]]$labels$subtitle<-"weekend"


#################################MANUSCRIPT FIGURE LAST BERLIN CONT ALL
library(grid)
library(gridExtra)
library(ggplotify)
tiff(file="C:/Users/David/OneDrive - Danmarks Tekniske Universitet/Facebook Greenspace ms/Figure_berlin_Z_CONT_repeats.tiff", width=40,height=40,units="cm",res=200)
grid.arrange(as.grob(gg.berlin.0),as.grob(gg.berlin.1),nrow=2,heights=c(10,10))
dev.off()

####################################################################################################################################################
#####################################################################################################################################################
############################MANUSCRIPT FIGURE LAST BERLIN CONT LOCKDOWN ONLY

gg.berlin.1<-plot(Berlin.final.effect2,limits=c(-1.25,2.75),colors= c("#4DAF4A","#377EB8","#E41A1C"),show.y.title=FALSE,show.x.title=FALSE,show.title=FALSE)
#need to reverse the color scale as 1 is high is 4 is low in Berlin
gg.berlin.1[[1]]$labels$y<-"Predicted difference in Z-score (Z08-Z00)"
gg.berlin.1$labels$x<-"Proportion of tile covered by greenspace"
gg.berlin.1[[1]]$labels$subtitle<-"Berlin - week days"
gg.berlin.1[[2]]$labels$subtitle<-"Berlin - weekend"
tiff(file="C:/Users/David/OneDrive - Danmarks Tekniske Universitet/Facebook Greenspace ms/Figure_berlin_Z_CONT_repeats_LOCKDOWN_ONLY.tiff", width=40,height=20,units="cm",res=200)
gg.berlin.1
dev.off()

####################################################################################################################################################
#####################################################################################################################################################
####################################################################################################################################################
#####################################################################################################################################################
####################################################################################################################################################
#####################################################################################################################################################
####################################################################################################################################################
#####################################################################################################################################################
####################0800 now


load("C:/Users/David/Documents/Facebook COVID/berlin_corona_FB_BI_Aug_2021.Rdata")
berlin_contrast0800<-subset(ber.pop.bi,time=="0800")

load("C:/Users/David/Documents/Facebook COVID/Twitter/tweet.volume.df.all.Rdata")

berlin_contrast0800$repeats<-tweet.vol.ber$repeats[match(as.Date(berlin_contrast0800$dates),tweet.vol.ber$date)]
rm(tweet.vol.lon,tweet.vol.ber,tweet.vol.par)
table(berlin_contrast0800$lockdown,berlin_contrast0800$repeats)


berlin_contrast0800$repeats<-as.numeric(berlin_contrast0800$repeats)
berlin_contrast0800$repeats[berlin_contrast0800$lockdown=="0"]<-berlin_contrast0800$repeats[berlin_contrast0800$lockdown=="0"]-1 #we start in March
berlin_contrast0800$repeats[berlin_contrast0800$lockdown=="1"]<-berlin_contrast0800$repeats[berlin_contrast0800$lockdown=="1"]-1 #we start in March

berlin_contrast0800$repeats<-factor(berlin_contrast0800$repeats)

ber.pop.rep<-subset(berlin_contrast0800,lockdown!="0")

ber.pop.rep$lockdown<-factor(ber.pop.rep$lockdown)

str(ber.pop.rep)

table(ber.pop.rep$lockdown,ber.pop.rep$repeats)
ber.pop.rep<-subset(ber.pop.rep,repeats!="4")
ber.pop.rep$repeats<-factor(ber.pop.rep$repeats)


formula<-c("clipped_z_score~lockdown",
"clipped_z_score~lockdown*repeats",
"clipped_z_score~lockdown*weekend",
"clipped_z_score~lockdown*repeats*weekend",
"clipped_z_score~lockdown*MSS",
"clipped_z_score~lockdown*repeats*MSS",
"clipped_z_score~lockdown*weekend*MSS",
"clipped_z_score~lockdown*repeats*weekend*MSS",
"clipped_z_score~lockdown*coverage_area",
"clipped_z_score~lockdown*repeats*coverage_area",
"clipped_z_score~lockdown*weekend*coverage_area",
"clipped_z_score~lockdown*repeats*weekend*coverage_area",
"clipped_z_score~lockdown*MSS*coverage_area",
"clipped_z_score~lockdown*repeats*MSS*coverage_area",
"clipped_z_score~lockdown*weekend*MSS*coverage_area",
"clipped_z_score~lockdown*repeats*weekend*MSS*coverage_area"

)

i=16

ber.lme.final<-lmer(as.formula(paste0(formula[i],"+ (1|dated) + (1|quadkeys)")) ,data=ber.pop.rep,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))

####################################################################################################################################################
#####################################################################################################################################################
Berlin.final.effect1<-ggpredict(ber.lme.final,terms=c("coverage_area [all]","MSS [4,3,1]","repeats","weekend"),condition=c(lockdown="1"))
Berlin.final.effect2<-ggpredict(ber.lme.final,terms=c("coverage_area [all]","MSS [4,3,1]","repeats","weekend"),condition=c(lockdown="2"))



gg.berlin.0<-plot(Berlin.final.effect1,limits=c(-4.5,3),colors= c("#4DAF4A","#377EB8","#E41A1C"),show.y.title=FALSE,show.x.title=FALSE,show.title=FALSE)
#need to reverse the color scale as 1 is high is 4 is low in Berlin

gg.berlin.0$labels$y<-"Predicted Z-score (Z08)"
gg.berlin.0[[1]]$labels$x<-"Proportion of tile covered by greenspace"
gg.berlin.0[[1]]$labels$title<-"Berlin - recommend not leaving house with exceptions"
gg.berlin.0[[1]]$labels$subtitle<-"week days"
gg.berlin.0[[2]]$labels$subtitle<-"weekend"


gg.berlin.1<-plot(Berlin.final.effect2,limits=c(-4.5,3),colors= c("#4DAF4A","#377EB8","#E41A1C"),show.y.title=FALSE,show.x.title=FALSE,show.title=FALSE)
#need to reverse the color scale as 1 is high is 4 is low in Berlin
gg.berlin.1[[1]]$labels$y<-"Predicted Z-score (Z08)"
gg.berlin.1$labels$x<-"Proportion of tile covered by greenspace"
gg.berlin.1[[1]]$labels$title<-"Berlin require not leaving house with exceptions"
gg.berlin.1[[1]]$labels$subtitle<-"week days"
gg.berlin.1[[2]]$labels$subtitle<-"weekend"


#################################MANUSCRIPT FIGURE LAST BERLIN 0800 ALL
library(grid)
library(gridExtra)
library(ggplotify)
tiff(file="C:/Users/David/OneDrive - Danmarks Tekniske Universitet/Facebook Greenspace ms/Figure_berlin_Z_0800_repeats.tiff", width=40,height=40,units="cm",res=200)
grid.arrange(as.grob(gg.berlin.0),as.grob(gg.berlin.1),nrow=2,heights=c(10,10))
dev.off()

####################################################################################################################################################
#####################################################################################################################################################
############################MANUSCRIPT FIGURE LAST BERLIN 0800 LOCKDOWN ONLY

gg.berlin.1<-plot(Berlin.final.effect2,limits=c(-2.75,2.5),colors= c("#4DAF4A","#377EB8","#E41A1C"),show.y.title=FALSE,show.x.title=FALSE,show.title=FALSE)
#need to reverse the color scale as 1 is high is 4 is low in Berlin
gg.berlin.1[[1]]$labels$y<-"Predicted Z-score (Z08)"
gg.berlin.1$labels$x<-"Proportion of tile covered by greenspace"
gg.berlin.1[[1]]$labels$subtitle<-"Berlin - week days"
gg.berlin.1[[2]]$labels$subtitle<-"Berlin - weekend"
tiff(file="C:/Users/David/OneDrive - Danmarks Tekniske Universitet/Facebook Greenspace ms/Figure_berlin_Z_0800_repeats_LOCKDOWN_ONLY.tiff", width=40,height=20,units="cm",res=200)
gg.berlin.1
dev.off()

####################################################################################################################################################
#####################################################################################################################################################
####################################################################################################################################################
#####################################################################################################################################################
####################################################################################################################################################
#####################################################################################################################################################
####################################################################################################################################################
#####################################################################################################################################################

library(grid)
library(gridExtra)
library(ggplotify)
tiff(file="C:/Users/David/OneDrive - Danmarks Tekniske Universitet/Facebook Greenspace ms/Figure_berlin_Z_CONT_repeats.tiff", width=40,height=40,units="cm",res=200)
grid.arrange(as.grob(gg.berlin.0),as.grob(gg.berlin.1),nrow=2,heights=c(10,10))
dev.off()

#############################################################################################################################

library(grid)
library(gridExtra)
library(ggplotify)
tiff(file="C:/Users/David/OneDrive - Danmarks Tekniske Universitet/Facebook Greenspace ms/Figure_berlin_Z_lockdownonly_0800_repeats.tiff", width=40,height=40,units="cm",res=200)
grid.arrange(as.grob(gg.berlin.0),as.grob(gg.berlin.1),nrow=2,heights=c(10,10))
dev.off()

save(gg.berlin.0,gg.berlin.1,file="C:/Users/David/OneDrive - Danmarks Tekniske Universitet/Facebook Greenspace ms/berlin_GROB_lockdown_wk.Rdata")


####################################################################################################################################################
#####################################################################################################################################################

 png(file="C:/Users/David/Documents/Facebook COVID/Berlin_lockdown_main_final_REPEATS_CONTRAST_lockdown1.png", width=60,height=42,units="cm",res=200)
 plot(Berlin.final.effect,limits=c(-4,3))
 dev.off()

Berlin.final.effect<-ggpredict(ber.lme.final,terms=c("coverage_area [all]","MSS [1,2,3,4]","repeats","weekend"),condition=c(lockdown="2"))
 png(file="C:/Users/David/Documents/Facebook COVID/Berlin_lockdown_main_final_REPEATS_CONTRAST_lockdown2.png", width=60,height=42,units="cm",res=200)
 plot(Berlin.final.effect,limits=c(-4,3))
 dev.off()


##############################################################################################################################
################################################################################################################################
###################################################################################################################################

##########################################################################################################################################
#####MANUSCRIPT FIGURE BERLIN Z CONTRAST WAVE 1
Berlin.final.effect1<-ggpredict(ber.lme.final,terms=c("coverage_area [all]","MSS [4,3,1]","repeats","weekend"),condition=c(lockdown="1"))
Berlin.final.effect2<-ggpredict(ber.lme.final,terms=c("coverage_area [all]","MSS [4,3,1]","repeats","weekend"),condition=c(lockdown="2"))



gg.berlin.0<-plot(Berlin.final.effect1,limits=c(-2,2),show.y.title=FALSE,show.x.title=FALSE,show.title=FALSE)

gg.berlin.0[[1]]$labels$y<-"Predicted Z-score (Z08-00)"
gg.berlin.0[[1]]$labels$title<-"Berlin - contrasting day and night - recommend not leaving house"

gg.berlin.2<-plot(Berlin.final.effect2,limits=c(-2,2),limit.range=FALSE,show.y.title=FALSE,show.x.title=FALSE,show.title=FALSE)
gg.berlin.2[[1]]$labels$y<-"Predicted Z-score (Z08-00)"
gg.berlin.2[[1]]$labels$title<-"Berlin - contrasting day and night - require not leaving house with exceptions"
gg.berlin.2[[1]]$labels$x<-"Proportion of tile covered by greenspace"
gg.berlin.2[[2]]$labels$x<-"Proportion of tile covered by greenspace"


library(grid)
library(gridExtra)
library(ggplotify)
tiff(file="C:/Users/David/OneDrive - Danmarks Tekniske Universitet/Facebook Greenspace ms/Figure_berlin_Z_contrast_repeats.tiff", width=40,height=40,units="cm",res=200)
grid.arrange(as.grob(gg.berlin.0),as.grob(gg.berlin.2),nrow=2,heights=c(10,10))
 dev.off()


##########################################################################################################################################################
###########################################################################################################################################################


##############################################################################################################################
################################################################################################################################
###################################################################################################################################
###################################################################################################################################
##### repeat these analyses 0800

#ber.bi.cont

#################################################
####given we do not have a 0 repeats we tackle the questions in two steps:
# lockdown 1 and 2 repeats
#no repeats (subset repeat 1) 0 v 1 v 2

#let's start there.


load("C:/Users/David/Documents/Facebook COVID/Twitter/tweet.volume.df.all.Rdata")

berlin_contrast0800$repeats<-tweet.vol.ber$repeats[match(as.Date(berlin_contrast0800$dates),tweet.vol.ber$date)]
rm(tweet.vol.lon,tweet.vol.ber,tweet.vol.par)
table(berlin_contrast0800$lockdown,berlin_contrast0800$repeats)


berlin_contrast0800$repeats<-as.numeric(berlin_contrast0800$repeats)
berlin_contrast0800$repeats[berlin_contrast0800$lockdown=="0"]<-berlin_contrast0800$repeats[berlin_contrast0800$lockdown=="0"]-1 #we start in March
berlin_contrast0800$repeats[berlin_contrast0800$lockdown=="1"]<-berlin_contrast0800$repeats[berlin_contrast0800$lockdown=="1"]-1 #we start in March

berlin_contrast0800$repeats<-factor(berlin_contrast0800$repeats)


ber.pop.wave1<-subset(berlin_contrast0800,repeats=="1")


###TO DO
formula<-c("clipped_z_score~lockdown",
"clipped_z_score~lockdown*weekend",
"clipped_z_score~lockdown*MSS",
"clipped_z_score~lockdown*weekend*MSS",
"clipped_z_score~lockdown*coverage_area",
"clipped_z_score~lockdown*weekend*coverage_area",
"clipped_z_score~lockdown*MSS*coverage_area",
"clipped_z_score~lockdown*weekend*MSS*coverage_area"
)


AIC.df.w<-data.frame(model=formula, AIC=NA, time=0)

for (i in 1:length(formula)) {
print(formula[i])
flush.console()
tic<-Sys.time()
ber.lme.final<-lmer(as.formula(paste0(formula[i],"+ (1|dated) + (1|quadkeys)")) ,data=ber.pop.wave1,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
AIC.df.w$time[i]<-Sys.time()-tic
AIC.df.w$AIC[i]<-AIC(ber.lme.final)
rm(ber.lme.final)
gc()
}
AIC.df.w

save(AIC.df.w,formula,file="C:/Users/David/Documents/Facebook COVID/modelselection_Berlin_CONTRAST_wave1_FB.Rdata")

ber.lme.final<-lmer(as.formula(paste0(formula[i],"+ (1|dated) + (1|quadkeys)")) ,data=ber.pop.wave1,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))


Berlin.final.effect<-ggpredict(ber.lme.final,terms=c("coverage_area [all]","MSS [4,3,1]","lockdown","weekend"))
 
##########################################################################################################################################
#####MANUSCRIPT FIGURE BERLIN Z 0800 WAVE 1

gg.berlin<-plot(Berlin.final.effect,limits=c(-4,3),limit.range=FALSE,show.y.title=FALSE,show.x.title=FALSE,show.title=FALSE)
gg.berlin[[1]]$labels$y<-"Predicted Z-score (Z08)"
gg.berlin[[2]]$labels$x<-"Proportion of tile covered by greenspace"

tiff(file="C:/Users/David/OneDrive - Danmarks Tekniske Universitet/Facebook Greenspace ms/Figure_Berlin_Z_0800_wave1.tiff", width=20,height=20,units="cm",res=200)
gg.berlin
dev.off()

##########################################
#### repeats

ber.pop.rep<-subset(berlin_contrast0800,lockdown!="0")

ber.pop.rep$lockdown<-factor(ber.pop.rep$lockdown)

str(ber.pop.rep)

table(ber.pop.rep$lockdown,ber.pop.rep$repeats)
ber.pop.rep<-subset(ber.pop.rep,repeats!="4")
ber.pop.rep$repeats<-factor(ber.pop.rep$repeats)


formula<-c("clipped_z_score~lockdown",
"clipped_z_score~lockdown*repeats",
"clipped_z_score~lockdown*weekend",
"clipped_z_score~lockdown*repeats*weekend",
"clipped_z_score~lockdown*MSS",
"clipped_z_score~lockdown*repeats*MSS",
"clipped_z_score~lockdown*weekend*MSS",
"clipped_z_score~lockdown*repeats*weekend*MSS",
"clipped_z_score~lockdown*coverage_area",
"clipped_z_score~lockdown*repeats*coverage_area",
"clipped_z_score~lockdown*weekend*coverage_area",
"clipped_z_score~lockdown*repeats*weekend*coverage_area",
"clipped_z_score~lockdown*MSS*coverage_area",
"clipped_z_score~lockdown*repeats*MSS*coverage_area",
"clipped_z_score~lockdown*weekend*MSS*coverage_area",
"clipped_z_score~lockdown*repeats*weekend*MSS*coverage_area"

)


AIC.df.r<-data.frame(model=formula, AIC=NA, time=0)

for (i in 1:length(formula)) {
print(formula[i])
flush.console()
tic<-Sys.time()
ber.lme.final<-lmer(as.formula(paste0(formula[i],"+ (1|dated) + (1|quadkeys)")) ,data=ber.pop.rep,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
AIC.df.r$time[i]<-Sys.time()-tic
AIC.df.r$AIC[i]<-AIC(ber.lme.final)
rm(ber.lme.final)
gc()
}
AIC.df.r

save(AIC.df.r,formula,file="C:/Users/David/Documents/Facebook COVID/modelselection_Berlin_CONTRAST_repeats_FB.Rdata")

ber.lme.final<-lmer(as.formula(paste0(formula[i],"+ (1|dated) + (1|quadkeys)")) ,data=ber.pop.rep,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))


##############################################################################################################################
################################################################################################################################
###################################################################################################################################

##########################################################################################################################################
#####MANUSCRIPT FIGURE BERLIN Z 0800 repeats
Berlin.final.effect1<-ggpredict(ber.lme.final,terms=c("coverage_area [all]","MSS [4,3,1]","repeats","weekend"),condition=c(lockdown="1"))
Berlin.final.effect2<-ggpredict(ber.lme.final,terms=c("coverage_area [all]","MSS [4,3,1]","repeats","weekend"),condition=c(lockdown="2"))



gg.berlin.0<-plot(Berlin.final.effect1,limits=c(-4,3),show.y.title=FALSE,show.x.title=FALSE,show.title=FALSE)

gg.berlin.0[[1]]$labels$y<-"Predicted Z-score (Z08)"
gg.berlin.0[[1]]$labels$title<-"Berlin - contrasting day and night - recommend not leaving house"

gg.berlin.2<-plot(Berlin.final.effect2,limits=c(-4,3),limit.range=FALSE,show.y.title=FALSE,show.x.title=FALSE,show.title=FALSE)
gg.berlin.2[[1]]$labels$y<-"Predicted Z-score (Z08)"
gg.berlin.2[[1]]$labels$title<-"Berlin - contrasting day and night - require not leaving house with exceptions"
gg.berlin.2[[1]]$labels$x<-"Proportion of tile covered by greenspace"
gg.berlin.2[[2]]$labels$x<-"Proportion of tile covered by greenspace"


library(grid)
library(gridExtra)
library(ggplotify)
tiff(file="C:/Users/David/OneDrive - Danmarks Tekniske Universitet/Facebook Greenspace ms/Figure_berlin_Z_0800_repeats.tiff", width=40,height=40,units="cm",res=200)
grid.arrange(as.grob(gg.berlin.0),as.grob(gg.berlin.2),nrow=2,heights=c(10,10))
 dev.off()


##########################################
#### repeats lockdown 2 only

ber.pop.rep<-subset(berlin_contrast0800,lockdown=="2")
ber.pop.rep<-subset(ber.pop.rep,repeats!="4")
ber.pop.rep$repeats<-factor(ber.pop.rep$repeats)


formula<-c(
"clipped_z_score~repeats*weekend*MSS*coverage_area"

)

i=1
ber.lme.final<-lmer(as.formula(paste0(formula[i],"+ (1|dated) + (1|quadkeys)")) ,data=ber.pop.rep,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))


##############################################################################################################################
################################################################################################################################
###################################################################################################################################

##########################################################################################################################################
#####MANUSCRIPT FIGURE BERLIN Z 0800 repeats lockdown only
Berlin.final.effect1<-ggpredict(ber.lme.final,terms=c("coverage_area [all]","MSS [4,3,1]","repeats"),condition=c(weekend="no"))
Berlin.final.effect2<-ggpredict(ber.lme.final,terms=c("coverage_area [all]","MSS [4,3,1]","repeats"),condition=c(weekend="yes"))



gg.berlin.0<-plot(Berlin.final.effect1,limits=c(-4,3),colors= c("#4DAF4A","#377EB8","#E41A1C"),show.y.title=FALSE,show.x.title=FALSE,show.title=FALSE)
#need to reverse the color scale as 1 is high is 4 is low in Berlin

gg.berlin.0$labels$y<-"Predicted Z-score (Z08)"
gg.berlin.0$labels$x<-"Proportion of tile covered by greenspace"
gg.berlin.0$labels$title<-"Berlin - week days"


gg.berlin.1<-plot(Berlin.final.effect2,limits=c(-4,3),colors= c("#4DAF4A","#377EB8","#E41A1C"),show.y.title=FALSE,show.x.title=FALSE,show.title=FALSE)
#need to reverse the color scale as 1 is high is 4 is low in Berlin

gg.berlin.1$labels$y<-"Predicted Z-score (Z08)"
gg.berlin.1$labels$x<-"Proportion of tile covered by greenspace"
gg.berlin.1$labels$title<-"Berlin - weekend"

library(grid)
library(gridExtra)
library(ggplotify)
tiff(file="C:/Users/David/OneDrive - Danmarks Tekniske Universitet/Facebook Greenspace ms/Figure_berlin_Z_lockdownonly_0800_repeats.tiff", width=40,height=40,units="cm",res=200)
grid.arrange(as.grob(gg.berlin.0),as.grob(gg.berlin.1),nrow=2,heights=c(10,10))
dev.off()

save(gg.berlin.0,gg.berlin.1,file="C:/Users/David/OneDrive - Danmarks Tekniske Universitet/Facebook Greenspace ms/berlin_GROB_lockdown_wk.Rdata")


