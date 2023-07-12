###PARIS
# we replicate London's analyses in PARIS

#first we assign IMD and coverage_area and lockdown OxGRT to quadkeys


#OxGRT codebook is at: https://github.com/OxCGRT/covid-policy-tracker/blob/master/documentation/codebook.md

#we are after C6 - stay at home
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

ParisIMD<-geojson_sf(paste0(deprivation_folder,"/paris/Paris-indice-de-defavorisation-sociale-fdep-par-iris.geojson"))
ParisIMD<-ParisIMD[grepl("PARIS",ParisIMD$c_nom_com),]

#IMD is the first eigenvector of population weighted PCA

ParisIMD$FDep<-princomp(data.frame(rev=ParisIMD$t1_rev_med*sqrt(ParisIMD$t1_p09_pop),bac=ParisIMD$t1_txbac09*sqrt(ParisIMD$t1_p09_pop),chom=ParisIMD$t1_txchom0*sqrt(ParisIMD$t1_p09_pop),ouvr=ParisIMD$t1_txouvr0*sqrt(ParisIMD$t1_p09_pop)),cor=TRUE)$scores[,1]

par.pop.files<-list.files("C:/Users/David/Documents/Facebook COVID/Facebook Paris",full.names=TRUE)

#ParisIMD and Parisgreenspace are both crs 4326

par.pop<-read.csv(par.pop.files[1],header=T)

###########
earth_circumference_m<-6378137*2*pi              
earth_circumference<-360 
##################

par.pop$tile_size_lat<-(cos(par.pop$lat * pi/180) * earth_circumference)/(2^(nchar(as.character(par.pop$quadkey))))
par.pop$tile_size_lon<-(cos(par.pop$lon * pi/180) * earth_circumference)/(2^(nchar(as.character(par.pop$quadkey))))
par.pop$lon.top<-par.pop$lon+(par.pop$tile_size_lon/2)
par.pop$lon.bot<-par.pop$lon-(par.pop$tile_size_lon/2)
par.pop$lat.top<-par.pop$lat+(par.pop$tile_size_lat/2)
par.pop$lat.bot<-par.pop$lat-(par.pop$tile_size_lat/2)

par.pop$gardens<-0
par.pop$coverage<-0


gardens.quadkey<-list()
gardens.quadkey.df<-data.frame(quadkey=0,gardens=0,coverage=0)



##############################################
#### inner file loop
#############################################
for (i in 1:dim(par.pop)[1]) {

try.sf<-st_sfc(st_polygon(list(rbind(c(par.pop$lon.top[i],par.pop$lat.top[i]),c(par.pop$lon.bot[i],par.pop$lat.top[i]),c(par.pop$lon.bot[i],par.pop$lat.bot[i]),c(par.pop$lon.top[i],par.pop$lat.bot[i]),c(par.pop$lon.top[i],par.pop$lat.top[i])))))
st_crs(try.sf)<-4326
try.inter<-st_intersection(Parisgreenspace,try.sf)

if (length(try.inter$nsq_espace_)>0) {
par.pop$gardens[i]<-length(try.inter$nsq_espace_)
par.pop$coverage[i]<-as.numeric(sum(st_area(try.inter)))
gardens.quadkey[[paste(par.pop$quadkey[i])]]<-data.frame(name=as.array(as.character(try.inter$nsq_espace_)),area=as.numeric(st_area(try.inter))) #let's keep area here too for when we subset some greenspace categories away
gardens.quadkey.df<-rbind(gardens.quadkey.df,data.frame(quadkey=par.pop$quadkey[i],gardens=length(try.inter$nsq_espace_),coverage=as.numeric(sum(st_area(try.inter)))))

#print(i)
#flush.console()

}  else {

gardens.quadkey.df<-rbind(gardens.quadkey.df,data.frame(quadkey=par.pop$quadkey[i],gardens=0,coverage=0))

}
}
par.pop$coverage_area<-par.pop$coverage/((par.pop$tile_size_lat*earth_circumference_m/360)*(par.pop$tile_size_lon*earth_circumference_m/360))

gardens.quadkey.df<-gardens.quadkey.df[-1,]


#######################################################
########inter-file loop starts here

for (j in 2:length(par.pop.files)) {


par.pop.it<-read.csv(par.pop.files[j],header=T)

par.pop.it$tile_size_lat<-(cos(par.pop.it$lat * pi/180) * earth_circumference)/(2^(nchar(as.character(par.pop.it$quadkey))))
par.pop.it$tile_size_lon<-(cos(par.pop.it$lon * pi/180) * earth_circumference)/(2^(nchar(as.character(par.pop.it$quadkey))))
par.pop.it$lon.top<-par.pop.it$lon+(par.pop.it$tile_size_lon/2)
par.pop.it$lon.bot<-par.pop.it$lon-(par.pop.it$tile_size_lon/2)
par.pop.it$lat.top<-par.pop.it$lat+(par.pop.it$tile_size_lat/2)
par.pop.it$lat.bot<-par.pop.it$lat-(par.pop.it$tile_size_lat/2)

par.pop.it$gardens<-0
par.pop.it$coverage<-0


for (i in 1:dim(par.pop.it)[1]) {  #I know it does not look it, but practically it is more time efficient

#######
##### let's first compare the quadkeys to the preexisting quadkeys
existing.quadkey<-which(par.pop.it$quadkey[i]==gardens.quadkey.df$quadkey,arr.ind=TRUE)
if (length(existing.quadkey)>0) {

par.pop.it$gardens[i]<-gardens.quadkey.df$gardens[existing.quadkey]
par.pop.it$coverage[i]<-gardens.quadkey.df$coverage[existing.quadkey]

} else {

try.sf<-st_sfc(st_polygon(list(rbind(c(par.pop.it$lon.top[i],par.pop.it$lat.top[i]),c(par.pop.it$lon.bot[i],par.pop.it$lat.top[i]),c(par.pop.it$lon.bot[i],par.pop.it$lat.bot[i]),c(par.pop.it$lon.top[i],par.pop.it$lat.bot[i]),c(par.pop.it$lon.top[i],par.pop.it$lat.top[i])))))
st_crs(try.sf)<-4326
try.inter<-st_intersection(Parisgreenspace,try.sf)

if (length(try.inter$nsq_espace_)>0) {
par.pop.it$gardens[i]<-length(try.inter$nsq_espace_)
par.pop.it$coverage[i]<-as.numeric(sum(st_area(try.inter)))
gardens.quadkey[[paste(par.pop.it$quadkey[i])]]<-data.frame(name=as.array(as.character(try.inter$nsq_espace_)),area=as.numeric(st_area(try.inter))) #let's keep area here too for when we subset some greenspace categories away
gardens.quadkey.df<-rbind(gardens.quadkey.df,data.frame(quadkey=par.pop.it$quadkey[i],gardens=length(try.inter$nsq_espace_),coverage=as.numeric(sum(st_area(try.inter)))))

}  else {

gardens.quadkey.df<-rbind(gardens.quadkey.df,data.frame(quadkey=par.pop.it$quadkey[i],gardens=0,coverage=0))

} #ifelse intersect

} #ifelse preexisting

} #iteration

par.pop.it$coverage_area<-par.pop.it$coverage/((par.pop.it$tile_size_lat*earth_circumference_m/360)*(par.pop.it$tile_size_lon*earth_circumference_m/360))

par.pop<-rbind(par.pop,par.pop.it)

print(j)
flush.console()

}


save(lockdown.paris,ParisIMD,Parisgreenspace,par.pop,gardens.quadkey.df,gardens.quadkey,file="C:/Users/David/Documents/Facebook COVID/paris_corona_greenspace_Aug_2021.Rdata")


par.pop$dated<-factor(substr(par.pop$date_time,1,10))
par.pop$time<-factor(substr(par.pop$date_time,12,15))

par.pop$dates<-as.POSIXct(par.pop$dated, format="%Y-%m-%d")

save(lockdown.paris,ParisIMD,Parisgreenspace,par.pop,gardens.quadkey.df,gardens.quadkey,file="C:/Users/David/Documents/Facebook COVID/paris_corona_greenspace_Aug_2021.Rdata")



par.pop$lockdown<-0
par.pop$lockdown<-lockdown.paris$C6_Stay_at_home_requirements[match(as.Date(par.pop$dates),lockdown.paris$date.pos)] #much faster than merge
par.pop$lockdown<-factor(par.pop$lockdown)

save(lockdown.paris,ParisIMD,Parisgreenspace,par.pop,gardens.quadkey.df,gardens.quadkey,file="C:/Users/David/Documents/Facebook COVID/paris_corona_greenspace_Aug_2021.Rdata")



###so we have to be a bit careful with the funky shape of the IMD and greenspace data, here we will us IMD data to limit the geographic scope
#of the analysis (like we did in London) we set IMD to NA, we should be able to get an IMD value for all qudkeys that overlap the FDep data
# if the quadkey IMD is still NA post aggregation, then the quadkeys are removed.

par.pop$FDep<-NA

#########################

quadkey.imd<-data.frame(quadkey=unique(par.pop$quadkey),FDep=NA)

iter<-match(quadkey.imd$quadkey,par.pop$quadkey)

m<-1
for (i in iter) {

try.sf<-st_sfc(st_polygon(list(rbind(c(par.pop$lon.top[i],par.pop$lat.top[i]),c(par.pop$lon.bot[i],par.pop$lat.top[i]),c(par.pop$lon.bot[i],par.pop$lat.bot[i]),c(par.pop$lon.top[i],par.pop$lat.bot[i]),c(par.pop$lon.top[i],par.pop$lat.top[i])))))
st_crs(try.sf)<-4326
try.inter<-st_intersection(ParisIMD,try.sf)

if (length(try.inter$c_dcomiris)>0) {
	quadkey.imd$FDep[m]<-median(try.inter$FDep) #LSOA$IMD does not contain any NAs so if the intersection length >0 we are safe to take the median without removing NAs
}


print(i)
flush.console()
m<-m+1

} 

quadkey.imd$lat<-par.pop$lat[iter]
quadkey.imd$lon<-par.pop$lon[iter]
#quadkey.imd.map<-st_as_sf(quadkey.imd,coords=c("lon","lat"),crs=4326)

save(quadkey.imd,file="C:/Users/David/Documents/deprivation_indices/london/paris_corona_IMD_quadkey.Rdata")


par.pop$FDep<-quadkey.imd$FDep[match(par.pop$quadkey,quadkey.imd$quadkey)]


save(par.pop,file="C:/Users/David/Documents/Facebook COVID/paris_corona_FB_Aug_2021.Rdata")

par.pop.periph<-par.pop[!is.na(par.pop$FDep),]

save(par.pop.periph,file="C:/Users/David/Documents/Facebook COVID/paris_corona_FB_periph_Aug_2021.Rdata")

#########################

par.pop.periph$weekday<-weekdays(par.pop.periph$dates)
par.pop.periph$weekend<-"no"
par.pop.periph$weekend[par.pop.periph$weekday=="Saturday"]<-"yes"
par.pop.periph$weekend[par.pop.periph$weekday=="Sunday"]<-"yes"
par.pop.periph$weekend<-factor(par.pop.periph$weekend)
par.pop.periph$weekday<-factor(par.pop.periph$weekday)

par.pop.periph$time[par.pop.periph$time=="00:0"]<-"0000"
par.pop.periph$time[par.pop.periph$time=="08:0"]<-"0800"
par.pop.periph$time[par.pop.periph$time=="16:0"]<-"1600"
par.pop.periph$time<-factor(as.character(par.pop.periph$time))
par.pop.periph$quadkeys<-factor(par.pop.periph$quadkey)

save(par.pop.periph,file="C:/Users/David/Documents/Facebook COVID/paris_corona_FB_periph_Aug_2021.Rdata")

library(lme4)
par.lme.final<-lmer(clipped_z_score~lockdown*weekend*time*FDep*coverage_area+ (1|dated) + (1|quadkeys) ,data=par.pop.periph,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))

library(ggeffects)
##plot 0000

paris.final.effect<-ggpredict(par.lme.final,terms=c("coverage_area [all]","FDep [-3, 0,3]","lockdown","weekend"),condition=c(time="0800"))
 png(file="C:/Users/David/Documents/Facebook COVID/Paris_lockdown_main_final_0800.png", width=60,height=42,units="cm",res=200)
 plot(paris.final.effect,limits=c(-4,3))
 dev.off()
paris.final.effect<-ggpredict(par.lme.final,terms=c("coverage_area [all]","FDep [-3, 0,3]","lockdown","weekend"),condition=c(time="0000"))
 png(file="C:/Users/David/Documents/Facebook COVID/Paris_lockdown_main_final_0000.png", width=60,height=42,units="cm",res=200)
 plot(paris.final.effect,limits=c(-4,3))
 dev.off()
paris.final.effect<-ggpredict(par.lme.final,terms=c("coverage_area [all]","FDep [-3, 0,3]","lockdown","weekend"),condition=c(time="1600"))
 png(file="C:/Users/David/Documents/Facebook COVID/Paris_lockdown_main_final_1600.png", width=60,height=42,units="cm",res=200)
 plot(paris.final.effect,limits=c(-4,3))
 dev.off()

###nope FDep is the right way around negative is derpived, so yes the effect is there but reverse compared to London!


library(lme4)
formula<-c("clipped_z_score~lockdown",
"clipped_z_score~lockdown*weekend",
"clipped_z_score~lockdown*FDep",
"clipped_z_score~lockdown*weekend*FDep",
"clipped_z_score~lockdown*coverage_area",
"clipped_z_score~lockdown*weekend*coverage_area",
"clipped_z_score~lockdown*FDep*coverage_area",
"clipped_z_score~lockdown*weekend*FDep*coverage_area",
"clipped_z_score~lockdown*time",
"clipped_z_score~lockdown*weekend*time",
"clipped_z_score~lockdown*FDep*time",
"clipped_z_score~lockdown*weekend*FDep*time",
"clipped_z_score~lockdown*coverage_area*time",
"clipped_z_score~lockdown*weekend*coverage_area*time",
"clipped_z_score~lockdown*FDep*coverage_area*time",
"clipped_z_score~lockdown*weekend*FDep*coverage_area*time"
)

AIC.df<-data.frame(model=formula, AIC=NA, time=0)
for (i in 1:length(formula)) {
print(formula[i])
flush.console()
tic<-Sys.time()
par.lme.final<-lmer(as.formula(paste0(formula[i],"+ (1|dated) + (1|quadkeys)")) ,data=par.pop.periph,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
AIC.df$time[i]<-Sys.time()-tic
AIC.df$AIC[i]<-AIC(par.lme.final)
rm(par.lme.final)
gc()
}

save(AIC.df,formula,file="C:/Users/David/Documents/Facebook COVID/modelselection_Paris_norepeats_FB.Rdata")

#introducing repeats

load("C:/Users/David/Documents/Facebook COVID/Twitter/tweet.volume.df.all.Rdata")

par.pop.periph$repeats<-tweet.vol.par$repeats[match(as.Date(par.pop.periph$dates),tweet.vol.par$date)]
rm(tweet.vol.lon,tweet.vol.ber,tweet.vol.par)
table(par.pop.periph$lockdown,par.pop.periph$repeats)


par.pop.periph$repeats<-as.numeric(par.pop.periph$repeats)
par.pop.periph$repeats[par.pop.periph$lockdown=="0"]<-par.pop.periph$repeats[par.pop.periph$lockdown=="0"]-1 #we start in March
par.pop.periph$repeats<-factor(par.pop.periph$repeats)

###we only have one sample of lockdown phase 1 so we subset to 0 and 2 only for this part of the model selection

par.pop.periph.sub<-subset(par.pop.periph,lockdown!="1")
par.pop.periph.sub$lockdown<-factor(as.numeric(par.pop.periph.sub$lockdown))


formula<-c("clipped_z_score~lockdown",
"clipped_z_score~lockdown*repeats",
"clipped_z_score~lockdown*weekend",
"clipped_z_score~lockdown*repeats*weekend",
"clipped_z_score~lockdown*FDep",
"clipped_z_score~lockdown*repeats*FDep",
"clipped_z_score~lockdown*weekend*FDep",
"clipped_z_score~lockdown*repeats*weekend*FDep",
"clipped_z_score~lockdown*coverage_area",
"clipped_z_score~lockdown*repeats*coverage_area",
"clipped_z_score~lockdown*weekend*coverage_area",
"clipped_z_score~lockdown*repeats*weekend*coverage_area",
"clipped_z_score~lockdown*FDep*coverage_area",
"clipped_z_score~lockdown*repeats*FDep*coverage_area",
"clipped_z_score~lockdown*weekend*FDep*coverage_area",
"clipped_z_score~lockdown*repeats*weekend*FDep*coverage_area",
"clipped_z_score~lockdown*time",
"clipped_z_score~lockdown*repeats*time",
"clipped_z_score~lockdown*weekend*time",
"clipped_z_score~lockdown*repeats*weekend*time",
"clipped_z_score~lockdown*FDep*time",
"clipped_z_score~lockdown*repeats*FDep*time",
"clipped_z_score~lockdown*weekend*FDep*time",
"clipped_z_score~lockdown*repeats*weekend*FDep*time",
"clipped_z_score~lockdown*coverage_area*time",
"clipped_z_score~lockdown*repeats*coverage_area*time",
"clipped_z_score~lockdown*weekend*coverage_area*time",
"clipped_z_score~lockdown*repeats*weekend*coverage_area*time",
"clipped_z_score~lockdown*FDep*coverage_area*time",
"clipped_z_score~lockdown*repeats*FDep*coverage_area*time",
"clipped_z_score~lockdown*weekend*FDep*coverage_area*time",
"clipped_z_score~lockdown*repeats*weekend*time*FDep*coverage_area"
)


AIC.df.r<-data.frame(model=formula, AIC=NA, time=0)
for (i in 5:length(formula)) {
print(formula[i])
flush.console()
tic<-Sys.time()
par.lme.final<-lmer(as.formula(paste0(formula[i],"+ (1|dated) + (1|quadkeys)")) ,data=par.pop.periph.sub,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
AIC.df.r$time[i]<-Sys.time()-tic
AIC.df.r$AIC[i]<-AIC(par.lme.final)
rm(par.lme.final)
gc()
}
AIC.df.r

save(AIC.df.r,formula,file="C:/Users/David/Documents/Facebook COVID/modelselection_Paris_wrepeats_FB.Rdata")

i=32
par.lme.final<-lmer(as.formula(paste0(formula[i],"+ (1|dated) + (1|quadkeys)")) ,data=par.pop.periph.sub,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))


library(ggeffects)
##plot repeats 1 #FDep ticks are 10% 50% and 90%

paris.final.effect<-ggpredict(par.lme.final,terms=c("coverage_area [all]","FDep [-2, -0.038,1.43]","lockdown","weekend"),condition=c(time="0800",repeats="1"))
 png(file="C:/Users/David/Documents/Facebook COVID/Paris_lockdown_main_final_0800_repeat1.png", width=60,height=42,units="cm",res=200)
 plot(paris.final.effect,limits=c(-4,3))
 dev.off()
paris.final.effect<-ggpredict(par.lme.final,terms=c("coverage_area [all]","FDep [-2, -0.038,1.43]","lockdown","weekend"),condition=c(time="0000",repeats="1"))
 png(file="C:/Users/David/Documents/Facebook COVID/Paris_lockdown_main_final_0000_repeat1.png", width=60,height=42,units="cm",res=200)
 plot(paris.final.effect,limits=c(-4,3))
 dev.off()
paris.final.effect<-ggpredict(par.lme.final,terms=c("coverage_area [all]","FDep [-2, -0.038,1.43]","lockdown","weekend"),condition=c(time="1600",repeats="1"))
 png(file="C:/Users/David/Documents/Facebook COVID/Paris_lockdown_main_final_1600_repeat1.png", width=60,height=42,units="cm",res=200)
 plot(paris.final.effect,limits=c(-4,3))
 dev.off()


##plot repeats 2

paris.final.effect<-ggpredict(par.lme.final,terms=c("coverage_area [all]","FDep [-2, -0.038,1.43]","lockdown","weekend"),condition=c(time="0800",repeats="2"))
 png(file="C:/Users/David/Documents/Facebook COVID/Paris_lockdown_main_final_0800_repeat2.png", width=60,height=42,units="cm",res=200)
 plot(paris.final.effect,limits=c(-4,3))
 dev.off()
paris.final.effect<-ggpredict(par.lme.final,terms=c("coverage_area [all]","FDep [-2, -0.038,1.43]","lockdown","weekend"),condition=c(time="0000",repeats="2"))
 png(file="C:/Users/David/Documents/Facebook COVID/Paris_lockdown_main_final_0000_repeat2.png", width=60,height=42,units="cm",res=200)
 plot(paris.final.effect,limits=c(-4,3))
 dev.off()
paris.final.effect<-ggpredict(par.lme.final,terms=c("coverage_area [all]","FDep [-2, -0.038,1.43]","lockdown","weekend"),condition=c(time="1600",repeats="2"))
 png(file="C:/Users/David/Documents/Facebook COVID/Paris_lockdown_main_final_1600_repeat2.png", width=60,height=42,units="cm",res=200)
 plot(paris.final.effect,limits=c(-4,3))
 dev.off()


####################################################################################
#####contrast approach
load("C:/Users/David/Documents/Facebook COVID/paris_corona_FB_periph_Aug_2021.Rdata")

paris_contrast0800<-subset(par.pop.periph,time=="0800")
paris_contrast0000<-subset(par.pop.periph,time=="0000")
paris_contrast<-merge(paris_contrast0800,paris_contrast0000,by.x=c("quadkeys","dated"),by.y=c("quadkeys","dated"),all.x=TRUE)
paris_contrast$z_score<-paris_contrast$clipped_z_score.x-paris_contrast$clipped_z_score.y
par.periph.cont<-paris_contrast[,c(1,2,57,23,25,26,27,29)]
names(par.periph.cont)<-c("quadkeys","dated","z_score","coverage_area","dates","lockdown","FDep","weekend")


load("C:/Users/David/Documents/Facebook COVID/Twitter/tweet.volume.df.all.Rdata")

par.periph.cont$repeats<-tweet.vol.par$repeats[match(as.Date(par.periph.cont$dates),tweet.vol.par$date)]
rm(tweet.vol.lon,tweet.vol.ber,tweet.vol.par)
table(par.periph.cont$lockdown,par.periph.cont$repeats)


par.periph.cont$repeats<-as.numeric(par.periph.cont$repeats)
par.periph.cont$repeats[par.periph.cont$lockdown=="0"]<-par.periph.cont$repeats[par.periph.cont$lockdown=="0"]-1 #we start in March
par.periph.cont$repeats<-factor(par.periph.cont$repeats)

###we only have one sample of lockdown phase 1 so we subset to 0 and 2 only for this part of the model selection

par.periph.cont.sub<-subset(par.periph.cont,lockdown!="1")
par.periph.cont.sub$lockdown<-factor(as.numeric(par.periph.cont.sub$lockdown))

library(lme4)
formula<-c("z_score~lockdown",
"z_score~lockdown*repeats",
"z_score~lockdown*weekend",
"z_score~lockdown*repeats*weekend",
"z_score~lockdown*FDep",
"z_score~lockdown*repeats*FDep",
"z_score~lockdown*weekend*FDep",
"z_score~lockdown*repeats*weekend*FDep",
"z_score~lockdown*coverage_area",
"z_score~lockdown*repeats*coverage_area",
"z_score~lockdown*weekend*coverage_area",
"z_score~lockdown*repeats*weekend*coverage_area",
"z_score~lockdown*FDep*coverage_area",
"z_score~lockdown*repeats*FDep*coverage_area",
"z_score~lockdown*weekend*FDep*coverage_area",
"z_score~lockdown*repeats*weekend*FDep*coverage_area"
)


AIC.df.c<-data.frame(model=formula, AIC=NA, time=0)
for (i in 1:length(formula)) {
print(formula[i])
flush.console()
tic<-Sys.time()
par.lme.final<-lmer(as.formula(paste0(formula[i],"+ (1|dated) + (1|quadkeys)")) ,data=par.periph.cont.sub,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
AIC.df.c$time[i]<-Sys.time()-tic
AIC.df.c$AIC[i]<-AIC(par.lme.final)
rm(par.lme.final)
gc()
}
AIC.df.c
#repeats as random effects instead: AIC: 592613.3

save(AIC.df.c,formula,file="C:/Users/David/Documents/Facebook COVID/modelselection_Paris_CONTRAST_wrepeats_FB.Rdata")

i=16 #best model
par.lme.final<-lmer(as.formula(paste0(formula[i],"+ (1|dated) + (1|quadkeys)")) ,data=par.periph.cont.sub,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))


library(ggeffects)
##plot repeats 1 #FDep ticks are 10% 50% and 90%

paris.final.effect0<-ggpredict(par.lme.final,terms=c("coverage_area [all]","FDep [-2, 0,2]","repeats","weekend"),condition=c(lockdown="1"))
paris.final.effect2<-ggpredict(par.lme.final,terms=c("coverage_area [all]","FDep [-2, 0,2]","repeats","weekend"),condition=c(lockdown="3"))


gg.paris.0<-plot(paris.final.effect0,limits=c(-2,2),show.y.title=FALSE,show.x.title=FALSE,show.title=FALSE)

gg.paris.0[[1]]$labels$y<-"Predicted difference in Z-score (Z08-Z00)"
gg.paris.0[[1]]$labels$title<-"Paris - contrasting day and night - no restriction measures"

gg.paris.2<-plot(paris.final.effect2,limits=c(-2,2),limit.range=FALSE,show.y.title=FALSE,show.x.title=FALSE,show.title=FALSE)
gg.paris.2[[1]]$labels$y<-"Predicted difference in Z-score (Z08-Z00)"
gg.paris.2[[1]]$labels$title<-"Paris - contrasting day and night - require not leaving house with exceptions"
gg.paris.2[[1]]$labels$x<-"Proportion of tile covered by greenspace"
gg.paris.2[[2]]$labels$x<-"Proportion of tile covered by greenspace"


library(grid)
library(gridExtra)
library(ggplotify)
###############################################################
##########MANUSCRIPT FIGURE PARIS CONTRAST
 tiff(file="C:/Users/David/OneDrive - Danmarks Tekniske Universitet/Facebook Greenspace ms/Figure_Paris_Z_contrast.tiff", width=40,height=40,units="cm",res=200)
grid.arrange(as.grob(gg.paris.0),as.grob(gg.paris.2),nrow=2,heights=c(10,10))
 dev.off()


####################### MANUSCRIPT FIGURE LOCKDOWN ONLY

gg.paris.2<-plot(paris.final.effect2,limits=c(-2,2),limit.range=FALSE,show.y.title=FALSE,show.x.title=FALSE,show.title=FALSE)
gg.paris.2[[1]]$labels$y<-"Predicted difference in Z-score (Z08-Z00)"
gg.paris.2[[1]]$labels$subtitle<-"Paris - week days"
gg.paris.2[[2]]$labels$subtitle<-"Paris - weekend"
gg.paris.2[[2]]$labels$x<-"Proportion of tile covered by greenspace"


 tiff(file="C:/Users/David/OneDrive - Danmarks Tekniske Universitet/Facebook Greenspace ms/Figure_Paris_Z_contrast_LOCKDOWN_ONLY.tiff", width=40,height=20,units="cm",res=200)
gg.paris.2
 dev.off()



############################################
paris.final.effect<-ggpredict(par.lme.final,terms=c("coverage_area [all]","FDep [-2, 0,2]","repeats","weekend"),condition=c(lockdown="3"))
 png(file="C:/Users/David/Documents/Facebook COVID/Paris_lockdown_main_final_CONTRAST_repeat2.png", width=60,height=42,units="cm",res=200)
 plot(paris.final.effect,limits=c(-2,2))
 dev.off()
#####################################

##############################################################################################################################################
##############################################################################################################################################
#### 08-1600

####################################################################################
#####0800 approach
load("C:/Users/David/Documents/Facebook COVID/paris_corona_FB_periph_Aug_2021.Rdata")

paris_contrast0800<-subset(par.pop.periph,time=="0800")


load("C:/Users/David/Documents/Facebook COVID/Twitter/tweet.volume.df.all.Rdata")

paris_contrast0800$repeats<-tweet.vol.par$repeats[match(as.Date(paris_contrast0800$dates),tweet.vol.par$date)]
rm(tweet.vol.lon,tweet.vol.ber,tweet.vol.par)
table(paris_contrast0800$lockdown,paris_contrast0800$repeats)


paris_contrast0800$repeats<-as.numeric(paris_contrast0800$repeats)
paris_contrast0800$repeats[paris_contrast0800$lockdown=="0"]<-paris_contrast0800$repeats[paris_contrast0800$lockdown=="0"]-1 #we start in March
paris_contrast0800$repeats<-factor(paris_contrast0800$repeats)

###we only have one sample of lockdown phase 1 so we subset to 0 and 2 only for this part of the model selection

par.periph.cont.sub<-subset(paris_contrast0800,lockdown!="1")
par.periph.cont.sub$lockdown<-factor(as.numeric(par.periph.cont.sub$lockdown))

library(lme4)
formula<-c("clipped_z_score~lockdown",
"clipped_z_score~lockdown*repeats",
"clipped_z_score~lockdown*weekend",
"clipped_z_score~lockdown*repeats*weekend",
"clipped_z_score~lockdown*FDep",
"clipped_z_score~lockdown*repeats*FDep",
"clipped_z_score~lockdown*weekend*FDep",
"clipped_z_score~lockdown*repeats*weekend*FDep",
"clipped_z_score~lockdown*coverage_area",
"clipped_z_score~lockdown*repeats*coverage_area",
"clipped_z_score~lockdown*weekend*coverage_area",
"clipped_z_score~lockdown*repeats*weekend*coverage_area",
"clipped_z_score~lockdown*FDep*coverage_area",
"clipped_z_score~lockdown*repeats*FDep*coverage_area",
"clipped_z_score~lockdown*weekend*FDep*coverage_area",
"clipped_z_score~lockdown*repeats*weekend*FDep*coverage_area"
)

##TO DO
AIC.df.c<-data.frame(model=formula, AIC=NA, time=0)
for (i in 1:length(formula)) {
print(formula[i])
flush.console()
tic<-Sys.time()
par.lme.final<-lmer(as.formula(paste0(formula[i],"+ (1|dated) + (1|quadkeys)")) ,data=par.periph.cont.sub,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))
AIC.df.c$time[i]<-Sys.time()-tic
AIC.df.c$AIC[i]<-AIC(par.lme.final)
rm(par.lme.final)
gc()
}
AIC.df.c
#repeats as random effects instead: AIC: 592613.3

save(AIC.df.c,formula,file="C:/Users/David/Documents/Facebook COVID/modelselection_Paris_0800_wrepeats_FB.Rdata")
##
i=16 #best model
par.lme.final<-lmer(as.formula(paste0(formula[i],"+ (1|dated) + (1|quadkeys)")) ,data=par.periph.cont.sub,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))


library(ggeffects)
##plot repeats 1 #FDep ticks are 10% 50% and 90%

paris.final.effect0<-ggpredict(par.lme.final,terms=c("coverage_area [all]","FDep [-2, 0,2]","repeats","weekend"),condition=c(lockdown="1"))
paris.final.effect2<-ggpredict(par.lme.final,terms=c("coverage_area [all]","FDep [-2, 0,2]","repeats","weekend"),condition=c(lockdown="3"))


##################################################################################################################################
##################################################################################################################################


gg.paris.0<-plot(paris.final.effect0,limits=c(-5,2),show.y.title=FALSE,show.x.title=FALSE,show.title=FALSE)

gg.paris.0[[1]]$labels$y<-"Predicted Z-score (Z08)"
gg.paris.0[[1]]$labels$title<-"Paris - daytime - no restriction measures"

gg.paris.2<-plot(paris.final.effect2,limits=c(-5,2),limit.range=FALSE,show.y.title=FALSE,show.x.title=FALSE,show.title=FALSE)
gg.paris.2[[1]]$labels$y<-"Predicted Z-score (Z08)"
gg.paris.2[[1]]$labels$title<-"Paris - daytime - require not leaving house with exceptions"
gg.paris.2[[1]]$labels$x<-"Proportion of tile covered by greenspace"
gg.paris.2[[2]]$labels$x<-"Proportion of tile covered by greenspace"


library(grid)
library(gridExtra)
library(ggplotify)
###############################################################
##########MANUSCRIPT FIGURE PARIS 0800
 tiff(file="C:/Users/David/OneDrive - Danmarks Tekniske Universitet/Facebook Greenspace ms/Figure_Paris_Z_0800.tiff", width=40,height=40,units="cm",res=200)
grid.arrange(as.grob(gg.paris.0),as.grob(gg.paris.2),nrow=2,heights=c(10,10))
 dev.off()


####################### MANUSCRIPT FIGURE LOCKDOWN ONLY

gg.paris.2<-plot(paris.final.effect2,limits=c(-3,1.25),limit.range=FALSE,show.y.title=FALSE,show.x.title=FALSE,show.title=FALSE)
gg.paris.2[[1]]$labels$y<-"Predicted Z-score (Z08)"
gg.paris.2[[1]]$labels$subtitle<-"Paris - week days"
gg.paris.2[[2]]$labels$subtitle<-"Paris - weekend"
gg.paris.2[[2]]$labels$x<-"Proportion of tile covered by greenspace"


 tiff(file="C:/Users/David/OneDrive - Danmarks Tekniske Universitet/Facebook Greenspace ms/Figure_Paris_Z_0800_LOCKDOWN_ONLY.tiff", width=40,height=20,units="cm",res=200)
gg.paris.2
 dev.off()



##################################################################################################################################
##################################################################################################################################


gg.paris.0<-plot(paris.final.effect0,limits=c(-4,3),show.y.title=FALSE,show.x.title=FALSE,show.title=FALSE)

gg.paris.0[[1]]$labels$y<-"Predicted Z-score (Z08)"
gg.paris.0[[1]]$labels$title<-"Paris - no restriction measures"

gg.paris.2<-plot(paris.final.effect2,limits=c(-4,3),limit.range=FALSE,show.y.title=FALSE,show.x.title=FALSE,show.title=FALSE)
gg.paris.2[[1]]$labels$y<-"Predicted Z-score (Z08)"
gg.paris.2[[1]]$labels$title<-"Paris - require not leaving house with exceptions"
gg.paris.2[[1]]$labels$x<-"Proportion of tile covered by greenspace"
gg.paris.2[[2]]$labels$x<-"Proportion of tile covered by greenspace"


library(grid)
library(gridExtra)
library(ggplotify)
###############################################################
##########MANUSCRIPT FIGURE PARIS CONTRAST
 tiff(file="C:/Users/David/OneDrive - Danmarks Tekniske Universitet/Facebook Greenspace ms/Figure_Paris_Z_0800.tiff", width=40,height=40,units="cm",res=200)
grid.arrange(as.grob(gg.paris.0),as.grob(gg.paris.2),nrow=2,heights=c(10,10))
 dev.off()


###############################################################
###############################################################

load("C:/Users/David/Documents/Facebook COVID/paris_corona_FB_periph_Aug_2021.Rdata")

paris_contrast0800<-subset(par.pop.periph,time=="0800")


load("C:/Users/David/Documents/Facebook COVID/Twitter/tweet.volume.df.all.Rdata")

paris_contrast0800$repeats<-tweet.vol.par$repeats[match(as.Date(paris_contrast0800$dates),tweet.vol.par$date)]
rm(tweet.vol.lon,tweet.vol.ber,tweet.vol.par)
table(paris_contrast0800$lockdown,paris_contrast0800$repeats)


paris_contrast0800$repeats<-as.numeric(paris_contrast0800$repeats)
paris_contrast0800$repeats[paris_contrast0800$lockdown=="0"]<-paris_contrast0800$repeats[paris_contrast0800$lockdown=="0"]-1 #we start in March
paris_contrast0800$repeats<-factor(paris_contrast0800$repeats)

par.periph.cont.sub<-subset(paris_contrast0800,lockdown!="1")

formula<-c(
"clipped_z_score~lockdown*repeats*weekend*FDep*coverage_area"
)

par.lme.final<-lmer(as.formula(paste0(formula[i],"+ (1|dated) + (1|quadkeys)")) ,data=par.periph.cont.sub,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))


paris.final.effect0<-ggpredict(par.lme.final,terms=c("coverage_area [all]","FDep [-2, 0,2]","repeats"),condition=c(weekend="no",lockdown="2"))
paris.final.effect2<-ggpredict(par.lme.final,terms=c("coverage_area [all]","FDep [-2, 0,2]","repeats"),condition=c(weekend="yes",lockdown="2"))


gg.paris.0<-plot(paris.final.effect0,limits=c(-4,3),show.y.title=FALSE,show.x.title=FALSE,show.title=FALSE)

gg.paris.0$labels$y<-"Predicted Z-score (Z08)"
gg.paris.0$labels$title<-"Paris - week days"
gg.paris.0$labels$x<-"Proportion of tile covered by greenspace"

gg.paris.2<-plot(paris.final.effect2,limits=c(-4,3),limit.range=FALSE,show.y.title=FALSE,show.x.title=FALSE,show.title=FALSE)
gg.paris.2$labels$y<-"Predicted Z-score (Z08)"
gg.paris.2$labels$title<-"Paris - weekend"
gg.paris.2$labels$x<-"Proportion of tile covered by greenspace"


library(grid)
library(gridExtra)
library(ggplotify)
###############################################################
##########MANUSCRIPT FIGURE PARIS CONTRAST
 tiff(file="C:/Users/David/OneDrive - Danmarks Tekniske Universitet/Facebook Greenspace ms/Figure_Paris_Z_lockdownonly_0800_repeats.tiff", width=40,height=40,units="cm",res=200)
grid.arrange(as.grob(gg.paris.0),as.grob(gg.paris.2),nrow=2,heights=c(10,10))
 dev.off()


save(gg.paris.0,gg.paris.2,file="C:/Users/David/OneDrive - Danmarks Tekniske Universitet/Facebook Greenspace ms/paris_GROB_lockdown_wk.Rdata")

###############################################################################################################################################
################################################################################################################################################

load("C:/Users/David/Documents/Facebook COVID/paris_corona_FB_periph_Aug_2021.Rdata")

paris_contrast0800<-subset(par.pop.periph,time=="0800")
paris_contrast0000<-subset(par.pop.periph,time=="0000")
paris_contrast<-merge(paris_contrast0800,paris_contrast0000,by.x=c("quadkeys","dated"),by.y=c("quadkeys","dated"),all.x=TRUE)
paris_contrast$z_score<-paris_contrast$clipped_z_score.x-paris_contrast$clipped_z_score.y
par.periph.cont<-paris_contrast[,c(1,2,57,23,25,26,27,29)]
names(par.periph.cont)<-c("quadkeys","dated","z_score","coverage_area","dates","lockdown","FDep","weekend")


load("C:/Users/David/Documents/Facebook COVID/Twitter/tweet.volume.df.all.Rdata")

par.periph.cont$repeats<-tweet.vol.par$repeats[match(as.Date(par.periph.cont$dates),tweet.vol.par$date)]
rm(tweet.vol.lon,tweet.vol.ber,tweet.vol.par)
table(par.periph.cont$lockdown,par.periph.cont$repeats)


par.periph.cont$repeats<-as.numeric(par.periph.cont$repeats)
par.periph.cont$repeats[par.periph.cont$lockdown=="0"]<-par.periph.cont$repeats[par.periph.cont$lockdown=="0"]-1 #we start in March
par.periph.cont$repeats<-factor(par.periph.cont$repeats)

par.periph.cont.sub<-subset(par.periph.cont,lockdown!="1")
par.periph.cont.sub$lockdown<-factor(par.periph.cont.sub$lockdown)

formula<-c(
"z_score~lockdown*repeats*weekend*FDep*coverage_area"
)

par.lme.final<-lmer(as.formula(paste0(formula[i],"+ (1|dated) + (1|quadkeys)")) ,data=par.periph.cont.sub,control=lmerControl(check.rankX="ignore",check.conv.grad="ignore"))


paris.final.effect0<-ggpredict(par.lme.final,terms=c("coverage_area [all]","FDep [-2, 0,2]","repeats"),condition=c(weekend="no",lockdown="2"))
paris.final.effect2<-ggpredict(par.lme.final,terms=c("coverage_area [all]","FDep [-2, 0,2]","repeats"),condition=c(weekend="yes",lockdown="2"))


gg.paris.0<-plot(paris.final.effect0,limits=c(-3,2),show.y.title=FALSE,show.x.title=FALSE,show.title=FALSE)

gg.paris.0$labels$y<-"Predicted Z-score (Z08-00)"
gg.paris.0$labels$title<-"Paris - week days"
gg.paris.0$labels$x<-"Proportion of tile covered by greenspace"

gg.paris.2<-plot(paris.final.effect2,limits=c(-3,2),limit.range=FALSE,show.y.title=FALSE,show.x.title=FALSE,show.title=FALSE)
gg.paris.2$labels$y<-"Predicted Z-score (Z08-00)"
gg.paris.2$labels$title<-"Paris - weekend"
gg.paris.2$labels$x<-"Proportion of tile covered by greenspace"


library(grid)
library(gridExtra)
library(ggplotify)
###############################################################
##########MANUSCRIPT FIGURE PARIS CONTRAST
 tiff(file="C:/Users/David/OneDrive - Danmarks Tekniske Universitet/Facebook Greenspace ms/Figure_Paris_Z_lockdownonly_CONTRAST_repeats.tiff", width=40,height=40,units="cm",res=200)
grid.arrange(as.grob(gg.paris.0),as.grob(gg.paris.2),nrow=2,heights=c(10,10))
 dev.off()


save(gg.paris.0,gg.paris.2,file="C:/Users/David/OneDrive - Danmarks Tekniske Universitet/Facebook Greenspace ms/paris_GROB_contrast_lockdown_wk.Rdata")
