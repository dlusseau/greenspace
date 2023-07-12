####################
### plot maps of variables

######################################
####let's start with Berlin to control that the difference observed with the other cities is not a data QA issue

load("C:/Users/David/Documents/Facebook COVID/berlin_corona_FB_BI_Aug_2021.Rdata")

str(ber.pop.bi)
library(sf)

ber.pop.bi$polygon<-NA
for (i in 1:dim(ber.pop.bi)[1]) {
ber.pop.bi$polygon[i]<-st_sfc(st_polygon(list(rbind(c(ber.pop.bi$lon.top[i],ber.pop.bi$lat.top[i]),c(ber.pop.bi$lon.bot[i],ber.pop.bi$lat.top[i]),c(ber.pop.bi$lon.bot[i],ber.pop.bi$lat.bot[i]),c(ber.pop.bi$lon.top[i],ber.pop.bi$lat.bot[i]),c(ber.pop.bi$lon.top[i],ber.pop.bi$lat.top[i])))))

}


quadkeys<-unique(ber.pop.bi$quadkey)
berlin<-data.frame(quadkey=quadkeys,lat=ber.pop.bi$lat[match(quadkeys,ber.pop.bi$quadkey)],
lon=ber.pop.bi$lon[match(quadkeys,ber.pop.bi$quadkey)],
lon.top=ber.pop.bi$lon.top[match(quadkeys,ber.pop.bi$quadkey)],
lon.bot=ber.pop.bi$lon.bot[match(quadkeys,ber.pop.bi$quadkey)],
lat.top=ber.pop.bi$lat.top[match(quadkeys,ber.pop.bi$quadkey)],
lat.bot=ber.pop.bi$lat.bot[match(quadkeys,ber.pop.bi$quadkey)],
gardens=ber.pop.bi$gardens[match(quadkeys,ber.pop.bi$quadkey)],
coverage=ber.pop.bi$coverage[match(quadkeys,ber.pop.bi$quadkey)],
coverage_area=ber.pop.bi$coverage_area[match(quadkeys,ber.pop.bi$quadkey)],
MSS=ber.pop.bi$MSS[match(quadkeys,ber.pop.bi$quadkey)]
)


earth_circumference_m<-6378137*2*pi              
earth_circumference<-360 
##################


berlin$tile_size_lat<-(cos(berlin$lat * pi/180) * earth_circumference)/(2^(nchar(as.character(berlin$quadkey))))
berlin$tile_size_lon<-(cos(berlin$lon * pi/180) * earth_circumference)/(2^(nchar(as.character(berlin$quadkey)))) 
berlin$lon.top<-berlin$lon+(berlin$tile_size_lon/2)
berlin$lon.bot<-berlin$lon-(berlin$tile_size_lon/2)
berlin$lat.top<-berlin$lat+(berlin$tile_size_lat/2)
berlin$lat.bot<-berlin$lat-(berlin$tile_size_lat/2)


berlin$Z08<-NA

for (i in 1:dim(berlin)[1]) {
berlin$Z08[i]<-median(ber.pop.bi$clipped_z_score[ber.pop.bi$quadkey==berlin$quadkey[i]&ber.pop.bi$time=="0800"&ber.pop.bi$lockdown=="2"],na.rm=TRUE)
}


berlin$polygon<-NA
for (i in 1:dim(berlin)[1]) {
berlin$polygon[i]<-st_sfc(st_polygon(list(rbind(c(berlin$lon.top[i],berlin$lat.top[i]),c(berlin$lon.bot[i],berlin$lat.top[i]),c(berlin$lon.bot[i],berlin$lat.bot[i]),c(berlin$lon.top[i],berlin$lat.bot[i]),c(berlin$lon.top[i],berlin$lat.top[i])))))

}


  

berlin.map<-st_sf(berlin,crs=4326)

save(berlin.map,file="C:/Users/David/Documents/Facebook COVID/berlin_map.Rdata")

library(ggplot2)

bg<-ggplot() +geom_sf(data=berlin.map,aes(fill=coverage_area))+
scale_fill_gradient(palette=5,name="proportion of greenspace")


ggplot() +geom_sf(data=berlin.map,aes(fill=coverage_area))
,values=c(0,10^-4,10^-3,10^-2,0.15,0.1,0.2,0.4,.6),aesthetics="fill",colors=c('#f7fcfd','#e5f5f9','#ccece6','#99d8c9','#66c2a4','#41ae76','#238b45','#006d2c','#00441b'))

title("proportion of greenspace")

,key.pos=1)
bm<-plot(berlin.map["MSS"],main="MSS",key.pos=1)
bz<-plot(berlin.map["Z08"],main="median daytime Z score during lockdown",key.pos=1)

library(grid)
library(gridExtra)
library(ggplotify)

tiff(file="C:/Users/David/OneDrive - Danmarks Tekniske Universitet/Facebook Greenspace ms/Figure berlin map.tiff",width=40,height=20,res=200,units="cm")
grid.arrange(as.grob(bg),as.grob(bm),as.grob(bz),nrow=1,ncol=3,widths=c(7,7,7))
dev.off()
############################################################################################
##### PARIS
load("C:/Users/David/Documents/Facebook COVID/paris_corona_FB_periph_Aug_2021.Rdata")
library(sf)



quadkeys<-unique(par.pop.periph$quadkey)
paris<-data.frame(quadkey=quadkeys,lat=par.pop.periph$lat[match(quadkeys,par.pop.periph$quadkey)],
lon=par.pop.periph$lon[match(quadkeys,par.pop.periph$quadkey)],
lon.top=par.pop.periph$lon.top[match(quadkeys,par.pop.periph$quadkey)],
lon.bot=par.pop.periph$lon.bot[match(quadkeys,par.pop.periph$quadkey)],
lat.top=par.pop.periph$lat.top[match(quadkeys,par.pop.periph$quadkey)],
lat.bot=par.pop.periph$lat.bot[match(quadkeys,par.pop.periph$quadkey)],
gardens=par.pop.periph$gardens[match(quadkeys,par.pop.periph$quadkey)],
coverage=par.pop.periph$coverage[match(quadkeys,par.pop.periph$quadkey)],
coverage_area=par.pop.periph$coverage_area[match(quadkeys,par.pop.periph$quadkey)],
FDep=par.pop.periph$FDep[match(quadkeys,par.pop.periph$quadkey)]
)


earth_circumference_m<-6378137*2*pi              
earth_circumference<-360 
##################


paris$tile_size_lat<-(cos(paris$lat * pi/180) * earth_circumference)/(2^(nchar(as.character(paris$quadkey))))
paris$tile_size_lon<-(cos(paris$lon * pi/180) * earth_circumference)/(2^(nchar(as.character(paris$quadkey)))) 
paris$lon.top<-paris$lon+(paris$tile_size_lon/2)
paris$lon.bot<-paris$lon-(paris$tile_size_lon/2)
paris$lat.top<-paris$lat+(paris$tile_size_lat/2)
paris$lat.bot<-paris$lat-(paris$tile_size_lat/2)


paris$Z08<-NA

for (i in 1:dim(paris)[1]) {
paris$Z08[i]<-median(par.pop.periph$clipped_z_score[par.pop.periph$quadkey==paris$quadkey[i]&par.pop.periph$time=="0800"&par.pop.periph$lockdown=="2"],na.rm=TRUE)
}


paris$polygon<-NA
for (i in 1:dim(paris)[1]) {
paris$polygon[i]<-st_sfc(st_polygon(list(rbind(c(paris$lon.top[i],paris$lat.top[i]),c(paris$lon.bot[i],paris$lat.top[i]),c(paris$lon.bot[i],paris$lat.bot[i]),c(paris$lon.top[i],paris$lat.bot[i]),c(paris$lon.top[i],paris$lat.top[i])))))

}


paris.map<-st_sf(paris,crs=4326)




sum(st_area(london.gardens))/st_area(lsoa.simp)
#0.161367 proportion of greenspace in london
berlin.gp<-st_intersection(Berlingreenspace,st_union(BerlinIMD))

sum(st_area(berling.gp))/st_area(st_union(BerlinIMD))
#0.06870316 proportion of greenspace in Berlin

paris.gp<-st_intersection(Parisgreenspace,st_union(ParisIMD))
sum(st_area(paris.gp))/st_area(st_union(ParisIMD))
#0.241453 proportion of greenspace in Paris


berlin.gp.hoch<-st_intersection(Berlingreenspace,st_union(subset(BerlinIMD,SI_V_2019=="hoch")))
sum(st_area(berlin.gp.hoch))/st_area(st_union(subset(BerlinIMD,SI_V_2019=="hoch")))
#0.05181076 
berlin.gp.sn<-st_intersection(Berlingreenspace,st_union(subset(BerlinIMD,SI_V_2019=="sehr niedrig")))
sum(st_area(berlin.gp.sn))/st_area(st_union(subset(BerlinIMD,SI_V_2019=="sehr niedrig")))
#0.07637213
berlin.gp.m<-st_intersection(Berlingreenspace,st_union(subset(BerlinIMD,SI_V_2019=="mittel")))
sum(st_area(berlin.gp.m))/st_area(st_union(subset(BerlinIMD,SI_V_2019=="mittel")))
#0.07191175
berlin.gp.n<-st_intersection(Berlingreenspace,st_union(subset(BerlinIMD,SI_V_2019=="niedrig")))
sum(st_area(berlin.gp.n))/st_area(st_union(subset(BerlinIMD,SI_V_2019=="niedrig")))
#.1307301

par(mfrow=c(2,2))
hist(log10(st_area(berlin.gp.hoch)),50,main="hoch")
hist(log10(st_area(berlin.gp.m)),50,main="mittel")
hist(log10(st_area(berlin.gp.n)),50,main="niedrig")
hist(log10(st_area(berlin.gp.sn)),50,main="sehr niedrig")
