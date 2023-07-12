##### map overlaid on satellite COVID berlin, london, paris

library(sf)

###############BERLIN
load("C:/Users/David/Documents/Facebook COVID/berlin_corona_FB_Aug_2021.Rdata")



quadkeys<-unique(ber.pop$quadkey)
berlin<-data.frame(quadkey=quadkeys,lat=ber.pop$lat[match(quadkeys,ber.pop$quadkey)],
lon=ber.pop$lon[match(quadkeys,ber.pop$quadkey)],
lon.top=ber.pop$lon.top[match(quadkeys,ber.pop$quadkey)],
lon.bot=ber.pop$lon.bot[match(quadkeys,ber.pop$quadkey)],
lat.top=ber.pop$lat.top[match(quadkeys,ber.pop$quadkey)],
lat.bot=ber.pop$lat.bot[match(quadkeys,ber.pop$quadkey)],
gardens=ber.pop$gardens[match(quadkeys,ber.pop$quadkey)],
coverage=ber.pop$coverage[match(quadkeys,ber.pop$quadkey)],
coverage_area=ber.pop$coverage_area[match(quadkeys,ber.pop$quadkey)],
MSS=ber.pop$MSS[match(quadkeys,ber.pop$quadkey)]
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

load("C:/Users/David/Documents/Facebook COVID/berlin_map.Rdata")
berlin<-berlin[berlin$lat>=st_bbox(berlin.map)[2],]
berlin<-berlin[berlin$lat<=st_bbox(berlin.map)[4],]
berlin<-berlin[berlin$lon<=st_bbox(berlin.map)[3],]
berlin<-berlin[berlin$lon>=st_bbox(berlin.map)[1],]


berlin$Z08<-NA

nas<-which(is.na(berlin$Z08))

for (i in nas) {
berlin$Z08[i]<-median(ber.pop$clipped_z_score[ber.pop$quadkey==berlin$quadkey[i]&ber.pop$time=="0800"&ber.pop$lockdown=="2"],na.rm=TRUE)
}

berlin$Z08w1<-NA
berlin$Z08w2<-NA


for (i in nas) {
berlin$Z08w1[i]<-median(ber.pop$clipped_z_score[ber.pop$quadkey==berlin$quadkey[i]&ber.pop$time=="0800"&ber.pop$lockdown=="2" & ber.pop$dates<as.POSIXct("2020-09-01")],na.rm=TRUE)
berlin$Z08w2[i]<-median(ber.pop$clipped_z_score[ber.pop$quadkey==berlin$quadkey[i]&ber.pop$time=="0800"&ber.pop$lockdown=="2" & ber.pop$dates>as.POSIXct("2020-09-01")],na.rm=TRUE)
}

berlin$polygon<-NA
for (i in 1:dim(berlin)[1]) {
berlin$polygon[i]<-st_sfc(st_polygon(list(rbind(c(berlin$lon.top[i],berlin$lat.top[i]),c(berlin$lon.bot[i],berlin$lat.top[i]),c(berlin$lon.bot[i],berlin$lat.bot[i]),c(berlin$lon.top[i],berlin$lat.bot[i]),c(berlin$lon.top[i],berlin$lat.top[i])))))

}

berlin.map<-st_sf(berlin,crs=4326)

save(berlin.map,file="C:/Users/David/Documents/Facebook COVID/berlin_map_all_waves.Rdata")

berlin.map<-subset(berlin.map,nchar(berlin.map$quadkey)>15)

load(url("https://userpage.fu-berlin.de/soga/300/30100_data_sets/berlin_district.RData"))
#plot(berlin.sf, main="")
berlin.cookie.cutter<-st_transform(berlin.sf,crs=4326)

berlin.map.district<-st_intersection(berlin.map,berlin.cookie.cutter)

berlin.map.district<-subset(berlin.map.district,nchar(berlin.map.district$quadkey)>15)


berlin.map.district$Z08w1<-NA
berlin.map.district$Z08w2<-NA


for (i in 1:dim(berlin.map.district)[1]) {
berlin.map.district$Z08w1[i]<-median(ber.pop$clipped_z_score[ber.pop$quadkey==berlin.map.district$quadkey[i]&ber.pop$time=="0800"&ber.pop$lockdown=="2" & ber.pop$dates<as.POSIXct("2020-09-01")],na.rm=TRUE)
berlin.map.district$Z08w2[i]<-median(ber.pop$clipped_z_score[ber.pop$quadkey==berlin.map.district$quadkey[i]&ber.pop$time=="0800"&ber.pop$lockdown=="2" & ber.pop$dates>as.POSIXct("2020-09-01")],na.rm=TRUE)
}


save(berlin.map.district,file="C:/Users/David/Documents/Facebook COVID/berlin_map_all_waves.Rdata")


berlin.map.district<-subset(berlin.map.district,nchar(berlin.map.district$quadkey)>15)

pal<-colorNumeric(palette=c('#b35806','#e08214','#fdb863','#fee0b6','#f7f7f7','#d8daeb','#b2abd2','#8073ac','#542788'), domain=c(-4,4))
map.berw1<-leaflet(berlin.map.district)%>%
addTiles() %>%
addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = .6,fillColor = ~pal(Z08w1))%>%
addLegend(pal = pal, values = ~Z08w1, opacity = 1.0,title="Z08")%>%
addProviderTiles("Esri.WorldImagery")

map.berw1

#pal <- colorNumeric(c('#b35806','#e08214','#fdb863','#fee0b6','#f7f7f7','#d8daeb'),NULL)
map.berw2<-leaflet(berlin.map.district)%>%
addTiles() %>%
addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = .6,fillColor = ~pal(Z08w2))%>%
addLegend(pal = pal, values = ~Z08w2, opacity = 1.0,title="Z08")%>%
addProviderTiles("Esri.WorldImagery")
map.berw2

mapshot(map.berw1,url=NULL,file="C:/Users/David/OneDrive - Danmarks Tekniske Universitet/Facebook Greenspace ms/material for submission/BerlinZ08lockdownmapwave1.jpeg")
mapshot(map.berw2,url=NULL,file="C:/Users/David/OneDrive - Danmarks Tekniske Universitet/Facebook Greenspace ms/material for submission/BerlinZ08lockdownmapwave2.jpeg")



######################################################################################

berlin.map.district$Z08[6769]


#ber.pop$clipped_z_score[ber.pop$quadkey==berlin.map.district$quadkey[6769]&ber.pop$time=="0800"&ber.pop$lockdown=="2"]
###no data

library(leaflet)

pal <- colorNumeric(c('#b35806','#e08214','#fdb863','#fee0b6','#f7f7f7','#d8daeb','#b2abd2','#8073ac','#542788'), NULL)
map.ber<-leaflet(berlin.map.district)%>%
addTiles() %>%
addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = .5,fillColor = ~pal(Z08))%>%
addLegend(pal = pal, values = ~Z08, opacity = 1.0)%>%
addProviderTiles("Esri.WorldImagery")
map.ber

library(mapview)
mapshot(map.ber,url=NULL,file="C:/Users/David/OneDrive - Danmarks Tekniske Universitet/Facebook Greenspace ms/material for submission/BerlinZ08lockdownmap.jpeg")
mapshot(map.ber,url="C:/Users/David/OneDrive - Danmarks Tekniske Universitet/Facebook Greenspace ms/material for submission/BerlinZ08lockdownmap.html",file=NULL,remove_controls=NULL)

##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##Paris
##
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################

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

save(paris.map,file="C:/Users/David/Documents/Facebook COVID/paris_map.Rdata")

library(leaflet)

pal <- colorNumeric(c('#b35806','#e08214','#fdb863','#fee0b6','#f7f7f7','#d8daeb'), NULL)
map.par<-leaflet(paris.map)%>%
addTiles() %>%
addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = .5,fillColor = ~pal(Z08))%>%
addLegend(pal = pal, values = ~Z08, opacity = 1.0)%>%
addProviderTiles("Esri.WorldImagery")
map.par

library(mapview)
mapshot(map.par,url=NULL,file="C:/Users/David/OneDrive - Danmarks Tekniske Universitet/Facebook Greenspace ms/material for submission/ParisZ08lockdownmap.jpeg")
mapshot(map.par,url="C:/Users/David/OneDrive - Danmarks Tekniske Universitet/Facebook Greenspace ms/material for submission/ParisZ08lockdownmap.html",file=NULL,remove_controls=NULL)

################################################################################################################
####wider Paris
library(sf)
load("C:/Users/David/Documents/Facebook COVID/paris_corona_FB_Aug_2021.Rdata")


quadkeys<-unique(par.pop$quadkey)
paris<-data.frame(quadkey=quadkeys,lat=par.pop$lat[match(quadkeys,par.pop$quadkey)],
lon=par.pop$lon[match(quadkeys,par.pop$quadkey)],
lon.top=par.pop$lon.top[match(quadkeys,par.pop$quadkey)],
lon.bot=par.pop$lon.bot[match(quadkeys,par.pop$quadkey)],
lat.top=par.pop$lat.top[match(quadkeys,par.pop$quadkey)],
lat.bot=par.pop$lat.bot[match(quadkeys,par.pop$quadkey)],
gardens=par.pop$gardens[match(quadkeys,par.pop$quadkey)],
coverage=par.pop$coverage[match(quadkeys,par.pop$quadkey)],
coverage_area=par.pop$coverage_area[match(quadkeys,par.pop$quadkey)],
FDep=par.pop$FDep[match(quadkeys,par.pop$quadkey)]
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
paris$Z08[i]<-median(par.pop$clipped_z_score[par.pop$quadkey==paris$quadkey[i]&par.pop$time=="0800"&par.pop$lockdown=="2"],na.rm=TRUE)
}


paris$Z08_max<-NA

for (i in 1:dim(paris)[1]) {
paris$Z08_max[i]<-max(par.pop$clipped_z_score[par.pop$quadkey==paris$quadkey[i]&par.pop$time=="0800"&par.pop$lockdown=="2"],na.rm=TRUE)
}

paris$polygon<-NA
for (i in 1:dim(paris)[1]) {
paris$polygon[i]<-st_sfc(st_polygon(list(rbind(c(paris$lon.top[i],paris$lat.top[i]),c(paris$lon.bot[i],paris$lat.top[i]),c(paris$lon.bot[i],paris$lat.bot[i]),c(paris$lon.top[i],paris$lat.bot[i]),c(paris$lon.top[i],paris$lat.top[i])))))

}

paris$Z08w1<-NA
paris$Z08w2<-NA

for (i in 1:dim(paris)[1]) {
paris$Z08w1[i]<-median(par.pop$clipped_z_score[par.pop$quadkey==paris$quadkey[i]&par.pop$time=="0800"&par.pop$lockdown=="2"& par.pop$dates<as.POSIXct("2020-09-01")],na.rm=TRUE)
paris$Z08w2[i]<-median(par.pop$clipped_z_score[par.pop$quadkey==paris$quadkey[i]&par.pop$time=="0800"&par.pop$lockdown=="2"& par.pop$dates>as.POSIXct("2020-09-01")],na.rm=TRUE)
}


paris.map<-st_sf(paris,crs=4326)

save(paris.map,file="C:/Users/David/Documents/Facebook COVID/paris_mapwaves.Rdata")


library(leaflet)

#pal <- colorNumeric(c('#b35806','#e08214','#fdb863','#fee0b6','#f7f7f7','#d8daeb'),NULL)
pal<-colorNumeric(palette=c('#b35806','#e08214','#fdb863','#fee0b6','#f7f7f7','#d8daeb','#b2abd2','#8073ac','#542788'), domain=c(-4,4))
map.parw1<-leaflet(paris.map)%>%
addTiles() %>%
addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = .6,fillColor = ~pal(Z08w1))%>%
addLegend(pal = pal, values = ~Z08w1, opacity = 1.0,title="Z08")%>%
addProviderTiles("Esri.WorldImagery")
map.parw1

#pal <- colorNumeric(c('#b35806','#e08214','#fdb863','#fee0b6','#f7f7f7','#d8daeb'),NULL)
map.parw2<-leaflet(paris.map)%>%
addTiles() %>%
addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = .6,fillColor = ~pal(Z08w2))%>%
addLegend(pal = pal, values = ~Z08w2, opacity = 1.0,title="Z08")%>%
addProviderTiles("Esri.WorldImagery")
map.parw2

mapshot(map.parw1,url=NULL,file="C:/Users/David/OneDrive - Danmarks Tekniske Universitet/Facebook Greenspace ms/material for submission/ParisZ08lockdownmapwave1.jpeg")
mapshot(map.parw2,url=NULL,file="C:/Users/David/OneDrive - Danmarks Tekniske Universitet/Facebook Greenspace ms/material for submission/ParisZ08lockdownmapwave2.jpeg")

#mapshot(map.par,url="C:/Users/David/OneDrive - Danmarks Tekniske Universitet/Facebook Greenspace ms/material for submission/ParisZ08lockdownmap.html",file=NULL,remove_controls=NULL)


###########################################################################################################################################################
#############################################################################################################################################################

library(sf)
load("C:/Users/David/Documents/Facebook COVID/london_corona_FB_M25_Aug_2021.Rdata")



quadkeys<-unique(lon.pop.M25$quadkey)
london<-data.frame(quadkey=quadkeys,lat=lon.pop.M25$lat[match(quadkeys,lon.pop.M25$quadkey)],
lon=lon.pop.M25$lon[match(quadkeys,lon.pop.M25$quadkey)],
lon.top=lon.pop.M25$lon.top[match(quadkeys,lon.pop.M25$quadkey)],
lon.bot=lon.pop.M25$lon.bot[match(quadkeys,lon.pop.M25$quadkey)],
lat.top=lon.pop.M25$lat.top[match(quadkeys,lon.pop.M25$quadkey)],
lat.bot=lon.pop.M25$lat.bot[match(quadkeys,lon.pop.M25$quadkey)],
gardens=lon.pop.M25$gardens[match(quadkeys,lon.pop.M25$quadkey)],
coverage=lon.pop.M25$coverage[match(quadkeys,lon.pop.M25$quadkey)],
coverage_area=lon.pop.M25$coverage_area[match(quadkeys,lon.pop.M25$quadkey)],
IMD=lon.pop.M25$IMD[match(quadkeys,lon.pop.M25$quadkey)]
)


earth_circumference_m<-6378137*2*pi              
earth_circumference<-360 
##################


london$tile_size_lat<-(cos(london$lat * pi/180) * earth_circumference)/(2^(nchar(as.character(london$quadkey))))
london$tile_size_lon<-(cos(london$lon * pi/180) * earth_circumference)/(2^(nchar(as.character(london$quadkey)))) 
london$lon.top<-london$lon+(london$tile_size_lon/2)
london$lon.bot<-london$lon-(london$tile_size_lon/2)
london$lat.top<-london$lat+(london$tile_size_lat/2)
london$lat.bot<-london$lat-(london$tile_size_lat/2)


london[nchar(as.character(london$quadkey))<16,]$tile_size_lat<-(cos(london[nchar(as.character(london$quadkey))<16,]$lat * pi/180) * earth_circumference)/(2^(nchar(as.character(london[nchar(as.character(london$quadkey))<16,]$quadkey))+1))
london[nchar(as.character(london$quadkey))<16,]$tile_size_lon<-(cos(london[nchar(as.character(london$quadkey))<16,]$lon * pi/180) * earth_circumference)/(2^(nchar(as.character(london[nchar(as.character(london$quadkey))<16,]$quadkey))+1)) 
london$lon.top<-london$lon+(london$tile_size_lon/2)
london$lon.bot<-london$lon-(london$tile_size_lon/2)
london$lat.top<-london$lat+(london$tile_size_lat/2)
london$lat.bot<-london$lat-(london$tile_size_lat/2)

london$Z08<-NA
london$Z08w1<-NA
london$Z08w2<-NA

for (i in 1:dim(london)[1]) {
london$Z08[i]<-median(lon.pop.M25$clipped_z_score[lon.pop.M25$quadkey==london$quadkey[i]&lon.pop.M25$time=="0800"&lon.pop.M25$lockdown=="2"],na.rm=TRUE)
london$Z08w1[i]<-median(lon.pop.M25$clipped_z_score[lon.pop.M25$quadkey==london$quadkey[i]&lon.pop.M25$time=="0800"&lon.pop.M25$lockdown=="2" & lon.pop.M25$dates<as.Date("2020-09-01")],na.rm=TRUE)
london$Z08w2[i]<-median(lon.pop.M25$clipped_z_score[lon.pop.M25$quadkey==london$quadkey[i]&lon.pop.M25$time=="0800"&lon.pop.M25$lockdown=="2" & lon.pop.M25$dates>as.Date("2020-09-01")],na.rm=TRUE)

}


london$polygon<-NA
for (i in 1:dim(london)[1]) {
london$polygon[i]<-st_sfc(st_polygon(list(rbind(c(london$lon.top[i],london$lat.top[i]),c(london$lon.bot[i],london$lat.top[i]),c(london$lon.bot[i],london$lat.bot[i]),c(london$lon.top[i],london$lat.bot[i]),c(london$lon.top[i],london$lat.top[i])))))

}


london.map<-st_sf(london,crs=4326)

save(london.map,file="C:/Users/David/Documents/Facebook COVID/london_mapwaves.Rdata")


#save(london.map,file="C:/Users/David/Documents/Facebook COVID/london_map.Rdata")

london.map.sm<-subset(london.map,nchar(london.map$quadkey)>14)

library(leaflet)

#pal <- colorNumeric(c('#b35806','#e08214','#fdb863','#fee0b6','#f7f7f7','#d8daeb','#b2abd2','#8073ac','#542788'), NULL)



pal<-colorNumeric(palette=c('#b35806','#e08214','#fdb863','#fee0b6','#f7f7f7','#d8daeb','#b2abd2','#8073ac','#542788'), domain=c(-4,4))
map.lonw1<-leaflet(london.map.sm)%>%
addTiles() %>%
addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = .6,fillColor = ~pal(Z08w1))%>%
addLegend(pal = pal, values = ~Z08w1, opacity = 1.0,title="Z08")%>%
addProviderTiles("Esri.WorldImagery")
map.lonw1

#pal <- colorNumeric(c('#b35806','#e08214','#fdb863','#fee0b6','#f7f7f7','#d8daeb'),NULL)
map.lonw2<-leaflet(london.map.sm)%>%
addTiles() %>%
addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = .6,fillColor = ~pal(Z08w2))%>%
addLegend(pal = pal, values = ~Z08w2, opacity = 1.0,title="Z08")%>%
addProviderTiles("Esri.WorldImagery")
map.lonw2

mapshot(map.lonw1,url=NULL,file="C:/Users/David/OneDrive - Danmarks Tekniske Universitet/Facebook Greenspace ms/material for submission/LondonZ08lockdownmapwave1.jpeg")
mapshot(map.lonw2,url=NULL,file="C:/Users/David/OneDrive - Danmarks Tekniske Universitet/Facebook Greenspace ms/material for submission/LondonZ08lockdownmapwave2.jpeg")






pal<-colorNumeric(palette=c('#b35806','#e08214','#fdb863','#fee0b6','#f7f7f7','#d8daeb','#b2abd2','#8073ac','#542788'), domain=c(-4,4))

map.lonw1<-leaflet(london.map.sm)%>%
addTiles() %>%
addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = .5,fillColor = ~pal(Z08w1))%>%
addLegend(pal = pal, values = ~Z08w1, opacity = 1.0,title="Z08")%>%
addProviderTiles("Esri.WorldImagery")
map.lonw1

map.lonw2<-leaflet(london.map.sm)%>%
addTiles() %>%
addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = .5,fillColor = ~pal(Z08w2))%>%
addLegend(pal = pal, values = ~Z08w2, opacity = 1.0,title="Z08")%>%
addProviderTiles("Esri.WorldImagery")
map.lonw2

