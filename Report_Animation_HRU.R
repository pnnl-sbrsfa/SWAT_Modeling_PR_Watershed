## Convert the spatial file to sf
library(raster)
library(ggplot2)
library(sf)
library(dplyr)
library(lubridate)
library(gganimate)
library(rmapshaper)


library(raster);library(rgdal);library(sp);library(ggspatial);library(tidyverse)
library(rgdal);library(gganimate);library(plyr);library(gridExtra)
library(RODBC);library(tictoc)



#db <- file.path("C:/Users/gebr547/Documents/PNNL2019/SWAT/ForUSU/ForUSU/Scenarios/Daily2Year/TablesOut/SWATOutput.mdb")

dir("C:/Users/gebr547/OneDrive - PNNL/Documents/PNNL2019/WaterQuality/Hydro_GIS/")
setwd("C:/Users/gebr547/OneDrive - PNNL/Documents/PNNL2019/WaterQuality/Hydro_GIS/")
my.shp <- readOGR(dsn = "C:/Users/gebr547/OneDrive - PNNL/Documents/PNNL2019/WaterQuality/Hydro_GIS/FullHRU.shp")
my.shp <- readOGR(dsn = "C:/Users/gebr547/OneDrive - PNNL/Documents/PNNL2019/WaterQuality/Hydro_GIS/FullHRU.shp")

db <- file.path("C:/Users/gebr547/OneDrive - PNNL/Documents/PNNL2019/SWAT/ForUSU/ForUSU/Scenarios/MeetingJuly1/TablesOut/SWATOutput.mdb")


channel <- odbcConnectAccess2007(db)
# data-of-interest "sub", "hru", "sed", "rch"
#Reach_info <- sqlFetch(channel, "rch")
Reach_info <- sqlFetch(channel, "hru")
# Reach_info$subbasin2 <- Reach_info$SUB
Reach_info$hru2 <- Reach_info$HRUGIS
# Reach_info$Subbasin <- Reach_info$SUB
#Reach_info$hru <- Reach_info$SUB ...........??

Reach_info$YYYYMMDD <- as.Date(as.character(Reach_info$YYYYMMDD), "%Y%m%d")
Reach_info2 <- Reach_info %>% filter(YEAR == 2001)
Reach_info2 <- Reach_info %>% filter(YEAR == 2000 | YEAR == 2001 | YEAR == 2001)
#Reach_info3 <- Reach_info2[,c(1:5,9,51,54)]
Reach_info3 <- Reach_info2[,c(1:5,9,13:15,63,84,86)] # 84 nitrate loading
colnames(Reach_info3)

my.shp_sf <- st_as_sf(my.shp) %>% 
  ms_simplify()

ggplot(my.shp_sf) +
  geom_sf() + 
  ggtitle("All HRU in the catchment") + 
  theme(plot.title = element_text(hjust = 0.5))

# Specific to HRU and NA's.....................Delete after animation works
# trial_sf <- as.data.frame(my.shp_sf)
# trial_sf$HRUno <- suppressWarnings(as.numeric(as.character(my.shp_sf$HRUGIS)))
# trial_sf2 <- trial_sf[which(complete.cases(trial_sf$HRUno) ==TRUE),]


##
my.shp@data$ID_1 <- suppressWarnings(as.integer(as.character(my.shp@data$HRUGIS))) #remove NA's
my.shp2 <- my.shp[complete.cases(my.shp$ID_1),]
my.shp_f <- fortify(my.shp2, region = "ID_1")
ggplot(data = my.shp_f, aes(long, lat, group = group)) + 
  geom_polygon(fill = "grey80") + 
  ggtitle("HRU that meets the threshold values") + 
  theme(plot.title = element_text(hjust = 0.5))

tic()

##
my.shp_sf2 <- st_as_sf(my.shp2) %>% 
  ms_simplify()
###
my.shp_sf2$HRUGIS <- as.numeric(paste(my.shp_sf2$HRUGIS))
joined_spatial <- my.shp_sf2 %>% 
  left_join(Reach_info3)
colnames(joined_spatial)[17] <- "Date"
toc()

#joined_spatial$Nitrate <- joined_spatial$`NO3CONCmg/l`   ; `TNO3kg/ha`
## Plot the spatial data and create an animation


i <- 1
while (i < 2) {
  tic("Simulation for Animation")
  t <- joined_spatial %>% 
    # filter(Date < as.Date("2005-12-31")) %>% 
    filter(Date < as.Date("2001-12-31")) %>% 
    ggplot() +
    geom_sf(aes(fill = NSURQkg_ha)) +
    scale_fill_viridis_c() +
    theme_void() +
    coord_sf(datum = NA) +
    labs(title = "Nitrate contributed by HRU into surface runoff to the reach at Rapid Upper Colombia River Basin \n Date: {current_frame}") + 
    transition_manual(Date) + 
    # shadow_mark(past = TRUE, future = FALSE) + 
    theme(plot.title = element_text(hjust = 0.5, size=48, face = "bold"), 
          axis.title.x = element_text(size = 24, face = "bold"), 
          axis.title.y = element_text(size = 24, face = "bold"), 
          axis.text = element_text(size = 16))
  
  tt <- animate(t, width = 2000, height =1000)
  anim_save("Trial_Report_Del02.gif")
  animate(t, nframes=365, fps = 50, width = 2000, height =1000)
  # tt <- animate(t, fps = 5/1, width = 2000, height =1000)
  #pp <- animate(p, fps = 5/1)
  anim_save("Trial_Report_Del03.gif")
  i <- i + 3
  toc()
  
}
length(unique(joined_spatial$Date)) # indicates number of frames (nframes defualt = 100)



#### to avoid zero values for better visualization
the1 <- joined_spatial$NSURQkg_ha
the1[the1 == 0] <- NA
joined_spatial$NSURQkg_ha <- the1


the2 <- joined_spatial$ETmm
the2[the2==0] <- NA
joined_spatial$ETmm <- the2