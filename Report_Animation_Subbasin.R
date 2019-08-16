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
library(RODBC);library(tictoc);library(sf)

#C:\Users\gebr547\OneDrive - PNNL

dir("C:/Users/gebr547/Documents/PNNL2019/WaterQuality/Hydro_GIS/")
#setwd("C:/Users/gebr547/Documents/PNNL2019/WaterQuality/Hydro_GIS/")
#my.shp <- readOGR(dsn = "C:/Users/gebr547/Documents/PNNL2019/WaterQuality/Hydro_GIS/Watershed.shp")

db <- file.path("C:/Users/gebr547/Documents/PNNL2019/SWAT/ForUSU/ForUSU/Scenarios/Daily2Year/TablesOut/SWATOutput.mdb")

dir("C:/Users/gebr547/OneDrive - PNNL/Documents/PNNL2019/WaterQuality/Hydro_GIS/")
setwd("C:/Users/gebr547/OneDrive - PNNL/Documents/PNNL2019/WaterQuality/Hydro_GIS/")
my.shp <- readOGR(dsn = "C:/Users/gebr547/OneDrive - PNNL/Documents/PNNL2019/WaterQuality/Hydro_GIS/Watershed.shp")
my.shp <- readOGR(dsn = "C:/Users/gebr547/OneDrive - PNNL/Documents/PNNL2019/WaterQuality/Hydro_GIS/Watershed.shp")

db <- file.path("C:/Users/gebr547/OneDrive - PNNL/Documents/PNNL2019/SWAT/ForUSU/ForUSU/Scenarios/Daily2Year/TablesOut/SWATOutput.mdb")

channel <- odbcConnectAccess2007(db)
# data-of-interest "sub", "hru", "sed", "rch"
Reach_info <- sqlFetch(channel, "rch")
Reach_info$subbasin2 <- Reach_info$SUB
Reach_info$Subbasin <- Reach_info$SUB

Reach_info$YYYYMMDD <- as.Date(as.character(Reach_info$YYYYMMDD), "%Y%m%d")
Reach_info2 <- Reach_info %>% filter(YEAR == 2000)
#Reach_info2 <- Reach_info %>% filter(YEAR == 2000 | YEAR == 2001 | YEAR == 2001)
#Reach_info3 <- Reach_info2[,c(1:5,9,51,54)]
Reach_info3 <- Reach_info2[,c(1:5,9,19,51,54)] # 19 nitrate loading
colnames(Reach_info3)

my.shp_sf <- st_as_sf(my.shp) %>% 
  ms_simplify()

ggplot(my.shp_sf) +
  geom_sf()


my.shp@data$ID_1 <- as.integer(rownames(my.shp@data)) + 1
my.shp_f <- fortify(my.shp, region = "ID_1")
ggplot(data = my.shp_f, aes(long, lat, group = group)) + geom_polygon(fill = "grey80")

tic()
## Join the rain.data with a properly formatted date with the spatial data. 
# my.shp_sf$Subbasin <- as.integer(my.shp_sf$Subbasin)
my.shp_sf$Subbasin <- as.numeric(paste(my.shp_sf$Subbasin)) # Xinguan comment

my.shp_sf$ID_1 <- as.numeric(paste(my.shp_sf$Subbasin))

joined_spatial <- my.shp_sf %>% 
  left_join(Reach_info3)
colnames(joined_spatial)[22] <- "Date"
toc()

#joined_spatial$Nitrate <- joined_spatial$`NO3CONCmg/l`
## Plot the spatial data and create an animation

# variables EVAPcms     `NO3CONCmg/l`......NO3_OUTkg

t <- joined_spatial %>% 
  # filter(Date < as.Date("2005-12-31")) %>% 
  filter(Date < as.Date("1997-02-01")) %>% 
  ggplot() +
  geom_sf(aes(fill = `NO3CONCmg/l`)) +
  scale_fill_viridis_c() +
  theme_void() +
  coord_sf(datum = NA) +
  labs(title = "Nitrate Concentration of Priest Rapid Upper Colombia River Basin \n Date: {current_frame}") + 
  transition_manual(Date) + 
  # shadow_mark(past = TRUE, future = FALSE) + 
  theme(plot.title = element_text(hjust = 0.5, size=48, face = "bold"), 
        axis.title.x = element_text(size = 24, face = "bold"), 
        axis.title.y = element_text(size = 24, face = "bold"), 
        axis.text = element_text(size = 16))

animate(t, nframes=731, fps = 100, width = 2000, height =1000)
anim_save("Subbasin_NitrateConcentration.gif")



#### to avoid zero values for better visualization
the1 <- joined_spatial$`NO3CONCmg/l`
the1[the1 == 0] <- NA
joined_spatial$`NO3CONCmg/l` <- the1


the2 <- joined_spatial$NO3_OUTkg
the2[the2==0] <- NA
joined_spatial$NO3_OUTkg <- the2