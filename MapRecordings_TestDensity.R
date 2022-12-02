# Plot locations of recordings
# Coded by Kate Snyder
# Created 10/12/2021
# Last Edited: 11/9/2021 - added code to search for densest area
# Edited 6/20/22 - can't figure out how to label with table, test for densest area kinda works

library("tidyverse")
library("ggplot2")
library("rnaturalearth")
library("lubridate")
library("ggpmisc")
library("tibble")


#### Load data ----
# 11/21/22
data=read.csv("/Users/kate/Library/CloudStorage/Box-Box/Maria_Kate_Nicole/Gzip Files/Song Sparrow_KTS_Aggregated072022/2022-11-19_SOSP_AverageFeatures_ByRecording_WithMedNotes.csv")
data=read.csv("/Users/kate/Library/CloudStorage/Box-Box/Maria_Kate_Nicole/All The Juncos/2022-11-19_DEJU_AverageFeatures_ByRecording_WithMedNotes.csv")

data = read.csv("/Users/kate/Library/CloudStorage/Box-Box/Maria_Kate_Nicole/Gzip Files/Song Sparrow_KTS_Aggregated072022/2022-11-16_SOSP_AverageFeatures_ByRecording.csv")

# New sparrows
setwd("/Users/kate/Library/CloudStorage/Box-Box/Maria_Kate_Nicole/Song sparrows from everywhere/R Analysis")

requestedfiles <- read.csv("/Users/kate/Documents/Creanza Lab/Ithaca Files/2021-11-09_SongSparrows_IthacaControlAreaRecordings_ToRequest.csv")
min(requestedfiles$Latitude)
max(requestedfiles$Latitude)
min(requestedfiles$Longitude)
max(requestedfiles$Longitude)

meta <- read.csv("XC_MelospizaMelodia_song_metadata.csv")
meta <-  read.csv("/Users/kate/Library/CloudStorage/Box-Box/Maria_Kate_Nicole/Song sparrows from everywhere/R Analysis/XC_MelospizaMelodia_song_metadata.csv")
#meta <- metadata_subset
meta$Date <- as.Date(meta$Date, format = "%Y-%m-%d")
meta = meta %>% 
  mutate(Date = ymd(Date)) %>% 
  mutate_at(vars(Date), funs(year, month, day))
#meta$year <- as.numeric(meta$year)

## Or get meta direct from file 6/20/22
sparrowdf <- read.csv("2022-06-20_SOSP_syllables_plusMetadata.csv")
meta <- sparrowdf
colnames(meta)[which(colnames(meta)=="DateRecorded")] <- "Date"

Era <- set.seed(10)
Era[meta$year < 2006] <- "2005 or earlier" 
Era[meta$year >= 2006 & meta$year < 2016] <- "2006-2015 (Pre)" 
Era[meta$year == 2016] <- "2016 (~During)"
Era[meta$year >= 2017 & meta$year < 2020] <- "2017-2019 (Post)"
Era[meta$year >= 2020] <- "2020-2021"
#meta <- cbind(meta, Era)
meta$Era <- Era
meta1 <- meta
metaSub <- meta %>% group_by(RecordingNum) %>% sample_n(1)
meta <- metaSub
# plots: 
# how many in a lat/lon grid of a certain size?
# how many do we already have parsed/in our data?

##
setwd("/Users/kate/Library/CloudStorage/Box-Box/Maria_Kate_Nicole/Individual Syll Analysis")


world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)
# Continental US: xlim = c(-127.15, -66), ylim = c(24.65, 50.97)
# NYState: xlim = c(-80.15, -72), ylim = c(38.65, 45.97)
# Whole US: xlim = c(-170.15, -50), ylim = c(24.65, 77.97)

ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-127.15, -66), ylim = c(24.65, 50.97), expand = FALSE) +
  geom_point(data = meta, mapping = aes(x = Longitude, y=Latitude, col = Era), size = 1.5, alpha = 0.6)

meta[which(is.na(meta$Latitude)),]

# New Juncos
setwd("~/Documents/Creanza Lab/Ithaca Files")
DEJmeta <- read.csv("XC_junco_song_metadata_NEWFILES.csv")
DEJmeta <- read.csv("XC_junco_song_metadata_NEWFILES_kts.csv")  #altered subspecies categories

min(DEJmeta$Latitude, na.rm = TRUE)
max(DEJmeta$Latitude, na.rm = TRUE)
min(DEJmeta$Longitude, na.rm = TRUE)
max(DEJmeta$Longitude, na.rm = TRUE)

ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-170.15, -50), ylim = c(24.65, 77.97), expand = FALSE) +
  geom_point(data = DEJmeta, mapping = aes(x = Longitude, y=Latitude, col = Subspecies_grouped), size = 0.9)



# Our sparrows (NY)
MLsparrows <- read.csv("/Users/kate/Documents/Creanza Lab/Ithaca Files/ML_2021-09-16T15-00_sonspa_audio.csv")
Era <- set.seed(10)
Era[MLsparrows$Year < 2006] <- "2005 or earlier" 
Era[MLsparrows$Year >= 2006 & MLsparrows$Year < 2016] <- "2006-2015 (Pre)" 
Era[MLsparrows$Year == 2016] <- "2016 (~During)"
Era[MLsparrows$Year >= 2017 & MLsparrows$Year < 2020] <- "2017-2019 (Post)"
Era[MLsparrows$Year >= 2020] <- "2020-2021"
MLsparrows <- cbind(MLsparrows, Era)
oursparrows <- read.csv("/Users/kate/Library/CloudStorage/Box-Box/Maria_Kate_Nicole/All The Sparrows/Gzips and R Analysis/2021-09-03Sparrow_Syll_Averaging.csv")
sum(oursparrows$V1 %in% MLsparrows$ML.Catalog.Number)
MLmeta <- MLsparrows[MLsparrows$ML.Catalog.Number %in% oursparrows$V1,]
MLmeta <- MLmeta[which(MLmeta$County %in% c("", "Tompkins", "Schuyler", "Wayne", "Seneca", "Cayuga", "Erie")),]
MLmeta$County[which(MLmeta$Latitude == max(MLmeta$Latitude, na.rm = T))]
MLmeta$Latitude[which(MLmeta$Latitude == max(MLmeta$Latitude, na.rm = T))]
MLmeta$Longitude[which(MLmeta$Longitude == max(MLmeta$Longitude, na.rm = T))]
# min Lat = 42.27741
# max Lat = 43.07931
# min Lon = -78.80951
# max Lon = -76.30545

XCrecsInRect <- meta[which(meta$Latitude < 43.07931-0.4 & meta$Latitude > 42.27741-0.4 & meta$Longitude < -76.30545+5.5 & meta$Longitude > -78.80951+5.5),] %>% 
  group_by(Era) %>% 
  summarise(nBirds = n())
dfLabel <- tibble(x = -73, y = 39, XCrecsInRect = list(XCrecsInRect))


#### Some older plots ----
library(RColorBrewer)
myColors <- brewer.pal(5, "Dark2")
names(myColors) <- levels(meta$Era)
custom_colors <- scale_colour_manual(name = "Era", values = myColors)

# plot XC recordings "from everywhere" on map by Era, with rectangle area size of West NY area
pdf(file = "XC_songsparrows_NE_mapped_withrect_AllEras_countTable.pdf", height = 6, width = 6)
ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-82.15, -67), ylim = c(33.65, 45.97), expand = FALSE) +
  geom_rect(aes(xmin = -78.80951, xmax = -76.30545, ymin = 42.27741, ymax = 43.07931)) + # ithaca
  geom_rect(aes(xmin = -78.80951+5.5, xmax = -76.30545+5.5, ymin = 42.27741-0.4, ymax = 43.07931-0.4)) + # Massachusetts
  geom_point(data = meta, mapping = aes(x = Longitude, y=Latitude, col = Era), size = 1.0, alpha = 0.7) +
  ggtitle("XC Song sparrow recordings") +
  geom_table(data = dfLabel, aes(x = -67.7, y = 34.1, label = XCrecsInRect)) +
  custom_colors
dev.off()

# find points within bounds in Massachusetts
meta[which(meta$Latitude < 43.07931-0.4 & meta$Latitude > 42.27741-0.4 & meta$Longitude < -76.30545+5.5 & meta$Longitude > -78.80951+5.5),]
MLsparrows <- meta
MLrecsInRect <- MLsparrows[which(MLsparrows$Latitude < 42 & MLsparrows$Latitude > 40 & MLsparrows$Longitude < -72.5 & MLsparrows$Longitude > -75),] %>% 
  group_by(Era) %>% 
  summarise(nBirds = n())
dfLabel <- tibble(x = -73, y = 39, MLrecsInRect = list(MLrecsInRect))



# plot ML recordings "from everywhere" on map by Era
library(RColorBrewer)
myColors <- brewer.pal(5, "Dark2")
names(myColors) <- levels(MLsparrows$Era)
custom_colors <- scale_colour_manual(name = "Era", values = myColors)


#### 11/21/22 ----
library(maps)
county_data <- ggplot2::map_data('county')
state_data <- ggplot2::map_data('state')
NE_county_data = county_data[which(county_data$region %in% c("new york","new jersey","vermont", "pennsylvania")),]
NE_county_data = cbind(NE_county_data, "county")
colnames(NE_county_data)[7] = "boundaries"
NE_state_data = state_data[which(state_data$region %in% c("new york","new jersey","vermont", "pennsylvania")),]
NE_state_data = cbind(NE_state_data, "state")
colnames(NE_state_data)[7] = "boundaries"
NE_data = rbind(NE_county_data, NE_state_data)
ggplot(NE_data, aes(x = long, y = lat, group = group)) +
     geom_polygon(colour = "black", fill = 'white') + 
  #   geom_polygon( colour = "grey50", fill='white') +
     geom_rect(aes(xmin = -78.80951, xmax = -76.30545, ymin = 42.27741, ymax = 43.07931))


#### 6/20/22 ----
MLsparrows <- meta
MLrecsInRect <- MLsparrows[which(MLsparrows$Latitude < 42 & MLsparrows$Latitude > 40 & MLsparrows$Longitude < -72.5 & MLsparrows$Longitude > -75),] %>% 
  group_by(Era) %>% 
  summarise(nBirds = n())
dfLabel <- tibble(x = -73, y = 39, MLrecsInRect = list(MLrecsInRect))

pdf(file = paste0(Sys.Date(),"songsparrows_NE_mapped_withrect_AllEras_countTable.pdf"), height = 6, width = 6)
ggplot(data = world) +
geom_sf() +
  coord_sf(xlim = c(-82.15, -67), ylim = c(33.65, 45.97), expand = FALSE) +
  geom_rect(aes(xmin = -78.80951, xmax = -76.30545, ymin = 42.27741, ymax = 43.07931)) + # ithaca
  geom_rect(aes(xmin = -75, xmax = -72.5, ymin = 40, ymax = 42)) + # NYC
#  geom_point(data = MLsparrows[!MLsparrows$Year %in% c(2017:2021), ], mapping = aes(x = Longitude, y=Latitude, col = Era), size = 1.0, alpha = 0.7) +
  geom_point(data = meta, mapping = aes(x = Longitude, y=Latitude, col = Era), size = 1.0, alpha = 0.7) +
  #ggtitle("ML Song sparrow recordings (need to request)") +
  geom_table(data = dfLabel, aes(x = -67.7, y = 34.1, label = MLrecsInRect)) 
  #custom_colors
  #geom_text(aes(x = -68, y = 39, label = as.character(MLrecsInRect[1]))) 
dev.off()




## ML sparrows east of Mississippi
AllSparrowRecs <- MLmeta <- meta[which(meta$year > 2005),]
MLmetaEast <- MLmeta[which(meta$Longitude > -89),]
#MLmeta <- MLsparrows[which(MLsparrows$State != "New York"),]
#MLmetaEast<- MLmeta[which(MLmeta$Longitude > -89),]
MLrecsInRect <- MLmetaEast %>% 
  group_by(Era) %>% 
  summarise(nBirds = n())
dfLabel <- tibble(x = -73, y = 39, MLrecsInRect = list(MLrecsInRect))

pdf(file = paste(Sys.Date(),"songsparrows_mapped_AllEras_countTable_EastOfMississippi.pdf"), height = 6, width = 10)
ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-127.15, -66), ylim = c(24.65, 50.97), expand = FALSE) +
  #  geom_point(data = MLsparrows[!MLsparrows$Year %in% c(2017:2021), ], mapping = aes(x = Longitude, y=Latitude, col = Era), size = 1.0, alpha = 0.7) +
  geom_point(data = MLmeta, mapping = aes(x = Longitude, y=Latitude, col = Era), size = 1.0, alpha = 0.7) +
  ggtitle("ML Song sparrow recordings (need to request)") +
  geom_table(data = dfLabel, aes(x = -65.7, y = 25.1, label = MLrecsInRect)) +
  custom_colors +
  geom_vline(xintercept = -89, linetype="dotted", color = "blue", size=1.5)
#geom_text(aes(x = -68, y = 39, label = as.character(MLrecsInRect[1]))) 
dev.off()



### Find densest set of points within given area
# Combine ML and XC databases
MLsparrows <- read.csv("/Users/kate/Library/CloudStorage/Box-Box/Maria_Kate_Nicole/Reference Sheets/Song Sparrow/ML_2021-09-16T15-00_sonspa_audio.csv")
Source <- rep("ML", times = length(MLsparrows$FileName))
MLsparrows <- cbind(MLsparrows, Source)
XCsparrows <- query_xc("Melospiza melodia type:song")
XCsparrows <- read.csv("/Users/kate/Library/CloudStorage/Box-Box/Maria_Kate_Nicole/Reference Sheets/Song Sparrow/XC_MelospizaMelodia_song_metadata.csv")
Source <- rep("XC", times = length(XCsparrows$Recording_ID))
XCsparrows <- cbind(XCsparrows, Source)

XCsparrows$Date <- as.Date(XCsparrows$Date, format = "%Y-%m-%d")
XCsparrows = XCsparrows %>% 
  mutate(Date = ymd(Date)) %>% 
  mutate_at(vars(Date), funs(year, month, day))
#MLSSsub <- MLsparrows[,c("ML.Catalog.Number", "Year", "Latitude", "Longitude", "Source")]
MLSSsub <- MLsparrows[,c("FileName", "Year", "Latitude", "Longitude", "Source")]
colnames(MLSSsub) <- c("Recording_ID", "year", "Latitude", "Longitude", "Source")
XCSSsub <- XCsparrows[,c("Recording_ID", "year", "Latitude", "Longitude", "Source")]

AllSparrowRecs <- rbind(XCSSsub,MLSSsub)
AllSparrowRecs <- MLmeta <- meta[which(meta$year > 2005),]  # 6/20/2022 process
AllSparrowRecs$Recording_ID <- AllSparrowRecs$RecordingNum
AllSparrowRecsPre <- AllSparrowRecs[which(AllSparrowRecs$year %in% c(2006:2016)),]
AllSparrowRecsPost <- AllSparrowRecs[which(AllSparrowRecs$year %in% c(2017:2019)),]
AllSparrowRecsNow <-  AllSparrowRecs[which(AllSparrowRecs$year %in% c(2019:2022)),]
#write.csv(AllSparrowRecs, file = "XCML_2021-11-09_SongSparrow_song_metadata.csv")

# strategy: (do this just using "pre" points)
# for loop, each loop a new circle of radius r centered on a new point "center" c(i,j)
# which of my points have a longitude between center-r and center+r? - could do to speed up next step, not essential
# calculate "distance" from "center" to each Lat-Lon point distance = sqrt((Xp^2 - Xc^2) + (Yp^2 - Yc^2))
# count # with distance <= r
# if count >= ~10(?) store lat/lon of center and count of points in df

r = 1
longitudevec <- -70:-84
latitudevec <- 33:50
outdf <- set.seed(10)
for (i in 1:length(longitudevec)) {
  for (j in 1:length(latitudevec)) {
    center = c(longitudevec[i],latitudevec[j])
    
    points <- set.seed(10)
    for (k in 1:length(AllSparrowRecsPre$Recording_ID)) {  # replace this with apply function
      points[[k]] <- c(AllSparrowRecsPre$Longitude[k], AllSparrowRecsPre$Latitude[k])
      # distance <- sqrt((point[1]^2 - center[1]^2) + (point[2]^2 - center[2]^2))
      # if (distance <= r) {
      #   
      #   newrow <- c(center[1], center[2], )
      # }
      
    }  # end (replace this with apply function)
    # pythagorean <- function (a,b) {
    #   sqrt((a[1] - b[1])^2 + (a[2] - b[2])^2)
    # }
    pythagorean <- function (a) {
      sqrt((a[1] - center[1])^2 + (a[2] - center[2])^2)
    }
    distance <- lapply(points, pythagorean)
    # print(head(distance))
    
    numPoints <- sum(distance <= r, na.rm = TRUE)
    
    if (numPoints > 10) {
    newrow <- c(center[1], center[2], numPoints)
    outdf <- rbind(outdf, newrow)
    }
  }
}
#colnames(outdf) <- c("CenterLon", "CenterLat", "numPoints")
#outdf <- as.data.frame(outdf)


metaRect<- meta[which(meta$Longitude < -72.5 & meta$Longitude > -73.8 & meta$Latitude < 43 & meta$Latitude > 40),]
MLrecsInRect <- metaRect %>% 
  group_by(Era) %>% 
  summarise(nBirds = n())
dfLabel <- tibble(x = -73, y = 39, MLrecsInRect = list(MLrecsInRect))

library(ggforce)

pdf(file = paste0(Sys.Date(),"OurChipperedSOSPs_mapped_Pre_circlesR1_min10_filled.pdf"), height = 6, width = 10)
ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-127.15, -66), ylim = c(24.65, 50.97), expand = FALSE) +
  #  geom_point(data = MLsparrows[!MLsparrows$Year %in% c(2017:2021), ], mapping = aes(x = Longitude, y=Latitude, col = Era), size = 1.0, alpha = 0.7) +
 # geom_circle(aes(x0 = CenterLon, y0 = CenterLat, r = 1, fill = numPoints, alpha = 0.9), data = outdf) +
  geom_rect(aes(xmin = -78.80951, xmax = -76.30545, ymin = 42.27741, ymax = 43.07931)) + # ithaca
  geom_rect(aes(xmin = -78.80951, xmax = -76.30545, ymin = 42.27741, ymax = 43.07931)) +
  geom_point(data = AllSparrowRecsPre, mapping = aes(x = Longitude, y=Latitude, col = as.factor(year)), size = 1.0, alpha = 0.7) +
  ggtitle("Pre-drought song sparrow recordings + circles with radius = 1 \ncontaining at least 10 recordings 2006-2016") +
  geom_table(as_tibble(outdf) ,aes(x = -65.7, y = 25.1, label = outdf)) +
 # custom_colors +
  geom_vline(xintercept = -89, linetype="dotted", color = "blue", size=1.5)

#geom_text(aes(x = -68, y = 39, label = as.character(MLrecsInRect[1]))) 
dev.off()

metaRect<- MLmeta[which(MLmeta$Longitude < -72.5 & MLmeta$Longitude > -73.8 & MLmeta$Latitude < 43 & MLmeta$Latitude > 40),]
MLrecsInRect <- metaRect %>% 
  group_by(Era) %>% 
  summarise(nBirds = n())
dfLabel <- tibble(x = -73, y = 39, MLrecsInRect = list(MLrecsInRect))


require(warbleR)
## Juncos - combine XC and ML recordings
xcDEJU <- query_xc("Junco hyemalis")
#write.csv(xcDEJU, file= "XC_2021-11-09_junco_song_metadata.csv")
xcDEJU <- read.csv("XC_2021-11-09_junco_song_metadata.csv")
MLjuncos <- read.csv("/Users/kate/Documents/Creanza Lab/Ithaca Files/ML_2021-11-09T18-03_daejun_audio.csv")
Source <- rep("XC", times = length(xcDEJU$Recording_ID))
xcDEJU <- cbind(xcDEJU, Source)
Source <- rep("ML", times = length(MLjuncos$ML.Catalog.Number))
MLjuncos <- cbind(MLjuncos, Source)

xcDEJU$Date <- as.Date(xcDEJU$Date, format = "%Y-%m-%d")
xcDEJU = xcDEJU %>% 
  mutate(Date = ymd(Date)) %>% 
  mutate_at(vars(Date), funs(year, month, day))
MLjuncosub <- MLjuncos[,c("ML.Catalog.Number", "Year", "Latitude", "Longitude")] #, "Source")]
colnames(MLjuncosub) <- c("Recording_ID", "year", "Latitude", "Longitude") #, "Source")
XCjuncosub <- xcDEJU[,c("Recording_ID", "year", "Latitude", "Longitude", "Source")]


AllJuncoRecs <- read.csv("/Users/kate/Documents/Creanza Lab/Ithaca Files/XCML_2021-11-09_Junco_song_metadata.csv")
#AllJuncoRecs <- rbind(XCjuncosub,MLjuncosub)
AllJuncoRecs <- AllJuncoRecs[which(AllJuncoRecs$year %in% c(2006:2021)),]
AllJuncoRecsPre <- AllJuncoRecs[which(AllJuncoRecs$year %in% c(2006:2016)),]
AllJuncoRecsPost <- AllJuncoRecs[which(AllJuncoRecs$year %in% c(2017:2019)),]
AllJuncoRecsNow <- AllJuncoRecs[which(AllJuncoRecs$year %in% c(2019:2022)),]
#write.csv(AllJuncoRecs, file = "XCML_2021-11-09_Junco_song_metadata.csv")

minlat <- 39.7
maxlat <- 43
minlon <- -75.6
maxlon <- -72.5
df <- AllJuncoRecsNow

areaRecs <- df[which(df$Latitude > minlat & df$Latitude < maxlat & df$Longitude > minlon & df$Longitude < maxlon),]
areaRecs
areaRecs <- areaRecs[,2:6]
write.csv(areaRecs, "2022-02-04_junco_control-region-files.csv")

ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-82.15, -67), ylim = c(33.65, 45.97), expand = FALSE) +
 # geom_rect(aes(xmin = -78.80951, xmax = -76.30545, ymin = 42.27741, ymax = 43.07931)) + # ithaca
  #geom_rect(aes(xmin = minlon, xmax = maxlon, ymin = minlat, ymax = maxlat)) +
#  geom_point(data = MLsparrows[!MLsparrows$Year %in% c(2017:2021), ], mapping = aes(x = Longitude, y=Latitude, col = Era), size = 1.0, alpha = 0.7) +
  geom_point(data = AllJuncoRecsNow, mapping = aes(x = Longitude, y=Latitude, col = year), size = 1.0, alpha = 0.7) +
  ggtitle("Junco recordings 2019-2021") 

  #geom_table(data = dfLabel, aes(x = -67.7, y = 34.1, label = MLrecsInRect)) +
  #custom_colors


#### Make plotting into a function - pdf getting messed up somehow, not sure why. 

#FindDenseRecs(data = AllJuncoRecs, minPoints = 20, speciesLabel = "Junco", subsetlabel = "AllYears")

data = requestNYCsparrows
minPoints = 10
speciesLabel = "SongSparrow" 
subsetlabel = "2006-2019_requestSubset"
longitudevec = -89:-63 
latitudevec = 29:50

AllJuncoRecs <- read.csv("XCML_2021-11-09_Junco_song_metadata.csv")
AllJuncoRecs <- AllJuncoRecs[which(AllJuncoRecs$year %in% c(2006:2021)),]
AllJuncoRecsPre <- AllJuncoRecs[which(AllJuncoRecs$year %in% c(2006:2016)),]
AllJuncoRecsPost <- AllJuncoRecs[which(AllJuncoRecs$year %in% c(2017:2019)),]
data = AllJuncoRecsPre
minPoints = 10
speciesLabel = "Junco" 
subsetlabel = ""
#longitudevec = -89:-63 
longitudevec = -80:-71
latitudevec = 38:46
r = 1.2
xlims <- c(-84.15, -72)
ylims <- c(33.65, 45.97)

xlims = c(-127.15, -66)
ylims = c(24.65, 50.97)

#FindDenseRecs <- function(data, r = 1, longitudevec = -89:-63, latitudevec = 29:50, minPoints, speciesLabel, subsetlabel) {

require(ggforce)  
  
outdf <- set.seed(10)
for (i in 1:length(longitudevec)) {
  for (j in 1:length(latitudevec)) {
    center = c(longitudevec[i],latitudevec[j])
    
    points <- set.seed(10)
    for (k in 1:length(data$Recording_ID)) {  
      points[[k]] <- c(data$Longitude[k], data$Latitude[k])
    }  # end for k

    pythagorean <- function (a) {
      sqrt((a[1] - center[1])^2 + (a[2] - center[2])^2)
    }
    
    distance <- lapply(points, pythagorean)
    
    numPoints <- sum(distance <= r, na.rm = TRUE)
    
    if (numPoints >= minPoints) {
      newrow <- c(center[1], center[2], numPoints)
      outdf <- rbind(outdf, newrow)
    }
  }
}
colnames(outdf) <- c("CenterLon", "CenterLat", "numPoints")
outdf <- as.data.frame(outdf)
print(head(outdf))
dfLabel <- tibble(x = -73, y = 39, outdf = list(outdf))

#pdf(file = paste0("MLXC_",speciesLabel,subsetlabel,"_mapped_circlesR",r,"_min",minPoints,"_filled.pdf"), height = 6, width = 10)
ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = xlims, ylim = ylims, expand = FALSE) +
 # geom_circle(aes(x0 = CenterLon, y0 = CenterLat, r = r, fill = numPoints, alpha = 0.9), data = outdf) +
  geom_point(data = data, mapping = aes(x = Longitude, y=Latitude, col= as.factor(year)), size = 1.0, alpha = 0.7) #+
  #ggtitle(paste0(speciesLabel," recordings (XC and ML) + circles with radius = ", r, "\ncontaining at least ", minPoints, " recordings ", subsetlabel)) #+

 # geom_table(data = dfLabel, aes(x = -84, y = 34.1, label = outdf)) 
  #geom_table(data = dfLabel, aes(x = -126.7, y = 25.1, label = outdf)) 
 # geom_vline(xintercept = -89, linetype="dotted", color = "blue", size=1.5)

dev.off()
print(head(data))
#} # end function


requestNYCsparrows <- AllSparrowRecs[which(AllSparrowRecs$year %in% 2006:2019 & AllSparrowRecs$Latitude > 39 & AllSparrowRecs$Latitude < 42 & AllSparrowRecs$Longitude > -75 & AllSparrowRecs$Longitude < -73),]


meta <- read.csv("/Users/kate/Library/CloudStorage/Box-Box/Maria_Kate_Nicole/Reference Sheets/Song Sparrow/2022-06-23_SOSP_Complex_Metadata_SelfRecsCoordinatesAdded_AllMLXC_addedMissingNYCoords_IncludeOGEras.csv")  # still need to add regions
OurMetaOut <- metaOut[which(metaOut$Era.OG %in% c("Pre","During","Post")),]
lamin=min(OurMetaOut$Latitude)
lamax=max(OurMetaOut$Latitude)
lomin=min(OurMetaOut$Longitude)
lomax=max(OurMetaOut$Longitude)

ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-82.15, -67), ylim = c(33.65, 45.97), expand = FALSE) +
   geom_rect(aes(xmin = lomin, xmax = lomax, ymin = lamin, ymax = lamax))



#### Plotting sparrows ----
meta <- read.csv("/Users/kate/Library/CloudStorage/Box-Box/Maria_Kate_Nicole/Reference Sheets/Song Sparrow/2022-06-24_SOSP_Complex_Metadata_SelfRecsCoordinatesAdded_AllMLXC_addedMissingNYCoords_IncludeOGEras_UsedRegions_Behaviors.csv")

metaReg<- meta[which(meta$Region %in% c("Drought","Control")),]
metaReg$Region <- factor(metaReg$Region, levels=c("Drought","Control"))
ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-82.15, -67), ylim = c(33.65, 45.97), expand = FALSE) +
  # geom_rect(aes(xmin = -78.80951, xmax = -76.30545, ymin = 42.27741, ymax = 43.07931)) + # ithaca
  #geom_rect(aes(xmin = minlon, xmax = maxlon, ymin = minlat, ymax = maxlat)) +
  #  geom_point(data = MLsparrows[!MLsparrows$Year %in% c(2017:2021), ], mapping = aes(x = Longitude, y=Latitude, col = Era), size = 1.0, alpha = 0.7) +
  geom_point(data = metaReg, mapping = aes(x = Longitude, y=Latitude, col = Region), size = 1.0, alpha = 0.7) +
  ggtitle("Song sparrow recordings used from drought and control sites") 



#### Better geo package ----
dataSOSP=read.csv("/Users/kate/Library/CloudStorage/Box-Box/Maria_Kate_Nicole/Gzip Files/Song Sparrow_KTS_Aggregated072022/2022-11-19_SOSP_AverageFeatures_ByRecording_WithMedNotes.csv")
dataDEJU=read.csv("/Users/kate/Library/CloudStorage/Box-Box/Maria_Kate_Nicole/All The Juncos/2022-11-19_DEJU_AverageFeatures_ByRecording_WithMedNotes.csv")
dataSOSP$species = "Song sparrow"
dataDEJU$species = "Dark-eyed junco"
colnames(dataSOSP)[colnames(dataSOSP) %in% colnames(dataDEJU)]
unique(dataDEJU$Region)
dataDEJU = dataDEJU[which(!is.na(dataDEJU$Region)),]
unique(dataSOSP$Region)
unique(dataSOSP$Era)
keepcols = c("RecordingNum","Latitude","Longitude","Era","Region","year","species")
dataAll = rbind(dataSOSP[,keepcols],dataDEJU[,keepcols])

library("maps")
library("mapdata")
state <- map_data("state")
NE <- subset(state, region %in% c("new york", "new jersey", "connecticut", "massachusetts", "vermont", "new hampshire", "pennsylvania"))
counties <- map_data("county")
NY_counties <- subset(counties,region=="new york")
# world <- ne_countries(scale = "medium", returnclass = "sf")
ggplot(data = NE, mapping = aes(x=long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(color="black", fill="gray") +
  geom_polygon(data=NY_counties, fill=NA, color="white") + 
  geom_polygon(color="black", fill=NA) +
  geom_rect(aes(xmin = -75, xmax = -72.5, ymin = 40, ymax = 42), color = "blue", fill = NA) +
  geom_rect(aes(xmin = -78.9, xmax = -76.3, ymin = 42.2, ymax = 43.1), color = "red", fill = NA) 


# add points?  
g <- ggplot(data = NE, mapping = aes(x=long, y = lat, group = group)) +
    coord_fixed(1.3) +
    geom_polygon(color="black", fill="grey90") +
    geom_polygon(data=NY_counties, fill=NA, color="white") + 
    geom_polygon(color="black", fill=NA) +
    geom_rect(aes(xmin = -74.5, xmax = -72, ymin = 42.2, ymax = 44.6), color = "blue", fill = NA) +   #DEJU
    geom_rect(aes(xmin = -75, xmax = -72.5, ymin = 40, ymax = 42), color = "blue", fill = NA) +   #SOSP
    geom_rect(aes(xmin = -75.45, xmax = -78.9, ymin = 42.2, ymax = 43.2), color = "red", fill = NA) +
    #geom_point(data = dataSOSP, mapping = aes(x = Longitude, y=Latitude, group = Era), size = 1.0, alpha = 0.7, color = "yellow") +
    #geom_point(data = dataDEJU, mapping = aes(x = Longitude, y=Latitude, group = Era), size = 1.0, alpha = 0.7, color = "violet") +
    geom_point(data = dataAll, mapping = aes(x = Longitude, y=Latitude, group = species, color = species), size = 0.8, alpha = 0.7) +
    scale_color_manual(values=c("orange","purple"))
g +   theme_void() + ggtitle("Recordings used from drought and control sites") # +
    theme_classic()
    
    
    g <- ggplot(data = NE, mapping = aes(x=long, y = lat, group = group)) +
      coord_fixed(1.3) +
      geom_polygon(color="black", fill="grey90") +
      geom_polygon(data=NY_counties, fill=NA, color="white") + 
      geom_polygon(color="black", fill=NA) +
      geom_rect(aes(xmin = -74.5, xmax = -72, ymin = 42.2, ymax = 44.6), color = "blue", fill = NA) +   #DEJU
      geom_rect(aes(xmin = -75, xmax = -72.5, ymin = 40, ymax = 42), color = "blue", fill = NA) +   #SOSP
      geom_rect(aes(xmin = -75.45, xmax = -78.9, ymin = 42.2, ymax = 43.2), color = "red", fill = NA) +
      #geom_point(data = dataSOSP, mapping = aes(x = Longitude, y=Latitude, group = Era), size = 1.0, alpha = 0.7, color = "yellow") +
      #geom_point(data = dataDEJU, mapping = aes(x = Longitude, y=Latitude, group = Era), size = 1.0, alpha = 0.7, color = "violet") +
      geom_point(data = dataSOSP, mapping = aes(x = Longitude, y=Latitude, group = species), size = 1, alpha = 0.9, color = "orange") +
      geom_point(data = dataDEJU, mapping = aes(x = Longitude, y=Latitude, group = species), size = 0.7, alpha = 0.7, color = "purple") 

      
    g +   theme_void() + ggtitle("Recordings used from drought and control sites") # +
    theme_classic()
    
    
    ggplot(data = NE, mapping = aes(x=long, y = lat, group = group)) + stat_density_2d(data = dataSOSP, mapping = aes(x = Longitude, y=Latitude, size = ..density..), geom = "point", n = 30, contour = FALSE)
  
      
    #saved in Dissertation Materials
    
    # theme(    legend.position = c(-72, 40),
    #legend.justification = c("right", "top"),
    #legend.box.just = "right",
    #legend.margin = margin(6, 6, 6, 6), 
    #axis.line = element_line()) +
    

# From postchipper processing 11/21/2022
#  enddfRectDrought <- enddf[which(enddf$Longitude <  -75.45 & enddf$Longitude > -78.9 & enddf$Latitude < 43.2 & enddf$Latitude > 42.2),]
# Juncos: enddfRectControl <- enddf[which(enddf$Latitude > 42 & enddf$Latitude < 44.6 & enddf$Longitude > -74.5 & enddf$Longitude < -72),]
# SOSP: enddfRectControl <- enddf[which(enddf$Longitude < -72.5 & enddf$Longitude > -75 & enddf$Latitude < 42 & enddf$Latitude > 40),]

as.character(c(100391541, 109768661, 169176771, 170568851,52270921,  61988811,  86994471,  94332811)) %in% data$RecordingNum
