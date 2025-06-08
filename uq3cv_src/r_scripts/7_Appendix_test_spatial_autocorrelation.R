rm(list=ls())

setwd()

library(ggpubr)
library(glmmTMB)
library(rgdal)
library(sp)
library(ggplot2)
library(DHARMa)

data <- read.csv('allplots_scenarios_comm_matrix.csv')
data <- data[,c(2,3,4,5,56)]

colnames(data)[1] <- 'plot_no'
colnames(data)[5] <- 'richness'

#get lat/lon

# 
# plots <- readOGR('filepath_for_shapefile')
# plot(plots) #check
# plots <- spTransform(plots, '+init=epsg:4326') # put in wgs84 coordinates
# centroids <- data.frame(gCentroid(plots, byid = T, id =
#                                     plots@data$plot_no)@coords) # calculate polygon centers / coordinates for each plot
# centroids$plot_no <- row.names(centroids)
# coordget <- centroids
# #rename lat and lon columns
# colnames(coordget) <- c("lon", 'lat', 'plot_no')
# 
# write.csv(coordget, 'ConFoBi_coordinates.csv')

coords <- read.csv('ConFoBi_coordinates.csv')
coords <- coords[,-1]

final <- left_join(data, coords)
final$plot_no <- as.factor(final$plot_no)
final$duration <- as.factor(final$duration)
final$intensity <- as.factor(final$intensity)
final$dayphase <- as.factor(final$dayphase)


m1 <- glmmTMB(richness ~ duration * intensity * dayphase + (1|plot_no), data=final)
summary(m1)
res_m1 <- simulateResiduals(m1)
recalc_m1 <- recalculateResiduals(res_m1, group = final$plot_no)

auto_data <- unique(final[,c(1,6,7)])
auto_data$scaled_resids <- recalc_m1$scaledResiduals

testSpatialAutocorrelation(x = auto_data$lon, y = auto_data$lat, auto_data$scaled_resids) 
