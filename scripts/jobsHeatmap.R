library(plyr)
library(ggmap)
library(zipcode)
library(dplyr)

myLocation <- c(-99.27,19.25,-99.045,19.5)
myMap <- get_map(location = myLocation,
                 source="google",
                 maptype = "hybrid",
                 crop=FALSE)

jobLocations <- read.csv("~/data/Joblocations_2017-11-8_1039 (1).csv")
jobLocations <- filter( jobLocations, ( -99.27 <= longitude )  &  ( longitude <= -99.045 )
                        & ( 19.25 <= latitude ) & ( latitude <= 19.5 ) )
clust <- kmeans(jobLocations[,c("latitude","longitude")],3,1000)
head(jobLocations)

ggmap(myMap) + geom_point(aes(x=longitude,y=latitude,colour=as.factor(clust$cluster)),
                         data=jobLocations,
                         alpha=0.5,
                         #color=,
                         size=3)

mutate(jobLocations,g=clust)



