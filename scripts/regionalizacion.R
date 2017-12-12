library(plyr)
library(ggmap)
library(zipcode)
library(dplyr)

myLocation <- c(-99.4,19.1,-99.0,19.8)
myMap <- get_map(location = myLocation,
                 source="google",
                 maptype = "roadmap",
                 crop=FALSE)

jobLocations <- read.csv("~/scripts/iguanafixData/data/Zona Metro Norte-Poniente - Zona Metro N-P.csv"
                         ,skip = 2,
                         header = TRUE)
#jobLocations <- mutate(jobLocations,longitude=lat,latitude=lat)
jobLocations <- jobLocations[1:25,]
#jobLocations <- filter( jobLocations, ( -99.27 <= longitude )  &  ( longitude <= -99.045 )
#                        & ( 19.25 <= latitude ) & ( latitude <= 19.5 ) )
#clust <- kmeans(jobLocations[,c("longitude","latitude")],2,1000)
head(jobLocations)

#mutate(jobLocations,g=clust$cluster)

ggmap(myMap) + geom_point(aes(x=longitude,
                              y=latitude,
                              colour=as.factor(Región)),
                         data=jobLocations,
                         #alpha=0.5,
                         #color=,
                         size=5)


jobOrd <- select(jobLocations,longitude:latitude)
jobOrd <- mutate(jobOrd,clust=jobLocations$Región)
jobOrd <- arrange(jobOrd,clust)

## cluster 1
cluster1 <- filter(jobOrd,clust=='Región 1')
res.c1 <- sapply(1:nrow(cluster1), function(i) paste('(',
                                                cluster1[i,2],
                                                ',',
                                                cluster1[i,1],
                                                ')',
                                                sep=''))
writeLines(res.c1,'cluster1.txt')

## cluster 5
cluster5 <- filter(jobOrd,clust=='Región 5')
res.c5 <- sapply(1:nrow(cluster5), function(i) paste('(',
                                                     cluster5[i,2],
                                                     ',',
                                                     cluster5[i,1],
                                                     ')',
                                                     sep=''))
writeLines(res.c5,'cluster5.txt')
