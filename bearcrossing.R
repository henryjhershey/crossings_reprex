bear <- st_read("bear.csv")
boundary <- cbind.data.frame(lon = rep(14.21058,4),lat=c(46.01988,45.93028,45.86729,45.82143))

tracks = bear[bear$tag.local.identifier %in% c("srecko","mishko","jana"),]
plot(location.lat~location.long,data=tracks,type="l",col=as.numeric(as.factor(tracks$tag.local.identifier)))
boundary <- cbind.data.frame(lon = rep(14.21058,4),lat=c(46.01988,45.93028,45.86729,45.82143))
#two points on jana's track that straddle the blue boundary line
points(14.12416107,45.88820898,pch=16,col="red")
points(14.22726928,45.88602848,pch=16,col="green")
lines(boundary,lwd=2,lty=2,col="blue")

#create sf objects.
#HELP
#I don't know how to preserve the timestamps of the nodes in the multilinestring.

tracks <- tracks %>%
  st_as_sf(coords = c("location.long", "location.lat"), agr = "identity",crs=4326) %>%
  group_by(tag.local.identifier) %>%
  summarise(do_union = FALSE) %>%
  st_cast("MULTILINESTRING")

boundary <- boundary %>% 
  st_as_sf(coords = c("lon","lat"), agr="identity",crs=4326) %>%
  summarise(do_union = FALSE)%>%
  st_cast("LINESTRING")

mapview(tracks) + mapview(boundary)

#find the nodes along individual bear tracks that straddle the boundary line.
#in the real world example, the line is not perfectly north/south, so can't cheat with hardcoding the longitude
