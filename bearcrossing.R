# ---- bring packages ---- 
{
  library(dplyr)
  library(ggplot2)
  library(lwgeom) # st_startpoint
  library(mapview)
  library(purrr)
  library(readr)
  library(sf)
  make_line <- function(lon, lat, llon, llat) {
    st_linestring(matrix(c(lon, llon, lat, llat), 2, 2,))
  }
}

# ---- bring csv ---- 
bear <- read.csv("bear.csv")

glimpse(bear)

# ---- create example boundary ---- 
boundary <- cbind.data.frame(lon = rep(14.21058, 4),
                             lat = c(46.01988, 45.93028, 45.86729, 45.82143))

# ---- create example boundary sf ---- 
boundary_sf <- boundary %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  summarise(do_union = FALSE) %>%
  st_cast("LINESTRING")

# ---- filter out tracks for example ---- 
tracks <- bear[bear$tag.local.identifier %in% c("srecko", "mishko", "jana"), ]

# ---- plot tracks ----- 
plot(location.lat ~ location.long,
     data = tracks,
     type = "l",
     col = as.numeric(as.factor(tracks$tag.local.identifier)))

# add two points on jana's track that straddle the blue boundary line
points(14.12416107, 45.88820898, pch = 16, col = "red")
points(14.22726928, 45.88602848, pch = 16, col = "green")
# add boundary 
lines(boundary, lwd = 2,lty = 2, col = "blue")

# ---- create sf objects ----- 
# HELP
# I don't know how to preserve the timestamps of the nodes in the multilinestring.
# 





# ------ This is a to and from problem -----  



# create duplicate column for destination 
tracks <- tracks %>% 
  mutate(
    to_ts = timestamp,
    to_lat = location.lat,
    to_lon = location.long,
    from_ts = lag(timestamp),
    from_lat = lag(location.lat),
    from_lon = lag(location.long),
  ) 



rep_path %>%
  mutate(across(lon:llat, as.numeric)) %>% 
  pmap(make_line) %>%
  st_as_sfc(crs = 4326) %>%
  st_sf() %>%  
  
  
  
  
  
  
  
  
  
  tracks_sf <- tracks %>% 
  st_as_sf(coords = c("location.long", "location.lat"), 
           # agr = "identity", # don't use arg and keep as points for now 
           crs = 4326) %>% 
  group_by(tag.local.identifier, timestamp) %>%
  summarise(do_union = FALSE) %>%
  st_cast("LINESTRING") %>% 
  ungroup()


tracks_sf 

# group_by(tag.local.identifier) %>%
#   summarise(do_union = FALSE) %>%
#   st_cast("MULTILINESTRING")
tracks_sf_ls <- tracks %>% 
  st_as_sf(coords = c("location.long", "location.lat"), 
           agr = "identity", 
           crs = 4326) %>% 
  group_by(tag.local.identifier) %>%
  summarise(do_union = FALSE) %>%
  st_cast("MULTILINESTRING")


tracks_sf_ls 


plot(tracks_sf)
ggplot() + 
  geom_sf(data = tracks_sf)
# group_by(tag.local.identifier) %>%
#   summarise(do_union = FALSE) %>%
#   st_cast("MULTILINESTRING")

glimpse(tracks)

boundary_sf <- boundary %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  summarise(do_union = FALSE) %>%
  st_cast("polygon")

mapview(tracks_sf) + mapview(boundary_sf)


tracks_sf <- tracks_sf %>% 
  mutate(
    x_bnd = st_intersects(boundary_sf, tracks_sf, sparse = FALSE)[TRUE]
  )



tracks_sf_ls %>% 
  st_cast("POINT") 
#find the nodes along individual bear tracks that straddle the boundary line.
#in the real world example, the line is not perfectly north/south, so can't cheat with hardcoding the longitude
