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

# create to and from time stamps, lats and lon
tracks_to_from <- tracks %>% 
  mutate(
    location.lat = as.numeric(location.lat),
    location.long = as.numeric(location.long),
    to_ts = timestamp,
    from_ts = dplyr::lag(timestamp),
    llat = location.lat,
    llon = location.long,
    lat = dplyr::lag(location.lat, default = first(location.lat)),
    lon = dplyr::lag(location.long, default = first(location.long)),
  ) 


# for example purposes I selected only the columns I thouhgt would be interest 
# to join to our linestrings of each to and from 
tracks_tf_select <- tracks_to_from %>%
  dplyr::select(tag.local.identifier, from_ts, to_ts, llat:lon) %>% 
  mutate(across(.cols = c(lon, lat, llon, llat), as.character))


# rows 21-24 for whatever reason are duplicate rows...my guess is that
# the animal is detected at the same location 3 times in a row?? 
# can remove if need be using the folllowing below or provide left_join with
# the correct argument
# tracks_to_from <- tracks_to_from[-c(21:24), ]
tracks_sf <- tracks_to_from %>%
  dplyr::select(lon:llat) %>% 
  pmap(make_line) %>%
  st_as_sfc(crs = 4326) %>%
  st_sf() %>% 
  mutate(
    lon = st_startpoint(.) %>%
      st_coordinates(.) %>%
      as_tibble() %>%
      .$X %>% 
      as.character(),
    llon = st_endpoint(.) %>%
      st_coordinates(.) %>%
      as_tibble() %>%
      .$X %>% 
      as.character()
  ) %>% 
  left_join(tracks_tf_select,
            by = c("lon", "llon"), 
            
  )

# ---- view our boundary and tracks ----- 
mapview(tracks_sf) + mapview(boundary_sf)

# ---- add in TRUE/FALSE if linestring intersects our boundary ----- 
tracks_sf <- tracks_sf %>% 
  mutate(
    x_bnd = st_intersects(boundary_sf, tracks_sf, sparse = FALSE)[TRUE]
  )

glimpse(tracks_sf)

tracks_sf_x <- tracks_sf %>% 
  filter(x_bnd %in% TRUE)
 

# ----- plot all tracks that cross boundary ---- 
ggplot() + 
  geom_sf(data = boundary_sf, colour = "blue", linetype = 2) + 
  geom_sf(data = tracks_sf_x)

# we can see all tracks do indeed corss the boundary 

# from here if we look at tracks_sf_x we can see the to and form timestamps 
# the animal crosses from there earlier on timestamps need to be converted into 
# posixct to take difference to know how long it took the animal to move across 
# the boundary. 

tracks_sf_x 
# find the nodes along individual bear tracks that straddle the boundary line.
# in the real world example, the line is not perfectly north/south, so can't cheat with hardcoding the longitude
