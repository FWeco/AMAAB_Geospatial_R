getwd()


# the following command sets the working directory to the folder where this script is located
# use this if you're not creating a project
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # if you get an error, try again, or try instead RStudio menu "Session -> "Set Working Directory -> To Source File Location"
# getwd()


options(scipen = 999) # cancel scientific notation in console/viewing pane (e.g. 1000 instead of 1E04)

# rm(list = ls())

# This script is the local import script for the AMAAB Workshop "Geospatial Analysis in R"
# Matt Shank (@fwEco), March 27, 2025; Berkely Springs, WV


# load packages -----------------------------------------------------------

library(ggspatial)
library(cowplot)
library(geodata)
library(terra)
library(sf)
library(mapview)
# library(htmlwidgets) # will save interactive maps as standalone document
library(tidyverse)
library(tidylog)



# Module 1 - import and clean ---------------------------------------------


#### Geospatial data ####
# we will import geospatial data of various types: point, line, and polygon
# data will be from various sources: github and REST Services (PASDA, PADEP)
# and will be in various formats: .rds, .gpkg, .shp

# first we will import an .shp file of PA counties
pa_co_sf <- 
  st_read('../workshop_files/pa_co.shp') 
pa_co_sf
st_crs(pa_co_sf) # coordinate reference system WGS 84


# we are going to use the crs of this object to transform each subsequent object
crs_wgs84 <- st_crs(pa_co_sf)
crs_wgs84


# were going to focus on the four county area that houses the Allegheny National Forest
# so we can use a filter to select 'Warren', 'McKean', 'Forest', 'Elk'
pa_co_sub_sf <- 
  pa_co_sf %>% 
  filter(name %in% c('Warren', 'McKean', 'Forest', 'Elk'))
pa_co_sub_sf
st_crs(pa_co_sub_sf) # this filtered sf object retains crs of parent: WGS 84


# quick base plot to visualize filter
plot(pa_co_sf$geometry)
plot(pa_co_sub_sf$geometry, col = 'red', add = T)

# where are we in relation to this study area?
# we'll get the coords from google maps and make an sf object

cacapon_sf <- 
  tibble(lat = 39.506481, 
         long = -78.299316,
         lab = 'Cacapon') %>% 
  st_as_sf(coords = c("long", "lat"), remove = FALSE, crs = 4326) %>% # convert to sf
  st_transform(crs_wgs84)
cacapon_sf

plot(cacapon_sf$geometry, add = T, pch = 8, col = 'blue')



# next we'll import watersheds of headwater streams from acidification study (Shank 2023)
# polygon layer in .rds format
wsheds_sf <- readRDS('../workshop_files/wsheds_sf.rds')
wsheds_sf


# quick base plot to visualize 
plot(pa_co_sub_sf$geometry)
plot(wsheds_sf$geometry, col = 'blue', add = T)

# wait, where are the watersheds?

st_crs(wsheds_sf) # in a different crs (Pseudo-Mercator; EPSG 3857)

# lets transform the crs of the watersheds sf to the same as pa counties

wsheds_sf <- 
  wsheds_sf %>% 
  st_transform(crs_wgs84) 
wsheds_sf
st_crs(wsheds_sf) # now in WGS 84

# quick base plot to visualize 
plot(pa_co_sub_sf$geometry)
plot(wsheds_sf$geometry, col = 'blue', add = T)

# much better....




# we have a .gpkg with multiple layers - what is included?

# check available layers from a geopackage
st_layers("../workshop_files/amaab_geospatialR.gpkg")
# geology, nhd, roads


# now bring in bedrock geology polygon layer geopackage (.gpkg)
geology_sf <- st_read(dsn = '../workshop_files/amaab_geospatialR.gpkg', layer='geology_sf') %>% 
  
  # instead of playing crs roulette, lets include this st_transform with every import
  st_transform(crs_wgs84)
geology_sf
st_crs(geology_sf) # now in WGS 84

# quick base plot to visualize filter
plot(pa_co_sub_sf$geometry)
plot(geology_sf$geom, add = T) # notice geometry column named 'geom'



# check available layers from a geopackage
st_layers("../workshop_files/amaab_geospatialR.gpkg")

# now bring in nhd flowlines layer geopackage (.gpkg)
nhd_sf <- st_read(dsn = '../workshop_files/amaab_geospatialR.gpkg', layer='nhd_sf') %>% 
  
  # instead of playing crs roulette, lets include this st_transform with every import
  st_transform(crs_wgs84)
nhd_sf
st_crs(nhd_sf) # now in WGS 84


# nhd_sf has > 13,000 segments, this will take a while to plot, 
# ggplot is a bit faster than base, plus has more control over aesthetics


ggplot() +
  geom_sf(data = pa_co_sub_sf, linewidth = 2, fill = NA) + 
  geom_sf(data = nhd_sf, color = '#2b8cbe') + # from colorbrewer2.org
  
  # add a text label for each county
  geom_label(data = sf::st_point_on_surface(pa_co_sub_sf) %>% # choose a point on the surface of each geometry
               sf::st_coordinates() %>% # retrieve the coordinates
               cbind(as_tibble(pa_co_sub_sf['name'])), # add county name back on
             aes(x = X, y = Y, label = name)) +
  labs(title = 'Rivers and Streams',
       x = 'Longitude', y = 'Latitude') +
  
  theme_minimal() + # one of my favorite themes
  theme(plot.title = element_text(hjust = 0.5))

# noice




# check available layers from a geopackage
st_layers("../workshop_files/amaab_geospatialR.gpkg")

# now bring in roads layer geopackage (.gpkg)
roads_sf <- st_read(dsn = '../workshop_files/amaab_geospatialR.gpkg', layer='roads_sf') %>% 
  
  # instead of playing crs roulette, lets include this st_transform with every import
  st_transform(crs_wgs84)
roads_sf
st_crs(roads_sf) # now in WGS 84

# this has over 25,000 segments

ggplot() +
  geom_sf(data = pa_co_sub_sf, linewidth = 2, fill = NA) + 
  geom_sf(data = roads_sf, color = '#800026') + # from colorbrewer2.org
  
  # add a text label for each county
  geom_label(data = sf::st_point_on_surface(pa_co_sub_sf) %>% # choose a point on the surface of each geometry
               sf::st_coordinates() %>% # retrieve the coordinates
               cbind(as_tibble(pa_co_sub_sf['name'])), # add county name back on
             aes(x = X, y = Y, label = name)) +
  labs(title = 'Roads') +
  
  theme_void() + # no gridlines, axes, labels
  theme(plot.title = element_text(hjust = 0.5))



# Geospatial data from REST Services

# AMD - Point Feature - 
# metadata: https://gis.dep.pa.gov/depgisprd/rest/services/emappa/eMapPA_External_Extraction/MapServer/23
# path to GNET:

# grab it directly from PA DEP REST services
begin <- Sys.time()
amd_sf <- sf::st_read('https://gis.dep.pa.gov/depgisprd/rest/services/emappa/eMapPA_External_Extraction/MapServer/23/query?returnGeometry=true&where=1=1&outFields=*&f=geojson') %>% 
  st_transform(crs_wgs84) 
amd_sf
end <- Sys.time()
end - begin # 44 secs

# quick base plot to visualize filter
plot(pa_co_sf$geometry)
plot(amd_sf$geometry, col = 'orange', add = T)

# this is the whole state, but that's ok, we'll clip to our watersheds later


# or alternatively in rds format
# amd_sf <- readRDS('workshop_files/amd_sf.rds') %>% 
#   st_transform(crs_wgs84)






# Class A streams from PASDA
# navigate to pasda Class A trout streams: https://mapservices.pasda.psu.edu/server/rest/services/pasda/PAFishBoat/MapServer/9

class_a_sf <- sf::st_read('https://mapservices.pasda.psu.edu/server/rest/services/pasda/PAFishBoat/MapServer/9/query?returnGeometry=true&where=1=1&outFields=*&f=geojson') %>% 
  st_transform(crs_wgs84) 
class_a_sf
st_crs(class_a_sf)

# this class A feature seems unstable. sometimes it doesn't successfully query

# quick base plot to visualize filter
plot(pa_co_sf$geometry)
plot(class_a_sf$geometry, add = T, col = 'green')

# this is the whole state, but that's ok, we'll clip to our watersheds later

# or alternatively in rds format
# class_a_sf <- readRDS('workshop_files/class_a_sf.rds') %>% 
#   st_transform(crs_wgs84)



# ANF boundary from PASDA
# navigate to pasda PSU PA National Forests: https://mapservices.pasda.psu.edu/server/rest/services/pasda/PennsylvaniaStateUniversity/MapServer/26

anf_sf <- sf::st_read('https://mapservices.pasda.psu.edu/server/rest/services/pasda/PennsylvaniaStateUniversity/MapServer/26/query?returnGeometry=true&where=1=1&outFields=*&f=geojson') %>% 
  st_transform(crs_wgs84) 
anf_sf
st_crs(anf_sf)

plot(pa_co_sub_sf$geometry)
plot(anf_sf$geometry, add = T, col = '#c7e9c0')
plot(wsheds_sf$geometry, add = T, col = 'blue')

# or alternatively in rds format
# anf_sf <- readRDS('workshop_files/anf_sf.rds') %>% 
#   st_transform(crs_wgs84)
# anf_sf



#### Tabular data ####
# we will also import tabular data in various formats: .csv and .rds

# macroinvertebrate Acid Tolerance Index (ATI) from Shank (2023) acidification study (.rds)
mi_df <- readRDS('../workshop_files/mi_df.rds')
mi_df
summary(mi_df)


# water chemistry data from Shank (2023) acidification study (.csv)
chem_df <- read_csv('../workshop_files/chem_df.csv')
chem_df
summary(chem_df)




# Module 2 - Geospatial Operations -----------------------------------------

# now that we have imported our geospatial and tabular data, we can begin 
# geospatial operations: clip, buffer, dissolve, spatial selection, spatial join


#### spatial clips to watershed ####

# geology (polygon)
geology_clip_sf <- st_intersection(x = wsheds_sf, y = geology_sf) # clip geology to wsheds
geology_clip_sf

# contains a list-column with the indexes of contributing geometries
# basically adds wsheds attributes to overlaps with geology!

plot(geology_clip_sf$geometry[geology_clip_sf$MONITORING_POINT_ALIAS_ID %in% 
                                c('E_HICKORY', 'WILDCAT_RUN', 'ELKHORN_RUN')])




# NHD (lines)
nhd_clip_sf <- st_intersection(x = wsheds_sf, y = nhd_sf) # clip NHD flowlines to wsheds
nhd_clip_sf


plot(wsheds_sf$geometry[wsheds_sf$MONITORING_POINT_ALIAS_ID %in% 
                          c('E_HICKORY', 'WILDCAT_RUN', 'ELKHORN_RUN')])

plot(nhd_clip_sf$geometry[nhd_clip_sf$MONITORING_POINT_ALIAS_ID %in% 
                            c('E_HICKORY', 'WILDCAT_RUN', 'ELKHORN_RUN')], add = T, col = 'blue')


# ggplot makes layer and aesthetic control easier!
ggplot() +
  
  # entire geology sf
  # geom_sf(data = geology_sf, fill = NA, linewidth = 0.5) +
  
  # clipped geology, filtered to 3 wsheds
  geom_sf(data = geology_clip_sf %>% 
            filter(MONITORING_POINT_ALIAS_ID %in% c('E_HICKORY', 'WILDCAT_RUN', 'ELKHORN_RUN')), 
          aes(fill = NAME), linewidth = 0.5) + 
  
  # nhd flowlines
  geom_sf(data = nhd_clip_sf %>% 
            filter(MONITORING_POINT_ALIAS_ID %in% c('E_HICKORY', 'WILDCAT_RUN', 'ELKHORN_RUN')), 
          color = 'blue', linewidth = 1) + 
  
  # wshed boundaries
  geom_sf(data = wsheds_sf %>% 
            filter(MONITORING_POINT_ALIAS_ID %in% c('E_HICKORY', 'WILDCAT_RUN', 'ELKHORN_RUN')), 
          color = '#636363', fill = NA, linewidth = 1.5) + 
  
  theme_minimal() + # one of my favorite themes
  theme(legend.position = 'bottom', legend.direction = "vertical")





# roads (lines)

# first an example of a spatial selection with st_intersects()
# MUCH quicker than a clip -- use for LARGE features

# make a vector of all roads intersecting a watershed
roads_vec <- 
  st_intersects(x = wsheds_sf, y = roads_sf, sparse = T, prepared = TRUE)
roads_vec
length(unlist(roads_vec)) # 639

# use filter to subset the selected road segments
roads_spatial_select_sf <- 
  roads_sf %>% 
  filter(row_number() %in% unlist(roads_vec)) 
roads_spatial_select_sf

plot(wsheds_sf$geometry[wsheds_sf$MONITORING_POINT_ALIAS_ID %in% 
                          c('E_HICKORY', 'WILDCAT_RUN', 'ELKHORN_RUN')])

plot(roads_spatial_select_sf, add = T, col = 'maroon')

# but this does not add wshed attributes to resulting roads sf!
# also does not clip to wshed boundary!
rm(roads_vec, roads_spatial_select_sf)


# so we'll proceed with a clip
# this takes a minute (roads are a large feature)

begin <- Sys.time()
roads_clip_sf <- st_intersection(x = wsheds_sf, y = roads_sf) # clip roads to wsheds
roads_clip_sf
end <- Sys.time()
end - begin # 0.28 secs

plot(wsheds_sf$geometry[wsheds_sf$MONITORING_POINT_ALIAS_ID %in% 
                          c('E_HICKORY', 'WILDCAT_RUN', 'ELKHORN_RUN')])

plot(roads_clip_sf$geometry[roads_clip_sf$MONITORING_POINT_ALIAS_ID %in% 
                              c('E_HICKORY', 'WILDCAT_RUN', 'ELKHORN_RUN')], add = T, col = 'maroon')

# adds MONITORING_POINT_ALIAS_ID attribute to all roads!
# clips to watershed boundary!




# Class A streams (lines)

# remember, when we imported Class A streams, we got the whole PA dataset from PFBC via PASDA
plot(pa_co_sf$geometry)
plot(class_a_sf$geometry, add = T, col = 'green')

# this spatial clip will take care of it

class_a_clip_sf <- st_intersection(x = wsheds_sf, y = class_a_sf) # clip Class A streams to wsheds
class_a_clip_sf


plot(wsheds_sf$geometry[wsheds_sf$MONITORING_POINT_ALIAS_ID %in% 
                          c('WB_HICKS', 'BELL_0.4', 'DENTS_9.2', 'PRCUPINE_0.1')])

plot(class_a_clip_sf$geometry[class_a_clip_sf$MONITORING_POINT_ALIAS_ID %in% 
                                c('WB_HICKS', 'BELL_0.4', 'DENTS_9.2', 'PRCUPINE_0.1')], add = T, col = 'green')




# AMD discharges (points)

# also statewide currently
plot(pa_co_sf$geometry)
plot(amd_sf$geometry, add = T, col = 'orange')


amd_clip_sf <- st_intersection(x = wsheds_sf, y = amd_sf) # clip AMD discharge points to wsheds
amd_clip_sf


plot(wsheds_sf$geometry[wsheds_sf$MONITORING_POINT_ALIAS_ID %in% 
                          c('WB_HICKS', 'BELL_0.4', 'DENTS_9.2', 'PRCUPINE_0.1')])

plot(amd_clip_sf$geometry[amd_clip_sf$MONITORING_POINT_ALIAS_ID %in% 
                            c('WB_HICKS', 'BELL_0.4', 'DENTS_9.2', 'PRCUPINE_0.1')], add = T, col = 'orange')


# Now all of our sf are clipped!
# clean up the environment to save space and limit confusion

# remove all unclipped features
rm(amd_sf, class_a_sf, nhd_sf, roads_sf)



#### spatial join ####

# we don't want to clip the watershed layer, just join an attribute whenever one lies within the ANF
wsheds_anf_sf <- st_join(wsheds_sf, left = TRUE, 
                         select(anf_sf, NAME10)) %>% 
  rename(ANF_position = NAME10) %>% 
  mutate(ANF_position = factor(ifelse(is.na(ANF_position),'outside ANF', 'ANF')))

wsheds_anf_sf

plot(pa_co_sub_sf$geometry)
plot(anf_sf$geometry, add = T, col = '#c7e9c0')
plot(wsheds_sf$geometry, add = T, col = 'blue')
plot(wsheds_anf_sf$geometry, add = T, col = wsheds_anf_sf$ANF_position)






#### buffer and dissolve ####

# put a 200 m buffer around the nhd flowline
st_crs(nhd_clip_sf) # LENGTHUNIT["metre",1]
st_crs(nhd_clip_sf)$proj4string # +units=m
?st_make_valid


begin <- Sys.time()
nhd_buff_sf <- 
  nhd_clip_sf %>% 
  st_buffer(dist = 100) %>% # this applies a 200 m buffer (100m on each side)
  st_union() %>%  # unite to a geometry object (AKA dissolve)
  # st_make_valid() %>% # prevents errors with invalid geometry 
  st_intersection(x = wsheds_sf, y = .) %>% # clip dissolved buffer to wsheds again
  st_sf()
nhd_buff_sf
end <- Sys.time()
end - begin # 0.7 secs

plot(wsheds_sf$geometry[wsheds_sf$MONITORING_POINT_ALIAS_ID %in% 
                          c('E_HICKORY', 'WILDCAT_RUN', 'ELKHORN_RUN')])

plot(nhd_buff_sf$geometry[nhd_buff_sf$MONITORING_POINT_ALIAS_ID %in% 
                            c('E_HICKORY', 'WILDCAT_RUN', 'ELKHORN_RUN')], add = T, col = 'lightblue')

plot(nhd_clip_sf$geometry[nhd_clip_sf$MONITORING_POINT_ALIAS_ID %in% 
                            c('E_HICKORY', 'WILDCAT_RUN', 'ELKHORN_RUN')], add = T, col = 'blue')






#### calculate statistics ####

##### lines #####

# clip roads within 200 m stream buffer
begin <- Sys.time()
roads_buff_int_sf <- st_intersection(x = nhd_buff_sf, y = roads_clip_sf) # clip roads to nhd buffers
end <- Sys.time()
end - begin # 0.22 secs Crickee!
# beepr::beep(8) 
roads_buff_int_sf
st_crs(roads_buff_int_sf)

plot(wsheds_sf$geometry[wsheds_sf$MONITORING_POINT_ALIAS_ID %in% 
                          c('WB_HICKS', 'BELL_0.4', 'DENTS_9.2', 'PRCUPINE_0.1')])
plot(nhd_buff_sf$geometry, add = T, col = 'grey')
plot(nhd_clip_sf$geometry, add = T, col = 'blue')
plot(roads_clip_sf$geometry, add = T, col = 'red')
plot(roads_buff_int_sf$geometry, add = T, col = 'gold')


# calculate road length within each buffer

roadLength_buff_df <-
  roads_buff_int_sf %>% 
  mutate(roadLength = st_length(geometry)) %>% 
  group_by(MONITORING_POINT_ALIAS_ID) %>% 
  summarize(totRoadLength_buff = sum(roadLength)) %>% 
  st_drop_geometry() %>%  # complicates things in joins later on
  mutate(totRoadLength_buff_km = as.numeric(totRoadLength_buff *.001),
         totRoadLength_buff_mi = as.numeric(totRoadLength_buff * 0.000621371)) %>% 
  select(-totRoadLength_buff) # drop meters column
roadLength_buff_df

# tibble is only 25 rows , one watershed has no roads!





# calculate stream length within each wshed

streamLength_df <- 
  nhd_clip_sf %>% 
  mutate(streamLength = st_length(geometry)) %>% 
  group_by(MONITORING_POINT_ALIAS_ID) %>% 
  summarize(totStreamLength = sum(streamLength)) %>% 
  st_drop_geometry() %>%  # complicates things in joins later on
  mutate(totStreamLength_km = as.numeric(totStreamLength *.001),
         totStreamLength_mi = as.numeric(totStreamLength * 0.000621371)) %>% 
  select(-totStreamLength) # drop meters column
streamLength_df


# calculate Class A stream length within each wshed

streamLength_class_a_df <- 
  class_a_clip_sf %>% 
  mutate(classA_length = st_length(geometry)) %>% 
  group_by(MONITORING_POINT_ALIAS_ID) %>% 
  summarize(tot_classA_length = sum(classA_length)) %>% 
  st_drop_geometry() %>%  # complicates things in joins later on
  mutate(tot_classA_length_km = as.numeric(tot_classA_length *.001),
         tot_classA_length_mi = as.numeric(tot_classA_length * 0.000621371)) %>% 
  select(-tot_classA_length) # drop meters column
streamLength_class_a_df

# only 8 watersheds contain class A streams




##### polygons #####

# df of drainage area from wsheds_sf

da_df <- 
  wsheds_sf %>% 
  mutate(da_m2 = st_area(geometry),
         da_km2 = as.numeric(da_m2 * 0.000001), # convert to sq km
         da_mi2 = as.numeric(da_m2 * 0.000000386102)) %>%  # convert to sq mi
  st_drop_geometry() %>%  # drop geometry col
  select(-da_m2) %>% 
  as_tibble()
da_df # we will join this dataframe to subsequent point/polygon summaries to express stats as %/density



# geology df

# first calculate area of each formation in each wshed
geo_NAME_df <-
  geology_clip_sf %>% 
  select(MONITORING_POINT_ALIAS_ID, NAME) %>% # grouping vars important
  mutate(geo_area_m2 = st_area(geometry),
         geo_area_km2 = as.numeric(geo_area_m2 * 0.000001),
         geo_area_mi2 = as.numeric(geo_area_m2 * 0.000000386102)) %>% 
  group_by(MONITORING_POINT_ALIAS_ID, NAME) %>% 
  summarize(geo_area_km2 = sum(geo_area_km2),
            geo_area_mi2 = sum(geo_area_mi2)) %>% 
  st_drop_geometry() %>% 
  left_join(da_df, by = "MONITORING_POINT_ALIAS_ID") %>% 
  mutate(perc_geo_NAME = as.numeric(round(geo_area_km2 / da_km2 * 100 ,1)),
         perc_check = as.numeric(sum(perc_geo_NAME, na.rm = T))) 
geo_NAME_df
summary(geo_NAME_df) # perc_check col looks good!



# vector of base cation poor geology formations!
base_poor_geo_vec <- c('Allegheny and Pottsville Formations, undivided',
                       'Antietam and Harpers Formations, undivided',
                       'Burgoon Sandstone',
                       'Burgoon Sandstone through Cuyahoga Group, undifferentiated',
                       'Harpers Formation',
                       # 'Huntley Mountain Formation',
                       'Montalto Member of Harpers Formation',
                       'Pocono Formation',
                       'Pottsville Formation',
                       'Tuscarora Formation',
                       'Antietam Formation', 
                       'Chickies Formation', 
                       'Weverton and Loudoun Formations, undivided', 
                       'Hardyston Formation')
base_poor_geo_vec

prob_geo_df <-
  geo_NAME_df %>% 
  filter(NAME %in% base_poor_geo_vec) %>% 
  summarize(perc_prob_geo = sum(perc_geo_NAME)) 
prob_geo_df
summary(prob_geo_df) # % base-poor geology ranged from 6 - 92% in our wsheds




##### points #####

# df with number of AMD discharges and density

amd_df <- 
  amd_clip_sf %>% 
  as_tibble() %>% 
  group_by(MONITORING_POINT_ALIAS_ID) %>% 
  summarize(n_amd_point = n()) %>% 
  left_join(da_df, by = "MONITORING_POINT_ALIAS_ID") %>% 
  mutate(amd_point_dens_mi2 = n_amd_point / da_mi2)

# add da df to normalize into density /km2 & / mi2

amd_df



# create master table of all summarized attributes for analysis!

all_att_df <-
  da_df %>% # start with da_df because there are observations of DA for every wshed!
  
  # join polygon dfs
  left_join(prob_geo_df, by = "MONITORING_POINT_ALIAS_ID") %>% # % base-poor geo
  
  # join line dfs
  left_join(streamLength_df, by = "MONITORING_POINT_ALIAS_ID") %>% # total stream length
  left_join(roadLength_buff_df, by = "MONITORING_POINT_ALIAS_ID") %>% # road length within buffers
  left_join(streamLength_class_a_df, by = "MONITORING_POINT_ALIAS_ID") %>% # class A stream length
  
  # drop metric cols, were going SU route
  select(-contains("km")) %>%  # slick!
  
  mutate(
    totRoadLength_buff_mi = replace_na(totRoadLength_buff_mi, 0), # replace NAs with 0 before calcs
    tot_classA_length_mi = replace_na(tot_classA_length_mi, 0), # replace NAs with 0 before calcs
    
    buff_road_stream_ratio = totRoadLength_buff_mi / totStreamLength_mi, # calculate ratio of roads:stream length
    class_a_stream_ratio = tot_classA_length_mi / totStreamLength_mi) %>%  # calculate ratio of class A streams:stream length
  
  # join point dfs
  left_join(amd_df[c('MONITORING_POINT_ALIAS_ID', 'amd_point_dens_mi2')], by = "MONITORING_POINT_ALIAS_ID") %>%  # amd point density
  
  mutate(amd_point_dens_mi2 = replace_na(amd_point_dens_mi2, 0)) # replace NAs with 0 before calcs
all_att_df
summary(all_att_df)
# phew!

names(all_att_df)

# now we can look at the association with these variables and chemical/biological variables

# chemistry (site median pH)
# pipe straight into a plot

all_att_df %>% 
  select(-c(totStreamLength_mi, totRoadLength_buff_mi, tot_classA_length_mi)) %>% 
  left_join(chem_df, by = "MONITORING_POINT_ALIAS_ID") %>% 
  
  # ggplot like long format
  pivot_longer(cols = c("da_mi2", "perc_prob_geo", "buff_road_stream_ratio", 
                        "class_a_stream_ratio", "amd_point_dens_mi2"),
               names_to = 'var', values_to = 'value') %>% 
  
  ggplot(aes(x = value, y = pH)) +
  facet_wrap(~var, scales = 'free_x') +
  geom_hline(yintercept = 6, lty = 'dashed') +
  geom_point(aes(fill = var), pch = 21) +
  geom_smooth(aes(color = var), method = 'lm', se = F) +
  theme(legend.position = 'none')




# biology (Acid Tolerance Index [ATI] Score)
# pipe straight into a plot

all_att_df %>% 
  select(-c(totStreamLength_mi, totRoadLength_buff_mi, tot_classA_length_mi)) %>% 
  left_join(mi_df[c('MONITORING_POINT_ALIAS_ID', 'ATI')], by = "MONITORING_POINT_ALIAS_ID") %>% 
  
  # ggplot like long format
  pivot_longer(cols = c("da_mi2", "perc_prob_geo", "buff_road_stream_ratio", 
                        "class_a_stream_ratio", "amd_point_dens_mi2"),
               names_to = 'var', values_to = 'value') %>% 
  
  ggplot(aes(x = value, y = ATI)) +
  facet_wrap(~var, scales = 'free_x') +
  geom_hline(yintercept = 53, lty = 'dashed') +
  geom_point(aes(fill = var), pch = 21) +
  geom_smooth(aes(color = var), method = 'lm', se = F) +
  theme(legend.position = 'none')




# Module 3 - Exporting spatial objects ------------------------------------

# The shortest module by far!


# Up until now all geospatil operations have been saved to our R environment!
# If you want to export features after geospatial operations, you've got options.


# create a folder for the output files of this course:
dir.create("output", showWarnings = FALSE)  # creates an 'ouput' folder in your project directory


# My favorite formats are .rds, .rda, and .gpkg
# .shp might be the most common, but she's got issues

# .rds is my favorite! The crs is stored, as is any variable groupings are column formats!
# say you save a numeric column with leadings zeroes (e.g. HUC) as a character, it stays a character! 
# try that in GIS or excel
# .rds files also have a badass compression - example, of the two formats that we save the 
# geology sf in below, the .shp is 4 separate files that total ~ 6.3 GB
# the .rds is a single file is < 4.9 GB (20% smaller - this adds up)

# export to .shp
st_write(geology_clip_sf, 'output/workshop_geology.shp')

# export to .rds
saveRDS(geology_clip_sf, 'output/workshop_geology.rds')

# or you can save combinations of output as and rda (my second favorite)
# think of this as a platter of .rds files that you can load with one line of code
save(wsheds_sf, pa_co_sf, geology_clip_sf, nhd_clip_sf, roads_clip_sf, 
     amd_clip_sf, class_a_clip_sf, 
     file = "output/workshop_geospatial.rda")

# this save as a single rda file
# can be loaded into other R projects with:
# load('output/workshop_geospatial.rda') # loads all individual objects into your environment!


# or you can combine all spatial features into a single geopackage (.gpkg)
# .gpkg is open source - can be read into ArcGIS, QGIS, R, etc...

sf::st_write(wsheds_sf,
             dsn = "output/amaab_geospatialR.gpkg",
             layer = 'wsheds_sf')

sf::st_write(pa_co_sf,
             dsn = "output/amaab_geospatialR.gpkg",
             layer = 'pa_co_sf')

sf::st_write(geology_clip_sf,
             dsn = "output/amaab_geospatialR.gpkg",
             layer = 'geology_clip_sf')

sf::st_write(nhd_clip_sf,
             dsn = "output/amaab_geospatialR.gpkg",
             layer = 'nhd_clip_sf')

# ....
# can even get into geodatabase management with:
# sf::st_write(..., append = F) # if append = TRUE append, if append = FALSE replace
# sf::st_delete() # to remove layers from gpkg
# see ?sf::st_write()

# view all layers in .gpkg
st_layers("output/amaab_geospatialR.gpkg")



# tired of importing a spreadsheet/csv into GIS and displaying xy coordinates? 
# Do it in R! and export as shapefile, read directly into GIS

mi_sf <- 
  mi_df %>%  
  st_as_sf(coords = c("long_dd", "lat_dd"), crs = 4326) # set to WGS 84 crs
mi_sf

# export as shapefile
st_write(mi_sf, 'output/workshop_mi.shp')

# or 

# export as layer in gpkg
sf::st_write(mi_sf,
             dsn = "output/amaab_geospatialR.gpkg",
             layer = 'mi_sf')

st_layers("output/amaab_geospatialR.gpkg") # mi_sf now included




# Module 4 - Map creation -------------------------------------------------

#### Static maps ####

##### base plots (simple) ######

# we did this a bunch above, first feature with plot(...), second with plot(..., add = T)

plot(wsheds_sf$geometry[wsheds_sf$MONITORING_POINT_ALIAS_ID %in% 
                          c('WB_HICKS', 'BELL_0.4', 'DENTS_9.2', 'PRCUPINE_0.1')])
plot(nhd_buff_sf$geometry, add = T, col = 'grey')
plot(nhd_clip_sf$geometry, add = T, col = 'blue')
plot(roads_clip_sf$geometry, add = T, col = 'red')
plot(roads_buff_int_sf$geometry, add = T, col = 'gold')


# WHAT


#### hillshade! ####
# this is a bit of a tangent but very cool 
# can grab an elevation raster for the centroid of the ANF and plot raster data

# raster! - switching to terra package (SpatRaster format)

anf_sf %>% st_transform(4326) # gives bounding box info

# set bounding box
bbox_anf <- c(-79.49392, -78.64515, 41.32496 , 41.99908)  # xmin, xmax, ymin, ymax
bbox_anf

# pull centroid of ANF
anf_sf %>% st_transform(4326) %>% 
  st_centroid(.$geometry)


# get 3 second elevation data for ANF from the geodata package
elev_anf_3s <- geodata::elevation_3s(lon = -79.02172, lat = 41.65201, path = "output", mask = FALSE)
# need to convert crs
elev_anf_3s
terra::crs(elev_anf_3s)

# overall extent
terra::plot(elev_anf_3s)
terra::plot(anf_sf$geometry %>% st_transform(4326), add = T, border = 'grey80', lwd = 2)
# zoomed way out

# use bounding box (bbox_anf) to set extent
terra::plot(elev_anf_3s, ext = bbox_anf, main = 'Elevation', col = hcl.colors(100, palette = "inferno"))
terra::plot(anf_sf$geometry %>% st_transform(4326), add = T, border = 'grey80', lwd = 2)



# extract slope 
slope_anf <- terra::terrain(elev_anf_3s, 'slope', unit = "radians")
terra::plot(slope_anf, ext = bbox_anf, main = 'Slope')
terra::plot(anf_sf$geometry %>% st_transform(4326), add = T, border = 'grey80', lwd = 2)



# extract aspect
aspect_anf <- terra::terrain(elev_anf_3s, v = "aspect", unit = "radians")
terra::plot(aspect_anf, ext = bbox_anf, main = 'Aspect')
terra::plot(anf_sf$geometry %>% st_transform(4326), add = T, border = 'red', lwd = 2)


# hillshade map - combines slope and aspect
shade_anf <- terra::shade(slope_anf, aspect_anf, angle = 20, direction = 0)
terra::plot(shade_anf, col = grey(0:100/100), ext = bbox_anf) # , legend = FALSE #
terra::plot(anf_sf$geometry %>% st_transform(4326), add = T, border = 'black', lwd = 2)


# plot roughness
terra::plot(terra::terrain(elev_anf_3s, 'roughness'), ext = bbox_anf, main = 'Roughness')
terra::plot(anf_sf$geometry %>% st_transform(4326), add = T, border = 'grey80', lwd = 2)





##### presentation quality - ggplot2 #####

# ggplot2 is more verbose, but allows much more control over aesthics (e.g. fills, colors)
# and other bells and whistles like legends, insets, scale bars, etc...



# were going to make 2 maps, the first will be used as a small inset, the second
# is a detailed map, zoomed into a few watersheds

# we will transform the crs of everything to EPSG 4326 (WGS 84) since that is what the
# basemaps from ggmap use (roads will overlay perfectly)

# inset map
inset_map <-
  ggplot() +
  
  # county boundary
  geom_sf(data = st_transform(pa_co_sf, 4326), fill = 'white', inherit.aes = FALSE,
          color = '#969696', linewidth = 0.5) +
  
  # wsheds
  geom_sf(data = wsheds_sf %>% 
            filter(MONITORING_POINT_ALIAS_ID %in% c('WB_HICKS', 'BELL_0.4', 'DENTS_9.2', 'PRCUPINE_0.1')) %>% 
            st_transform(4326), 
          color = '#000000', fill = '#000000', inherit.aes = FALSE, linewidth = 1) +
  
  theme_void()
inset_map


# the second requires more detail


# basemaps
# explore get_stadiamap - need an API key https://github.com/dkahle/ggmap/pull/351
# or can use ggmap::get_map() if you register with google and get and API key: https://mapsplatform.google.com/
# we'll use ggspatial::annotation_map_tile()
# types of map tiles include 'cartolight', 'osm', 'stamenwatercolor'


wshed_map <-
  ggplot() +
  
  ggspatial::annotation_map_tile("osm", zoom = 12) +
  
  geom_sf(data = geology_clip_sf %>% 
            filter(NAME %in% base_poor_geo_vec & 
                     MONITORING_POINT_ALIAS_ID %in% c('WB_HICKS', 'BELL_0.4', 'DENTS_9.2', 'PRCUPINE_0.1')) %>%  
            st_transform(4326), 
          aes(fill = NAME), alpha = 0.5, color = NA, inherit.aes = FALSE, show.legend = T) +
  
  geom_sf(data = amd_clip_sf  %>% filter(MONITORING_POINT_ALIAS_ID %in% c('WB_HICKS', 'BELL_0.4', 'DENTS_9.2', 'PRCUPINE_0.1')) %>% 
            st_transform(4326), 
          fill = '#f03b20', alpha = 0.5, inherit.aes = F, show.legend = F, pch = 21) +
  
  geom_sf(data = nhd_clip_sf  %>% filter(MONITORING_POINT_ALIAS_ID %in% c('WB_HICKS', 'BELL_0.4', 'DENTS_9.2', 'PRCUPINE_0.1')) %>% 
            st_transform(4326), 
          color = '#3182bd', linewidth = 1, show.legend = F, inherit.aes = FALSE) + 
  
  geom_sf(data = roads_clip_sf  %>% filter(MONITORING_POINT_ALIAS_ID %in% c('WB_HICKS', 'BELL_0.4', 'DENTS_9.2', 'PRCUPINE_0.1')) %>% 
            st_transform(4326), #
          color = '#a50f15', size = 0.5, show.legend = F, inherit.aes = FALSE) +
  
  geom_sf(data = wsheds_sf %>% filter(MONITORING_POINT_ALIAS_ID %in% c('WB_HICKS', 'BELL_0.4', 'DENTS_9.2', 'PRCUPINE_0.1')) %>% 
            st_transform(4326), 
          color ='#525252', linewidth = 2, fill = NA, show.legend = F, inherit.aes = FALSE) +
  
  geom_sf(data = wsheds_sf %>% filter(MONITORING_POINT_ALIAS_ID %in% c('WB_HICKS', 'BELL_0.4', 'DENTS_9.2', 'PRCUPINE_0.1')) %>% 
            st_transform(4326), 
          color ='#f0f0f0', linewidth = 0.5, fill = NA, show.legend = F, inherit.aes = FALSE) +
  
  geom_sf(data = mi_df %>% distinct(MONITORING_POINT_ALIAS_ID, lat_dd, long_dd) %>% 
            filter(MONITORING_POINT_ALIAS_ID %in% c('WB_HICKS', 'BELL_0.4', 'DENTS_9.2', 'PRCUPINE_0.1')) %>% 
            st_as_sf(coords = c("long_dd", "lat_dd"), crs = 4326), 
          size = 3, inherit.aes = FALSE) +
  
  geom_sf_label(data = mi_df %>% distinct(MONITORING_POINT_ALIAS_ID, lat_dd, long_dd) %>% 
                  filter(MONITORING_POINT_ALIAS_ID %in% c('WB_HICKS', 'BELL_0.4', 'DENTS_9.2', 'PRCUPINE_0.1')) %>% 
                  st_as_sf(coords = c("long_dd", "lat_dd"), crs = 4326),
                aes(label = MONITORING_POINT_ALIAS_ID), 
                nudge_x = 1100, # move label 1100 m east to avoid overplotting - will have to change depending on scale of map
                inherit.aes = FALSE, fill ='#d9d9d9', alpha = 0.5,
                size = 2) +
  
  # Add scale and North arrow
  ggspatial::annotation_scale(location = "bl", bar_cols = c("grey60", "white")) + 
  
  ggspatial::annotation_north_arrow(
    location = "bl", which_north = "true", pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(fill = c("grey40", "white"), line_col = "grey20")) +
  
  scale_fill_manual(name = 'Base Poor Geology', values = c('#FF7F00')) +
  
  theme_void() +
  theme(legend.position = 'bottom', plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size = 6), legend.title = element_text(size = 8))

wshed_map

cowplot::ggdraw() +
  cowplot::draw_plot(wshed_map) +
  cowplot::draw_plot(inset_map, x = 0.7, y = 0.8, width = 0.2, height = 0.2)

ggsave(file = 'output/pres_quality_wshed_map.png', scale=1, units='in', dpi=300, width=6, height=6, bg = 'white')



# OK!



#### interactive maps ####

# Acid Tolerance Index (ATI) scores - make tabular data sf with st_as_sf()
mi_sf <- 
  mi_df %>% 
  sf::st_as_sf(coords = c('long_dd', 'lat_dd'), crs = 4269) %>%  # saved in NAD 83
  sf::st_transform(4326)
mi_sf



# really simple, just ATI scores in 25 point bins
mapview::mapview(mi_sf, zcol = "ATI", at = seq(0, 100, 25), legend = TRUE)

# interactive map with ATI, ANF boundary, wsheds, streams 
# additional layer controls: viridis color palettes (continuous), layer.name, legend control, 
ati_map <- 
  
  # ATI scores (points)
  mapview::mapview(mi_sf, 
                   zcol = "ATI", legend = TRUE, layer.name = 'ATI',
                   col.regions = viridis::viridis_pal(option = "magma", direction = -1)) +
  
  # ANF boundary (polygon)
  mapview::mapview(anf_sf %>% st_transform(4326), 
                   legend = F, col.regions = '#a1d99b', layer.name = 'ANF') +
  
  # wsheds (polygon) - symbolized by % base poor geology - with all attributes joined
  mapview::mapview(wsheds_sf %>% 
                     left_join(all_att_df, by = 'MONITORING_POINT_ALIAS_ID') %>% 
                     st_transform(4326), 
                   zcol = "perc_prob_geo", layer.name = '% base-poor geology',
                   legend = T, col.regions = viridis::viridis_pal(option = "magma", direction = -1)) +
  
  # Streams (lines)
  mapview::mapview(nhd_clip_sf %>% select(MONITORING_POINT_ALIAS_ID, GNIS_ID, GNIS_NAME, REACHCODE, COMID) %>% 
                     st_transform(4326), 
                   zcol = 'GNIS_NAME', legend = F, color = '#3182bd', layer.name = 'Streams')

ati_map


# YEEAAHH!

# htmlwidgets::saveWidget(ati_map@map, # object needs to be followed by @map to use saveWidget
#                         file = paste0('output/ati_map_', Sys.Date(), ".html"),
#                         selfcontained = TRUE)



# For detailed training on interactive maps with leaflet, see the 
# 2021 PA AFS workshop 'Interactive Mapping in R: Don't get caught up in the static
# https://rpubs.com/mattshank20/intMappingR


# For detailed training on accessing/querying data from REST services, see
# https://rpubs.com/mattshank20/pasda_api



