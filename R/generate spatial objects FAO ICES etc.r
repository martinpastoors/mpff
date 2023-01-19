# ==================================================================
# generate spatial objects.r
# 
# Martin Pastoors
#
# 11/01/2023 First (new) coding
# ==================================================================
# 
# 1. World maps
# 2. FAO areas
# 3. ICES area (subset of FAO areas)
# 4. ICES rectangles
# 
#
# ==================================================================
# Reset lists
# rm(list=ls())


library(tidyverse)     # Tidyverse ofcourse
library(sf)            # Support for simple features, a standardized way to encode spatial vector data.
# library(mregions)      # Marine Regions package

onedrive <- "C:/DATA"

# Functions ----------------------------------------------------------------------

create_sf <- function(path, folder, layer, simplify=NA) {
  sf <-
    sf::st_read(dsn   = file.path(path, folder), layer = layer) %>% 
    sf::st_transform(crs = 4326) %>%
    sf::st_make_valid() %>% 
    { if(is.numeric(simplify)) {rmapshaper::ms_simplify(., keep = simplify, keep_shapes = TRUE) } else {.}}
  
  return(sf)
}

create_spatial <- function(path, folder, layer, simplify=NA) {
  sp  <-
    rgdal::readOGR(dsn=file.path(path, folder), layer=layer) %>%
    sp::spTransform(sp::CRS("+init=epsg:4326")) %>%  
    { if(is.numeric(simplify)) {rmapshaper::ms_simplify(., keep = simplify, keep_shapes = TRUE) } else {.}}
  
  return(sp)
}

create_spatial_df <- function(path, folder, layer, simplify=NA) {
  
  sp         <- create_spatial(path=path, folder=folder, layer=layer, simplify=simplify) 
  sp@data$id <- rownames(sp@data)
  sp.points  <- broom::tidy(sp)
  
  df  <- left_join(sp.points, sp@data, by="id")
  
  return(df)
}

create_spatial_df_from_sp <- function(sp) {
  
  if(class(sp) != "SpatialPolygonsDataFrame") stop("sp must be a SpatialPolygonsDataFrame")
  
  sp@data$id <- rownames(sp@data)
  sp.points  <- broom::tidy(sp)
  
  df  <- left_join(sp.points, sp@data, by="id")
  
  return(df)
}

# ------------------------------------------------------------------------------
# 1. World
# ------------------------------------------------------------------------------

# install.packages("rnaturalearth")
# install.packages("rnaturalearthdata")
# install.packages("rnaturalearthhires",
#                  repos = "http://packages.ropensci.org",
#                  type = "source")


# Low resolutions

# spatial object
world_lr <-
  rnaturalearth::ne_countries (returnclass = "sp", scale="small") %>% 
  sp::spTransform(., sp::CRS("+init=epsg:4326")) %>% 
  rgeos::gBuffer(., byid=TRUE, width=0) 

# sf object
world_lr_sf <- 
  rnaturalearth::ne_countries (returnclass = "sf", scale="small") %>% 
  sf::st_transform(crs = 4326) 

# df object
world_lr_df <- create_spatial_df_from_sp(world_lr)

# Medium resolutions

# spatial object
world_mr <-
  rnaturalearth::ne_countries (returnclass = "sp", scale="medium") %>% 
  sp::spTransform(., sp::CRS("+init=epsg:4326")) %>% 
  rgeos::gBuffer(., byid=TRUE, width=0) 

# sf object
world_mr_sf <- 
  rnaturalearth::ne_countries (returnclass = "sf", scale="medium") %>% 
  sf::st_transform(crs = 4326) 

# df object (from sp object)
world_mr_df <- create_spatial_df_from_sp(world_mr)

# High resolutions

# spatial object
world_hr <-
  rnaturalearth::ne_countries (returnclass = "sp", scale="large") %>% 
  sp::spTransform(., sp::CRS("+init=epsg:4326")) %>% 
  rgeos::gBuffer(., byid=TRUE, width=0) 

# sf object
world_hr_sf <- 
  rnaturalearth::ne_countries (returnclass = "sf", scale="large") %>% 
  sf::st_transform(crs = 4326) 

# df object (from sp object)
world_hr_df <- create_spatial_df_from_sp(world_hr)

# Save
save(world_lr   , file=paste(onedrive, "RDATA/world_lr.RData", sep="/"))
save(world_lr_df, file=paste(onedrive, "RDATA/world_lr_df.RData", sep="/"))
save(world_lr_sf, file=paste(onedrive, "RDATA/world_lr_sf.RData", sep="/"))

save(world_mr   , file=paste(onedrive, "RDATA/world_mr.RData", sep="/"))
save(world_mr_df, file=paste(onedrive, "RDATA/world_mr_df.RData", sep="/"))
save(world_mr_sf, file=paste(onedrive, "RDATA/world_mr_sf.RData", sep="/"))

save(world_hr   , file=paste(onedrive, "RDATA/world_hr.RData", sep="/"))
save(world_hr_df, file=paste(onedrive, "RDATA/world_hr_df.RData", sep="/"))
save(world_hr_sf, file=paste(onedrive, "RDATA/world_hr_sf.RData", sep="/"))

# ------------------------------------------------------------------------------
# 2. FAO areas
# ------------------------------------------------------------------------------

path   <- file.path("C:/DATA/GIS") 
folder <- "FAO_AREAS"
layer  <- 'FAO_AREAS'

fao_sf <- create_sf(path=path, folder=folder, layer=layer, simplify=NA)
fao_df <- create_spatial_df(path=path, folder=folder, layer=layer, simplify=NA)
fao    <- create_spatial(path=path, folder=folder, layer=layer, simplify=NA)

save(fao   , file=paste(onedrive, "RDATA/fao.RData", sep="/"))
save(fao_df, file=paste(onedrive, "RDATA/fao_df.RData", sep="/"))
save(fao_sf, file=paste(onedrive, "RDATA/fao_sf.RData", sep="/"))

# ------------------------------------------------------------------------------
# 3. ICES areas (subset of FAO areas)
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# 4. ICES rectangles
# ------------------------------------------------------------------------------

path   <- file.path("C:/DATA/GIS") 
folder <- "ices_rectangles"
layer  <- "ices_rectangles"

rect_sf <- create_sf(path=path, folder=folder, layer=layer, simplify=NA)
rect    <- create_spatial(path=path, folder=folder, layer=layer, simplify=NA)
rect_df <- create_spatial_df_from_sp(rect)

rect_lr_sf <- create_sf(path=path, folder=folder, layer=layer, simplify=0.05)

save(rect   , file=paste(onedrive, "RDATA/rect.RData", sep="/"))
save(rect_df, file=paste(onedrive, "RDATA/rect_df.RData", sep="/"))
save(rect_sf, file=paste(onedrive, "RDATA/rect_sf.RData", sep="/"))

save(rect_lr_sf, file=paste(onedrive, "RDATA/rect_lr_sf.RData", sep="/"))







# Add required libraries
library(maps)          # world maps
library(rworldmap)     # getmap; world map with possibility of adjusting resolution

library(rgdal   )      # read spatial polygons (e.g. EEZ)
library(maptools)      # manipulating spatial data

# library(geo)         # getting rectangles etc.; no longer available
library(geosphere)     # spatial manipulation

library(sp)            # spatial manipulation
library(rgeos)         # spatial manipulation e.g. gdifference
library(PBSmapping)    # for cutting polygons
library(rmapshaper)    # simplify polygons
library(broom)         # for tidy; replacement of fortify
# library(mapdata)

library(readxl)
library(units)         # for changing units of r vectors

# install.packages("mregions")
# library(mregions)

source("r/my utils.r")

# set onedrive directory
onedrive <- get_onedrive()


# t <- mregions::mr_layers()
# for (i in 1:length(t)) {print(t[[i]]$Name)}
# 
# shp <- mregions::mr_shp(key = "MarineRegions:eez_iho", maxFeatures = 5)
# shp <- mregions::mr_shp(key = "MarineRegions:eez_iho_union_v2", maxFeatures = 600)
# shp.sf <- sf::st_as_sf(shp)
# plot(shp.sf$mrgid)




create_spatial <- function(path, folder, layer, simplify=NA) {
  sp  <-
    rgdal::readOGR(dsn=file.path(path, folder), layer=layer) %>%
    sp::spTransform(CRS("+init=epsg:4326")) %>%  
    { if(is.numeric(simplify)) {rmapshaper::ms_simplify(., keep = simplify, keep_shapes = TRUE) } else {.}}
  
  return(sp)
}

create_spatial_df <- function(path, folder, layer, simplify=NA) {
  
  sp         <- create_spatial(path=path, folder=folder, layer=layer, simplify=simplify) 
  sp@data$id <- rownames(sp@data)
  sp.points  <- broom::tidy(sp)
  
  df  <- left_join(sp.points, sp@data, by="id")
  
  return(df)
}


# ================================================================================
# World map (high resolution)
# ================================================================================


# ================================================================================
# read ICES areas, shapes downloaded from ICES website and convert to df and sf objects
# then manually add some areas. 
# The ICES areas are generated from the lowest specification upwards
# Subdivisions for 6a North and 6a South have been added
# The final sf and df object are structured as an FAO sf object for later merging
# Create both a lowres and a hires version
# ================================================================================

path   <- file.path(onedrive, "gis") 
folder <- "ICES_areas_20160601"
layer  <- 'ICES_Areas_20160601_cut_dense_3857'

ices    <- create_spatial(path=path, folder=folder, layer=layer, simplify=0.1)
ices.sf <- create_sf(path=path, folder=folder, layer=layer, simplify=0.1)
ices.df <- create_df(path=path, folder=folder, layer=layer, simplify=0.1)

# spatial object
ices <- 
  st_read(dsn=file.path(onedrive, "gis/ICES_areas_20160601/ICES_Areas_20160601_cut_dense_3857.shp")) %>% 
  st_transform(crs = 4326) %>% 
  st_simplify(preserveTopology = FALSE, dTolerance = 0.01) %>% 
  rename(
    F_AREA     = Major_FA,
    F_SUBAREA  = SubArea,
    F_DIVISION = Division,
    F_SUBDIVIS = SubDivisio,
    F_SUBUNIT  = Unit, 
    F_CODE     = Area_Full
  ) %>% 
  sf::st_as_sf(ices) 
  

# generate sf object and prepare in similar format as FAO dataset
ices.sf   <-
  ices %>% 
  # sf::st_as_sf(ices) %>%
  mutate(
    F_SUBAREA    = ifelse(!is.na(F_SUBAREA)   , paste(F_AREA,F_SUBAREA,sep="."), F_SUBAREA),
    F_DIVISION   = ifelse(!is.na(F_DIVISION)  , paste(F_SUBAREA,F_DIVISION,sep="."), F_DIVISION),
    F_SUBDIVIS   = ifelse(!is.na(F_SUBDIVIS)  , paste(F_DIVISION,F_SUBDIVIS, sep="."), F_SUBDIVIS),
    F_SUBUNIT    = ifelse(!is.na(F_SUBUNIT)   , paste(F_SUBDIVIS,F_SUBUNIT,sep="."), F_SUBUNIT),
    F_STATUS     = 1,
    OCEAN        = "Atlantic",
    SUBOCEAN     = "2",
    F_LEVEL      =   case_when(
        !is.na(F_SUBUNIT)  ~ "SUBUNIT",
        !is.na(F_SUBDIVIS) ~ "SUBDIVISION",
        !is.na(F_DIVISION) ~ "DIVISION",
        !is.na(F_SUBAREA)  ~ "SUBAREA",
        !is.na(F_AREA)     ~ "MAJOR",
        TRUE               ~  "other"
        )
  ) %>%
 dplyr::select(-Area_27, -Area_km2, -OBJECTID, -OBJECTID_1)

# ices.sf %>% filter(F_CODE == "27.3.d.27") %>% st_area() %>% set_units(., km^2) %>% as.numeric()

# save(ices      , file=paste(onedrive, "rdata/ices.tmp.RData", sep="/"))
# save(ices.sf   , file=paste(onedrive, "rdata/ices.sf.tmp.RData", sep="/"))

# load(file=paste(onedrive, "rdata/ices.tmp.RData", sep="/"))
# load(file=paste(onedrive, "rdata/ices.sf.tmp.RData", sep="/"))

# create subdivisions 27.4.a.E and 27.4.a.W
bb4aeast <- data.frame(x = c(2 ,  2, 12, 12,  2), 
                       y = c(57.5, 62, 62, 57.5,  57.5)) %>% 
  sf::st_as_sf(coords = c("x","y")) %>% 
  sf::st_set_crs(4326) %>% 
  group_by() %>% 
  summarize(do_union=FALSE) %>% 
  st_cast("POLYGON")

# 4a east
t4ae <-
  st_intersection(ices.sf, bb4aeast) %>%
  filter(grepl("27.4.a", F_CODE)) %>%
  mutate(
    F_SUBDIVIS = "27.4.a.E",
    F_CODE     = "27.4.a.E",
    F_LEVEL    = "SUBDIVISION"
  ) %>%
  st_sf()

# 4a west
t4aw <-
  st_difference(ices.sf, bb4aeast) %>%
  filter(grepl("27.4.a", F_CODE)) %>%
  mutate(
    F_SUBDIVIS = "27.4.a.W",
    F_CODE     = "27.4.a.W",
    F_LEVEL    = "SUBDIVISION"
  ) %>%
  st_sf()


# create subdivisions 27.6.a.N and 27.6.a.S
bb6asouth <- data.frame(x = c(-7  , -7, -12, -12,  -7  ), 
                        y = c(54  , 56,  56,  54,  54)) %>% 
  sf::st_as_sf(coords = c("x","y")) %>% 
  sf::st_set_crs(4326) %>% 
  group_by() %>% 
  summarize(do_union=FALSE) %>% 
  st_cast("POLYGON")

t6as <-
  st_intersection(ices.sf, bb6asouth) %>%
  filter(grepl("27.6.a", F_CODE)) %>%
  mutate(
    F_SUBDIVIS = "27.6.a.S",
    F_CODE     = "27.6.a.S",
    F_LEVEL    = "SUBDIVISION"
  ) %>%
  st_sf()

# 6a north
t6an <-
  st_difference(ices.sf, bb6asouth) %>%
  filter(grepl("27.6.a", F_CODE)) %>%
  mutate(
    F_SUBDIVIS = "27.6.a.N",
    F_CODE     = "27.6.a.N",
    F_LEVEL    = "SUBDIVISION"
  ) %>%
  st_sf()

# Irish Sea subdivisions
bb7an  <- data.frame(x = c(  -7,   -2, -2, -7,   -7), 
                     y = c(52.5, 52.5, 55, 55, 52.5)) %>% 
  sf::st_as_sf(coords = c("x","y")) %>% 
  sf::st_set_crs(4326) %>% 
  group_by() %>% 
  summarize(do_union=FALSE) %>% 
  st_cast("POLYGON")

bb7as  <- data.frame(x = c(-8, -4, -4, -8, -8  ), 
                     y = c(52, 52, 52.5, 52.5, 52)) %>% 
  sf::st_as_sf(coords = c("x","y")) %>% 
  sf::st_set_crs(4326) %>% 
  group_by() %>% 
  summarize(do_union=FALSE) %>% 
  st_cast("POLYGON")

t7as <-
  st_intersection(ices.sf, bb7as) %>%
  filter(grepl("27.7.a", F_CODE)) %>%
  mutate(
    F_SUBDIVIS = "27.7.a.s",
    F_CODE     = "27.7.a.s",
    F_LEVEL    = "SUBDIVISION"
  ) %>%
  st_sf()

t7an <-
  st_intersection(ices.sf, bb7an) %>%
  filter(grepl("27.7.a", F_CODE)) %>%
  mutate(
    F_SUBDIVIS = "27.7.a.n",
    F_CODE     = "27.7.a.n",
    F_LEVEL    = "SUBDIVISION"
  ) %>%
  st_sf()

t7as %>%
  mutate(n=row_number()) %>% 
  ggplot() +
  geom_sf(aes(fill=F_CODE), alpha=0.5) +
  geom_sf_text(aes(label = F_CODE)) +
  facet_wrap(~n)

# Baltic subunits

bb29    <- st_bbox(subset(ices, grepl("27.3.d.29", F_CODE)))

# subunits of area 27.3.d.29, 27.3.d30 and 27.3.d31
bb29NW <- data.frame(x = c(18  , 21.0, 21.0, 20.0, 18  ), 
                     y = c(59.5, 59.5, 60.0, 60.6, 60.6)) %>% 
  sf::st_as_sf(coords = c("x","y")) %>% 
  sf::st_set_crs(4326) %>% 
  group_by() %>% 
  summarize(do_union=FALSE) %>% 
  st_cast("POLYGON")

bb29NE <- data.frame(x = c(24  , 21.0, 21.0, 20.0, 24  ), 
                     y = c(59.5, 59.5, 60.0, 60.6, 60.6)) %>% 
  sf::st_as_sf(coords = c("x","y")) %>% 
  sf::st_set_crs(4326) %>% 
  group_by() %>% 
  summarize(do_union=FALSE) %>% 
  st_cast("POLYGON")

bb29S  <- data.frame(x = c(18  , 24  , 24, 18  ), 
                     y = c(58.4, 58.4, 59.5, 59.5)) %>% 
  sf::st_as_sf(coords = c("x","y")) %>% 
  sf::st_set_crs(4326) %>% 
  group_by() %>% 
  summarize(do_union=FALSE) %>% 
  st_cast("POLYGON")

bb30W  <- data.frame(x = c(17  , 20.2, 19.0, 20.0, 21.0, 17.0  ), 
                     y = c(60.5, 60.5, 61.5, 63.0, 63.5, 63.5)) %>% 
  sf::st_as_sf(coords = c("x","y")) %>% 
  sf::st_set_crs(4326) %>% 
  group_by() %>% 
  summarize(do_union=FALSE) %>% 
  st_cast("POLYGON")

bb30E  <- data.frame(x = c(23  , 20.2, 19.0, 20.0, 21.0, 23.0  ), 
                     y = c(60.5, 60.5, 61.5, 63.0, 63.5, 63.5)) %>% 
  sf::st_as_sf(coords = c("x","y")) %>% 
  sf::st_set_crs(4326) %>% 
  group_by() %>% 
  summarize(do_union=FALSE) %>% 
  st_cast("POLYGON")

# ices.sf %>% filter(F_CODE == "27.3.d.31") %>% ggplot() + theme_publication() + geom_sf()

bb31W  <- data.frame(x = c(19.0, 21.0, 23.5, 23.5, 19.0  ), 
                     y = c(63.5, 63.5, 65.0, 66.5, 66.5)) %>% 
  sf::st_as_sf(coords = c("x","y")) %>% 
  sf::st_set_crs(4326) %>% 
  group_by() %>% 
  summarize(do_union=FALSE) %>% 
  st_cast("POLYGON")

bb31E  <- data.frame(x = c(26.0, 21.0, 23.5, 23.5, 26.0  ), 
                     y = c(63.5, 63.5, 65.0, 66.5, 66.5)) %>% 
  sf::st_as_sf(coords = c("x","y")) %>% 
  sf::st_set_crs(4326) %>% 
  group_by() %>% 
  summarize(do_union=FALSE) %>% 
  st_cast("POLYGON")

# Intersections

t29NE <-
  st_intersection(ices.sf, bb29NE) %>%
  filter(grepl("27.3.d.29", F_CODE)) %>%
  mutate(
    F_SUBUNIT  = paste(F_CODE, "NE", sep="."),
    F_CODE     = F_SUBUNIT,
    F_LEVEL    = "SUBUNIT"
  ) %>%
  st_sf()

t29NW <-
  st_intersection(ices.sf, bb29NW) %>%
  filter(grepl("27.3.d.29", F_CODE)) %>%
  mutate(
    F_SUBUNIT  = paste(F_CODE, "NW", sep="."),
    F_CODE     = F_SUBUNIT,
    F_LEVEL    = "SUBUNIT"
  ) %>%
  st_sf()

t29S <-
  st_intersection(ices.sf, bb29S) %>%
  filter(grepl("27.3.d.29", F_CODE)) %>%
  mutate(
    F_SUBUNIT  = paste(F_CODE, "S", sep="."),
    F_CODE     = F_SUBUNIT,
    F_LEVEL    = "SUBUNIT"
  ) %>%
  st_sf()

t30E <-
  st_intersection(ices.sf, bb30E) %>%
  filter(grepl("27.3.d.30", F_CODE)) %>%
  mutate(
    F_SUBUNIT  = paste(F_CODE, "E", sep="."),
    F_CODE     = F_SUBUNIT,
    F_LEVEL    = "SUBUNIT"
  ) %>%
  st_sf()

t30W <-
  st_intersection(ices.sf, bb30W) %>%
  filter(grepl("27.3.d.30", F_CODE)) %>%
  mutate(
    F_SUBUNIT  = paste(F_CODE, "W", sep="."),
    F_CODE     = F_SUBUNIT,
    F_LEVEL    = "SUBUNIT"
  ) %>%
  st_sf()

t31E <-
  st_intersection(ices.sf, bb31E) %>%
  filter(grepl("27.3.d.31", F_CODE)) %>%
  mutate(
    F_SUBUNIT  = paste(F_CODE, "E", sep="."),
    F_CODE     = F_SUBUNIT,
    F_LEVEL    = "SUBUNIT"
  ) %>%
  st_sf()

t31W <-
  st_intersection(ices.sf, bb31W) %>%
  filter(grepl("27.3.d.31", F_CODE)) %>%
  mutate(F_SUBUNIT  = paste(F_CODE, "W", sep="."), F_CODE = F_SUBUNIT, F_LEVEL    = "SUBUNIT") %>%
  st_sf()

# t29NE %>% ggplot() + theme_publication() + geom_sf()
# do.call(rbind, list(t29NE, t29NW, t29S, t30E, t30W, t31W, t31E)) %>% 
#   ggplot() + 
#   theme_publication() + 
#   geom_sf(aes(fill = F_CODE), colour="black")

# combine the objects for additional areas and add the ices.sf object
ices.sf <-
  do.call(rbind, list(ices.sf, 
                      # t4transfer,
                      t4ae, t4aw,
                      t6an, t6as,
                      t7an, t7as,
                      t29NE, t29NW, t29S,
                      t30E, t30W,
                      t31E, t31W)) 

# ices.sf %>% 
#   filter(grepl("27.6", F_CODE)) %>%
#   ggplot() +
#   geom_sf(aes(fill=F_CODE)) +
#   facet_wrap(~F_LEVEL)
  

# summarize to subunits
ices.subunits.sf <-
  ices.sf %>%
  filter(F_LEVEL %in% c("SUBUNIT") )

# summarize to subdivisions
ices.subdivisions.sf <-
  ices.sf %>%
  filter(F_LEVEL %in% c("SUBUNIT", "SUBDIVISION")) %>%

  group_by(F_STATUS, OCEAN, SUBOCEAN, F_AREA, F_SUBAREA, F_DIVISION, F_SUBDIVIS) %>%
  summarise() %>%
  mutate(
    F_SUBUNIT = NA,
    F_CODE    = F_SUBDIVIS,
    F_LEVEL   = "SUBDIVISION")

# summarize to divisions
ices.divisions.sf <-
  do.call(rbind, list(ices.sf, ices.subdivisions.sf)) %>%
  filter(F_LEVEL %in% c("SUBDIVISION","DIVISION")) %>%
  group_by(F_STATUS, OCEAN, SUBOCEAN, F_AREA, F_SUBAREA, F_DIVISION) %>%
  summarise() %>%
  mutate(
    F_SUBUNIT  = NA,
    F_SUBDIVIS = NA,
    F_CODE     = F_DIVISION,
    F_LEVEL    = "DIVISION")

# summarize to subareas
ices.subareas.sf <-
  do.call(rbind, list(ices.sf, ices.divisions.sf)) %>%
  arrange(F_CODE) %>%
  filter(F_LEVEL %in% c("DIVISION","SUBAREA")) %>%

  group_by(F_STATUS, OCEAN, SUBOCEAN, F_AREA, F_SUBAREA) %>%
  summarise() %>%
  mutate(
    F_SUBUNIT  = NA,
    F_SUBDIVIS = NA,
    F_DIVISION = NA,
    F_CODE     = F_SUBAREA,
    F_LEVEL    = "SUBAREA")

# ices total area
ices.area.sf <-
  do.call(rbind, list(ices.sf, ices.subareas.sf)) %>%
  arrange(F_CODE) %>%
  filter(F_LEVEL %in% c("SUBAREA")) %>%

  group_by(F_STATUS, OCEAN, SUBOCEAN, F_AREA) %>%
  summarise() %>%
  mutate(
    F_SUBUNIT  = NA,
    F_SUBDIVIS = NA,
    F_DIVISION = NA,
    F_SUBAREA  = NA,
    F_CODE     = F_AREA,
    F_LEVEL    = "MAJOR")

# Create the final merged ices.sf object
ices.sf <-
  do.call(rbind, list(ices.area.sf, ices.subareas.sf, ices.divisions.sf, ices.subdivisions.sf, ices.subunits.sf)) %>%
  arrange(F_CODE)

# convert back to ices spatial polygons dataframe
ices <- as(ices.sf, 'Spatial')

# Check for errors in the polys
# sum(gIsValid(ices, byid=TRUE)==FALSE)

# Create the data frame object. First: create an id for coupling
ices@data$id <- rownames(ices@data)

# create polygons with id
# ices.points  <- broom::tidy(ices, region="id")
ices.points  <- broom::tidy(ices)

# merge together to generate data frame
ices.df      <- left_join(ices.points, ices@data, by="id")

save(ices   , file=paste(onedrive, "rdata/ices.RData", sep="/"))
save(ices.df, file=paste(onedrive, "rdata/ices.df.RData", sep="/"))
save(ices.sf, file=paste(onedrive, "rdata/ices.sf.RData", sep="/"))

load(file=paste(onedrive, "rdata/ices.RData", sep="/"))
load(file=paste(onedrive, "rdata/ices.df.RData", sep="/"))
load(file=paste(onedrive, "rdata/ices.sf.RData", sep="/"))

#plot
ices.sf %>%
  # filter(grepl("27.4|27.6|27.7", F_CODE)) %>%
  # filter(grepl("27.3.d", F_CODE)) %>%
  filter(grepl("27.4.a", F_CODE)) %>%
  
  ggplot() +
  theme(legend.position = "none") +
  geom_sf(aes(fill=F_CODE)) +
  geom_sf_text(aes(label = F_CODE)) +
  facet_wrap(~F_LEVEL)



# ---------------------------------------------------------------------
# create transfer area
# ---------------------------------------------------------------------

bbtransfer <- data.frame(x = c(3 ,    3,    7,    7,    8,  8,  3), 
                         y = c(57, 59.5, 59.5, 57.5, 57.5, 57, 57)) %>% 
  sf::st_as_sf(coords = c("x","y")) %>% 
  sf::st_set_crs(4326) %>% 
  group_by() %>% 
  summarize(do_union=FALSE) %>% 
  st_cast("POLYGON")
plot(bbtransfer)

# 4a east
transfer.sf <-
  st_intersection(ices.sf, bbtransfer) %>%
  group_by() %>% 
  summarize(do_union=TRUE) %>% 
  mutate(
    F_CODE  ="transfer", 
    F_LEVEL ="SUBUNIT") %>% 
  st_sf()


# convert back to ices spatial polygons dataframe
transfer <- as(transfer.sf, 'Spatial')

# Create the data frame object. First: create an id for coupling
transfer@data$id <- rownames(transfer@data)

# create polygons with id
transfer.points  <- broom::tidy(transfer)

# merge together to generate data frame
transfer.df      <- left_join(transfer.points, transfer@data, by="id")

save(transfer   , file=paste(onedrive, "rdata/transfer.RData", sep="/"))
save(transfer.df, file=paste(onedrive, "rdata/transfer.df.RData", sep="/"))
save(transfer.sf, file=paste(onedrive, "rdata/transfer.sf.RData", sep="/"))


# ================================================================================
# FAO areas, shp's downloaded from http://www.fao.org/fishery/area/search/en 
# ================================================================================

# see: https://github.com/tidyverse/ggplot2/wiki/plotting-polygon-shapefiles

# read the shapefiles
fao         <-
  readOGR(dsn=file.path(onedrive, "gis\\FAO_AREAS"), layer='FAO_AREAS') %>%

  # simplify
  rmapshaper::ms_simplify(., keep = 0.01, keep_shapes = TRUE) %>% 
  
  rgeos::gBuffer(., byid=TRUE, width=0)  # clean up errors

# sum(gIsValid(fao, byid=TRUE)==FALSE)

# fao simple features object
fao.sf.tmp  <- sf::st_as_sf(fao) %>%
  filter(F_AREA != "27") 


# View(fao.sf.tmp)
# names(ices.sf)
# names(fao.sf.tmp)
# setdiff(fao.sf.tmp, ices.sf)


# now replace area 27 with ices sf object
# fao.sf     <- sf::st_as_sf(fao)
fao.sf     <- 
  do.call(rbind, list(fao.sf.tmp, ices.sf)) %>% 
  mutate(F_LEVEL = factor(F_LEVEL, levels=c("MAJOR", "SUBAREA","DIVISION","SUBDIVISION", "SUBUNIT")))

# redo fao spatial polygons objects with the new area 27 embedded
fao <- 
  as(fao.sf, 'Spatial') %>% 
  
  # clean up errors
  rgeos::gBuffer(., byid=TRUE, width=0)  


# Check for errors in polys
print("number of errors in the polys")
sum(gIsValid(fao, byid=TRUE)==FALSE)

# create an id for coupling
fao@data$id <- rownames(fao@data)

# create polygons with id
fao.points  <- broom::tidy(fao, region="id")

# merge together
fao.df      <- left_join(fao.points, fao@data, by="id")

#save
save(fao   , file=paste(onedrive, "rdata/fao.RData", sep="/"))
save(fao.df, file=paste(onedrive, "rdata/fao.df.RData", sep="/"))
save(fao.sf, file=paste(onedrive, "rdata/fao.sf.RData", sep="/"))

load(file=paste(onedrive, "rdata/fao.RData", sep="/"))
load(file=paste(onedrive, "rdata/fao.df.RData", sep="/"))
load(file=paste(onedrive, "rdata/fao.sf.RData", sep="/"))

# plot

# fao.sf %>%
#   filter(grepl("27.6", F_CODE)) %>%
#   # filter(F_LEVEL == "SUBDIVISION") %>%
# 
#   ggplot() +
#   theme(legend.position = "none") +
#   geom_sf(aes(fill=F_CODE)) +
#   geom_sf_text(aes(label = F_CODE)) +
#   facet_wrap(~F_LEVEL)

# ================================================================================
# EEZ boundary shp's downloaded from www.marineregions.org
#
# 25/01/2019 converted v10 to lower resolution via  https://mapshaper.org/ at 1% 
# ================================================================================

# Old version (9)
# eez   <- 
#   readOGR(dsn=paste(onedrive, "gis/World_EEZ_v9_20161021_LR", sep="/"), layer='eez_lr') %>% 
#   rmapshaper::ms_simplify(., keep = 0.2, keep_shapes = TRUE)

# first the high resolution data; takes a long time
eez.hires     <- readOGR(dsn=file.path(onedrive,"gis/World_EEZ_v10_20180221"), layer='eez_v10')

# then the low resolution data derived from https://mapshaper.org/ at 0.01 resolution;
# and combine with the data frame from high resolution data
eez     <-
  sp::SpatialPolygonsDataFrame(
    Sr   = readOGR(dsn=file.path(onedrive,"gis/World_EEZ_v10_0.01"), layer='eez_v10'),
    data = eez.hires@data ) %>%
  spTransform(., CRS( "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" ) ) %>%
  rgeos::gBuffer(., byid=TRUE, width=0)  # clean up errors

eez.sf.hires <- 
  sf::st_as_sf(eez.hires) %>%
  filter(st_is_valid(.) == TRUE) %>% 
  ungroup() %>% 
  
  # Remove ISL_SJM area
  filter(paste(ISO_Ter1, ISO_Ter2,sep="_") %notin% c("ISL_SJM","FRA_ESP")) %>% 
  
  mutate(zone = ifelse(!is.na(ISO_Ter3),
                       paste(ISO_Ter1, ISO_Ter2, ISO_Ter3, sep="_"),
                       as.character(NA)),
         zone = ifelse(!is.na(ISO_Ter2) & is.na(ISO_Ter3),
                       paste(ISO_Ter1, ISO_Ter2, sep="_"),
                       zone),
         zone = ifelse( is.na(ISO_Ter2) & is.na(ISO_Ter3),
                        ISO_Ter1,
                        zone))  

eez.sf <- 
  rmapshaper::ms_simplify(eez.sf.hires, keep = 0.01, keep_shapes = FALSE)


# simple features object
eez.sf  <-
  sf::st_as_sf(eez) %>%

  mutate(
    ISO_Ter1     = as.character(ifelse(!is.na(ISO_Ter1), as.character(ISO_Ter1), "IHO")),
    coastalstate = ifelse(
      ISO_Ter1 %in% c("GBR","GGY","JEY", "DNK", "FRA", "ESP", "PRT", "BEL", "NLD",
                      "DEU","SWE", "FIN", "ITA", "GRC", "CYP", "MLT", "IRL",
                      "POL", "LVA","LTU", "EST", "ROU", "BGR", "SVN", "HRV"),
      "EUR", as.character(ISO_Ter1)))

object.size(eez.sf.hires)

# create an id for coupling
eez@data$id <- rownames(eez@data)

# create polygons with id
eez.points  <- broom::tidy(eez, region="id")

# merge together
eez.df      <-
  left_join(eez.points, eez@data, by="id") %>%
  mutate(
    ISO_Ter1     = as.character(ifelse(!is.na(ISO_Ter1), as.character(ISO_Ter1), "IHO")),
    coastalstate = ifelse(
      ISO_Ter1 %in% c("GBR","GGY","JEY", "DNK", "FRA", "ESP", "PRT", "BEL", "NLD",
                      "DEU","SWE", "FIN", "ITA", "GRC", "CYP", "MLT", "IRL",
                      "POL", "LVA","LTU", "EST", "ROU", "BGR", "SVN", "HRV"),
      "EUR", as.character(ISO_Ter1))) %>%
  as.data.frame()

save(eez,       file=paste(onedrive, "rdata/eez.RData", sep="/"))
save(eez.df,    file=paste(onedrive, "rdata/eez.df.RData", sep="/"))
save(eez.sf,    file=paste(onedrive, "rdata/eez.sf.RData", sep="/"))

load(file=paste(onedrive, "rdata/eez.RData", sep="/"))
load(file=paste(onedrive, "rdata/eez.df.RData", sep="/"))
load(file=paste(onedrive, "rdata/eez.sf.RData", sep="/"))

# eez.sf %>%
#   # filter(coastalstate %in% c("EUR", "ISL","NOR", "FRO", "GRL")) %>%
#   # st_intersection(eez.sf, box) %>%
#   group_by(coastalstate) %>% 
#   summarize() %>% 
#   
#   ggplot() + 
#   theme_publication() +
#   geom_sf(aes(fill=coastalstate))

# eez.sf %>%
#   filter(ISO_Ter1 %in% c("FRA")) %>%
#   ggplot() +
#   theme_publication() +
#   geom_sf(aes(fill=ISO_Ter1))+
#   geom_sf(data=filter(world.sf, country=="France"), fill="cornsilk")


# sort(unique(eez.sf$ISO_TER1))


# ================================================================================
# Intersection of the Exclusive Economic Zones and IHO areas
# VLIZ (2012). Intersect of IHO Sea Areas and Exclusive Economic Zones (version 2). 
# 27/04/2017 Available online at http://www.marineregions.org/.
# 23/01/2019 Version 3. downloaded, but just like eez, throws an error
# 25/01/2019 Converted version 3 via  https://mapshaper.org/ at 1%; repaired polys with gBuffer
# ================================================================================

# read the shapefiles
iho.eez.hires <- readOGR(dsn=file.path(onedrive,"gis/Intersect_EEZ_IHO_v4_2020"), layer='Intersect_EEZ_IHO_v4_2020')
# sum(gIsValid(iho.eez.hires, byid=TRUE)==FALSE)
# summary(iho.eez.hires)
# gdal_crs(iho.eez.hires)
# rgdal::CRSargs(iho.eez.hires)

# rmapshaper approach does not work??
# iho.eez <- rmapshaper::ms_simplify(iho.eez.hires, keep = 0.01)

# Create the low resolution data derived from https://mapshaper.org/ at 10% resolution;
iho.eez <- readOGR(dsn=file.path(onedrive,"gis/Intersect_EEZ_IHO_v4_2020_0.01"), layer='Intersect_EEZ_IHO_v4_2020') 
# sum(gIsValid(iho.eez, byid=TRUE)==FALSE)

# save(iho.eez   , file=file.path(onedrive,"rdata/iho.eez.tmp.RData"))
# load(file=file.path(onedrive,"rdata/iho.eez.tmp.RData"))

# simple features object
iho.eez.sf.hires <- 
  sf::st_read(dsn=file.path(onedrive,"gis/Intersect_EEZ_IHO_v4_2020"), 
              layer='Intersect_EEZ_IHO_v4_2020') %>% 
  filter(st_is_valid(.) == TRUE) %>% 
  mutate(ISO_TER1 = as.character(ifelse(!is.na(ISO_TER1), as.character(ISO_TER1), "IHO"))) %>% 
  ungroup()

iho.eez.sf.01 <- rmapshaper::ms_simplify(filter(iho.eez.sf.hires,                    row_number() <=100), keep = 0.01,keep_shapes = FALSE)
iho.eez.sf.02 <- rmapshaper::ms_simplify(filter(iho.eez.sf.hires, row_number() >100, row_number() <=200), keep = 0.01,keep_shapes = FALSE)
iho.eez.sf.03 <- rmapshaper::ms_simplify(filter(iho.eez.sf.hires, row_number() >200, row_number() <=300), keep = 0.01,keep_shapes = FALSE)
iho.eez.sf.04 <- rmapshaper::ms_simplify(filter(iho.eez.sf.hires, row_number() >300, row_number() <=400), keep = 0.01,keep_shapes = FALSE)
iho.eez.sf.05 <- rmapshaper::ms_simplify(filter(iho.eez.sf.hires, row_number() >400, row_number() <=500), keep = 0.01,keep_shapes = FALSE)
iho.eez.sf.06 <- rmapshaper::ms_simplify(filter(iho.eez.sf.hires, row_number() >500                    ), keep = 0.01,keep_shapes = FALSE)

# save(iho.eez.sf.01, file=file.path(onedrive,"rdata/iho.eez.sf.01.RData"))
# save(iho.eez.sf.02, file=file.path(onedrive,"rdata/iho.eez.sf.02.RData"))
# save(iho.eez.sf.03, file=file.path(onedrive,"rdata/iho.eez.sf.03.RData"))
# save(iho.eez.sf.04, file=file.path(onedrive,"rdata/iho.eez.sf.04.RData"))
# save(iho.eez.sf.05, file=file.path(onedrive,"rdata/iho.eez.sf.05.RData"))
# save(iho.eez.sf.06, file=file.path(onedrive,"rdata/iho.eez.sf.06.RData"))

iho.eez.sf <- rbind(iho.eez.sf.01, iho.eez.sf.02, iho.eez.sf.03, iho.eez.sf.04, iho.eez.sf.05, iho.eez.sf.06)

save(iho.eez.sf, file=file.path(onedrive,"rdata/iho.eez.sf.RData"))
  
# osmdata::available_features()
# coords <- matrix(c(50,65,15,25), byrow = TRUE, nrow = 2, ncol = 2, dimnames = list(c('x','y'),c('min','max'))) 
# location <- coords %>% osmdata::opq()
# alwusta <- location %>%
#   osmdata::add_osm_feature(key = "addr:state", value = c("Dhofar")) %>% 
#   osmdata::osmdata_sf()
# ggplot() + geom_sf(data = alwusta$osm_multipolygons, fill = 'light blue') + theme_minimal()

# simple features North Atlantic only; by country and boundary issues
temp.sf <- 
  iho.eez.sf.hires %>%
  filter(IHO_SEA %in% c("Arctic Ocean", "Barentsz Sea",
                        "North Atlantic Ocean","North Sea","Norwegian Sea",
                        "English Channel","Greenland Sea","Irish Sea and St. George's Channel",
                        "Celtic Sea", "Bristol Channel", "Bay of Biscay",
                        "Inner Seas off the West Coast of Scotland",
                        "Skagerrak", "Kattegat","Baltic Sea", 
                        "Gulf of Bothnia", "Gulf of Finland", "Gulf of Riga")) %>% 
  mutate(ISO_SOV1 = ifelse(is.na(ISO_SOV1), "INT", ISO_SOV1) ) %>% 
  mutate(ISO_TER1 = ifelse(grepl("Svalbard", EEZ), "SVA", ISO_TER1)) %>% 
  mutate(ISO_SOV1 = ifelse(ISO_TER1 %in% c("FRO","GRL","SJM", "SVA"), ISO_TER1, ISO_SOV1) ) %>% 
  mutate(ISO_SOV2 = ifelse(ISO_TER2 %in% c("FRO","GRL","SJM", "SVA"), ISO_TER2, ISO_SOV2)) %>% 
  mutate(ISO_SOV3 = ifelse(ISO_TER3 %in% c("FRO","GRL","SJM", "SVA"), ISO_TER3, ISO_SOV3)) %>% 
  
  # Remove ISL_SJM area
  filter(paste(ISO_TER1, ISO_TER2,sep="_") %notin% c("ISL_SJM","FRA_ESP")) %>% 
  
  mutate(zone = ifelse(!is.na(ISO_SOV3),
                       paste(ISO_SOV1, ISO_SOV2, ISO_SOV3, sep="_"),
                       as.character(NA)),
         zone = ifelse(!is.na(ISO_SOV2) & is.na(ISO_SOV3),
                       paste(ISO_SOV1, ISO_SOV2, sep="_"),
                       zone),
         zone = ifelse( is.na(ISO_SOV2) & is.na(ISO_SOV3),
                        ISO_SOV1,
                        zone))  

# Separate into primary and contested regions (t1, t2) and erase the contested area from the primary regions; then put together again. 
t1 <- temp.sf %>% filter(!grepl("_", zone)) 
t2 <- temp.sf %>% filter(grepl("_", zone))
d <- rmapshaper::ms_erase(target=t1, erase=t2)
iho.eezbycountry.na.sf.hires <- rbind(d, t2)
iho.eezbycountry.na.sf <- rmapshaper::ms_simplify(rbind(d, t2), keep = 0.01,keep_shapes = FALSE)

# ggplot() +
#   theme_publication() +
#   theme(legend.position="none") +
#   geom_sf(data=iho.eezbycountry.na.sf.hires, aes(fill=zone)) +
#   geom_sf(data=iho.eezbycountry.na.sf, aes(fill=zone), colour="red", alpha=0.5) +
#   coord_sf(xlim=c(-20,10), ylim=c(50,62))

# round(c(object.size(iho.eezbycountry.na.sf.hires), object.size(iho.eezbycountry.na.sf)) / 1024)

save(iho.eezbycountry.na.sf, file=file.path(onedrive,"rdata/iho.eezbycountry.na.sf.RData"))
load(file=file.path(onedrive,"rdata/iho.eezbycountry.na.sf.RData"))


# simple features North Atlantic only; deal with EU27 and boundary issues
temp.sf <- 
  iho.eez.sf.hires %>%
  filter(IHO_SEA %in% c("Arctic Ocean", "Barentsz Sea",
                        "North Atlantic Ocean","North Sea","Norwegian Sea",
                        "English Channel","Greenland Sea","Irish Sea and St. George's Channel",
                        "Celtic Sea", "Bristol Channel", "Bay of Biscay",
                        "Inner Seas off the West Coast of Scotland",
                        "Skagerrak", "Kattegat","Baltic Sea", 
                        "Gulf of Bothnia", "Gulf of Finland", "Gulf of Riga")) %>% 
  mutate(ISO_SOV1 = ifelse(is.na(ISO_SOV1), "INT", ISO_SOV1) ) %>% 
  mutate(ISO_TER1 = ifelse(grepl("Svalbard", EEZ), "SVA", ISO_TER1)) %>% 
  mutate(ISO_SOV1b = ISO_SOV1,
         ISO_SOV1b = ifelse(ISO_SOV1b %in% c("DNK", "FRA", "ESP", "PRT", "BEL", "NLD",
                                             "DEU","SWE", "FIN", "ITA", "GRC", "CYP", "MLT", "IRL",
                                             "POL", "LVA","LTU", "EST", "ROU", "BGR", "SVN", "HRV"),
                            "EU27",ISO_SOV1b),
         ISO_SOV1b = ifelse(ISO_TER1 %in% c("FRO","GRL","SJM", "SVA"), ISO_TER1, ISO_SOV1b) ) %>% 
  
  mutate(ISO_SOV2b = ISO_SOV2,
         ISO_SOV2b = ifelse(ISO_SOV2b %in% c("DNK", "FRA", "ESP", "PRT", "BEL", "NLD",
                                             "DEU","SWE", "FIN", "ITA", "GRC", "CYP", "MLT", "IRL",
                                             "POL", "LVA","LTU", "EST", "ROU", "BGR", "SVN", "HRV"),
                            "EU27",ISO_SOV2b),
         ISO_SOV2b = ifelse(ISO_TER2 %in% c("FRO","GRL","SJM", "SVA"), ISO_TER2, ISO_SOV2b)) %>% 
  mutate(ISO_SOV3b = ISO_SOV3,
         ISO_SOV3b = ifelse(ISO_SOV3b %in% c("DNK", "FRA", "ESP", "PRT", "BEL", "NLD",
                                             "DEU","SWE", "FIN", "ITA", "GRC", "CYP", "MLT", "IRL",
                                             "POL", "LVA","LTU", "EST", "ROU", "BGR", "SVN", "HRV"),
                            "EU27",ISO_SOV3b),
         ISO_SOV3b = ifelse(ISO_TER3 %in% c("FRO","GRL","SJM", "SVA"), ISO_TER3, ISO_SOV3b)) %>% 
  
  # Remove ISL_SJM and FRA_ESP area
  filter(paste(ISO_TER1, ISO_TER2,sep="_") %notin% c("ISL_SJM")) %>% 

  mutate(zone = ifelse(!is.na(ISO_SOV3b),
                              paste(ISO_SOV1b, ISO_SOV2b, ISO_SOV3b, sep="_"),
                              as.character(NA)),
         zone = ifelse(!is.na(ISO_SOV2b) & is.na(ISO_SOV3b),
                              paste(ISO_SOV1b, ISO_SOV2b, sep="_"),
                              zone),
         zone = ifelse( is.na(ISO_SOV2b) & is.na(ISO_SOV3b),
                              ISO_SOV1b,
                              zone),
         zone = ifelse(zone == "EU27_EU27", "EU27", zone))  

# Separate into primary and contested regions (t1, t2) and erase the contested area from the primary regions; then put together again. 
t1 <- temp.sf %>% filter(!grepl("_", zone)) 
t2 <- temp.sf %>% filter(grepl("_", zone))
d <- rmapshaper::ms_erase(target=t1, erase=t2)
iho.eez.na.sf.hires <- rbind(d, t2)
iho.eez.na.sf <- rmapshaper::ms_simplify(rbind(d, t2), keep = 0.01,keep_shapes = FALSE)

# South Pacific
ggplot() +
  theme_publication() +
  theme(legend.position="none") +
  geom_sf(data=iho.eez.sf.hires, aes(fill=IHO_SEA)) +
  geom_sf_text (data=iho.eez.sf.hires, aes(label=IHO_SEA)) +
  coord_sf(xlim=c(-120,-70), ylim=c(-60,-20))

temp.sf <- 
  iho.eez.sf.hires %>%
  filter(IHO_SEA %in% c("Arctic Ocean", "Barentsz Sea",
                        "North Atlantic Ocean","North Sea","Norwegian Sea",
                        "English Channel","Greenland Sea","Irish Sea and St. George's Channel",
                        "Celtic Sea", "Bristol Channel", "Bay of Biscay",
                        "Inner Seas off the West Coast of Scotland",
                        "Skagerrak", "Kattegat","Baltic Sea", 
                        "Gulf of Bothnia", "Gulf of Finland", "Gulf of Riga")) %>% 
  mutate(ISO_SOV1 = ifelse(is.na(ISO_SOV1), "INT", ISO_SOV1) ) %>% 
  mutate(ISO_TER1 = ifelse(grepl("Svalbard", EEZ), "SVA", ISO_TER1)) %>% 
  mutate(ISO_SOV1b = ISO_SOV1,
         ISO_SOV1b = ifelse(ISO_SOV1b %in% c("DNK", "FRA", "ESP", "PRT", "BEL", "NLD",
                                             "DEU","SWE", "FIN", "ITA", "GRC", "CYP", "MLT", "IRL",
                                             "POL", "LVA","LTU", "EST", "ROU", "BGR", "SVN", "HRV"),
                            "EU27",ISO_SOV1b),
         ISO_SOV1b = ifelse(ISO_TER1 %in% c("FRO","GRL","SJM", "SVA"), ISO_TER1, ISO_SOV1b) ) %>% 
  
  mutate(ISO_SOV2b = ISO_SOV2,
         ISO_SOV2b = ifelse(ISO_SOV2b %in% c("DNK", "FRA", "ESP", "PRT", "BEL", "NLD",
                                             "DEU","SWE", "FIN", "ITA", "GRC", "CYP", "MLT", "IRL",
                                             "POL", "LVA","LTU", "EST", "ROU", "BGR", "SVN", "HRV"),
                            "EU27",ISO_SOV2b),
         ISO_SOV2b = ifelse(ISO_TER2 %in% c("FRO","GRL","SJM", "SVA"), ISO_TER2, ISO_SOV2b)) %>% 
  mutate(ISO_SOV3b = ISO_SOV3,
         ISO_SOV3b = ifelse(ISO_SOV3b %in% c("DNK", "FRA", "ESP", "PRT", "BEL", "NLD",
                                             "DEU","SWE", "FIN", "ITA", "GRC", "CYP", "MLT", "IRL",
                                             "POL", "LVA","LTU", "EST", "ROU", "BGR", "SVN", "HRV"),
                            "EU27",ISO_SOV3b),
         ISO_SOV3b = ifelse(ISO_TER3 %in% c("FRO","GRL","SJM", "SVA"), ISO_TER3, ISO_SOV3b)) %>% 
  
  # Remove ISL_SJM and FRA_ESP area
  filter(paste(ISO_TER1, ISO_TER2,sep="_") %notin% c("ISL_SJM")) %>% 
  
  mutate(zone = ifelse(!is.na(ISO_SOV3b),
                       paste(ISO_SOV1b, ISO_SOV2b, ISO_SOV3b, sep="_"),
                       as.character(NA)),
         zone = ifelse(!is.na(ISO_SOV2b) & is.na(ISO_SOV3b),
                       paste(ISO_SOV1b, ISO_SOV2b, sep="_"),
                       zone),
         zone = ifelse( is.na(ISO_SOV2b) & is.na(ISO_SOV3b),
                        ISO_SOV1b,
                        zone),
         zone = ifelse(zone == "EU27_EU27", "EU27", zone))  

# Separate into primary and contested regions (t1, t2) and erase the contested area from the primary regions; then put together again. 
t1 <- temp.sf %>% filter(!grepl("_", zone)) 
t2 <- temp.sf %>% filter(grepl("_", zone))
d <- rmapshaper::ms_erase(target=t1, erase=t2)
iho.eez.na.sf.hires <- rbind(d, t2)
iho.eez.na.sf <- rmapshaper::ms_simplify(rbind(d, t2), keep = 0.01,keep_shapes = FALSE)


# ggplot() +
#   theme_publication() +
#   theme(legend.position="none") +
#   geom_sf(data=iho.eez.na.sf.hires, aes(fill=zone)) +
#   geom_sf(data=iho.eez.na.sf, aes(fill=zone), colour="red", alpha=0.5) +
#   coord_sf(xlim=c(-20,10), ylim=c(50,62))

# round(c(object.size(iho.eez.na.sf.hires), object.size(iho.eez.na.sf)) / 1024)

save(iho.eez.na.sf, file=file.path(onedrive,"rdata/iho.eez.na.sf.RData"))
load(file=file.path(onedrive,"rdata/iho.eez.na.sf.RData"))

bb <- sf::st_bbox(c(xmin = -50, xmax = 50, ymin = 45, ymax = 85), crs = st_crs(4326)) %>% sf::st_as_sfc()
t1 <-
  iho.eez.na.sf %>%
  group_by(zone) %>%
  summarise() 

iho.eez.na.df <-
  sf::as_Spatial(t1, ID = t1$zone) %>% 
  ggplot2::fortify() %>% 
  left_join(t1 %>% st_drop_geometry() %>% mutate(id=as.character(row_number())), 
            by="id")

save(iho.eez.na.df, file=file.path(onedrive,"rdata/iho.eez.na.df.RData"))

  
# Several plotting options
# iho.eez.na.sf %>% filter(MRGID == 49698) %>% 
#   ggplot() + 
#   theme_bw() +
#   geom_sf(aes(fill=ISO_TER1), alpha=0.5) +
#   coord_sf(xlim=c(10,50), ylim=c(65,85))

# st_centroid(iho.eez.na.sf) %>% filter(Y_1 > 65, Y_1 < 85, X_1 > 10, X_1 < 50 ) %>% View()

# iho.eez.na.sf %>% 
#   filter(zone != "ISL_SJM") %>% 
#   ggplot() + theme_bw() +
#   geom_sf(aes(fill=zone)) +
#   # geom_sf(data=t2, aes(fill=zone), alpha=0.5, colour="red") +
#   geom_sf(data=st_difference(t1,t2), aes(fill=zone)) +
#   geom_sf(data=t2, aes(fill=zone), alpha=0.5)

# iho.eez.sf %>% filter(MRGID_IHO == "4283") %>% View()

# xlim = c(-20, 5); ylim = c(45,62) # UK
# iho.eez.na.sf %>% 
#   filter(X_1 >= min(xlim), X_1 <= max(xlim), Y_1 >= min(ylim), Y_1 <= max(ylim)) %>% 
#   ggplot() + theme_bw() +
#   geom_sf(aes(fill=EEZ), alpha=0.5)  

# iho.eez.na.sf %>%
#   ggplot() +
#   theme(legend.position="none") +
#   # geom_sf(aes(fill=ISO_TER1)) +
#   # geom_sf_text(aes(label=ISO_TER1), stat="sf_coordinates") +
#   geom_sf(aes(fill=zone)) +
#   geom_sf_text(aes(label=zone), stat="sf_coordinates") +
#   # coord_sf(xlim=c(0,10), ylim=c(48,55))
#   coord_sf(xlim=c(-40,50), ylim=c(50,90))

# iho.eez.sf %>%
#   ggplot() +
#   theme(legend.position="none") +
#   geom_sf(aes(fill=ISO_TER1)) +
#   coord_sf(xlim=c(-50,65), ylim=c(25,90))

# iho.eez.sf %>%
#   ggplot() +
#   theme(legend.position="none") +
#   geom_sf(aes(fill=IHO_SEA)) +
#   geom_sf_text(aes(label=MRGID_IHO), stat="sf_coordinates") +
#   coord_sf(xlim=c(-50,65), ylim=c(25,90))

# create a data.frame from a simple features object?
# df2 = data.frame(st_coordinates(iho.eez.sf[,1]))

# create an id for coupling
iho.eez@data$id <- rownames(iho.eez@data)

# create polygons with id
iho.eez.points  <- tidy(iho.eez, region="id")

# merge together
iho.eez.df      <-
  left_join(iho.eez.points, iho.eez@data, by="id")

# save
save(iho.eez   , file=file.path(onedrive,"rdata/iho.eez.RData"))
save(iho.eez.df, file=file.path(onedrive,"rdata/iho.eez.df.RData"))
save(iho.eez.sf, file=file.path(onedrive,"rdata/iho.eez.sf.RData"))
save(iho.eez.na.sf, file=file.path(onedrive,"rdata/iho.eez.na.sf.RData"))

load(file=file.path(onedrive,"rdata/iho.eez.RData"))
load(file=file.path(onedrive,"rdata/iho.eez.df.RData"))
load(file=file.path(onedrive,"rdata/iho.eez.sf.RData"))
load(file=file.path(onedrive,"rdata/iho.eez.na.sf.RData"))

iho.eez.na.sf %>% 
  group_by(zone) %>% 
  summarise() %>% 
  ggplot() +
  theme_bw() +
  geom_sf(aes(fill=zone))


# # plot Ireland EEZ
# iho.eez.sf %>% 
#   filter(ISO_TER1 == "IRL") %>% 
#   ggplot() + geom_sf(aes(fill=ISO_TER1))
# 
# # plot EU EEZ
# iho.eez.sf %>%
#   filter(coastalstate == "EUR") %>%
#   ggplot() + geom_sf(aes(fill=coastalstate)) +
#   geom_sf(data=world2.sf, fill="cornsilk", alpha=0.5)
# 
# # plot EEZ and IHO around Ireland
# st_intersection(iho.eez.sf, 
#                 st_as_sfc(st_bbox(subset(iho.eez, ISO_TER1 == "IRL")))) %>% 
#   ggplot() + geom_sf(aes(fill=ISO_TER1))

# Still need to fix a number of islands ???

# plot international waters only
# iho.eez.sf %>%
#   filter(coastalstate == "EUR") %>%
#   ggplot() +
#   geom_sf(aes(fill=coastalstate)) 
 
# # plot eez with multiple ISO_TER
# iho.eez.sf %>% 
#   filter(!is.na(ISO_TER2)) %>% 
#   ggplot() + 
#   theme(legend.position = "none") +
#   geom_sf(aes(fill=ISO_TER1), alpha = 0.5) +
#   geom_sf(data=world2.sf, fill="cornsilk", alpha=0.5)

# IHO EEZ in NE Atlantic
# bb      <- st_bbox(subset(ices, F_CODE=="27"))
# bb[2]   <- 50
# bb[3]   <- 25
# bb[4]   <- 75
# box     <- st_as_sfc(bb)
# 
# st_intersection(iho.eez.sf, box) %>%
#   group_by(coastalstate) %>%
#   summarize() %>%
# 
#   ggplot() +
#   theme_publication() +
#   geom_sf(aes(fill=coastalstate))


# ----------------------------------------------------------------------------------------------------------
# Intersect the FAO and IHO/EEZ areas. 
# ----------------------------------------------------------------------------------------------------------

# calculate intersection between fao and iho.eez
intersect.sf <-
  
  st_intersection(fao.sf, iho.eez.sf) %>%
  
  # generate a new variable, the fao code and then country iso
  mutate(F_CODE = paste(F_CODE, coastalstate, sep="_"),
         F_LEVEL  = paste(F_LEVEL, "EEZ", sep="_")) %>% 
  
  # select only columns that are in fao.sf
  dplyr::select(one_of(c(names(fao.sf), "coastalstate")))

# intersect.sf %>% filter(grepl("27.3.d.29", F_CODE)) %>% View()

# create an object with FAO areas and the intersection between FAO and EEZ areas
fao.eez.sf <-
  
  # this is the way to bind rows for SF objects
  do.call(rbind, list(intersect.sf, 
                      mutate(fao.sf, coastalstate = as.character(NA)))) %>% 

  # aggregate over the F_CODEs (to remove specifics for sea areas that were in IHO.EEZ)
  group_by(F_LEVEL, F_CODE, OCEAN, SUBOCEAN,F_AREA,F_SUBAREA,F_DIVISION,F_SUBDIVIS,
           F_SUBUNIT, coastalstate) %>% 
  summarize() %>% 
  ungroup() %>% 
  st_as_sf() %>% 
  
  # Remove certain subdivisions or subunits from area 27. 
  filter(!(grepl("SUBDIVISION_EEZ", F_LEVEL) & grepl("27.1.", F_CODE, fixed=TRUE))) %>% 
  filter(!(grepl("SUBDIVISION_EEZ", F_LEVEL) & grepl("27.2.", F_CODE, fixed=TRUE))) %>% 
  filter(!(grepl("SUBDIVISION_EEZ", F_LEVEL) & grepl("27.5.", F_CODE, fixed=TRUE))) %>% 
  filter(!(grepl("SUBUNIT"        , F_LEVEL) & grepl("27.5.", F_CODE, fixed=TRUE))) %>% 
  filter(!(grepl("SUBDIVISION_EEZ", F_LEVEL) & grepl("27.6.b.", F_CODE, fixed=TRUE))) %>% 
  filter(!(grepl("SUBDIVISION_EEZ", F_LEVEL) & grepl("27.7.", F_CODE, fixed=TRUE))) %>% 
  filter(!(grepl("SUBDIVISION_EEZ", F_LEVEL) & grepl("27.8.", F_CODE, fixed=TRUE))) %>% 
  filter(!(grepl("SUBDIVISION_EEZ", F_LEVEL) & grepl("27.9.", F_CODE, fixed=TRUE))) %>% 
  filter(!(grepl("SUBDIVISION_EEZ", F_LEVEL) & grepl("27.10.", F_CODE, fixed=TRUE))) %>% 
  filter(!(grepl("SUBDIVISION_EEZ", F_LEVEL) & grepl("27.12.", F_CODE, fixed=TRUE))) %>% 
  filter(!(grepl("SUBDIVISION_EEZ", F_LEVEL) & grepl("27.14.", F_CODE, fixed=TRUE))) %>% 
  filter(!(grepl("SUBDIVISION_EEZ", F_LEVEL) & grepl("87."   , F_CODE, fixed=TRUE))) %>% 
  
  # make sure all the polygons are closed, by first exploding and then reconnecting
  # st_cast(.,"MULTILINESTRING") %>% 
  # st_polygonize() %>% 
  
  
  # filter only non-zero surfaces
  mutate(area = st_area(geometry)) %>%
  mutate(type = st_dimension(geometry)) %>% 
  mutate(F_LEVEL = factor(F_LEVEL, levels=c("MAJOR", "MAJOR_EEZ", 
                                            "SUBAREA","SUBAREA_EEZ", 
                                            "DIVISION","DIVISION_EEZ",
                                            "SUBDIVISION", "SUBDIVISION_EEZ",
                                            "SUBUNIT", "SUBUNIT_EEZ" ))) %>%   
  filter(as.numeric(type) == 2) %>% 
  arrange(F_CODE) 

# fao.eez.sf %>% filter(grepl("27.6", F_CODE)) %>% View()

# class(fao.eez.sf)
# glimpse(fao.eez.sf)
# unique(st_dimension(fao.eez.sf))
# filter(fao.eez.sf, as.numeric(type) == 1) %>% View()
# st_dimension(ices.sf)

# construct a spatial object from the sf object
# THIS GIVES AN ERROR. WHY? IT WORKS FOR THE ICES AND FAO OBJECTS???

# fao.eez <- 
#   as(fao.eez.sf, 'Spatial') %>% 
# 
#   # clean up errors
#   rgeos::gBuffer(., byid=TRUE, width=0)  

# Check: any bad polys?
# print("number of errors in the polys")
# sum(gIsValid(fao.eez, byid=TRUE)==FALSE)

# create an id for coupling
# fao.eez@data$id <- rownames(fao.eez@data)

# create polygons with id
# fao.eez.points  <- tidy(fao.eez, region="id")

# merge together
# fao.eez.df      <- 
#   left_join(fao.eez.points, fao.eez@data, by="id") 

# save
# save(fao.eez   , file=file.path(onedrive,"rdata/fao.eez.RData"))
# save(fao.eez.df, file=file.path(onedrive,"rdata/fao.eez.df.RData"))
save(fao.eez.sf, file=file.path(onedrive,"rdata/fao.eez.sf.RData"))

# load(file=file.path(onedrive,"rdata/fao.eez.RData"))
# load(file=file.path(onedrive,"rdata/fao.eez.df.RData"))
load(file=file.path(onedrive,"rdata/fao.eez.sf.RData"))


# my.area <- "27.7.a"
# 
# fao.eez.sf %>%
#   # filter(F_LEVEL == "SUBAREA") %>%
#   # filter(grepl("SUB", F_LEVEL)) %>%
#   filter(grepl(my.area, F_CODE, fixed=TRUE)) %>%
#   # filter(grepl("2_IHO", F_CODE)) %>%
#   mutate(valid     = st_is_valid(geometry),
#          dimension = st_dimension(geometry) ) %>%
#   # View()
# 
#   ggplot() +
#   theme(legend.position = "none") +
#   geom_sf(aes(fill=F_CODE), alpha=0.2) +
#   geom_sf_text(aes(label = F_CODE, colour=F_CODE)) +
# 
#   # geom_sf(data=filter(fao.sf, grepl(my.area, F_CODE)), colour="red", fill=NA) +
#   # geom_sf(data=world2.sf, fill="cornsilk", alpha=0.5, inherit.aes=FALSE) +
#   # coord_sf(xlim=c(-50,-5), ylim=c(53, 65)) +
#   facet_wrap(~F_LEVEL, ncol=3)
 



# ================================================================================
# 12 NM zone
# ================================================================================

# read the shapefiles
# NM12     <- 
#   readOGR(dsn=paste(onedrive, "gis\\World_12NM_v1_20161021", sep="/"), layer='eez_12NM') %>% 
#   
#   # simplify
#   rmapshaper::ms_simplify(., keep = 0.01, keep_shapes = TRUE) %>% 
#   
#   rgeos::gBuffer(., byid=TRUE, width=0)

# Check: any bad polys?
# sum(gIsValid(NM12, byid=TRUE)==FALSE)

# simple features object
# NM12.sf  <- sf::st_as_sf(NM12) 

# create an id for coupling
# NM12@data$id <- rownames(NM12@data)

# create polygons with id
# NM12.points  <- tidy(NM12, region="id")

# merge together
# NM12.df      <- left_join(NM12.points, NM12@data, by="id")


# save(NM12,       file=paste(onedrive, "rdata/nm12.RData", sep="/"))
# save(NM12.df,    file=paste(onedrive, "rdata/nm12.df.RData", sep="/"))
# save(NM12.sf,    file=paste(onedrive, "rdata/nm12.sf.RData", sep="/"))

# bb      <- st_bbox(ices)
# bb[[1]] <- -2      # set north boundary
# bb[[2]] <- 51      # set north boundary
# bb[[3]] <- 10      # set north boundary
# bb[[4]] <- 56      # set north boundary
# box     <- st_as_sfc(bb)
# t       <- st_intersection(ices.sf, box)
  
# eez.sf %>%
#   filter(grepl("Neth|Germ", Territory1)) %>%
#   ggplot() +
#   geom_sf() +
#   geom_sf(data=t, fill=NA, colour="red") +
#   geom_sf(data= filter(NM12.sf, grepl("Neth|United King", Territory1)), fill=NA, colour="blue")



# ================================================================================
# read ICES rectangles
# ================================================================================

# rect      <- readOGR(dsn="gis\\ices_rectangles", layer='ices_rectangles') 
# rect.df   <- fortify(rect)
icesrectangles     <-
  readOGR(dsn=paste(onedrive, "gis\\ices_rectangles", sep="/"), layer='ices_rectangles') 

# Check: any bad polys?
# sum(gIsValid(icesrectangles, byid=TRUE)==FALSE)

# simple features object
# icesrectangles.sf  <- sf::st_as_sf(icesrectangles) 
icesrectangles.sf <- 
  sf::st_read(dsn=file.path(onedrive,"gis/ices_rectangles"), layer='ices_rectangles') %>% 
  filter(st_is_valid(.) == TRUE) %>% 
  
# save(icesrectangles.sf,    file=paste(onedrive, "rdata/icesrectangles.sf.RData", sep="/"))

# ================================================================================
# read UK 12m fishing limit, shp's downloaded from http://aws2.caris.com/ukho/mapViewer/map.action
# ================================================================================

# uklimit    <- readOGR(dsn="gis\\UK_12M_Fish_Limit", layer='UK_12M_Fish_Limit') 
# uklimit.df <- fortify(uklimit)
# save(uklimit.df,file="rdata/uklimit.df.RData")


# ================================================================================
# 200m depth contour downloaded from http://www.naturalearthdata.com/downloads
# ================================================================================

# read the shapefiles
# depth200         <- 
#   readOGR(dsn="D:/XXX/PRF/gis/ne_10m_bathymetry_K_200", layer="ne_10m_bathymetry_K_200") 

# create an id for coupling
# depth200@data$id <- rownames(depth200@data)

# create polygons with id
# depth200.points  <- tidy(depth200, region="id")

# merge together
# depth200.df      <- left_join(depth200.points, depth200@data, by="id")


# save(depth200.df,   file="rdata/depth200.df.RData")
# save(depth200,      file="rdata/depth200.RData")

# ================================================================================
# multiple depth contour downloaded from https://www.naturalearthdata.com/downloads/10m-physical-vectors/
# ================================================================================

# 0 m
depth0.sf <- 
  st_read(dsn=file.path(onedrive, "gis/ne_10m_bathymetry_all/ne_10m_bathymetry_L_0.shp")) %>% 
  st_transform(crs = 4326) 

# convert back to ices spatial polygons dataframe
depth0 <- as(depth0.sf, 'Spatial')

# Create the data frame object. First: create an id for coupling
depth0@data$id <- rownames(depth0@data)

# create polygons with id
depth0.points  <- broom::tidy(depth0)

# merge together to generate data frame
depth0.df      <- left_join(depth0.points, depth0@data, by="id")

save(depth0   , file=paste(onedrive, "rdata/depth0.RData", sep="/"))
save(depth0.df, file=paste(onedrive, "rdata/depth0.df.RData", sep="/"))
save(depth0.sf, file=paste(onedrive, "rdata/depth0.sf.RData", sep="/"))

# 200 m
depth200.sf <- sf::st_read(dsn=file.path(onedrive, "gis/ne_10m_bathymetry_all/ne_10m_bathymetry_K_200.shp")) %>% 
  sf::st_transform(crs = 4326) %>% 
  sf::st_make_valid()

# sf::st_is_valid(depth200.sf, reason=TRUE)

# convert back to ices spatial polygons dataframe
depth200 <- as(depth200.sf, 'Spatial')

# Create the data frame object. First: create an id for coupling
depth200@data$id <- rownames(depth200@data)

# create polygons with id
depth200.points  <- broom::tidy(depth200)

# merge together to generate data frame
depth200.df      <- left_join(depth200.points, depth200@data, by="id")

# sf::st_is_valid(depth200.sf, reason=TRUE)
# any(is.na(sf::st_dimension(depth200.sf))) # empty geometries
# any(is.na(sf::st_is_valid(depth200.sf)))  # corrupt geometries
# any(na.omit(sf::st_is_valid(depth200.sf)) == FALSE)  # invalid geometries
# rgeos::gIsValid(depth200, byid=TRUE)

save(depth200   , file=paste(onedrive, "rdata/depth200.RData", sep="/"))
save(depth200.df, file=paste(onedrive, "rdata/depth200.df.RData", sep="/"))
save(depth200.sf, file=paste(onedrive, "rdata/depth200.sf.RData", sep="/"))


# 1000 m
depth1000.sf <- st_read(dsn=file.path(onedrive, "gis/ne_10m_bathymetry_all/ne_10m_bathymetry_J_1000.shp")) %>% 
  st_transform(crs = 4326) 

# convert back to ices spatial polygons dataframe
depth1000 <- as(depth1000.sf, 'Spatial')

# Create the data frame object. First: create an id for coupling
depth1000@data$id <- rownames(depth1000@data)

# create polygons with id
depth1000.points  <- broom::tidy(depth1000)

# merge together to generate data frame
depth1000.df      <- left_join(depth1000.points, depth1000@data, by="id")

save(depth1000   , file=paste(onedrive, "rdata/depth1000.RData", sep="/"))
save(depth1000.df, file=paste(onedrive, "rdata/depth1000.df.RData", sep="/"))
save(depth1000.sf, file=paste(onedrive, "rdata/depth1000.sf.RData", sep="/"))

# 2000 m
depth2000.sf <- st_read(dsn=file.path(onedrive, "gis/ne_10m_bathymetry_all/ne_10m_bathymetry_I_2000.shp")) %>% 
  st_transform(crs = 4326) 

# convert back to ices spatial polygons dataframe
depth2000 <- as(depth2000.sf, 'Spatial')

# Create the data frame object. First: create an id for coupling
depth2000@data$id <- rownames(depth2000@data)

# create polygons with id
depth2000.points  <- broom::tidy(depth2000)

# merge together to generate data frame
depth2000.df      <- left_join(depth2000.points, depth2000@data, by="id")

save(depth2000   , file=paste(onedrive, "rdata/depth2000.RData", sep="/"))
save(depth2000.df, file=paste(onedrive, "rdata/depth2000.df.RData", sep="/"))
save(depth2000.sf, file=paste(onedrive, "rdata/depth2000.sf.RData", sep="/"))

# 5000 m
depth5000.sf <- st_read(dsn=file.path(onedrive, "gis/ne_10m_bathymetry_all/ne_10m_bathymetry_F_5000.shp")) %>% 
  st_transform(crs = 4326) 

# convert back to ices spatial polygons dataframe
depth5000 <- as(depth5000.sf, 'Spatial')

# Create the data frame object. First: create an id for coupling
depth5000@data$id <- rownames(depth5000@data)

# create polygons with id
depth5000.points  <- broom::tidy(depth5000)

# merge together to generate data frame
depth5000.df      <- left_join(depth5000.points, depth5000@data, by="id")

save(depth5000   , file=paste(onedrive, "rdata/depth5000.RData", sep="/"))
save(depth5000.df, file=paste(onedrive, "rdata/depth5000.df.RData", sep="/"))
save(depth5000.sf, file=paste(onedrive, "rdata/depth5000.sf.RData", sep="/"))

load(file.path(onedrive, "rdata/depth200.RData"))
load(file.path(onedrive, "rdata/depth200.sf.RData"))
load(file.path(onedrive, "rdata/depth200.df.RData"))

# ================================================================================
# OSPAR MPAs
# ================================================================================

# sf::sf_use_s2(FALSE)

path   <- file.path(onedrive, "gis") 
folder <- "OSPAR Offshore MPAs 20220712 WGS84"
layer  <- 'carto_amp.ospar_polygon_wdpa_simplified'


st_is_valid(ospar)

ospar    <- create_spatial(path=path, folder=folder, layer=layer, simplify=NA)
ospar.sf <- create_sf(path=path, folder=folder, layer=layer, simplify=NA)
ospar.df <- create_spatial_df(path=path, folder=folder, layer=layer, simplify=NA)

save(ospar,       file=paste(onedrive, "rdata/ospar.RData", sep="/"))
save(ospar.df,    file=paste(onedrive, "rdata/ospar.df.RData", sep="/"))
save(ospar.sf,    file=paste(onedrive, "rdata/ospar.sf.RData", sep="/"))


# ================================================================================
# UK SAC
# ================================================================================

# sac_shape <- "c20190329_UKSACswithMarineComponents"
# sac_layer <- 'c20190328_UKSACswithMarineComponents_WGS84'
# sac_excel <- file.path(sac.shape, "UKSACswithMarineComponents_20190328_LIVE_WEB.xlsx")

path   <- file.path(onedrive, "gis") 
folder <- "UK SACs with Marine Components 20200527"
layer  <- 'c20200527_UKSACswithMarineComponents_WGS84'
excel  <- file.path(path, folder, "UK SACs with Marine Components 20200527-LIVE-WEB.xlsx")


uksac    <- create_spatial(path=path, folder=folder, layer=layer, simplify=NA)
uksac.sf <- create_sf(path=path, folder=folder, layer=layer, simplify=NA)
uksac.df <- create_spatial_df(path=path, folder=folder, layer=layer, simplify=NA)
uksac.features <- 
  read_excel(path=excel, sheet = "SAC Marine Interest Features", col_names=TRUE, col_types="text")  %>% 
  lowcase() %>% 
  dplyr::select(SITE_CODE=sitecode, featuretype, qualifyingmarineinterestfeatures, commonname=laytitleorcommonname) 

save(uksac,          file=paste(onedrive, "rdata/uksac.RData", sep="/"))
save(uksac.df,       file=paste(onedrive, "rdata/uksac.df.RData", sep="/"))
save(uksac.sf,       file=paste(onedrive, "rdata/uksac.sf.RData", sep="/"))
save(uksac.features, file=paste(onedrive, "rdata/uksac.features.RData", sep="/"))


hpma_sf <- 
  readxl::read_excel(
    path=file.path(onedrive, "excel", "hpma.xlsx")) %>% 
  # data.frame(lat = c( 50.14633056, 50.28140833, 50.15691667, 50.12358333, 50.14633056), 
  #            lon = c(-1.231713889,-0.434886111,-0.415144444,-0.501141667,-1.231713889)) %>% 
  # mutate(sitename = "dolphinhead", group="1") %>% 
  sf::st_as_sf(coords = c("lon","lat")) %>% 
  sf::st_set_crs(4326) %>% 
  group_by(sitename, group, L2) %>% 
  summarize(do_union=FALSE) %>% 
  st_cast("POLYGON")

hpma_df <- 
  st_coordinates(hpma_sf) %>%
  as.data.frame() %>% 
  left_join(st_drop_geometry(hpma_sf), by="L2") %>% 
  setNames(gsub("X","lon", names(.))) %>% 
  setNames(gsub("Y","lat", names(.))) %>% 
  as.data.frame() 

# read the shapefiles
sac     <-
  readOGR(dsn   = file.path(onedrive, "gis",sac_shape), 
          layer = sac_layer) %>%
  
  # simplify
  rmapshaper::ms_simplify(., keep = 0.01, keep_shapes = TRUE) %>%

  rgeos::gBuffer(., byid=TRUE, width=0)

# Check: any bad polys?
sum(gIsValid(sac, byid=TRUE)==FALSE)

# read descriptive files
sac.sites <- 
  read_excel(
    path = file.path(onedrive, "gis",sac_excel), 
    sheet = "Sites",  
    col_names=TRUE, 
    col_types="text")  %>% 
  data.frame()  %>% 
  lowcase() 

sac.marineinterestfeatures <- 
  read_excel(
    file.path(onedrive, "gis", sac_excel), 
    sheet = "Protected Features",  
    col_names=TRUE, 
    col_types="text")  %>% 
  data.frame()  %>% 
  lowcase() %>% 
  dplyr::select(sitecode, featuretypebroad, featuretypespecific, marinefeaturesprotected, commonname)


# simple features object
sac.sf  <- sf::st_as_sf(sac) %>% 
  rename(sitecode = SITE_CODE) %>% 
  left_join(sac.marineinterestfeatures, by="sitecode")

# create an id for coupling
sac@data$id <- rownames(sac@data)

# create polygons with id
sac.points  <- tidy(sac, region="id")

# merge together
sac.df      <- 
  left_join(sac.points, sac@data, by="id") %>% 
  rename(sitecode = SITE_CODE) %>% 
  left_join(sac.marineinterestfeatures, by="sitecode")


save(hpma_sf, file=paste(onedrive, "rdata/hpma_sf.RData", sep="/"))
save(hpma_df, file=paste(onedrive, "rdata/hpma_df.RData", sep="/"))

# ================================================================================
# Scotland energy areas
# ================================================================================



# read the shapefiles
scottish_energy     <-
  readOGR(dsn   = file.path(onedrive, "gis","Scotland 2022 energy_resources_smp_wind_plan_options"), 
          layer = "energy_resources_smp_wind_plan_optionsPolygon") %>%
  
  # simplify
  rmapshaper::ms_simplify(., keep = 0.01, keep_shapes = TRUE) %>%
  
  rgeos::gBuffer(., byid=TRUE, width=0)

# Check: any bad polys?
sum(gIsValid(scottish_energy, byid=TRUE)==FALSE)


# simple features object
scottish_energy.sf  <- 
  sf::st_as_sf(scottish_energy) 

# create an id for coupling
scottish_energy@data$id <- rownames(scottish_energy@data)

# create polygons with id
scottish_energy.points  <- tidy(scottish_energy, region="id")

# merge together
scottish_energy.df      <- 
  left_join(scottish_energy.points, scottish_energy@data, by="id") 

save(scottish_energy,       file=paste(onedrive, "rdata/scottish_energy.RData", sep="/"))
save(scottish_energy.df,    file=paste(onedrive, "rdata/scottish_energy.df.RData", sep="/"))
save(scottish_energy.sf,    file=paste(onedrive, "rdata/scottish_energy.sf.RData", sep="/"))

# scottish_energy.sf %>%
#   ggplot() + theme_bw() +
#   # geom_sf(data=iho.eez.na.sf) +
#   geom_sf(aes(fill=name), alpha=0.5) +
#   geom_sf_text(aes(label=name), size=3, stat = "sf_coordinates") +
#   coord_sf(xlim=c(-15,10), ylim=c(55,62))







# read the shapefiles
scottish_intog     <-
  readOGR(dsn   = file.path(onedrive, "gis","energy_resources_smp_intog_aos_and_innovation"), 
          layer = "energy_resources_smp_intog_aos_and_innovationPolygon") %>%
  
  # simplify
  rmapshaper::ms_simplify(., keep = 0.01, keep_shapes = TRUE) %>%
  
  rgeos::gBuffer(., byid=TRUE, width=0)

# Check: any bad polys?
sum(gIsValid(scottish_energy, byid=TRUE)==FALSE)


# simple features object
scottish_intog.sf  <- 
  sf::st_as_sf(scottish_intog) 

# create an id for coupling
scottish_intog@data$id <- rownames(scottish_intog@data)

# create polygons with id
scottish_intog.points  <- tidy(scottish_intog, region="id")

# merge together
scottish_intog.df      <- 
  left_join(scottish_intog.points, scottish_intog@data, by="id") 

save(scottish_intog,       file=paste(onedrive, "rdata/scottish_intog.RData", sep="/"))
save(scottish_intog.df,    file=paste(onedrive, "rdata/scottish_intog.df.RData", sep="/"))
save(scottish_intog.sf,    file=paste(onedrive, "rdata/scottish_intog.sf.RData", sep="/"))

# ================================================================================
# NL Wind
# ================================================================================

# path   <- file.path(onedrive, "gis") 
# folder <- "UK SACs with Marine Components 20200527"
# layer  <- 'c20200527_UKSACswithMarineComponents_WGS84'

# TO BE DONE


# ================================================================================
# Mauritania
# ================================================================================

limit <- 
  readxl::read_excel(
    path=file.path(onedrive, "gis","limiet mauritanie new2.xlsx"),
    sheet="data",
    col_names = TRUE,
    col_types = "text",
    .name_repair = ~make.names(., unique = TRUE)
  ) %>% 
  mutate(
    dlon = as.numeric(dlon),
    dlat = as.numeric(dlat),
    code = paste(limit, section, sep="_")
  ) %>% 
  filter(!is.na(dlon) ) %>% 
  filter(section == "north") %>% 
  sf::st_as_sf(coords = c("dlon","dlat")) %>% 
  sf::st_set_crs(4326) %>% 
  group_by(limit, section, code) %>%
  summarize(do_union=FALSE) %>% 
  st_cast("LINESTRING")


ggplot() + 
  theme_publication() +
  theme(axis.text.x=element_text(angle=90)) +
  # geom_sf(data=tmp) + 
  # geom_sf(data=eez, colour="red", fill=NA) +
  # geom_sf_text(data=tmp, aes(label=F_DIVISION)) +
  geom_sf(data=limit, aes(colour=code)) 




eez <-
  loadRData(file=paste(onedrive, "rdata/eez.sf.RData", sep="/")) %>%
  filter(grepl("sahara|mauritan",tolower(Sovereign1)))

tmp <-
  fao.sf %>% 
  filter(F_DIVISION %in% c("34.1.3","34.3.1")) %>% 
  filter(F_LEVEL == "DIVISION") 


# ================================================================================
# Intersection of ICES rectangles and IHO/EEZ (by coastalstate)
# ================================================================================

# North Atlantic: calculate proportions of ices rectangles by EEZ

sf::sf_use_s2(FALSE)

load(file=file.path(onedrive,"rdata/icesrectangles.sf.RData"))
load(file=file.path(onedrive,"rdata/iho.eez.na.sf.RData"))

icesrectangles.eez.sf <- 
  icesrectangles.sf %>% 
  sf::st_intersection(iho.eez.na.sf %>% group_by(zone) %>% summarise(do_union=TRUE) %>% ungroup() %>% sf::st_as_sf() ) %>% 
  rename(rect = ICESNAME) %>% 
  mutate(lon = (WEST + EAST) / 2) %>% 
  mutate(lat = (SOUTH + NORTH) / 2) %>% 
  
  mutate(area = as.numeric(sf::st_area(.))/1000000) %>% 
  group_by(rect) %>% 
  mutate(
    n    = n(),
    prop = area / sum(area)
  )

# save excel
writexl::write_xlsx(dplyr::select(icesrectangles.eez.sf,
                                  rect, lon, lat, zone, area, prop) %>% arrange(rect), 
                    path="ZbyCoastalstate_withUK.xlsx")

# save rdata
save(icesrectangles.eez.sf, file=file.path(onedrive, "icesrectangles.eez.sf.RData"))

# plotting
xlim = c(-15, 10); ylim = c(64,75) # Jan Mayen
xlim = c(-20, 5); ylim = c(45,62) # UK
xlim = c(20, 50); ylim = c(65,80) # Russia
xlim = c(-20, 15); ylim = c(45,62) # EU

icesrectangles.eez.sf %>% 
  filter(zone %in% c("FRO","GBR","GBR_FRO", "FRO_ISL","ISL","INT", "SJM", "SVA","GRL","NOR", "EU27", "RUS")) %>% 
  # filter(zone %in% c("FRO_ISL")) %>% 
  filter(lon >= min(xlim), lon <= max(xlim), lat >= min(ylim), lat <= max(ylim)) %>% 
  ggplot() + theme_bw() +
  geom_sf(aes(fill=zone), alpha=0.5) +
  geom_sf_text(aes(label=round(prop, digits=2)), size=3, stat = "sf_coordinates") 

## create maps in separate plots, force common scale between them
maps <- map(.x = unique(icesrectangles.eez.sf$zone), 
            .f = function(x) icesrectangles.eez.sf %>% 
              filter(zone == x, prop <1) %>% 
              ggplot() +
              theme_bw() +
              geom_sf(fill="lightblue") +
              geom_sf_text(aes(label=round(prop, digits=2)), size=3, stat="sf_coordinates")+
              facet_wrap(~zone))

cowplot::plot_grid(plotlist = maps, nrow = 3)
patchwork::wrap_plots(maps)

# ================================================================================
# calculate seasurface per rectangle
# ================================================================================

sf::sf_use_s2(FALSE)

load(file=file.path(onedrive,"rdata/icesrectangles.sf.RData"))
load(file=file.path(onedrive,"rdata/iho.eez.na.sf.RData"))

icesrectangles.seasurfaces <-
  icesrectangles.sf %>% 
  sf::st_intersection(iho.eez.na.sf %>% ungroup() %>% summarise(do_union=TRUE) %>% ungroup() %>% sf::st_as_sf() ) %>%
  rename(rect = ICESNAME) %>%
  mutate(lon = (WEST + EAST) / 2) %>%
  mutate(lat = (SOUTH + NORTH) / 2) %>%
  mutate(area = as.numeric(sf::st_area(.))/1000000) %>% 
  mutate(diff = AREA_KM2 - area)

writexl::write_xlsx(icesrectangles.seasurfaces, path = "C:/TEMP/icesrectangles_seasurfaces.xlsx")

icesrectangles.seasurfaces %>% 
  filter(diff > 500) %>% 
  filter(lon >= -4, lon <= 10, lat >= 50, lat <= 60) %>% 
  ggplot() + theme_bw() +
  geom_sf(alpha=0.5) +
  geom_sf_text(aes(label=round(area, digits=0)), size=3, stat = "sf_coordinates") 

# icesrectangles.sf %>% 
#   mutate(
#     lon = (WEST+EAST)/2,
#     lat = (NORTH+SOUTH)/2
#   ) %>% 
#   filter(ICESNAME %in% c("26E9","26F0","33E1","36E1")) %>% 
#   # filter(lon >= -4, lon <= 10, lat >= 40, lat <= 50) %>% 
#   ggplot() + theme_bw() +
#   geom_sf(data=iho.eez.na.sf) +
#   geom_sf(alpha=0.5) +
#   geom_sf_text(aes(label=ICESNAME), size=3, stat = "sf_coordinates") +
#   coord_sf(xlim=c(-4,10), ylim=c(40,50))


# ================================================================================
# Intersection of ICES rectangles and IHO/EEZ (by country)
# ================================================================================

# North Atlantic: calculate proportions of ices rectangles by EEZ

sf::sf_use_s2(FALSE)

load(file=file.path(onedrive,"rdata/icesrectangles.sf.RData"))
load(file=file.path(onedrive,"rdata/iho.eezbycountry.na.sf.RData"))
# glimpse(iho.eez.na.sf)

icesrectangles.eezbycountry.sf <- 
  icesrectangles.sf %>% 
  sf::st_intersection(iho.eezbycountry.na.sf %>% group_by(zone) %>% summarise(do_union=TRUE) %>% ungroup() %>% sf::st_as_sf() ) %>% 
  rename(rect = ICESNAME) %>% 
  mutate(lon = (WEST + EAST) / 2) %>% 
  mutate(lat = (SOUTH + NORTH) / 2) %>% 
  
  mutate(area = as.numeric(sf::st_area(.))/1000000) %>% 
  group_by(rect) %>% 
  mutate(
    n    = n(),
    prop = area / sum(area)
  )

# save excel
writexl::write_xlsx(dplyr::select(icesrectangles.eezbycountry.sf,
                                  rect, lon, lat, zone, area, prop) %>% arrange(rect), 
                    path="ZbyCountry_withUK.xlsx")

# save rdata
save(icesrectangles.eezbycountry.sf, file=file.path(onedrive, "icesrectangles.eezbycountry.sf.RData"))

# plotting
xlim = c(-15, 10); ylim = c(64,75) # Jan Mayen
xlim = c(-20, 5); ylim = c(45,62) # UK
xlim = c(20, 50); ylim = c(65,80) # Russia
xlim = c(-20, 15); ylim = c(45,62) # EU

icesrectangles.eezbycountry.sf %>% 
  # filter(zone %in% c("FRO","GBR","GBR_FRO", "FRO_ISL","ISL","INT", "SJM", "SVA","GRL","NOR", "EU27", "RUS")) %>% 
  # filter(zone %in% c("FRO_ISL")) %>% 
  filter(lon >= min(xlim), lon <= max(xlim), lat >= min(ylim), lat <= max(ylim)) %>% 
  ggplot() + theme_bw() +
  geom_sf(aes(fill=zone), alpha=0.5) +
  geom_sf_text(aes(label=round(prop, digits=2)), size=3, stat = "sf_coordinates") 

## create maps in separate plots, force common scale between them
maps <- map(.x = unique(icesrectangles.eezbycountry.sf$zone), 
            .f = function(x) icesrectangles.eezbycountry.sf %>% 
              filter(zone == x, prop <1) %>% 
              ggplot() +
              theme_bw() +
              geom_sf(fill="lightblue") +
              geom_sf_text(aes(label=round(prop, digits=2)), size=3, stat="sf_coordinates")+
              facet_wrap(~zone))

# cowplot::plot_grid(plotlist = maps, nrow = 3)
patchwork::wrap_plots(maps)

devtools::install_github("ropenscilabs/rnaturalearth")
devtools::install_github("ropensci/rnaturalearthhires")

# ================================================================================
# Marocco 15 and 20 mile zones
# ================================================================================

sf::sf_use_s2(FALSE)

load(file=paste(onedrive, "rdata/world.sf.RData", sep="/"))
load(file=paste(onedrive, "rdata/eez.sf.RData", sep="/"))

bb  <- sf::st_bbox(c(xmin = -20, xmax = -14, ymin = 24, ymax = 25), crs = st_crs(4326)) %>% sf::st_as_sfc()
bb2 <- sf::st_bbox(c(xmin = -22, xmax = -14, ymin = 20, ymax = 26), crs = st_crs(4326)) %>% sf::st_as_sfc()

lon = c(-14.6,  -16.0, -17.0, -17.3,   -16.80,  -14.7, -14.40, -14.60)
lat = c( 25.99,  24.0,  22.5,  20.78,  20.78,  24.7,  25.99,  25.99)

bb3 <-
  data.frame(lon, lat) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  summarise((geometry = st_combine(geometry))) %>% 
  st_cast("POLYGON")

eez <-
  eez.sf %>% 
  filter(Sovereign1 == "Morocco" | Sovereign2 == "Morocco") %>% 
  sf::st_intersection(bb2) 

# country_border <-
#   world.sf %>% 
#   st_cast("MULTILINESTRING") %>% 
#   sf::st_intersection(bb3) 

# just the coastline derived from the eez file; through intersection with bb3
eez_border <-
  eez %>%
  st_cast("MULTILINESTRING") %>% 
  sf::st_intersection(bb3) 

# eez %>% st_coordinates() %>% View()
# ggplot() + geom_sf(data=eez_border) + geom_sf(data=country_border, aes(colour=country))

country <-
  world.sf %>%
  # filter(country == "Morocco") %>%
  filter(country %in% c("Morocco", "Mauritania")) %>%
  sf::st_intersection(bb2)

fifteenmile <-
  eez_border %>% 
  st_transform(crs=7801) %>% 
  st_buffer(dist = -14 * 1852, singleSide = TRUE) %>% 
  st_cast("POLYGON") %>% 
  st_transform(crs=4326) %>% 
  mutate(area="0_15mile")

twentymile <-
  eez_border %>% 
  # morocco %>% 
  # st_cast("MULTILINESTRING") %>% 
  st_transform(crs=7801) %>% 
  st_buffer(dist = -20 * 1852, singleSide = TRUE) %>% 
  st_cast("POLYGON") %>% 
  st_transform(crs=4326) %>% 
  mutate(area="20mile")

# Define special area between 15 and 20 mile between 24 and 25 degree latitude
special_area <-
  rmapshaper::ms_erase(target=twentymile, erase=fifteenmile) %>% 
  st_intersection(bb) %>% 
  mutate(area="extension to 20mile")
  
# ggplot() +
#   theme_bw()+
#   geom_sf(data=twentymile, colour="blue", fill="blue", alpha=0.5) +
#   geom_sf(data=fifteenmile, colour="red", fill="red", alpha=0.5) +
#   geom_sf(data=special_area, colour="green", fill="green", alpha=0.8) +
#   geom_sf(data=eez_border,  colour="black", size=1) 

# eez minus fifteen mile and minus special area
eez_offshore <- 
  rmapshaper::ms_erase(target=eez, erase=fifteenmile) %>% 
  rmapshaper::ms_erase(target=., erase=special_area) %>% 
  mutate(area="offshore eez")  

# ggplot() +
#   geom_sf(data=eez, fill="green", alpha=0.5) +
#   geom_sf(data=fifteenmile, colour="yellow", fill=NA) +
#   geom_sf(data=country, aes(fill=country), colour="black") +
#   geom_sf(data=sf::st_intersection(fifteenmile, bb), fill="purple") +
#   geom_sf(data=sph.sf)

# ggplot() + geom_sf(data=eez_border, fill="green", colour="red", alpha=0.5) + geom_sf(data=bb3, colour="blue", fill=NA)
# ggplot() + geom_sf(data=eez, fill="green", colour="red", alpha=0.5) + geom_sf(data=eez_border, fill="green", colour="black", size=1.0)
# ggplot() + geom_path(data=eez_border, aes(x=X, y=Y), fill="green", colour="red", alpha=0.5) 

morocco_zones <- 
  bind_rows(
    eez_offshore,
    fifteenmile,  
    special_area) 

save(morocco_zones, file=file.path(onedrive, "rdata/morocco_zones.RData"))
load(file=file.path(onedrive, "rdata/morocco_zones.RData"))

# ggplot() + geom_sf(data=morocco_zones, aes(fill=area), colour="black", alpha=0.5) 

# ggplot() +
#   theme_bw()+
#   geom_sf(data=sph.sf, size=0.2) +
#   geom_sf(data=morocco_zones, aes(fill=area), alpha=0.4)
  





# bb <- sf::st_bbox(c(xmin = -40, xmax = 50, ymin = 45, ymax = 85), crs = st_crs(4326)) %>% sf::st_as_sfc()
# 
# iho.eez.na.sf %>% 
#   group_by(zone) %>% 
#   summarise() %>% 
#   sf::st_intersection(bb) %>% 
#   
#   ggplot() +
#   theme_bw() +
#   geom_sf(aes(fill=zone))

# ================================================================================
# Oman maps
# ================================================================================

load(file=file.path(onedrive,"rdata/eez.sf.RData"))
load(file=file.path(onedrive,"rdata/iho.eez.sf.RData"))
load(file=file.path(onedrive,"rdata/world.sf.RData"))

load(file.path(onedrive, "rdata/depth200.df.RData"))
load(file.path(onedrive, "rdata/depth1000.df.RData"))
load(file.path(onedrive, "rdata/depth2000.df.RData"))
load(file.path(onedrive, "rdata/depth5000.df.RData"))

myblues <- RColorBrewer::brewer.pal(9, "Blues")[2:9]

sf::sf_use_s2(FALSE)

bb  <- sf::st_bbox(c(xmin = 50, xmax = 63, ymin = 12, ymax = 28), crs = st_crs(4326)) %>% sf::st_as_sfc()

oman <-
  eez.sf %>% 
  filter(Sovereign1 == "Oman" | Sovereign2 == "Oman") %>% 
  sf::st_intersection(bb) 

oman <-
  world.sf %>% 
  filter(country == "Oman") %>% 
  sf::st_intersection(bb) 

# pol1 <- 
#   sf::st_polygon(list(matrix(c(52, 57, 60, 54, 52,
#                                17, 19, 15, 12, 17), ncol=2, byrow=FALSE))) %>% 
#   sf::st_sfc() %>% 
#   sf::st_set_crs(4326) 
# 
# pol2 <- 
#   sf::st_polygon(list(matrix(c(57, 59, 62, 60, 57,
#                                19, 21, 18, 15, 19), ncol=2, byrow=FALSE))) %>% 
#   sf::st_sfc() %>% 
#   sf::st_set_crs(4326) 
# 
# pol3 <- 
#   sf::st_polygon(list(matrix(c(59, 60, 55, 62, 64, 62, 59, 
#                                21, 22, 26, 24, 21, 18, 21), ncol=2, byrow=FALSE))) %>% 
#   sf::st_sfc() %>% 
#   sf::st_set_crs(4326) 

t      <- 
  st_sfc(
    sf::st_polygon(list(matrix(c(52, 57, 60, 54, 52,
                                 17, 19, 15, 12, 17), ncol=2, byrow=FALSE))), 
    sf::st_polygon(list(matrix(c(57, 59, 62, 60, 57,
                                 19, 21, 18, 15, 19), ncol=2, byrow=FALSE))),
    sf::st_polygon(list(matrix(c(59, 60, 55, 62, 64, 62, 59, 
                                 21, 22, 26, 24, 21, 18, 21), ncol=2, byrow=FALSE)))
  ) %>% 
  sf::st_set_crs(4326) 


twentymile <-
  oman %>% 
  st_cast("MULTILINESTRING") %>% 
  st_transform(crs=7801) %>% 
  st_buffer(dist = 20 * 1852, singleSide = TRUE) %>% 
  st_cast("POLYGON") %>% 
  st_transform(crs=4326) %>% 
  mutate(area=">20mile") %>% 
  sf::st_intersection(eez.sf) 


d <- 
  rmapshaper::ms_erase(target=eez.sf, erase=twentymile) %>% 
  sf::st_intersection(bb) %>% 
  sf::st_intersection(t) %>% 
  mutate(area = "outside 20 mile") %>% 
  filter(Sovereign1 == "Oman" | Sovereign2 == "Oman") %>% 
  dplyr::select(ID=MRGID, country=GeoName, area) %>% 
  bind_cols(region=c("south","centre","north"))

# cities
t <- maps::world.cities %>% filter(country.etc=="Oman", name %in% c("Muscat", "Dhofar","Musandum","Al Batinah", "Ash Sharqiyah","Al Wusta"))

# Oman map
# d %>% 
  # filter(grepl("Arabian|Oman|Persian", IHO_SEA)) %>% 
ggOceanMaps::basemap(bathymetry = TRUE, limits=c(c(52,63), c(13,27)), depth) +
  # ggplot() +
  theme_publication() +
  # theme(legend.position="none") +
  # geom_sf(aes(fill=ISO_TER1)) +
  
  geom_sf(data=d, aes(colour=region), alpha=0.3, inherit.aes = FALSE, show_legend=FALSE, fill="gray") +
  geom_sf_text(data=d, aes(colour=region, label=region), size=3, show_legend=FALSE) +
  
  geom_sf(data=world.sf, fill="cornsilk") +
  geom_sf_text(data=filter(world.sf, country=="Oman"), aes(label=country), size=3) +
  
  geom_point(data=t, aes(x=long, y=lat), size=1, colour="red") +
  geom_text(data=t, aes(x=long, y=lat, label=name), size=3, colour="red") +
  labs(x="",y="") +
  guides(colour = "none") +
  coord_sf(xlim=c(52,63), ylim=c(13,27))
