#Green areas analysis
#Date: October, 2025
#Objective: Calculate the share of green areas in Lima by district

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Parameters
library(sf)
library(dplyr)
library (tidyverse)
library(units)
library(rmapshaper)
library(leaflet)

rm(list = ls())
setwd(here::here())

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------
# 1. Paths
# ---------------------------------------------------------------------
#Peru districts
district_path <- "./00_raw_data/geo/per/DISTRITO.gpkg"
#Lima province polygon
lima_v1 <- "./02_intermediate_data/green/per/lima_v1.gpkg"
#Green areas in Lima
green_v1 <- "./01_clean_data/green/per/green_areas_lima.gpkg"

outdir <- "./02_intermediate_data/green/per/"

# ---------------------------------------------------------------------
# 2. Load data
# ---------------------------------------------------------------------
district_all <- st_read(district_path, quiet = TRUE)
lima_prov <- st_read(lima_v1, quiet = TRUE)
green_lp <- st_read(green_v1, quiet = TRUE)

summary(district_all)
summary(lima_prov)
summary(green_lp)

#Transform to suitable projection for Lima (UTM Zone 18S) to ensure meters for area calculation
proj_crs <- 32718  # WGS 84 / UTM zone 18S
district_all <- st_transform(district_all, proj_crs)
lima_prov <- st_transform(lima_prov, proj_crs)
green_lp <- st_transform(green_lp, proj_crs)

district_all |> map_dbl(~length(unique(.)))

# ---------------------------------------------------------------------
# 3. Keep only Lima province districts
# ---------------------------------------------------------------------
# Intersect with our polygon for Lima
district_lima <- district_all |> 
  st_transform(st_crs(lima_prov)) |>
  st_make_valid() |>
  st_intersection(lima_prov)

# Filter Lima province and Callao
district_lima <- district_all |>
  filter(NOMBPROV %in% c("LIMA", "CALLAO"))

table(district_lima$NOMBPROV)

cat("Number of districts in Lima Province:", nrow(district_lima), "\n")

# ---------------------------------------------------------------------
# 4. Compute total area of each LSOA
# ---------------------------------------------------------------------
district_lima$area_total_m2 <- as.numeric(st_area(district_lima))

#Calculate total area in Lima to check figures are correct
area_gl <- district_lima |>
  summarise(
    across(c(
      area_total_m2),
      ~sum(., na.rm = TRUE)
    )) |>
  as.data.frame() |>
  select(-geom)

# ---------------------------------------------------------------------
# 5. Intersect green areas with districts
# ---------------------------------------------------------------------
cat("Computing intersection of districts with green areas in Lima province \n")

district_green <- st_intersection(st_make_valid(district_lima), st_make_valid(green_lp))
#Compute green areas by LSOA
district_green$area_green_m2 <- as.numeric(st_area(district_green))

green_gl <- district_green |>
  summarise(
    across(c(
      area_green_m2),
      ~sum(., na.rm = TRUE)
    )) |>
  as.data.frame() |>
  select(-geom)

district_green |> map_dbl(~length(unique(.)))
district_green |> map_dbl(~sum(is.na(.)))

table(district_green$CCDI)
table(district_green$NOMBDIST)

# Aggregate green area per district
green_summary <- district_green |>
  st_drop_geometry() |>
  group_by(NOMBDIST) |>
  summarise(area_green_m2 = sum(area_green_m2, na.rm = TRUE))

# ---------------------------------------------------------------------
# 6. Join back to LSOA polygons and compute share
# ---------------------------------------------------------------------
district_lima <- district_lima |>
  left_join(green_summary, by = "NOMBDIST") |>
  mutate(
    area_green_m2 = replace_na(area_green_m2, 0),
    share_green = 100 * area_green_m2 / area_total_m2
  )

summary(district_lima$share_green)

# ---------------------------------------------------------------------
# 7. Save output
# ---------------------------------------------------------------------

outpath <- paste0(outdir, "lima_district_green_share.gpkg")

st_write(district_lima, outpath,
         delete_dsn = TRUE)

st_write(
  district_lima,
  "./02_intermediate_data/green/per/lima_district_green_share.gpkg",
  delete_dsn = TRUE
)
