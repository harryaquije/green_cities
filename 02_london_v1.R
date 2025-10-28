#Green areas analysis
#Date: October, 2025
#Objective: Calculate the share of green areas in London by LSOA

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Parameters
library(sf)
library(dplyr)
library(tidyverse)
library(units)
library(rmapshaper)
library(leaflet)

rm(list = ls())
setwd(here::here())

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------
# 1. Paths
# ---------------------------------------------------------------------
#Load LSOA 2021 (BGC)
#https://geoportal.statistics.gov.uk/datasets/ons::lower-layer-super-output-areas-december-2021-boundaries-ew-bgc-v5-2/explore
lsoa_path <- "./00_raw_data/geo/uk/Lower_layer_Super_Output_Areas_December_2021_Boundaries_EW_BGC_V5_-6970154227154374572.gpkg"
combined_v2 <- "./02_intermediate_data/green/gbr/london_osm_greenbelt_v2.gpkg"
outdir <- "./02_intermediate_data/green/gbr/"

#Greater London
#Projected CRS
greater_london_v2 <- "./02_intermediate_data/green/gbr/greater_london_v2.gpkg"

# ---------------------------------------------------------------------
# 2. Load data
# ---------------------------------------------------------------------
lsoa_all <- st_read(lsoa_path, quiet = TRUE)
combined_london <- st_read(combined_v2, quiet = TRUE)
greater_london <- st_read(greater_london_v2, quiet = TRUE)

summary(lsoa_all)
summary(combined_london)
summary(greater_london)

# Transform to BNG (EPSG:27700) to ensure meters for area calculation
#The three polygons already have epsg: 27700

#proj_crs <- 27700
#lsoa_all <- st_transform(lsoa_all, proj_crs)
#combined_london <- st_transform(combined_london, proj_crs)

# ---------------------------------------------------------------------
# 3. Keep only Greater London LSOAs
# ---------------------------------------------------------------------
# Intersect with our polygon for Greater London
lsoa_london <- lsoa_all |> 
  st_transform(st_crs(greater_london)) |>
  st_make_valid() |>
  st_intersection(greater_london)

cat("Number of LSOAs in Greater London:", nrow(lsoa_london), "\n")
#Number of LSOAs in Greater London: 5127

# ---------------------------------------------------------------------
# 4. Compute total area of each LSOA
# ---------------------------------------------------------------------
lsoa_london$area_total_m2 <- as.numeric(st_area(lsoa_london))

#Calculate total area in London to check figures are correct
area_gl <- lsoa_london |>
  summarise(
    across(c(
      area_total_m2),
      ~sum(., na.rm = TRUE)
    )) |>
  as.data.frame() |>
  select(-SHAPE)

# ---------------------------------------------------------------------
# 5. Intersect green areas with LSOAs
# ---------------------------------------------------------------------
cat("Computing intersection of LSOAs with green areas in Greater London \n")

lsoa_green <- st_intersection(st_make_valid(lsoa_london), st_make_valid(combined_london))
#Compute green areas by LSOA
lsoa_green$area_green_m2 <- as.numeric(st_area(lsoa_green))

green_gl <- lsoa_green |>
  summarise(
    across(c(
      area_green_m2),
      ~sum(., na.rm = TRUE)
    )) |>
  as.data.frame() |>
  select(-SHAPE)

lsoa_green |> map_dbl(~length(unique(.)))

# Aggregate green area per LSOA code
green_summary <- lsoa_green |>
  st_drop_geometry() |>
  group_by(LSOA21CD) |>
  summarise(area_green_m2 = sum(area_green_m2, na.rm = TRUE))

# ---------------------------------------------------------------------
# 6. Join back to LSOA polygons and compute share
# ---------------------------------------------------------------------
lsoa_london <- lsoa_london |>
  left_join(green_summary, by = "LSOA21CD") |>
  mutate(
    area_green_m2 = replace_na(area_green_m2, 0),
    share_green = 100 * area_green_m2 / area_total_m2
  )

summary(lsoa_london$share_green)

# ---------------------------------------------------------------------
# 7. Save output
# ---------------------------------------------------------------------

outpath <- paste0(outdir, "london_lsoa_green_share.gpkg")

st_write(lsoa_london, outpath,
         delete_dsn = TRUE)

st_write(
  lsoa_london,
  "./02_intermediate_data/green/gbr/london_lsoa_green_share.gpkg",
  delete_dsn = TRUE
)

cat("✅ Saved LSOA-level green share to:", outpath, "\n")
