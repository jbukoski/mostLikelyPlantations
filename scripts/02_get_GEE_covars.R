
library(geojsonio)
library(rgee)
library(sf)

ee_Authenticate()

ee_Initialize()

set.seed(123)
n_points <- 100
lon <- runif(n_points, min = -122.5, max = -122.0)
lat <- runif(n_points, min = 37.5, max = 38.0)
points_df <- data.frame(lon = lon, lat = lat)

# Convert to sf object
points_sf <- st_as_sf(points_df, coords = c("lon", "lat"), crs = 4326)

# Convert to Earth Engine FeatureCollection
ee_points <- sf_as_ee(points_sf)



image <- ee$Image("projects/vivid-vent-313422/assets/predictorVars/all_predictors")

# Sample the image at point locations
sampled <- image$sampleRegions(
  collection = ee_points,
  scale = 30,  # adjust scale based on your needs
  geometries = TRUE
)

# Convert back to R
sampled_sf <- ee_as_sf(sampled)


sampled_random <- image$sample(
  region = image$geometry(),
  scale = 30,
  numPixels = 1000,  # number of sample points
  seed = 123,
  geometries = TRUE
)

# Convert to R
sampled_random_sf <- ee_as_sf(sampled_random)
