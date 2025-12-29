
library(rgee)
library(sf)
library(terra)
library(tidyverse)

gdb_path <- "~/Documents/datasets/sdpt_v3_final_gfw_only.gdb/"

layers_info <- st_layers(gdb_path)

layers_df <- data.frame(
  name = layers_info$name,
  features = layers_info$features,
  fields = layers_info$fields,
  stringsAsFactors = FALSE
)

layers_df %>%
  arrange(-features)

#--------------------
# Figure out which layers have genera relevant to the mapping exercise

# Get all layer names
layer_names <- st_layers(gdb_path)$name

# Initialize empty list to store results
species_by_layer <- list()

# Loop through each layer
for (layer in layer_names) {
  
  tryCatch({
    # Read the layer
    data <- st_read(gdb_path, layer = layer, quiet = TRUE)
    
    # Check if sciName column exists
    if ("sciName" %in% names(data)) {
      # Get unique species, remove NAs
      species_list <- unique(data$sciName)
      species_list <- species_list[!is.na(species_list)]
    } else {
      species_list <- "No sciName column"
    }
    
    # Store in list
    species_by_layer[[layer]] <- species_list
    
    # Print progress
    cat("Processed:", layer, "- Found", length(species_list), "unique species\n")
    
  }, error = function(e) {
    species_by_layer[[layer]] <<- paste("Error:", e$message)
    cat("Error processing:", layer, "\n")
  })
}

# Combine in a data frame

species_df <- enframe(species_by_layer, name = "layer", value = "sciName") %>%
  unnest(sciName)

species_df_wide <- species_df %>%
  separate(sciName, 
           into = paste0("species_", 1:10),  # Creates species_1, species_2, etc.
           sep = " \\| ",
           fill = "right",   # Fill missing values with NA
           remove = FALSE)   # Keep original column

print(species_df_wide)

#-------------------------
# Filter out uncommon genera
# Select country layers that have training data for top 17 genera

priorityGenera <- read_csv("./data/top_genera.csv")$genus

filtered_df <- species_df_wide %>%
  filter(is.na(species_2)) %>% # Get all single species plantations
  filter(species_1 != "Unknown") %>%
  separate(species_1, into = c("genus", "species"), sep = " ",
           extra = "merge", remove = FALSE) %>%
  select(layer, genus) %>%
  distinct(layer, genus) %>%
  mutate(priority = genus %in% priorityGenera)

priority_layers <- filtered_df %>%
  group_by(layer) %>%
  filter(any(priority == TRUE)) %>%
  ungroup() %>%
  arrange(layer) %>%
  pull(layer) %>%
  unique()
  
priority_layers

#----------------------------
# Combine into one spatial object

writeDir <- "./data/filteredSDPT/"

for(layer in priority_layers) {
  
  print(paste0("Processing ", layer, "..."))
  
  data <- st_read(gdb_path, layer = layer) %>%
    filter(is.na(sciName2)) %>%   # filter to just monocultures
    separate(sciName1, into = c("genus", "species"), sep = " ",
             extra = "merge", remove = FALSE) %>%
    filter(genus %in% priorityGenera) %>%
    mutate(source_layer = layer)
  
  write_sf(data, paste0(writeDir, layer, ".shp"), driver = "ESRI Shapefile")
  
}

#------------------------------

# Path to shapefile directory
shp_dir <- "./data/filteredSDPT/"
shp_files <- list.files(shp_dir, pattern = "\\.shp$", full.names = TRUE)

# Read all shapefiles and combine
all_data <- lapply(shp_files, function(shp) {
  print(paste0("processing ... ", shp))
  data <- st_read(shp, quiet = TRUE)
  
  # Check CRS and reproject if not EPSG:4326
  if (st_crs(data) != st_crs(4326)) {
    print(paste0("  reprojecting from ", st_crs(data)$input, " to EPSG:4326"))
    data <- st_transform(data, crs = 4326)
  } else {
    print("  already in EPSG:4326")
  }
  
  data$source_file <- basename(shp)
  return(data)
}) %>%
  bind_rows()

unique_genera <- unique(all_data$genus)

# Output geopackage path
gpkg_path <- "./data/filteredSDPT/groupedByGenus.gpkg"

for (gen in unique_genera) {
  
  # Filter data for this genus
  genus_data <- all_data %>%
    filter(genus == gen)
  
  # Write to geopackage (each genus as a layer)
  st_write(genus_data, 
           dsn = gpkg_path, 
           layer = gen,
           append = TRUE,
           quiet = TRUE)
  
  cat("Added layer:", gen, "with", nrow(genus_data), "features\n")
}

#-----------------------------
# Extract n samples

# Parameters
gpkg_path <- "./data/filteredSDPT/groupedByGenus.gpkg"
n_samples <- 500  # Number of features to sample per layer

# Get all layers in the geopackage
layers <- st_layers(gpkg_path)$name

sf_use_s2(FALSE)

sampled_data <- lapply(layers, function(layer) {
  
  print(paste0("Processing: ", layer))
  
  tryCatch({
    data <- st_read(gpkg_path, layer = layer, quiet = TRUE)
    print(paste0("  Read ", nrow(data), " features"))
    
    if (nrow(data) == 0) {
      return(NULL)
    }
    
    # Sample indices only (very fast)
    n_to_sample <- min(n_samples, nrow(data))
    sample_indices <- sample(nrow(data), size = n_to_sample, replace = nrow(data) < n_samples)
    
    # Read only sampled features
    sampled <- data[sample_indices, ]
    print(paste0("  Sampled ", nrow(sampled), " features"))
    
    # Fix geometries
    sampled <- st_make_valid(sampled)
    
    # Generate random points
    random_points <- st_sample(sampled, size = rep(1, nrow(sampled)), type = "random")
    
    # Filter valid
    valid <- !st_is_empty(random_points)
    
    if (sum(valid) == 0) {
      return(NULL)
    }
    
    sampled_points <- sampled[valid, ]
    st_geometry(sampled_points) <- random_points[valid]
    sampled_points$layer <- layer
    
    print(paste0("  SUCCESS - ", nrow(sampled_points), " points"))
    return(sampled_points)
    
  }, error = function(e) {
    print(paste0("  ERROR: ", e$message))
    return(NULL)
  })
  
}) %>%
  purrr::compact() %>%
  bind_rows()

sf_use_s2(TRUE)

sampled_data

write_sf(sampled_data, "./data/firstSample.shp")

#------------------------------
# Extract covariates from the GEE image

ee_Initialize()

## Convert sf object to Earth Engine FeatureCollection
ee_points <- sf_as_ee(sampled_data)

# Load your GEE image (example with Landsat)
# Replace with your actual image
image <- ee$Image("projects/vivid-vent-313422/assets/predictorVars/all_predictors")

# Sample the image at point locations
sampled_values <- image$sampleRegions(
  collection = ee_points,
  scale = 1000,  # Resolution in meters (adjust based on your image)
  geometries = TRUE,  # Keep point geometries
  tileScale = 4  # Increase if you get memory errors
)

# Export to Drive instead of downloading directly
task <- ee_table_to_drive(
  collection = sampled_values,
  description = "plantation_samples",
  folder = "wri_plantations",
  fileFormat = "GPKG"
)

task$start()
ee_monitoring(task)

# View results
View(result_sf)

