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

class(layers)

#--------------------

library(sf)


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

# View results
print(species_by_layer)


species_df <- enframe(species_by_layer, name = "layer", value = "sciName") %>%
  unnest(sciName)

species_df_wide <- species_df %>%
  separate(sciName, 
           into = paste0("species_", 1:10),  # Creates species_1, species_2, etc.
           sep = " \\| ",
           fill = "right",   # Fill missing values with NA
           remove = FALSE)   # Keep original column

print(species_df_wide)
