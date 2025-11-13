library(reticulate)
library(dplyr)
library(readr)

# Use the correct Python environment if needed
use_condaenv("your_conda_environment", required = TRUE)

# Import pykakasi for Romaji conversion
pykakasi <- import("pykakasi")

# Alias the convert function for convenience
convert <- pykakasi$kakasi()$convert

# Convert the Japanese words to Romaji
convert_to_romaji <- function(japanese_text) {
  sapply(japanese_text, function(x) {
    result <- convert(x)
    if (length(result) > 0) {
      return(result[[1]]$hepburn)  # Get the Hepburn Romaji representation
    }
    return(NA)
  })
}

# Create the dictionary
japanese_CDI_romaji <- japanese_instrument_data %>%
  distinct(item_definition) %>%
  mutate(romaji = convert_to_romaji(item_definition))

# Write the dictionary to a CSV file
write.csv(japanese_CDI_romaji , "norms/japanese/japanese_CDI_romaji .csv", row.names = FALSE)

