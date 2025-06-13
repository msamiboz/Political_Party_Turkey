library(tidyverse)
library(tmaptools)

# Load your data
data <- read_csv("Party_data1.csv")

# Function to clean and simplify Turkish addresses for better geocoding
clean_turkish_address <- function(address) {
  if(is.na(address) || address == "") return(NA)
  
  # Convert to uppercase for consistent processing
  addr <- toupper(address)
  
  # Remove common Turkish address prefixes/suffixes that confuse geocoding
  addr <- gsub("MAH\\.|MAHALLESI|MAHALLESİ", "mahallesi", addr, ignore.case = TRUE)
  addr <- gsub("SK\\.|SOKAK|SOKAGI", "sokak", addr, ignore.case = TRUE)
  addr <- gsub("CAD\\.|CADDE|CADDESİ", "caddesi", addr, ignore.case = TRUE)
  addr <- gsub("NO:|NO\\.|NUMARA", "", addr, ignore.case = TRUE)
  
  # Extract main components: try to get district/city and main street/neighborhood
  # Look for city pattern (usually after / or at the end)
  city_match <- str_extract(addr, "([A-ZÇĞIİÖŞÜ]+/[A-ZÇĞIİÖŞÜ]+|[A-ZÇĞIİÖŞÜ]+)$")
  
  # Extract neighborhood (mahallesi)
  mahalle_match <- str_extract(addr, "[A-ZÇĞIİÖŞÜ\\s]+(?=\\s+mahallesi)")
  
  # Extract main street (sokak or caddesi)
  street_match <- str_extract(addr, "[A-ZÇĞIİÖŞÜ0-9\\s]+(?=\\s+(sokak|caddesi))")
  
  # Create simplified address
  simplified_parts <- c()
  
  if(!is.na(city_match)) {
    simplified_parts <- c(simplified_parts, city_match)
  }
  
  if(!is.na(mahalle_match)) {
    simplified_parts <- c(simplified_parts, paste(mahalle_match, "mahallesi"))
  }
  
  if(!is.na(street_match)) {
    simplified_parts <- c(simplified_parts, paste(street_match, "sokak"))
  }
  
  # If we couldn't parse well, try a different approach
  if(length(simplified_parts) == 0) {
    # Just take city/district part
    if(!is.na(city_match)) {
      return(city_match)
    } else {
      # Last resort: take last two words which are often city/district
      words <- str_split(addr, "\\s+")[[1]]
      if(length(words) >= 2) {
        return(paste(tail(words, 2), collapse = " "))
      }
    }
  }
  
  return(paste(simplified_parts, collapse = ", "))
}

# Interactive function to test address cleaning and geocoding
test_address_geocoding <- function() {
  cat("Turkish Address Geocoding Tester\n")
  cat("Type 'quit' to exit\n\n")
  
  while(TRUE) {
    # Get user input
    cat("Enter a Turkish address to test: ")
    user_address <- readline()
    
    if(tolower(user_address) == "quit") {
      break
    }
    
    if(user_address == "") {
      next
    }
    
    cat("\n--- Testing Address ---\n")
    cat("Original:", user_address, "\n")
    
    # Clean the address
    cleaned <- clean_turkish_address(user_address)
    cat("Cleaned: ", cleaned, "\n")
    
    # Try geocoding with original
    cat("\nTrying original address...\n")
    tryCatch({
      coords_orig <- tmaptools::geocode_OSM(user_address, as.data.frame = TRUE)
      if(!is.na(coords_orig$lat)) {
        cat("✓ Original worked! Lat:", coords_orig$lat, "Lon:", coords_orig$lon, "\n")
      } else {
        cat("✗ Original failed\n")
      }
    }, error = function(e) {
      cat("✗ Original failed with error:", e$message, "\n")
    })
    
    # Try geocoding with cleaned
    if(!is.na(cleaned)) {
      cat("Trying cleaned address...\n")
      tryCatch({
        coords_clean <- tmaptools::geocode_OSM(cleaned, as.data.frame = TRUE)
        if(!is.na(coords_clean$lat)) {
          cat("✓ Cleaned worked! Lat:", coords_clean$lat, "Lon:", coords_clean$lon, "\n")
        } else {
          cat("✗ Cleaned failed\n")
        }
      }, error = function(e) {
        cat("✗ Cleaned failed with error:", e$message, "\n")
      })
    }
    
    # Suggest manual alternatives
    cat("\nSuggested manual alternatives to try:\n")
    words <- str_split(toupper(user_address), "\\s+")[[1]]
    
    # Extract potential city/district
    potential_city <- tail(words, 2)
    cat("1. Just city/district:", paste(potential_city, collapse = " "), "\n")
    
    # Extract neighborhood if exists
    mah_idx <- which(grepl("MAH", words))
    if(length(mah_idx) > 0 && mah_idx[1] > 1) {
      neighborhood <- paste(words[max(1, mah_idx[1]-2):(mah_idx[1]-1)], collapse = " ")
      cat("2. Neighborhood + city:", neighborhood, "mahallesi,", paste(potential_city, collapse = " "), "\n")
    }
    
    cat("\n" , rep("-", 50), "\n\n")
  }
}

# Function to geocode all addresses with multiple strategies - FIXED VERSION
geocode_all_addresses <- function(data) {
  results <- data %>%
    mutate(
      # Strategy 1: Original address
      coords_orig = map(Address, ~{
        if(is.na(.x) || .x == "") return(list(lat = NA, lon = NA, success = FALSE))
        tryCatch({
          coords <- tmaptools::geocode_OSM(.x, as.data.frame = TRUE)
          # Handle case where geocode_OSM returns empty data frame
          if(nrow(coords) == 0 || is.null(coords$lat) || length(coords$lat) == 0) {
            return(list(lat = NA, lon = NA, success = FALSE))
          }
          list(lat = coords$lat[1], lon = coords$lon[1], success = !is.na(coords$lat[1]))
        }, error = function(e) {
          list(lat = NA, lon = NA, success = FALSE)
        })
      }),
      
      # Strategy 2: Cleaned address
      cleaned_address = map_chr(Address, ~{
        result <- clean_turkish_address(.x)
        if(is.null(result)) return(NA_character_)
        return(result)
      }),
      
      coords_clean = map(cleaned_address, ~{
        if(is.na(.x) || .x == "") return(list(lat = NA, lon = NA, success = FALSE))
        tryCatch({
          coords <- tmaptools::geocode_OSM(.x, as.data.frame = TRUE)
          # Handle case where geocode_OSM returns empty data frame
          if(nrow(coords) == 0 || is.null(coords$lat) || length(coords$lat) == 0) {
            return(list(lat = NA, lon = NA, success = FALSE))
          }
          list(lat = coords$lat[1], lon = coords$lon[1], success = !is.na(coords$lat[1]))
        }, error = function(e) {
          list(lat = NA, lon = NA, success = FALSE)
        })
      }),
      
      # Extract coordinates with safe extraction
      lat_orig = map_dbl(coords_orig, ~{
        if(is.null(.x$lat) || length(.x$lat) == 0) return(NA_real_)
        return(.x$lat)
      }),
      lon_orig = map_dbl(coords_orig, ~{
        if(is.null(.x$lon) || length(.x$lon) == 0) return(NA_real_)
        return(.x$lon)
      }),
      success_orig = map_lgl(coords_orig, ~{
        if(is.null(.x$success) || length(.x$success) == 0) return(FALSE)
        return(.x$success)
      }),
      
      lat_clean = map_dbl(coords_clean, ~{
        if(is.null(.x$lat) || length(.x$lat) == 0) return(NA_real_)
        return(.x$lat)
      }),
      lon_clean = map_dbl(coords_clean, ~{
        if(is.null(.x$lon) || length(.x$lon) == 0) return(NA_real_)
        return(.x$lon)
      }),
      success_clean = map_lgl(coords_clean, ~{
        if(is.null(.x$success) || length(.x$success) == 0) return(FALSE)
        return(.x$success)
      }),
      
      # Use best available coordinates
      lat = ifelse(success_orig, lat_orig, ifelse(success_clean, lat_clean, NA)),
      lon = ifelse(success_orig, lon_orig, ifelse(success_clean, lon_clean, NA)),
      geocode_success = success_orig | success_clean,
      geocode_method = case_when(
        success_orig ~ "original",
        success_clean ~ "cleaned",
        TRUE ~ "failed"
      )
    ) %>%
    select(-coords_orig, -coords_clean, -lat_orig, -lon_orig, -lat_clean, -lon_clean, 
           -success_orig, -success_clean)
  
  return(results)
}

# Test the problematic address from your example
cat("Testing your example address:\n")
test_addr <- "MÜRSEL ULUÇ MAH. 956 SK. NO: 8 / 7 ÇANKAYA/ANKARA"
cleaned_test <- clean_turkish_address(test_addr)
cat("Original:", test_addr, "\n")
cat("Cleaned:", cleaned_test, "\n")

# Run the interactive tester
cat("\nStarting interactive address tester...\n")
test_address_geocoding()

# Add coordinates to data
data$lat <- coords$lat
data$lon <- coords$lon
data$geocode_success <- !is.na(coords$lat)

# Save the geocoded data
write_csv(data, "Party_data_geocoded.csv")

