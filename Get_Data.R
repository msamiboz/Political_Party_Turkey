# Load necessary libraries
library(tidyverse)  # For data manipulation and visualization
library(rvest)      # For web scraping
library(stringr)    # For string manipulation
library(httr)

setwd("~/Documents/GitHub/Political_Party_Turkey")

# Read the HTML document from the given URL
mainurl <- "http://www.yargitaycb.gov.tr/kategori/38/siyasi-parti-genel-bilgileri"
page <- GET(mainurl, config = config(ssl_verifypeer = FALSE))
document <- read_html(content(page, as = "text"))

# Extract all 'href' attributes from 'a' elements on the page
party_list <- document %>% html_elements("a") %>% html_attr("href")

# Filter the URLs to include only those containing "icerik"
party_list <- party_list[party_list %>% str_detect("icerik")]

# Define a function to convert "http" URLs to "https"
fix_http_to_https <- function(url) {
  if (grepl("^http://", url)) {
    url <- sub("^http://", "https://", url)
  }
  return(url)
}

# Apply the fix_http_to_https function to all URLs and remove names
party_list <- sapply(party_list, fix_http_to_https) %>% unname()

# Create an empty tibble to store the scraped data
res <- tibble(X1 = character(), X2 = character())

# Initialize a count variable
count = 0

# Loop through the list of party URLs
for (url in party_list) {
  # Skip a specific URL
  if (url == "https://www.yargitaycb.gov.tr/icerik/1093/siyasi-parti-genel-bilgileri") {
    next
  }
  
  # Print the current URL
  print(url)
  
  page <- GET(url, config = config(ssl_verifypeer = FALSE))
  content <- read_html(content(page, as = "text"))
  # Scrape the party name from the page
  party_name <- content %>% html_nodes("div.newstitle") %>% html_text()
  
  # Scrape the party table data from the page
  party_table <- content %>% html_table()
  
  
  # Combine the party name and table into a tibble and pivot it
  abc <- rbind(tibble(X1 = "Name", X2 = party_name), party_table[[1]][,1:2]) %>%
    pivot_wider(names_from = X1, values_from = X2)
  possible_colnames <- c("Name","Başkanı","Kuruluş Tarihi",
                         "Kuruluş Tarihi","Telefon","Telfon",
                         "Adres","Üye Sayısı","BAŞKANI")
  if (length(abc)>6){
    abc <- abc[,colnames(abc) %in% possible_colnames]
  }
  
  
  # Rename the columns
  colnames(abc) <- c("Party_Name", "Chair", "Founded", "Telephone", "Address", "Member_Count")
  
  # Append the data to the result tibble
  res <- rbind(res, abc)
  
  # Increment the count
  count = count + 1
}

# Convert Founded to Date format and Member_Count to numeric
res <- res %>% mutate(Founded = as.Date(Founded, format = "%d.%m.%Y"),
                      Member_Count = as.numeric(gsub("\\.", "", Member_Count)))

# Write the data to a CSV file
write_csv(res, "Party_data1.csv")
