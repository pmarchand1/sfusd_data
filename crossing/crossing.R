library(tidyverse)
library(readxl)
library(tidygeocoder)
library(sf)
library(mapview)

# Prepare data and geocode

fname <- "List of Intersections - CG Sunshine Request.xlsx"

cross_dates <- excel_sheets(fname)
cross_list <- map(cross_dates, ~ read_excel(fname, sheet = .)[[1]])
names(cross_list) <- cross_dates

# Fix variants for names
fix_names <- function(s) {
    str_replace_all(s, c("Street$" = "St", "24h" = "24th", "23rd$" = "23rd St",
                         " #1" = "", "Bacon & Bayshore" = "Phelps & Bayshore",
                         "Corbett & John's Way" = "565 Corbett"))
}
cross_list <- map(cross_list, fix_names)

all_addr <- unique(unlist(cross_list))

setdiff(cross_list[[1]], cross_list[[3]])

all_addr <- tibble(addr = all_addr)
all_addr <- mutate(all_addr, addr = paste0(addr, ", San Francisco, CA"))
all_addr2 <- geocode(all_addr, address = addr, method = "census")

na_addr <- filter(all_addr2, is.na(lat)) %>% select(addr)
na_addr <- map_df(1:nrow(na_addr), ~geocode(na_addr[.,], address = addr, method = "census"))

all_addr2 <- bind_rows(filter(all_addr2, !is.na(lat)), na_addr)

all_addr <- mutate(all_addr2, addr = str_replace(addr, ", San Francisco, CA", "")) %>% 
    mutate(end_2425 = addr %in% cross_list$`Last Week of School Year 24-25`,
           start_2526 = addr %in% cross_list$`First Week of School Year 25-26`,
           sept16_25 = addr %in% cross_list$`As of 9-16-25`) 

all_addr <- mutate(all_addr,
    status = if_else(end_2425, 
                     if_else(start_2526, 
                            if_else(sept16_25, "unchanged", "removed sept."),
                            if_else(sept16_25, "reinstated", "removed aug.")
                     ),
                     if_else(start_2526, "added aug.", "added sept.")
    )
)

all_addr <- st_as_sf(all_addr, coords = c("long", "lat"), crs = 4326)

mapview(all_addr, zcol = "status", 
        col.regions = c("green", "darkgreen", "yellow", "darkred", "red", "grey"))
