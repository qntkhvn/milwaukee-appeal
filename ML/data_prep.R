library(tidyverse)
library(janitor)
library(scales)
library(lubridate)
theme_set(theme_light()) 

library(here)
path <- "Desktop/consulting/project"
load(here(path, "consulting_data.RData"))
load(here(path, "location_data.RData"))
# load(here(path, "nbhd_map.RData"))

geo_cleaned <- read_csv("https://www.dropbox.com/s/9jcsk1477qc4k60/geo_cleaned.csv?dl=1") %>% 
  mutate(prop_id = as.character(prop_id),
         zip = as.character(zip))

# properties in the main dataset but are not in the location data
# 207147 207148 212702 212704 222335 224675
final$PROP_ID[!final$PROP_ID %in% final_loc$PROP_ID]

# fill in missing info based on properties nearby

# props close to 207147, 207148
# housing %>% 
#   select(prop_id, zip, lat, long) %>% 
#   filter(str_detect(prop_id, "^2071"))
# 
# # props close to 212702, 212704
# housing %>% 
#   select(prop_id, zip, lat, long) %>% 
#   filter(str_detect(prop_id, "^21270"))
#   
# # props close to 222335
# housing %>% 
#   select(prop_id, zip, lat, long) %>% 
#   filter(str_detect(prop_id, "^22233"))
# 
# # props close to 224675
# housing %>% 
#   select(prop_id, zip, lat, long) %>% 
#   filter(str_detect(prop_id, "^22467"))

geo_miss <- tibble(
  prop_id = c("207147", "207148", "212702", "212704", "222335", "224675"),
  zip = c("53204", "53204", "53215", "53215", "53215", "53215"),
  lat = c(43.024, 43.024, 43.0106, 43.0106, 42.997, 42.9978),
  long = c(-87.91378, -87.91378, -87.948, -87.948, -87.94965, -87.9259)
)

geo_final <- geo_cleaned %>% 
  bind_rows(geo_miss)


# cleaning by column order
housing <- final %>%
  clean_names() %>%
  mutate(
    
    prop_id = as.character(prop_id),
    
    bld_type = str_remove(bld_type, ".*- "),
    
    appraiser = str_sub(appraiser, start = 7),
    
    nbhd = str_sub(nbhd, 1, 4),
    
    qual = ifelse(str_detect(qual, "M"), "C", str_remove(qual, " -.*")),
    
    cond = str_remove(cond, ".*- "),
    
    kitchen_rating = str_remove(kitchen_rating, ".*- "),
    kitchen_rating = ifelse(kitchen_ct == 0, NA, kitchen_rating),
    
    
    full_bath_ct = ifelse(is.na(full_bath_ct), 0, full_bath_ct),
    full_bath_rating = str_remove(full_bath_rating, ".*- "),
    full_bath_rating = ifelse(full_bath_ct == 0, NA, full_bath_rating),
    
    half_bath_ct = ifelse(is.na(half_bath_ct), 0, half_bath_ct),
    half_bath_rating = str_remove(half_bath_rating, ".*- "),
    half_bath_rating = ifelse(half_bath_ct == 0, NA, half_bath_rating),
    
    year_built = ifelse(year_built == 0, NA, year_built),
    
    finished_area = ifelse(finished_area == 0, NA, finished_area),
    
    land_sf = ifelse(land_sf == 0, NA, land_sf),
    
    #sale_month = ifelse(is.na(sale_date), 0, month(sale_date)), # month
    sale_year = ifelse(is.na(sale_date), 0, year(sale_date)), # year
    # sale_wday = ifelse(is.na(sale_date), 0, wday(sale_date)), # day of the week
    # sale_week = ifelse(is.na(sale_date), 0, week(sale_date)), # week number (1-52)
    # sale_year = ifelse(sale_year == 2021 & sale_month > 8, 2020, sale_year),
    
    sale_status = ifelse(sale_year < 2020, 1, 0),
    
    # sale_price = ifelse(is.na(sale_price), 0, sale_price),
    sale_price = case_when(
      sale_price <= 100000 ~ "<100k",
      sale_price > 100000 & sale_price <= 150000 ~ "100-150k",
      sale_price > 150000 & sale_price <= 200000 ~"150-200k",
      sale_price > 200000 ~ ">200k",
      is.na(sale_price) ~ "no_sale"),   # several sale price of 0
    
    appealed19 = ifelse(is.na(appealed19), 0, 1), 
    appealed20 = ifelse(is.na(appealed20), "no", "appealed"),
    appealed21 = ifelse(is.na(appealed21), "no", "yes"),
    
  ) %>% 
  left_join(geo_final, by = "prop_id") # finally, joined with latitude/longitude data


# ordinal encoding
rating_levels <- c("Excellent", "Very Good", "Good", "Average", 
                   "Fair", "Poor", "Very Poor", "Unsound")
quality <- tibble(
  qual = c("AA+", "AA", "AA-", "A+", "A", "A-", "B+", "B", "B-", 
           "C+", "C", "C-", "D+", "D", "D-", "E+", "E", "E-"),
  qual_score = c(2.75, 2.63, 2.5, 2.35, 2, 1.65, 1.34, 1.14, 1.075, 1.04, 
                 1.015, 0.985, 0.955, 0.925, 0.895, 0.65, 0.55, 0.45))

raw <- housing %>% 
  left_join(quality) %>% 
  mutate(qual_score = ifelse(is.na(qual_score), 0, qual_score), 
         cond = as.numeric(factor(cond, levels = rev(rating_levels))),
         cond = ifelse(is.na(cond), 0, cond),
         kitchen_rating = as.numeric(factor(kitchen_rating, levels = rev(rating_levels))),
         kitchen_rating = ifelse(is.na(kitchen_rating), 0 , kitchen_rating),
         full_bath_rating = as.numeric(factor(full_bath_rating, levels = rev(rating_levels))),
         full_bath_rating = ifelse(is.na(full_bath_rating), 0 , full_bath_rating),
         half_bath_rating = as.numeric(factor(half_bath_rating, levels = rev(rating_levels))),
         half_bath_rating = ifelse(is.na(half_bath_rating), 0 , half_bath_rating),
         finished_area = log1p(finished_area),
         land_sf = log1p(land_sf)) %>% 
  select(-sale_date, -sale_price, -appealed21, -sale_year, -qual, -address, -geo_tract) %>%
  mutate(across(where(is.character), factor))


# how many are still missing?
# only 9
raw %>% 
  anti_join(drop_na(raw))

# 2 with missing years
# fill in with mode
# new function shortcut
get_mode <- \(v) unique(v)[which.max(tabulate(match(v, unique(v))))]
mode_year <- get_mode(raw$year_built)


# missing finished and land sf
# fill in with mean
avg_fa <- mean(raw$finished_area, na.rm = TRUE)
avg_lsf <- mean(raw$land_sf, na.rm = TRUE)

raw <- raw %>% 
  mutate(year_built = ifelse(is.na(year_built), mode_year, year_built),
         finished_area = ifelse(is.na(finished_area), avg_fa, finished_area),
         land_sf = ifelse(is.na(land_sf), avg_lsf, land_sf),
         zip = fct_lump_min(zip, min = 1000),
         bld_type = fct_lump_min(bld_type, min = 1000)) %>% 
  select(-prop_id)