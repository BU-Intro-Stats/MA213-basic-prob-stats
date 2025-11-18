library(tidyverse)
library(sf)
library(tigris)   # for state + ZCTA shapes
options(tigris_use_cache = TRUE)

# This file came from: https://data.cdc.gov/500-Cities-Places/PLACES-Local-Data-for-Better-Health-ZCTA-Data-2024/qnzd-25i4/explore/query/SELECT%0A%20%20%60year%60%2C%0A%20%20%60locationname%60%2C%0A%20%20%60datasource%60%2C%0A%20%20%60category%60%2C%0A%20%20%60measure%60%2C%0A%20%20%60data_value_unit%60%2C%0A%20%20%60data_value_type%60%2C%0A%20%20%60data_value%60%2C%0A%20%20%60data_value_footnote_symbol%60%2C%0A%20%20%60data_value_footnote%60%2C%0A%20%20%60low_confidence_limit%60%2C%0A%20%20%60high_confidence_limit%60%2C%0A%20%20%60totalpop18plus%60%2C%0A%20%20%60totalpopulation%60%2C%0A%20%20%60geolocation%60%2C%0A%20%20%60locationid%60%2C%0A%20%20%60categoryid%60%2C%0A%20%20%60measureid%60%2C%0A%20%20%60datavaluetypeid%60%2C%0A%20%20%60short_question_text%60/page/filter
places_long <- read_csv("PLACES__Local_Data_for_Better_Health,_ZCTA_Data_2024_release_20251117.csv",
                        show_col_types = FALSE)

places_long <- places_long %>%
  mutate(
    LocationID = as.character(LocationID),
    MeasureId  = as.character(MeasureId)
  )

# 2. Filter to the two measures you care about: MHLTH and LPA
places_two <- places_long %>%
  filter(
    Year == 2022,
    Data_Value_Type == "Crude prevalence",
    MeasureId %in% c("MHLTH", "LPA")
  ) %>%
  select(LocationID, Geolocation, MeasureId, Data_Value)

# 3. Turn the WKT "Geolocation" into sf POINTs, CRS = 4326
places_points <- places_two %>%
  distinct(LocationID, Geolocation) %>%
  st_as_sf(wkt = "Geolocation", crs = 4326)

# 4. Get MA state polygon, transform to match points' CRS
states_sf <- tigris::states(cb = TRUE, year = 2022) %>%
  st_transform(st_crs(places_points))

ma_sf <- states_sf %>% filter(STUSPS == "MA")

# 5. Which LocationIDs fall inside MA?
places_ma_ids <- st_join(places_points, ma_sf, join = st_within, left = FALSE) %>%
  st_drop_geometry() %>%
  pull(LocationID)

# 6. Keep only MA rows and go wide: one row per ZCTA with MHLTH + LPA
places_ma_wide <- places_two %>%
  filter(LocationID %in% places_ma_ids) %>%
  select(LocationID, MeasureId, Data_Value) %>%
  pivot_wider(
    names_from = MeasureId,
    values_from = Data_Value
  ) %>%
  rename(
    mental_distress = MHLTH,
    no_leisure_pa   = LPA
  )

# 7. Find the right field for the LocationID
zcta_sf <- tigris::zctas(cb = TRUE, year = 2020) %>%
  st_transform(st_crs(places_points))
zcta_sf <- zcta_sf %>%
  mutate(LocationID = ZCTA5CE20)

# 8. Join shapes to MA data
ma_map_data <- zcta_sf %>%
  inner_join(places_ma_wide, by = "LocationID")

# Save the map data for future instructors
saveRDS(ma_map_data, "ma_map_data.rds")

# Drop geometry for modeling
df_reg <- ma_map_data %>%
  st_drop_geometry() %>%
  drop_na(mental_distress, no_leisure_pa)

# Save the simplified data as csv for R demos and students
write_csv(df_reg, "ma_mental_health_physical_activity_2022.csv")
