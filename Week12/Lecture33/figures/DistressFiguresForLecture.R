library(ggplot2)

# Load ma_map_data.rds
ma_map_data <- readRDS("ma_map_data.rds")

### Plot maps
ggplot(ma_map_data) +
  geom_sf(aes(fill = mental_distress), color = NA) +
  labs(
    title = "Frequent mental distress among adults (PLACES, 2022)",
    fill  = "% distressed"
  ) +
  theme_minimal()

ggplot(ma_map_data) +
  geom_sf(aes(fill = no_leisure_pa), color = NA) +
  labs(
    title = "No leisure-time physical activity (PLACES, 2022)",
    fill  = "% no LTPA"
  ) +
  theme_minimal()

### Regression
# Load ma_mental_health_physical_activity_2022.csv
df_reg <- read.csv("ma_mental_health_physical_activity_2022.csv")

# Fit the linear model
mod <- lm(mental_distress ~ no_leisure_pa, data = df_reg)
summary(mod)

# Scatter with regression line
ggplot(df_reg, aes(x = no_leisure_pa, y = mental_distress)) +
  geom_point(alpha = 0.6) +
  labs(
    x = "% with no leisure-time physical activity",
    y = "% reporting frequent mental distress",
    title = "Regression of Mental Distress on Physical Inactivity",
    subtitle = "Massachusetts ZCTAs (PLACES 2022)"
  ) +
  theme_minimal(base_size = 13)



