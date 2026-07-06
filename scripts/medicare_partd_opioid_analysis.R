getwd()
setwd("/Users/josiahbarnes")
library(readxl)
install.packages("dplyr")
library(dplyr)
hl <- read.csv("~/Desktop/Capstone Documentation/Capstone Data/HL.csv", skip= 3)
dl <- read.csv("~/Desktop/Capstone Documentation/Capstone Data/DL.csv", skip= 3)
geo <- read.csv("~/Desktop/Capstone Documentation/Capstone Data/MUP_DPR_RY25_P04_V10_DY23_Geo.csv")
opioid_rate <- read.csv("~/Desktop/Capstone Documentation/Capstone Data/Prescribing Rates/OMT_MDCR_RY24_P04_V10_YTD22_GEO.csv")
opioid_data <- hl %>%
  filter(`Total.Claims.for.Opioid.Drugs` > 0,
         !is.na(`Total.Claims.for.Beneficiaries...Age.65..`))

colnames(opioid_rate)
library(dplyr)

# Filter to county-level data only
county_data <- opioid_rate %>%
  filter(Prscrbr_Geo_Lvl == "County")

# Check top counties by opioid prescribing rate
top_counties <- county_data %>%
  arrange(desc(Opioid_Prscrbng_Rate)) %>%
  select(Prscrbr_Geo_Desc, Opioid_Prscrbng_Rate) %>%
  head(10)
top_counties

# Check bottom counties by opioid prescribing rate
bottom_counties <- county_data %>%
  arrange(desc(Opioid_Prscrbng_Rate)) %>%
  select(Prscrbr_Geo_Desc, Opioid_Prscrbng_Rate) %>%
  head(10)
bottom_counties

#rate by year summary
rate_by_year <- county_data %>%
  group_by(Year) %>%
  summarize(avg_rate = mean(Opioid_Prscrbng_Rate, na.rm = TRUE))

print(rate_by_year)

#Opioid Prescribing Over Time Plot
install.packages("ggplot2")
library(ggplot2)
ggplot(county_data, aes(x = Year, y = Opioid_Prscrbng_Rate)) +
  geom_line(aes(group = Prscrbr_Geo_Desc), alpha = 0.1) +
  stat_summary(fun = mean, geom = "line", color = "red", size = 1.2) +
  labs(title = "Opioid Prescribing Rate Over Time (Avg and by County)",
       y = "Opioid Prescribing Rate", x = "Year")

#get mean rate: rural
rural_analysis <- county_data %>%
  mutate(area_type = ifelse(RUCA_Cd %in% c(1, 2, 3), "Urban", "Rural")) %>%
  group_by(area_type) %>%
  summarize(mean_rate = mean(Opioid_Prscrbng_Rate, na.rm = TRUE))

#filter county data by RucaCd: not work bc missing/incorrect values
county_data %>%
  mutate(area_type = ifelse(RUCA_Cd %in% c(1, 2, 3), "Urban", "Rural")) %>%
  count(area_type)
county_data$RUCA_Cd <- as.numeric(county_data$RUCA_Cd)
county_data %>%
  mutate(area_type = ifelse(RUCA_Cd %in% c(1, 2, 3), "Urban", "Rural")) %>%
  count(area_type)
county_data %>%
  mutate(area_type = case_when(
    RUCA_Cd %in% c(1, 2, 3) ~ "Urban",
    RUCA_Cd %in% c(4:10) ~ "Rural",
    TRUE ~ "Unknown"
  )) %>%
  count(area_type)

#mean opioid prescribing rates: not work due to rural/urban unclear
ggplot(rural_analysis, aes(x = area_type, y = mean_rate, fill = area_type)) +
  geom_bar(stat = "identity") +
  labs(title = "Mean Opioid Prescribing Rate: Rural vs Urban",
       x = "Area Type", y = "Mean Prescribing Rate") +
  theme_minimal()
table(county_data$RUCA_Cd, useNA = "ifany")

#top county dataset
top_counties <- county_data %>%
  group_by(Prscrbr_Geo_Desc) %>%
  summarize(mean_rate = mean(Opioid_Prscrbng_Rate, na.rm = TRUE)) %>%
  arrange(desc(mean_rate)) %>%
  slice(1:10)

library(dplyr)

top_counties <- county_data %>%
  group_by(Prscrbr_Geo_Desc) %>%
  summarize(mean_rate = mean(Opioid_Prscrbng_Rate, na.rm = TRUE)) %>%
  arrange(desc(mean_rate)) %>%
  slice(1:10)

library(ggplot2)

ggplot(top_counties, aes(x = reorder(Prscrbr_Geo_Desc, mean_rate), y = mean_rate)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 10 Counties by Opioid Prescribing Rate",
       x = "County", y = "Mean Prescribing Rate") +
  theme_minimal()

rate_by_year <- county_data %>%
  group_by(Year) %>%
  summarize(avg_rate = mean(Opioid_Prscrbng_Rate, na.rm = TRUE)) %>%
  arrange(desc(avg_rate)) %>%
  slice(1:10)

ggplot(rate_by_year, aes(x = Year, y = avg_rate)) +
  geom_line(size = 1.2, color = "red") +
  geom_point() +
  labs(title = "Average Opioid Prescribing Rate by Year",
       x = "Year", y = "Prescribing Rate")

colnames(opioid_rate)
install.packages(c("tidyverse", "tigris", "sf", "viridis"))
library(tidyverse)
library(tigris)
library(sf)
library(viridis)
options(tigris_use_cache = TRUE)

# Filter to county level and clean FIPS
county_data <- opioid_rate %>%
  filter(Prscrbr_Geo_Lvl == "County") %>%
  mutate(fips = sprintf("%05d", as.integer(Prscrbr_Geo_Cd)))  # Ensure 5-digit FIPS as character

# Load county shapefiles
us_counties <- counties(cb = TRUE, resolution = "5m", class = "sf") %>%
  mutate(fips = GEOID)

# Join your data with the map shapefile
map_data <- left_join(us_counties, county_data, by = "fips")

#turn off spherical geo
sf::sf_use_s2(FALSE)

#filter outliers
map_data_contig <- map_data %>%
  filter(!STATEFP %in% c("02", "15", "72"))  # 02 = Alaska, 15 = Hawaii, 72 = Puerto Rico

#plot opioid map by County
ggplot(map_data_contig) +
  geom_sf(aes(fill = Opioid_Prscrbng_Rate), color = NA) +
  scale_fill_viridis(
    name = "Prescribing Rate", option = "C", na.value = "gray90"
  ) +
  labs(
    title = "Opioid Prescribing Rate by County (Lower 48)",
    subtitle = "Among Medicare Part D Beneficiaries",
    caption = "Source: CMS Opioid Prescribing Data"
  ) +
  coord_sf(crs = st_crs(2163)) +  # FIXED U.S.-friendly projection
  theme_minimal

ggplot(map_data_contig) +
  geom_sf(aes(fill = Opioid_Prscrbng_Rate), color = NA) +
  scale_fill_viridis(
    name = "Prescribing Rate", option = "C", na.value = "gray90"
  ) +
  labs(
    title = "Opioid Prescribing Rate by County (Lower 48)",
    subtitle = "Among Medicare Part D Beneficiaries",
    caption = "Source: CMS Opioid Prescribing Data"
  ) +
  coord_sf(crs = st_crs(9311)) +
  theme_void()  # removes everything including axes and grid


ggplot(map_data_contig) +
  geom_sf(aes(fill = Opioid_Prscrbng_Rate), color = NA) +
  scale_fill_viridis(
    name = "Prescribing Rate", option = "C", na.value = "gray90"
  ) +
  labs(
    title = "Opioid Prescribing Rate by County (Lower 48)",
    subtitle = "Among Medicare Part D Beneficiaries",
    caption = "Source: CMS Opioid Prescribing Data"
  ) +
  coord_sf(crs = st_crs(9311), default_crs = st_crs(9311), datum = NA) +
  theme_void(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    plot.subtitle = element_text(hjust = 0.5, size = 14),
    plot.caption = element_text(hjust = 0.5, size = 10),
    legend.position = "right",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    plot.margin = margin(5, 5, 5, 5)  # minimal margins
  )

library(tigris)
states_lookup <- tigris::fips_codes %>%
  select(state_code, state_name, state = state)

#state lookup table
library(tigris)

states_lookup <- tigris::fips_codes %>%
  select(state_code, state_name, state) %>%
  distinct()

# Make sure state abbreviations are joined
library(tigris)

states_lookup <- tigris::fips_codes %>%
  select(state_code, state_name, state) %>%
  distinct()

map_data_named <- map_data %>%
  left_join(states_lookup, by = c("STATEFP" = "state_code"))

# Loop through each state and save a map
state_list <- unique(map_data_named$state)

for (st in state_list) {
  state_data <- map_data_named %>% filter(state == st)
  
  p <- ggplot(state_data) +
    geom_sf(aes(fill = Opioid_Prscrbng_Rate), color = "white", size = 0.2) +
    scale_fill_viridis_c(name = "Prescribing Rate", option = "C", na.value = "gray90") +
    labs(
      title = paste("Opioid Prescribing Rate in", st),
      subtitle = "Medicare Part D Beneficiaries",
      caption = "Source: CMS Opioid Prescribing Data"
    ) +
    coord_sf(crs = st_crs(9311)) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5),
      plot.caption = element_text(hjust = 0.5)
    )
  
  ggsave(paste0("opioid_map_", st, ".png"), plot = p, width = 8, height = 6, dpi = 300)
}

#read opioid dataset
opioids <- read.csv("~/Desktop/Capstone Documentation/Capstone Data/Prescribing Rates/OMT_MDCR_RY24_P04_V10_YTD22_GEO.csv")

# make state level
library(dplyr)
state_data <- opioids %>%
  filter(Prscrbr_Geo_Lvl == "State", Breakout == "Overall") %>%
  select(State = Prscrbr_Geo_Desc, Tot_Opioid_Clms, LA_Tot_Opioid_Clms, Tot_Clms)

#view summary
print(state_data)

# making bar chart pt 1
library(tidyverse)

opioid_states <- opioids %>%
  filter(Prscrbr_Geo_Lvl == "State", Breakout == "Overall") %>%
  mutate(
    Opioid_Prescribing_Rate = (Tot_Opioid_Clms / Tot_Clms) * 100
  )

# latest yeaer
latest_year <- max(opioid_states$Year, na.rm = TRUE)

# Filter to latest year
latest_data <- opioid_states %>%
  filter(Year == latest_year)

top20 <- latest_data %>%
  arrange(desc(Opioid_Prescribing_Rate)) %>%
  slice(1:20)
#bar chart pt 2
ggplot(top20, aes(x = reorder(Prscrbr_Geo_Desc, Opioid_Prescribing_Rate), 
                  y = Opioid_Prescribing_Rate)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = paste("Top 20 States by Opioid Prescribing Rate (", latest_year, ")", sep = ""),
    x = "State",
    y = "Opioid Prescribing Rate (%)"
  ) +
  theme_minimal()

# Rank by highest opioid prescribing rate
ranked_opioids <- state_data %>%
  mutate(Opioid_Rate = Tot_Opioid_Clms / Tot_Clms * 100) %>%
  arrange(desc(Opioid_Rate))

print(ranked_opioids)

top20 <- latest_data %>%
  filter(!is.na(Opioid_Prescribing_Rate), is.finite(Opioid_Prescribing_Rate)) %>%
  arrange(desc(Opioid_Prescribing_Rate)) %>%
  slice(1:20)

# Bar plot of opioid prescribing rate
ggplot(ranked_opioids, aes(x = reorder(State, -Opioid_Rate), y = Opioid_Rate)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Opioid Prescribing Rate by State",
       x = "State", y = "Prescribing Rate (%)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#interactive bar chart
install.packages("plotly")
library(plotly)

plot_ly(top20, 
        x = ~Opioid_Prescribing_Rate, 
        y = ~reorder(Prscrbr_Geo_Desc, Opioid_Prescribing_Rate), 
        type = 'bar', 
        orientation = 'h') %>%
  layout(title = "Top 20 States by Opioid Prescribing Rate",
         xaxis = list(title = "Opioid Prescribing Rate (%)"),
         yaxis = list(title = "State"))

# heat map 2022
install.packages(c("tidyverse", "sf", "tmap", "tigris"))
library(tidyverse)
library(sf)
library(tmap)
library(tigris)
states_sf <- states(cb = TRUE)  # cb = cartographic boundary
counties_sf <- counties(cb = TRUE)
opioid_data <- read.csv("~/Desktop/Capstone Documentation/Capstone Data/Prescribing Rates/OMT_MDCR_RY24_P04_V10_YTD22_GEO.csv")
opioid_clean <- opioid_data %>%
  filter(Prscrbr_Geo_Lvl == "State", Breakout == "Overall") %>%
  mutate(Opioid_Rate = (Tot_Opioid_Clms / Tot_Clms) * 100) %>%
  select(State = Prscrbr_Geo_Desc, Opioid_Rate)
map_data <- states_sf %>%
  left_join(opioid_clean, by = c("NAME" = "State"))

tmap_mode("plot")  # use "view" for interactive

tm_shape(map_data) +
  tm_polygons(
    fill = "Opioid_Rate",
    fill.scale = tm_scale_intervals(style = "quantile", values = "YlOrRd"),
    fill.legend = tm_legend(title = "Opioid Prescribing Rate (%)")
  ) +
  tm_borders() +
  tm_title("Opioid Prescribing Rate by State (2022)") +
  tm_crs("auto")
map_data <- counties_sf %>%
  left_join(opioid_clean, by = c("GEOID" = "your_fips_column"))
tmap_save(tm = last_map(), filename = "opioid_heatmap.png")

hl$LIS_Percent <- (hl$Total.Claims.for.LIS.Beneficiaries / hl$Total.Claims) * 100
county_data <- left_join(county_data, hl %>% select(Calendar.Year, LIS_Percent), 
                         by = c("Year" = "Calendar.Year"))
county_data <- county_data %>%
  mutate(LIS_Group = ifelse(LIS_Percent > 50, "High LIS (50%+)", "Low LIS (<50%)"))

library(ggplot2)

county_data %>%
  group_by(LIS_Group) %>%
  summarise(Average_Opioid_Rate = mean(Opioid_Prscrbng_Rate, na.rm = TRUE)) %>%
  ggplot(aes(x = LIS_Group, y = Average_Opioid_Rate, fill = LIS_Group)) +
  geom_bar(stat = "identity", width = 0.6) +
  labs(
    title = "Average Opioid Prescribing Rate by LIS Group",
    x = "LIS Group",
    y = "Prescribing Rate (per 1,000 beneficiaries)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

table(county_data$LIS_Group)
High LIS (50+%)   0
Low LIS (<50%)   1980
county_data <- county_data %>%
  mutate(LIS_Group = ifelse(LIS_Percent > 40, "High LIS (40%+)", "Low LIS (<40%)"))

