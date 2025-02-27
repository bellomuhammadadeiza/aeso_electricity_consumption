## ----setup, include = FALSE----------------------------------------------------------------------
knitr::opts_chunk$set(eval = TRUE)


## ------------------------------------------------------------------------------------------------

library(readr)
library(sf)
library(janitor)
library(tidyr)
library(stargazer)
library(ggplot2)
library(here)
library(dplyr)
library(scales)
library(lubridate)

options(warn = -1) # To supress warnings


## ------------------------------------------------------------------------------------------------
# Load population data and display the first few rows
population <- read.csv(here("data","population.csv")) 
head(population)

# Load electricity consumption data and display the first few rows
electricity <- read.csv(here("data","electricity_district.csv")) 
head(electricity)

# Load dictionary data to link municipalities with AESO regions
dictionary <- read.csv(here("data","dictionary.csv")) 
head(dictionary)

# Load shapefile for AESO regions, convert Area_ID to numeric for easy merging
mapping_areas <- st_read(here("data", "shape", "AESO_Planning_Areas.shp")) |>
  mutate(Area_ID = as.numeric(Area_ID))
head(mapping_areas)


## ------------------------------------------------------------------------------------------------

# Join the population data with the dictionary
pop_dictionary <- population %>%
  left_join(dictionary, by = "code", relationship = "many-to-many")  

# Check the joined data
head(pop_dictionary)

# Aggregate population by Area_ID and year, summing up the populations
## Also include the district name in the grouping if it's a column in the dictionary
population_aggregated <- pop_dictionary %>%
  drop_na() %>%
  group_by(Area_ID, year, district = name) %>%
  arrange(Area_ID, year) %>%
  summarise(total_population = sum(population, na.rm = TRUE), .groups = 'drop')

# Check the aggregated data
head(population_aggregated)


## ------------------------------------------------------------------------------------------------
# Importing temp data for each district

## For Alliance District
alliance_temp <- read.csv(here("data", "Alliance_temp.csv"))

## For Athabasca District
athabasca_temp <- read.csv(here("data", "Athabasca_temp.csv"))

## For Brooks District
brooks_temp <- read.csv(here("data", "Brooks_temp.csv"))

## For GrandePraire District
grandeprairie_temp <- read.csv(here("data", "GrandePrairie_temp.csv"))


## For HighLevel District
highlevel_temp <- read.csv(here("data", "HighLevel_temp.csv"))

## For HintonEdson District
hintonedson_temp <- read.csv(here("data", "HintonEdson_temp.csv"))

## For Lethbridge District
lethbridge_temp <- read.csv(here("data", "lethbridge_temp.csv"))

## For Vegreville District
vegreville_temp <- read.csv(here("data", "vegreville_temp.csv"))

## For Wabamun District
wabamun_temp <- read.csv(here("data", "Wabamun_temp.csv"))



# Combine all district temperature data into one dataframe
temperature_data2 <- bind_rows(
  alliance_temp %>% mutate(district = "Alliance"),
  athabasca_temp %>% mutate(district = "Athabasca"),
  brooks_temp %>% mutate(district = "Brooks"),
  grandeprairie_temp %>% mutate(district = "Grande Prairie"),
  highlevel_temp %>% mutate(district = "High Level"),
  hintonedson_temp %>% mutate(district = "Hinton/Edson"),
  lethbridge_temp %>% mutate(district = "Lethbridge"),
  vegreville_temp %>% mutate(district = "Vegreville"),
  wabamun_temp %>% mutate(district = "Wabamun")
)

# Convert datetime to POSIXct format, extract year, and add a month column
temperature_data2 <- temperature_data2 %>%
  mutate(
    datetime = ymd_hms(datetime),  # Ensure datetime is in POSIXct format
    year = year(datetime),  # Extract the year from datetime
    month = month(datetime)  # Extract the month from datetime
  )

# Check the results
head(temperature_data2) 


## ------------------------------------------------------------------------------------------------

# Rename columns for Area_ID in electricity data to make it long format compatible
electricity <- electricity %>%
  rename("18" = AREA18, "20" = AREA20, "27" = AREA27, "29" = AREA29, 
         "36" = AREA36, "40" = AREA40, "47" = AREA47, "54" = AREA54, 
         "56" = AREA56)

# Reshape electricity data to long format for analysis
electricity_long <- electricity %>%
  pivot_longer(
    cols = c("18", "20", "27", "29", "36", "40", "47", "54", "56"),         
    names_to = "Area_ID", 
    values_to = "electricity_use"
  ) %>%
  mutate(
    Area_ID = as.numeric(Area_ID),  # Convert Area_ID to numeric
    year = year(datetime)  # Extract the year from the datetime column
  )

# View the transformed data
head(electricity_long)

# Merge the electricity data with the aggregated population data on 'Area_ID' and 'year'
population_electricity <- electricity_long %>%
  left_join(population_aggregated, by = c("Area_ID", "year"))

# View the combined data
head(population_electricity)


## ------------------------------------------------------------------------------------------------
# Ensure datetime in population_electricity is in datetime format
population_electricity <- population_electricity %>%
  mutate(datetime = ymd_hms(datetime))

# Join the temperature data with the main data frame
combined_data <- population_electricity %>%
  left_join(temperature_data2, by = c("Area_ID", "year", "datetime"))

# Check the results of the join
head(combined_data)

# Remove rows with NA values
combined_data_cleaned <- combined_data %>%
  drop_na()

# Combine district.x and district.y into a single column called district
combined_data_cleaned <- combined_data_cleaned %>%
  mutate(district = coalesce(district.x, district.y)) %>%
  select(-district.x, -district.y)

# Check the results
print(head(combined_data_cleaned))



## ------------------------------------------------------------------------------------------------

# Calculate per capita electricity consumption
final_combined_data_cleaned <- combined_data_cleaned %>%
  mutate(pc_consumption = electricity_use / total_population)

# Calculate average per capita electricity consumption for each Area_ID and year
avg_pc_consumption_data <- final_combined_data_cleaned %>%
  group_by(Area_ID, year) %>%
  summarise(avg_pc_consumption = mean(pc_consumption, na.rm = TRUE), .groups = 'drop')

# Calculate average temperature for each Area_ID and year
avg_temp_data <- final_combined_data_cleaned %>%
  group_by(Area_ID, year) %>%
  summarise(avg_temp = mean(temp_c, na.rm = TRUE), .groups = 'drop')

# Join the average temperature data with the cleaned combined data
final_combined_data_cleaned <- final_combined_data_cleaned %>%
  left_join(avg_temp_data, by = c("Area_ID", "year"))

# Join the average per capita consumption data as well
final_combined_data_cleaned <- final_combined_data_cleaned %>%
  left_join(avg_pc_consumption_data, by = c("Area_ID", "year"))

# Check the results after adding average per capita consumption and average temperature
head(final_combined_data_cleaned, 9)


## ------------------------------------------------------------------------------------------------

# Create a basic map of Alberta from shapefile data
base_map <- ggplot(mapping_areas) +
  geom_sf() +    # Plot geometry
  labs(title = "Alberta") 
base_map


## ------------------------------------------------------------------------------------------------

# Convert spatial data to non-spatial data for easier filtering
mapping_areas_district <- st_drop_geometry(mapping_areas)

# Filter shapefile data to focus on nine selected districts
selected_districts <- c("Lethbridge", "Athabasca / Lac La Biche", "Vegreville", "High Level", 
                        "Alliance / Battle River", "Brooks", "Grande Prairie", "Wabamun", 
                        "Hinton / Edson") 

# Select distinct records for these districts with Area_IDs
MA_district_dta <- mapping_areas_district %>%
  filter(NAME %in% selected_districts) %>%
  select(NAME, Area_ID) %>%
  distinct()  # Ensure each district is shown only once
print(MA_district_dta)


## ------------------------------------------------------------------------------------------------

# Join the filtered district data to shapefile data for mapping purposes
mapping_areas_trial <- mapping_areas %>%
  inner_join(MA_district_dta, by = "Area_ID")
head(mapping_areas_trial)

# Create map with labeled Area IDs
alberta <- ggplot(mapping_areas_trial) +
  geom_sf(fill = "lightblue", color = "black") +    # Basic region coloring
  geom_sf_text(data = st_centroid(mapping_areas_trial), aes(label = Area_ID), 
               color = "darkred", size = 3) +       # Add Area ID labels
  labs(title = "Alberta") 

# show the first map
print(alberta)

# Save the first map
ggsave(filename = here("data", "viz", "alberta_labeled_map.png"), plot = alberta, width = 10, height = 8)

# Perform a join to include focus districts in shapefile data
mapping_areas <- mapping_areas %>%
  left_join(MA_district_dta, by = "Area_ID")
head(mapping_areas)

# Generate map with focus districts
alberta_complete <- ggplot(mapping_areas) +
  geom_sf(fill = "lightblue", color = "black") +    # Fill Alberta with color
  geom_sf_text(data = st_centroid(mapping_areas), aes(label = Area_ID), 
               color = "darkred", size = 3) +       # Add Area ID labels
  labs(title = "Alberta Complete")

# show the second map
print (alberta_complete)

# Save the second map
ggsave(filename = here("data", "viz", "alberta_complete_map.png"), plot = alberta_complete, width = 10, height = 8)



## ------------------------------------------------------------------------------------------------

# Adding an 'id_label' to each district for easier identification on maps and selecting relevant columns

MA_district_dta <- MA_district_dta |>
  mutate(id_label = Area_ID) |>
  select(Area_ID, id_label)
head(MA_district_dta)

# Merging labeled district data with AESO regions' shapefile data and converting 'id_label' to character

mapping_areas_joined <- mapping_areas |>
  left_join(MA_district_dta, by = "Area_ID") |>
  mutate(id_label = as.character(id_label)) |>
  mutate(id_label = ifelse(is.na(id_label), "", id_label))
head(mapping_areas_joined)


## ------------------------------------------------------------------------------------------------
# extract for summer
summer_data <- final_combined_data_cleaned %>%
  filter(month %in% c(6, 7, 8))  # Filtering for June (6), July (7), August (8)
head(summer_data)


## ------------------------------------------------------------------------------------------------

# Calculate average summer temperature for each Area_ID and year
summer_avg_temp <- summer_data %>%
  group_by(Area_ID, year) %>%
  summarise(avg_temp_summer = mean(temp_c, na.rm = TRUE), .groups = 'drop')  # Calculating the mean and remove grouping

# Join the average summer temperature data back to the original summer_data dataframe
summer_data <- summer_data %>%
  left_join(summer_avg_temp, by = c("Area_ID", "year"))

head(summer_data)


## ------------------------------------------------------------------------------------------------

# Join summer data to shapefile data for mapping
map_summer_temp <- mapping_areas_joined %>%
  left_join(summer_data, by = "Area_ID")
head(map_summer_temp)

# Plot map showing per capita electricity consumption for the selected districts
ggplot() +
  geom_sf(data = map_summer_temp, aes(fill = ifelse(!is.na(avg_temp_summer), avg_temp_summer, NA))) +
  scale_fill_viridis_c(na.value = "gray90", labels = label_number(accuracy = 0.1)) +  # Format fill values to one decimal place
  labs(title = "Average Summer Temperature", fill = "Temperature (°C)")

########################################################################################################################################################################################

# Map with per capita consumption labels for each district
summer_temperature_map <- ggplot() +
  geom_sf(data = map_summer_temp, aes(fill = ifelse(!is.na(avg_temp_summer), avg_temp_summer, NA))) +
  scale_fill_viridis_c(na.value = "gray90", labels = label_number(accuracy = 0.1)) +  # Format fill values to one decimal place
  geom_sf_text(data = st_centroid(map_summer_temp), aes(label = id_label), 
               color = "white", size = 3) +
  labs(title = "Average Summer Temperature", fill = "Temperature (°C)")

# Display the map
print(summer_temperature_map)

# Save the map
ggsave(filename = here("data", "viz", "average_summer_temperature_map.png"), plot = summer_temperature_map, width = 10, height = 8)



## ------------------------------------------------------------------------------------------------
# Extract data for winter months (December, January, February)
winter_data <- final_combined_data_cleaned %>%
  filter(month %in% c(12, 1, 2))  # Filtering for December (12), January (1), February (2)

# Display the first 50 rows of the winter data to check it
head(winter_data)


## ------------------------------------------------------------------------------------------------

# Calculate average winter temperature for each Area_ID and year
winter_avg_temp <- winter_data %>%
  group_by(Area_ID, year) %>%
  summarise(avg_temp_winter = mean(temp_c, na.rm = TRUE), .groups = 'drop')  # Calculate the mean and remove grouping

# Join the average winter temperature data back to the original winter_data dataframe
winter_data <- winter_data %>%
  left_join(winter_avg_temp, by = c("Area_ID", "year"))

# Display the first few rows of the winter data to check it
head(winter_data)



## ------------------------------------------------------------------------------------------------
mapping_areas_joined <- mapping_areas |>
  left_join(MA_district_dta, by = "Area_ID") |>
  mutate(id_label = as.character(id_label)) |>
  mutate(id_label = ifelse(is.na(id_label), "", id_label))
head(mapping_areas_joined)


## ------------------------------------------------------------------------------------------------

# Join winter data to shapefile data for mapping
map_winter_temp <- mapping_areas_joined %>%
  left_join(winter_data, by = "Area_ID")
head(map_winter_temp)

# Plot map with average temperature for the selected districts
ggplot() +
  geom_sf(data = map_winter_temp, aes(fill = ifelse(avg_temp_winter < 0, avg_temp_winter, NA))) +
  scale_fill_viridis_c(na.value = "gray90", labels = label_number(accuracy = 0.1)) +  # Format fill values to one decimal place
  labs(title = "Average Winter Temperature", fill = "Temperature (°C)")

# Map with average temperature for selected districts
winter_temperature_map <- ggplot() +
  geom_sf(data = map_winter_temp, aes(fill = ifelse(avg_temp_winter < 0, avg_temp_winter, NA))) +
  scale_fill_viridis_c(na.value = "gray90", labels = label_number(accuracy = 0.1)) +  # Format fill values to one decimal place
  geom_sf_text(data = st_centroid(map_winter_temp), aes(label = id_label), 
               color = "white", size = 3) +
  labs(title = "Average Winter Temperature", fill = "Temperature (°C)")

# Display the map
print(winter_temperature_map)

# Save the map
ggsave(filename = here("data", "viz", "average_winter_temperature_map.png"), plot = winter_temperature_map, width = 10, height = 8)



## ------------------------------------------------------------------------------------------------

# Calculate per capita electricity consumption for summer
summer_data <- summer_data %>%
  mutate(summer_pc_consumption = electricity_use / total_population)  # Calculate per capita consumption

# Check the first few rows to confirm the new column has been added
head(summer_data)


## ------------------------------------------------------------------------------------------------
# Join summer data to shapefile data for mapping
map_summer_pc_consumption <- mapping_areas_joined %>%
  left_join(summer_data, by = "Area_ID")
head(map_summer_pc_consumption)

# Plot map showing per capita electricity consumption for the selected districts
ggplot() +
  geom_sf(data = map_summer_pc_consumption, aes(fill = ifelse(summer_pc_consumption > 0, summer_pc_consumption, NA))) +
  scale_fill_viridis_c(na.value = "gray90") +  
  labs(title = "Summer Electricity Consumption per Capita", fill = "Consumption (KWh)")

########################################################################################################################################################################################

# Map with per capita consumption labels for each district
summer_consumption_map <- ggplot() +
  geom_sf(data = map_summer_pc_consumption, aes(fill = ifelse(summer_pc_consumption > 0, summer_pc_consumption, NA))) +
  scale_fill_viridis_c(na.value = "gray90") + 
  geom_sf_text(data = st_centroid(map_summer_pc_consumption), aes(label = id_label), 
               color = "white", size = 3) +
  labs(title = "Summer Electricity Consumption per Capita", fill = "Consumption (KWh)")

# Display the map
print(summer_consumption_map)

# Save the map
ggsave(filename = here("data", "viz", "summer_consumption_map.png"), plot = summer_consumption_map, width = 10, height = 8)



## ------------------------------------------------------------------------------------------------

# Calculate per capita electricity consumption for winter
winter_data <- winter_data %>%
  mutate(winter_pc_consumption = electricity_use / total_population)  # Calculate per capita consumption

# Check the first few rows to confirm the new column has been added
head(winter_data)


## ------------------------------------------------------------------------------------------------
# Join winter data to shapefile data for mapping
map_winter_pc_consumption <- mapping_areas_joined %>%
  left_join(winter_data, by = "Area_ID")
head(map_winter_pc_consumption)

# Plot map showing per capita electricity consumption for the selected districts
ggplot() +
  geom_sf(data = map_winter_pc_consumption, aes(fill = ifelse(winter_pc_consumption > 0, winter_pc_consumption, NA))) +
  scale_fill_viridis_c(na.value = "gray90") +  # Format fill values to one decimal place
  labs(title = "Winter Electricity Consumption per Capita", fill = "Consumption (KWh)")

########################################################################################################################################################################################

# Map with per capita consumption labels for each district during winter
winter_consumption_map <- ggplot() +
  geom_sf(data = map_winter_pc_consumption, aes(fill = ifelse(winter_pc_consumption > 0, winter_pc_consumption, NA))) +
  scale_fill_viridis_c(na.value = "gray90") +  # Format fill values to one decimal place
  geom_sf_text(data = st_centroid(map_winter_temp), aes(label = id_label), 
               color = "white", size = 3) +
  labs(title = "Winter Electricity Consumption per Capita", fill = "Consumption (KWh)")

# Display the map
print(winter_consumption_map)

# Save the map
ggsave(filename = here("data", "viz", "winter_consumption_map.png"), plot = winter_consumption_map, width = 10, height = 8)


## ------------------------------------------------------------------------------------------------
# Calculate the average of summer_pc_consumption
average_summer_pc_consumption <- summer_data %>%
  summarise(avg_summer_pc_consumption = mean(summer_pc_consumption, na.rm = TRUE))

# Display the result for summer
print(average_summer_pc_consumption)

# Calculate the average of winter_pc_consumption
average_winter_pc_consumption <- winter_data %>%
  summarise(avg_winter_pc_consumption = mean(winter_pc_consumption, na.rm = TRUE))

# Display the result for winter
print(average_winter_pc_consumption)


## ------------------------------------------------------------------------------------------------

# Define dodge width and bar width for better control over bar spacing
dodge_width <- 0.8
bar_width <- 0.7

# Plotting average summer temperature with bar graph
summer_temp_plot <- ggplot(summer_data, aes(x = factor(year), y = avg_temp_summer, fill = factor(Area_ID))) +
  geom_bar(stat = "identity", position = position_dodge(width = dodge_width), width = bar_width) +
  scale_fill_brewer(palette = "Paired", name = "Area ID") +
  labs(title = "Average Summer Temperature by District and Year",
       x = "Year",
       y = "Average Temperature (°C)",
       fill = "Area ID") +
  theme_minimal()

# show plot
print(summer_temp_plot)

# save plot
ggsave(filename = here("data", "viz", "summer_temp_plot.png"), plot = summer_temp_plot)

########################################################################################################################################################################################

# Plotting average winter temperature with bar graph
winter_temp_plot <- ggplot(winter_data, aes(x = factor(year), y = avg_temp_winter, fill = factor(Area_ID))) +
  geom_bar(stat = "identity", position = position_dodge(width = dodge_width), width = bar_width) +
  scale_fill_brewer(palette = "Paired", name = "Area ID") +
  labs(title = "Average Winter Temperature by District and Year",
       x = "Year",
       y = "Average Temperature (°C)",
       fill = "Area ID") +
  theme_minimal()

# show plot
print(winter_temp_plot)

# save plot
ggsave(filename = here("data", "viz", "winter_temp_plot.png"), plot = winter_temp_plot)

########################################################################################################################################################################################

# Plotting summer electricity consumption per capita
summer_elec_plot <- ggplot(summer_data, aes(x = factor(year), y = summer_pc_consumption, fill = factor(Area_ID))) +
  geom_bar(stat = "identity", position = position_dodge(width = dodge_width), width = bar_width) +
  scale_fill_brewer(palette = "Paired", name = "Area ID") +
  labs(title = "Summer Electricity Consumption Per Capita by District and Year",
       x = "Year",
       y = "Electricity Consumption Per Capita (kWh)",
       fill = "Area ID") +
  theme_minimal()

# show plot
print(summer_elec_plot)

# save plot
ggsave(filename = here("data", "viz", "summer_elec_plot.png"), plot = summer_elec_plot)

########################################################################################################################################################################################

# Plotting winter electricity consumption per capita
winter_elec_plot <- ggplot(winter_data, aes(x = factor(year), y = winter_pc_consumption, fill = factor(Area_ID))) +
  geom_bar(stat = "identity", position = position_dodge(width = dodge_width), width = bar_width) +
  scale_fill_brewer(palette = "Paired", name = "Area ID") +
  labs(title = "Winter Electricity Consumption Per Capita by District and Year",
       x = "Year",
       y = "Electricity Consumption Per Capita (kWh)",
       fill = "Area ID") +
  theme_minimal()

# show plot
print(winter_elec_plot)

# save plot
ggsave(filename = here("data", "viz", "winter_elec_plot.png"), plot = winter_elec_plot)


## ------------------------------------------------------------------------------------------------

# Calculate the average of summer per capita electricity consumption
average_summer_pc_consumption <- summer_data %>%
  summarise(avg_summer_pc_consumption = mean(summer_pc_consumption, na.rm = TRUE))

# Display the result
print(average_summer_pc_consumption)


# Calculate the average of summer per capita electricity consumption for each Area_ID and year
grouped_average_summer_pc_consumption <- summer_data %>%
  group_by(Area_ID, year) %>%  # Adjust grouping variables as necessary
  summarise(avg_summer_pc_consumption = mean(summer_pc_consumption, na.rm = TRUE), .groups = 'drop')

# Display the results
print(grouped_average_summer_pc_consumption)

# Calculate the average of winter per capita electricity consumption
average_winter_pc_consumption <- winter_data %>%
  summarise(avg_winter_pc_consumption = mean(winter_pc_consumption, na.rm = TRUE))

# Display the result
print(average_winter_pc_consumption)

# Calculate the average of winter per capita electricity consumption for each Area_ID and year
grouped_average_winter_pc_consumption <- winter_data %>%
  group_by(Area_ID, year) %>% # Adjust the grouping variables as necessary
  summarise(avg_winter_pc_consumption = mean(winter_pc_consumption, na.rm = TRUE), .groups = 'drop')

# Display the results
print(grouped_average_winter_pc_consumption)





## ------------------------------------------------------------------------------------------------

# Filter for July and extract the day
july_data <- summer_data %>%
  filter(month(datetime) == 7) %>%
  mutate(day = day(datetime))

# Display the modified data
head(july_data)

# Group by Area_ID, year, and day to calculate needed data
july_summary <- july_data %>%
  group_by(Area_ID, year, day) %>%
  summarise(
    daily_max_temp = max(temp_c, na.rm = TRUE),  # Maximum temperature for the day
    daily_pc_consumption= sum(pc_consumption, na.rm = TRUE),  # Total electricity used
    .groups = 'drop'
  )

# Display the summarized data
head(july_summary)



## ------------------------------------------------------------------------------------------------

# converting variables as factor
july_summary <- july_summary %>%
  mutate(
    Area_ID = as.factor(Area_ID),  # Convert Area_ID to factor
    year = as.factor(year)         # Convert year to factor
  )


# Ensure data has no zero or negative values for log transformation
july_summary <- july_summary %>%
  filter(daily_pc_consumption > 0, daily_max_temp > 0)

# Fitting the model with Area_ID and year as factors
model_july <- lm(log(daily_pc_consumption) ~ log(daily_max_temp) + Area_ID + year, data = july_summary)

# Display the summary of the model
summary(model_july)

# formatting with stargazer
stargazer(model_july, type = "text", 
          title = "Effect of July Temperature Spikes on Daily Electricity Consumption across Regions",
          align = TRUE)


## ------------------------------------------------------------------------------------------------

# Filter for all summer months (June, July, August) and extract the day
summer_data_extended <- summer_data %>%
  filter(month(datetime) %in% c(6, 7, 8)) %>%
  mutate(day = day(datetime))

# Display the modified data
head(summer_data_extended)

# Group by Area_ID, year, and day to calculate needed data for all summer months
summer_summary <- summer_data_extended %>%
  group_by(Area_ID, year, day) %>%
  summarise(
    daily_max_temp = max(temp_c, na.rm = TRUE),  # Maximum temperature for each day
    daily_pc_consumption = sum(pc_consumption, na.rm = TRUE),  # Total electricity used per day
    .groups = 'drop'
  )

# Display the summarized data
head(summer_summary)



## ------------------------------------------------------------------------------------------------
# Convert Area_ID and year to factor types for all summer data
summer_summary <- summer_summary %>%
  mutate(
    Area_ID = as.factor(Area_ID),  # Convert Area_ID to factor
    year = as.factor(year)         # Convert year to factor
  )

# Ensure data has no zero or negative values for log transformation
summer_model <- summer_summary %>%
  filter(daily_pc_consumption > 0, daily_max_temp > 0)

# Fitting the model with Area_ID and year as factors
summer_model <- lm(log(daily_pc_consumption) ~ log(daily_max_temp) + Area_ID + year, data = summer_summary)

# Display the summary of the model
summary(summer_model)

# Using stargazer to format the model output for publication
stargazer(summer_model, type = "text", 
          title = "Effect of Summer Temperature on Daily Electricity Consumption across Regions",
          align = TRUE)



## ------------------------------------------------------------------------------------------------
# Filter for December and extract the day
december_data <- winter_data %>%
  filter(month(datetime) == 12) %>%
  mutate(day = day(datetime))

# Display the modified December data
head(december_data)

# Filter for January and extract the day
january_data <- winter_data %>%
  filter(month(datetime) == 1) %>%
  mutate(day = day(datetime))

# Display the modified January data
head(january_data)

# Filter for February and extract the day
february_data <- winter_data %>%
  filter(month(datetime) == 2) %>%
  mutate(day = day(datetime))

# Display the modified February data
head(february_data)



## ------------------------------------------------------------------------------------------------

# Group by Area_ID, year, and day to calculate needed data
december_min <- december_data %>%
  group_by(Area_ID, year, day) %>%
  summarise(
    daily_min_temp = min(temp_c, na.rm = TRUE),  # Maximum temperature for the day
    daily_pc_consumption = sum(pc_consumption, na.rm = TRUE),  # Total per capita electricity used
    .groups = 'drop'
  )

# Display the summarized data
head(december_min)

########################################################################################################################################################################################


## For january

# Group by Area_ID, year, and day to calculate needed data
january_min <- january_data %>%
  group_by(Area_ID, year, day) %>%
  summarise(
    daily_min_temp = min(temp_c, na.rm = TRUE),  # Maximum temperature for the day
    daily_pc_consumption = sum(pc_consumption, na.rm = TRUE),  # Total per capita electricity used
    .groups = 'drop'
  )

# Display the summarized data
head(january_min)

###########################################################################################################################################################################################


## For February

# Group by Area_ID, year, and day to calculate needed data
february_min <- february_data %>%
  group_by(Area_ID, year, day) %>%
  summarise(
    daily_min_temp = min(temp_c, na.rm = TRUE),  # Maximum temperature for the day
    daily_pc_consumption = sum(pc_consumption, na.rm = TRUE),  # Total per capita electricity used
    .groups = 'drop'
  )

# Display the summarized data
head(february_min)



## ------------------------------------------------------------------------------------------------

# Calculate the average of daily_min_temp for December
december_avg_temp <- december_min %>%
  summarise(avg_daily_min_temp = mean(daily_min_temp, na.rm = TRUE))

# Display the result
print(december_avg_temp)

# Calculate the average of daily_min_temp for January
january_avg_temp <- january_min %>%
  summarise(avg_daily_min_temp = mean(daily_min_temp, na.rm = TRUE))

# Display the result
print(january_avg_temp)

# Calculate the average of daily_min_temp for February
february_avg_temp <- february_min %>%
  summarise(avg_daily_min_temp = mean(daily_min_temp, na.rm = TRUE))

# Display the result
print(february_avg_temp)


## ------------------------------------------------------------------------------------------------

# Convert variables to factor
january_min <- january_min %>%
  mutate(
    Area_ID = as.factor(Area_ID),  # Convert Area_ID to factor
    year = as.factor(year)         # Convert year to factor
  )

# Fitting the model with Area_ID and year as factors
january_model <- lm(daily_pc_consumption ~ daily_min_temp + Area_ID + year, data = january_min)

# Display the summary of the model
summary(january_model)

# formatting with stargazer
stargazer(january_model, type = "text", 
          title = "Effect of January Cold Snaps on Daily Electricity Consumption across Regions",
          align = TRUE)



## ------------------------------------------------------------------------------------------------

# Convert variables to factor for December data
december_min <- december_min %>%
  mutate(
    Area_ID = as.factor(Area_ID),  # Convert Area_ID to factor
    year = as.factor(year)         # Convert year to factor
  )

# Fitting the model with Area_ID and year as factors for December using actual values
december_model <- lm(daily_pc_consumption ~ daily_min_temp + Area_ID + year, data = december_min)


# Display the summary of the model
summary(december_model)

# Use stargazer to format the output of the regression model
stargazer(december_model, type = "text", 
          title = "Effect of December Cold Snaps on Daily Electricity Consumption across Regions",
          align = TRUE)



## ------------------------------------------------------------------------------------------------

# Filter for all winter months (December, January, February) and extract the day
winter_data_extended <- winter_data %>%
  filter(month(datetime) %in% c(12, 1, 2)) %>%
  mutate(day = day(datetime))

# Display the modified data
head(winter_data_extended)

# Group by Area_ID, year, and day to calculate needed data for all winter months
winter_summary <- winter_data_extended %>%
  group_by(Area_ID, year, day) %>%
  summarise(
    daily_min_temp = min(temp_c, na.rm = TRUE),  # Minimum temperature for each day
    daily_pc_consumption = sum(pc_consumption, na.rm = TRUE),  # Total electricity used per day
    .groups = 'drop'
  )

# Display the summarized data
head(winter_summary)



## ------------------------------------------------------------------------------------------------

# Convert variables to factors for the entire winter data
winter_summary <- winter_summary %>%
  mutate(
    Area_ID = as.factor(Area_ID),  # Convert Area_ID to factor
    year = as.factor(year)         # Convert year to factor
  )

# Fitting the model with Area_ID and year as factors for the entire winter using actual values
winter_model <- lm(daily_pc_consumption ~ daily_min_temp + Area_ID + year, data = winter_summary)

# Display the summary of the model
summary(winter_model)

# Use stargazer to format the output of the regression model for the entire winter
stargazer(winter_model, type = "text", 
          title = "Effect of Winter Cold Snaps on Daily Electricity Consumption across Regions",
          align = TRUE)


