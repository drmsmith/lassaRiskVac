####################
### SET FILEPATH ###
####################

filepath = this.path::here()
setwd(filepath)


source("housekeeping.R")

# Set file paths
path_data <- paste0(filepath, "/Raster") ### data are too large 
path_output <- paste0(filepath, "/RasterOutput")

# Define the countries, sexes, and age groups
countries <- c("gin", "lbr", "nga", "sle")  # Guinea, Liberia, Nigeria, Sierra Leone
sexes <- c("m", "f")  # Male, Female
age_groups <- c("0", "1", "5", "10", "15", "20", "25", "30", "35", "40", "45", "50", "55", "60", "65", "70", "75", "80")

# Define the year
year <- "2020"

# Initialize an empty tibble for final data
final_data <- tibble()

# Loop over each country, sex, and age group
for (country in countries) {
  for (sex in sexes) {
    for (age_group in age_groups) {
      
      print(paste0("on ", sex, " ", age_group, " in ", country))
      
      # Construct the file name
      file_name <- paste0(country, "_", sex, "_", age_group, "_", year, "_constrained_UNadj.tif")
      
      # Load the raster file
      rst <- raster(file.path(path_data, file_name))
      
      # Download and load the district shapefile using geodata
      shp_district <- gadm(country = toupper(country), level = 2, path = path_data)
      
      # Convert SpatVector to sf object
      shp_district_sf <- st_as_sf(shp_district)
      
      # Extract population data by district
      dat_pop <- exact_extract(rst, shp_district_sf)
      
      # Loop through each district and combine the extracted data with the district info
      for (i in seq_along(dat_pop)) {
        temp1 <- dat_pop[[i]]$value[!is.na(dat_pop[[i]]$value)]
        if (length(temp1) > 0) {
          output <- tibble(
            pop = sum(temp1),
            district = shp_district_sf$NAME_2[i],
            GID_1 = shp_district_sf$GID_1[i],
            country = toupper(country),
            sex = sex,
            age_group = age_group
          )
        } else {
          output <- tibble(
            pop = NA,
            district = shp_district_sf$NAME_2[i],
            GID_1 = shp_district_sf$GID_1[i],
            country = toupper(country),
            sex = sex,
            age_group = age_group
          )
        }
        final_data <- bind_rows(final_data, output)
      }
      
    }
  }
}

# Write the final data to a CSV file with an updated name
age_group_data <- final_data
# write.csv(age_group_data, file.path(path_output, "pop_district_age_sex_UNadj.csv"), row.names = FALSE)

###########################
# Load the generated data #
###########################

age_group_data <- read.csv(file.path(path_output, "pop_district_age_sex_UNadj.csv"))


# Sum the population by GID_1, sex, and age group
population_data_subnational <- age_group_data %>%
  group_by(GID_1, country) %>%
  summarize(pop_subnational = sum(pop, na.rm = TRUE))

population_data_national <- population_data_subnational%>%
  group_by(country)%>%
  summarise(pop_national = sum(pop_subnational))

population_data_proportion_subnational = population_data_subnational%>%
  left_join(., population_data_national, by = "country")%>%
  mutate(proportion_subnational = pop_subnational/pop_national)
  
write.csv(population_data_proportion_subnational, file.path(path_output, "pop_per_district_UNadj.csv"), row.names = FALSE)