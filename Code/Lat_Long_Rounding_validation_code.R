# install.packages("USpopcenters")
rm(list=ls()) 

library(tidyverse)
library(USpopcenters)
library(arrow)

setwd("C:\\Users\\Asus\\Box\\Flood Damage PredictionProject\\Dataset")

data <- read_parquet("july_24_flood_data.parquet.gzip")

df10 <- data %>% filter(yearOfLoss > 2009) %>% filter(yearOfLoss < 2020) %>% 
  mutate(censusBlockGroupFips = as.character(censusBlockGroupFips)) %>% 
  mutate(censusBlockGroupFips = str_pad(censusBlockGroupFips, width = 12, pad = "0"))

# Limited dataset to multiple group
df10_con <- df10 %>% group_by(censusBlockGroupFips, latitude) %>% 
  summarize(n = n()) %>% ungroup() %>% 
  group_by(censusBlockGroupFips) %>% 
  mutate(n_count = n(), tot = sum(n)) %>% filter(n_count > 1) %>%  
  select(-latitude) %>% distinct(censusBlockGroupFips, .keep_all = TRUE)

# All dataset
df10_all <- df10 %>% group_by(censusBlockGroupFips, latitude) %>% 
  summarize(n = n()) %>% ungroup() %>% 
  group_by(censusBlockGroupFips) %>% 
  mutate(n_count = n(), tot = sum(n)) %>%  
  select(-latitude) %>% distinct(censusBlockGroupFips, .keep_all = TRUE)


df10_lat <- df10 %>%select(censusBlockGroupFips, latitude) %>% 
  distinct(censusBlockGroupFips, .keep_all = TRUE)

bcg10 <- block_group2010 %>% 
  mutate(censusBlockGroupFips = str_c(STATEFP, COUNTYFP, TRACTCE, BLKGRPCE)) %>% 
  distinct(censusBlockGroupFips, .keep_all = TRUE)

con_lat <- df10_con %>% left_join(df10_lat) %>% left_join(bcg10) %>% 
  mutate(lat_diff = LATITUDE - latitude) %>% 
  mutate(lat10 = LATITUDE*10,
         latlast = lat10 - floor(lat10),
         last_digit_lat = latlast/10)

full_lat <- df10_all %>% left_join(df10_lat) %>% left_join(bcg10) %>% 
  mutate(lat_diff = LATITUDE - latitude) %>% 
  mutate(lat10 = LATITUDE*10,
         latlast = lat10 - floor(lat10),
         last_digit_lat = latlast/10)

# Custom bin intervals from -0.05 to 0.05
bin_seq <- seq(-0.05, 0.05, by = 0.005)

ggplot(con_lat, aes(x=lat_diff)) +
  geom_histogram(fill="lightgray", color="black", alpha=0.7) +
  #geom_bar(color="blue", lwd=0.5) +
  labs(title="Density Histogram of Block Group Centroid and Reported Latitude", x="lat_diff", y="Density",
       subtitle="Data Limited to ~140.000 observation with 2010 census block that have multiple latitude") +
  theme_minimal()



ggplot(full_lat, aes(x=lat_diff)) +
  geom_histogram(fill="lightgray", color="black", alpha=0.7) +
  labs(title="Frequency Histogram of Block Group Centroid and Reported Latitude", x="lat_diff", y="Frequency",  # Update y-axis label to "Frequency"
       subtitle="For all ~600,000 dataset from 2010 - 2019") +
  theme_minimal()



# Limiting value to -0.05 to 0.05
ggplot(full_lat, aes(x = lat_diff)) +
  geom_histogram(aes(y = ..density..), fill = "lightgray", color = "black", alpha = 0.7, breaks = bin_seq) +
  geom_density(color = "blue", lwd = 0.5) +
  labs(title = "Density Histogram of Block Group Centroid and Reported Latitude",
       x = "lat_diff", y = "Density",
       subtitle = "For all ~93% obs in 2010 - 2019 with abs(lat_diff) < 0.05") +
  theme_minimal() +
  scale_x_continuous(limits = c(-0.05, 0.05))


ggplot(con_lat, aes(x=lat_diff)) +
  geom_histogram(aes(y=..density..), fill="lightgray", color="black", alpha=0.7,  breaks = bin_seq) +
  geom_density(color="blue", lwd=0.5) +
  labs(title="Density Histogram of Block Group Centroid and Reported Latitude", x="lat_diff", y="Density",
       subtitle="Data Limited to ~140.000 observation with multiple latitude AND abs(lat_diff) <0.05") +
  theme_minimal() +
  scale_x_continuous(limits = c(-0.05, 0.05))


# Percentage of data with certain ggplot
full_lat2 <- full_lat %>% filter(abs(lat_diff) < 0.05)

lat_con2 <- con_lat %>% filter(abs(lat_diff) < 0.05)


# For all dataset


# Alternative stuff: rounding in latitude -- All dataset
ggplot(full_lat, aes(x=last_digit_lat)) +
  geom_histogram(fill="lightgray", color="black", alpha=0.7) +
  #geom_density(color="blue", lwd=0.5) +
  labs(title="Histogram of Last Two Digits of Latitude", x="Last two digits of Latitude", y="Frequency",
       subtitle="Full Census Block Group in 2010 - 2019, ~61,043 CBG") +
  theme_minimal() +
  scale_x_continuous(limits = c(0, 0.1))

# Alternative stuff: rounding in latitude -- only census block with multiple latitude
ggplot(con_lat, aes(x=last_digit_lat)) +
  geom_histogram(fill="lightgray", color="black", alpha=0.7) +
  # geom_density(color="blue", lwd=0.5) +
  labs(title="Histogram of Last Two Digits of Latitude", x="Last two digits of Latitude", y="Frequency",
       subtitle="Census Block Group which record flood in multiple latitude, 2010 - 2019, ~4,270 CBG") +
  theme_minimal()
