rm(list=ls()) 
library(tidyverse)
library(arrow)

# This do-file create census-tract based socio-economic variables over the
# Fema v2 database. The variables that we have here are:
# 1. Population density
# 2. Housing density
# 3. Percentage of White Population   
# 4. Percentage of Black Population    
# 5. Percentage of Indian Population  
# 6. Percentage of Asian Population  
# 7. Percentage of Dual Race Population  (avalaible only 1990 onwards)
# 8. Median Income of the census tract
# 9. Median Housing value of the census tract

# Our main source of data are:
# for 1990, 2000, 2010, 2011 - 2021 data: IPUMS crosswalk + IPUMS dataset
# for 1980 data: Logan et al (2014)



#### Processing 1990's dataset

cw90 <- read.csv("C:/Users/Asus/Box/Flood Damage PredictionProject/Dataset/IPUMS Crosswalk/nhgis_blk1990_blk2010_gj/nhgis_blk1990_blk2010_gj.csv")

# Getting string length
cw90 <- cw90 %>% as_tibble()
cw90$len90 <- nchar(cw90$GJOIN1990)
cw90$len10 <- nchar(cw90$GJOIN2010)

# Instead of recreating 2010's CBG, we instead try to identify which 1990's 
#census tract does the 2010's CBG belong to

# Crosswalk: cw90 results

# If census tract code = 6 digits -> 18 or 17 -> then first 14 digits
# If census tract code = 4 digits -> 16 or 15 -> then first 12 digits

cw90 <- cw90 %>% mutate(tract90 = ifelse(len90 >= 17, str_sub(GJOIN1990, 1, 14), 
                                         str_sub(GJOIN1990, 1, 12))) %>% 
  mutate(blockgroup10 = str_sub(GJOIN2010, 1, 15)) %>% 
  filter(tract90 != "")

# Checking if blockgroup10 uniquely identify tract90 (it does not)
bg10 <- cw90 %>% group_by(blockgroup10) %>% summarize(n = n())
bg10_90 <- cw90 %>% group_by(blockgroup10, tract90) %>% summarize(n_2 = n())

bg10_nomatchtract90 <- bg10_90 %>% left_join(bg10) %>% 
  mutate(n_diff = n - n_2) %>% filter(n_diff != 0) %>% 
  group_by(blockgroup10) %>% summarize(n_match = n())

# Write which part is matching multiple tract
write.csv(bg10_nomatchtract90, "C:/Users/Asus/Box/Flood Damage PredictionProject/Dataset/IPUMS Crosswalk/nhgis_blk1990_blk2010_gj/multi_match90.csv") 


# Exact tract matching
exact_match90 <- bg10_90 %>% left_join(bg10) %>% 
  mutate(n_diff = n - n_2) %>% filter(n_diff == 0) %>% 
  group_by(blockgroup10, tract90) %>% summarize(n = n())

# write.csv(exact_match90, "exact_match90.csv")

# Creating weighting factor in bcg + diff census tract level
bg10_nomatchtract90w <- cw90 %>% left_join(exact_match90) %>% 
  group_by(blockgroup10, tract90) %>% 
  summarize(WEIGHT = sum(WEIGHT), n = n())

# Creating total weight in bcg
bg10_totweight <- bg10_nomatchtract90w %>% group_by(blockgroup10) %>% 
  summarize(WEIGHT_TOTAL = sum(WEIGHT))

# Writing as CBG and combining match and nonmatch values
bg10_nomatchtract90weight <- bg10_nomatchtract90w %>% left_join(bg10_totweight) %>% 
  mutate(weight = WEIGHT/WEIGHT_TOTAL) %>% 
  select(blockgroup10, tract90, weight) %>% 
  rename(GJOIN1990 = tract90)

write.csv(bg10_nomatchtract90weight, "C:/Users/Asus/Box/Flood Damage PredictionProject/Dataset/IPUMS Crosswalk/nhgis_blk1990_blk2010_gj/bcg_tract90_weighted.csv")

#### 2000 dataset -- similar methods

cw00 <- read.csv("C:/Users/Asus/Box/Flood Damage PredictionProject/Dataset/IPUMS Crosswalk/nhgis_blk2000_blk2010_gj/nhgis_blk2000_blk2010_gj.csv")

cw00$len00 <- nchar(cw00$GJOIN2000)
cw00$len10 <- nchar(cw00$GJOIN2010)

cw00 <- cw00 %>% as_tibble()

cw00 <- cw00 %>% mutate(tract00 = str_sub(GJOIN2000, 1, 14),
                        blockgroup10 = str_sub(GJOIN2010, 1, 15)) %>% 
  filter(tract00 != "")

# Total weight in cbg
bg10 <- cw00 %>% group_by(blockgroup10) %>% 
  summarize(weight_total = sum(WEIGHT))

# Total weight in cbg + census tract
bg10_00 <- cw00 %>% group_by(blockgroup10, tract00) %>% 
  summarize(weight_b = sum(WEIGHT))

# Calculating weight portion
bg10_nomatchtract00weight <- bg10_00 %>% left_join(bg10) %>% 
  mutate(weight = weight_b/weight_total) %>% select(blockgroup10, tract00, weight) %>% 
  rename(GJOIN2000 = tract00)

write.csv(bg10_nomatchtract00weight, "C:/Users/Asus/Box/Flood Damage PredictionProject/Dataset/IPUMS Crosswalk/nhgis_blk2000_blk2010_gj/bcg_tract00_weighted.csv")

#### 2020 dataset
cw20 <- read.csv("C:/Users/Asus/Box/Flood Damage PredictionProject/Dataset/IPUMS Crosswalk/nhgis_blk2020_blk2010_gj/nhgis_blk2020_blk2010_gj.csv") %>% as_tibble()

cw20 <- cw20 %>% mutate(tract20 = str_sub(GJOIN2020, 1, 14),
                        blockgroup10 = str_sub(GJOIN2010, 1, 15)) %>% 
  filter(tract20 != "") %>% select(-GJOIN2020) %>% 
  rename(GJOIN2020 = tract20)

# Total weight in cbg
bg10 <- cw20 %>% group_by(blockgroup10) %>% 
  summarize(weight_total = sum(WEIGHT))

# Total weight in cbg + census tract
bg10_20 <- cw20 %>% group_by(blockgroup10, GJOIN2020) %>% 
  summarize(weight_b = sum(WEIGHT))

# Calculating weight portion
bg20_nomatchtract00weight <- bg10_20 %>% left_join(bg10) %>% 
  mutate(weight = weight_b/weight_total) %>% select(blockgroup10, GJOIN2020, weight)

write.csv(bg20_nomatchtract00weight, "C:/Users/Asus/Box/Flood Damage PredictionProject/Dataset/IPUMS Crosswalk/bcg_tract20_weighted.csv")


#### 1980 dataset -> Using tract TS from Trent

# Lowest unit in GSJOIN2010 -> Census tract instead of census block

# cwts <- read.csv("C:/Users/Asus/Box/Flood Damage PredictionProject/Dataset/nghis census tract/nhgisCensusTract_csv/nhgis0009_csv/nhgis0009_ts_nominal_tract.csv") %>% as_tibble()
# cw_80 <- cwts %>% select(GJOIN1980, GJOIN2010, AV0AA1980) %>% 
#  filter(GJOIN1980 != "") %>% filter(GJOIN2010 != "")

# cw_80_10 <- cw_80 %>% group_by(GJOIN2010) %>% 
  # summarize(tot_person = AV0AA1980)

# bg80_nomatchtract00weight <- cw_80 %>% left_join(cw_80_10) %>% 
  # mutate(weight = AV0AA1980/tot_person) %>% 
  # select(GJOIN1980, GJOIN2010, weight)


# write.csv(bg80_nomatchtract00weight, "C:/Users/Asus/Box/Flood Damage PredictionProject/Dataset/IPUMS Crosswalk/bcg_tract80_weighted.csv")


##### Trying to combine the data with original dataset

data2 <- read_parquet("C:/Users/Asus/Box/Flood Damage PredictionProject/Dataset/july_24_flood_data.parquet.gzip")
data2 <- read.csv("C:/Users/Asus/Box/Flood Damage PredictionProject/Dataset/FimaNfipClaims.csv", 
                  colClasses = c(censusBlockGroupFips = "character")) %>% 
  as_tibble()


# All CBG data:
# 1980: bg80_nomatchtract00weight
# 1990: bg10_nomatchtract90weight
# 2000: bg10_nomatchtract00weight
# 2020: bg20_nomatchtract00weight




gjoin_splitter <- function(var_name) {
  a = substr(var_name, 2, 3)
  b = substr(var_name, 5, 7)
  c = substr(var_name, 9, 15)
  return(str_flatten(c(a,b,c)))
}

gjoin_splitter("ancadasdadasdaadasds")

# # Create censusblockgroup
# exact90 <- bg10_nomatchtract90weight %>% filter(weight == 1) %>% 
#   mutate(censusBlockGroupFips = gjoin_splitter(blockgroup10)) %>% 
#   select(censusBlockGroupFips, tract90, weight)
# 
# exact00 <- bg10_nomatchtract00weight %>% filter(weight == 1) %>% 
#   mutate(censusBlockGroupFips = gjoin_splitter(blockgroup10)) %>% 
#   select(censusBlockGroupFips, tract00, weight)
# 
# exact00 <- exact00 %>% mutate(len00 = nchar(censusBlockGroupFips))
# 
# exact20 <- bg20_nomatchtract00weight %>% filter(weight == 1) %>% 
#   mutate(censusBlockGroupFips = gjoin_splitter(blockgroup10)) %>% 
#   select(censusBlockGroupFips, tract20, weight)






# Checking which percentage of data is there
# fema90 %>% left_join(exact90) %>% summary()
# 
# fema00 %>% left_join(exact00) %>% summary()
# 
# fema20 %>% left_join(exact20) %>% summary()


# Checking for all stuff -- whats the match percentage

all90 <- read.csv("C:/Users/Asus/Box/Flood Damage PredictionProject/Dataset/bcg_tract90_weighted.csv") %>% as_tibble()

# all90 <- all90 %>% mutate(a = substr(blockgroup10, 2, 3),
#                           b = substr(blockgroup10, 5, 7),
#                           c = substr(blockgroup10, 9, 15)) %>% 
#   mutate(censusBlockGroupFips = paste0(a, b, c)) %>% 
#   group_by(censusBlockGroupFips) %>% 
#   summarize(tot = sum(weight)) %>% 
#   select(censusBlockGroupFips, tot) 

all00 <- read.csv("C:/Users/Asus/Box/Flood Damage PredictionProject/Dataset/bcg_tract00_weighted.csv") %>% as_tibble()

all20 <- read.csv("C:/Users/Asus/Box/Flood Damage PredictionProject/Dataset/bcg_tract20_weighted.csv") %>% as_tibble()

# all20 <- all20 %>% mutate(a = substr(blockgroup10, 2, 3),
#                           b = substr(blockgroup10, 5, 7),
#                           c = substr(blockgroup10, 9, 15)) %>% 
#   mutate(censusBlockGroupFips = paste0(a, b, c)) %>% 
#   group_by(censusBlockGroupFips) %>% 
#   summarize(tot = sum(weight)) %>% 
#   select(censusBlockGroupFips, tot) 

# Now need to match required values with the cwts stuff.. weighted

# 1990


# Variable list
# TOTAL POPULATION - population
# PERSON BY RACE - By Sum
# TOTAL HOUSING UNIT - By Sum
# HOUSING UNIT BY URBAN/RURAL STATUS - By Sum

# MEDIAN HH INCOME IN PREVIOUS YEAR - Weighted mean

# TO GET:
# AREA BY CENSUS TRACT (1980, 1990, 2000, 2010, 2020)



cwts <- read.csv("C:/Users/Asus/Box/Flood Damage PredictionProject/Dataset/nghis census tract/nhgisCensusTract_csv/nhgis0009_csv/nhgis0009_ts_nominal_tract.csv") %>% as_tibble()

empty_counts <- colSums(cwts == "")

# values in 80
# all is 11 digits
all80 <- read.csv("C:/Users/Asus/Box/Flood Damage PredictionProject/Dataset/nghis census tract/nhgisCensusTract_csv/nhgis0009_csv/crosswalk_1980_2010.csv", colClasses = c(trtid80 = "character", trtid10 = "character")) %>% 
  as_tibble() %>% 
  mutate(nchar80 = nchar(trtid80),
         nchar10 = nchar(trtid10))

# cwts_80 <- cwts_80 %>% mutate(nchar80c = nchar(GJOIN1980)) 

all80 <- all80 %>% mutate(nchar80 = nchar(trtid80), nchar10 = nchar(trtid10)) %>% 
  as_tibble() %>% 
  mutate(
    state_fips = substr(trtid10, 1, 2),
    county_fips = substr(trtid10, 3, 5),
    census_block_group = substr(trtid10, 6, 11)
  ) %>% mutate(censusTract10 = paste("G", state_fips, "0", county_fips, "0", census_block_group, sep = "")) %>% 
  mutate(
    state_fips = substr(trtid10, 1, 2),
    county_fips = substr(trtid10, 3, 5),
    census_block_group = substr(trtid10, 6, 11)
  ) %>% mutate(GJOIN1980 = paste("G", state_fips, "0", county_fips, "0", census_block_group, sep = "")) %>% 
  select(GJOIN1980, censusTract10, weight)

# Checking if the weight add up to 100 by census tract10 (it doesn't)
all80 %>% group_by(censusTract10) %>% summarize(tot_weight = sum(weight))


# This one is super-approximation as there's no good crosswalk between 1980 and 2010 data
# cwts_80 <- cwts %>% select(AV0AA1980, GJOIN1980, GJOIN1980, B18AA1980, B18AB1980, 
#                           B18AC1980,
#                           B18AD1980, AZ7AA1980, AZ7AD1980, B79AA1980, A41AA1980,
#                           ) %>% 
#  filter(GJOIN1980 != "") %>% 
#  rename(population = AV0AA1980,
#         populationWhite = B18AA1980,
#         populationBlack = B18AB1980,
#         populationIndian = B18AC1980,
#         populationAsian = B18AD1980,
#         housingUrban = AZ7AA1980,
#         housingRural = AZ7AD1980,
#         medianIncome = B79AA1980,
#         housingTotal = A41AA1980) %>% 
#  mutate(exist = 1)


# values in 90
cwts_90 <- cwts %>% select(AV0AA1990, GJOIN1990, B18AA1990, B18AB1990, B18AC1990,
                           B18AD1990, AZ7AA1990, AZ7AD1990, B79AA1990, A41AA1990,
) %>% 
  filter(GJOIN1990 != "") %>% 
  rename(population = AV0AA1990,
         populationWhite = B18AA1990,
         populationBlack = B18AB1990,
         populationIndian = B18AC1990,
         populationAsian = B18AD1990,
         housingUrban = AZ7AA1990,
         housingRural = AZ7AD1990,
         medianIncome = B79AA1990,
         housingTotal = A41AA1990) %>% 
  mutate(exist = 1)

# Changing population into percentage
cwts_90 <- cwts_90 %>% 
  mutate(population_tot = populationWhite + populationBlack + populationIndian + populationAsian) %>% 
  mutate(percpopulationWhite = populationWhite/population_tot,
         percpopulationBlack = populationBlack/population_tot,
         percpopulationIndian = populationIndian/population_tot,
         percpopulationAsian = populationAsian/population_tot)





# Joining with the value (checking)
all90 %>% rename(GJOIN1990 = tract90) %>% left_join(cwts_90)

# values in 00
cwts_00 <- cwts %>% select(AV0AA2000, GJOIN2000, B18AA2000, B18AB2000, B18AC2000,
                           B18AD2000, AZ7AA2000, AZ7AD2000, B79AA2000, A41AA2000,
                           B18AE2000
) %>% 
  filter(GJOIN2000 != "") %>% 
  rename(population = AV0AA2000,
         populationWhite = B18AA2000,
         populationBlack = B18AB2000,
         populationIndian = B18AC2000,
         populationAsian = B18AD2000,
         populationDualRace = B18AE2000,
         housingUrban = AZ7AA2000,
         housingRural = AZ7AD2000,
         medianIncome = B79AA2000,
         housingTotal = A41AA2000) %>% 
  mutate(exist = 1)

# TODO!
# values in 2010 onwards (need some flip-join maybe)


# Also to do: Obtain values of LAND_AREA and median house value in these areas:

sqmt_to_sqmiles = 3.86102e-7


# Normal methods for 1990 - 2020 -> Approximate value for each blockgroup10 based on tract values
df90 <- read.csv("C:/Users/Asus/Box/Flood Damage PredictionProject/Dataset/nghis census tract/nhgisCensusTract_csv/nhgis0009_csv/nhgis0009_ds120_1990_tract.csv") %>% as_tibble() %>% 
  select(GISJOIN, AREALAND, EST001) %>% 
  rename(LAND_AREA = AREALAND,
         
         Value = EST001) %>% 
  mutate(LAND_AREA = LAND_AREA*sqmt_to_sqmiles)

df00 <- read.csv("C:/Users/Asus/Box/Flood Damage PredictionProject/Dataset/nghis census tract/nhgisCensusTract_csv/nhgis0009_csv/nhgis0009_ds151_2000_tract.csv") %>% as_tibble() %>% 
  select(GISJOIN, AREALAND, GB7001) %>% 
  rename(LAND_AREA = AREALAND,
         
         Value = GB7001) %>% 
  mutate(LAND_AREA = LAND_AREA*sqmt_to_sqmiles)

df10 <- read.csv("C:/Users/Asus/Box/Flood Damage PredictionProject/Dataset/nghis census tract/nhgisCensusTract_csv/nhgis0009_csv/pdb2012bgv9_us.csv" , colClasses = c(GIDBG = "character")) %>% as_tibble() %>% 
  select(GIDBG, LAND_AREA, Med_house_val_tr_ACS_06_10) %>% 
  rename(
    Value = Med_house_val_tr_ACS_06_10) %>% 
  mutate(
    Value = parse_number(
    Value)) %>% 
  mutate(gidbg = str_sub(GIDBG, 1, 11)) %>% mutate(
    state_fips = substr(gidbg, 1, 2),
    county_fips = substr(gidbg, 3, 5),
    census_tract = substr(gidbg, 6, 11)
  ) %>% 
  mutate(GISJOIN = paste("G", state_fips, "0", county_fips, "0", census_tract, sep = "")) %>% 
  select(gidbg, GISJOIN, LAND_AREA, 
         Value) %>% 
  group_by(GISJOIN) %>% summarize(LAND_AREA = sum(LAND_AREA), 
                                  
                                  Value = mean(
                                    Value))

               
df20 <- read.csv("C:/Users/Asus/Box/Flood Damage PredictionProject/Dataset/nghis census tract/nhgisCensusTract_csv/nhgis0009_csv/pdb2022tr.csv", colClasses = c(GIDTR = "character")) %>% as_tibble() %>% 
  select(GIDTR, LAND_AREA, Med_House_Value_ACS_16_20) %>% 
  mutate(
    Value = parse_number(Med_House_Value_ACS_16_20)) %>%
  mutate(
    nchar = char(GIDTR),
    state_fips = substr(GIDTR, 1, 2),
    county_fips = substr(GIDTR, 3, 5),
    census_tract = substr(GIDTR, 6, 11)
  ) %>% mutate(GISJOIN = paste("G", state_fips, "0", county_fips, "0", census_tract, sep = "")) %>% 
  select(GISJOIN, LAND_AREA, 
         Value)
  

# Check if the combination for 1990 and 2000 data works

# Correcting the name convention for all
all90 <- all90 %>% rename(GJOIN1990 = tract90) %>% 
  select(blockgroup10, GJOIN1990, weight)

df90 <- df90 %>% rename(GJOIN1990 = GISJOIN)

all00 <- all00 %>% rename(GJOIN2000 = tract00) %>% 
  select(blockgroup10, GJOIN2000, weight)

df00 <- df00 %>% rename(GJOIN2000 = GISJOIN)

# Writing 90's data
full_90_data <- all90 %>% left_join(df90) %>% left_join(cwts_90) %>% 
  mutate(Year = 1990)

write.csv(full_90_data, "C:/Users/Asus/Box/Flood Damage PredictionProject/Dataset/nghis census tract/nhgisCensusTract_csv/nhgis0009_csv/all90_data.csv")

# Writing 00's data
full_00_data <- all00 %>% left_join(df00) %>% left_join(cwts_00) %>% 
  mutate(Year = 2000)
write.csv(full_00_data, "C:/Users/Asus/Box/Flood Damage PredictionProject/Dataset/nghis census tract/nhgisCensusTract_csv/nhgis0009_csv/all00_data.csv")

# Writing 10's to 19's data -> More based on CWTS
# Idea: Use 2010 census tract, extract all relevant variables, and pivot to widelong

cwts10 <- cwts %>% filter(GJOIN2010 != "") %>% 
  select(ends_with("5"), GJOIN2010, AZ7AA2010, AZ7AD2010) %>%
  select(-GJOIN2015, -NAME2015) %>% 
  select(starts_with(c("AV","B18", "AZ7", "B79", "A41")), GJOIN2010)

cwts10 <- cwts10 %>% 
  rename_with(~gsub("AV0AA(\\d+)", "population_\\1", .x),
                  starts_with("AV0AA")
  ) %>% 
  rename_with(~gsub("B18AA(\\d+)", "populationWhite_\\1", .x),
              starts_with("B18AA")) %>% 
  rename_with(~gsub("B18AB(\\d+)", "populationBlack_\\1", .x),
              starts_with("B18AB")) %>% 
  rename_with(~gsub("B18AC(\\d+)", "populationIndian_\\1", .x),
              starts_with("B18AC")) %>% 
  rename_with(~gsub("B18AD(\\d+)", "populationAsian_\\1", .x),
              starts_with("B18AD")) %>% 
  rename_with(~gsub("B18AE(\\d+)", "populationDualRace_\\1", .x),
              starts_with("B18AE")) %>% 
  rename_with(~gsub("AZ7AA(\\d+)", "housingUrban_\\1", .x),
              starts_with("AZ7AA")) %>% 
  rename_with(~gsub("AZ7AD(\\d+)", "housingRural_\\1", .x),
              starts_with("AZ7AD")) %>% 
  rename_with(~gsub("B79AA(\\d+)", "medianIncome_\\1", .x),
              starts_with("B79AA")) %>% 
  rename_with(~gsub("A41AA(\\d+)", "housingTotal_\\1", .x),
              starts_with("A41AA"))

cwts_10 <- cwts10 %>% pivot_longer(ends_with(c("5")),
                         names_to = c(".value", "Group"),
                         names_sep = 
                           "_") %>% 
  mutate(Group = as.integer(Group)) %>% 
  mutate(Year = (Group - 5)/10 + 2000) %>% 
  rename(GISJOIN = GJOIN2010)

full_10_data <- cwts_10 %>% left_join(df10) %>% 
  filter(Year < 2020)

write.csv(full_10_data, "C:/Users/Asus/Box/Flood Damage PredictionProject/Dataset/nghis census tract/nhgisCensusTract_csv/nhgis0009_csv/all10_data.csv")
                         
# For 2020 and 2021
missing_values <- colSums(is.na(cwts))

cwts20 <- cwts %>% filter(GJOIN2020 != "") %>% 
  select(ends_with("5"), GJOIN2020) %>%
  select(-GJOIN2015, -NAME2015) %>% 
  select(starts_with(c("AV","B18", "AZ7", "B79", "A41")), GJOIN2020)


cwts20 <- cwts20 %>% 
  rename_with(~gsub("AV0AA(\\d+)", "population_\\1", .x),
              starts_with("AV0AA")
  ) %>% 
  rename_with(~gsub("B18AA(\\d+)", "populationWhite_\\1", .x),
              starts_with("B18AA")) %>% 
  rename_with(~gsub("B18AB(\\d+)", "populationBlack_\\1", .x),
              starts_with("B18AB")) %>% 
  rename_with(~gsub("B18AC(\\d+)", "populationIndian_\\1", .x),
              starts_with("B18AC")) %>% 
  rename_with(~gsub("B18AD(\\d+)", "populationAsian_\\1", .x),
              starts_with("B18AD")) %>% 
  rename_with(~gsub("B18AE(\\d+)", "populationDualRace_\\1", .x),
              starts_with("B18AE")) %>% 
  rename_with(~gsub("AZ7AA(\\d+)", "housingUrban_\\1", .x),
              starts_with("AZ7AA")) %>% 
  rename_with(~gsub("AZ7AD(\\d+)", "housingRural_\\1", .x),
              starts_with("AZ7AD")) %>% 
  rename_with(~gsub("B79AA(\\d+)", "medianIncome_\\1", .x),
              starts_with("B79AA")) %>% 
  rename_with(~gsub("A41AA(\\d+)", "housingTotal_\\1", .x),
              starts_with("A41AA"))

  
cwts_20 <- cwts20 %>% pivot_longer(ends_with(c("5")),
                                   names_to = c(".value", "Group"),
                                   names_sep = 
                                     "_") %>% 
  mutate(Group = as.integer(Group)) %>% 
  mutate(Year = (Group - 5)/10 + 2000) %>% 
  rename(GISJOIN = GJOIN2020)

all21 <- all20

cwts_20_only <- cwts_20 %>% filter(Year == 2020)
cwts_21_only <- cwts_20 %>% filter(Year == 2021)
  
full_20_data <- all20 %>% rename(GISJOIN = tract20) %>% 
  left_join(cwts_20_only) %>% left_join(df20) %>% 
  select(-Group)

write.csv(full_20_data, "CC:/Users/Asus/Box/Flood Damage PredictionProject/Dataset/nghis census tract/nhgisCensusTract_csv/nhgis0009_csv/all20_data.csv")



full_21_data <- all21 %>% rename(GISJOIN = tract20) %>% 
  left_join(cwts_21_only) %>% left_join(df20)

write.csv(full_21_data, "C:/Users/Asus/Box/Flood Damage PredictionProject/Dataset/nghis census tract/nhgisCensusTract_csv/nhgis0009_csv/all21_data.csv")




# data22 <- read.csv("C:/Users/Asus/Box/Flood Damage PredictionProject/Dataset/nghis census tract/nhgisCensusTract_csv/nhgis0009_csv/pdb2022tr.csv", colClasses = c(GIDTR = "character"))


# Record which CBG/LAT-LONG Combination in FEMA is multi-tract
# And if it can be singled out to one tract


# Going by subset of FEMA data

fema90 <- data2 %>% select(yearOfLoss, censusBlockGroupFips, latitude, longitude) %>% 
  filter(yearOfLoss >= 1990 & yearOfLoss < 2000)

fema00 <- data2 %>% select(yearOfLoss, censusBlockGroupFips, latitude, longitude) %>% 
  filter(yearOfLoss >= 2000 & yearOfLoss < 2010)

fema20 <- data2 %>% select(yearOfLoss, censusBlockGroupFips, latitude, longitude) %>% 
  filter(yearOfLoss >= 2020 & yearOfLoss < 2029)

# For 1990 dataset

weight_nonmatch90 <- all90 %>% filter(weight != 1)
non_match90 <- weight_nonmatch90 %>% group_by(blockgroup10) %>% 
  summarize(n = n(), tot_weight = sum(weight))

weight_nonmatch00 <- all00 %>% filter(weight != 1)
non_match00 <- weight_nonmatch00 %>% group_by(blockgroup10) %>% 
  summarize(n = n(), tot_weight = sum(weight))

weight_nonmatch20 <- all20 %>% filter(weight != 1)
non_match20 <- weight_nonmatch20 %>% group_by(blockgroup10) %>% 
  summarize(n = n(), tot_weight = sum(weight))


# Creating fema 90, 00, and 20 to become GIS mode and joining with the dataset
fema90_GIS <- fema90 %>%   mutate(
  nchar = nchar(censusBlockGroupFips),
  state_fips = substr(censusBlockGroupFips, 1, 2),
  county_fips = substr(censusBlockGroupFips, 3, 5),
  census_block_group = substr(censusBlockGroupFips, 6, 12)
) %>% mutate(blockgroup10 = paste("G", state_fips, "0", county_fips, "0", census_block_group, sep = ""))


fema90_GIS <- fema90_GIS %>% left_join(non_match90) %>% filter(!is.na(tot_weight))

write.csv(fema90_GIS, "C:/Users/Asus/Box/Flood Damage PredictionProject/Dataset/nghis census tract/nhgisCensusTract_csv/nhgis0009_csv/multi_tract90_confirm.csv")


fema00_GIS <- fema00 %>%   mutate(
  nchar = nchar(censusBlockGroupFips),
  state_fips = substr(censusBlockGroupFips, 1, 2),
  county_fips = substr(censusBlockGroupFips, 3, 5),
  census_block_group = substr(censusBlockGroupFips, 6, 12)
) %>% mutate(blockgroup10 = paste("G", state_fips, "0", county_fips, "0", census_block_group, sep = ""))

fema00_GIS <- fema00_GIS %>% left_join(non_match00) %>% filter(!is.na(tot_weight))

write.csv(fema00_GIS, "C:/Users/Asus/Box/Flood Damage PredictionProject/Dataset/nghis census tract/nhgisCensusTract_csv/nhgis0009_csv/multi_tract00_confirm.csv")


fema20_GIS <- fema20 %>% mutate(
  nchar = nchar(censusBlockGroupFips),
  state_fips = substr(censusBlockGroupFips, 1, 2),
  county_fips = substr(censusBlockGroupFips, 3, 5),
  census_block_group = substr(censusBlockGroupFips, 6, 12)
) %>% mutate(blockgroup10 = paste("G", state_fips, "0", county_fips, "0", census_block_group, sep = ""))

fema20_GIS <- fema20_GIS %>% left_join(non_match20) %>% filter(!is.na(tot_weight))

write.csv(fema20_GIS, "C:/Users/Asus/Box/Flood Damage PredictionProject/Dataset/nghis census tract/nhgisCensusTract_csv/nhgis0009_csv/multi_tract20_confirm.csv")


# df90 <- read.csv("C:/Users/Asus/Box/Flood Damage PredictionProject/Dataset/nghis census tract/nhgisCensusTract_csv/nhgis0009_csv/nhgis_ds104_1980_block_all/nhgis_ds104_1980_block_all.csv")

# df80 <- as_tibble(df90)

# Lets do some stuff for the 1980's crosswalk using this publications:
# https://forum.ipums.org/t/can-census-tracts-from-1980-1990-and-2000-be-linked/4745

# As we have no LAND-AREA which is crucial for 1980, we use different methods
# comprared to other stuff by simulating the values to 2010's


# Lets use 80's dataset using the new dataset: Code from different laptop copy pasted


multi20 <- read.csv("C:/Users/Asus/Box/Flood Damage PredictionProject/Dataset/nghis census tract/nhgisCensusTract_csv/nhgis0009_csv/multi_tract00_confirm.csv") %>% as_tibble()

# Example of dataset for 00's 
all00 <- read.csv("C:/Users/Asus/Box/Flood Damage PredictionProject/Dataset/nghis census tract/nhgisCensusTract_csv/nhgis0009_csv/all00_data.csv") %>% as_tibble()


all80 <- read.csv("C:/Users/Asus/Box/Flood Damage PredictionProject/Dataset/nghis census tract/nhgisCensusTract_csv/nhgis0009_csv/LTDB_Std_1980_fullcount.csv", colClasses = c(TRTID10 = "character")) %>% 
  as_tibble()

df80 <- read.csv("crosswalk_1980_2010.csv",   
                 colClasses = c(trtid80 = "character",
                                trtid10 = "character")) %>% as_tibble()

all80sam <- read.csv("LTDB_Std_1980_Sample.csv", 
                     colClasses = c(trtid10 = "character")) %>% as_tibble()

# Got a decent-ish data value 

fema80 <- data2 %>% select(yearOfLoss, censusBlockGroupFips, latitude, longitude) %>% 
  filter(yearOfLoss < 1990)

# just to check how much fema trtid80 have values
fema80_test <- fema80 %>% mutate(trtid10 = substr(censusBlockGroupFips, 1, 11)) %>% 
  group_by(trtid10) %>% summarize(n = n())

df80 %>%  group_by(trtid80) %>% summarize(tot_weight = sum(weight))

weight_80_data <- fema80_test %>% left_join(df80)

fema80 <- fema80 %>% mutate(len = nchar(censusBlockGroupFips))

# Lets extract important variable first
# Actually, lets just use 10's w/o crosswalks...


all80_new <- all80 %>% rename(
  populationWhite = NHWHT80,
  populationIndian = NTV80,
  populationBlack = NHBLK80,
  populationAsian = ASIAN80,
  population = POP80,
  Value = MHMVAL80,
  trtid10 = TRTID10) %>% 
  mutate(population_tot = populationWhite + populationBlack + populationIndian + populationAsian) %>% 
  mutate(population_diff = population_tot - population) %>%  #for some reason, there is different value for this?
  mutate(percpopulationWhite = populationWhite/population_tot,
         percpopulationBlack = populationBlack/population_tot,
         percpopulationIndian = populationIndian/population_tot,
         percpopulationAsian = populationAsian/population_tot) %>% 
  select(trtid10, percpopulationWhite, percpopulationBlack, percpopulationIndian, percpopulationAsian, population, Value) %>% 
  mutate(len = nchar(trtid10)) %>% 
  mutate(trtid10 = ifelse(len < 11, str_c("0", trtid10), trtid10)) 



all80sam_new <- all80sam %>% rename(
  medianIncome = hinc80
) %>% 
  mutate(Year = 1980) %>% 
  mutate(exist = 1) %>% 
  select(medianIncome, Year, exist, trtid10, hh80) %>%
  rename(house = hh80) %>% 
  mutate(len = nchar(trtid10)) %>% 
  mutate(trtid10 = ifelse(len < 11, str_c("0", trtid10), trtid10)) 


area <- read.csv("C:/Users/Asus/Box/Flood Damage PredictionProject/Dataset/nghis census tract/nhgisCensusTract_csv/nhgis0009_csv/pdb2012bgv9_us.csv") %>% as_tibble() %>% 
  mutate(len = nchar(GIDBG)) %>% 
  mutate(GIDBG = ifelse(len < 12, str_c("0", GIDBG), GIDBG)) %>% 
  mutate(len2 = nchar(GIDBG)) %>%   
  mutate(trtid10 = substr(GIDBG, 1, 11)) %>% 
  group_by(trtid10) %>% summarize(LAND_AREA = sum(LAND_AREA))

# Joining all the dataset
all80_new <- all80_new %>% left_join(all80sam_new) %>% left_join(area) %>% 
  mutate(len2 = nchar(trtid10)) %>% select(-len)

# Final adjustment to standarize each year

# No median housing value (Despite saying there is one)
all80_final <- all80_new %>% mutate(
  populationDensity = population/LAND_AREA,
  housingDensity = house/LAND_AREA
) %>% 
  select(percpopulationWhite,
         percpopulationBlack,
         percpopulationIndian,
         percpopulationAsian,
         populationDensity,
         housingDensity,
         medianIncome,
         Year,
         trtid10)

write.csv(all80_final, "C:/Users/Asus/Box/Flood Damage PredictionProject/Dataset/nghis census tract/nhgisCensusTract_csv/nhgis0009_csv/all80_data.csv")


# TODO: *1000 for area in census 1990
all90_final <- full_90_data %>% 
  mutate(LAND_AREA = 1000*LAND_AREA, # Adjusting for conversion
         populationDensity = population/LAND_AREA,
         housingDensity = housingTotal/LAND_AREA) %>% 
  select(percpopulationWhite,
         percpopulationBlack,
         percpopulationIndian,
         percpopulationAsian,
         populationDensity,
         housingDensity,
         medianIncome,
         Year,
         blockgroup10,
         GJOIN1990)


fema80new <- fema80 %>% mutate(cbgexist = ifelse(len == 0, 0, 1)) %>% 
  mutate(trtid10 = substr(censusBlockGroupFips, 1, 11))

final_data  <- fema80new %>% left_join(all80_final)