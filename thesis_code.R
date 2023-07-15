#install.packages("haven") # Install haven package to read .sav

# load packages
library(haven)
library(ggpubr)
library(tidyverse)
library(qacEDA)
library(tmap)
library(osmdata)
library(sf)
library(gstat)


### OxIS: OAs -----------------------------------------------------------------
# read OxIS .sav data
oxis_raw <- read_sav("data/oxis_2019_public.sav")
#write.csv(oxis_raw, "data/oxis_raw.csv")

colnames(oxis_raw)
length(unique(oxis_raw$MATCHED)) # 396 OAs in GB OxIS sample

# OA lookup 2011
lookup <- read_csv("data/2011_OAC_CSV_Lookup.csv")
lookup <- select(lookup, 1:8)
colnames(lookup)
unique(lookup$REGION_OR_COUNTRY_NAME)

# keep OAs for England only
lookup <- filter(lookup, !REGION_OR_COUNTRY_NAME %in% c("Northern Ireland", "Scotland", "Wales"))
# 171,372 OAs for England in total

oxis_england <- inner_join(oxis_raw, lookup, by = c("MATCHED" = "OA"))
# check only England areas kept
unique(oxis_england$REGION_OR_COUNTRY_NAME)
length(unique(oxis_england$MATCHED)) # 342 OAs kept for England only in OxIS dataset

# OAs shapefile
oa <- read_sf("data/oa_lyr_2011/infuse_oa_lyr_2011.shp")

colnames(oxis_england)
# view shapes for OxIS england OAs
oxis_england_sf <- oxis_england %>%
  select(MATCHED) %>%
  distinct() %>%
  left_join(y = oa, by = c("MATCHED" = "geo_code")) %>%
  st_as_sf()

# view
tmap_mode("view")
tm_shape(oxis_england_sf) + tm_polygons(col = "black")


  


### Census2021: LSOAs ---------------------------------------------------------
# 33,755 LSOAs in England: Census 2021
# England total population 56.5 million

# requires Census2021 data
df1 <- read_csv("data/Census2021/census_age_gender_edu.csv")
df2 <- read_csv("data/Census2021/census_disability.csv")
df3 <- read_csv("data/Census2021/census_ethnicity.csv")
df4 <- read_csv("data/Census2021/census_language.csv")
df5 <- read_csv("data/Census2021/census_religion.csv")

# check all LSOAs present in all tables
# length(unique(df1$`Lower layer Super Output Areas Code`))
# length(unique(df2$`Lower layer Super Output Areas Code`))
# length(unique(df3$`Lower layer Super Output Areas Code`))
# length(unique(df4$`Lower layer Super Output Areas Code`))
# length(unique(df5$`Lower layer Super Output Areas Code`))

# total population
# sum(df1$Observation)

colnames(df1)
# create new LSOA table 
LSOA <- df1 %>%
  select(1, 2, 9) %>%
  group_by(`Lower layer Super Output Areas Code`, `Lower layer Super Output Areas`) %>%
  summarise(population = sum(Observation)) %>%
  ungroup()

# rename columns
LSOA <- rename(LSOA,
               LSOA_code = `Lower layer Super Output Areas Code`,
               LSOA_name = `Lower layer Super Output Areas`)

## 1. gender
# left join onto LSOA: count of males in LSOA
LSOA <- df1 %>%
  group_by(`Lower layer Super Output Areas Code`, `Sex (2 categories) Code`) %>%
  summarise(male_count = sum(Observation)) %>%
  filter(`Sex (2 categories) Code` == 2) %>%
  select(1, 3) %>%
  left_join(x = LSOA, by = c("LSOA_code" = "Lower layer Super Output Areas Code"))

# proportion of LSOA male
LSOA$prop_male <- (LSOA$male_count)/(LSOA$population)
summary(LSOA$prop_male)

## 2. age
# create over65 binary variable
df1$over65 <- ifelse(df1$`Age (6 categories) Code` == 6,1,0)
  
# left join onto LSOA: count of over65s per LSOA
LSOA <- df1 %>%
  group_by(`Lower layer Super Output Areas Code`, over65) %>%
  summarise(over65count = sum(Observation)) %>%
  filter(over65 == 1) %>%
  select(1,3) %>%
  left_join(x = LSOA, by = c("LSOA_code" = "Lower layer Super Output Areas Code"))

# proportion of LSOA over 65
LSOA$prop_over65 <- (LSOA$over65count)/(LSOA$population)
summary(LSOA$prop_over65)

## 3. Education (% of 16+ population with no qualifications (less than A-levels)



# Religiosity

# Language

# Ethnic diversity

# Disability
df2$disability <- ifelse(df2$`Disability (3 categories) Code` == 1, 1, 0)

LSOA <- df2 %>%
  group_by(`Lower layer Super Output Areas Code`, disability) %>%
  summarise(disability_count = sum(Observation)) %>%
  filter(disability == 1) %>%
  select(-2) %>%
  left_join(x = LSOA, by = c("LSOA_code" = "Lower layer Super Output Areas Code"))



