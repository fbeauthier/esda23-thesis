#install.packages("haven") # Install haven package to read .sav
#install.packages("sae") # required for SAE method to estimate values missing for OAs

# load packages
library(haven)
library(ggpubr)
library(tidyverse)
library(qacEDA)
library(tmap)
library(sf)
library(gstat)
library(missRanger)
library(naniar)
library(VIM)
library(lme4)

### master look-up OA to LSOA to MSOA ------------------------------------
master_lookup <- read_csv("data/OA_LSOA_MSOA_lookup.csv")
# remove duplicates from OA
master_lookup <- master_lookup %>%
  distinct(COA_CODE, .keep_all = TRUE)

### OxIS: OAs - digital skills, use frequency ---------------------------------
## 1. load raw data (requires OxIS 2019 .sav data)
oxis_raw <- read_sav("data/OxIS2019/oxis_2019_public.sav")
#write.csv(oxis_raw, "data/oxis_raw.csv")

colnames(oxis_raw)
length(unique(oxis_raw$MATCHED)) # 396 OAs in GB OxIS sample

# OA look-up 2011
lookup <- read_csv("data/2011_OAC_Lookup.csv")
lookup <- dplyr::select(lookup, 1:8)
colnames(lookup)
unique(lookup$REGION_OR_COUNTRY_NAME)


## 2. keep OAs for England only
lookup <- dplyr::filter(lookup, !REGION_OR_COUNTRY_NAME %in% c("Northern Ireland", "Scotland", "Wales"))
unique(lookup$REGION_OR_COUNTRY_NAME) # check
# 171,372 OAs for England in total

## OxIS data for England OAs only ##
oxis_england <- dplyr::filter(oxis_raw,
                              MATCHED %in% lookup$OA) # 1,563 observations (people)

length(unique(oxis_england$MATCHED)) # 342 OAs kept for England only in OxIS dataset


## 3. view shapes for OxIS England OAs kept
# OAs shapefile
oa <- read_sf("data/oa_lyr_2011/infuse_oa_lyr_2011.shp")

oxis_england_sf <- oxis_england %>%
  select(MATCHED) %>%
  distinct() %>%
  left_join(y = oa, by = c("MATCHED" = "geo_code")) %>%
  st_as_sf()

# view map
tmap_mode("view")
tm_shape(oxis_england_sf) + tm_polygons(col = "black")


## 4. Estimate users' digital skills + use frequency for all OAs in England based on OxIS + Census

# count number of observations per OA
oxis_england %>%
  dplyr::group_by(MATCHED) %>% # matched OA
  dplyr::count() %>%
  #print(n = 20) %>%
  summary()
# 4-10 observations (individuals' responses) per OA (OAs typically 100-600 residents)

colnames(oxis_england)
# a. select + rename relevant variables related to digital skills + usage (see Google doc)
oxis_england <- oxis_england %>%
  dplyr::select(MATCHED, QD1_yrborn2, QD2_gender, QL1_dgorig, QO1_Labfor, QD16_disab, GOR, QD5_childhh, QD14_ethnic,
                QB2_actrel, QD3_marstat, QC6_u_ability, QC39_u_time, weight) %>%
  rename(OA = MATCHED,
         ageband = QD1_yrborn2,
         gender = QD2_gender,
         education = QL1_dgorig,
         employment_status = QO1_Labfor,
         disability = QD16_disab,
         region = GOR,
         children_hh = QD5_childhh,
         ethnicity = QD14_ethnic,
         religious = QB2_actrel,
         marital_status = QD3_marstat,
         digital_skills = QC6_u_ability,
         use_frequency = QC39_u_time)

colSums(is.na(oxis_england))

# save raw selected variables from OxIS as .csv
write_csv(oxis_england, "data/OxIS2019/oxis_england_sae_variables_raw.csv")


# read in oxis_england as csv
oxis_england <- read_csv("data/OxIS2019/oxis_england_sae_variables_raw.csv")

# mutate variables
str(oxis_england)
unique(oxis_england$religious)

oxis_england <- oxis_england %>%
  mutate(ageband = factor(ageband,
                          levels = c(1,2,3,4,5,6,7,8,-2),
                          labels = c("18-24","25-34","35-44","45-54","55-64","65-74","75-84","85+", -2)),
         gender = factor(gender,
                         levels = c(0,1),
                         labels = c("Male","Female")),
         disability = factor(disability),
         region = factor(region),
         religious = factor(religious))

# change levels
oxis_england <- oxis_england %>%
  mutate(education = factor(ifelse(education %in% c(0:17),0,  # No qualifications/Less than HS
                                          ifelse(education %in% c(18:26),1,  # A-levels/HS
                                                 ifelse(education == 27, 2,  # Undergraduate degree
                                                        ifelse(education %in% c(28,29),3, # Other
                                                               ifelse(education %in% c(30,31),4,-2)))))), # Post-graduate degree
         
         employment_status = factor(ifelse(employment_status %in% c(1,2), 1, # Employed
                                           ifelse(employment_status == 3, 2, # Retired
                                                  ifelse(employment_status == 4, 3, # Unemployed
                                                         ifelse(employment_status %in% c(5,6), 4, # Other
                                                                ifelse(employment_status %in% c(7:10), 5, # In Education
                                                                       ifelse(employment_status == 11, 4, -2))))))),  # Other
         ethnicity = factor(ifelse(ethnicity %in% c(1:5),1, # Asian
                                   ifelse(ethnicity %in% c(6:8),2, # Black
                                          ifelse(ethnicity %in% c(9:12),3, # White
                                                 ifelse(ethnicity == 13, 4, -2))))), # Other
         marital_status = factor(ifelse(marital_status %in% c(4,5), 4, marital_status))  # 4 = "Divorced/separated/widowed"
  )

# change digital skills and frequency variables to factors as well
oxis_england <- oxis_england %>%
  mutate(digital_skills = factor(digital_skills),
         use_frequency = factor(use_frequency ))

#unique(oxis_england$use_frequency)
summary(oxis_england)


# b. fill in missing data
# replace -2s and -3s with NA
oxis_england <- replace(oxis_england, oxis_england == -2, NA)
oxis_england <- replace(oxis_england, oxis_england == -3, NA)
colSums(is.na(oxis_england))

unique(oxis_england$ethnicity)

# re-define levels to remove -2s and -2s, just keep as NA
oxis_england <- oxis_england %>%
  mutate(ageband = factor(ageband, levels = c("18-24","25-34","35-44","45-54","55-64","65-74","75-84","85+")),
         education = factor(education, levels = c(0,1,2,3,4)),
         employment_status = factor(employment_status, levels = c(1,2,3,4,5)),
         disability = factor(disability, levels = c(0,1)),
         ethnicity = factor(ethnicity, levels = c(1,2,3,4)),
         marital_status = factor(marital_status, levels = c(1,2,3,4)),
         digital_skills = factor(digital_skills, levels = c(1,2,3,4,5)),
         use_frequency = factor(use_frequency, levels = c(1,2,3,4,5))
  )

# view missing data
gg_miss_var(oxis_england)
summary(aggr(oxis_england, sortVar=TRUE))$combinations

# use missRanger package: rpart to impute missing data
oxis_england$children_hh <- as.numeric(oxis_england$children_hh)

oxis_england_imputed <- missRanger(oxis_england, 
                                   formula = . ~ . -weight ,  # default uses all columns in the data set to impute all columns with NAs
                                   num.trees = 500, 
                                   verbose = 2, 
                                   seed = 123,  
                                   returnOOB = T)

# round children_hh
oxis_england_imputed$children_hh <- round(oxis_england_imputed$children_hh, digits = 1)
oxis_england_imputed$children_hh <- as.integer(oxis_england_imputed$children_hh)
summary(oxis_england_imputed)

# save as .csv raw (no counts/proportions) but NAs imputed
write_csv(oxis_england_imputed, "data/OxIS2019/oxis_england_sae_variables_raw_imputed.csv")
  
# c. create dummy variables for model
data <- read_csv("data/OxIS2019/oxis_england_sae_variables_raw_imputed.csv")
str(data)

unique(data$marital_status)
data <- data %>%
  mutate(education = factor(education, levels = c(0,1,2,3,4),
                            labels = c("no_quals", "secondary_education", "undergraduate", "other", "postgraduate")),
         employment_status = factor(employment_status, levels = c(1,2,3,4,5),
                                    labels = c("employed", "retired", "unemployed", "other", "in_education")),
         region = factor(region),
         have_children = ifelse(children_hh > 0, 1,0),
         ethnicity = factor(ethnicity, levels = c(1,2,3,4),
                            labels = c("asian", "black", "white", "other")),
         marital_status = factor(marital_status, levels = c(1,2,3,4),
                                 labels = c("single","married","live_w_partner","Divorced/separated/widowed"))
  )

data <- dplyr::select(data, -children_hh)

# create dummy vars
dummy_data <- dummy_cols(data, select_columns = c("ageband","gender", "education", "employment_status",
                                                  "region", "ethnicity", "marital_status"))

dummy_data <- dplyr::select(dummy_data,
                     -ageband, -gender, -education, -employment_status, -region, -ethnicity, -marital_status)

# save as .csv
write_csv(dummy_data, "data/OxIS2019/oxis_england_sae_imputed_dummy.csv")

# d. fit SAE models
# model 1 for digital skills
model1 <-lmer(digital_skills ~ . - OA - use_frequency - weight +(1|OA), # random intercept models for OAs
              data = dummy_data,
              weights = weight)
summary(model1)

# model 2 for online use frequency
model2 <-lmer(use_frequency ~ . - OA - digital_skills - weight +(1|OA), # random intercept models for OAs
              data = dummy_data,
              weights = weight)
summary(model2)

# Census 2021 auxiliary data


# predict
newdata$Predicted_Response <- predict(fit1, newdata = newdata)

### Census2021: LSOAs - gender, age, education, religion, language, ethnicity, disability -----
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

# proportion of LSOA population: male
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

# proportion of LSOA population over 65
LSOA$prop_over65 <- (LSOA$over65count)/(LSOA$population)
summary(LSOA$prop_over65)

## 3. Education: % of 16+ population with no qualifications (less than A-levels)
# over 16 population
df1$over16_flag <- ifelse(df1$`Age (6 categories) Code`>1, 1,0)

# left join onto LSOA: population over 16 per LSOA
LSOA <- df1 %>%
  group_by(`Lower layer Super Output Areas Code`, over16_flag) %>%
  summarise(over16_population = sum(Observation)) %>%
  filter(over16_flag == 1) %>%
  select(1,3) %>%
  left_join(x = LSOA, by = c("LSOA_code" = "Lower layer Super Output Areas Code"))

# over 16 population without qualifications
df1$over16noquals_flag <- ifelse((df1$over16_flag == 1)&(df1$`Highest level of qualification (7 categories) Code` %in% c(0,1,2)),
                                 1, 0)

# left join onto LSOA: over 16s no qualifications per LSOA
LSOA <- df1 %>%
  group_by(`Lower layer Super Output Areas Code`, over16noquals_flag) %>%
  summarise(over16_noquals_count = sum(Observation)) %>%
  filter(over16noquals_flag == 1) %>%
  select(1,3) %>%
  left_join(x = LSOA, by = c("LSOA_code" = "Lower layer Super Output Areas Code"))

# proportion over 16s no qualifications
LSOA$prop_over16s_noquals <- (LSOA$over16_noquals_count)/(LSOA$over16_population)
summary(LSOA$prop_over65)

## 4. Religiosity
df5$religious <- ifelse(df5$`Religion (10 categories) Code` %in% c(-8, 1, 9), 0, 1)

# left join onto LSOA: population per LSOA religious
LSOA <- df5 %>%
  group_by(`Lower layer Super Output Areas Code`, religious) %>%
  summarise(religious_count = sum(Observation)) %>%
  filter(religious == 1) %>%
  select(1,3) %>%
  left_join(x = LSOA, by = c("LSOA_code" = "Lower layer Super Output Areas Code"))

# proportion of LSOA population religious
LSOA$prop_religious <- (LSOA$religious_count)/(LSOA$population)
summary(LSOA$prop_religious)

## 5. Language
df4$english <- ifelse(df4$`Main language (11 categories) Code` == 1, 1,0)

# left join onto LSOA: population per LSOA English main language
LSOA <- df4 %>%
  group_by(`Lower layer Super Output Areas Code`, english) %>%
  summarise(eng_mainlang_count = sum(Observation)) %>%
  filter(english == 1) %>%
  select(1,3) %>%
  left_join(x = LSOA, by = c("LSOA_code" = "Lower layer Super Output Areas Code"))

# proportion LSOA population: English main language
LSOA$prop_eng_mainlang <- (LSOA$eng_mainlang_count)/(LSOA$population)
summary(LSOA$prop_eng_mainlang)

## 6. Ethnic diversity
# Define white = English, Welsh, Scottish, Northern Irish, British or Irish
# Everything else is "Other" including White: Gypsy or Irish Traveler, Roma
df3$white <- ifelse(df3$`Ethnic group (8 categories) Code` %in% c(4,5) ,1,0)

# left join onto LSOA: population per LSOA white
LSOA <- df3 %>%
  group_by(`Lower layer Super Output Areas Code`, white) %>%
  summarise(count_white = sum(Observation)) %>%
  filter(white == 1) %>%
  select(1,3) %>%
  left_join(x = LSOA, by = c("LSOA_code" = "Lower layer Super Output Areas Code"))

# proportion of LSOA population ethnicity white
LSOA$prop_white <- (LSOA$count_white)/(LSOA$population)
summary(LSOA$prop_white)

## 7. Disability
df2$disability <- ifelse(df2$`Disability (3 categories) Code` == 1, 1, 0)

# left join onto LSOA: population per LSOA with disability
LSOA <- df2 %>%
  group_by(`Lower layer Super Output Areas Code`, disability) %>%
  summarise(disability_count = sum(Observation)) %>%
  filter(disability == 1) %>%
  select(1,3) %>%
  left_join(x = LSOA, by = c("LSOA_code" = "Lower layer Super Output Areas Code"))

# proportion of LSOA population disabled
LSOA$prop_disabled <- (LSOA$disability_count)/(LSOA$population)
summary(LSOA$prop_disabled)

# save as .csv
#write_csv(LSOA, "data/census21_variables.csv")


### IoD2019: income ; join to census variables --------------------------------
iod2019_income <- read_csv("data/2019_IoD_income.csv")
summary(iod2019_income)

# read processed census variables
census <- read_csv("data/census21_variables.csv")

# join income data to census data
census <- iod2019_income %>%
  dplyr::select(1,5) %>%
  left_join(x = census, by = c("LSOA_code" = "LSOA code (2011)"))

# rename income
census <- rename(census,
                 income_dep_rate = `Income Score (rate)`)

colSums(is.na(census))

# impute missing income points using missRanger
census <- missRanger(census, 
                     formula = income_dep_rate ~ . -LSOA_code -LSOA_name,  # default uses all columns in the data set to impute all columns with NAs
                     num.trees = 100, 
                     verbose = 2, 
                     seed = 123,  
                     returnOOB = T)

summary(census$income_dep_rate)

### Point-Topic BII ----------------------------------------------------------
bii <- read_csv("data/PointTopic/BIIquery.csv")
histograms(bii)
summary(bii)
str(bii)

dplyr::filter(bii[c(1,7,8,9)], is.na(DOWN_MBPS))

bii <- na.omit(bii)

# normalise all to 0-1 scale using min-max:
# outliers don't get weighted as much

# define min-max normalisation function
min_max_norm <- function(x){
  (x - min(x)) / (max(x) - min(x))
}
# apply normalisation
bii_norm <- as.data.frame(lapply(bii[c("OPERATOR_COUNT", "DOWN_MBPS", "UP_MBPS")], min_max_norm))


