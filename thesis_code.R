#install.packages("haven") # Install haven package to read .sav
#install.packages("sae") # required for SAE method to estimate values missing for OAs

# load packages
library(haven)
library(ggpubr)
library(tidyverse)
library(qacEDA)
library(gstat)
library(missRanger)
library(naniar)
library(VIM)
library(lme4)
library(lmerTest)
library(relaimpo)

### master look-up OA to LSOA to MSOA ------------------------------------
master_lookup <- read_csv("data/OA_LSOA_MSOA_lookup.csv")
# remove duplicates from OA
master_lookup <- master_lookup %>%
  distinct(COA_CODE, .keep_all = TRUE)

### OxIS: OAs - estimate 1) digital skills, 2) use frequency ------------------
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


# read OA Census auxiliary data
oa_age <- read_csv("data/census_oa/age_oa.csv")
oa_gender <- read_csv("data/census_oa/gender_oa.csv")
oa_employment <- read_csv("data/census_oa/employment_oa.csv")
oa_ethnicity <- read_csv("data/census_oa/ethnicity_oa.csv")
oa_education <- read_csv("data/census_oa/oa_education.csv")
oa_marital <- read_csv("data/census_oa/oa_maritalstatus.csv")
length(unique(oa_marital$`Output Areas Code`))

# create unique OAs table
colnames(oa_gender)
OAs <- oa_gender %>%
  dplyr::select(1, 2, 5) %>%
  group_by(`Output Areas Code`) %>%
  summarise(population = sum(Observation)) %>%
  ungroup()

# mutate age levels
oa_age <- oa_age %>%
  dplyr::rename(age_code = 3) %>%
  mutate(ageband = ifelse(age_code %in% c(1,2), "under24",
                                 ifelse(age_code == 3, "25-34",
                                        ifelse(age_code == 4, "35-44",
                                               ifelse(age_code == 5, "45-54",
                                                      ifelse(age_code == 6, "55-64",
                                                             ifelse(age_code == 7, "65-74", "75+")))))))
# convert age to pivot wider
oa_age_wider <- oa_age %>%
  dplyr::select(1,6,5) %>%
  group_by(`Output Areas Code`,ageband) %>%
  summarise(number = sum(Observation)) %>%
  pivot_wider(names_from = ageband,
              values_from = number)
# reorder cols
oa_age_wider <- oa_age_wider[, c(1,8,2,3,4,5,6,7)]

# mutate employment levels
oa_employment <- oa_employment %>%
  dplyr::rename(emp_code = 3) %>%
  mutate(employment = factor(ifelse(emp_code %in% c(3,4,6), "in_education",
                                   ifelse(emp_code == 1, "employed",
                                          ifelse(emp_code %in% c(2,5), "unemployed", "other")))))
# employment to wider
oa_employment_wider <- oa_employment %>%
  dplyr::select(1,6,5) %>%
  group_by(`Output Areas Code`,employment) %>%
  summarise(number = sum(Observation)) %>%
  pivot_wider(names_from = employment,
              values_from = number)

# mutate ethnicity levels
oa_ethnicity <- oa_ethnicity %>%
  dplyr::rename(eth_code = 3) %>%
  mutate(ethnicity = factor(ifelse(eth_code == 1, "asian",
                                    ifelse(eth_code == 2, "black",
                                           ifelse(eth_code == 4, "white", "other")))))
# ethnicity to wider
oa_ethnicity_wider <- oa_ethnicity %>%
  dplyr::select(1,6,5) %>%
  group_by(`Output Areas Code`,ethnicity) %>%
  summarise(number = sum(Observation)) %>%
  pivot_wider(names_from = ethnicity,
              values_from = number)

# mutate education levels
oa_education <- oa_education %>%
  dplyr::rename(hle_code = 3) %>%
  mutate(education = factor(ifelse(hle_code %in% c(0,1,2), "no_quals",
                                 ifelse(hle_code == 3, "secondary_education",
                                        ifelse(hle_code == 4, "higher_education", "other")))))

# education to wider
oa_education_wider <- oa_education %>%
  dplyr::select(1,6,5) %>%
  group_by(`Output Areas Code`,education) %>%
  summarise(number = sum(Observation)) %>%
  pivot_wider(names_from = education,
              values_from = number)


# mutate marital status levels
oa_marital <- oa_marital %>%
  dplyr::rename(ms_code = 3) %>%
  mutate(marital_status = factor(ifelse(ms_code == 1, "single",
                                   ifelse(ms_code == 2, "married",
                                          ifelse(ms_code %in% c(3,4,5), "divorced/separated/widowed", "other")))))

# marital status to wider
oa_marital_wider <- oa_marital %>%
  dplyr::select(1,6,5) %>%
  group_by(`Output Areas Code`,marital_status) %>%
  summarise(number = sum(Observation)) %>%
  pivot_wider(names_from = marital_status,
              values_from = number)

# gender to wider
oa_gender_wider <- oa_gender %>%
  rename(gender = 4) %>%
  dplyr::select(1,4,5) %>%
  pivot_wider(names_from = gender,
              values_from = Observation)

# rename "other"
oa_education_wider <- rename(oa_education_wider, education_other = other)
oa_employment_wider <- rename(oa_employment_wider, employment_other = other)
oa_ethnicity_wider <- rename(oa_ethnicity_wider, ethnicity_other = other)
oa_marital_wider <- rename(oa_marital_wider, marital_other = other)

## join to OAs table all together
OAs <- left_join(OAs, oa_gender_wider, by = 'Output Areas Code') %>%
  left_join(., oa_marital_wider, by = 'Output Areas Code') %>%
  left_join(., oa_education_wider, by = 'Output Areas Code') %>%
  left_join(., oa_ethnicity_wider, by = 'Output Areas Code') %>%
  left_join(., oa_employment_wider, by = 'Output Areas Code') %>%
  left_join(., oa_age_wider, by = 'Output Areas Code') 

# save as .csv
write_csv(OAs, "data/census_oa/census_oa_clean.csv")

# read in oxis_england as csv
oxis_england <- read_csv("data/OxIS2019/oxis_england_sae_variables_raw.csv")

## mutate OxIS variables
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

# use missRanger package to impute missing data
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

unique(data$employment_status)
data <- data %>%
  mutate(ageband = factor(ifelse(ageband == "18-24", "under24",
                                 ifelse(ageband %in% c("75-84","85+"), "75+", ageband))),
         gender = factor(gender),
         education = factor(ifelse(education == 4, 2, education), levels = c(0,1,2,3),
                            labels = c("no_quals", "secondary_education", "higer_education", "other")),
         employment_status = factor(ifelse(employment_status == 2, 4, employment_status),
                                    levels = c(1,3,4,5),
                                    labels = c("employed", "unemployed", "other", "in_education")),
         disability = factor(disability, levels = c(0,1),
                             labels= c("disabled", "not_disabled")),
         region = factor(region),
         children = factor(ifelse(children_hh > 0, "has_children", "no_children")),
         ethnicity = factor(ethnicity, levels = c(1,2,3,4),
                            labels = c("asian", "black", "white", "other")),
         religious = factor(religious, levels = c(0,1),
                            labels = c("not_religious", "religious")),
         marital_status = factor(marital_status, levels = c(1,2,3,4),
                                 labels = c("single","married","other","Divorced/separated/widowed"))
  )

data <- dplyr::select(data, -children_hh)
# save as .csv raw (no counts/proportions) but NAs imputed
write_csv(data, "data/OxIS2019/oxis_england_sae_variables_imputed_clean.csv") ## final OxIS data matches census cleaned


## d. fit SAE models
data <- read_csv("data/OxIS2019/oxis_england_sae_variables_imputed_clean.csv")
dummy_data <- fastDummies::dummy_cols(data,
                                      select_columns = c("ageband","gender","education","employment_status","disability","region","ethnicity","religious","marital_status","children"))
dummy_data <- select(dummy_data, -c(2:10,14))
dummy_data <- dummy_data[, c(1, 5:44, 2:4)]
str(dummy_data)

# save dummy_data as .csv
write_csv(dummy_data, "data/OxIS2019/oxis_england_sae_variables_imputed_clean_dummy.csv")


## random intercept models ##
dummy_data <- read_csv("data/OxIS2019/oxis_england_sae_variables_imputed_clean_dummy.csv")
dummy_data <- mutate(dummy_data,
                     digital_skills = as.integer(digital_skills),
                     use_frequency = as.integer(use_frequency))

# baseline model
mod = lmer(digital_skills ~ 1 + (1|OA), data = dummy_data) # intercept is a random effect (each cluster's mean is going to differ)
visualize(mod, plot = "model")
summary(mod)

# model 1 for digital skills
model1 <- lmer(digital_skills ~ 1 + . -OA -use_frequency -weight + (1|OA), # random intercept models for OAs
              data = dummy_data,
              weights = weight)
summary(model1)
BIC(model1)

# second model for digital skills using only significant vars: age, gender, education, marital status
model1.2 <- lmer(digital_skills ~ 1 + age_under24 + `age_25-34` + `age_35-44` + `age_45-54` + `age_55-64` + `age_65-74` + `age_75+`
                 + gender_Female + gender_Male
                 + edu_no_quals + edu_secondary_education + edu_higher_education + edu_other 
                 + marital_stat_single + marital_stat_married + `marital_stat_Divorced/separated/widowed` + marital_stat_other
                 + (1|OA), # random intercept models for OAs
                 data = dummy_data,
                 weights = weight)

summary(model1.2)
anova(model1, model1.2) # model 1.2 is better


# model 2 for online use frequency
model2 <- lmer(use_frequency ~ 1 + . -OA -digital_skills -weight + (1|OA), # random intercept models for OAs
              data = dummy_data,
              weights = weight)
summary(model2)

# second model for use frequency using only significant vars: age, education, region, ethnicity, marital status
model2.2 <- lmer(use_frequency ~ 1 + age_under24 + `age_25-34` + `age_35-44` + `age_45-54` + `age_55-64` + `age_65-74` + `age_75+`
                 + edu_no_quals + edu_secondary_education + edu_higher_education + edu_other
                 + marital_stat_single + marital_stat_married + `marital_stat_Divorced/separated/widowed` + marital_stat_other
                 + ethnicity_asian + ethnicity_black + ethnicity_other + ethnicity_white
                 + `region_East Midlands` + `region_East of England` + `region_London` + `region_North East` + `region_North West` + `region_South East` + `region_South West` + `region_West Midlands` + `region_Yorkshire and The Humber`
                 + (1|OA), # random intercept models for OAs
                          data = dummy_data,
                          weights = weight)
summary(model2.2)

# third model for use frequency using only significant vars: age, education, ethnicity, marital status
model2.3 <- lmer(use_frequency ~ 1 + age_under24 + `age_25-34` + `age_35-44` + `age_45-54` + `age_55-64` + `age_65-74` + `age_75+`
                 + edu_no_quals + edu_secondary_education + edu_higher_education + edu_other
                 + marital_stat_single + marital_stat_married + `marital_stat_Divorced/separated/widowed` + marital_stat_other
                 + ethnicity_asian + ethnicity_black + ethnicity_other + ethnicity_white
                 + (1|OA), # random intercept models for OAs
                 data = dummy_data,
                 weights = weight)
summary(model2.3)
anova(model2, model2.2, model2.3) # model 2.3 is better

# view ICC for each model
icc(model1.2)
icc(model2.3)

# view OA-specific intercept coefficients
coef(model1.2)$OA ## digital skills
coef(model2.3)$OA ## use frequency

# read census data in
census_oa <- read_csv("data/census_oa/census_oa_clean.csv")
str(census_oa)
census_oa_prop <- read_csv("data/census_oa/census_oa_clean_prop.csv")

## predict both variables across all OAs using census data (proportions)
census_oa_prop <- select(census_oa_prop, -population)
census_oa_prop <- dplyr::rename(census_oa_prop, OA = `Output Areas Code`)

# filter out existing OAs
census_oa_prop <- filter(census_oa_prop, !OA %in% dummy_data$OA)

# predict average digital skills across OAs
census_oa_prop$pred_avgskill <- predict(model1.2, newdata = census_oa_prop, allow.new.levels = TRUE)
summary(census_oa_prop$pred_avgskill)
summary(dummy_data$digital_skills)

dummy_data %>% ggplot(aes(x = digital_skills)) +
  geom_boxplot()
census_oa_prop %>% ggplot(aes(x = pred_avgskill)) +
  geom_boxplot()

# predict average usage frequency across OAs
census_oa_prop$pred_avg_usefreq <- predict(model2.3, newdata = census_oa_prop, allow.new.levels = TRUE)
summary(census_oa_prop$pred_avg_usefreq)
summary(dummy_data$use_frequency)

dummy_data %>% ggplot(aes(x = use_frequency)) +
  geom_boxplot()
census_oa_prop %>% ggplot(aes(x = pred_avg_usefreq)) +
  geom_boxplot()


# create new table with just digital skills and use frequency
oa_predicted <- census_oa_prop[,c(1,27,28)] %>%
  dplyr::rename(digital_skills = pred_avgskill,
         use_frequency = pred_avg_usefreq)


# bind existing average OA estimates from models trained on OxIS
# get coefficients
coef(model1.2)$OA[,1] ## digital skills
coef(model2.3)$OA[,1] ## use frequency

skills_df <- data.frame(coef(model1.2)$OA[,c(1,2)])
use_fr_df <- data.frame(coef(model2.3)$OA[,c(1,2)])

skills_df <- cbind(newColName = rownames(skills_df), skills_df)
rownames(skills_df) <- 1:nrow(skills_df)

skills_df <- skills_df %>%
  select(-3) %>%
  rename(OA = 1,
         digital_skills = 2)

use_fr_df <- cbind(newColName = rownames(use_fr_df), use_fr_df)
rownames(use_fr_df) <- 1:nrow(use_fr_df)

use_fr_df <- use_fr_df %>%
  select(-3) %>%
  rename(OA = 1,
         use_frequency = 2)

skills_and_use <- left_join(skills_df, use_fr_df, by = "OA")

# bind together
oa_predicted <- bind_rows(oa_predicted, skills_and_use)

# join LSOA look-up & group by LSOA
oa_predicted <- left_join(oa_predicted, master_lookup[,1:2], by = c("OA" = "oa21cd"))
colSums(is.na(oa_predicted))
filter(oa_predicted, is.na(lsoa21cd))

# fill missing NAs
oa_predicted <- arrange(oa_predicted, OA)
oa_predicted <- oa_predicted %>%
  fill(lsoa21cd, .direction = "downup")


# create table grouped by LSOA
LSOA_estimates <- oa_predicted %>%
  select(digital_skills,use_frequency,lsoa21cd) %>%
  group_by(lsoa21cd) %>%
  summarise(digital_skills = floor(mean(digital_skills)),
            use_frequency = floor(mean(use_frequency))
            )
  
summary(LSOA_estimates)
# save as .csv
write_csv(LSOA_estimates, "data/OxIS2019/final_LSOA_estimates.csv")

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


### IoD 2019 LSOAs - income ; join to census variables -------------------------
iod2019_income <- read_csv("data/Census21_LSOA/2019_IoD_income.csv")
summary(iod2019_income) # 32,844 LSOAs

# read LSOA 2011 to 2021 lookup
lsoa_lookup <- read_csv("data/LSOA(2011)_to_LSOA(2021).csv")

# join to income
iod2019_income <- left_join(iod2019_income, lsoa_lookup[2:4], by = c("LSOA code (2011)" = "LSOA11CD"))

# read processed census variables
census <- read_csv("data/Census21_LSOA/census21_variables_clean.csv")

# join income data to census data
census <- iod2019_income %>%
  dplyr::select(5,7) %>%
  left_join(x = census, by = c("LSOA_code" = "LSOA21CD"))

census <- distinct(census, LSOA_code, .keep_all = TRUE)

# rename income
census <- rename(census,income_dep_rate = `Income Score (rate)`)

colSums(is.na(census)) # 1013 income deprivation values missing

# impute missing income points using missRanger
census <- missRanger(census, 
                     formula = income_dep_rate ~ . -LSOA_code -LSOA_name,  # default uses all columns in the data set to impute all columns with NAs
                     num.trees = 100, 
                     verbose = 2, 
                     seed = 123,  
                     returnOOB = T)

summary(census$income_dep_rate)


### Point-Topic LSOAs BII; final joins ----------------------------------------
bii <- read_csv("data/PointTopic/BIIquery.csv")
histograms(bii)
summary(bii)
str(bii)

# drop G-fast column
bii <- dplyr::select(bii, -GFAST_AVAILABILITY)

# join BII to census+income data
final <- left_join(census, bii, by = c("LSOA_code" = "LSOA"))
colSums(is.na(final)) # 1,945 broadband values missing

# impute missing data using missRanger
final <- missRanger(final, 
                     formula = . ~ . -LSOA_code -LSOA_name,  # default uses all columns in the data set to impute all columns with NAs
                     num.trees = 100, 
                     verbose = 2, 
                     seed = 123,  
                     returnOOB = T)

summary(final)

# add digital use LSOA estimates to "final"
LSOA_estimates <- read_csv("data/OxIS2019/final_LSOA_estimates.csv")

final <- left_join(final, LSOA_estimates, by = c("LSOA_code" = "lsoa21cd"))
colSums(is.na(final))

# drop unnecessary variables: very final data-frame
final <- dplyr::select(final, -c(population, male_count, over65count, over16_population, over16_noquals_count, disability_count,
                          count_white, eng_mainlang_count, religious_count))

summary(final)
# 17 final indicators

# save as .csv
write_csv(final, "data/index_indicators_final_cleaned_31072023.csv") # not normalised/standardised

### create Index -------------------------------------------------------------
# read final indicators dataframe
final <- read_csv("data/index_indicators_final_cleaned_31072023.csv")

## Use existing DERI scores + regress on predictor variables to determine weights
# read DERI
deri <- read_csv("data/LSOA_DERI_scores_v1.6.csv")
deri <- dplyr::select(deri, 1,46) # select England DERI scores
deri <- rename(deri,
               LSOA_code = 1,
               DERI_score = 2)

final_w_deri <- left_join(final, deri, by = "LSOA_code")
colSums(is.na(final_w_deri))

# proportion missing values
1946/nrow(final_w_deri) # 0.057

# OK to omit NAs for regression
final_w_deri <- na.omit(final_w_deri)
nrow(final_w_deri)/nrow(final)


# 1. only socio-demographic vars
model1 <- lm(DERI_score ~ prop_male + prop_over65 + prop_over16s_noquals + prop_disabled + prop_white
             + prop_eng_mainlang + prop_religious + income_dep_rate,
             data = final_w_deri)

summary(model1) # Adjusted R2 = 0.4897


# 2. only BII vars
model2 <- lm(DERI_score ~ FTTP_AVAILABILITY + FTTC_AVAILABILITY + DSL_AVAILABILITY + CABLE_AVAILABILITY + OPERATOR_COUNT
             + DOWN_MBPS + UP_MBPS,
             data = final_w_deri)

summary(model2) # Adjusted R2 = 0.0445


# 3. all variables
model3 <- lm(DERI_score ~ . - LSOA_code - LSOA_name,
             data = final_w_deri)

summary(model3) # Adjusted R2 = 0.5534, highest result

# rela_impo of regressors as weights
calc.relimp(model3, type = "lmg", rela = TRUE)

model3_weights <- as.data.frame(calc.relimp(model3, type = "lmg", rela = TRUE)$lmg)
model3_weights <- cbind(newColName = rownames(model3_weights), model3_weights)
rownames(model3_weights) <- 1:nrow(model3_weights)
model3_weights <- rename(model3_weights,
                         var = 1,
                         weight = 2)

# new table for index, apply weights to corresponding variables
index <- data.frame(final)
#tracemem(final)==tracemem(index)

# normalise 0-1: prop_white, OPERATOR_COUNT, DOWN_MBPS, UP_MBPS, digital_skills, use_frequency
# define Min-Max normalization function
min_max_norm <- function(x){
  (x - min(x)) / (max(x) - min(x))
}

index <- mutate(index,
                prop_white = min_max_norm(prop_white),
                OPERATOR_COUNT = min_max_norm(OPERATOR_COUNT),
                DOWN_MBPS = min_max_norm(DOWN_MBPS),
                UP_MBPS = min_max_norm(UP_MBPS),
                digital_skills = min_max_norm(digital_skills),
                use_frequency = min_max_norm(use_frequency))
summary(index)

# multiply by weights
index <- mutate(index,
                prop_male = prop_male*(model3_weights[1,2]),
                prop_over65 = prop_over65*(model3_weights[2,2]),
                prop_over16s_noquals = prop_over16s_noquals*(model3_weights[3,2]),
                prop_disabled = prop_disabled*(model3_weights[4,2]),
                prop_white = prop_white*(model3_weights[5,2]), 
                prop_eng_mainlang = prop_eng_mainlang*(model3_weights[6,2]), 
                prop_religious = prop_religious*(model3_weights[7,2]),
                income_dep_rate = income_dep_rate*(model3_weights[8,2]),
                FTTP_AVAILABILITY = FTTP_AVAILABILITY*(model3_weights[9,2]),
                FTTC_AVAILABILITY = FTTC_AVAILABILITY*(model3_weights[10,2]),
                DSL_AVAILABILITY = DSL_AVAILABILITY*(model3_weights[11,2]),
                CABLE_AVAILABILITY = CABLE_AVAILABILITY*(model3_weights[12,2]),
                OPERATOR_COUNT = OPERATOR_COUNT*(model3_weights[13,2]),
                DOWN_MBPS = DOWN_MBPS*(model3_weights[14,2]),
                UP_MBPS = UP_MBPS*(model3_weights[15,2]),
                digital_skills = digital_skills*(model3_weights[16,2]),
                use_frequency = use_frequency*(model3_weights[17,2]))

# sum across rows = final index
index$sum_index <- rowSums(index[3:19])
summary(index$sum_index)

index %>% ggplot(aes(x = sum_index)) + geom_histogram()

write_csv(index[,c(1,2,20)], "data/final_index_31072023.csv")


## second attempt: index using PCA ##
library(corrr)
library(ggcorrplot)
library(FactoMineR)
library(factoextra)

# compute correlation matrix
corr_matrix <- cor(index_pca)
ggcorrplot(corr_matrix) # view


# get principal components
pca_model <- prcomp(final[3:19], center = TRUE, scale. = TRUE, retx = TRUE)

pca_model2 <- princomp(final[3:19], cor = TRUE, scores = TRUE) # cor standardises the data
pca2_summary <- summary(pca_model2, loadings = TRUE, scores = TRUE)

## same results for both functions

# view scree plot
fviz_eig(pca_model, addlabels = TRUE, ncp = 17)
fviz_eig(pca_model, choice = "eigenvalue",
         addlabels = TRUE, ncp = 17)

# model 2 scree plots
fviz_eig(pca_model2, choice = "eigenvalue", addlabels = TRUE, ncp = 17,
         xlab = "Principal Components", geom = "line")

fviz_eig(pca_model2, addlabels = TRUE, ncp = 17, xlab = "Principal Components")

pca_model2$sdev
pr.var <- (pca_model2$sdev)^2
pve <- (pr.var/sum(pr.var))

plot(cumsum(pve),
     xlab = "Principal Components",
     ylab = "Cumulative Proportion of Variance Explained",
     ylim  = c(0, 1),
     xaxt = "n",
     type = "b", cex = 0.5, col = "dark blue")
# Define x-axis manually
axis(1, at = 1:17, labels = c(1:17))


scores = as.data.frame(pca_model2$scores)
plot3d(scores[,1:3], 
       size=5,
       col = seq(nrow(scores)))

# view pca loadings: first 3
pca_model2$loadings[, 1:3]
# view eigenvals
get_eig(pca_model2)
## use first 7 PCs

head(pca2_summary$scores)

index_pca <- final %>%
  dplyr::select("LSOA_code", "LSOA_name") %>% 
  bind_cols(pca2_summary$scores %>% as.data.frame()) %>%
  dplyr::select(1:5) %>%
  rename(PC1 = 3, PC2 = 4, PC3 = 5)

# cumulative variance explained by first 3 PCs
CVE = 0.5453200
# weights dictionary
weights <- c("PC1wt" = 0.2782986/CVE, "PC2wt" = 0.1431391/CVE, "PC3wt" = 0.1238824/CVE)

# weighted index -- multiply PCs by weights
index_pca_wt <- data.frame(index_pca)

index_pca_wt$PC1 <- (index_pca_wt$PC1)*weights["PC1wt"] # PC1
index_pca_wt$PC2 <- (index_pca_wt$PC2)*weights["PC2wt"] # PC2
index_pca_wt$PC3 <- (index_pca_wt$PC3)*weights["PC3wt"] # PC3

# sum rows to get raw index
index_pca_wt$sum <- rowSums(index_pca_wt[3:5])
summary(index_pca_wt)

# standardise using min-max normalisation
index_pca_wt$S_index <- min_max_norm(index_pca_wt$sum)

# view standardised index distribution
library(hrbrthemes)
index_pca_wt %>%
  ggplot(aes(x = S_index)) +
  geom_histogram(bins = 50, fill="navy", color="#e9ecef", alpha=0.9) +
  ylab("Count of LSOAs") + xlab("SI score") + 
  theme_ipsum() +
  theme(axis.title.x = element_text(size=10, face="bold", hjust = 0.5),
        axis.title.y = element_text(size=10, face="bold", hjust = 0.5)
        )

# save as .csv
write_csv(index_pca_wt, "data/final_index_PCA_04082023.csv")

# get OxIS OAs only
library(shapefiles)
library(st)
OAs <- read_sf("data/geos/2011_oa_lyr/infuse_oa_lyr_2011.shp")
oxis <- read_csv("data/OxIS2019/oxis_england_sae_variables_imputed_clean.csv")

length(unique(oxis$OA))

# filter OAs shp to OAs in OxIS
colnames(OAs)
oxis_oa <- filter(OAs, geo_code %in% unique(oxis$OA))

st_write(oxis_oa, "data/geos/oxis_oa/oxis_oa.shp")


### Cluster analysis ----------------------------------------------------------
library(FactoMineR)
library(factoextra)

# PCA on subset dimensions of variables
# 1. socio-demographic 
pc_soc <- princomp(final[3:10], cor = TRUE, scores = TRUE) # cor standardises the data
summary(pc_soc, loadings = TRUE, scores = TRUE)

# view scree plot
fviz_eig(pc_soc, addlabels = TRUE, ncp = 10) # first 2 PCs
fviz_eig(pc_soc, choice = "eigenvalue",
         addlabels = TRUE, ncp = 10)

# cum_var
pc_soc$sdev
pc_soc.var <- (pc_soc$sdev)^2
pc_soc_pve <- (pc_soc.var/sum(pc_soc.var))

plot(cumsum(pc_soc_pve),
     xlab = "Principal Components",
     ylab = "Cumulative Proportion of Variance Explained",
     ylim  = c(0, 1),
     xaxt = "n",
     type = "b", cex = 0.5, col = "dark blue")
# Define x-axis manually
axis(1, at = 1:10, labels = c(1:10))

# bind 2 PCs to final df
final_cluster <- dplyr::select(final, 1:2) %>%
  bind_cols(pc_soc$scores %>% as.data.frame()) %>%
  dplyr::select(1:4) %>%
  rename(var1 = 3, var2 = 4)
  
# 2. broadband
pc_int <- princomp(final[11:17], cor = TRUE, scores = TRUE) # cor standardises the data
summary(pc_int, loadings = TRUE, scores = TRUE)

# view scree plot
fviz_eig(pc_int, addlabels = TRUE, ncp = 7) # first 2 PCs
fviz_eig(pc_int, choice = "eigenvalue",
         addlabels = TRUE, ncp = 7)

# cum_var
pc_int$sdev
pc_int.var <- (pc_int$sdev)^2
pc_int_pve <- (pc_int.var/sum(pc_int.var))

plot(cumsum(pc_int_pve),
     xlab = "Principal Components",
     ylab = "Cumulative Proportion of Variance Explained",
     ylim  = c(0, 1),
     xaxt = "n",
     type = "b", cex = 0.5, col = "dark blue")
# Define x-axis manually
axis(1, at = 1:10, labels = c(1:10))

# bind 2 PCs to final df
final_cluster <- dplyr::select(final_cluster, 1:4) %>%
  bind_cols(pc_int$scores %>% as.data.frame()) %>%
  dplyr::select(1:6) %>%
  rename(var3 = 5, var4 = 6)

  
# 3. internet use 
pc_dig <- princomp(final[18:19], cor = TRUE, scores = TRUE) # cor standardises the data
summary(pc_dig, loadings = TRUE, scores = TRUE)

# view scree plot
fviz_eig(pc_dig, addlabels = TRUE, ncp = 2) # first 2 PCs
fviz_eig(pc_dig, choice = "eigenvalue",
         addlabels = TRUE, ncp = 7)

# bind PC1 to final df
final_cluster <- dplyr::select(final_cluster, 1:6) %>%
  bind_cols(pc_dig$scores %>% as.data.frame()) %>%
  dplyr::select(1:7) %>%
  rename(var5 = 7)


# k-means clustering analysis
final_cluster_kmeans <- scale(final_cluster[3:7])

fviz_nbclust(final_cluster_kmeans, kmeans, method = "silhouette") # silhouette
# optimal number of clusters k = 2

fviz_nbclust(final_cluster_kmeans, kmeans, method = "wss")
# crashes... cannot compute wss on such large data dimension

## conclusion: not worth doing clustering


