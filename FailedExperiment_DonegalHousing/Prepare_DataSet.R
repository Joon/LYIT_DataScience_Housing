# Install or update the package:
# install.packages("csodata")

library(csodata)

# Load all datasets to be used for developing the end dataset
# Load CSO datasets - population stats
population_06_11 <- cso_get_data("CD303")
population_11_16 <- cso_get_data("EB001")

# Load RTB dataset
donegal_rent_prices <- read.csv("Data/RTB_Average_Rents_Filtered.csv")

# Load AIRO small area population details for 2016
census_population_2016 <- read.csv("Data/CENSUS_2016_PRELIM_RESULTS.csv")
all_ireland_income_average <- read.csv("Data/Ireland_Average_Earnings.csv")

# Load mapping table to connect AIRO electoral wards to RTB districts
AIRO_Area_District_Map <- read.csv("Data/DonegalEDMapping.csv", sep="\t", header=TRUE)

# Calculate an employed dataset with headline employed per year for 2006 - 2016 for Donegal
donegal_employed <- population_06_11[population_06_11$Province.County.or.City == "Donegal" & population_06_11$Sex == "Both sexes" & 
                   population_06_11$Principal.Economic.Status == "Persons at work",]

donegal_employed_11_16 <- population_11_16[population_11_16$County.and.City == "Donegal" & population_11_16$Sex == "Both sexes" & (
                    population_11_16$Principal.Economic.Status == "Employer or own account worker" | 
                    population_11_16$Principal.Economic.Status == "Employee" | 
                    population_11_16$Principal.Economic.Status == "Assisting relative"),]

donegal_employed$`2016` <- sum(donegal_employed_11_16$`2016`)

# Now distribute difference between 06, 11 and 16 values to all years in the gaps
employed_06 <- donegal_employed$`2006`
employed_11 <- donegal_employed$`2011`
employed_16 <- donegal_employed$`2016`
employed_movement_06_11_pa <- (employed_11 - employed_06) / 5
donegal_employed$`2007` <- round(employed_06 + employed_movement_06_11_pa)
donegal_employed$`2008` <- round(employed_06 + (employed_movement_06_11_pa * 2))
donegal_employed$`2009` <- round(employed_06 + (employed_movement_06_11_pa * 3))
donegal_employed$`2010` <- round(employed_06 + (employed_movement_06_11_pa * 4))
employed_movement_11_16_pa <- (employed_16 - employed_11) / 5
donegal_employed$`2012` <- round(employed_11 + employed_movement_11_16_pa)
donegal_employed$`2013` <- round(employed_11 + (employed_movement_11_16_pa * 2))
donegal_employed$`2014` <- round(employed_11 + (employed_movement_11_16_pa * 3))
donegal_employed$`2015` <- round(employed_11 + (employed_movement_11_16_pa * 4))
# Project the 2011 to 2016 trend forward to 2019
donegal_employed$`2017` <- round(employed_16 + (employed_movement_11_16_pa * 1))
donegal_employed$`2018` <- round(employed_16 + (employed_movement_11_16_pa * 2))
donegal_employed$`2019` <- round(employed_16 + (employed_movement_11_16_pa * 3))

donegal_employed <- donegal_employed[,c("2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019")]
colnames(donegal_employed) <- c("EMPLOYED_2008", "EMPLOYED_2009", "EMPLOYED_2010", "EMPLOYED_2011", "EMPLOYED_2012", 
                                "EMPLOYED_2013", "EMPLOYED_2014", "EMPLOYED_2015", "EMPLOYED_2016", "EMPLOYED_2017", 
                                "EMPLOYED_2018", "EMPLOYED_2019")

# Calculate a dataset that rolls up population, housing stock and average rental payment across Donegal by different areas
# Calculate population per district
donegal_population_and_housing <- census_population_2016[census_population_2016$County == "Donegal", 
                                                         c("ED_NAME", "POPULATION_06", "POPULATION_11", "POPULATION_16", 
                                                            "HOUSING_STOCK_11", "HOUSING_STOCK_16", "VACANT_HOUSES_LESS_HOLIDAY_HOMES_11", 
                                                           "VACANT_HOUSES_LESS_HOLIDAY_HOMES_16", "HOUSEHOLD_INCOME_16")]
# Distribute the date from 06, 11 and 16 census between all years in the range 06 to 2016
donegal_population_and_housing$POPULATION_07 <- round(donegal_population_and_housing$POPULATION_06 + 
                                                        ((donegal_population_and_housing$POPULATION_11 - 
                                                            donegal_population_and_housing$POPULATION_06) / 5))
donegal_population_and_housing$POPULATION_08 <- round(donegal_population_and_housing$POPULATION_06 + 
                                                        (((donegal_population_and_housing$POPULATION_11 - 
                                                             donegal_population_and_housing$POPULATION_06) / 5) * 2))
donegal_population_and_housing$POPULATION_09 <- round(donegal_population_and_housing$POPULATION_06 + 
                                                        (((donegal_population_and_housing$POPULATION_11 - 
                                                             donegal_population_and_housing$POPULATION_06) / 5) * 3))
donegal_population_and_housing$POPULATION_10 <- round(donegal_population_and_housing$POPULATION_06 + 
                                                        (((donegal_population_and_housing$POPULATION_11 -
                                                             donegal_population_and_housing$POPULATION_06) / 5) * 4))

donegal_population_and_housing$POPULATION_12 <- round(donegal_population_and_housing$POPULATION_11 + 
                                                        ((donegal_population_and_housing$POPULATION_16 - 
                                                            donegal_population_and_housing$POPULATION_11) / 5))
donegal_population_and_housing$POPULATION_13 <- round(donegal_population_and_housing$POPULATION_11 + 
                                                        (((donegal_population_and_housing$POPULATION_16 - 
                                                             donegal_population_and_housing$POPULATION_11) / 5) * 2))
donegal_population_and_housing$POPULATION_14 <- round(donegal_population_and_housing$POPULATION_11 + 
                                                        (((donegal_population_and_housing$POPULATION_16 - 
                                                             donegal_population_and_housing$POPULATION_11) / 5) * 3))
donegal_population_and_housing$POPULATION_15 <- round(donegal_population_and_housing$POPULATION_11 + 
                                                        (((donegal_population_and_housing$POPULATION_16 - 
                                                             donegal_population_and_housing$POPULATION_11) / 5) * 4))

# Distribute trend from 11 to 16 forward to 2019
donegal_population_and_housing$POPULATION_17 <- round(donegal_population_and_housing$POPULATION_16 + 
                                                        ((donegal_population_and_housing$POPULATION_16 - 
                                                             donegal_population_and_housing$POPULATION_11) / 5))
donegal_population_and_housing$POPULATION_18 <- round(donegal_population_and_housing$POPULATION_16 + 
                                                        (((donegal_population_and_housing$POPULATION_16 - 
                                                             donegal_population_and_housing$POPULATION_11) / 5) * 2))
donegal_population_and_housing$POPULATION_19 <- round(donegal_population_and_housing$POPULATION_16 + 
                                                        (((donegal_population_and_housing$POPULATION_16 - 
                                                             donegal_population_and_housing$POPULATION_11) / 5) * 3))



donegal_population_and_housing$HOUSING_STOCK_08 <- round(donegal_population_and_housing$HOUSING_STOCK_11 - 
                                                           (((donegal_population_and_housing$HOUSING_STOCK_16 - 
                                                                donegal_population_and_housing$HOUSING_STOCK_11) / 5) * 3))
donegal_population_and_housing$HOUSING_STOCK_09 <- round(donegal_population_and_housing$HOUSING_STOCK_11 - 
                                                           (((donegal_population_and_housing$HOUSING_STOCK_16 - 
                                                               donegal_population_and_housing$HOUSING_STOCK_11) / 5) * 2))
donegal_population_and_housing$HOUSING_STOCK_10 <- round(donegal_population_and_housing$HOUSING_STOCK_11 - 
                                                           ((donegal_population_and_housing$HOUSING_STOCK_16 - 
                                                               donegal_population_and_housing$HOUSING_STOCK_11) / 5))

donegal_population_and_housing$HOUSING_STOCK_12 <- round(donegal_population_and_housing$HOUSING_STOCK_11 + 
                                                           ((donegal_population_and_housing$HOUSING_STOCK_16 - 
                                                               donegal_population_and_housing$HOUSING_STOCK_11) / 5))
donegal_population_and_housing$HOUSING_STOCK_13 <- round(donegal_population_and_housing$HOUSING_STOCK_11 + 
                                                           (((donegal_population_and_housing$HOUSING_STOCK_16 - 
                                                                donegal_population_and_housing$HOUSING_STOCK_11) / 5) * 2))
donegal_population_and_housing$HOUSING_STOCK_14 <- round(donegal_population_and_housing$HOUSING_STOCK_11 + 
                                                           (((donegal_population_and_housing$HOUSING_STOCK_16 - 
                                                                donegal_population_and_housing$HOUSING_STOCK_11) / 5) * 3))
donegal_population_and_housing$HOUSING_STOCK_15 <- round(donegal_population_and_housing$HOUSING_STOCK_11 + 
                                                           (((donegal_population_and_housing$HOUSING_STOCK_16 - 
                                                                donegal_population_and_housing$HOUSING_STOCK_11) / 5) * 4))

donegal_population_and_housing$HOUSING_STOCK_17 <- round(donegal_population_and_housing$HOUSING_STOCK_16 + 
                                                           ((donegal_population_and_housing$HOUSING_STOCK_16 - 
                                                               donegal_population_and_housing$HOUSING_STOCK_11) / 5))
donegal_population_and_housing$HOUSING_STOCK_18 <- round(donegal_population_and_housing$HOUSING_STOCK_16 + 
                                                           (((donegal_population_and_housing$HOUSING_STOCK_16 - 
                                                                donegal_population_and_housing$HOUSING_STOCK_11) / 5) * 2))
donegal_population_and_housing$HOUSING_STOCK_19 <- round(donegal_population_and_housing$HOUSING_STOCK_16 + 
                                                           (((donegal_population_and_housing$HOUSING_STOCK_16 - 
                                                                donegal_population_and_housing$HOUSING_STOCK_11) / 5) * 3))


donegal_population_and_housing$VACANT_HOUSES_LESS_HOLIDAY_HOMES_08 <- round(donegal_population_and_housing$VACANT_HOUSES_LESS_HOLIDAY_HOMES_11 - 
                                                                              (((donegal_population_and_housing$VACANT_HOUSES_LESS_HOLIDAY_HOMES_16 - 
                                                                                   donegal_population_and_housing$VACANT_HOUSES_LESS_HOLIDAY_HOMES_11) / 5) * 3))
donegal_population_and_housing$VACANT_HOUSES_LESS_HOLIDAY_HOMES_09 <- round(donegal_population_and_housing$VACANT_HOUSES_LESS_HOLIDAY_HOMES_11 - 
                                                                              (((donegal_population_and_housing$VACANT_HOUSES_LESS_HOLIDAY_HOMES_16 - 
                                                                                   donegal_population_and_housing$VACANT_HOUSES_LESS_HOLIDAY_HOMES_11) / 5) * 2))
donegal_population_and_housing$VACANT_HOUSES_LESS_HOLIDAY_HOMES_10 <- round(donegal_population_and_housing$VACANT_HOUSES_LESS_HOLIDAY_HOMES_11 - 
                                                                              ((donegal_population_and_housing$VACANT_HOUSES_LESS_HOLIDAY_HOMES_16 - 
                                                                                  donegal_population_and_housing$VACANT_HOUSES_LESS_HOLIDAY_HOMES_11) / 5))

donegal_population_and_housing$VACANT_HOUSES_LESS_HOLIDAY_HOMES_12 <- round(donegal_population_and_housing$VACANT_HOUSES_LESS_HOLIDAY_HOMES_11 + 
                                                                              ((donegal_population_and_housing$VACANT_HOUSES_LESS_HOLIDAY_HOMES_16 - 
                                                                                  donegal_population_and_housing$VACANT_HOUSES_LESS_HOLIDAY_HOMES_11) / 5))
donegal_population_and_housing$VACANT_HOUSES_LESS_HOLIDAY_HOMES_13 <- round(donegal_population_and_housing$VACANT_HOUSES_LESS_HOLIDAY_HOMES_11 + 
                                                                              (((donegal_population_and_housing$VACANT_HOUSES_LESS_HOLIDAY_HOMES_16 - 
                                                                                   donegal_population_and_housing$VACANT_HOUSES_LESS_HOLIDAY_HOMES_11) / 5) * 2))
donegal_population_and_housing$VACANT_HOUSES_LESS_HOLIDAY_HOMES_14 <- round(donegal_population_and_housing$VACANT_HOUSES_LESS_HOLIDAY_HOMES_11 + 
                                                                              (((donegal_population_and_housing$VACANT_HOUSES_LESS_HOLIDAY_HOMES_16 - 
                                                                                   donegal_population_and_housing$VACANT_HOUSES_LESS_HOLIDAY_HOMES_11) / 5) * 3))
donegal_population_and_housing$VACANT_HOUSES_LESS_HOLIDAY_HOMES_15 <- round(donegal_population_and_housing$VACANT_HOUSES_LESS_HOLIDAY_HOMES_11 + 
                                                                              (((donegal_population_and_housing$VACANT_HOUSES_LESS_HOLIDAY_HOMES_16 - 
                                                                                   donegal_population_and_housing$VACANT_HOUSES_LESS_HOLIDAY_HOMES_11) / 5) * 4))

donegal_population_and_housing$VACANT_HOUSES_LESS_HOLIDAY_HOMES_17 <- round(donegal_population_and_housing$VACANT_HOUSES_LESS_HOLIDAY_HOMES_16 + 
                                                                              ((donegal_population_and_housing$VACANT_HOUSES_LESS_HOLIDAY_HOMES_16 - 
                                                                                  donegal_population_and_housing$VACANT_HOUSES_LESS_HOLIDAY_HOMES_11) / 5))
donegal_population_and_housing$VACANT_HOUSES_LESS_HOLIDAY_HOMES_18 <- round(donegal_population_and_housing$VACANT_HOUSES_LESS_HOLIDAY_HOMES_16 + 
                                                                              (((donegal_population_and_housing$VACANT_HOUSES_LESS_HOLIDAY_HOMES_16 - 
                                                                                   donegal_population_and_housing$VACANT_HOUSES_LESS_HOLIDAY_HOMES_11) / 5) * 2))
donegal_population_and_housing$VACANT_HOUSES_LESS_HOLIDAY_HOMES_19 <- round(donegal_population_and_housing$VACANT_HOUSES_LESS_HOLIDAY_HOMES_16 + 
                                                                              (((donegal_population_and_housing$VACANT_HOUSES_LESS_HOLIDAY_HOMES_16 - 
                                                                                   donegal_population_and_housing$VACANT_HOUSES_LESS_HOLIDAY_HOMES_11) / 5) * 3))

# using the census per-district household income for 2016, calculate a household income for every other year by 
# using a dataset that contains average all-ireland labour costs for every year from 2008 to 2018. Project 2018 to
# 2019 by adding the difference between 2017 and 2018 to the value of 2018
representative_income <- all_ireland_income_average[all_ireland_income_average$EARNING_TYPE == "Average Total Labour Costs (Euro)",]
donegal_population_and_housing$HOUSEHOLD_INCOME_16 <- as.character(donegal_population_and_housing$HOUSEHOLD_INCOME_16)
donegal_population_and_housing$HOUSEHOLD_INCOME_16 <- gsub(",","",donegal_population_and_housing$HOUSEHOLD_INCOME_16)
donegal_population_and_housing$HOUSEHOLD_INCOME_16 <- as.numeric(donegal_population_and_housing$HOUSEHOLD_INCOME_16)
donegal_population_and_housing$HOUSEHOLD_INCOME_08 <- donegal_population_and_housing$HOUSEHOLD_INCOME_16 * 
  (representative_income$X2008 / representative_income$X2016)
donegal_population_and_housing$HOUSEHOLD_INCOME_09 <- donegal_population_and_housing$HOUSEHOLD_INCOME_16 * 
  (representative_income$X2009 / representative_income$X2016)
donegal_population_and_housing$HOUSEHOLD_INCOME_10 <- donegal_population_and_housing$HOUSEHOLD_INCOME_16 * 
  (representative_income$X2010 / representative_income$X2016)
donegal_population_and_housing$HOUSEHOLD_INCOME_11 <- donegal_population_and_housing$HOUSEHOLD_INCOME_16 * 
  (representative_income$X2011 / representative_income$X2016)
donegal_population_and_housing$HOUSEHOLD_INCOME_12 <- donegal_population_and_housing$HOUSEHOLD_INCOME_16 * 
  (representative_income$X2012 / representative_income$X2016)
donegal_population_and_housing$HOUSEHOLD_INCOME_13 <- donegal_population_and_housing$HOUSEHOLD_INCOME_16 * 
  (representative_income$X2013 / representative_income$X2016)
donegal_population_and_housing$HOUSEHOLD_INCOME_14 <- donegal_population_and_housing$HOUSEHOLD_INCOME_16 * 
  (representative_income$X2014 / representative_income$X2016)
donegal_population_and_housing$HOUSEHOLD_INCOME_15 <- donegal_population_and_housing$HOUSEHOLD_INCOME_16 * 
  (representative_income$X2015 / representative_income$X2016)
donegal_population_and_housing$HOUSEHOLD_INCOME_17 <- donegal_population_and_housing$HOUSEHOLD_INCOME_16 * 
  (representative_income$X2017 / representative_income$X2016)
donegal_population_and_housing$HOUSEHOLD_INCOME_18 <- donegal_population_and_housing$HOUSEHOLD_INCOME_16 * 
  (representative_income$X2018 / representative_income$X2016)
donegal_population_and_housing$HOUSEHOLD_INCOME_19 <- donegal_population_and_housing$HOUSEHOLD_INCOME_18 + 
  (donegal_population_and_housing$HOUSEHOLD_INCOME_18 - donegal_population_and_housing$HOUSEHOLD_INCOME_17)

# Calculate join ids
donegal_population_and_housing$DISTRICT_ID <- substr(donegal_population_and_housing$ED_NAME, 1, 3)
AIRO_Area_District_Map$DISTRICT_ID <- substr(AIRO_Area_District_Map$ElectoralWard, 1, 3)

# Join population data with the AIRO translation data
population_RTB_Area <- merge(donegal_population_and_housing, AIRO_Area_District_Map, by.x = "DISTRICT_ID", by.y = "DISTRICT_ID")


colnames(population_RTB_Area) <- c("DISTRICT_ID", "ED_NAME", "POPULATION_06", "POPULATION_11", "POPULATION_16", "HOUSING_STOCK_11", 
                                   "HOUSING_STOCK_16", "VACANT_HOUSES_LESS_HOLIDAY_HOMES_11", "VACANT_HOUSES_LESS_HOLIDAY_HOMES_16", 
                                   "HOUSEHOLD_INCOME_16", "POPULATION_07", "POPULATION_08", "POPULATION_09", "POPULATION_10", 
                                   "POPULATION_12", "POPULATION_13", "POPULATION_14", "POPULATION_15", "POPULATION_17", "POPULATION_18",
                                   "POPULATION_19", "HOUSING_STOCK_08", "HOUSING_STOCK_09", "HOUSING_STOCK_10", "HOUSING_STOCK_12", 
                                   "HOUSING_STOCK_13", "HOUSING_STOCK_14", "HOUSING_STOCK_15", "HOUSING_STOCK_17", "HOUSING_STOCK_18", 
                                   "HOUSING_STOCK_19", "VACANT_HOUSES_LESS_HOLIDAY_HOMES_08",  "VACANT_HOUSES_LESS_HOLIDAY_HOMES_09", 
                                   "VACANT_HOUSES_LESS_HOLIDAY_HOMES_10", "VACANT_HOUSES_LESS_HOLIDAY_HOMES_12", "VACANT_HOUSES_LESS_HOLIDAY_HOMES_13",
                                   "VACANT_HOUSES_LESS_HOLIDAY_HOMES_14", "VACANT_HOUSES_LESS_HOLIDAY_HOMES_15", "VACANT_HOUSES_LESS_HOLIDAY_HOMES_17", 
                                   "VACANT_HOUSES_LESS_HOLIDAY_HOMES_18", "VACANT_HOUSES_LESS_HOLIDAY_HOMES_19", "HOUSEHOLD_INCOME_08", 
                                   "HOUSEHOLD_INCOME_09", "HOUSEHOLD_INCOME_10", "HOUSEHOLD_INCOME_11", "HOUSEHOLD_INCOME_12", "HOUSEHOLD_INCOME_13", 
                                   "HOUSEHOLD_INCOME_14", "HOUSEHOLD_INCOME_15", "HOUSEHOLD_INCOME_17", "HOUSEHOLD_INCOME_18", "HOUSEHOLD_INCOME_19",
                                   "AREA", "ELECTORAL_WARD", "RTB_AREA")
to_rollup_sum <- population_RTB_Area[, c("POPULATION_06", "POPULATION_07", "POPULATION_08", "POPULATION_09", "POPULATION_10", 
                                         "POPULATION_11",  "POPULATION_12", "POPULATION_13", "POPULATION_14", "POPULATION_15", "POPULATION_16", 
                                         "POPULATION_17",  "POPULATION_18",  "POPULATION_19", "HOUSING_STOCK_08", "HOUSING_STOCK_09", "HOUSING_STOCK_10", 
                                         "HOUSING_STOCK_11","HOUSING_STOCK_12", "HOUSING_STOCK_13", "HOUSING_STOCK_14", "HOUSING_STOCK_15", "HOUSING_STOCK_16", 
                                         "HOUSING_STOCK_17", "HOUSING_STOCK_18", "HOUSING_STOCK_19", "VACANT_HOUSES_LESS_HOLIDAY_HOMES_08",
                                         "VACANT_HOUSES_LESS_HOLIDAY_HOMES_09", "VACANT_HOUSES_LESS_HOLIDAY_HOMES_10", "VACANT_HOUSES_LESS_HOLIDAY_HOMES_11", 
                                         "VACANT_HOUSES_LESS_HOLIDAY_HOMES_12", "VACANT_HOUSES_LESS_HOLIDAY_HOMES_13", "VACANT_HOUSES_LESS_HOLIDAY_HOMES_14",
                                         "VACANT_HOUSES_LESS_HOLIDAY_HOMES_15", "VACANT_HOUSES_LESS_HOLIDAY_HOMES_16", "VACANT_HOUSES_LESS_HOLIDAY_HOMES_17", 
                                         "VACANT_HOUSES_LESS_HOLIDAY_HOMES_18","VACANT_HOUSES_LESS_HOLIDAY_HOMES_19", "RTB_AREA")]
to_rollup_avg <- population_RTB_Area[, c("HOUSEHOLD_INCOME_08", "HOUSEHOLD_INCOME_09", "HOUSEHOLD_INCOME_10", "HOUSEHOLD_INCOME_11", "HOUSEHOLD_INCOME_12",
                                         "HOUSEHOLD_INCOME_13", "HOUSEHOLD_INCOME_14", "HOUSEHOLD_INCOME_15", "HOUSEHOLD_INCOME_16", "HOUSEHOLD_INCOME_17", 
                                         "HOUSEHOLD_INCOME_18", "HOUSEHOLD_INCOME_19", "RTB_AREA")]

# Roll up the individual districts by RTB area to join the rental values in
donegal_district_housing_population <- aggregate(cbind(to_rollup_sum$POPULATION_06, to_rollup_sum$POPULATION_07, to_rollup_sum$POPULATION_08, 
                                                       to_rollup_sum$POPULATION_09, to_rollup_sum$POPULATION_10, to_rollup_sum$POPULATION_11, 
                                                       to_rollup_sum$POPULATION_12, to_rollup_sum$POPULATION_13, to_rollup_sum$POPULATION_14, 
                                                       to_rollup_sum$POPULATION_15, to_rollup_sum$POPULATION_16, to_rollup_sum$POPULATION_17, 
                                                       to_rollup_sum$POPULATION_18, to_rollup_sum$POPULATION_19, to_rollup_sum$HOUSING_STOCK_08, 
                                                       to_rollup_sum$HOUSING_STOCK_09, to_rollup_sum$HOUSING_STOCK_10, to_rollup_sum$HOUSING_STOCK_11, 
                                                       to_rollup_sum$HOUSING_STOCK_12, to_rollup_sum$HOUSING_STOCK_13, to_rollup_sum$HOUSING_STOCK_14, 
                                                       to_rollup_sum$HOUSING_STOCK_15, to_rollup_sum$HOUSING_STOCK_16, to_rollup_sum$HOUSING_STOCK_17, 
                                                       to_rollup_sum$HOUSING_STOCK_18, to_rollup_sum$HOUSING_STOCK_19, to_rollup_sum$VACANT_HOUSES_LESS_HOLIDAY_HOMES_08,
                                                       to_rollup_sum$VACANT_HOUSES_LESS_HOLIDAY_HOMES_09, to_rollup_sum$VACANT_HOUSES_LESS_HOLIDAY_HOMES_10,
                                                       to_rollup_sum$VACANT_HOUSES_LESS_HOLIDAY_HOMES_11, to_rollup_sum$VACANT_HOUSES_LESS_HOLIDAY_HOMES_12, 
                                                       to_rollup_sum$VACANT_HOUSES_LESS_HOLIDAY_HOMES_13, to_rollup_sum$VACANT_HOUSES_LESS_HOLIDAY_HOMES_14, 
                                                       to_rollup_sum$VACANT_HOUSES_LESS_HOLIDAY_HOMES_15, to_rollup_sum$VACANT_HOUSES_LESS_HOLIDAY_HOMES_16,
                                                       to_rollup_sum$VACANT_HOUSES_LESS_HOLIDAY_HOMES_17, to_rollup_sum$VACANT_HOUSES_LESS_HOLIDAY_HOMES_18,
                                                       to_rollup_sum$VACANT_HOUSES_LESS_HOLIDAY_HOMES_19), 
                                                 by=list(RTB_AREA=to_rollup_sum$RTB_AREA), FUN=sum)

colnames(donegal_district_housing_population) <- c("RTB_AREA", "POPULATION_06", "POPULATION_07", "POPULATION_08", "POPULATION_09", "POPULATION_10", "POPULATION_11", 
                                                   "POPULATION_12", "POPULATION_13", "POPULATION_14", "POPULATION_15", "POPULATION_16", "POPULATION_17", 
                                                   "POPULATION_18", "POPULATION_19", "HOUSING_STOCK_08", "HOUSING_STOCK_09", "HOUSING_STOCK_10", "HOUSING_STOCK_11", 
                                                   "HOUSING_STOCK_12", "HOUSING_STOCK_13", "HOUSING_STOCK_14", "HOUSING_STOCK_15", "HOUSING_STOCK_16", 
                                                   "HOUSING_STOCK_17", "HOUSING_STOCK_18", "HOUSING_STOCK_19", "AVAILABLE_HOUSING_08", "AVAILABLE_HOUSING_09",
                                                   "AVAILABLE_HOUSING_10", "AVAILABLE_HOUSING_11", "AVAILABLE_HOUSING_12", "AVAILABLE_HOUSING_13", 
                                                   "AVAILABLE_HOUSING_14", "AVAILABLE_HOUSING_15", "AVAILABLE_HOUSING_16", "AVAILABLE_HOUSING_17", 
                                                   "AVAILABLE_HOUSING_18", "AVAILABLE_HOUSING_19")

donegal_district_income <- aggregate(cbind(to_rollup_avg$HOUSEHOLD_INCOME_08, to_rollup_avg$HOUSEHOLD_INCOME_09, to_rollup_avg$HOUSEHOLD_INCOME_10,
                                           to_rollup_avg$HOUSEHOLD_INCOME_11, to_rollup_avg$HOUSEHOLD_INCOME_12, to_rollup_avg$HOUSEHOLD_INCOME_13,
                                           to_rollup_avg$HOUSEHOLD_INCOME_14, to_rollup_avg$HOUSEHOLD_INCOME_15, to_rollup_avg$HOUSEHOLD_INCOME_16,
                                           to_rollup_avg$HOUSEHOLD_INCOME_17, to_rollup_avg$HOUSEHOLD_INCOME_18, to_rollup_avg$HOUSEHOLD_INCOME_19), 
                                     by=list(RTB_AREA=to_rollup_sum$RTB_AREA), FUN=mean)
colnames(donegal_district_income) <- c("RTB_AREA", "HOUSEHOLD_INCOME_08", "HOUSEHOLD_INCOME_09", "HOUSEHOLD_INCOME_10", "HOUSEHOLD_INCOME_11", 
                                       "HOUSEHOLD_INCOME_12", "HOUSEHOLD_INCOME_13","HOUSEHOLD_INCOME_14", "HOUSEHOLD_INCOME_15", "HOUSEHOLD_INCOME_16", 
                                       "HOUSEHOLD_INCOME_17", "HOUSEHOLD_INCOME_18", "HOUSEHOLD_INCOME_19")

donegal_district_housing_population <- merge(donegal_district_housing_population, donegal_district_income, by.x = "RTB_AREA", by.y = "RTB_AREA")


donegal_rent_all <- donegal_rent_prices[donegal_rent_prices$ï..Bedrooms == "All bedrooms" & donegal_rent_prices$Property.Type == "All property types",]
colnames(donegal_rent_all) <- c("BEDROOMS", "PROPERTY_TYPE", "LOCATION", "RENT_2008", "RENT_2009", "RENT_2010", "RENT_2011", "RENT_2012", "RENT_2013", 
                                "RENT_2014", "RENT_2015", "RENT_2016", "RENT_2017", "RENT_2018", "RENT_2019")


population_rent <- merge(donegal_district_housing_population, donegal_rent_all, by.x = "RTB_AREA", by.y = "LOCATION")
# Remap the name to reflect that it contains all items outside of town areas
levels(population_rent$RTB_AREA)[levels(population_rent$RTB_AREA)=="Donegal"] <- "RUR"
levels(population_rent$RTB_AREA)[levels(population_rent$RTB_AREA)=="Letterkenny, Donegal"] <- "LK"
levels(population_rent$RTB_AREA)[levels(population_rent$RTB_AREA)=="Bundoran, Donegal"] <- "BUND"
levels(population_rent$RTB_AREA)[levels(population_rent$RTB_AREA)=="Ballybofey, Donegal"] <- "BALL"
levels(population_rent$RTB_AREA)[levels(population_rent$RTB_AREA)=="Carndonagh, Donegal"] <- "CARN"
levels(population_rent$RTB_AREA)[levels(population_rent$RTB_AREA)=="Buncrana, Donegal"] <- "BUNC"
levels(population_rent$RTB_AREA)[levels(population_rent$RTB_AREA)=="Lifford, Donegal"] <- "LIFF"
levels(population_rent$RTB_AREA)[levels(population_rent$RTB_AREA)=="Donegal Town"] <- "DNG"
levels(population_rent$RTB_AREA)[levels(population_rent$RTB_AREA)=="Stranorlar, Donegal"] <- "STRN"

population_rent$BEDROOMS <- NA
population_rent$PROPERTY_TYPE <- NA

#population_rent$POP_PERC_OF_TOTAL <- population_rent$POPULATION_16 / sum(population_rent$POPULATION_16)
#population_rent$HOUSE_PERC_OF_TOTAL <- population_rent$HOUSING_STOCK_16 / sum(population_rent$HOUSING_STOCK_16)

population_rent$EMPLOYED_08 <- round(donegal_employed$EMPLOYED_2008 * (population_rent$POPULATION_08 / sum(population_rent$POPULATION_08)))
population_rent$EMPLOYED_09 <- round(donegal_employed$EMPLOYED_2009 * (population_rent$POPULATION_09 / sum(population_rent$POPULATION_09)))
population_rent$EMPLOYED_10 <- round(donegal_employed$EMPLOYED_2010 * (population_rent$POPULATION_10 / sum(population_rent$POPULATION_10)))
population_rent$EMPLOYED_11 <- round(donegal_employed$EMPLOYED_2011 * (population_rent$POPULATION_11 / sum(population_rent$POPULATION_11)))
population_rent$EMPLOYED_12 <- round(donegal_employed$EMPLOYED_2012 * (population_rent$POPULATION_12 / sum(population_rent$POPULATION_12)))
population_rent$EMPLOYED_13 <- round(donegal_employed$EMPLOYED_2013 * (population_rent$POPULATION_13 / sum(population_rent$POPULATION_13)))
population_rent$EMPLOYED_14 <- round(donegal_employed$EMPLOYED_2014 * (population_rent$POPULATION_14 / sum(population_rent$POPULATION_14)))
population_rent$EMPLOYED_15 <- round(donegal_employed$EMPLOYED_2015 * (population_rent$POPULATION_15 / sum(population_rent$POPULATION_15)))
population_rent$EMPLOYED_16 <- round(donegal_employed$EMPLOYED_2016 * (population_rent$POPULATION_16 / sum(population_rent$POPULATION_16)))
population_rent$EMPLOYED_17 <- round(donegal_employed$EMPLOYED_2017 * (population_rent$POPULATION_17 / sum(population_rent$POPULATION_17)))
population_rent$EMPLOYED_18 <- round(donegal_employed$EMPLOYED_2018 * (population_rent$POPULATION_18 / sum(population_rent$POPULATION_18)))
population_rent$EMPLOYED_19 <- round(donegal_employed$EMPLOYED_2019 * (population_rent$POPULATION_19 / sum(population_rent$POPULATION_19)))

# Variables:
#   - employed
#   - population
#   - avg rent 
#   - house stock 
#   - vacant house stock 

# Skip 08 - it has some rental data missing for some of the areas
data_09 <- population_rent[, c("RTB_AREA", "POPULATION_09", "EMPLOYED_09", "HOUSING_STOCK_09", "RENT_2009", "AVAILABLE_HOUSING_09", "HOUSEHOLD_INCOME_09")]
colnames(data_09) <- c("RTB_AREA", "POPULATION", "EMPLOYED", "HOUSING_STOCK", "AVG_RENT", "VACANT_HOUSES", "HOUSEHOLD_INCOME")
data_09$YEAR <- "2009"
data_09$POP_DIFF_FROM_MEAN <- data_09$POPULATION - mean(data_09$POPULATION)
data_09$RENT_DIFF_FROM_MEAN <- data_09$AVG_RENT - mean(data_09$AVG_RENT)
data_10 <- population_rent[, c("RTB_AREA", "POPULATION_10", "EMPLOYED_10", "HOUSING_STOCK_10", "RENT_2010", "AVAILABLE_HOUSING_10", "HOUSEHOLD_INCOME_10")]
colnames(data_10) <- c("RTB_AREA", "POPULATION", "EMPLOYED", "HOUSING_STOCK", "AVG_RENT", "VACANT_HOUSES", "HOUSEHOLD_INCOME")
data_10$YEAR <- "2010"
data_10$POP_DIFF_FROM_MEAN <- data_10$POPULATION - mean(data_10$POPULATION)
data_10$RENT_DIFF_FROM_MEAN <- data_10$AVG_RENT - mean(data_10$AVG_RENT)
data_11 <- population_rent[, c("RTB_AREA", "POPULATION_11", "EMPLOYED_11", "HOUSING_STOCK_11", "RENT_2011", "AVAILABLE_HOUSING_11", "HOUSEHOLD_INCOME_11")]
colnames(data_11) <- c("RTB_AREA", "POPULATION", "EMPLOYED", "HOUSING_STOCK", "AVG_RENT", "VACANT_HOUSES", "HOUSEHOLD_INCOME")
data_11$YEAR <- "2011"
data_11$POP_DIFF_FROM_MEAN <- data_11$POPULATION - mean(data_11$POPULATION)
data_11$RENT_DIFF_FROM_MEAN <- data_11$AVG_RENT - mean(data_11$AVG_RENT)
data_12 <- population_rent[, c("RTB_AREA", "POPULATION_12", "EMPLOYED_12", "HOUSING_STOCK_12", "RENT_2012", "AVAILABLE_HOUSING_12", "HOUSEHOLD_INCOME_12")]
colnames(data_12) <- c("RTB_AREA", "POPULATION", "EMPLOYED", "HOUSING_STOCK", "AVG_RENT", "VACANT_HOUSES", "HOUSEHOLD_INCOME")
data_12$YEAR <- "2012"
data_12$POP_DIFF_FROM_MEAN <- data_12$POPULATION - mean(data_12$POPULATION)
data_12$RENT_DIFF_FROM_MEAN <- data_12$AVG_RENT - mean(data_12$AVG_RENT)
data_13 <- population_rent[, c("RTB_AREA", "POPULATION_13", "EMPLOYED_13", "HOUSING_STOCK_13", "RENT_2013", "AVAILABLE_HOUSING_13", "HOUSEHOLD_INCOME_13")]
colnames(data_13) <- c("RTB_AREA", "POPULATION", "EMPLOYED", "HOUSING_STOCK", "AVG_RENT", "VACANT_HOUSES", "HOUSEHOLD_INCOME")
data_13$YEAR <- "2013"
data_13$POP_DIFF_FROM_MEAN <- data_13$POPULATION - mean(data_13$POPULATION)
data_13$RENT_DIFF_FROM_MEAN <- data_13$AVG_RENT - mean(data_13$AVG_RENT)
data_14 <- population_rent[, c("RTB_AREA", "POPULATION_14", "EMPLOYED_14", "HOUSING_STOCK_14", "RENT_2014", "AVAILABLE_HOUSING_14", "HOUSEHOLD_INCOME_14")]
colnames(data_14) <- c("RTB_AREA", "POPULATION", "EMPLOYED", "HOUSING_STOCK", "AVG_RENT", "VACANT_HOUSES", "HOUSEHOLD_INCOME")
data_14$YEAR <- "2014"
data_14$POP_DIFF_FROM_MEAN <- data_14$POPULATION - mean(data_14$POPULATION)
data_14$RENT_DIFF_FROM_MEAN <- data_14$AVG_RENT - mean(data_14$AVG_RENT)
data_15 <- population_rent[, c("RTB_AREA", "POPULATION_15", "EMPLOYED_15", "HOUSING_STOCK_15", "RENT_2015", "AVAILABLE_HOUSING_15", "HOUSEHOLD_INCOME_15")]
colnames(data_15) <- c("RTB_AREA", "POPULATION", "EMPLOYED", "HOUSING_STOCK", "AVG_RENT", "VACANT_HOUSES", "HOUSEHOLD_INCOME")
data_15$YEAR <- "2015"
data_15$POP_DIFF_FROM_MEAN <- data_15$POPULATION - mean(data_15$POPULATION)
data_15$RENT_DIFF_FROM_MEAN <- data_15$AVG_RENT - mean(data_15$AVG_RENT)
data_16 <- population_rent[, c("RTB_AREA", "POPULATION_16", "EMPLOYED_16", "HOUSING_STOCK_16", "RENT_2016", "AVAILABLE_HOUSING_16", "HOUSEHOLD_INCOME_16")]
colnames(data_16) <- c("RTB_AREA", "POPULATION", "EMPLOYED", "HOUSING_STOCK", "AVG_RENT", "VACANT_HOUSES", "HOUSEHOLD_INCOME")
data_16$YEAR <- "2016"
data_16$POP_DIFF_FROM_MEAN <- data_16$POPULATION - mean(data_16$POPULATION)
data_16$RENT_DIFF_FROM_MEAN <- data_16$AVG_RENT - mean(data_16$AVG_RENT)
data_17 <- population_rent[, c("RTB_AREA", "POPULATION_17", "EMPLOYED_17", "HOUSING_STOCK_17", "RENT_2017", "AVAILABLE_HOUSING_17", "HOUSEHOLD_INCOME_17")]
colnames(data_17) <- c("RTB_AREA", "POPULATION", "EMPLOYED", "HOUSING_STOCK", "AVG_RENT", "VACANT_HOUSES", "HOUSEHOLD_INCOME")
data_17$YEAR <- "2017"
data_17$POP_DIFF_FROM_MEAN <- data_17$POPULATION - mean(data_17$POPULATION)
data_17$RENT_DIFF_FROM_MEAN <- data_17$AVG_RENT - mean(data_17$AVG_RENT)
data_18 <- population_rent[, c("RTB_AREA", "POPULATION_18", "EMPLOYED_18", "HOUSING_STOCK_18", "RENT_2018", "AVAILABLE_HOUSING_18", "HOUSEHOLD_INCOME_18")]
colnames(data_18) <- c("RTB_AREA", "POPULATION", "EMPLOYED", "HOUSING_STOCK", "AVG_RENT", "VACANT_HOUSES", "HOUSEHOLD_INCOME")
data_18$YEAR <- "2018"
data_18$POP_DIFF_FROM_MEAN <- data_18$POPULATION - mean(data_18$POPULATION)
data_18$RENT_DIFF_FROM_MEAN <- data_18$AVG_RENT - mean(data_18$AVG_RENT)
data_19 <- population_rent[, c("RTB_AREA", "POPULATION_19", "EMPLOYED_19", "HOUSING_STOCK_19", "RENT_2019", "AVAILABLE_HOUSING_19", "HOUSEHOLD_INCOME_19")]
colnames(data_19) <- c("RTB_AREA", "POPULATION", "EMPLOYED", "HOUSING_STOCK", "AVG_RENT", "VACANT_HOUSES", "HOUSEHOLD_INCOME")
data_19$YEAR <- "2019"
data_19$POP_DIFF_FROM_MEAN <- data_19$POPULATION - mean(data_19$POPULATION)
data_19$RENT_DIFF_FROM_MEAN <- data_19$AVG_RENT - mean(data_19$AVG_RENT)

analysis_dataset <- rbind(data_09, data_10, data_11, data_12, data_13, data_14, data_15, data_16, data_17, data_18, data_19)
analysis_dataset$PERCENT_AVAILABLE <- analysis_dataset$VACANT_HOUSES / analysis_dataset$HOUSING_STOCK
write.csv(analysis_dataset, "Data/Analyze_data.csv")


