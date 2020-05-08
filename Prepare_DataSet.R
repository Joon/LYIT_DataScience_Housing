# Install or update the package:
# install.packages("csodata")

library(csodata)

# Load all datasets to be used for developing the end dataset
# Load CSO datasets - population stats
esb_connections <- cso_get_data("HSA11")
population_06_11 <- cso_get_data("CD303")
population_11_16 <- cso_get_data("EB001")

# Load RTB dataset
donegal_rent_prices <- read.csv("Data/RTB_Average_Rents_Filtered.csv")

# Load AIRO small area population details for 2016
census_population_2016 <- read.csv("Data/CENSUS_2016_PRELIM_RESULTS.csv")

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

donegal_employed <- donegal_employed[,c("2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016")]
colnames(donegal_employed) <- c("EMPLOYED_2008", "EMPLOYED_2009", "EMPLOYED_2010", "EMPLOYED_2011", "EMPLOYED_2012", 
                                "EMPLOYED_2013", "EMPLOYED_2014", "EMPLOYED_2015", "EMPLOYED_2016")

# Calculate a new house dataset with new houses (from esb connections) for 2006 - 2016 for Donegal
donegal_new_houses <- esb_connections[esb_connections$Local.Authority == "Donegal (County Council)" & 
                                        esb_connections$Housing.Sector == "All housing sectors", 
                                      c("2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016")]

# Calculate a dataset that rolls up population, housing stock and average rental payment across Donegal by different areas
# Calculate population per district
donegal_population_and_housing <- census_population_2016[census_population_2016$County == "Donegal", 
                                                         c("ED_NAME", "POPULATION_06", "POPULATION_11", "POPULATION_16", 
                                                            "HOUSING_STOCK_11", "HOUSING_STOCK_16")]
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

# Calculate join ids
donegal_population_and_housing$DISTRICT_ID <- substr(donegal_population_and_housing$ED_NAME, 1, 3)
AIRO_Area_District_Map$DISTRICT_ID <- substr(AIRO_Area_District_Map$ElectoralWard, 1, 3)

# Join population data with the AIRO translation data
population_RTB_Area <- merge(donegal_population_and_housing, AIRO_Area_District_Map, by.x = "DISTRICT_ID", by.y = "DISTRICT_ID")

colnames(population_RTB_Area) <- c("DISTRICT_ID", "ED_NAME", "POPULATION_06", "POPULATION_11", "POPULATION_16", "HOUSING_STOCK_11", 
                                   "HOUSING_STOCK_16", "POPULATION_07", "POPULATION_08", "POPULATION_09", "POPULATION_10", "POPULATION_12",
                                   "POPULATION_13", "POPULATION_14", "POPULATION_15", "HOUSING_STOCK_12", "HOUSING_STOCK_13", "HOUSING_STOCK_14",
                                   "HOUSING_STOCK_15", "AREA", "ELECTORAL_WARD", "RTB_AREA")
to_rollup <- population_RTB_Area[, c("POPULATION_06", "POPULATION_07", "POPULATION_08", "POPULATION_09", "POPULATION_10", 
                                                     "POPULATION_11",  "POPULATION_12", "POPULATION_13", "POPULATION_14", "POPULATION_15", "POPULATION_16", 
                                                     "HOUSING_STOCK_11","HOUSING_STOCK_12", "HOUSING_STOCK_13", "HOUSING_STOCK_14", "HOUSING_STOCK_15", 
                                                     "HOUSING_STOCK_16", "RTB_AREA")]

# Roll up the individual districts by RTB area to join the rental values in
donegal_district_housing_population <- aggregate(cbind(to_rollup$POPULATION_06, to_rollup$POPULATION_07, to_rollup$POPULATION_08, 
                                                       to_rollup$POPULATION_09, to_rollup$POPULATION_10, to_rollup$POPULATION_11, 
                                                       to_rollup$POPULATION_12, to_rollup$POPULATION_13, to_rollup$POPULATION_14, 
                                                       to_rollup$POPULATION_15, to_rollup$POPULATION_16, to_rollup$HOUSING_STOCK_11, 
                                                       to_rollup$HOUSING_STOCK_12, to_rollup$HOUSING_STOCK_13, to_rollup$HOUSING_STOCK_14, 
                                                       to_rollup$HOUSING_STOCK_15, to_rollup$HOUSING_STOCK_16), by=list(RTB_AREA=to_rollup$RTB_AREA), FUN=sum)

colnames(donegal_district_housing_population) <- c("RTB_AREA", "POPULATION_06", "POPULATION_07", "POPULATION_08", "POPULATION_09", "POPULATION_10", "POPULATION_11", 
                                                   "POPULATION_12", "POPULATION_13", "POPULATION_14", "POPULATION_15", "POPULATION_16", "HOUSING_STOCK_11", 
                                                   "HOUSING_STOCK_12", "HOUSING_STOCK_13", "HOUSING_STOCK_14", "HOUSING_STOCK_15", "HOUSING_STOCK_16")

donegal_rent_all <- donegal_rent_prices[donegal_rent_prices$ï..Bedrooms == "All bedrooms" & donegal_rent_prices$Property.Type == "All property types",]
colnames(donegal_rent_all) <- c("BEDROOMS", "PROPERTY_TYPE", "LOCATION", "RENT_2008", "RENT_2009", "RENT_2010", "RENT_2011", "RENT_2012", "RENT_2013", 
                                "RENT_2014", "RENT_2015", "RENT_2016", "RENT_2017", "RENT_2018", "RENT_2019")


population_rent <- merge(donegal_district_housing_population, donegal_rent_all, by.x = "RTB_AREA", by.y = "LOCATION")
# Remap the name to reflect that it contains all items outside of town areas
levels(population_rent$RTB_AREA)[levels(population_rent$RTB_AREA)=="Donegal"] <- "Rural Donegal"

population_rent$BEDROOMS <- NA
population_rent$PROPERTY_TYPE <- NA

population_rent$POP_PERC_OF_TOTAL <- population_rent$POPULATION_16 / sum(population_rent$POPULATION_16)
population_rent$HOUSE_PERC_OF_TOTAL <- population_rent$HOUSING_STOCK_16 / sum(population_rent$HOUSING_STOCK_16)

population_rent$EMPLOYED_08 <- donegal_employed$EMPLOYED_2008 * population_rent$POP_PERC_OF_TOTAL
population_rent$EMPLOYED_09 <- donegal_employed$EMPLOYED_2009 * population_rent$POP_PERC_OF_TOTAL
population_rent$EMPLOYED_10 <- donegal_employed$EMPLOYED_2010 * population_rent$POP_PERC_OF_TOTAL
population_rent$EMPLOYED_11 <- donegal_employed$EMPLOYED_2011 * population_rent$POP_PERC_OF_TOTAL
population_rent$EMPLOYED_12 <- donegal_employed$EMPLOYED_2012 * population_rent$POP_PERC_OF_TOTAL
population_rent$EMPLOYED_13 <- donegal_employed$EMPLOYED_2013 * population_rent$POP_PERC_OF_TOTAL
population_rent$EMPLOYED_14 <- donegal_employed$EMPLOYED_2014 * population_rent$POP_PERC_OF_TOTAL
population_rent$EMPLOYED_15 <- donegal_employed$EMPLOYED_2015 * population_rent$POP_PERC_OF_TOTAL
population_rent$EMPLOYED_16 <- donegal_employed$EMPLOYED_2016 * population_rent$POP_PERC_OF_TOTAL

# Variables:
#   - employed
#   - population
#   - avg rent 
#   - house stock 

data_11 <- population_rent[, c("RTB_AREA", "POPULATION_11", "EMPLOYED_11", "HOUSING_STOCK_11", "RENT_2011")]
colnames(data_11) <- c("RTB_AREA", "POPULATION", "EMPLOYED", "HOUSING_STOCK", "AVG_RENT")
data_11$YEAR <- "2011"
data_12 <- population_rent[, c("RTB_AREA", "POPULATION_12", "EMPLOYED_12", "HOUSING_STOCK_12", "RENT_2012")]
colnames(data_12) <- c("RTB_AREA", "POPULATION", "EMPLOYED", "HOUSING_STOCK", "AVG_RENT")
data_12$YEAR <- "2012"
data_13 <- population_rent[, c("RTB_AREA", "POPULATION_13", "EMPLOYED_13", "HOUSING_STOCK_13", "RENT_2013")]
colnames(data_13) <- c("RTB_AREA", "POPULATION", "EMPLOYED", "HOUSING_STOCK", "AVG_RENT")
data_13$YEAR <- "2013"
data_14 <- population_rent[, c("RTB_AREA", "POPULATION_14", "EMPLOYED_14", "HOUSING_STOCK_14", "RENT_2014")]
colnames(data_14) <- c("RTB_AREA", "POPULATION", "EMPLOYED", "HOUSING_STOCK", "AVG_RENT")
data_14$YEAR <- "2014"
data_15 <- population_rent[, c("RTB_AREA", "POPULATION_15", "EMPLOYED_15", "HOUSING_STOCK_15", "RENT_2015")]
colnames(data_15) <- c("RTB_AREA", "POPULATION", "EMPLOYED", "HOUSING_STOCK", "AVG_RENT")
data_15$YEAR <- "2015"
data_16 <- population_rent[, c("RTB_AREA", "POPULATION_16", "EMPLOYED_16", "HOUSING_STOCK_16", "RENT_2016")]
colnames(data_16) <- c("RTB_AREA", "POPULATION", "EMPLOYED", "HOUSING_STOCK", "AVG_RENT")
data_16$YEAR <- "2016"

analysis_dataset <- rbind(data_11, data_12, data_13, data_14, data_15, data_16)
write.csv(analysis_dataset, "Data/Analyze_data.csv")
