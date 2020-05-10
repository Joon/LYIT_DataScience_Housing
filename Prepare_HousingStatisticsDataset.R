#install.packages("tidyverse", "tidyr", "csodata") 
library(csodata)
library(tidyverse) 

src_population_density <- as_tibble(cso_get_data("E2014"))
src_household_income <- as_tibble(cso_get_data("IIA14"))
src_period_built_sewerage_type <- as_tibble(cso_get_data("E1041"))
src_period_built_water_supp <- as_tibble(cso_get_data("E1040"))
src_central_hearing <- as_tibble(cso_get_data("E1054"))
src_computer_access <- as_tibble(cso_get_data("E1061"))
src_population <- as_tibble(cso_get_data("E2016"))
rent <- as_tibble(read.csv("Data/RentByTown.csv"))

# Reformat data to give data columns by town
population <- src_population_density[src_population_density$Statistic == "2016 - Total population (Number)", c("Towns.by.Size", "Towns.by.Size.id", "2016")]
colnames(population) <- c("Towns.by.Size", "Towns.by.Size.id", "TotalPopulation")
population_density <- src_population_density[src_population_density$Statistic == "2016 - Population density (persons per sq km) (Number)", c("Towns.by.Size", "Towns.by.Size.id", "2016")]
colnames(population_density) <- c("Towns.by.Size", "Towns.by.Size.id", "PersonsPerSqKm")
rm(src_population_density)

household_income <- src_household_income[src_household_income$Statistic == "Household Median Gross Income (Euro)",]
rm(src_household_income)
sewerage_to_pivot <- src_period_built_sewerage_type[src_period_built_sewerage_type$Period.in.which.Built == "All years", 
                                           c("Type.of.Sewerage.Facility", "Towns.by.Size", "Towns.by.Size.id", "2016")]
rm(src_period_built_sewerage_type)
sewerage <- sewerage_to_pivot %>% tidyr::pivot_wider(names_from = Type.of.Sewerage.Facility, 
                   values_from = `2016`)
rm(sewerage_to_pivot)
heating_to_pivot <- src_central_hearing[, c("Central.Heating", "Towns.by.Size", "Towns.by.Size.id", "2016")]
rm(src_central_hearing)
heating <- heating_to_pivot %>% tidyr::pivot_wider(names_from = Central.Heating, 
                                                     # the values will be the result of the fight
                                                     values_from = `2016`)
rm(heating_to_pivot)
computer_access_to_pivot <- src_computer_access[, c("Computer.and.Internet.Access", "Towns.by.Size", "Towns.by.Size.id", "2016")]
rm(src_computer_access)
computer_internet_access <- computer_access_to_pivot %>% tidyr::pivot_wider(names_from = Computer.and.Internet.Access, 
                                                   values_from = `2016`)
rm(computer_access_to_pivot)

water_supp_to_pivot <- src_period_built_water_supp[src_period_built_water_supp$Period.in.which.Built == "All years", 
                                                   c("Type.of.Water.Supply", "Towns.by.Size", "Towns.by.Size.id", "2016")]
rm(src_period_built_water_supp)
water_supply <- water_supp_to_pivot %>% tidyr::pivot_wider(names_from = Type.of.Water.Supply, 
                                                   values_from = `2016`)
rm(water_supp_to_pivot)

# Select and rename desired columns
water_supply <- water_supply[, c("Towns.by.Size", "Towns.by.Size.id", "Public mains", "Public group scheme", 
                                 "Private group scheme", "Other private source", "No piped water")]
colnames(water_supply) <- c("Towns.by.Size", "Towns.by.Size.id", "Water_PublicMains", "Water_PublicGroupScheme", 
                            "Water_PrivateGroupScheme", "Water_OtherPrivateSource", "Water_NoPipedWater")

computer_internet_access <- computer_internet_access[, c("Towns.by.Size", "Towns.by.Size.id", "All households", 
                                                         "Yes, owns a personal computer", "No, does not own a personal computer", 
                                                         "Yes, broadband internet access",  "Yes, other than broadband internet access", 
                                                         "No internet access")]
colnames(computer_internet_access) <- c("Towns.by.Size", "Towns.by.Size.id", "AllHouseholds", "ICT_OwnsPC", "ICT_DoesNotOwnPC", "ICT_BroadBand", 
                                        "ICT_InternetNonBroadband", "ICT_NoInternet" )

heating <- heating[, c("Towns.by.Size", "Towns.by.Size.id", "Yes", "No")]
colnames(heating) <- c("Towns.by.Size", "Towns.by.Size.id", "Heating_HasCentral", "Heating_NoCentral")

household_income <- household_income[, c("Towns", "Towns.id", "2016")]
colnames(household_income) <- c("Towns.by.Size", "Towns.by.Size.id", "Household_Income")

sewerage <- sewerage[, c("Towns.by.Size", "Towns.by.Size.id", "Public scheme", "Individual septic tank", 
                         "Individual treatment not septic tank", "Other type of sewerage", "No sewerage facility")]
colnames(sewerage) <- c("Towns.by.Size", "Towns.by.Size.id", "Sewerage_Public", "Sewerage_IndSeptic", 
                        "Sewerage_IndNonSeptic", "Sewerage_Other", "Sewerage_None")

w1 <- inner_join(population_density, population)
w2 <- inner_join(w1, computer_internet_access)
w3 <- inner_join(w2, heating)
w4 <- inner_join(w3, sewerage)
w5 <- inner_join(w4, water_supply)
w6 <- inner_join(w5, household_income)
housing_statistics <- inner_join(w6, rent, by = c("Towns.by.Size" = "Town"))

rm(w1, w2, w3, w4, w5, w6)

housing_statistics <- housing_statistics[, c("Towns.by.Size", "County", "PersonsPerSqKm", "TotalPopulation", "AllHouseholds", 
                                             "ICT_OwnsPC", "ICT_DoesNotOwnPC", "ICT_BroadBand", "ICT_InternetNonBroadband", 
                                             "ICT_NoInternet", "Heating_HasCentral", "Heating_NoCentral", "Sewerage_Public", 
                                             "Sewerage_IndSeptic", "Sewerage_IndNonSeptic", "Sewerage_Other", "Sewerage_None", 
                                             "Water_PublicMains", "Water_PublicGroupScheme", "Water_PrivateGroupScheme",
                                             "Water_OtherPrivateSource", "Water_NoPipedWater", "Household_Income", "Avg_Rent")]

write.csv(housing_statistics, "Data/HousingStatsByTown.csv")
