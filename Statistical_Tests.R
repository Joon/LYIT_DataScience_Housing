# Load Dataset
accomodation <- read.csv("Data/HousingStatsProcessed.csv")

#Hypothesis:
#  NULL - affordability does not increase household density
#  Person per house in areas with low rental/income measure == person per house in areas with high rental / income measure
#  ALTERNATIVE - affordability increases household density
#  Person per house in areas with high rental/income measure > person per house in areas with low rental / income measure


rentIncomeCharacteristics <- summary(accomodation$RentIncomeRatio)

people_per_house_expensive_areas = accomodation$PeoplePerHouse[accomodation$RentIncomeRatio > rentIncomeCharacteristics['3rd Qu.']]
people_per_house_rest_of_ire = accomodation$PeoplePerHouse[accomodation$RentIncomeRatio < rentIncomeCharacteristics['3rd Qu.']]

# We will compare people per household for the top quartile of affordability (highest rent to income ratio) 
# to the people per household in the rest of ireland - where accomodation is by definition more affordable
length(people_per_house_expensive_areas)
length(people_per_house_rest_of_ire)

hist(accomodation$PeoplePerHouse)
hist(people_per_house_expensive_areas)
hist(people_per_house_rest_of_ire)


library(pwr)

# Calculate power for the observations selected
test_power <- pwr.t.test(n = length(people_per_house_expensive_areas), d = 0.5, sig.level = 0.05, 
                         type="two.sample", alternative = "greater")

test_power

# Perform a Welch two-sample T test on the data to test they hypothesis
t.test(people_per_house_expensive_areas,people_per_house_rest_of_ire, alternative = "greater")

