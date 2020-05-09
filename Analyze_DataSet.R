# Load Dataset
lk_accomodation <- read.csv("Data/Analyze_data.csv")

######################################################################
#                     DESCRIPTIVE STATISTICS
######################################################################

sapply(lk_accomodation, summary)

# First finding: Population is comparable, it would be expected that there would be more
#                competition for rentals in heavily populated areas. We can not meaningfully
#                compare employed count, it would be more meaningful to compare percentage
#                employed.


numeric_variable_list <- c("POPULATION", "HOUSING_STOCK", "AVG_RENT", "EMPLOYED")
numerical_data <- lk_accomodation[numeric_variable_list]
numerical_data

numerical_summary <- do.call(cbind, lapply(numerical_data, summary))
numerical_summary

alt_numerical_summary <-  do.call(rbind, lapply(numerical_data, summary))
alt_numerical_summary

# Largest Rent
lk_accomodation[which.max(lk_accomodation$AVG_RENT),]

# Largest Population
lk_accomodation[which.max(lk_accomodation$POPULATION),]
lk_accomodation[which.max(lk_accomodation$EMPLOYED),]

# Largest Housing Stock
lk_accomodation[which.max(lk_accomodation$HOUSING_STOCK),]


# Lowest Rent
lk_accomodation[which.min(lk_accomodation$AVG_RENT),]

# Lowest Population
lk_accomodation[which.min(lk_accomodation$POPULATION),]
lk_accomodation[which.min(lk_accomodation$EMPLOYED),]

# Lowest Housing Stock
lk_accomodation[which.min(lk_accomodation$HOUSING_STOCK),]

