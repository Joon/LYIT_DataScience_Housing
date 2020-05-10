# Load Dataset
dl_accomodation <- read.csv("Data/Analyze_data.csv")

######################################################################
#                     DESCRIPTIVE STATISTICS
######################################################################

sapply(dl_accomodation, summary)

numeric_variable_list <- c("POPULATION", "HOUSING_STOCK", "AVG_RENT", "EMPLOYED", "VACANT_HOUSES", 
                           "POP_DIFF_FROM_MEAN", "RENT_DIFF_FROM_MEAN", "PERCENT_AVAILABLE", "HOUSEHOLD_INCOME")
numerical_data <- dl_accomodation[numeric_variable_list]

numerical_summary <- do.call(cbind, lapply(numerical_data, summary))
numerical_summary

alt_numerical_summary <-  do.call(rbind, lapply(numerical_data, summary))
alt_numerical_summary

# Largest Rent
dl_accomodation[which.max(dl_accomodation$AVG_RENT),]
# Largest Population
dl_accomodation[which.max(dl_accomodation$POPULATION),]
dl_accomodation[which.max(dl_accomodation$EMPLOYED),]
# Largest Housing Stock
dl_accomodation[which.max(dl_accomodation$HOUSING_STOCK),]
# Largest Vacant Housing Stock
dl_accomodation[which.max(dl_accomodation$VACANT_HOUSES),]
# Largest Income
dl_accomodation[which.max(dl_accomodation$HOUSEHOLD_INCOME),]

# Lowest Rent
dl_accomodation[which.min(dl_accomodation$AVG_RENT),]
# Lowest Population
dl_accomodation[which.min(dl_accomodation$POPULATION),]
dl_accomodation[which.min(dl_accomodation$EMPLOYED),]
# Lowest Housing Stock
dl_accomodation[which.min(dl_accomodation$HOUSING_STOCK),]
# Lowest Available Stock
dl_accomodation[which.min(dl_accomodation$VACANT_HOUSES),]
# Lowest Income
dl_accomodation[which.min(dl_accomodation$HOUSEHOLD_INCOME),]

#install.packages("ggplot2")
#install.packages("viridis")
library(ggplot2)
library(viridis)

# Plot average rent per area, color by population
plot <- ggplot(dl_accomodation, aes(x = RTB_AREA, y = AVG_RENT, color = POPULATION))
plot <- plot + geom_point() + scale_color_viridis()
print((plot))

# Plot vacant houses per area, color by rent
plot <- ggplot(dl_accomodation, aes(x = RTB_AREA, y = VACANT_HOUSES, color = AVG_RENT))
plot <- plot + geom_point() + scale_color_viridis()
print((plot))

# Plot average rent per area, color by average income
plot <- ggplot(dl_accomodation, aes(x = RTB_AREA, y = AVG_RENT, color = HOUSEHOLD_INCOME))
plot <- plot + geom_point() + scale_color_viridis()
print((plot))

# Plot average rent per area, color by percentage houses vacant
plot <- ggplot(dl_accomodation, aes(x = RTB_AREA, y = AVG_RENT, color = PERCENT_AVAILABLE))
plot <- plot + geom_point() + scale_color_viridis()
print((plot))

# Line chart of rental per area over time
areas <- levels(dl_accomodation$RTB_AREA)
area_count <- length(areas)

# get the range for the x and y axis
xrange <- range(2009:2019)
yrange <- range(dl_accomodation$AVG_RENT)

# set up the plot
plot(xrange, yrange, type="n", xlab="Year",
     ylab="Average Rent" )

linetype <- 1
colors <- rainbow(area_count)

# add lines
for (i in 1:area_count) {
  area <- subset(dl_accomodation, RTB_AREA==areas[i])
  lines(area$YEAR, area$AVG_RENT, type="l", lwd=1.5,
        lty=1, col=colors[i])
}

# add a title and subtitle
title("Average Rent by Area")

# add a legend
legend("bottomleft", legend=areas, cex=0.8, col=colors,
       lty=1, title="Area")


# Investigate correlation between variables  
variables_of_interest <- c("AVG_RENT", "POPULATION", "EMPLOYED", "HOUSING_STOCK", "VACANT_HOUSES", 
                           "YEAR", "POP_DIFF_FROM_MEAN", "RENT_DIFF_FROM_MEAN", "HOUSEHOLD_INCOME")
pairs(dl_accomodation[variables_of_interest])

# Zoom in on income, available housing and rental cost
variables_of_interest <- c("AVG_RENT", "PERCENT_AVAILABLE", "HOUSEHOLD_INCOME")
pairs(dl_accomodation[variables_of_interest])

# Scatter plot - avg rent vs vacant housing percentage - colored by area
xrange <- range(dl_accomodation$AVG_RENT)
yrange <- range(dl_accomodation$PERCENT_AVAILABLE)

ggplot(dl_accomodation, aes("AVG_RENT", "PERCENT_AVAILABLE"), xName='AVG_RENT',yName='PERCENT_AVAILABLE', 
                    groupName='RTB_AREA', size=3,
                    backgroundColor="white",
                    groupColors=colors)  

# Change point shapes, colors and sizes
ggplot(dl_accomodation, aes(x=AVG_RENT, y=PERCENT_AVAILABLE, color=RTB_AREA)) +
  geom_point() + geom_smooth(method=lm, se=FALSE, fullrange=TRUE)

######################################################################
#                   Principal Component Analysis
######################################################################

data_numeric_variables <- sapply(dl_accomodation, is.numeric)
data_numeric_variables

# Exclude ID column from PCA
data_numeric_variables["X"] <- FALSE

# Remove undesired columns from PCA dataset
dl_accomodation_adjusted <- dl_accomodation[, data_numeric_variables]

pca <- prcomp(dl_accomodation_adjusted, center = TRUE, scale. = TRUE)
summary(pca)

# Inspect PCA details
str(pca)

library("factoextra")
eig_values <- get_eigenvalue(pca)
eig_values

library("FactoMineR")
pca2 <- PCA(dl_accomodation_adjusted, graph = FALSE)
print(pca2)
pca2_eig_values <- get_eigenvalue(pca2)
pca2_eig_values


fviz_eig(pca, addlabels = TRUE, ylim = c(0, 50))

pca_for_variables <- get_pca_var(pca)
pca_for_variables

# Correlation plot - visualize proporion of contriubution to the variance
library("corrplot")
corrplot(pca_for_variables$cos2, is.corr = FALSE)
fviz_pca_var(pca, col.var = "black")

# Cos2 - quality of representation
fviz_cos2(pca, choice = "var", axes = 1:3)

# Plot quality of representation
fviz_pca_var(pca, col.var = "cos2",
             gradient.cols = c("red", "Blue", "Green"), 
             repel = TRUE # Avoid text overlapping
)  

head(pca_for_variables$contrib, 20)

# most contributing variables
fviz_pca_var(pca, col.var = "contrib",
             gradient.cols = c("red", "Blue", "Green"),
)


library(factoextra)
fviz_contrib(pca, choice = "var", axes = 1, top = 20)

# Contributions of variables to PC2
fviz_contrib(pca, choice = "var", axes = 2, top = 20)
# Contributions of variables to PC3
fviz_contrib(pca, choice = "var", axes = 3, top = 20)
# Contribution to PC1 - PC5
fviz_contrib(pca, choice = "var", axes = 1:3, top = 20)

fviz_pca_ind(pca,
             axes = c(1, 2),
             geom.ind = "point", # show points only (but not "text values")
             col.ind = data_file$Vote, # colour by groups
             palette = c("Red", "Green"),
             addEllipses = TRUE, # Concentration ellipses
             legend.title = "Vote"
)


biplot <- fviz_pca_ind(pca, geom = "point", col.ind = dl_accomodation$AVG_RENT)
ggpubr::ggpar(biplot,
              title = "Principal Component Analysis",
              subtitle = "Averate Rental in Donegal",
              caption = "Source: RTB, Census",
              xlab = "PC 1", ylab = "PC 2",
              legend.title = "Average Rent", legend.position = "top",
              ggtheme = theme_gray())

##############################################################################
#               Basic statistical analysis of variables
##############################################################################

hist(dl_accomodation$AVG_RENT)

hist(dl_accomodation$HOUSEHOLD_INCOME)

hist(dl_accomodation$POPULATION)

hist(dl_accomodation$EMPLOYED)

hist(dl_accomodation$VACANT_HOUSES)

hist(dl_accomodation$HOUSING_STOCK)

hist(dl_accomodation$POP_DIFF_FROM_MEAN)

hist(dl_accomodation$RENT_DIFF_FROM_MEAN)

hist(dl_accomodation$PERCENT_AVAILABLE)

library("ggpubr")
ggboxplot(dl_accomodation, x = "RTB_AREA", y = "AVG_RENT", 
          color = "RTB_AREA", palette = colors,
          ylab = "Average Rent", xlab = "Area")

ggboxplot(dl_accomodation, x = "RTB_AREA", y = "HOUSING_STOCK", 
          color = "RTB_AREA", palette = colors,
          ylab = "Housing Stock", xlab = "Area")

ggboxplot(dl_accomodation, x = "RTB_AREA", y = "HOUSEHOLD_INCOME", 
          color = "RTB_AREA", palette = colors,
          ylab = "Household Income", xlab = "Area")
