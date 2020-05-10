# Load Dataset
accomodation <- read.csv("Data/HousingStatsByTown.csv")

######################################################################
#                     ENRICH DATA (for analysis)
######################################################################
# Add some variables to reflect the proportion of households for all variables tracked
accomodation$PC_ICTOwnsPC <- accomodation$ICT_OwnsPC / accomodation$AllHouseholds
accomodation$PC_ICTNoPC <- accomodation$ICT_DoesNotOwnPC / accomodation$AllHouseholds
accomodation$PC_ICTBroadband <- accomodation$ICT_BroadBand / accomodation$AllHouseholds
accomodation$PC_ICTNonBBInternet <- accomodation$ICT_InternetNonBroadband / accomodation$AllHouseholds
accomodation$PC_ICTNoInternet <- accomodation$ICT_NoInternet / accomodation$AllHouseholds
accomodation$PC_HeatingCentral <- accomodation$Heating_HasCentral / accomodation$AllHouseholds
accomodation$PC_HeatingNoCentral <- accomodation$Heating_NoCentral / accomodation$AllHouseholds
accomodation$PC_SeweragePublic <- accomodation$Sewerage_Public / accomodation$AllHouseholds
accomodation$PC_SewerageIndSeptic <- accomodation$Sewerage_IndSeptic / accomodation$AllHouseholds
accomodation$PC_SewerageIndNonSeptic <- accomodation$Sewerage_IndNonSeptic / accomodation$AllHouseholds
accomodation$PC_SewerageOther <- accomodation$Sewerage_Other / accomodation$AllHouseholds
accomodation$PC_SewerageNone <- accomodation$Sewerage_None / accomodation$AllHouseholds
accomodation$PC_WaterPublicMains <- accomodation$Water_PublicMains / accomodation$AllHouseholds
accomodation$PC_WaterPublicGroup <- accomodation$Water_PublicGroupScheme / accomodation$AllHouseholds
accomodation$PC_WaterPrivateGroup <- accomodation$Water_PrivateGroupScheme / accomodation$AllHouseholds
accomodation$PC_WaterOtherPrivate <- accomodation$Water_OtherPrivateSource / accomodation$AllHouseholds
accomodation$PC_WaterNoPiped <- accomodation$Water_NoPipedWater / accomodation$AllHouseholds
accomodation$PC_ICTInternet <- accomodation$PC_ICTBroadband + accomodation$PC_ICTNonBBInternet
accomodation$TownSize[accomodation$TotalPopulation < 2000] <- "< 2K"
accomodation$TownSize[accomodation$TotalPopulation >= 2000 & 
                        accomodation$TotalPopulation < 5000] <- "2K-5K"
accomodation$TownSize[accomodation$TotalPopulation >= 5000 & 
                        accomodation$TotalPopulation < 20000] <- "5K-20K"
accomodation$TownSize[accomodation$TotalPopulation > 20000] <- "> 20K"
accomodation$PeoplePerHouse <- accomodation$TotalPopulation / accomodation$AllHouseholds
accomodation$RentIncomeRatio <- accomodation$Avg_Rent / (accomodation$Household_Income / 12)
accomodation$TownSize <- factor(accomodation$TownSize,
       levels = c("< 2K", "2K-5K", "5K-20K", "> 20K"))
  
######################################################################
#                     DESCRIPTIVE STATISTICS
######################################################################

data_numeric_variables <- sapply(accomodation, is.numeric)
numerical_data <- accomodation[, data_numeric_variables]

numerical_summary <- do.call(cbind, lapply(numerical_data, summary))
numerical_summary

alt_numerical_summary <-  do.call(rbind, lapply(numerical_data, summary))
alt_numerical_summary

######################################################################
#                       REFORMAT DATASET
######################################################################

# Remove Area-local variables, and percentages with low variance
accomodation <- accomodation[, c("Towns.by.Size", "County", "TownSize", "PersonsPerSqKm", 
                                 "TotalPopulation", "AllHouseholds", "Household_Income", 
                                 "Avg_Rent", "PC_ICTOwnsPC", "PC_ICTInternet", 
                                 "PC_HeatingNoCentral", "PC_SeweragePublic", 
                                 "PC_SewerageIndSeptic", "RentIncomeRatio", "PeoplePerHouse")]

######################################################################
#                DESCRIPTIVE STATISTICS (cotd)
######################################################################

# Largest Rent
accomodation[which.max(accomodation$Avg_Rent),]
# Largest Population
accomodation[which.max(accomodation$TotalPopulation),]
# Highest Income
accomodation[which.max(accomodation$Household_Income),]
# Densest Town
accomodation[which.max(accomodation$PersonsPerSqKm),]

# Lowest Rent
accomodation[which.min(accomodation$Avg_Rent),]
# Lowest Population
accomodation[which.min(accomodation$TotalPopulation),]
# Lowest Income
accomodation[which.min(accomodation$Household_Income),]
# Lowest Density
accomodation[which.min(accomodation$PersonsPerSqKm),]


#install.packages("ggplot2")
#install.packages("viridis")
library(ggplot2)
library(viridis)

# Plot average rent per area, color by population
plot <- ggplot(accomodation, aes(x = TownSize, y = Avg_Rent, color = Household_Income))
plot <- plot + geom_point() + scale_color_viridis()
print((plot))

# Plot average rent per area, color by density
plot <- ggplot(accomodation, aes(x = TownSize, y = Avg_Rent, color = PersonsPerSqKm))
plot <- plot + geom_point() + scale_color_viridis()
print((plot))

# Plot average rent per area, color by overall population
plot <- ggplot(accomodation, aes(x = TownSize, y = Avg_Rent, color = TotalPopulation))
plot <- plot + geom_point() + scale_color_viridis()
print((plot))

# Plot average rent per area, color by people per house
plot <- ggplot(accomodation, aes(x = TownSize, y = Avg_Rent, color = PeoplePerHouse))
plot <- plot + geom_point() + scale_color_viridis()
print((plot))

# Investigate correlation between variables  
ict_variables_of_interest <- c("PC_ICTOwnsPC", "PC_ICTInternet", "Household_Income")
pairs(accomodation[ict_variables_of_interest])

physical_variables_of_interest <- c("PersonsPerSqKm", "TotalPopulation", "PC_HeatingNoCentral", 
                                    "PC_SeweragePublic", "PC_SewerageIndSeptic")
pairs(accomodation[physical_variables_of_interest])

financial_variables_of_interest <- c("PersonsPerSqKm", "TotalPopulation", "PeoplePerHouse",
                                     "Household_Income", "Avg_Rent", "RentIncomeRatio")
pairs(accomodation[financial_variables_of_interest])

colors <- rainbow(length(unique(accomodation$TownSize)))

ggplot(accomodation, aes(x=Avg_Rent, y=Household_Income, color=TownSize)) +
  geom_point() 
ggplot(accomodation, aes(x=Avg_Rent, y=PersonsPerSqKm, color=TownSize)) +
  geom_point() 
ggplot(accomodation, aes(x=TownSize, y=Household_Income, color=TownSize)) +
  geom_point() 
ggplot(accomodation, aes(x=PeoplePerHouse, y=RentIncomeRatio, color=TownSize)) +
  geom_point() 
ggplot(accomodation, aes(x=PeoplePerHouse, y=Avg_Rent, color=TownSize)) +
  geom_point() 

######################################################################
#                   Principal Component Analysis
######################################################################

data_numeric_variables <- sapply(accomodation, is.numeric)
data_numeric_variables

# Remove undesired columns from PCA dataset
accomodation_adjusted <- accomodation[, data_numeric_variables]

pca <- prcomp(accomodation_adjusted, center = TRUE, scale. = TRUE)
summary(pca)

# Inspect PCA details
str(pca)

library("factoextra")
eig_values <- get_eigenvalue(pca)
eig_values

library("FactoMineR")
pca2 <- PCA(accomodation_adjusted, graph = FALSE)
print(pca2)
pca2_eig_values <- get_eigenvalue(pca2)
pca2_eig_values


fviz_eig(pca, addlabels = TRUE, ylim = c(0, 50))

pca_for_variables <- get_pca_var(pca)
pca_for_variables

# Correlation plot - visualize proporion of contribution to the variance
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
             geom.ind = "point",
             col.ind = accomodation$TownSize, # colour by town size
             addEllipses = TRUE,
             legend.title = "Town Size"
)


biplot <- fviz_pca_ind(pca, geom = "point", col.ind = accomodation$Avg_Rent)
ggpubr::ggpar(biplot,
              title = "Principal Component Analysis",
              subtitle = "Average Rental in Ireland",
              caption = "Source: RTB, Census",
              xlab = "PC 1", ylab = "PC 2",
              legend.title = "Average Rent", legend.position = "top",
              ggtheme = theme_gray())

##############################################################################
#               Basic statistical analysis of variables
##############################################################################

hist(accomodation$PersonsPerSqKm)
hist(accomodation$TotalPopulation)
hist(accomodation$Household_Income)
hist(accomodation$Avg_Rent)
hist(accomodation$PC_ICTOwnsPC)
hist(accomodation$PC_ICTInternet)
hist(accomodation$PC_HeatingNoCentral)
hist(accomodation$PC_SeweragePublic)
hist(accomodation$PC_SewerageIndSeptic)
hist(accomodation$PC_WaterPublicMains)

library("ggpubr")
ggboxplot(accomodation, x = "TownSize", y = "PersonsPerSqKm", 
          color = "TownSize", palette = colors,
          ylab = "People per km2", xlab = "Town Size")

ggboxplot(accomodation, x = "TownSize", y = "Avg_Rent", 
          color = "TownSize", palette = colors,
          ylab = "Average Rent", xlab = "Area")

ggboxplot(accomodation, x = "TownSize", y = "Household_Income", 
          color = "TownSize", palette = colors,
          ylab = "Household Income", xlab = "Area")

ggboxplot(accomodation, x = "TownSize", y = "PeoplePerHouse", 
          color = "TownSize", palette = colors,
          ylab = "People per house", xlab = "Area")

write.csv(accomodation, "Data/HousingStatsProcessed.csv")

#Cork	C
#Clare	CE
#Cavan	CN
#Carlow	CW
#Dublin	D
#Donegal	DL
#Galway	G
#Kildare	KE
#Kilkenny	KK
#Kerry	KY
#Limerick	L
#Longford	LD
#Louth	LH
#Leitrim	LM
#Laois	LS
#Meath	MH
#Monaghan	MN
#Mayo	MO
#Offaly	OY
#Roscommon	RN
#Sligo	SO
#Tipperary	T
#Waterford	W
#Westmeath	WH
#Wexford	WX
#Wicklow	WW
