###Data Loading###
library(readxl)
library(openxlsx)
library(tidyr)
# Set the directory path where your files are located
directory_path <- "/Users/sarthak/Desktop/Latest Dissertation/"

# Create a list to store the data frames
data_list <- list()

# Loop through the files
for (i in 1:12) {
  # Generate the file name
  file_name <- paste0("hw_2012_", sprintf("%02d", i), ".xlsx")
  
  # Create the file path
  file_path <- paste0(directory_path, file_name)
  
  # Read the Excel file
  data <- read_excel(file_path)
  
  # Extract the year and month from the file name
  year <- substr(file_name, 4, 7)
  month <- substr(file_name, 9, 10)
  
  # Create new variables for year and month
  data$Year <- as.integer(year)
  data$Month <- as.integer(month)
  
  # Store the data frame in the list
  data_list[[i]] <- data
}

# Combine all data frames into a single data frame
combined_data <- do.call(rbind, data_list)

hw_2012=combined_data
# Reset row names if necessary
rownames(combined_data) <- NULL



####hw

file="/Users/sarthak/Desktop/Latest Dissertation/hw.xls"
hw_2013 <- read_excel(file ,sheet = 1)
hw_2014 <- read_excel(file ,sheet = 2,col_names = FALSE)
hw_2019 <- read_excel(file ,sheet = 3,col_names = FALSE)
hw_2022 <- read_excel(file ,sheet = 4,col_names = FALSE)

col_names <- colnames(hw_2013)
colnames(hw_2014) <- col_names
colnames(hw_2019) <- col_names
colnames(hw_2022) <- col_names

hw=rbind(hw_2013,hw_2014,hw_2019,hw_2022)

#### Merge it with 2012 data#####

hw_2012$continent_name=""


library(dplyr)
# Rearrange the dataset
reshaped_hw_2012 <- hw_2012 %>%
  select(continent_name,name,Year, Month, totalPositive, totalTested)


names(reshaped_hw_2012) =col_names


total_hw=rbind(reshaped_hw_2012,hw)


total_hw$total_positive= as.numeric(total_hw$total_positive)
total_hw$total_tested=as.numeric(total_hw$total_tested)

total_hw$percentage_positive=total_hw$total_tested/total_hw$total_positive

write.xlsx(total_hw, "/Users/sarthak/Desktop/Latest Dissertation/total_hw.xlsx")


lapi<- read_excel("/Users/sarthak/Desktop/Latest Dissertation/lapi1122.xlsx")

file2="USA.xlsx"
subnational1_tree_cover_loss <- read_excel(file2 ,sheet = 4)
subnational2_tree_cover_loss <- read_excel(file2 ,sheet = 6)

# Rename "subnational1" to "subnational"
subnational1_tree_cover_loss <- rename(subnational1_tree_cover_loss, subnational = subnational1)

# Rename "subnational1" to "subnational"
subnational2_tree_cover_loss<- rename(subnational2_tree_cover_loss, subnational = subnational2)


# Remove the "subnational1" variable
subnational2_tree_cover_loss  <- select(subnational2_tree_cover_loss , -subnational1)

# Merge the datasets
cover_loss_data <- bind_rows(subnational1_tree_cover_loss, subnational2_tree_cover_loss)


# Reshape the dataset
reshaped_data <- cover_loss_data %>%
  gather(key = "Year", value = "tc_loss_ha", starts_with("tc_loss_ha_"))

# Extract the year from the variable name
reshaped_data$Year <- gsub("tc_loss_ha_", "", reshaped_data$Year)

# Remove the original variable columns
reshaped_tree_cover_loss <- select(reshaped_data, -starts_with("tc_loss_ha_"))


write.xlsx(reshaped_tree_cover_loss , "tree_cover_loss.xlsx")

###

subnational1_carbon_data <- read_excel(file2 ,sheet = 5)
subnational2_carbon_data <- read_excel(file2 ,sheet = 7)

# Rename "subnational1" to "subnational"
subnational1_carbon_data <- rename(subnational1_carbon_data , subnational = subnational1)

# Rename "subnational1" to "subnational"
subnational2_carbon_data<- rename(subnational2_carbon_data , subnational = subnational2)


# Remove the "subnational1" variable
subnational2_carbon_data  <- select(subnational2_carbon_data  , -subnational1)

# Merge the datasets
carbon_data <- bind_rows(subnational1_carbon_data , subnational2_carbon_data )


colnames(carbon_data)


reshaped_data <- carbon_data %>%
  gather(key = "Year", value = "carbon_gross_emissions", starts_with("gfw_forest_carbon_gross_emissions_"))


# Extract the year from the variable name
reshaped_data$Year <- gsub("gfw_forest_carbon_gross_emissions_", "", reshaped_data$Year)
reshaped_data$Year <- gsub("__Mg_CO2e", "", reshaped_data$Year)
reshaped_data$Year <- as.integer(reshaped_data$Year)

# Remove the original variable columns
reshaped_carbon_data<- select(reshaped_data, -starts_with("gfw_forest_carbon_gross_emissions_"))


write.xlsx(reshaped_carbon_data, "carbon_data.xlsx")


##Data formatting & Merge###

library(readxl)
library(dplyr)
carbon_data=read_xlsx("carbon_data.xlsx")
forest_data=read_xlsx("forest data.xlsx")
total_hw=read_xlsx("total_hw.xlsx")
tree_cover_loss=read_xlsx("tree_cover_loss.xlsx")

View(carbon_data)
View(forest_data)
View(total_hw)
View(tree_cover_loss)

carbon_data<- rename(carbon_data, State= subnational)
total_hw<- rename(total_hw, State= county_name)
total_hw<- rename(total_hw, Year= year)
tree_cover_loss<- rename(tree_cover_loss, State= subnational)



merged_dataset <- merge(carbon_data,forest_data, by = "State")

merged_dataset_2 <- merge(total_hw,merged_dataset, by = "State")

total_rows <- nrow(merged_dataset_2 )
sample_size <- 10000
random_indices <- sample(total_rows, sample_size)
merged_dataset_2_N <- merged_dataset_2 [random_indices, ]


total_rows <- nrow(tree_cover_loss)
sample_size <- 10000
random_indices <- sample(total_rows, sample_size)
tree_cover_loss_1 <- tree_cover_loss[random_indices, ]


merged_dataset_3 <- merge(tree_cover_loss_1,merged_dataset_2_N, by = "State")



openxlsx::write.xlsx(merged_dataset_3, file = "input_data.xlsx", rowNames = FALSE)


########Analysis Part###############


library(readxl)
library(ggplot2)
library(caret)
dataset=read_xlsx("input_data.xlsx")

View(dataset)
colnames(data)
# View the structure of the dataset
str(data)


# Step 2: Combine year variables
dataset$Year <- ifelse(is.na(dataset$Year.x...8), dataset$Year.y...11, dataset$Year.x...8)
dataset$Year <- ifelse(is.na(dataset$Year), dataset$Year.x...22, dataset$Year)

# Step 3: Remove duplicate Year variable and unneeded variables
dataset <- dataset[, !(names(dataset) %in% c("Year.x...8", "Year.y...11", "Year.x...22", "Year.x...23","Year.y...25","country.x","country.y","month","total_tested","continent_name","total_positive"))]


# Step 4: Deal with missing values
# Identify variables with missing values
vars_with_missing <- colnames(dataset)[apply(dataset, 2, function(x) any(is.na(x)))]

# Replace missing values with mean
for (var in vars_with_missing) {
  dataset[is.na(dataset[[var]]), var] <- mean(dataset[[var]], na.rm = TRUE)
}

##data exploration
str(dataset)
dataset$`Percent forest`=as.double(dataset$`Percent forest`)
dataset$Year=as.double(dataset$Year)
summary(dataset[ c("threshold", "area_ha", "extent_2000_ha", "extent_2010_ha", "gain_2000-2012_ha", "tc_loss_ha", "percentage_positive", "umd_tree_cover_density_2000__threshold", "umd_tree_cover_extent_2000__ha", "whrc_aboveground_biomass_stock_2000__Mg", "avg_whrc_aboveground_biomass_2000_Mg_ha-1", "gfw_forest_carbon_gross_removals__Mg_CO2_yr-1", "gfw_forest_carbon_net_flux__Mg_CO2e_yr-1",  "carbon_gross_emissions", "Percent forest", "Year")])

par(mfrow = c(3, 7))

for (var in c("threshold", "area_ha", "extent_2000_ha", "extent_2010_ha", "gain_2000-2012_ha", "tc_loss_ha", "percentage_positive", "umd_tree_cover_density_2000__threshold", "umd_tree_cover_extent_2000__ha", "whrc_aboveground_biomass_stock_2000__Mg", "avg_whrc_aboveground_biomass_2000_Mg_ha-1", "gfw_forest_carbon_gross_removals__Mg_CO2_yr-1", "gfw_forest_carbon_net_flux__Mg_CO2e_yr-1",  "carbon_gross_emissions", "Percent forest", "Year")) {
  hist(dataset[[var]], main = var, xlab = var)
}

pairs(dataset[, c("threshold", "area_ha", "extent_2000_ha", "extent_2010_ha", "gain_2000-2012_ha", "tc_loss_ha", "percentage_positive", "umd_tree_cover_density_2000__threshold", "umd_tree_cover_extent_2000__ha", "whrc_aboveground_biomass_stock_2000__Mg", "avg_whrc_aboveground_biomass_2000_Mg_ha-1", "gfw_forest_carbon_gross_removals__Mg_CO2_yr-1", "gfw_forest_carbon_net_flux__Mg_CO2e_yr-1",  "carbon_gross_emissions", "Percent forest", "Year")])
e
par(mfrow = c(2, 2))

barplot(table(dataset$State), main = "State")
barplot(table(dataset$Year), main = "Year")

cor_matrix <- cor(dataset[, c("threshold", "area_ha", "extent_2000_ha", "extent_2010_ha", "gain_2000-2012_ha", "tc_loss_ha", "percentage_positive", "umd_tree_cover_density_2000__threshold", "umd_tree_cover_extent_2000__ha", "whrc_aboveground_biomass_stock_2000__Mg", "avg_whrc_aboveground_biomass_2000_Mg_ha-1", "gfw_forest_carbon_gross_removals__Mg_CO2_yr-1", "gfw_forest_carbon_net_flux__Mg_CO2e_yr-1",  "carbon_gross_emissions", "Percent forest")])
print(cor_matrix)

library(corrplot)
corrplot(cor_matrix, method = "color")

library(ggplot2)

ggplot(dataset, aes(x = Year, y = percentage_positive)) +
  geom_line() +
  labs(x = "Year", y = "positive cases", title = "Trends in positive over Time")

ggplot(data = dataset, aes(x = State, y = percentage_positive)) +
  stat_summary(fun = "mean", geom = "bar") +
  stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", width = 0.2) +
  labs(x = "State", y = "percentage_positive", title = "Mean percentage_positive by State") +
  theme(axis.text.x = element_text(angle = 90))

# Modeling part
# Rename the column
dataset<- dataset %>%
  rename(gain_2000_2012_ha = `gain_2000-2012_ha`,gfw_forest_carbon_gross_removals__Mg_CO2_yr_1=`gfw_forest_carbon_gross_removals__Mg_CO2_yr-1`
         
         ,gfw_forest_carbon_net_flux__Mg_CO2e_yr_1=`gfw_forest_carbon_net_flux__Mg_CO2e_yr-1`,avg_whrc_aboveground_biomass_2000_Mg_ha_1=`avg_whrc_aboveground_biomass_2000_Mg_ha-1`)


set.seed(42)
train_indices <- sample(1:nrow(dataset), 0.7 * nrow(dataset))
train_data <- dataset[train_indices, ]
test_data <- dataset[-train_indices, ]

total_rows <- nrow(train_data )
sample_size <- 10000
random_indices <- sample(total_rows, sample_size)
train_data <- train_data [random_indices, ]

total_rows <- nrow(test_data )
sample_size <- 3000
random_indices <- sample(total_rows, sample_size)
test_data <- test_data [random_indices, ]


# Train the linear regression model
lm_model <- lm(percentage_positive ~extent_2000_ha+threshold+area_ha+extent_2000_ha+gain_2000_2012_ha+ tc_loss_ha+umd_tree_cover_density_2000__threshold    
               +Year+umd_tree_cover_extent_2000__ha+whrc_aboveground_biomass_stock_2000__Mg+gfw_forest_carbon_gross_removals__Mg_CO2_yr_1+gfw_forest_carbon_net_flux__Mg_CO2e_yr_1+avg_whrc_aboveground_biomass_2000_Mg_ha_1+carbon_gross_emissions, data = train_data)


# Make predictions on the test set
lm_predictions <- predict(lm_model, newdata = test_data)

# Evaluate the model

#Calculate RMSE
lm_rmse <- sqrt(mean((test_data$percentage_positive - lm_predictions)^2))


#Calculate MAE
lm_mae <- mean(abs(test_data$percentage_positive - lm_predictions))


#Calculate MAPE
lm_mape <- mean(abs((test_data$percentage_positive - lm_predictions) / test_data$percentage_positive)) * 100

print(lm_rmse)
print(lm_mae)
print(lm_mape)


library(randomForest)

# Train the random forest model
rf_model <- randomForest(percentage_positive ~extent_2000_ha+threshold+area_ha+extent_2000_ha+gain_2000_2012_ha+ tc_loss_ha+umd_tree_cover_density_2000__threshold    
          +Year+umd_tree_cover_extent_2000__ha+whrc_aboveground_biomass_stock_2000__Mg+gfw_forest_carbon_gross_removals__Mg_CO2_yr_1+gfw_forest_carbon_net_flux__Mg_CO2e_yr_1+avg_whrc_aboveground_biomass_2000_Mg_ha_1+carbon_gross_emissions, data = train_data)
# Make predictions on the test set
rf_predictions <- predict(rf_model, newdata = test_data)

# Evaluate the model

#Calculate RMSE
rf_rmse <- sqrt(mean((test_data$percentage_positive - rf_predictions)^2))

#Calculate MAE
rf_mae <- mean(abs(test_data$percentage_positive - rf_predictions))


#Calculate MAPE
rf_mape <- mean(abs((test_data$percentage_positive - rf_predictions) / test_data$percentage_positive)) * 100

print(rf_rmse)
print(rf_mae)
print(rf_mape)


###Xgboost
library(xgboost)

# Train the xgboost model
xgb_model <- xgboost(data = as.matrix(train_data[,setdiff(names(train_data), c("percentage_positive","`Percent forest`","State"))]),
                     label = train_data$percentage_positive,
                     nrounds = 100,
                     verbose = 0)

# Make predictions on the test set
xgb_predictions <- predict(xgb_model, newdata = as.matrix(test_data[,setdiff(names(test_data), c("percentage_positive","`Percent forest`","State"))]))

# Evaluate the model

#Calculate RMSE
xgb_rmse <- sqrt(mean((test_data$percentage_positive - xgb_predictions)^2))

#Calculate MAE
xgb_mae <- mean(abs(test_data$percentage_positive - xgb_predictions))


#Calculate MAPE
xgb_mape <- mean(abs((test_data$percentage_positive - xgb_predictions) / test_data$percentage_positive)) * 100


print(xgb_rmse)
print(xgb_mae)
print(xgb_mape)

library(rpart)

# Train the decision tree model
dt_model <- rpart(percentage_positive ~extent_2000_ha+threshold+area_ha+extent_2000_ha+gain_2000_2012_ha+ tc_loss_ha+umd_tree_cover_density_2000__threshold    
                  +Year+umd_tree_cover_extent_2000__ha+whrc_aboveground_biomass_stock_2000__Mg+gfw_forest_carbon_gross_removals__Mg_CO2_yr_1+gfw_forest_carbon_net_flux__Mg_CO2e_yr_1+avg_whrc_aboveground_biomass_2000_Mg_ha_1+carbon_gross_emissions, data = train_data)


# Make predictions on the test set
dt_predictions <- predict(dt_model, newdata = test_data)

# Evaluate the model
#Calculate RMSE
dt_rmse <- sqrt(mean((test_data$percentage_positive - dt_predictions)^2))

#Calculate MAE
dt_mae <- mean(abs(test_data$percentage_positive - dt_predictions))


#Calculate MAPE
dt_mape <- mean(abs((test_data$percentage_positive - dt_predictions) / test_data$percentage_positive)) * 100


print(dt_rmse)
print(dt_mae)
print(dt_mape)


