library(dplyr)
library(tidymodels)
tidymodels_prefer()
library(DescTools)
library(stringr)
library(caret)
library(randomForest)
# Load the file
ames <- read.csv("C:/Users/chimi/Desktop/R Data Science Projects/AmesHousing.csv")

# DataFrame
str(ames)

# Number of Rows and Columns
dim(ames)

# Another way of printing rows and columns
nrow(ames)
ncol(ames)


# Column Names
colnames(ames)


# Another
head(ames)

# Summary of Ames Housing
summary(ames)


# Drop all irrelevant columns
ames <- select(ames, -Order,-PID)

# Check for duplicates
sum(duplicated(ames))

# Check for Missing values
print(colMeans(is.na(ames)))

#Dropping columns which consists of more than 50% missing values

ames <- select(ames,-Alley,- Pool.QC,-Fence,-Misc.Feature)

# Handle NULL values for Numeric Features
numeric_cols <- colnames(ames[sapply(ames, is.numeric) == TRUE])

for (col_name in numeric_cols) {
  
  if (any(is.na(ames[[col_name]]))) {
    mean_value <- mean(ames[[col_name]], na.rm = TRUE)
    ames[col_name][is.na(ames[col_name])] <- mean_value
    message <- paste("Null values of", col_name, "feature have been replaced by the mean value", mean_value)
    print(message)
  }
}

# Handle Missing values in Categorical Variables
# Find the mode of each categorical variable
# Load the DescTools package

# Select the categorical variables
cat_vars <- ames %>% select_if(is.character) %>% names()

for (var in cat_vars) {
  mode_val <- Mode(ames[[var]], na.rm = TRUE)
  ames[[var]] <- ifelse(is.na(ames[[var]]), mode_val, ames[[var]])
}

# Housing EDA
colSums(is.na(ames))

# Un-variate-analysis of the target variable - Right skewed with a round bound tail
ggplot(ames, aes(x = SalePrice)) +
  geom_histogram(fill = 'purple', color = 'black') +
  labs(x = 'Sale Price', y='Frequency' , title = 'Histogram of Sale Price') +
  theme_classic()

#Log Scaled

ggplot(ames, aes(x = SalePrice)) +
  geom_histogram(fill = 'purple', color = 'black')  +
  labs(x = 'SalePrice', y = 'Frequency', title = "Histogram of Sale Price with Logarithmic X-Axis") +
  scale_x_continuous(trans = 'log10') +
  theme_classic()

# Un-variate Analysis of categorical variables
for (col in names(ames)) {
  # Check if the column contains character class
  if (is.character(ames[[col]])) {
    # Print the name of the column
    print(col)
    
    # Plot a countplot of the character variable
    p1 <- ggplot(ames, aes_string(x = col)) +
      geom_bar(fill = 'purple', color = 'black') +
      labs(x = col, y = 'Frequency', title = paste('Countplot of', col)) +
      theme_classic()
    
    # Display the plot in the default graphical device
    print(p1)
  }
}
# Data Cleaning after evaluating CountPlots
# C (all) and I (all)- barely account 0 frequency
ames <- subset(ames, !(MS.Zoning %in% c('A (agr)', 'I (all)')))

# Drop Gravel from Street Parking as there is no variance
ames <- subset(ames, !(Street %in% c('Grvl')))

# Assign LV1 new class
ames$Land.Contour <- as.integer(ames$Land.Contour == 'Lvl')

# Drop NoSeWa and NoSewr from utilities
ames <- subset(ames, !(Utilities %in% c('NoSeWa', 'NoSewr')))

# Land Slope
ames$Land.Slope <- as.integer(ames$Land.Slope == 'Gtl')

#Lands Filter and Convert to binary

ames <- subset(ames, !(Condition.1 %in% c('RRNe','RRNn')))
ames$Condition.1 <- as.integer(ames$Condition.1 == 'Norm')


ames <- subset(ames, !(Condition.2 %in% c('RRAe','RRAn','RRNn')))
ames$Condition.2 <- as.integer(ames$Condition.2 == 'Norm')


# Create a new level for Building
ames$Bldg.Type <- as.integer (ames$Bldg.Type == '1Fam')


# Roof
ames <- subset(ames, !( Roof.Matl %in% c('ClyTile','Membran','Metal','Roll')))

# Mas
ames <- subset(ames, !(Mas.Vnr.Type %in% c('CBlock')))

# Drop few labels
ames <- subset(ames, !(Exter.Cond %in% c('Po')))

# Basements
ames <- subset(ames, !(Bsmt.Qual %in% c('Po','')))
ames <- subset(ames, !(Bsmt.Cond %in% c('','Ex','Po')))
ames <- subset(ames, !(Bsmt.Exposure %in% c('')))
ames <- subset(ames, !(BsmtFin.Type.1 %in% c('')))
ames <- subset(ames, !(BsmtFin.Type.2 %in% c('','NA')))
# Categorize BsmtFin.Type2
ames$Bsmt.Cond <- as.integer(ames$Bsmt.Cond == 'TA')
ames$BsmtFin.Type.2 <- as.integer(ames$BsmtFin.Type.2 == 'Unf')

# Heating
ames <- subset(ames, !(Heating %in% c('Floor','GasW','Grav','OthW','Wall')))
ames <- subset(ames, !(Heating.QC  %in% c('Po')))

# Electrical
ames <- subset(ames, !(Electrical  %in% c('','Mix','FuseP')))

# Kitchen Quality
ames <- subset(ames, !(Kitchen.Qual  %in% c('Po')))

# Functional
ames <- subset(ames, !(Functional %in% c('Sal','Sev')))
ames$Functional <- as.integer(ames$Functional == 'Typ')

#Garage
ames <- subset(ames, !(Garage.Finish %in% c('')))
ames <- subset(ames, !(Garage.Qual %in% c('', 'Ex', 'Po')))
ames <- subset(ames, !(Garage.Cond %in% c('', 'Ex')))
 
# Binary Conversion of Garage Condition
ames$Garage.Qual <- as.integer(ames$Garage.Qual == 'TA')
ames$Garage.Cond <- as.integer(ames$Garage.Cond == 'TA')

# Paved Drive
ames$Paved.Drive <- as.integer(ames$Paved.Drive == 'Y')

# Sales
ames <- subset(ames, !(Sale.Type %in% c('VWD')))
ames$Sale.Type <- as.integer(ames$Sale.Type == 'WD')


# How many houses were sold each Month?
monthly_sales <- ames %>%
  group_by(Mo.Sold) %>%
  summarise(n = n()) %>%
  mutate(Month = factor(month.abb[Mo.Sold], levels = month.abb))

ggplot(data = monthly_sales, aes(x = Month, y = n)) +
  geom_col(fill = "purple", color = "black") +
  geom_text(aes(label = n), vjust = -0.2, color = "black", size = 3) +
  labs(title = 'Properties Sold in Ames, Iowa',
       x = 'Month',
       y = 'Number of properties') +
  theme_minimal()

# What Year has the highest record for number of House Sales?
yearly_sales <- ames %>%
  group_by(Yr.Sold) %>%
  summarise(n = n())

ggplot(data = yearly_sales, aes(x = Yr.Sold, y = n)) +
  geom_col(fill = 'purple', color = 'black') +
  geom_text(aes(label = n), vjust = -0.2, color = "black", size = 3) +
  labs(title = "Individual residential properties sold in Ames, IA from 2006 to 2010",
       x = 'Year Sold',
       y = 'Number of Properties') +
  theme_minimal()


#Bivariate Analysis between categorical and numerical from few variables to give insights
selected_vars <- c('Neighborhood','Street', 'MS.Zoning', 'House.Style', 'Central.Air', 'Heating', 'Garage.Qual', 'Kitchen.Qual', 'Paved.Drive', 'Sale.Type', 'Sale.Condition')

for (col in selected_vars){
  
  #Plot the boxplot
  p2 <- ggplot(ames, aes(x=col, y= SalePrice)) +
    geom_boxplot(fill='purple',color='black') +
    labs(x = col, y = 'SalePrice', title = paste('Bivariate Relationship between', col, "and Sales Price")) +
    theme_classic()
  
  #Display the plot
  print(p2)
}

# Analysis between Numerical and some target variables

# Create a list of numerical variables
numerical_vars <- c('Lot.Area','Overall.Cond','Gr.Liv.Area','Total.Bsmt.SF','X1st.Flr.SF','X2nd.Flr.SF','Garage.Area','Open.Porch.SF','Pool.Area')

  # Loop through each numerical variable
  for (var in numerical_vars) {
  
# Create a scatterplot of the variable and SalePrice
  p3 <- ggplot(ames, aes(x = get(var), y = SalePrice)) +
    geom_point(color = "purple") +
    labs(x = var, y = 'SalePrice', title = paste("Scatterplot of", var, "and SalePrice")) +
    theme_classic()
  
  # Display the plot
  print(p3)
}

# Multivariate- Analysis

# What was the average monthly sales by Neighborhood?
monthly_sales_by_neighborhood <- ames %>%
  group_by(Neighborhood, Mo.Sold) %>%
  summarise(mean_sale = mean(SalePrice))

ggplot(monthly_sales_by_neighborhood, aes(x = Neighborhood, y = mean_sale, fill = factor(Mo.Sold))) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = c('#a6cee3', '#b2df8a', '#fdae6b', '#d9d9d9', '#bcbddc', '#ffffb3', '#ff7f00', '#fb9a99', '#b15928', '#c9a0dc', '#90ee90', '#add8e6')) +
  labs(x = 'Neighborhood', y = 'Average Monthly Sale', fill = 'Month of Sale') +
  ggtitle('Average Month Sales by Neighborhood') +
  theme_minimal()

# What was the average year sales by neighborhood?
yearly_sales_by_neighborhood <- ames %>%
  group_by(Neighborhood, Yr.Sold) %>%
  summarise(mean_sale = mean(SalePrice))

ggplot(yearly_sales_by_neighborhood , aes(x=Neighborhood, y = mean_sale, fill = Yr.Sold)) +
  geom_bar(stat='identity') +
  scale_fill_gradient(low = 'pink', high = 'purple', name = 'Month of Sale') +
  labs(x ='Neighborhood', y = 'Average Yearly Sale', fill='Year of Sale') +
  ggtitle('Average Yearly Sales by Neighborhood')+
  theme_minimal()



# Convert some of the numerical variables labeled as numeric to factors

# Define a vector of variable names to convert
# Display 5 rows of the data
head(ames,5)

# Handling Year Features

# Age of The House
ames$age_at_sale <- ames$Yr.Sold - ames$Year.Built

# Remodel
ames$time_since_remodel <- ames$Yr.Sold - ames$Year.Remod.Add

# Season of Sale
ames$season <- ifelse(ames$Mo.Sold %in% c(3,4,5), 'spring',
                              ifelse(ames$Mo.Sold %in% c(6,7,8), 'summer',
                                     ifelse(ames$Mo.Sold %in% c(9,10,11), 'fall',
                                            'winter')))

# Total Living Area
ames$total_living_area <- ames$Gr.Liv.Area + ames$Total.Bsmt.SF + ames$X1st.Flr.SF

# Total Porch
ames$total_porch_area <- ames$Open.Porch.SF + ames$Enclosed.Porch + ames$X3Ssn.Porch + ames$Screen.Porch


# Total Bath
ames$total_bath = ames$Bsmt.Full.Bath + 1/2 * ames$Bsmt.Half.Bath + ames$Full.Bath + 1/2 * ames$Half.Bath

# TotalFlRSF
ames$total_flr_sf <- ames$X1st.Flr.SF + ames$X2nd.Flr.SF

# overall Grade
ames$overall_condition <- (ames$Overall.Cond * ames$Overall.Qual)

# Kitchen Grade
ames$kitchen_score <- ames$Kitchen.AbvGr * as.numeric(factor(ames$Kitchen.Qual, levels=c('Ex', 'Fa' , 'Gd', 'TA' )))

#Basement Score
ames$basement_grade <- as.numeric(factor(ames$Bsmt.Qual, levels=c('Ex', 'Fa', 'Gd', 'TA'))) *
  as.numeric(factor(ames$BsmtFin.Type.1, levels=c('ALQ','BLQ','GLQ','LwQ','Rec','Unf')))


# Garage Score
ames$garage_score <- ames$Garage.Qual * ames$Garage.Cond

# Exeter Score
ames$exterior_score <- as.numeric(factor(ames$Exter.Qual, levels=c('Ex','Fa','Gd','TA' ))) *
  as.numeric(factor(ames$Exter.Cond, levels=c('Ex','Fa','Gd','TA')))


# Encode Central AC
ames$Central = as.integer(ames$Central.Air == 'Y')

# Price Per Square Foot
ames$total_area <- ames$Gr.Liv.Area + ames$Total.Bsmt.SF + ames$Garage.Area + ames$Wood.Deck.SF + ames$Open.Porch.SF + ames$Enclosed.Porch+ ames$X3Ssn.Porch + ames$Screen.Porch


# Handling Features Based on Area- SQ FT
columns <- colnames(ames)

columns_to_drop <- c('Street', 'Utilities', 'Year.Built', 'Year.Remod.Add', 'Yr.Sold', 'Garage.Yr.Blt',
                     'Garage.Type', 'Paved.Drive', 'Land.Contour', 'Land.Slope', 'Neighborhood', 'Exterior.1st',
                     'Exterior.2nd', 'Electrical', 'X2nd.Flr.SF', 'Mas.Vnr.Type', 'Wood.Deck.SF', 'Open.Porch.SF',
                     'Enclosed.Porch', 'X3Ssn.Porch', 'Screen.Porch', 'Pool.Area', 'Low.Qual.Fin.SF', 'Misc.Val',
                     'Foundation','Heating')

predictors <- setdiff(columns, columns_to_drop)
ames <- ames[, predictors]


# Convert character variables to factors
# select categorical variables to encode
cat_vars <- c('MS.SubClass','MS.Zoning', 'Lot.Shape', 'Lot.Config', 'House.Style', 'Roof.Style', 'Roof.Matl', 'Exter.Qual',
              'Exter.Cond', 'Bsmt.Qual', 'Bsmt.Exposure', 'BsmtFin.Type.1', 'Heating.QC', 'Central.Air',
              'Kitchen.Qual', 'Fireplace.Qu', 'Garage.Finish', 'Sale.Condition', 'season')

# One-hot encode categorical variables and store the encoded columns in a new dataframe
encoded_cols <- model.matrix(~.+0, data = ames[, cat_vars])

# Combine the encoded columns with the original dataframe
ames <- cbind(ames, encoded_cols)

# Remove original categorical columns
ames <- ames[, !names(ames) %in% cat_vars]


# Convert everything to numeric
ames <- apply(ames, 2, function(x) as.numeric(as.character(x)))

# Convert price to log form/Nornalize it
ames <-  log(ames$SalePrice)

# Convert back into dataframe
ames <- data.frame(ames)


set.seed(123) # for reproducibility
trainIndex <- createDataPartition(ames$SalePrice, p = 0.8, list = FALSE)
train <- ames[trainIndex,]
test <- ames[-trainIndex,]


# Remove the variables with Zero-Variance
train <- train[, -nearZeroVar(train)]


# Build a simple linear regression model using the train dataset
model <- lm(SalePrice ~ ., data = train)

# Print the summary of the model
# R^2 0.88
summary(model)



