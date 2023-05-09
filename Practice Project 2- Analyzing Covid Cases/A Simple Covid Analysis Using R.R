library(readr)
library(tibble)
library(dplyr)


covid_df <- read_csv("C:/Users/chimi/Desktop/R Data Science Projects/R Data Science Project 2/covid19.csv")

# Dimension of the dataframe
dim(covid_df)

# Columnn names
vector_cols <- colnames(covid_df)
print(vector_cols)

# Display the first few rows
head(covid_df)

# 
glimpse(covid_df)


# 


# filter rows where Province_State is "All States" and remove Province_State column
covid_df_all_states <- covid_df %>% 
  filter(Province_State == "All States") %>% 
  select(-Province_State)

# Daily Measures
# select specific columns related to daily measures
covid_df_all_states_daily <- covid_df_all_states %>% 
  select(Date, Country_Region, active, hospitalizedCurr, daily_tested, daily_positive)


# Summarize the data by Country_Region
covid_df_all_states_daily_sum <- covid_df_all_states_daily %>% 
  group_by(Country_Region) %>% 
  summarize(tested = sum(daily_tested),
            positive = sum(daily_positive),
            active = sum(active),
            hospitalized = sum(hospitalizedCurr)) %>% 
  arrange(desc(tested))

# Extract the top ten rows from the covid_df_all_states_daily_sum

covid_top_10 <- head(covid_df_all_states_daily_sum, 10)


# create vectors from covid_top_10 dataframe
countries <- covid_top_10$Country_Region
tested_cases <- covid_top_10$tested
positive_cases <- covid_top_10$positive
active_cases <- covid_top_10$active
hospitalized_cases <- covid_top_10$hospitalized


# name vectors with countries
names(tested_cases) <- countries
names(positive_cases) <- countries
names(active_cases) <- countries
names(hospitalized_cases) <- countries


# divide positive_cases by tested_cases
positive_tested_ratio <- positive_cases / tested_cases

# get top three ratios
top_3_ratios <- sort(positive_tested_ratio, decreasing = TRUE)[1:3]

# create named vector of top 3 ratios
positive_tested_top_3 <- setNames(top_3_ratios, names(positive_tested_ratio)[positive_tested_ratio %in% top_3_ratios])
positive_tested_top_3

# Create vectors
united_kingdom <- c(0.11, 1473672, 166909, 0, 0)
united_states <- c(0.10, 17282363, 1877179, 0, 0)
turkey <- c(0.08, 2031192, 163941, 2980960, 0)

# Create matrix
covid_mat <- rbind(united_kingdom, united_states, turkey)

# Rename columns
colnames(covid_mat) <- c("Ratio", "tested", "positive", "active", "hospitalized")
print(colnames)


question <- "Which countries have had the highest number of positive cases against the number of tests?"

answer <- c("Positive tested cases" = positive_tested_top_3).


# Create data structures
df1 <- data.frame(a = c(1,2,3), b = c(4,5,6))
df2 <- data.frame(x = c(7,8,9), y = c(10,11,12))
mat1 <- matrix(1:6, ncol = 2)
mat2 <- matrix(7:12, ncol = 2)
vec1 <- c(1,2,3)
vec2 <- c(4,5,6)

# Create lists
df_list <- list(df1, df2)
mat_list <- list(mat1, mat2)
vec_list <- list(vec1, vec2)

# Name lists
names(df_list) <- c("df1", "df2")
names(mat_list) <- c("mat1", "mat2")
names(vec_list) <- c("vec1", "vec2")

# Combine into named list
data_structure_list <- list(dataframes = df_list, matrices = mat_list, vectors = vec_list)

# Create final list
covid_analysis_list <- list(question = "What is the current COVID-19 situation?", answer = "The situation is still ongoing.", data_structure_list)


# Display second element
covid_analysis_list[[2]]
