# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)

# Set Directory
setwd('C:/Users/Jithin Jayachandran/Downloads/R')

# Load Data
df <- read.csv('cities_by_gdp.csv')
View(df)


#Exploring Data

# Display the structure of the dataframe
str(df)

# Display summary statistics
summary(df)

# Check for missing values
sum(is.na(df))

# Display the first few rows of the dataframe
head(df)


# Handling Data

# Convert columns to appropriate data types
df$Estimated_GDP_Billion_USD <- as.numeric(gsub(",", "", df$Estimated_GDP_Billion_USD))
df$Metropolitian_Population <- as.numeric(gsub(",", "", df$Metropolitian_Population))

# Adjust the column names to remove special characters and spaces for easier handling
names(df) <- gsub(" ", "_", names(df))
names(df) <- gsub("/", "", names(df))

# Verify the changes
str(df)

# name of columns
names(df)

# Analyzing the Data

# 1. Distribution of GDP among cities
ggplot(df, aes(x=Estimated_GDP_Billion_USD)) + 
  geom_histogram(binwidth=10, fill='blue', color='black') +
  labs(title="Distribution of GDP among Cities", x="GDP (Billion USD)", y="Frequency")

# 2. Distribution of populations among cities
ggplot(df, aes(x=Metropolitian_Population)) + 
  geom_histogram(binwidth=500000, fill='green', color='black') +
  labs(title="Distribution of Population among Cities", x="Population", y="Frequency")


# 3. Top 10 cities by GDP
top_10_gdp <- df %>% arrange(desc(Estimated_GDP_Billion_USD)) %>% head(10)
ggplot(top_10_gdp, aes(x=reorder(Metropolitian.Area.City, Estimated_GDP_Billion_USD), y=Estimated_GDP_Billion_USD)) + 
  geom_bar(stat='identity', fill='blue') +
  coord_flip() +
  labs(title="Top 10 Cities by GDP", x="City", y="GDP (Billion USD)")

# 4. Top 10 cities by population
top_10_population <- df %>% arrange(desc(Metropolitian_Population)) %>% head(10)
ggplot(top_10_population, aes(x=reorder(Metropolitian.Area.City, Metropolitian_Population), y=Metropolitian_Population)) + 
  geom_bar(stat='identity', fill='#f94144') +
  coord_flip() +
  labs(title="Top 10 Cities by Population", x="City", y="Population")


# 5. Correlation between GDP and population
ggplot(df, aes(x=Metropolitian_Population, y=Estimated_GDP_Billion_USD)) + 
  geom_point(color='blue') +
  geom_smooth(method='lm', color='red') +
  labs(title="Correlation between GDP and Population", x="Population", y="GDP (Billion USD)")
cor(df$Metropolitian_Population, df$Estimated_GDP_Billion_USD, use='complete.obs')


# 6. Average GDP by country/region
avg_gdp_country <- df %>% group_by(Country.Region) %>% summarize(avg_GDP = mean(Estimated_GDP_Billion_USD, na.rm=TRUE))
ggplot(avg_gdp_country, aes(x=reorder(Country.Region, avg_GDP), y=avg_GDP)) + 
  geom_bar(stat='identity', fill='purple') +
  coord_flip() +
  labs(title="Average GDP by Country/Region", x="Country/Region", y="Average GDP (Billion USD)")

# 7. Average population by country/region
avg_pop_country <- df %>% group_by(Country.Region) %>% summarize(avg_Population = mean(Metropolitian_Population, na.rm=TRUE))
ggplot(avg_pop_country, aes(x=reorder(Country.Region, avg_Population), y=avg_Population)) + 
  geom_bar(stat='identity', fill='orange') +
  coord_flip() +
  labs(title="Average Population by Country/Region", x="Country/Region", y="Average Population")

# 8. Total GDP by country/region
total_gdp_country <- df %>% group_by(Country.Region) %>% summarize(total_GDP = sum(Estimated_GDP_Billion_USD, na.rm=TRUE))
ggplot(total_gdp_country, aes(x=reorder(Country.Region, total_GDP), y=total_GDP)) + 
  geom_bar(stat='identity', fill='red') +
  coord_flip() +
  labs(title="Total GDP by Country/Region", x="Country/Region", y="Total GDP (Billion USD)")

# 9. Total population by country/region
total_population_country <- df %>% group_by(Country.Region) %>% summarize(total_Population = sum(Metropolitian_Population, na.rm=TRUE))
ggplot(total_population_country, aes(x=reorder(Country.Region, total_Population), y=total_Population)) + 
  geom_bar(stat='identity', fill='#952748') +
  coord_flip() +
  labs(title="Total Population by Country/Region", x="Country/Region", y="Total Population")


# 10. Median GDP by country/region
median_gdp_country <- df %>% group_by(Country.Region) %>% summarize(median_GDP = median(Estimated_GDP_Billion_USD, na.rm=TRUE))
ggplot(median_gdp_country, aes(x=reorder(Country.Region, median_GDP), y=median_GDP)) + 
  geom_bar(stat='identity', fill='brown') +
  coord_flip() +
  labs(title="Median GDP by Country/Region", x="Country/Region", y="Median GDP (Billion USD)")

# 11. Median population by country/region
median_population_country <- df %>% group_by(Country.Region) %>% summarize(median_Population = median(Metropolitian_Population, na.rm=TRUE))
ggplot(median_population_country, aes(x=reorder(Country.Region, median_Population), y=median_Population)) + 
  geom_bar(stat='identity', fill='pink') +
  coord_flip() +
  labs(title="Median Population by Country/Region", x="Country/Region", y="Median Population")

# 12. Distribution of GDP by country/region
ggplot(df, aes(x=Estimated_GDP_Billion_USD, fill=Country.Region)) + 
  geom_density(alpha=0.5) +
  labs(title="Distribution of GDP by Country/Region", x="GDP (Billion USD)", y="Density")

# 13. Distribution of population by country/region
ggplot(df, aes(x=Metropolitian_Population, fill=Country.Region)) + 
  geom_density(alpha=0.5) +
  labs(title="Distribution of Population by Country/Region", x="Population", y="Density")

# 14. Cities with GDP greater than a specific threshold (e.g., 100 billion USD)
threshold <- 100
high_gdp_cities <- df %>% filter(Estimated_GDP_Billion_USD > threshold)
ggplot(high_gdp_cities, aes(x=reorder(Metropolitian.Area.City, Estimated_GDP_Billion_USD), y=Estimated_GDP_Billion_USD)) + 
  geom_bar(stat='identity', fill='cyan') +
  coord_flip() +
  labs(title=paste("Cities with GDP Greater Than", threshold, "Billion USD"), x="City", y="GDP (Billion USD)")

# 15. Cities with population greater than a specific threshold (e.g., 5 million)
population_threshold <- 5000000
high_population_cities <- df %>% filter(Metropolitian_Population > population_threshold)
ggplot(high_population_cities, aes(x=reorder(Metropolitian.Area.City, Metropolitian_Population), y=Metropolitian_Population)) + 
  geom_bar(stat='identity', fill='magenta') +
  coord_flip() +
  labs(title=paste("Cities with Population Greater Than", population_threshold), x="City", y="Population")

#Thankyou








