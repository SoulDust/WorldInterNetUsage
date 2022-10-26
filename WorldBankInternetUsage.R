############################## 
# Tom Wheetley
# HarvardX CYO Capstone Project
# World bank internet usage and Poverty Rates
# 10-19-2022
##############################

# Install necessary packages if not exist

if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")


#Libraries

library(data.table)
library(ggplot2)
library(tidyverse)
library(caret)
library(dplyr)

############################
# Define the RMSE function that will be used
RMSE <- function(true_poverty = NULL, predicted_poverty = NULL) {
  sqrt(mean((true_poverty - predicted_poverty)^2))
}

############################
#Download the data

dl <- tempfile()
download.file("https://sds-platform-private.s3-us-east-2.amazonaws.com/uploads/P2-Demographic-Data.csv", dl)
demographics <- fread(dl, select = c("Country Name",	"Country Code",	"Birth rate",	"Internet users",	"Income Group"))


dl_crv <- tempfile()
download.file("https://sds-platform-private.s3-us-east-2.amazonaws.com/uploads/P2-Country-Region-Vectors.zip", dl_crv)

source(unzip(dl_crv)) 

head(Countries_2012_Dataset)
head(Regions_2012_Dataset)
head(Codes_2012_Dataset)

#Internet History
dl_ih_zip <- tempfile()
download.file("https://databank.worldbank.org/data/download/WDI_csv.zip", dl_ih_zip)
df_ih <- readLines(unzip(dl_ih_zip, "WDIData.csv"))
head(df_ih)

head(dl_ih_zip)

#Load internet usage data
#you are going to need to select the internetusage.csv here. The CSV was not available for auto download from https://ourworldindata.org/internet

internetusage <- read.csv(file.choose())

head(internetusage)

str(internetusage)

#Rename the columns
colnames(internetusage) <- c("Country", "Code", "Year", "Internet.Usage")



#Load the World bank Poverty explorer datasets the file is poverty-explorer.csv the csv The CSV was not available for auto download from https://ourworldindata.org/internet

poverty_explorer <- read.csv(file.choose())
str(poverty_explorer)
head(poverty_explorer)
df_poverty <- poverty_explorer %>% select(Below..2.15.a.day,	Entity,	Year)

head(df_poverty)
###########################
#Data Validation

#Column Check
head(demographics)

#Row Count Should = 195
nrow(demographics)

#Structure 
str(demographics)

#Summary
summary(demographics)

#Data Set load check
head(Countries_2012_Dataset)
head(Regions_2012_Dataset)
head(Codes_2012_Dataset)

###########################

#Plot shows the relation ship between declining birth rates and higher income countries. 
#The higher incomees show a greater inter net usage show an increase in 
qplot(data=demographics, x=`Internet users`, y=`Birth rate`,
      size=I(3), color=`Income Group`)



##########################
#Put vectors into Data Frame
CountriesRegionsDF <- data.frame(Countries_2012_Dataset, Codes_2012_Dataset, Regions_2012_Dataset)

#Change the column names
colnames(CountriesRegionsDF) <- c("Country", "Code", "Region")

#########################
#Merge the data frame

mergeddf <- merge(demographics, CountriesRegionsDF, by.x = "Country Code", by.y = "Code")

head(mergeddf)

#Remove redundant column
mergeddf$`Country Name` <- NULL


#merge Internet usage with regions
mergediuregionsdf <- merge(internetusage, CountriesRegionsDF, by.x = "Code", by.y = "Code")

#Remove redundant column
mergediuregionsdf$Country.y <-NULL
head(mergediuregionsdf)

#Add in percentage of people below the poverty line $2.15 a day

df_final = merge(mergediuregionsdf, df_poverty, by.x=c("Country.x", "Year"), by.y=c("Entity", "Year"))
head(df_final)


########################
#Visualize the New data Frame

options("scipen"=100, "digits"=4)


#This plot shows higher Birth Rates vs Internet usage by Continent. (Africa shows the slowest adoption)
qplot(data=mergeddf, x=`Internet users`, y=`Birth rate`,
      size=I(3), color=Region, shape=I(17), alpha =I(0.4), main="Birth Rate vs Internet Usage")

df_intu <- mergediuregionsdf %>%
  select(Internet.Usage, Region, Year) %>%
  group_by (Region, Year) %>%
  summarize(Mean_Internet_Usage = mean(Internet.Usage, na.rm = TRUE))

print(df_intu, n=200)

str(df_intu)

#Plot shows Average Internet usage by Continent
ggplot(data=df_intu, aes(x=Year, y=Mean_Internet_Usage, group=Region, color=Region)) +
  geom_line()

str(df_ih)


df_final_plot <- df_final %>%
  select(Below..2.15.a.day, Region, Year) %>%
  group_by (Region, Year) %>%
  summarize(Mean_Poverty_By_Region = mean(Below..2.15.a.day, na.rm = TRUE))

df_final_plot <- df_final_plot %>% 
  group_by(Region, Year) %>% 
  mutate(Mean_Poverty_By_Region = replace_na(Mean_Poverty_By_Region, min(Mean_Poverty_By_Region, na.rm = TRUE)))

any(is.na(df_final_plot))


#Plot FOUR shows mean poverty by area for the last thirty years all droping  
ggplot(data=df_final_plot, aes(x=Year, y=Mean_Poverty_By_Region, group=Region, color=Region)) +
  geom_smooth(se = FALSE,
              method = "loess",
              formula = y ~ x)


#PLOT Poverty By region for the same time period as internet adoption
ggplot() +
  geom_smooth(data=df_final_plot, aes(x=Year, y=Mean_Poverty_By_Region, group=Region, color=Region) , method = "loess", formula = y ~ x)

# Validation set will be 10% of poverty_explorer dataset
set.seed(1)
test_index <- createDataPartition(y = poverty_explorer$Below..2.15.a.day, times = 1, p = 0.1, list = FALSE)
pov_exp <- poverty_explorer[-test_index,]
temp <- poverty_explorer[test_index,]

validation <- temp

# Mean-Baseline Model#########################
#World bank calculates the poverty line at $2.15 per person per day 
# Calculate the mean of pov_exp dataset and assign to mu_hat

mu_hat <- mean(pov_exp$Below..2.15.a.day)

# Predict the RMSE on the validation dataset

rmse_mean_model_result <- RMSE(validation$Below..2.15.a.day, mu_hat)

# Creating a results dataframe that contains all RMSE results and add the result to it

results <- data.frame(model="Mean-Baseline Model", RMSE=rmse_mean_model_result)
# results

print(results)



##############################
#Poverty Based Model
str(df_final)
str(df_poverty)
str(pov_exp)

valid <- validation %>% select(Below..2.15.a.day, Entity, Year)
colnames(valid) <- c("pct_below", "country", "Year")

str(valid)
#Rename the columns
colnames(df_poverty) <- c("pct_below", "country", "Year")
colnames(df_final) <- c("country", "Year", "Code", "Internet.Usage", "Region", "pct_below")

# Calculate the average poverty by country

pov_avgs <- df_final %>%
  left_join(df_poverty, by='country') %>%
  group_by(Region) %>%
  summarize(b_u = mean( df_poverty$pct_below - mu_hat))

# Calculate the predicted poverty on validation dataset

rmse_poverty_based_model <- mean(df_final$pct_below * (df_final$Internet.Usage/100)) - mu_hat
                                    
rmse_poverty_based_model_result <- RMSE(valid$pct_below, rmse_poverty_based_model)

# Adding the results to the results dataset

results <- results %>% add_row(model="Poverty Based Model", RMSE=rmse_poverty_based_model_result)
print(results)


