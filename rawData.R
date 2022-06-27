### Michael Dahl - Assignment #2
### Step 1: Data Retrieval
### For this Assignment, I am using the lab work I completed after class to demonstrate my ability to code - I ran into some major issues with my dataset of choice, and I need to spend some time reading the 90 page codebook
### Housing Data - 
### Eviction Data - 
library(janitor)
library(tidyverse)

#import data
NYChousing_data <- read_csv("Housing_New_York_Units_by_Building.csv")

# cleaning up the column names
NYChousing_clean <- clean_names(NYChousing_data) %>% 
  mutate(as.numeric("BBL"))

#light analysis
NYCTopUnits <- NYChousing_data %>%
  clean_names() %>%
  select(project_name,borough, studio_units, x1_br_units) %>%
  filter(borough == "Brooklyn", studio_units >0, x1_br_units >0) %>%
  mutate(total = studio_units + x1_br_units) %>%
  arrange(desc(total)) 

#Using group_by and summarize functions, find the community board with the largest number of Very Low Income Units
NYCCommBoardVLIU <- NYChousing_clean %>%
  select(community_board, very_low_income_units) %>%
  group_by(community_board) %>%
  summarize(very_low_income_units = n()) 

#Find the average number of Counted Rental Units by borough
NYCCRU <- NYChousing_clean %>%
  select(borough, counted_rental_units) %>%
  group_by(borough) %>%
  summarize(avgnum = mean(counted_rental_units, na.rm=TRUE))

#Find which Housing New York buildings faced an Eviction (hint: join tables together using the “BBL” column) 
EvictionData <- read_csv("Evictions.csv")
EvictionDataClean <- clean_names(EvictionData) %>% mutate(as.numeric("bbl"))
###Make sure the changes to BBL were implemented
glimpse(EvictionDataClean)
glimpse(NYChousing_clean)
###Join
NYCHousingwithEviction <- 
  inner_join(NYChousing_clean, EvictionDataClean, by = "bbl")