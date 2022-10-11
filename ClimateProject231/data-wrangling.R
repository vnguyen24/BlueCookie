library(tidyverse)
library(lubridate)
library(ical)
library(RColorBrewer)
library(knitr)
library(kableExtra)
library(readxl)

 # contributions 

 # justin: collected data from half of the cities, created the template for 
 # importing one city, cleaning it up, and some wrangling.
 # created comments on the r script, final wrangling of large dataset into the 
 # two datasets for our visualizations

 # cuong: collected data from half of the cities. Used the template to import 
 # the rest of the cities, pivoted the datasets of the individual cities and
 # combined them

 # our data came from {https://www.weather.gov/wrh/Climate?wfo=box/} 
 # in which we found the data for each of the cities that we used, by searching
 # for the city and selecting "Monthly summarized data", our variable of 
 # interest and our summary statistic (eg. mean). The data was presented in a 
 # pop-up window that we then saved as a pdf. We used pdf to excel file 
 # converter to create files for each variable of interest for each city. 

#BOSTON DATA FRAME
# our first file was...
# boston max temps 

# we read the data in from the excel file and cleaned the data up
bos_max_long <- read_excel("ClimateProject231/data/boston/bos.max.xlsx", range = "A7:N110") %>%
  janitor::clean_names() %>%
  # filtered out the items in the year variable that did not start appropriately
  filter(grepl("18|19|20", year)) %>%
  # changed the variables to numeric values
  mutate(across(year:annual, ~ as.numeric(.)), var = "maximum") %>%
  # created a longer dataset to reorganize the data
  pivot_longer(cols = 2:14, names_to = "month", values_to = "max_temp") %>%
  select(!var)

# do the same process for the next boston file
bos_min_long <- read_excel("ClimateProject231/data/boston/bos.min.xlsx", range = "A7:N110") %>%
  janitor::clean_names() %>%
  filter(grepl("18|19|20", year)) %>%
  mutate(across(year:annual, ~ as.numeric(.)), var = "minimum") %>%
  pivot_longer(cols = 2:14, names_to = "month", values_to = "min_temp") %>%
  select(!var)

# and the same for the file
bos_avg_long <- read_excel("ClimateProject231/data/boston/bos.avgtemp.xlsx", 
                           range = "A7:N110") %>%
  janitor::clean_names() %>%
  filter(grepl("18|19|20", year)) %>%
  mutate(across(year:annual, ~ as.numeric(.)), var = "average") %>%
  pivot_longer(cols = 2:14, names_to = "month", values_to = "avg_temp") %>%
  select(!var)

# as well as this file
bos_pre_long <- read_excel("ClimateProject231/data/boston/bos.pre.xlsx", range = "A7:N110") %>%
  janitor::clean_names() %>%
  filter(grepl("18|19|20", year)) %>%
  mutate(across(year:annual, ~ as.numeric(.)), var = "precipitation") %>%
  pivot_longer(cols = 2:14, names_to = "month", values_to = "precipitation") %>%
  select(!var)

# join the separate data sets by year and month to create a dataset with all
# of the variables of interest
boston <- left_join(bos_min_long, bos_max_long, by = c("year", "month"))
boston <- left_join(boston, bos_avg_long, by = c("year", "month"))
boston <- left_join(boston, bos_pre_long, by = c("year", "month")) %>%
  mutate(city = "boston")

# we did the same process for the remaining 9 cities below

#SEATTLE DATAFRAME
set_max_long <- read_excel("ClimateProject231/data/seattle/set.max.xlsx", range = "A7:N163") %>%
  janitor::clean_names() %>%
  filter(grepl("18|19|20", year)) %>%
  mutate(across(year:annual, ~ as.numeric(.)), var = "maximum") %>%
  pivot_longer(cols = 2:14, names_to = "month", values_to = "max_temp") %>%
  select(!var)

set_min_long <- read_excel("ClimateProject231/data/seattle/set.min.xlsx", range = "A7:N163") %>%
  janitor::clean_names() %>%
  filter(grepl("18|19|20", year)) %>%
  mutate(across(year:annual, ~ as.numeric(.)), var = "minimum") %>%
  pivot_longer(cols = 2:14, names_to = "month", values_to = "min_temp") %>%
  select(!var)

set_avg_long <- read_excel("ClimateProject231/data/seattle/set.avg.xlsx", range = "A7:N163") %>%
  janitor::clean_names() %>%
  filter(grepl("18|19|20", year)) %>%
  mutate(across(year:annual, ~ as.numeric(.)), var = "average") %>%
  pivot_longer(cols = 2:14, names_to = "month", values_to = "avg_temp") %>%
  select(!var)

set_pre_long <- read_excel("ClimateProject231/data/seattle/set.pre.xlsx", range = "A7:N163") %>%
  janitor::clean_names() %>%
  filter(grepl("18|19|20", year)) %>%
  mutate(across(year:annual, ~ as.numeric(.)), var = "precipitation") %>%
  pivot_longer(cols = 2:14, names_to = "month", values_to = "precipitation") %>%
  select(!var)

seattle <- left_join(set_min_long, set_max_long, by = c("year", "month")) 
seattle <- left_join(seattle, set_avg_long, by = c("year", "month")) 
seattle <- left_join(seattle, set_pre_long, by = c("year", "month")) %>%
  mutate(city = "seattle")

#NYC DATAFRAME
nyc_max_long <- read_excel("ClimateProject231/data/nyc/nyc.max.xlsx", range = "A7:N188") %>%
  janitor::clean_names() %>%
  filter(grepl("18|19|20", year)) %>%
  mutate(across(year:annual, ~ as.numeric(.)), var = "maximum") %>%
  pivot_longer(cols = 2:14, names_to = "month", values_to = "max_temp") %>%
  select(!var)

nyc_min_long <- read_excel("ClimateProject231/data/nyc/nyc.min.xlsx", range = "A7:N188") %>%
  janitor::clean_names() %>%
  filter(grepl("18|19|20", year)) %>%
  mutate(across(year:annual, ~ as.numeric(.)), var = "minimum") %>%
  pivot_longer(cols = 2:14, names_to = "month", values_to = "min_temp") %>%
  select(!var)

nyc_avg_long <- read_excel("ClimateProject231/data/nyc/nyc.avgtemp.xlsx", range = "A7:N188") %>%
  janitor::clean_names() %>%
  filter(grepl("18|19|20", year)) %>%
  mutate(across(year:annual, ~ as.numeric(.)), var = "average") %>%
  pivot_longer(cols = 2:14, names_to = "month", values_to = "avg_temp") %>%
  select(!var)

nyc_pre_long <- read_excel("ClimateProject231/data/nyc/nyc.pre.xlsx", range = "A7:N188") %>%
  janitor::clean_names() %>%
  filter(grepl("18|19|20", year)) %>%
  mutate(across(year:annual, ~ as.numeric(.)), var = "precipitation") %>%
  pivot_longer(cols = 2:14, names_to = "month", values_to = "precipitation") %>%
  select(!var)

nyc <- left_join(nyc_min_long, nyc_max_long, by = c("year", "month")) 
nyc <- left_join(nyc, nyc_avg_long, by = c("year", "month")) 
nyc <- left_join(nyc, nyc_pre_long, by = c("year", "month")) %>%
  mutate(city = "nyc")


#AMHERST DATA FRAME
am_max_long <- read_excel("ClimateProject231/data/amherst/amherst.max.xlsx", 
                          range = "A7:N164") %>%
  janitor::clean_names() %>%
  filter(grepl("18|19|20", year)) %>%
  mutate(across(year:annual, ~ as.numeric(.)), var = "maximum") %>%
  pivot_longer(cols = 2:14, names_to = "month", values_to = "max_temp") %>%
  select(!var)

am_min_long <- read_excel("ClimateProject231/data/amherst/amherst.min.xlsx", 
                          range = "A7:N164") %>%
  janitor::clean_names() %>%
  filter(grepl("18|19|20", year)) %>%
  mutate(across(year:annual, ~ as.numeric(.)), var = "minimum") %>%
  pivot_longer(cols = 2:14, names_to = "month", values_to = "min_temp") %>%
  select(!var)

am_avg_long <- read_excel("ClimateProject231/data/amherst/amherst.avgtemp.xlsx", 
                          range = "A7:N164") %>%
  janitor::clean_names() %>%
  filter(grepl("18|19|20", year)) %>%
  mutate(across(year:annual, ~ as.numeric(.)), var = "average") %>%
  pivot_longer(cols = 2:14, names_to = "month", values_to = "avg_temp") %>%
  select(!var)

am_pre_long <- read_excel("ClimateProject231/data/amherst/amherst.pre.xlsx", 
                          range = "A7:N164") %>%
  janitor::clean_names() %>%
  filter(grepl("18|19|20", year)) %>%
  mutate(across(year:annual, ~ as.numeric(.)), var = "precipitation") %>%
  pivot_longer(cols = 2:14, names_to = "month", values_to = "precipitation") %>%
  select(!var)

amherst <- left_join(am_max_long, am_min_long, by = c("year", "month"))
amherst <- left_join(amherst, am_avg_long, by = c("year", "month"))
amherst <- left_join(amherst, am_pre_long, by = c("year", "month")) %>%
  mutate(city = "amherst")


#ANCHORAGE DATA FRAME

anc_max_long <- read_excel("ClimateProject231/data/anchorage/anchorage_max.xlsx", 
                           range = "A7:N93") %>%
  janitor::clean_names() %>%
  filter(grepl("18|19|20", year)) %>%
  mutate(across(year:annual, ~ as.numeric(.)), var = "maximum") %>%
  pivot_longer(cols = 2:14, names_to = "month", values_to = "max_temp") %>%
  select(!var)

anc_min_long <- read_excel("ClimateProject231/data/anchorage/anchorage_min.xlsx", 
                           range = "A7:N93") %>%
  janitor::clean_names() %>%
  filter(grepl("18|19|20", year)) %>%
  mutate(across(year:annual, ~ as.numeric(.)), var = "minimum") %>%
  pivot_longer(cols = 2:14, names_to = "month", values_to = "min_temp") %>%
  select(!var)

anc_avg_long <- read_excel("ClimateProject231/data/anchorage/anchorage_avg.xlsx", 
                           range = "A7:N93") %>%
  janitor::clean_names() %>%
  filter(grepl("18|19|20", year)) %>%
  mutate(across(year:annual, ~ as.numeric(.)), var = "average") %>%
  pivot_longer(cols = 2:14, names_to = "month", values_to = "avg_temp") %>%
  select(!var)

anc_pre_long <- read_excel("ClimateProject231/data/anchorage/anchorage_pre.xlsx", 
                           range = "A7:N93") %>%
  janitor::clean_names() %>%
  filter(grepl("18|19|20", year)) %>%
  mutate(across(year:annual, ~ as.numeric(.)), var = "precipitation") %>%
  pivot_longer(cols = 2:14, names_to = "month", values_to = "precipitation") %>%
  select(!var)

anchorage <- left_join(anc_max_long, anc_min_long, by = c("year", "month"))
anchorage <- left_join(anchorage, anc_avg_long, by = c("year", "month"))
anchorage <- left_join(anchorage, anc_pre_long, by = c("year", "month")) %>%
  mutate(city = "anchorage")

#CHICAGO DATA FRAME
chi_max_long <- read_excel("ClimateProject231/data/chicago/chicago_max.xlsx", 
                           range = "A7:N185") %>%
  janitor::clean_names() %>%
  filter(grepl("18|19|20", year)) %>%
  mutate(across(year:annual, ~ as.numeric(.)), var = "maximum") %>%
  pivot_longer(cols = 2:14, names_to = "month", values_to = "max_temp") %>%
  select(!var)

chi_min_long <- read_excel("ClimateProject231/data/chicago/chicago_min.xlsx", 
                           range = "A8:N186") %>%
  janitor::clean_names() %>%
  filter(grepl("18|19|20", year)) %>%
  mutate(across(year:annual, ~ as.numeric(.)), var = "minimum") %>%
  pivot_longer(cols = 2:14, names_to = "month", values_to = "min_temp") %>%
  select(!var)

chi_avg_long <- read_excel("ClimateProject231/data/chicago/chicago_avg.xlsx", 
                           range = "A7:N185") %>%
  janitor::clean_names() %>%
  filter(grepl("18|19|20", year)) %>%
  mutate(across(year:annual, ~ as.numeric(.)), var = "average") %>%
  pivot_longer(cols = 2:14, names_to = "month", values_to = "avg_temp") %>%
  select(!var)

chi_pre_long <- read_excel("ClimateProject231/data/chicago/chicago_pre.xlsx", range = "A7:N186") %>%
  janitor::clean_names() %>%
  filter(grepl("18|19|20", year)) %>%
  mutate(across(year:annual, ~ as.numeric(.)), var = "precipitation") %>%
  pivot_longer(cols = 2:14, names_to = "month", values_to = "precipitation") %>%
  select(!var)

chicago <- left_join(chi_pre_long, chi_min_long, by = c("year", "month"))
chicago <- left_join(chicago, chi_avg_long, by = c("year", "month"))
chicago <- left_join(chicago, chi_max_long, by = c("year", "month")) %>%
  mutate(city = "chicago")

#DALLAS DATA FRAME
dal_max_long <- read_excel("ClimateProject231/data/dallas/dallas_max.xlsx", range = "A7:N144") %>%
  janitor::clean_names() %>%
  filter(grepl("18|19|20", year)) %>%
  mutate(across(year:annual, ~ as.numeric(.)), var = "maximum") %>%
  pivot_longer(cols = 2:14, names_to = "month", values_to = "max_temp") %>%
  select(!var)

dal_min_long <- read_excel("ClimateProject231/data/dallas/dallas_min.xlsx", range = "A7:N144") %>%
  janitor::clean_names() %>%
  filter(grepl("18|19|20", year)) %>%
  mutate(across(year:annual, ~ as.numeric(.)), var = "minimum") %>%
  pivot_longer(cols = 2:14, names_to = "month", values_to = "min_temp") %>%
  select(!var)

dal_avg_long <- read_excel("ClimateProject231/data/dallas/dallas_avg.xlsx", range = "A7:N144") %>%
  janitor::clean_names() %>%
  filter(grepl("18|19|20", year)) %>%
  mutate(across(year:annual, ~ as.numeric(.)), var = "average") %>%
  pivot_longer(cols = 2:14, names_to = "month", values_to = "avg_temp") %>%
  select(!var)

dal_pre_long <- read_excel("ClimateProject231/data/dallas/dallas_pre.xlsx", range = "A7:N144") %>%
  janitor::clean_names() %>%
  filter(grepl("18|19|20", year)) %>%
  mutate(across(year:annual, ~ as.numeric(.)), var = "precipitation") %>%
  pivot_longer(cols = 2:14, names_to = "month", values_to = "precipitation") %>%
  select(!var)

dallas <- left_join(dal_max_long, dal_min_long, by = c("year", "month"))
dallas <- left_join(dallas, dal_avg_long, by = c("year", "month"))
dallas <- left_join(dallas, dal_pre_long, by = c("year", "month")) %>%
  mutate(city = "dallas")

#HONOLULU DATA FRAME
hol_max_long <- read_excel("ClimateProject231/data/honolulu/honolulu_max.xlsx", 
                           range = "A7:N106") %>%
  janitor::clean_names() %>%
  filter(grepl("18|19|20", year)) %>%
  mutate(across(year:annual, ~ as.numeric(.)), var = "maximum") %>%
  pivot_longer(cols = 2:14, names_to = "month", values_to = "max_temp") %>%
  select(!var)

hol_min_long <- read_excel("ClimateProject231/data/honolulu/honolulu_min.xlsx", 
                           range = "A7:N106") %>%
  janitor::clean_names() %>%
  filter(grepl("18|19|20", year)) %>%
  mutate(across(year:annual, ~ as.numeric(.)), var = "minimum") %>%
  pivot_longer(cols = 2:14, names_to = "month", values_to = "min_temp") %>%
  select(!var)

hol_avg_long <- read_excel("ClimateProject231/data/honolulu/honolulu_avg.xlsx", 
                           range = "A7:N106") %>%
  janitor::clean_names() %>%
  filter(grepl("18|19|20", year)) %>%
  mutate(across(year:annual, ~ as.numeric(.)), var = "average") %>%
  pivot_longer(cols = 2:14, names_to = "month", values_to = "avg_temp") %>%
  select(!var)

hol_pre_long <- read_excel("ClimateProject231/data/honolulu/honolulu_pre.xlsx", 
                           range = "A7:N106") %>%
  janitor::clean_names() %>%
  filter(grepl("18|19|20", year)) %>%
  mutate(across(year:annual, ~ as.numeric(.)), var = "precipitation") %>%
  pivot_longer(cols = 2:14, names_to = "month", values_to = "precipitation") %>%
  select(!var)

honolulu <- left_join(hol_max_long, hol_min_long, by = c("year", "month"))
honolulu <- left_join(honolulu, hol_avg_long, by = c("year", "month"))
honolulu <- left_join(honolulu, hol_pre_long, by = c("year", "month")) %>%
  mutate(city = "honolulu")

#LA DATA FRAME
la_max_long <- read_excel("ClimateProject231/data/la/la.max.xlsx", range = "A7:N180") %>%
  janitor::clean_names() %>%
  filter(grepl("18|19|20", year)) %>%
  mutate(across(year:annual, ~ as.numeric(.)), var = "maximum") %>%
  pivot_longer(cols = 2:14, names_to = "month", values_to = "max_temp") %>%
  select(!var)

la_min_long <- read_excel("ClimateProject231/data/la/la.min.xlsx", range = "A7:N180") %>%
  janitor::clean_names() %>%
  filter(grepl("18|19|20", year)) %>%
  mutate(across(year:annual, ~ as.numeric(.)), var = "minimum") %>%
  pivot_longer(cols = 2:14, names_to = "month", values_to = "min_temp") %>%
  select(!var)

la_avg_long <- read_excel("ClimateProject231/data/la/la.avgtemp.xlsx", range = "A7:N180") %>%
  janitor::clean_names() %>%
  filter(grepl("18|19|20", year)) %>%
  mutate(across(year:annual, ~ as.numeric(.)), var = "average") %>%
  pivot_longer(cols = 2:14, names_to = "month", values_to = "avg_temp") %>%
  select(!var)

la_pre_long <- read_excel("ClimateProject231/data/la/la.pre.xlsx", range = "A7:N180") %>%
  janitor::clean_names() %>%
  filter(grepl("18|19|20", year)) %>%
  mutate(across(year:annual, ~ as.numeric(.)), var = "precipitation") %>%
  pivot_longer(cols = 2:14, names_to = "month", 
               values_to = "precipitation") %>%
  select(!var)

la <- left_join(la_max_long, la_min_long, by = c("year", "month"))
la <- left_join(la, la_avg_long, by = c("year", "month"))
la <- left_join(la, la_pre_long, by = c("year", "month")) %>%
  mutate(city = "los angeles")

#MIAMI DATA FRAME
miami_max_long <- read_excel("ClimateProject231/data/miami/miami_max.xlsx", range = "A7:N162") %>%
  janitor::clean_names() %>%
  filter(grepl("18|19|20", year)) %>%
  mutate(across(year:annual, ~ as.numeric(.)), var = "maximum") %>%
  pivot_longer(cols = 2:14, names_to = "month", values_to = "max_temp") %>%
  select(!var)

miami_min_long <- read_excel("ClimateProject231/data/miami/miami_min.xlsx", range = "A7:N162") %>%
  janitor::clean_names() %>%
  filter(grepl("18|19|20", year)) %>%
  mutate(across(year:annual, ~ as.numeric(.)), var = "minimum") %>%
  pivot_longer(cols = 2:14, names_to = "month", values_to = "min_temp") %>%
  select(!var)

miami_avg_long <- read_excel("ClimateProject231/data/miami/miami_avg.xlsx", range = "A7:N162") %>%
  janitor::clean_names() %>%
  filter(grepl("18|19|20", year)) %>%
  mutate(across(year:annual, ~ as.numeric(.)), var = "average") %>%
  pivot_longer(cols = 2:14, names_to = "month", values_to = "avg_temp") %>%
  select(!var)

miami_pre_long <- read_excel("ClimateProject231/data/miami/miami_pre.xlsx", range = "A7:N162") %>%
  janitor::clean_names() %>%
  filter(grepl("18|19|20", year)) %>%
  mutate(across(year:annual, ~ as.numeric(.)), var = "precipitation") %>%
  pivot_longer(cols = 2:14, names_to = "month", values_to = "precipitation") %>%
  select(!var)

miami <- left_join(miami_max_long, miami_min_long, by = c("year", "month"))
miami <- left_join(miami, miami_avg_long, by = c("year", "month"))
miami <- left_join(miami, miami_pre_long, by = c("year", "month")) %>%
  mutate(city = "miami")

# then we created the dataframe of all of the cities combined by binding rows
data <- rbind(boston, seattle, nyc, anchorage, amherst, 
              chicago, dallas, honolulu, la, miami) 

# this will be used for the first visual
# removed the annual variable and changed the dates into a ymd format
first_visual <- data %>%
  filter(month != "annual") %>%
  mutate(month_num = case_when(month == "jan" ~ 1,
                               month == "feb" ~ 2,
                               month == "mar" ~ 3,
                               month == "apr" ~ 4,
                               month == "may" ~ 5,
                               month == "jun" ~ 6,
                               month == "jul" ~ 7,
                               month == "aug" ~ 8,
                               month == "sep" ~ 9,
                               month == "oct" ~ 10,
                               month == "nov" ~ 11,
                               month == "dec" ~ 12
  )) %>%
  mutate(dates = paste(year, month_num, sep = "/")) %>%
  mutate(date = ym(dates)) %>%
  select(date, month_num, avg_temp, city) %>%
  drop_na()

# this will be used for the second visual
second_visual <- data %>%
  filter(month == "annual") %>%
  select(year, min_temp, max_temp, avg_temp, precipitation, city) %>%
  drop_na()

# our final datasets are first_visual and second_visual
write_csv(first_visual, "ClimateProject231/data/first_visual.csv")
write_csv(second_visual, "ClimateProject231/data/second_visual.csv")
  





