### Tidy Data ###

# Load relevant libraries
library(tidyverse)
library(tidylog)
library(vtable)
library(jtools)
library(car)
library(purrr)
library(lubridate)

# data path
data_path <- "../data/"

# read in google trend files to a single data frame
files <- dir(data_path, pattern = "trends_up_to*")
raw_sch_df<- files %>%
  map(~ read.csv(file.path(data_path, .))) %>%
  reduce(rbind)

# remove incomplete rows
sch_df <- raw_sch_df[complete.cases(raw_sch_df), ]

# read college scoreboard files into data frames
id_name_link <- read.csv(file = "../data/id_name_link.csv")
scorecard <- read.csv(file = "../data/Most+Recent+Cohorts+(Scorecard+Elements).csv")
dictionary_scorecard <- read.csv(file = "../data/CollegeScorecardDataDictionary-09-08-2015.csv")

# Check if unitid is key
# check for duplicates
check_dupes <- function(data, vars) {
  data %>%
    select(vars) %>%
    duplicated() %>%
    max()
}

check_dupes(id_name_link, 'unitid')
check_dupes(id_name_link, 'schname')

# generate list of duplicates in id_name_link
id_dupes <- id_name_link[duplicated(id_name_link$schname ),]

# remove duplicates in id_name_link
id_name_link <- anti_join(id_name_link, id_dupes, by = 'schname')
check_dupes(id_name_link, 'schname')
check_dupes(id_name_link, 'unitid')

# transform scorecard data to new data frame with selected and renamed rows
# filters for schools where the primary awarded degree is a 4-year
scoredata <- scorecard %>%
  filter(PREDDEG==3) %>%
  select(UNITID, INSTNM, md_earn_wne_p10.REPORTED.EARNINGS, PREDDEG) %>%
  rename(unitid = UNITID, yr10_earnings = md_earn_wne_p10.REPORTED.EARNINGS)

# convert earnings column to numeric, remove NAs and remove duplicates
scoredata$yr10_earnings <- as.numeric(scoredata$yr10_earnings)
scoredata <- scoredata[complete.cases(scoredata), ]
check_dupes(scoredata, 'INSTNM')
scoredupes <- scoredata[duplicated(scoredata$INSTNM),]
scoredata <- anti_join(scoredata, scoredupes, by = 'INSTNM')
check_dupes(scoredata, 'INSTNM')

# merge id data frame to trend dataframe, keeping only schools with trend data
id_to_trends <- merge(sch_df, id_name_link, by = "schname", all.x = TRUE, all.y = FALSE) %>%
  select(unitid, schname, keyword, monthorweek, index)

# standardize keyword index for each institution for later comparison
id_to_trends <- id_to_trends %>% group_by(unitid, keyword) %>%
  mutate(index = (index - mean(index)) / sd(index)) 

# break down the date variable by taking the end date of each weekly data collection,
id_to_trends <- id_to_trends %>% separate(monthorweek, c(NA, "date"), " - " ) %>%
  mutate(date = as.Date(date))
id_to_trends$date <- format(as_date(id_to_trends$date), "%Y-%m-01")
id_to_trends$date <- as_date(id_to_trends$date)

# create a monthly index score for the school
id_to_trends <- id_to_trends %>% group_by(schname, date) %>% 
  mutate(mo_index = mean(index))

# create college scorecard existance dummy variable for pre and post september 2015 for
# index analysis
id_to_trends$post_report <- if_else(id_to_trends$date >= '2015-09-01', 1, 0)
id_to_trends <- id_to_trends %>% mutate(post_report = factor(post_report))

# removes unused variables in id_to_trends and remove duplicate months
# resulting df has a standardized index for each month for each school
id_to_trends <- id_to_trends %>% select(unitid, schname, date, index, mo_index, post_report)
id_to_trends <- unique(id_to_trends)

# merge scorecard data to the trend data based on unitid
# Keeping only the scorecard data we have trend data for and removing NAs
analysis_df <- merge(id_to_trends, scoredata, by = "unitid", all.x = TRUE, all.y = FALSE) %>%
  select(unitid, INSTNM, date, index, mo_index, post_report, yr10_earnings)
analysis_df <- analysis_df[complete.cases(analysis_df), ]

# create dummy variable. 0 for low earning schools and 1 for high earning schools
# earning status is based on the descriptive statistics of earnings for all schools
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 11800   35100   40700   42159   47800  166200 
# low earning is based on schools falling bellow the median reported earnings
desc_stats <- summary(scoredata$yr10_earnings)

analysis_df$earning_status <- ifelse(analysis_df$yr10_earnings >=40700, 1, 0)
analysis_df <- analysis_df %>% mutate(earning_status = factor(earning_status))