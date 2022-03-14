#install libraries
library(dplyr)
library(data.table)
library(tidyverse)

#read in data
cb_data = fread(file = 'Big_Dance_CSV.csv')

#rename duplicate column names
names(cb_data)[5] <- 'Seed1'
names(cb_data)[6] <- 'Score1'
names(cb_data)[7] <- 'Team1'
names(cb_data)[8] <- 'Team2'
names(cb_data)[9] <- 'Score2'
names(cb_data)[10] <- 'Seed2'

#filter data for Round 1 only
cb_Round1 = cb_data %>% filter(Round == 1)
write.csv(cb_Round1, 'Data/cb_Round1.csv')

cb_Round1_updated = cb_Round1 %>% filter(Seed1 != 8)

#filter for upsets by 10 seeds or lower
cb_upsets = cb_Round1 %>% filter(Score2 > Score1, Seed2 >= 10) %>%
  select(Year, Round, Seed1:Seed2)

#add in the data for upsets occuring after 2019 (the final year in this dataset)
newUpsets = data.frame(c(2021, 2021, 2021, 2021, 2021, 2021, 2021, 2021, 2021),
                       c(1, 1, 1, 1, 1, 1, 1, 1, 1),
                       c(2, 3, 4, 4, 5, 6, 6, 7, 7),
                       c(72, 52, 58, 69, 56, 62, 62, 54, 56),
                       c('Ohio State', 'Texas', 'Virginia', 'Purdue', 'Tennessee', 'BYU', 'San Diego St',
                         'Connecticut', 'Clemson'),
                       c('Oral Roberts', 'Abilene Christian', 'Ohio', 'North Texas', 'Oregon St', 'UCLA',
                         'Syracuse', 'Maryland', 'Rutgers'),
                       c(75, 53, 62, 78, 70, 73, 78, 63, 60),
                       c(15, 14, 13, 13, 12, 11, 11, 10, 10))
names(newUpsets) = c('Year', 'Round', 'Seed1', 'Score1', 'Team1', 'Team2', 'Score2', 'Seed2')

cb_upsets = rbind(cb_upsets, newUpsets)
write.csv(cb_upsets, 'Data/cb_upsets.csv')

cb_upsets = fread('cb_upsets.csv')

#set working directory for file list creation to read in stats datasets
setwd('/Users/AydanKoyles/Desktop/Research/March Madness/March Madness Project')
files = list.files(path = 'Data')
#create empty dataframe to house stats datasets and change working directory
cb_stats = data.frame()
setwd('/Users/AydanKoyles/Desktop/Research/March Madness/March Madness Project/Data')

#use a for loop and the fread function to read in stats datasets for each season
#use.names uses the column names from the files
for (i in 1:length(files)) {
  tmp = fread(files[i], stringsAsFactors = F)
  cb_stats = rbindlist(list(cb_stats, tmp), use.names = T)
}

#change the names of teams in the stats datasets to be consistent with the upsets dataset for easier joining
cb_stats$School[cb_stats$School == 'Louisiana State'] <- 'LSU'
cb_stats$School[cb_stats$School == 'Murray State'] <- 'Murray St'
cb_stats$School[cb_stats$School == 'Florida State'] <- 'Florida St'
cb_stats$School[cb_stats$School == 'Kansas State'] <- 'Kansas St'
cb_stats$School[cb_stats$School == 'Oregon State'] <- 'Oregon St'
cb_stats$School[cb_stats$School == 'New Mexico State'] <- 'New Mexico St'
cb_stats$School[cb_stats$School == 'Mississippi State'] <- 'Mississippi St'
cb_stats$School[cb_stats$School == "St. John's (NY)"] <- 'St Johns'
cb_stats$School[cb_stats$School == 'Saint Louis'] <- 'St Louis'
cb_stats$School[cb_stats$School == 'Michigan State'] <- 'Michigan St'
cb_stats$School[cb_stats$School == 'Penn State'] <- 'Penn St'
cb_stats$School[cb_stats$School == 'Mississippi'] <- 'Ole Miss'
cb_stats$School[cb_stats$School == 'Ohio State'] <- 'Ohio St'
cb_stats$School[cb_stats$School == 'Iowa State'] <- 'Iowa St'
cb_stats$School[cb_stats$School == 'Southern California'] <- 'USC'
cb_stats$School[cb_stats$School == 'Oklahoma State'] <- 'Oklahoma St'
cb_stats$School[cb_stats$School == "Saint Joseph's"] <- 'St Josephs'
cb_stats$School[cb_stats$School == 'Wichita State'] <- 'Wichita St'
cb_stats$School[cb_stats$School == 'Nevada-Las Vegas'] <- 'UNLV'
cb_stats$School[cb_stats$School == 'San Diego State'] <- 'San Diego St'
cb_stats$School[cb_stats$School == "Saint Mary's (CA)"] <- 'St Marys'
cb_stats$School[cb_stats$School == 'Virginia Commonwealth'] <- 'VCU'
cb_stats$School[cb_stats$School == 'Southern Methodist'] <- 'SMU'
cb_upsets$Team1[cb_upsets$Team1 == 'Ohio State'] <- 'Ohio St'
cb_stats$School[cb_stats$School == 'Brigham Young'] <- 'BYU'
cb_stats$School[cb_stats$School == 'Miami (FL)'] <- 'Miami'
cb_stats$School[cb_stats$School == 'Cleveland State'] <- 'Cleveland St'
cb_stats$School[cb_stats$School == 'Little Rock'] <- 'Arkansas Little Rock'
cb_stats = cb_stats %>% mutate(School = case_when(
  School == 'Missouri State' & Year < 2005 ~ 'Southwest Missouri St',
  TRUE ~ School
))
cb_stats$School[cb_stats$School == 'Middle Tennessee'] <- 'Middle Tennessee St'
cb_stats$School[cb_stats$School == 'Colorado State'] <- 'Colorado St'
cb_stats$School[cb_stats$School == 'Ball State'] <- 'Ball St'
cb_stats$School[cb_stats$School == 'East Tennessee State'] <- 'East Tennessee St'
cb_stats$School[cb_stats$School == 'Louisiana'] <- 'Louisiana Lafayette'
cb_stats$School[cb_stats$School == 'Green Bay'] <- 'Wisconsin Green Bay'
cb_stats$School[cb_stats$School == 'Miami (OH)'] <- 'Miami Ohio'
cb_stats$School[cb_stats$School == 'Weber State'] <- 'Weber St'
cb_stats$School[cb_stats$School == 'Coppin State'] <- 'Coppin St'
cb_stats$School[cb_stats$School == 'Detroit Mercy'] <- 'Detroit'
cb_stats$School[cb_stats$School == 'Indiana State'] <- 'Indiana St'
cb_stats$School[cb_stats$School == 'Utah State'] <- 'Utah St'
cb_stats$School[cb_stats$School == 'Kent State'] <- 'Kent St'
cb_stats$School[cb_stats$School == 'Georgia State'] <- 'Georgia St'
cb_stats$School[cb_stats$School == 'Arizona State'] <- 'Arizona St'
cb_stats$School[cb_stats$School == 'Milwaukee'] <- 'Wisconsin Milwaukee'
cb_stats$School[cb_stats$School == 'Northwestern State'] <- 'Northwestern St'
cb_stats$School[cb_stats$School == 'Morehead State'] <- 'Morehead St'
cb_stats$School[cb_stats$School == 'Norfolk State'] <- 'Norfolk St'
cb_stats$School[cb_stats$School == 'Stephen F. Austin'] <- 'Stephen F Austin'
cb_stats$School[cb_stats$School == 'North Dakota State'] <- 'North Dakota St'
cb_stats$School[cb_stats$School == 'Maryland-Baltimore County'] <- 'UMBC'
cb_stats$School[cb_stats$School == 'Loyola (IL)'] <- 'Loyola Chicago'
cb_stats$School[cb_stats$School == 'Illinois State'] <- 'Illinois St'
cb_stats$School[cb_stats$School == "Mount St. Mary's"] <- "Mount St Marys"
cb_stats$School[cb_stats$School == 'Loyola Chicago' & cb_stats$Year == 1985] <- 'Loyola Illinois'
cb_stats$School[cb_stats$School == 'Washington State'] <- 'Washington St'
cb_stats$School[cb_stats$School == 'Montana State'] <- 'Montana St'
cb_stats$School[cb_stats$School == 'Washington State'] <- 'Washington St'
cb_stats$School[cb_stats$School == 'Louisiana-Monroe'] <- 'Louisiana Monroe'
cb_stats$School[cb_stats$School == 'Mississippi Valley State'] <- 'Mississippi Valley St'
cb_stats$School[cb_stats$School == 'Idaho State'] <- 'Idaho St'
cb_stats = cb_stats %>% mutate(School = case_when(
  School == 'North Texas' & Year < 1989 ~ 'North Texas St',
  TRUE ~ School
))
cb_stats$School[cb_stats$School == 'Boise State'] <- 'Boise St'
cb_stats$School[cb_stats$School == 'UTSA'] <- 'Texas San Antonio'
cb_stats$School[cb_stats$School == 'UC Santa Barbara'] <- 'Santa Barbara'
cb_stats$School[cb_stats$School == 'McNeese State'] <- 'McNeese St'
cb_stats$School[cb_stats$School == 'South Carolina State'] <- 'South Carolina St'
cb_stats$School[cb_stats$School == 'Saint Francis (PA)'] <- 'St Francis'
cb_stats$School[cb_stats$School == "Saint Peter's"] <- 'St Peters'
cb_stats$School[cb_stats$School == 'Tennessee State'] <- 'Tennessee St'
cb_stats$School[cb_stats$School == 'Cal State Long Beach'] <- 'Long Beach St'
cb_stats$School[cb_stats$School == 'Wright State'] <- 'Wright St'
cb_stats$School[cb_stats$School == 'Loyola (MD)'] <- 'Loyola Maryland'
cb_stats$School[cb_stats$School == 'Idaho State'] <- 'Idaho St'
cb_stats = cb_stats %>% mutate(School = case_when(
  School == 'Texas State' & Year < 2003 ~ 'Southwest Texas St',
  TRUE ~ School
))
cb_stats$School[cb_stats$School == 'Nicholls State'] <- 'Nicholls St'
cb_stats$School[cb_stats$School == 'San Jose State'] <- 'San Jose St'
cb_stats$School[cb_stats$School == 'Jackson State'] <- 'Jackson St'
cb_stats$School[cb_stats$School == 'Long Island University'] <- 'Long Island Brooklyn'
cb_stats$School[cb_stats$School == 'Arkansas State'] <- 'Arkansas St'
cb_stats$School[cb_stats$School == 'Alcorn State'] <- 'Alcorn St'
cb_stats$School[cb_stats$School == 'Southeast Missouri State'] <- 'Southeast Missouri St'
cb_stats$School[cb_stats$School == 'Central Connecticut State'] <- 'Central Connecticut St'
cb_stats$School[cb_stats$School == 'St. Bonaventure'] <- 'St Bonaventure'
cb_stats$School[cb_stats$School == 'Appalachian State'] <- 'Appalachian St'
cb_stats$School[cb_stats$School == 'Cal State Northridge'] <- 'Cal St Northridge'
cb_stats$School[cb_stats$School == 'Alabama State'] <- 'Alabama St'
cb_stats$School[cb_stats$School == 'Illinois-Chicago'] <- 'Illinois Chicago'
cb_stats$School[cb_stats$School == 'Sam Houston State'] <- 'Sam Houston St'
cb_stats$School[cb_stats$School == 'Delaware State'] <- 'Delaware St'
cb_stats$School[cb_stats$School == 'Albany (NY)'] <- 'Albany'
cb_stats$School[cb_stats$School == 'Texas A&M-Corpus Christi'] <- 'Texas A&M Corpus Christi'
cb_stats$School[cb_stats$School == 'Portland State'] <- 'Portland St'
cb_stats$School[cb_stats$School == 'UT Arlington'] <- 'Texas Arlington'
cb_stats$School[cb_stats$School == 'Cal State Fullerton'] <- 'Cal St Fullerton'
cb_stats$School[cb_stats$School == 'Morgan State'] <- 'Morgan St'
cb_stats$School[cb_stats$School == 'Arkansas-Pine Bluff'] <- 'Arkansas Pine Bluff'
cb_stats$School[cb_stats$School == 'South Dakota State'] <- 'South Dakota St'
cb_stats$School[cb_stats$School == 'UC Irvine' & cb_stats$Year == 2015] <- 'Cal Irvine'
cb_stats$School[cb_stats$School == 'Cal State Bakersfield'] <- 'Cal St Bakersfield'
cb_stats$School[cb_stats$School == 'Fresno State'] <- 'Fresno St'
cb_stats$School[cb_stats$School == 'Jacksonville State'] <- 'Jacksonville St'
cb_stats$School[cb_stats$School == 'Gardner-Webb'] <- 'Gardner Webb'


#reset working directory
setwd('/Users/AydanKoyles/Desktop/Research/March Madness/March Madness Project')

write.csv(cb_stats, 'cb_stats.csv')

#use inner join to join team 1 stats to the upsets dataset
cb_final1 = inner_join(cb_upsets, cb_stats, by = c('Team1' = 'School', 'Year'))

#remove unneccesary columns
cb_final1 = cb_final1 %>% select(Year:Seed2, W:SRS)

#rename each of the columns from this join to represent team1
cb_final1 = rename(cb_final1, 'W1' = 'W', 'L1' = 'L', 'Pts1' = 'Pts', 'Opp1' = 'Opp', 'MOV1' = 'MOV', 
                   'SOS1' = 'SOS', 'OSRS1' = 'OSRS', 'DSRS1' = 'DSRS', 'SRS1' = 'SRS')

#use another inner join function to add in the stats for team 2
cb_final = inner_join(cb_final1, cb_stats, by = c('Team2' = 'School', 'Year'))

#SW Missouri State over Clemson 1987 removed for missing data
cb_final = na.omit(cb_final)

#remove unneccessary columns
cb_final = cb_final %>% select(Year:SRS1, W:SRS)

#rename columns for team 2
cb_final = rename(cb_final, 'W2' = 'W', 'L2' = 'L', 'Pts2' = 'Pts', 'Opp2' = 'Opp', 'MOV2' = 'MOV', 
                  'SOS2' = 'SOS', 'OSRS2' = 'OSRS', 'DSRS2' = 'DSRS', 'SRS2' = 'SRS')

write.csv(cb_final, 'cb_final.csv')

length(unique(cb_upsets$Year))

