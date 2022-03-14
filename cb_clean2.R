#install libraries
library(dplyr)
library(data.table)
library(tidyverse)

#read in upsets data
cb_upsets = fread(file = 'cb_upsets.csv')

#read in round 1 data
cb_Round1 = fread(file = 'cb_Round1.csv')

#remove games between 8 and 9 seeds (not considered an upset for the purposes of this study)
cb_Round1_updated = cb_Round1 %>% filter(Seed1 != 8)

#read in stats data
cb_stats = fread(file = 'cb_stats.csv')

#join dataframes
mm_data1 = inner_join(cb_Round1_updated, cb_stats, by = c('Team1' = 'School', 'Year'))

#remove unneccessary columns
mm_data1 = mm_data1 %>% select(Year:Seed2, W:SRS)

#rename each of the columns from this join to represent team1
mm_data1 = rename(mm_data1, 'W1' = 'W', 'L1' = 'L', 'Pts1' = 'Pts', 'Opp1' = 'Opp', 'MOV1' = 'MOV', 
                   'SOS1' = 'SOS', 'OSRS1' = 'OSRS', 'DSRS1' = 'DSRS', 'SRS1' = 'SRS')

#final join to obtain final dataframe
mm_final = inner_join(mm_data1, cb_stats, by = c('Team2' = 'School', 'Year'))

#remove unneccessary columns
mm_final = mm_final %>% select(Year, Seed1:SRS1, W:SRS)

#rename columns for team2
mm_final = rename(mm_final, 'W2' = 'W', 'L2' = 'L', 'Pts2' = 'Pts', 'Opp2' = 'Opp', 'MOV2' = 'MOV', 
                  'SOS2' = 'SOS', 'OSRS2' = 'OSRS', 'DSRS2' = 'DSRS', 'SRS2' = 'SRS')

#add a binary variable indicating whether the game was an upset (0 = no upset, 1 = upset)
mm_final$Upset = rep(0, length(mm_final$Year))
mm_final$Upset[mm_final$Score2>mm_final$Score1] = 1

#create winning percentage variable for each team
createWP1 = function(x, output) {
  W = as.integer(x[8])
  L = as.integer(x[9])
  return(round(W/(W+L), 2))
}

mm_final$WP1 = apply(mm_final, 1, createWP1)

createWP2 = function(x, output) {
  W = as.integer(x[17])
  L = as.integer(x[18])
  return(round(W/(W+L), 2))
}

mm_final$WP2 = apply(mm_final, 1, createWP2)

#create variable for the MOV for team 2 in this particular game
createUpsetMOV = function(x, output) {
  S1 = as.integer(x[3])
  S2 = as.integer(x[6])
  return(S2-S1)
}

mm_final$upsetMOV = apply(mm_final, 1, createUpsetMOV)

#create seed difference variable
createSeedDiff = function(x, output) {
  seed1 = as.integer(x[2])
  seed2 = as.integer(x[7])
  return(seed2-seed1)
}

mm_final$seedDiff = apply(mm_final, 1, createSeedDiff)

#remove rows with N/A values
mm_final = na.omit(mm_final)

#1987 Clemson vs. Southwest Missouri St removed for missing values

mm_final = subset(mm_final, select = -V1)

write.csv(mm_final, 'mm_final.csv')

#read in 2021 data
games21 = fread(file = '2021_games.csv')

#read in 2022 data (will need two files because of play in games)

games22_v1 = fread(file = '2022_games1.csv')
games22_v2 = fread(file = '2022_games2.csv')


