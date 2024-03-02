# A2 Team 3 Assignment: Moneyball Capstone Project
# Authors: Adhitya Venkatesan, Ester Van De Put, Isabel Bañuelos,
#          Karen Raji Mathew, Omodesola Owolabi Afolabi, Takumi Someya, 
# Master of Science in Business Analytics, Hult International Business School
# Visualizing & Analyzing Data with R: Methods & Tools - DAT-5323 - FMBANDD1
# Professor: Priyanka Shrivastava
# February 7, 2024
###############################################################################

### Environment Building


setwd("C:/Users/takum/Downloads/HultMBAN/Visualizing & Analyzing Data with R Methods & Tools-Shrivastava/A2")
getwd()

Batting<-read.csv("Batting.csv", na.strings = c("", "N/A", "NULL"))
Salaries<-read.csv("Salaries.csv", na.strings = c("", "N/A", "NULL"))

### Get an overview of the dataset

head(Batting)
head(Salaries)


str(Batting)
str(Salaries)


names(Batting)
names(Salaries)


#Fundamental statistic
summary(Batting)
summary(Salaries)

#################################################################################################

### 1. Handle missing values in the datasets

#Checking for missing values
colSums(is.na(Batting))
colSums(is.na(Salaries))

# Batting Table
# playerID       Player ID code
# yearID         Year
# stint          player's stint (order of appearances within a season)
# teamID         Team
# lgID           League
# G              Games
# G_batting      Game as batter
# AB             At Bats
# R              Runs
# H              Hits
# 2B             Doubles
# 3B             Triples
# HR             Homeruns
# RBI            Runs Batted In
# SB             Stolen Bases
# CS             Caught Stealing
# BB             Base on Balls
# SO             Strikeouts
# IBB            Intentional walks
# HBP            Hit by pitch
# SH             Sacrifice hits
# SF             Sacrifice flies
# GIDP           Grounded into double plays
# G_Old          Old version of games (deprecated)

# Salaries table
# yearID         Year
# teamID         Team
# lgID           League
# playerID       Player ID code
# salary         Salary



#Capture statistical trends in the data set to determine appropriate filling data

# Histograms
hist(Batting$G_batting, main="Histogram of Batting", xlab="Batting") 
hist(Batting$AB, main="Histogram of AB", xlab="AB")
hist(Batting$R, main="Histogram of R", xlab="R")
hist(Batting$H, main="Histogram of H", xlab="H")
hist(Batting$X2B, main="Histogram of X2B", xlab="X2B")
hist(Batting$X3B, main="Histogram of X3B", xlab="X3B")
hist(Batting$HR, main="Histogram of HR", xlab="HR")
hist(Batting$RBI, main="Histogram of RBI", xlab="RBI")
hist(Batting$SB, main="Histogram of SB", xlab="SB")
hist(Batting$CS, main="Histogram of CS", xlab="CS")
hist(Batting$BB, main="Histogram of BB", xlab="BB")
hist(Batting$SO, main="Histogram of SO", xlab="SO")
hist(Batting$IBB, main="Histogram of IBB", xlab="IBB")
hist(Batting$HBP, main="Histogram of HBP", xlab="HBP")
hist(Batting$SH, main="Histogram of SH", xlab="SH")
hist(Batting$SF, main="Histogram of SF", xlab="SF")
hist(Batting$GIDP, main="Histogram of GIDP", xlab="GIDP")
hist(Batting$G_old, main="Histogram of G_old", xlab="G_old")



### Pre-Processing 

# Install zoo package (run only the first time)
install.packages("zoo")

# Load zoo package
library(zoo)

# Fill in missing values with the median *From this point forward, we will proceed with these median values as used in the latter part of the data analysis.
Batting$G_batting <- zoo::na.aggregate(Batting$G_batting, FUN = median)
Batting$AB <- zoo::na.aggregate(Batting$AB, FUN = median)
Batting$R <- zoo::na.aggregate(Batting$R, FUN = median)
Batting$H <- zoo::na.aggregate(Batting$H, FUN = median)
Batting$X2B <- zoo::na.aggregate(Batting$X2B, FUN = median)
Batting$X3B <- zoo::na.aggregate(Batting$X3B, FUN = median)
Batting$HR <- zoo::na.aggregate(Batting$HR, FUN = median)
Batting$RBI <- zoo::na.aggregate(Batting$RBI, FUN = median)
Batting$SB <- zoo::na.aggregate(Batting$SB, FUN = median)
Batting$CS <- zoo::na.aggregate(Batting$CS, FUN = median)
Batting$BB <- zoo::na.aggregate(Batting$BB, FUN = median)
Batting$SO <- zoo::na.aggregate(Batting$SO, FUN = median)
Batting$IBB <- zoo::na.aggregate(Batting$IBB, FUN = median)
Batting$HBP <- zoo::na.aggregate(Batting$HBP, FUN = median)
Batting$SH <- zoo::na.aggregate(Batting$SH, FUN = median)
Batting$SF <- zoo::na.aggregate(Batting$SF, FUN = median)
Batting$GIDP <- zoo::na.aggregate(Batting$GIDP, FUN = median)
Batting$G_old <- zoo::na.aggregate(Batting$G_old, FUN = median)

#Double Checking for missing values
colSums(is.na(Batting))
colSums(is.na(Salaries))

# Joining the Batting and Salaries datasets based on playerID and yearID
merged_data <- merge(Batting, Salaries, by =c("playerID", "yearID"), all.x = FALSE)

# Display the first few rows of the merged dataset
head(merged_data)
str(merged_data)

### 2. Combine data sets and add three more statistics that were used in Moneyball
#Batting average (BA) is defined by the number of hits divided by at bats, AVG=H/AB. Check Note #1

#On-base percentage (OBP) measures how frequently a batter reaches base, OBP=(H+BB+HBP)/(AB+BB+HBP+SF). Check Note #2
#H = Hits
#BB = Bases on Balls (Walks)
#HBP = Hit By Pitch
#AB = At bat
#SF = Sacrifice fly

#OBP is added to slugging average (SLG) to determine on-base plus slugging (OPS), OPS=OBP+SLG

#Slugging percentage (SLG) is a measure of the batting productivity of a hitter, SLG=(1*B+2*2B+3*3B+4*HR)/AB. Check Note #3


# Calculate batting average (AVG)
merged_data$AVG <- merged_data$H / merged_data$AB

# Calculate on-base percentage (OBP)
merged_data$OBP <- (merged_data$H + merged_data$BB + merged_data$HBP) / 
  (merged_data$AB + merged_data$BB + merged_data$HBP + merged_data$SF)

# Calculate slugging percentage (SLG)
#Singles (X1B) missing from the data, we will calculate it based on the total hits minus rest of hits
merged_data$X1B <- merged_data$H - merged_data$X2B - merged_data$X3B - merged_data$HR
merged_data$SLG <- (1 * merged_data$X1B  + 2 * merged_data$X2B + 3 * merged_data$X3B + 4 * merged_data$HR) / merged_data$AB

# Calculate on-base plus slugging (OPS)
merged_data$OPS <- merged_data$OBP + merged_data$SLG

# Display the first few rows of the updated dataset
head(merged_data,100)

# Replace AVG, OBP, SLG, and OPS with 0 if 0/0 = NA at the time of calculation.
merged_data$AVG[is.na(merged_data$AVG)] <- 0
merged_data$OBP[is.na(merged_data$OBP)] <- 0
merged_data$SLG[is.na(merged_data$SLG)] <- 0
merged_data$OPS[is.na(merged_data$OPS)] <- 0

head(merged_data, 100)
colSums(is.na(merged_data))

### 3. Analyze data to find suitable replacements for the pivotal players
#Visualize score for (giambja01), (damonjo01) and ('saenzol01').

install.packages("reshape2")
library(reshape2)


library(ggplot2)

# Filter the merged data for the players lost during the 2001-02 off-season
players_lost <- c('giambja01', 'damonjo01', 'saenzol01')
lost_players_data <- merged_data[merged_data$playerID %in% players_lost, ]

lost_players_data

# Create a data frame for ggplot
ggplot_data <- melt(lost_players_data, id.vars = c('playerID', 'yearID', 'teamID.x'),
                    measure.vars = c('AVG', 'OBP', 'SLG', 'OPS'))

# Plotting using ggplot2
ggplot(ggplot_data, aes(x = yearID, y = value, group = playerID, color = playerID)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ variable, scales = "free_y", ncol = 2) +
  labs(title = "Performance Metrics for Lost Players",
       x = "Year",
       y = "Metrics",
       color = "Player ID") +
  theme_minimal()


# Filtering and refining the data set based on specific criteria.

# [Assumptions]
# Setting team's (Oakland Athletics) strategy as assumptions
# Oakland Athletics adopted the following as their team strategy at the time.
# By deploying a tactic of increasing the number of balls against opposing pitchers and fatiguing them, 
# and by using players with a high on-base percentage, the team aimed to induce mistakes by the opponent and create an offensive advantage. 
# And OBP and other metrics were introduced to evaluate player performance to analyze this quantitatively.
# Since we sign players for several years, we evaluate the stability of those indicators in YEARS and hire players with small variations in them.
# Our team has a small budget and we cannot pay our replacements more salary than we were paying the players being replaced.
# Therefore, we utilize the above indicators and conditions to narrow down the list of potential replacements to three players.

# a)Set thresholds for each indicator based on your team's (Oakland Athletics) strategy
#   Then prioritize and value among the following thresholds in accordance with that strategy. 

# Fine-tune the parameters here
threshold_AVG <- 0.3 #AVG higher than .300 is considered to be excellent (Wikipedia, 2023)
threshold_OBP <- 0.360 #Batting average higher than .36 is considered to be above average (Rogers, 2023)
threshold_SLG <- 0.450 #A good slugging percentage in baseball is 0.450 (Rogers, 2023)
threshold_OPS <- 0.800
year <- 2002 #available players after the departure of Jason Giambi, Johnny Damon, and Jason Isringhausen

# Extract rows where AVG, OBP, SLG, and OPS are all above the threshold
filtered_players <- merged_data[merged_data$AVG >= threshold_AVG & 
                                  merged_data$OBP >= threshold_OBP & 
                                  merged_data$SLG >= threshold_SLG & 
                                  merged_data$OPS >= threshold_OPS &
                                  merged_data$yearID <= year, ]

# Calculate the count of samples for each playerID
sample_counts <- aggregate(yearID ~ playerID, filtered_players, length)

# Filter for playerIDs with at least 3 samples to calculate std correctly
filtered_players <- filtered_players[filtered_players$playerID %in% sample_counts$playerID[sample_counts$yearID >=3], ]

sum(is.na(filtered_players))

# Calculate standard deviation for AVG, OBP, SLG, and OPS for each playerID
standard_deviations <- aggregate(cbind(AVG, OBP, SLG, OPS) ~ playerID, filtered_players, sd)
standard_deviations


# Order by ascending standard deviations for each variable to find most stable, high-performing players
ordered_standard_deviations <- standard_deviations[order(standard_deviations$AVG, 
                                                         standard_deviations$OBP, 
                                                         standard_deviations$SLG, 
                                                         standard_deviations$OPS), ]

# Extract top 50 playerIDs with smallest standard deviations * There is no special meaning to "50"
top_50_players <- head(ordered_standard_deviations$playerID, 50)

# Display the top 50 playerIDs
print(top_50_players)


# Filter the rows of filtered_players to include only those with playerIDs in top_50_players
filtered_players <- filtered_players[filtered_players$playerID %in% top_50_players, ]

# Display the result or use the filtered_players
print(filtered_players)

# Get unique playerID list
unique_playerIDs <- unique(filtered_players$playerID)

# Print unique_playerIDs
print(unique_playerIDs)


# b)Set thresholds for Salary to stay within budget (total annual salary of players to be transferred)
#   Make sure that the total salary is within budget when deciding on the three potential players.

# Extract rows with YearID up to 2001
lost_players_data_2001 <- lost_players_data[lost_players_data$yearID <= 2001, ]
lost_players_data_2001
# Calculate average salary for each playerID
average_salary <- aggregate(salary ~ playerID, lost_players_data_2001, mean)

# Store the average salary in a variable named salary_budget
salary_budget <- sum(average_salary$salary)
salary_budget




# c) Choosing the right players based on a) and b)
#Vizualized the filtered options to find undervalued players comparing salary to OPS (Note 4)
ggplot(filtered_players,aes(x=OPS,y=salary/1000000, color= teamID.x)) + 
  geom_point() +
  labs(title = "OPS compared to Salary",
       x = "OPS",
       y = "Salary (in millions)")

#Determing final selection pool of players considering 1/3 of budget salary for each 
#and OPS choosing top 10%
quantile(filtered_players$OPS, probs = 0.90, na.rm = TRUE) # 90% quartile equal to 1.035 
final_selection<-subset(filtered_players, OPS > 1.035)
final_selection



# Create input to function(final_selection, budget_salary): budget_salary
budget_salary <- salary_budget 

# Create input to function(final_selection, budget_salary): Sort players in order of highest OPS
final_selection <- final_selection[order(final_selection$OPS, decreasing = TRUE), ]

# Explore combinations of three players that will maximize the OPS that will fit within the budget 
find_best_players <- function(final_selection, budget_salary) {
  # Sort players in order of highest OPS
  final_selection <- final_selection[order(final_selection$OPS, decreasing = TRUE), ]
  
  # Finding the best three players
  best_players <- NULL
  for (i in 1:(nrow(final_selection) - 2)) {
    # First player
    player1 <- final_selection[i, ]
    # Index of players selected from the remaining players
    selected_indexes <- c(i)
    for (j in (i + 1):nrow(final_selection)) {
      # Next Player
      player2 <- final_selection[j, ]
      # The sum of the first two players' salaries
      total_salary <- player1$salary + player2$salary
      if (total_salary <= budget_salary) {
        # If on a budget, look for a third player.
        for (k in (j + 1):nrow(final_selection)) {
          player3 <- final_selection[k, ]
          # Total salary when the third player is included
          total_salary <- total_salary + player3$salary
          if (total_salary <= budget_salary) {
            # Update the best combination if within budget
            best_players <- rbind(player1, player2, player3)
            break
          }
        }
        if (!is.null(best_players)) {
          break
        }
      }
    }
    if (!is.null(best_players)) {
      break
    }
  }
  
  # Return optimal combination
  return(best_players)
}


best_players <- find_best_players(final_selection, budget_salary)

#Savings in budget by hiring these replacements
print(savings<-sum(best_players$salary)/salary_budget) # 27% of the budget is saved

### 4.  Best_players; the final selection of replacements
print(best_players)

### 5. Conclusion

# [Insights & Comments]
# By replacing injured players on the Oakland Athletics team with Glenallen Hill(hillgl01), 
# Wade Boggs(boggswa01), and Kal Daniels(danieka01) based on their statistical comparisons, 
# the team can potentially save a significant portion of their salary while 
# increasing their chances of winning. These players' performance indicators 
# suggest that they can achieve comparable outcomes at a much lower cost, 
# providing a tactical advantage in terms of optimizing resources and competitiveness.

# [Our Decision-Making Approach & Implementation]
# We set out to figure out the team's situation and fundamental strategy, which we then put into a quantitative indicator. 
# First, we evaluated AVG, OBP, SLG, and OPS as indicators of a player's ability in order to find them among a large number of potential players. 
# In addition, we used the year-to-year deviations of the above indicators to filter the candidates in order to select players who are expected to be stable and successful. 
# We then systematically identified them by searching for the optimal combination of a larger OPS, which was of particular importance, 
# and the sum of the salaries of the three alternative players, which fell within the budget.



###############################################################################
# Notes:
# 1. "Batting average (AVG) is determined by dividing a player's hits by his total at-bats" (MLB, 2024)
# AVG = Hits / At Bats = H/AB
# 2."On Base Percentage (OBP) refers to how frequently a batter reaches base per plate appearance. 
# Times on base include hits, walks and hit-by-pitches, but do not include errors, 
# times reached on a fielder's choice or a dropped third strike." (MLB, 2024)
# OBP = Hits + BaseonBalls + HitByPitch  / At Bats + BaseonBalls + HitByPitch + Sacrifice Flies
# OBP = H + BB + HBP/AB + BB + HBP + SF
# 3."Slugging percentage (SLG) represents the total number of bases a player records per at-bat." (MLB, 2024)
# SLG = (X1B + 2*X2B + 3*X3B + 4*HR)/AB
#Singles (X1B) missing from the data, we will calculate it based on the total hits minus rest of hits
# 4. "On-base Plus Slugging (OPS) adds on-base percentage and slugging percentage to get one number that 
# unites the two. It's meant to combine how well a hitter can reach base, with how well he can hit for 
# average and for power." (MLB, 2024)
# On-base percentage + Slugging percentage = OBP + SLG

###############################################################################
# References:
# Standard Stats | Glossary | MLB (2024). MLB.com. https://www.mlb.com/glossary/standard-stats
# wikiHow. (2023, July 4). How to Calculate On Base Percentage: 12 Steps (with Pictures). wikiHow. https://www.wikihow.com/Calculate-On-Base-Percentage#:~:text=To%20find%20the%20on%20base,on%20balls%2C%20and%20sacrifice%20flies.
# Rogers, M. (2023, October 4). What is a good On-Base percentage in baseball? (Explained). Nations-Baseball. https://www.nations-baseball.com/good-on-base-percentage-in-baseball/
# Wikipedia. (2023, July 14). Batting average. Wikipedia. https://en.wikipedia.org/wiki/Batting_average
# OpenAI. (2024). ChatGPT (3.5) [Large language model]. https://chat.openai.com;https://chat.openai.com/share/1b0dde3e-3b7e-4b18-9ffd-ed5268bfaa1d