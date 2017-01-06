setwd("~/Dropbox/research/state_tweets")

data <- readr::read_csv("csvs/twitterProfiles.csv")

str(data)

length(data$location[data$location == "N/A"])

library(dplyr)

data <- select(data, handle, profile, joinDate, numTweets)

(length(grep("teach", data$profile)) + length(grep("Teach", data$profile))) / length(data$profile) # 24.9 % mention "teach" in their profiles
