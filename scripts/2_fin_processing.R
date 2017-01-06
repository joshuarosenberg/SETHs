# -----------------------------------------------------------------------------------------
# 2_fin_processing, takes "all_data.RDS", makes "all_data_ss.RDS" and "summary_data.RDS"
# -----------------------------------------------------------------------------------------

# -------------------
# 1. set workspace, load data, load packages
# -------------------

setwd("~/dropbox/research/state_tweets/") # Josh

library(lubridate)
library(dplyr)
library(stringr)

rm(list = ls())

all_data <- readr::read_rds("/Users/joshuarosenberg/Google\ Drive/1_Research/SETHs/all_data.RDS") # get this from mjk_optimized
tbl_df(all_data)

sum(duplicated(dplyr::select(all_data, `Twitter.User`:`Tweet.Text`)))

# -------------------
# 2. processing
# -------------------

# Merges two miched hashtags

all_data$hashtag[all_data$hashtag == "#miched_one"] <- "#miched"
all_data$hashtag[all_data$hashtag == "#miched_two"] <- "#miched"

all_data$state[all_data$state == "MI2"] <- "MI"

# Filters all_data by followers and followees

my_followers_bool <- all_data$Followers > 100000 # change these parameters
my_followees_bool <- all_data$Follows > 100000 # change these parameters

# how many per week # 181/7 weeks
# how many per days # 181 days
# how many per month # 6 mos.

all_data_ss <- all_data[!my_followers_bool | !my_followees_bool, ]

# Fixing username

all_data_ss$user <- sapply(all_data_ss$URL, function(x)
                           strsplit(x, "/")[[1]][4])

# Filters all_data_ss by dates

head(all_data_ss) #1, 3 hours behind, #2 3 hours behind, #3 3 hours behind

all_data_ss$NewTime <- parse_date_time(all_data_ss$NewTime, "ymd_hms", tz = "EST")

after <- parse_date_time("2015-01-01 00:00:00", "ymd_hms", tz = "EST")
before <- parse_date_time("2015-06-30 23:59:59", "ymd_hms", tz = "EST")

all_data_ss <- filter(all_data_ss, # need to think through how dates are filtered this way - may start too early and end too late?
                      NewTime >= after,
                      NewTime <= before)

all_data_ss <- arrange(all_data_ss, NewTime)

# how many filtered

paste0("Filtered ", nrow(all_data) - nrow(all_data_ss), " tweets")

# Making better mentions

# samp <- sample_n(all_data_ss, 100)

all_data_ss$reply <- ifelse(grepl("@", substr(all_data_ss$Tweet.Text, 1, 1)) | grepl(".@", substr(all_data_ss$Tweet.Text, 1, 2)), 1, 0)
all_data_ss$rt <- ifelse(grepl("RT", substr(all_data_ss$Tweet.Text, 1, 2)), 1, 0)

all_data_ss$mentions_no_replies <- ifelse(all_data_ss$reply == 1, all_data_ss$Mentions - all_data_ss$reply, all_data_ss$Mentions)
all_data_ss$mentions_no_retweets <- ifelse(all_data_ss$Retweets >= 1, all_data_ss$Mentions - 1, all_data_ss$Mentions)

# Making yday

all_data_ss$yday <- yday(all_data_ss$NewDate)

# removing all data

rm(all_data)

# Makes new summary data for all data grouped by state

# all_data_ss <- readRDS("dev/all_data_ss.RDS")

all_data_ss_grouped <- group_by(all_data_ss, state)

summary_data <- summarise(all_data_ss_grouped,
                          SETH = first(hashtag),
                          N = length(Twitter.User),
                          Teachers = teachers[1],
                          AveChars = mean(CharCount, na.rm = T),
                          AveWords = mean(WordCount, na.rm = T),
                          AveHashTags = mean(HashTagsNum, na.rm = T),
                          AveMentions = mean(Mentions, na.rm = T),
                          AveLinks = mean(Links, na.rm = T),
                          AveFollowers = mean(Followers, na.rm = T),
                          AveFollows = mean(Follows, na.rm = T),
                          AveRetweets = mean(Retweets, na.rm = T),
                          AveFavorites = mean(Favorites, na.rm = T),
                          Voices = length(unique(Twitter.User)),
                          Activity = length(hashtag) / teachers[1],
                          VoicesPer = length(unique(Voices)) / teachers[1],
                          TweetsPer = length(unique(Voices)) / N)

write.csv(summary_data, "summary_data.csv")

saveRDS(summary_data, "dev/summary_data.rds")

saveRDS(all_data_ss, "dev/all_data_ss.rds")

# nrow(readRDS("dev/all_data_ss.rds"))

# rm(list = ls())
