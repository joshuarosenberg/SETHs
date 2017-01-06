# -----------------------------------------------------------------------------------------
# 4_fin_analysis, takes "summary_data.RDS", "sna_out.RDS", and "all_data_ss.RDS", 
# also takes "chat_data_sync_users.RDS", "chat_data_async_users.RDS", and "no_chat_data_async_users.RDS" # need to be made before parts of analysis
# makes: "all_data_chat.RDS", "all_data_no_chat.RDS", "all_data_chat_sync.RDS", "all_data_chat_async.RDS", "all_data_no_chat_async.RDS"
# makes output: stat tests and data frames for 
# "summary_data_chat.RDS", "summary_data_time.RDS", and "summary_data_time_bin.RDS"
# also for LIWC: "all_data_wrt_chat.csv" and "all_data_wrt_time.csv"
# -----------------------------------------------------------------------------------------

library(lubridate)
library(dplyr)
library(stringr)
library(readr)
library(ggplot2)
library(AnomalyDetection)

rm(list = ls())

options(stringsAsFactors = F)

setwd("~/Dropbox/research/state_tweets")

all_data_ss <- readRDS("dev/all_data_ss.rds") 

all_data_ss <- arrange(all_data_ss, Date)

all_data_ss$yday <- yday(all_data_ss$NewDate)

# 
# state_tweets_users <- unique(all_data_ss$user)
# 
# write.csv(state_tweets_users, "state_tweets_users.csv")

summary_data <- readRDS("dev/summary_data.RDS")
sna_out <- read.csv("sna_out.csv") # need to optimize
sync_chat <- read.csv("csvs/data_ss_sync_chat.csv", stringsAsFactors = F) # need to optimize this

# For setting up anomaly detection

my_vec <- select(all_data_ss, hashtag, state)
my_vec <- arrange(unique(my_vec), state)

my_tmp_df <- left_join(my_vec, sync_chat, by = "state")
my_tmp_df <- select(my_tmp_df, hashtag, state, sync_chat)
my_tmp_df <- filter(my_tmp_df, sync_chat == 1)
sync_vec_state <- select(my_tmp_df, state)
sync_vec <- select(my_tmp_df, hashtag)

# For anomaly detection

out_ls <- list()
unique_dates <- sort(unique(all_data_ss$NewDate))
unique_dates <- unique_dates[1:181]

for (i in 1:length(sync_vec$hashtag)){
      
      df <- arrange(data.frame(date = unique_dates, hour = rep(0:23, length(unique_dates)), freq = rep(0, length(unique_dates) * 24)), date, hour)

      tmp_df <- filter(all_data_ss, hashtag == sync_vec$hashtag[i])
      
      for (j in 1:nrow(tmp_df)){
            df$freq[which(df$date == tmp_df[j, ]$NewDate & df$hour == tmp_df$Hour[j])] <- df$freq[which(df$date == tmp_df[j, ]$NewDate & df$hour == tmp_df$Hour[j])] + 1
            print(paste0("Processing hashtag ", i, "/", length(sync_vec$hashtag), " tweet ", j))
      }
      
      df$date_time <- ymd(df$date) + hours(df$hour)
      res <- select(df, date_time, freq)
      out_ls[[i]] <- res
#       data_anomaly = AnomalyDetectionTs(res, max_anoms=0.01, direction="pos", plot=TRUE, e_value = F)
#       out_ls[[i]] <- data_anomaly
}

names(out_ls) <- sync_vec_state$state
out_df <- as.data.frame(out_ls)
names(out_df)[1] <- "date_time"
out_df$hour <- hour(out_df$date_time)
out_df$yday <- yday(out_df$date_time)
out_df <- select(out_df,
                 date_time,
                 yday,
                 hour,
                 contains("freq"))

names(out_df) <- c("date_time", "yday", "hour", sync_vec_state$state)

anomaly_ls <- list()
      
for (i in 1:30){
      tmp_df <- out_df[, c(1, (i + 3))]
      # tmp_df <- select(out_df, date_time, sync_vec_state$state[i])
      data_anomaly = AnomalyDetectionTs(tmp_df, max_anoms=0.01, alpha = .005, direction="pos", plot=TRUE, e_value = F)
#       print(data_anomaly)
#       dev.copy2pdf(file= paste0("anomaly", i, ".pdf"), width = 7, height = 5)
      out <- data_anomaly$anoms
      out$hour <- hour(out$timestamp)
      out$yday <- yday(out$timestamp)
      anomaly_ls[[i]] <- out
}

names(anomaly_ls) <- sync_vec_state$state
sync_vec_state$state

anomaly_ls[[1]]

# names(out_df)[1] <- "date_time"
# out_df$hour <- hour(out_df$date_time)
# out_df$yday <- yday(out_df$date_time)
# out_df <- select(out_df,
#                  date_time,
#                  yday,
#                  hour,
#                  contains("freq"))

# tmp_df
# colSums(out_df[, 3:32])
# tmp_df <- out_df[, c(1, (i + 3))]
# data_anomaly = AnomalyDetectionTs(tmp_df, max_anoms=0.01, direction="pos", plot=TRUE, e_value = F)

# -------------------
# subsetting sync chats
# -------------------

all_data_ss$hour <- hour(all_data_ss$NewTime) # move to processing file
all_data_ss$wday <- wday(all_data_ss$NewTime, label = F) # can add label

# Finds which states have a one hour period with 10 times as much activity as the average hour

# results <- data.frame(state = names(table(all_data_ss$state)), sync_chat = rep(FALSE, length(table(all_data_ss$state))))
# table_ls <- list()
# 
# for (i in 1:length(table(all_data_ss$state))){
#       temp_df <- all_data_ss[all_data_ss$state == sort(names(table(all_data_ss$state)))[i], ]
#       
#       if (any(as.vector(table(temp_df$hour, temp_df$wday)) > mean(as.vector(table(temp_df$hour, temp_df$wday))) * 15)){
#             results$sync_chat[i] <- TRUE
#       }
#       
#       table_ls[[i]] <- table(temp_df$hour, temp_df$wday)
#       
#       results$sync_time[i] <- which.max(as.vector(table(temp_df$hour, temp_df$wday)))
#       results$sync_day[i] <- ceiling(results$sync_time[i] / 24)
#       results$sync_hour[i] <- results$sync_time[i] %% 24
#       
#       print(paste0(names(table(all_data_ss$state))[i], " is ", results[i, 2]))
#       
# }

# merges data for sync chats

# need to build this into one

summary_data_chat_all <- cbind(summary_data, sync_chat = sync_chat$sync_chat, auto_sync = sync_chat$auto_sync_chat, 
                               unsure = sync_chat$not_sure, day = sync_chat$sync_day, hour = sync_chat$sync_hour,
                               density = sna_out$density, reciprocity = sna_out$reciprocity, length = sna_out$length,
                               distance = sna_out$mean_distance, degree = sna_out$mean_degree)

str(summary_data_chat_all)

chat <- filter(summary_data_chat_all, sync_chat == 1)
no_chat <- filter(summary_data_chat_all, sync_chat == 0)

all_data_chat <- filter(all_data_ss, state %in% chat$state)
all_data_no_chat <- filter(all_data_ss, state %in% no_chat$state)

all_data_chat <- mutate(all_data_chat, group = "chat")
all_data_no_chat <- mutate(all_data_no_chat, group = "no_chat")

all_data_wrt_chat <- bind_rows(all_data_chat, all_data_no_chat)

# write.csv(all_data_wrt_chat, "~/google drive/research/state_tweets/all_data_wrt_chat.csv")

# write.csv(all_data_wrt_chat, "~/google drive/research/state_tweets/all_data_wrt_chat.csv")

saveRDS(all_data_chat, "dev/all_data_chat.RDS")
saveRDS(all_data_no_chat, "dev/all_data_no_chat.RDS")

# summary_data_chat <- select(summary_data_chat_all,
#                             N:Activity,
#                             sync_chat,
#                             hour,
#                             day)
#need to add social network stats)

# Compares hashtags on the basis of having a sync chat

sync_1 <- filter(summary_data_chat_all, sync_chat == 1)

sync_0 <- filter(summary_data_chat_all, sync_chat == 0)

summary_data_sync <- bind_rows(sync_1, sync_0)

summary_data_sync <- mutate(summary_data_sync,
                            count = 1)

summary_data_sync <- group_by(summary_data_sync, sync_chat)

summary_data_sync

saveRDS(summary_data_sync, "summary_data_chat.RDS")

# Use this for statistical tests

# sum_dat <- select(summary_data_sync,
#                   N:Activity,
#                   density:degree,
#                   sync_chat)
# 
# str(sum_dat)
# 
# sum_dat <- as.data.frame(sum_dat)

# Hotelling's t-test
# 
# library(Hotelling)
# 
# x1 <- filter(sum_dat,
#              sync_chat == 1)
# x2 <- filter(sum_dat,
#              sync_chat == 0)
# x1 <- x1[, -19]
# x2 <- x2[, -19]
# res <- hotelling.test(x1, x2)
# print(res)
# 
# t-tests
# 
# for (i in 1:18){
#       print(paste0("For ", names(sum_dat)[i]))
#       print(t.test(sum_dat[, i] ~ sum_dat[, 20]))
# }

# Y <- cbind(sum_dat_sync_out$N, sum_dat_sync_out$AveChars, 
#           sum_dat_sync_out$AveHashTags, sum_dat_sync_out$AveMentions,
#           sum_dat_sync_out$AveLinks,
#           sum_dat_sync_out$AveRetweets, sum_dat_sync_out$AveFavorites, sum_dat_sync_out$Voices)
# 
# fit <- manova(Y ~ sync_chat, data = sum_dat_sync_out)
# summary(fit)
# summary.aov(fit)

# Use this for summary output

summary_data_sync_grouped_out <- summarize(summary_data_sync,
                                           
                                           AveTweets = mean(N),
                                           NumChats = sum(count),
                                           AveTeachers = mean(Teachers),
                                           AveActivity = mean(Activity),
                                           AveVoices = mean(Voices),
                                           #                                        VoicesPer = length(unique(Voices)) / Teachers[1],
                                           #                                        TweetsPer = length(unique(Voices)) / N,
                                           
                                           AveChars = mean(AveChars),
                                           AveWords = mean(AveWords),
                                           AveHashTags = mean(AveHashTags),
                                           AveMentions = mean(AveMentions),
                                           AveLinks = mean(AveLinks),
                                           AveFollowers = mean(AveFollowers), 
                                           AveFollows = mean(AveFollows), 
                                           AveRetweets = mean(AveRetweets),
                                           AveFavorites = mean(AveFavorites),
                                           
                                           Density = mean(density),
                                           Reciprocity = mean(reciprocity), 
                                           Length = mean(length),
                                           Distance = mean(distance), 
                                           Degree = mean(degree))

summary_data_sync_grouped_out <- t(summary_data_sync_grouped_out)

round(summary_data_sync_grouped_out, 3)

saveRDS(summary_data_sync_grouped_out, "dev/summary_data_chat.RDS") # for output for sync chat

# write.csv(results, "auto_sync_chat_detection.csv")

# Compares tweets on the basis of being during a synchronous chat

# -------------------
# 6. Making merged summary data for time (sync_chat versus not) - need to add additional hours
# -------------------

# For chat hashtags 

sync_chat_bin <- select(sync_chat,
                        state, sync_chat)

sync_chat_bin <- filter(sync_chat_bin,
                        sync_chat == 1)

tmp_bool <- names(table(all_data_ss$state)) %in% sync_chat_bin$state

chat_data_sync_ls <- list()
chat_data_async_ls <- list()

for (i in 1:length(table(all_data_ss$state)[tmp_bool])){
      
      my_state <- all_data_ss$state == sort(names(table(all_data_ss$state)))[tmp_bool][i]
      
      temp_df <- all_data_ss[my_state, ]
      
      sync_time <- temp_df$wday == summary_data_chat_all$day[tmp_bool][i] & (temp_df$hour == summary_data_chat_all$hour[tmp_bool][i] - 1 | 
                                                                                   temp_df$hour == summary_data_chat_all$hour[tmp_bool][i] - 1 - 1 |
                                                                                   temp_df$hour == summary_data_chat_all$hour[tmp_bool][i] - 1 + 1)
      
      data_sync <- temp_df[sync_time, ] # need to include hour before and after
      data_async <- temp_df[!sync_time, ] # need to include hour before and after
      
      chat_data_sync_ls[[i]] <- data_sync # saves sync data to list
      chat_data_async_ls[[i]] <- data_async # saves async data to list
      
      print(paste0("Processed ", names(table(all_data_ss$state))[tmp_bool][i]))
}

chat_data_sync <- plyr::ldply(chat_data_sync_ls)
chat_data_async <- plyr::ldply(chat_data_async_ls)

# 
# chat_data_sync <- mutate(chat_data_sync,
#                          sync_chat = 1)
# 
# chat_data_async <- mutate(chat_data_async,
#                           sync_chat = 0)
# 
# all_chat_data <- bind_rows(chat_data_sync, chat_data_async)
# 
# all_chat_data_ss <- select(all_chat_data,
#                            Followers:Favorites, CharCount, WordCount, HashTagsNum, Links, Mentions)
# 
# t.test(all_chat_data$CharCount ~ all_chat_data$sync_chat)
# t.test(all_chat_data$WordCount ~ all_chat_data$sync_chat)
# t.test(all_chat_data$HashTagsNum ~ all_chat_data$sync_chat)
# t.test(all_chat_data$Mentions ~ all_chat_data$sync_chat)
# t.test(all_chat_data$Links ~ all_chat_data$sync_chat)
# t.test(all_chat_data$Followers ~ all_chat_data$sync_chat)
# t.test(all_chat_data$Follows ~ all_chat_data$sync_chat)
# t.test(all_chat_data$Retweets ~ all_chat_data$sync_chat)
# t.test(all_chat_data$Favorites ~ all_chat_data$sync_chat)

# For no chat hashtags

tmp_bool <- !(names(table(all_data_ss$state)) %in% sync_chat_bin$state)
no_chat_data_async_ls <- list()
for (i in 1:length(table(all_data_ss$state)[tmp_bool])){
      my_state <- all_data_ss$state == sort(names(table(all_data_ss$state)))[tmp_bool][i]
      no_chat_data_async_ls[[i]] <- all_data_ss[my_state, ]
      print(paste0("Processed ", names(table(all_data_ss$state))[tmp_bool][i]))
}

no_chat_data_async <- plyr::ldply(no_chat_data_async_ls)

# Creating summary statistics - with all three groups

# Loading user data

chat_data_sync_users <- readRDS("chat_data_sync_users.RDS")
chat_data_async_users <- readRDS("chat_data_async_users.RDS")
no_chat_data_async_users <- readRDS("no_chat_data_async_users.RDS")

# Need to optimize

chat_data_sync <- mutate(chat_data_sync,
                         group = "chat_data_sync",
                         count = 1)

chat_data_sync <- left_join(chat_data_sync, chat_data_sync_users, by = "user")

chat_data_async <- mutate(chat_data_async,
                          group = "chat_data_async",
                          count = 1)

chat_data_async <- left_join(chat_data_async, chat_data_async_users, by = "user")

no_chat_data_async <- mutate(no_chat_data_async,
                             group = "no_chat_data_async",
                             count = 1)

no_chat_data_async <- left_join(no_chat_data_async, chat_data_sync_users, by = "user")

# merging

all_time_data <- bind_rows(chat_data_sync, chat_data_async, no_chat_data_async)

write.csv(all_time_data, "~/google drive/research/state_tweets/all_data_wrt_time.csv")

saveRDS(filter(all_time_data, group == "chat_data_sync"), "dev/all_data_sync.RDS")
saveRDS(filter(all_time_data, group == "chat_data_async"), "dev/all_data_async.RDS")
saveRDS(filter(all_time_data, group == "no_chat_data_async"), "dev/all_data_async.RDS")

# corrs

chat_data_sync_corr <- select(chat_data_sync, CharCount, WordCount, HashTagsNum, Links, Retweets, Mentions, Favorites, Followers, Follows, n_sustained, n_sustained_bin)
Hmisc::rcorr(as.matrix(chat_data_sync_corr))

chat_data_async_corr <- select(chat_data_async, CharCount, WordCount, HashTagsNum, Links, Retweets, Mentions, Favorites, Followers, Follows, n_sustained, n_sustained_bin)
Hmisc::rcorr(as.matrix(chat_data_async_corr))

no_chat_data_async_corr <- select(no_chat_data_async, CharCount, WordCount, HashTagsNum, Links, Retweets, Mentions, Favorites, Followers, Follows, n_sustained, n_sustained_bin)
Hmisc::rcorr(as.matrix(no_chat_data_async_corr))

# optimize

all_time_data_ss <- select(all_time_data,
                           Followers:Favorites, CharCount, WordCount, HashTagsNum, Links, Mentions, user, group)

# fit <- manova(as.matrix(all_time_data_ss[, 1:ncol(all_time_data_ss) - 1]) ~ all_time_data_ss$group) # check this
# summary(fit)
# 
# fit <- fit <- aov(all_time_data_ss$CharCount ~ all_time_data_ss$group)
# summary(fit)
# TukeyHSD(fit)
# fit <- aov(all_time_data_ss$WordCount ~ all_time_data_ss$group)
# summary(fit)
# TukeyHSD(fit)
# fit <- aov(all_time_data_ss$HashTagsNum ~ all_time_data_ss$group)
# summary(fit)
# TukeyHSD(fit)
# fit <- aov(all_time_data_ss$Mentions ~ all_time_data_ss$group)
# summary(fit)
# TukeyHSD(fit)
# fit <- aov(all_time_data_ss$Links ~ all_time_data_ss$group)
# summary(fit)
# TukeyHSD(fit)
# fit <- aov(all_time_data_ss$Followers ~ all_time_data_ss$group)
# summary(fit)
# TukeyHSD(fit)
# fit <- aov(all_time_data_ss$Follows ~ all_time_data_ss$group)
# summary(fit)
# TukeyHSD(fit)
# fit <- aov(all_time_data_ss$Retweets ~ all_time_data_ss$group)
# summary(fit)
# TukeyHSD(fit)
# fit <- aov(all_time_data_ss$Favorites ~ all_time_data_ss$group)
# summary(fit)
# TukeyHSD(fit)

all_time_data_grouped <- group_by(all_time_data_ss, group)

summary_time <- summarise(all_time_data_grouped,
                          N = length(user),
                          Voices = length(unique(user)),
                          
                          AveChars = mean(CharCount, na.rm = T),
                          AveWords = mean(WordCount, na.rm = T),
                          AveHashTags = mean(HashTagsNum, na.rm = T),
                          AveMentions = mean(Mentions, na.rm = T),
                          AveLinks = mean(Links, na.rm = T),
                          AveFollowers = mean(Followers, na.rm = T),
                          AveFollows = mean(Follows, na.rm = T),
                          AveRetweets = mean(Retweets, na.rm = T),
                          AveFavorites = mean(Favorites, na.rm = T))

#                           VoicesPer = length(unique(Voices)) / teachers[1],
#                           TweetsPer = length(unique(Voices)) / N)

saveRDS(summary_time, "dev/summary_data_time.RDS")

# as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
# 
# for (i in 2:length(summary_time)){
#   tmp_vec <- c(as.numeric.factor(summary_time[1, i]), as.numeric.factor(summary_time[2, i]), as.numeric.factor(summary_time[3, i]))
#   print(paste0("For ", names(summary_time)[i]))
#   print(chisq.test(tmp_vec))
# }
# 
# x <- as.data.frame(sapply(summary_time, function(x) as.numeric.factor(x)))
# chisq.test(x[, 2:ncol(x)])
# 
# summary_time[1, 2]
# summary_time[2, 2]
# summary_time[3, 2]
# 
# summary_time[1, 1]
# , summary_time[2, 1], summary_time[3, 1])
# summary_time
# summary_time <- t(summary_time)

# Creating summary statistics - with just sync time and non-sync time

df <- data.frame(var_1 = )

sync_data <- mutate(chat_data_sync,
                    group = "sync_data",
                    count = 1)

async_data <- bind_rows(chat_data_async, no_chat_data_async)

async_data <- mutate(async_data,
                     group = "async_data",
                     count = 1)

all_time_data_two <- bind_rows(sync_data, async_data)

all_time_data_ss <- select(all_time_data_two,
                           Followers:Favorites, CharCount, WordCount, HashTagsNum, Links, Mentions, group)
# 
# fit <- manova(as.matrix(all_time_data_ss[, 1:ncol(all_time_data_ss) - 1]) ~ all_time_data_ss$group) # check this
# summary(fit)
# 
# fit <- fit <- aov(all_time_data_ss$CharCount ~ all_time_data_ss$group)
# summary(fit)
# TukeyHSD(fit)
# fit <- aov(all_time_data_ss$WordCount ~ all_time_data_ss$group)
# summary(fit)
# TukeyHSD(fit)
# fit <- aov(all_time_data_ss$HashTagsNum ~ all_time_data_ss$group)
# summary(fit)
# TukeyHSD(fit)
# fit <- aov(all_time_data_ss$Mentions ~ all_time_data_ss$group)
# summary(fit)
# TukeyHSD(fit)
# fit <- aov(all_time_data_ss$Links ~ all_time_data_ss$group)
# summary(fit)
# TukeyHSD(fit)
# fit <- aov(all_time_data_ss$Followers ~ all_time_data_ss$group)
# summary(fit)
# TukeyHSD(fit)
# fit <- aov(all_time_data_ss$Follows ~ all_time_data_ss$group)
# summary(fit)
# TukeyHSD(fit)
# fit <- aov(all_time_data_ss$Retweets ~ all_time_data_ss$group)
# summary(fit)
# TukeyHSD(fit)
# fit <- aov(all_time_data_ss$Favorites ~ all_time_data_ss$group)
# summary(fit)
# TukeyHSD(fit)

all_time_data_grouped_two <- group_by(all_time_data_two, group)

summary_time_bin <- summarise(all_time_data_grouped_two,
                              N = length(user),
                              Voices = length(unique(user)),
                              
                              AveChars = mean(CharCount, na.rm = T),
                              AveWords = mean(WordCount, na.rm = T),
                              AveHashTags = mean(HashTagsNum, na.rm = T),
                              AveMentions = mean(Mentions, na.rm = T),
                              AveLinks = mean(Links, na.rm = T),
                              AveFollowers = mean(Followers, na.rm = T),
                              AveFollows = mean(Follows, na.rm = T),
                              AveRetweets = mean(Retweets, na.rm = T),
                              AveFavorites = mean(Favorites, na.rm = T))

#                           VoicesPer = length(unique(Voices)) / teachers[1],
#                           TweetsPer = length(unique(Voices)) / N)

saveRDS(summary_time_bin, "dev/summary_data_time_bin.RDS")

# rm(list = ls())
