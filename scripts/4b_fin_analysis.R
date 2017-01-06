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

all_data_ss$yday <- yday(all_data_ss$NewDate)

sync_chat <- read.csv("csvs/data_ss_sync_chat.csv", stringsAsFactors = F) # need to optimize this

# For setting up anomaly detection

my_vec <- select(all_data_ss, hashtag, state)
my_vec <- arrange(unique(my_vec), state)

my_tmp_df <- left_join(my_vec, sync_chat, by = "state")
my_tmp_df <- select(my_tmp_df, hashtag, state, sync_chat)
async_vec_state <- filter(my_tmp_df, sync_chat == 0)
sync_vec_tmp <- filter(my_tmp_df, sync_chat == 1)
sync_vec_state <- select(my_tmp_df, state)
# async_vec_state <- select(async_vec_state, state)
sync_vec <- select(my_tmp_df, hashtag)

# # # # For anomaly detection
# 
# out_ls <- list()
# unique_dates <- sort(unique(all_data_ss$NewDate))
# unique_dates <- unique_dates[1:181]
# 
# for (i in 1:length(sync_vec$hashtag)){
#       
#       df <- arrange(data.frame(date = unique_dates, hour = rep(0:23, length(unique_dates)), freq = rep(0, length(unique_dates) * 24)), date, hour)
#       
#       tmp_df <- filter(all_data_ss, hashtag == sync_vec$hashtag[i])
#       
#       for (j in 1:nrow(tmp_df)){
#             df$freq[which(df$date == tmp_df[j, ]$NewDate & df$hour == tmp_df$Hour[j])] <- df$freq[which(df$date == tmp_df[j, ]$NewDate & df$hour == tmp_df$Hour[j])] + 1
#             print(paste0("Processing hashtag ", i, "/", length(sync_vec$hashtag), " tweet ", j))
#       }
#       
#       df$date_time <- ymd(df$date) + hours(df$hour)
#       res <- select(df, date_time, freq)
#       out_ls[[i]] <- res
#       #       data_anomaly = AnomalyDetectionTs(res, max_anoms=0.01, direction="pos", plot=TRUE, e_value = F)
#       #       out_ls[[i]] <- data_anomaly
# }
# 
# # saveRDS(out_ls, "out_ls.rds")

out_ls <- readRDS("out_ls.rds")

# something is up with MN and MO

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
for (i in 1:47){
      tmp_df <- out_df[, c(1, (i + 3))]
      # tmp_df <- select(out_df, date_time, sync_vec_state$state[i])
      data_anomaly = AnomalyDetectionTs(tmp_df, max_anoms = 0.006, alpha = .001, direction = "pos", plot = TRUE, e_value = F, title = sync_vec_state$state[i])
      print(data_anomaly)
      dev.copy2pdf(file = paste0("anomaly", sync_vec_state$state[i], ".pdf"), width = 7, height = 5)
      out <- data_anomaly$anoms
      out$hour <- hour(out$timestamp)
      out$yday <- yday(out$timestamp)
      anomaly_ls[[i]] <- out
}

names(anomaly_ls) <- names(out_df)[4:50]

# saveRDS(anomaly_ls, "anomaly_ls.RDS")

# Messing around to improve sync chats

anomaly_ls <- readRDS("anomaly_ls.RDS")
anomaly_df <- plyr::ldply(anomaly_ls)

# Messing around with id of sync chats

sync_tags <- sync_vec_tmp$state

tmp_bool <- names(anomaly_ls) %in% sync_tags

str(anomaly_ls[tmp_bool])
str(anomaly_ls[!tmp_bool])

anomaly_ls_sync <- plyr::ldply(anomaly_ls[tmp_bool])

str(anomaly_ls_sync)
View(anomaly_ls_sync)

write.csv(anomaly_ls_sync, "anomaly_df_sync.csv")

anomaly_ls_async <- plyr::ldply(anomaly_ls[!tmp_bool])

View(anomaly_ls_sync)
View(anomaly_ls_async)

str(anomaly_ls_async)

View(out_df)

tmp_bool <- names(out_df) %in% sync_tags

x <- out_df[, tmp_bool]
y <- out_df[, tmp_bool]

for (i in 1:ncol(x)){
      print(i)
      plot(out_df[, (i + 3)])
}

names(out_ls)
sync_vec_state$state
# -------------------
# 6. Making merged summary data for time (sync_chat versus not) - need to add additional hours
# -------------------

# For chat hashtags 

# sync_chat_bin <- select(sync_chat,
#                         state, sync_chat)
# 
# sync_chat_bin <- filter(sync_chat_bin,
#                         sync_chat == 1)
# 
# tmp_bool <- names(table(all_data_ss$state)) %in% sync_chat_bin$state

chat_data_sync_ls <- list()
chat_data_async_ls <- list()

str(anomaly_ls)

for (i in 1:length(sync_vec_state$state)){
      tmp_df <- filter(all_data_ss, state == sync_vec_state$state[i])
      anom <- anomaly_ls[[i]]
      
      data_sync_ls <- list()
      less_tmp_bool <- rep(F, nrow(tmp_df))
      
      for (j in 1:nrow(anom)){
            tmp_bool <- tmp_df$yday == anom$yday[j] & tmp_df$Hour == anom$hour[j]
            data_sync_ls[[j]] <- tmp_df[tmp_bool, ]
            less_tmp_bool <- less_tmp_bool + tmp_bool
      }

      chat_data_sync_ls[[i]] <- plyr::ldply(data_sync_ls)# saves sync data to list
      chat_data_async_ls[[i]] <- tmp_df[!less_tmp_bool, ] # saves async data to list
      print(paste0("Processed ", sync_vec_state$state[i]))
}

chat_data_sync <- plyr::ldply(chat_data_sync_ls)
chat_data_async <- plyr::ldply(chat_data_async_ls)

# For no chat hashtags

no_chat_data_async_ls <- list()

for (i in 1:length(async_vec_state$state)){
      no_chat_data_async_ls[[i]] <- filter(all_data_ss, state == async_vec_state$state[i])
      print(paste0("Processed ", async_vec_state$state[i]))
}

no_chat_data_async <- plyr::ldply(no_chat_data_async_ls)


# Number of sync hours versus async hours

anomaly_df <- plyr::ldply(anomaly_ls)

nrow(chat_data_sync) / nrow(anomaly_df) # 105.76 tweets per hour
nrow(chat_data_async) / ((24 * 182 * 30) - nrow(anomaly_df)) # 2.41 tweets per hour
nrow(no_chat_data_async) / (24 * 182 * 17) # 1.39 tweets per hour

# Creating summary statistics - with all three groups

chat_data_sync <- mutate(chat_data_sync,
                         group = "chat_data_sync",
                         count = 1)

# chat_data_sync <- left_join(chat_data_sync, chat_data_sync_users, by = "user")

chat_data_async <- mutate(chat_data_async,
                          group = "chat_data_async",
                          count = 1)

# chat_data_async <- left_join(chat_data_async, chat_data_async_users, by = "user")

no_chat_data_async <- mutate(no_chat_data_async,
                             group = "no_chat_data_async",
                             count = 1)

chat_sync_samp <- sample_n(chat_data_sync, 100)
chat_async_samp <- sample_n(chat_data_async, 100)
no_chat_async_samp <- sample_n(no_chat_data_async, 100)

chat_sync_samp <- select(chat_sync_samp, hashtag, NewTime, Tweet.Text, group)
chat_async_samp <- select(chat_async_samp, hashtag, NewTime, Tweet.Text, group)
no_chat_async_samp <- select(no_chat_async_samp, hashtag, NewTime, Tweet.Text, group)

all_samp <- rbind(chat_sync_samp, chat_async_samp, no_chat_async_samp)
write.csv(all_samp, "all_samp.csv")

# no_chat_data_async <- left_join(no_chat_data_async, chat_data_sync_users, by = "user")

# merging

all_time_data <- bind_rows(chat_data_sync, chat_data_async, no_chat_data_async)

res <- all_time_data %>%
           group_by(group) %>%
           summarize(N = length(user),
                 Voices = length(unique(user)),
                 AveChars = mean(CharCount, na.rm = T),
                 AveWords = mean(WordCount, na.rm = T),
                 AveHashTags = mean(HashTagsNum, na.rm = T),
                 AveMentions = mean(Mentions, na.rm = T),
                 AveLinks = mean(Links, na.rm = T),
                 AveFollowers = mean(Followers, na.rm = T),
                 AveFollows = mean(Follows, na.rm = T),
                 AveRetweets = mean(Retweets, na.rm = T),
                 AveFavorites = mean(Favorites, na.rm = T),
                 AveReplies = mean(reply, na.rm = T),
#                  AveMentionsNoRetweets = mean(mentions_no_retweets, na.rm = T),
                 AveRT = mean(rt, na.rm = T))

res

View(res)


# Stats tests

all_time_data_ss <- select(all_time_data, CharCount, WordCount, HashTagsNum, Mentions, Links, Retweets, Favorites, reply, group)

str(all_time_data_ss)

fit <- manova(as.matrix(all_time_data_ss[, 1:ncol(all_time_data_ss) - 1]) ~ all_time_data_ss$group) # check this
summary(fit)

fit <- fit <- aov(all_time_data_ss$CharCount ~ all_time_data_ss$group)
summary(fit)
TukeyHSD(fit)

fit <- aov(all_time_data_ss$WordCount ~ all_time_data_ss$group)
summary(fit)
TukeyHSD(fit)

fit <- aov(all_time_data_ss$HashTagsNum ~ all_time_data_ss$group)
summary(fit)
TukeyHSD(fit)

fit <- aov(all_time_data_ss$Mentions ~ all_time_data_ss$group)
summary(fit)
TukeyHSD(fit)

fit <- aov(all_time_data_ss$Links ~ all_time_data_ss$group)
summary(fit)
TukeyHSD(fit)

fit <- aov(all_time_data_ss$Retweets ~ all_time_data_ss$group)
summary(fit)
TukeyHSD(fit)

fit <- aov(all_time_data_ss$Favorites ~ all_time_data_ss$group)
summary(fit)
TukeyHSD(fit)

fit <- aov(all_time_data_ss$reply ~ all_time_data_ss$group)
summary(fit)
TukeyHSD(fit)
