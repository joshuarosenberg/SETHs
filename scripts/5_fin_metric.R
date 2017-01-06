# -----------------------------------------------------------------------------------------
# 5_fin_metric, takes "all_data_ss.RDS", all_data_chat.RDS", and "all_data_time.RDS"
# makes: "sustainedUserCount.RDS", "sustainedUserCount_chat.RDS", and "sustainedUserCount_time.RDS"
# also makes: "chat_data_sync_users.RDS", "chat_data_async_users.RDS", and "no_chat_data_async_users.RDS" # need to be made before parts of analysis
# -----------------------------------------------------------------------------------------
 
setwd("~/Dropbox/research/state_tweets")

rm(list = ls())

data <- readRDS("dev/all_data_ss.rds") # get this from mjk_optimized

library(lubridate)
library(dplyr)

# preparing variables

data$user <- sapply(data$URL, function(x) # may need to change to lowercase
      strsplit(x, "/")[[1]][4])

data$month <- month(data$NewTime)

# with dplyr for all
 
data_ss <- mutate(data, count = 1)
data_ss_grouped <- group_by(data_ss, user)

data_ss_grouped <- mutate(data_ss_grouped,
                  month_1 = ifelse(sum(month == 1) >= 1, sum(month = 1), 0),
                  month_2 = ifelse(sum(month == 2) >= 1, sum(month = 2), 0),
                  month_3 = ifelse(sum(month == 3) >= 1, sum(month = 3), 0),
                  month_4 = ifelse(sum(month == 4) >= 1, sum(month = 4), 0),
                  month_5 = ifelse(sum(month == 5) >= 1, sum(month = 5), 0),
                  month_6 = ifelse(sum(month == 6) >= 1, sum(month = 6), 0))

res <- summarize(data_ss_grouped,
                 month_1_sum = sum(month_1),
                 month_2_sum = sum(month_2),
                 month_3_sum = sum(month_3),
                 month_4_sum = sum(month_4),
                 month_5_sum = sum(month_5),
                 month_6_sum = sum(month_6),
                 
                 month_1_bin = month_1_sum >= 1,
                 month_2_bin = month_2_sum >= 1,
                 month_3_bin = month_3_sum >= 1,
                 month_4_bin = month_4_sum >= 1,
                 month_5_bin = month_5_sum >= 1,
                 month_6_bin = month_6_sum >= 1)

res

res_bin <- select(res, contains("bin"))

table(rowSums(res_bin))

# For sync

rm(list = ls())

data <- readr::read_csv("~/google drive/research/state_tweets/all_data_wrt_time.csv")

data$user <- sapply(data$URL, function(x)
      strsplit(x, "/")[[1]][4])

data$month <- month(data$NewTime)

table(data$group)

chat_data_sync <- data[data$group == "chat_data_sync", ]
chat_data_async <- data[data$group == "chat_data_async", ]
no_chat_data_async <- data[data$group == "no_chat_data_async", ]

# chat data sync

data_ss <- mutate(chat_data_sync, count = 1)
data_ss_grouped <- group_by(data_ss, user)

data_ss_grouped <- mutate(
      data_ss_grouped,
      month_1 = ifelse(sum(month == 1) >= 1, sum(month = 1), 0),
      month_2 = ifelse(sum(month == 2) >= 1, sum(month = 2), 0),
      month_3 = ifelse(sum(month == 3) >= 1, sum(month = 3), 0),
      month_4 = ifelse(sum(month == 4) >= 1, sum(month = 4), 0),
      month_5 = ifelse(sum(month == 5) >= 1, sum(month = 5), 0),
      month_6 = ifelse(sum(month == 6) >= 1, sum(month = 6), 0)
)

res <- summarize(
      data_ss_grouped,
      month_1_sum = sum(month_1),
      month_2_sum = sum(month_2),
      month_3_sum = sum(month_3),
      month_4_sum = sum(month_4),
      month_5_sum = sum(month_5),
      month_6_sum = sum(month_6),
      
      month_1_bin = month_1_sum >= 1,
      month_2_bin = month_2_sum >= 1,
      month_3_bin = month_3_sum >= 1,
      month_4_bin = month_4_sum >= 1,
      month_5_bin = month_5_sum >= 1,
      month_6_bin = month_6_sum >= 1,

      n_sustained = sum(month_1_bin + month_2_bin + month_3_bin + month_4_bin + month_5_bin + month_6_bin),
      n_sustained_bin = sum(month_1_bin + month_2_bin + month_3_bin + month_4_bin + month_5_bin + month_6_bin) == 6
)

chat_data_sync_users <- select(res, user, n_sustained, n_sustained_bin)

res_bin <- select(res, contains("bin"))

chat_data_sync_res <- table(rowSums(res_bin))

# chat data async

data_ss <- mutate(chat_data_async, count = 1)
data_ss_grouped <- group_by(data_ss, user)

data_ss_grouped <- mutate(
      data_ss_grouped,
      month_1 = ifelse(sum(month == 1) >= 1, sum(month = 1), 0),
      month_2 = ifelse(sum(month == 2) >= 1, sum(month = 2), 0),
      month_3 = ifelse(sum(month == 3) >= 1, sum(month = 3), 0),
      month_4 = ifelse(sum(month == 4) >= 1, sum(month = 4), 0),
      month_5 = ifelse(sum(month == 5) >= 1, sum(month = 5), 0),
      month_6 = ifelse(sum(month == 6) >= 1, sum(month = 6), 0)
)

res <- summarize(
      data_ss_grouped,
      month_1_sum = sum(month_1),
      month_2_sum = sum(month_2),
      month_3_sum = sum(month_3),
      month_4_sum = sum(month_4),
      month_5_sum = sum(month_5),
      month_6_sum = sum(month_6),
      
      month_1_bin = month_1_sum >= 1,
      month_2_bin = month_2_sum >= 1,
      month_3_bin = month_3_sum >= 1,
      month_4_bin = month_4_sum >= 1,
      month_5_bin = month_5_sum >= 1,
      month_6_bin = month_6_sum >= 1,
      
      n_sustained = sum(month_1_bin + month_2_bin + month_3_bin + month_4_bin + month_5_bin + month_6_bin),
      n_sustained_bin = sum(month_1_bin + month_2_bin + month_3_bin + month_4_bin + month_5_bin + month_6_bin) == 6
)

chat_data_async_users <- select(res, user, n_sustained, n_sustained_bin)

res_bin <- select(res, contains("bin"))

chat_data_async_res <- table(rowSums(res_bin))

# no chat data async

data_ss <- mutate(no_chat_data_async, count = 1)
data_ss_grouped <- group_by(data_ss, user)

data_ss_grouped <- mutate(
      data_ss_grouped,
      month_1 = ifelse(sum(month == 1) >= 1, sum(month = 1), 0),
      month_2 = ifelse(sum(month == 2) >= 1, sum(month = 2), 0),
      month_3 = ifelse(sum(month == 3) >= 1, sum(month = 3), 0),
      month_4 = ifelse(sum(month == 4) >= 1, sum(month = 4), 0),
      month_5 = ifelse(sum(month == 5) >= 1, sum(month = 5), 0),
      month_6 = ifelse(sum(month == 6) >= 1, sum(month = 6), 0)
)

res <- summarize(
      data_ss_grouped,
      month_1_sum = sum(month_1),
      month_2_sum = sum(month_2),
      month_3_sum = sum(month_3),
      month_4_sum = sum(month_4),
      month_5_sum = sum(month_5),
      month_6_sum = sum(month_6),
      
      month_1_bin = month_1_sum >= 1,
      month_2_bin = month_2_sum >= 1,
      month_3_bin = month_3_sum >= 1,
      month_4_bin = month_4_sum >= 1,
      month_5_bin = month_5_sum >= 1,
      month_6_bin = month_6_sum >= 1,
      
      n_sustained = sum(month_1_bin + month_2_bin + month_3_bin + month_4_bin + month_5_bin + month_6_bin),
      n_sustained_bin = sum(month_1_bin + month_2_bin + month_3_bin + month_4_bin + month_5_bin + month_6_bin) == 6
)

no_chat_data_async_users <- select(res, user, n_sustained, n_sustained_bin)

res_bin <- select(res, contains("bin"))

no_chat_data_async_res <- table(rowSums(res_bin))

# Overall res

chat_data_sync_res
chat_data_sync_res / sum(chat_data_sync_res)

chat_data_async_res
chat_data_async_res / sum(chat_data_async_res)

no_chat_data_async_res
no_chat_data_async_res / sum(no_chat_data_async_res)

# Summaries

saveRDS(chat_data_sync_users, "chat_data_sync_users.RDS")
saveRDS(chat_data_async_users, "chat_data_async_users.RDS")
saveRDS(no_chat_data_async_users, "no_chat_data_async_users.RDS")