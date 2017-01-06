# For time

all_liwc <- readr::read_csv("~/google drive/research/state_tweets/all_data_wrt_time_results.csv")

str(all_liwc)

sum(table(all_liwc$`Source (AB)`))

# For chat
# 
# all_liwc <- readr::read_csv("~/google drive/research/state_tweets/all_data_wrt_chat_results.csv")
# 
# str(all_liwc)

# all_data_wrt_chat_results_group <- group_by(all_data_wrt_chat_results, sync_chat)

all_liwc_ss <- select(all_liwc,
                       # all
                       affect, social, cogproc, percept, drives, informal, AllPunc,
                       # affect
                       posemo, negemo,
                       # social
                       family, friend, female, male,
                       # cog proc
                       insight, cause, discrep, tentat, certain, differ,
#                        # percept
#                        see, hear, feel,
#                        # bio
#                        body, health, sexual, ingest,
                       # drives
                       affiliation, achieve, power, reward, risk,
                       # time orient - need to make
                       focuspast, focuspresent, focusfuture,
#                        #relativ - need to make
#                        motion, space, time,
                       #persconc
                       work, leisure, home, money, relig, death,
                       #informal
                       swear, netspeak, assent, nonflu, filler,

                       #meta
                       group = `Source (AB)`)

all_liwc_ss <- mutate(all_liwc_ss,
                      timeorient = focuspast + focuspresent + focusfuture,
                      persconc = work + leisure + home + money + relig + death)

str(all_liwc_ss)

all_liwc_ss_grouped <- group_by(all_liwc_ss, group)

all_liwc_ss_grouped

out_df <- summarize_each(all_liwc_ss_grouped, funs(mean))

write.csv(out_df, "liwc_results_time.csv")

# Other

# out_df <- data.frame(var = names(all_liwc_ss)[2:42], out = rep(0, 41))
# 
# t.test(all_liwc_ss[, 2] ~ all_liwc_ss[, 1])
# 
# x <- t.test(all_liwc_ss$posemo ~ all_liwc_ss$group)
# 
# for (i in 2:42){
#       print(paste0("For ", names(all_liwc_ss)[i]))
#       x <- (t.test(unlist(all_liwc_ss[, i]) ~ unlist(all_liwc_ss[, 1])))
#       out_df[i, 2] <- x$p.value
# }
# 
# liwc_grouped <- group_by(all_liwc_ss, group)
# 
# liwc_summary <- summarize_each(liwc_grouped, funs(mean))
# 
# liwc_sum_out <- cbind(t(liwc_summary), as.numeric(round(out_df[, 2], 4)))
# 
# write.csv(liwc_sum_out, "liwc_sum_out.csv")
# 
# 
# View(t(liwc_summary))
# 
# ?summarize_each
# 
# res <- sync_liwc_ss %>% 
#             group_by(group) %>%
#             summarize_each(mean)
# 
# attributes(sync_liwc_ss)
# 
# rowSums(sync_liwc_ss[1:100, ])
# 
# async_liwc <- read.csv()