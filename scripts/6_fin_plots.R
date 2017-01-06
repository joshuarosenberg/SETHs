# -----------------------------------------------------------------------------------------
# 5_fin_output, takes "all_data_ss.RDS", "summary_data.RDS", "summary_data_chat.RDS", "summary_data_time.RDS", and "summary_data_time_bin.RDS"
# makes output: "activity_map.png", "adj_activity_map.png", and "users.png"
# -----------------------------------------------------------------------------------------

rm(list = ls())

# ------------------
## Maps
# ------------------
      
setwd("~/Dropbox/research/state_tweets")

library(ggplot2)
library(dplyr)
library(ggthemes)

states <- read.csv("~/dropbox/research/state_tweets/csvs/states.csv", stringsAsFactors = F)
states <- arrange(states, state)

summary_data <- readRDS("dev/summary_data_filtered.rds")
summary_data <- arrange(summary_data,
                        state)

summary_data <- select(summary_data, -Teachers)

summary_data_sn <- left_join(summary_data, states, by = "state")

summary_data_sn[48, 1] <- "WV"
summary_data_sn[48, 15] <- "West Virginia"
summary_data_sn[48, 16] <- 1

summary_data_sn[48, 1] <- "NM"
summary_data_sn[48, 15] <- "West Virginia"
summary_data_sn[48, 16] <- 1

summary_data_sn

# write file

# write.csv(summary_data_sn, "sd_sn.csv")

# load fixed file

# summary_data_sn <- read.csv("sd_sn_rev.csv", stringsAsFactors = F)

states <- map_data("state")

summary_data_sn <- mutate(summary_data_sn, 
                          `Number of Tweets Per Week (in Thousands)` = (N / (181/7)),
                          `Number of Tweets Per Week Per Teacher` = `Number of Tweets Per Week (in Thousands)` / Teachers)

nums <- summary_data_sn

names <- read.csv("csvs/names.csv", header = F, stringsAsFactors = F)
names(names) <- c("hashtag", "state")

nums <- left_join(nums, names, by = "state")
nums <- select(nums, -SETH)

str(nums)

names(nums)[14] <- "region"
names(nums)[18] <- "SETH"


choro <- left_join(nums, states, by = "region")
choro <- choro[order(choro$order), ]

cnames <- aggregate(cbind(long, lat) ~ SETH, data = choro, FUN=function(x) mean(range(x)))

cnames[22, 2] <- cnames[22, 2] + 1.45
cnames[30, 3] <- cnames[30, 3] - .5
cnames[17, 3] <- cnames[17, 3] + .3
cnames[2, 3] <- cnames[2, 3] + .5
cnames[10, 2] <- cnames[10, 2] + 1
cnames[7, 3] <- cnames[7, 3] - .2
cnames[44, 3] <- cnames[44, 3] + .2
cnames[27, 3] <- cnames[27, 3] + .3
cnames[35, 2] <- cnames[35, 2] + .67
cnames[34, 3] <- cnames[34, 3] + .25
cnames[21, 2] <- cnames[21, 2] + 1
cnames[22, 2] <- cnames[22, 2] - 1.5

# Plot uing geom_text

ggplot() +
  geom_polygon(data = choro, 
               aes(x = long, y = lat, group = region, fill = N)) +
  scale_fill_gradient(low="#deebf7", high="#3182bd") +
  theme_map() +
  labs(title="Number of Tweets per SETH (from 1-1-2015 through 6-30-2015)") +
  geom_text(data=cnames, aes(long, lat, label = SETH), size=3.5, family="Garamond") +
    theme(text=element_text(size=14, family="Garamond"))
ggsave("st_map.png")

# Plot uing geom_text adjusting for teachers

ggplot() +
      geom_polygon(data = choro, 
                   aes(x = long, y = lat, group = region, fill = N / Teachers)) +
      scale_fill_gradient(low="#deebf7", high="#3182bd") +
      theme_map() +
      labs(title="Number of Tweets per SETH Adjusted for the Number of Teachers in the Associated State (from 1-1-2015 through 6-30-2015)") +
      geom_text(data=cnames, aes(long, lat, label = SETH), size=3.5, family="Garamond") +
      theme(text=element_text(size=14, family="Garamond"))
ggsave("st_map_adj.png")

-----------------------------
# Plots
-----------------------------
    
    # Teachers
  p1 <- qplot(long, lat, data = choro, group = region, fill = Teachers, 
                geom = "polygon", main = "Teachers", xlab = NULL, ylab = NULL)
  p1 <- p1 + scale_fill_gradient(low="gray85", high="orangered4")
  p1
  ggsave("plots/maps_teachers.pdf")
  
# N
p2 <- qplot(long, lat, data = choro, group = region, fill = N, geom = "polygon", xlab = NULL, ylab = NULL)
p2 <- p2 + scale_fill_gradient(low="gray85", high="orangered4", breaks = c(750, 1500, 2250)) + theme(legend.position="bottom")
p2
ggsave("maps_N.png")
  
  # Activity
  p3 <- qplot(long, lat, data = choro, group = region, fill = `Number of Tweets Per Week Per Teacher`, geom = "polygon", 
              main = "Activity", xlab = NULL, ylab = NULL)
  p3 <- p3 + scale_fill_gradient(low="gray85", high="orangered4") + theme(legend.position="bottom")
  p3
  ggsave("maps_Activity.png")
  
  # Voices
  p4 <- qplot(long, lat, data = choro, group = region, fill = Voices, geom = "polygon", 
              main = "Voices", xlab = NULL, ylab = NULL)
  p4 <- p4 + scale_fill_gradient(low="gray85", high="orangered4")
  p4
  ggsave("plots/maps_Voices.pdf")
  
  # Voices Per Total Teachers
  p5 <- qplot(long, lat, data = choro, group = region, fill = VoicesPer, geom = "polygon", 
              main = "Voices Per", xlab = NULL, ylab = NULL)
  p5 <- p5 + scale_fill_gradient(low="gray85", high="orangered4")
  p5
  
  # Retweets
  p6 <- qplot(long, lat, data = choro, group = region, fill = AveRetweets, geom = "polygon", 
              main = "Ave Retweets", xlab = NULL, ylab = NULL)
  p6 <- p6 + scale_fill_gradient(low="gray85", high="orangered4")
  p6
  
  # Favorites
  p7 <- qplot(long, lat, data = choro, group = region, fill = AveFavorites, geom = "polygon",
              main = "Ave Favorites", xlab = NULL, ylab = NULL)
  p7 <- p7 + scale_fill_gradient(low="gray85", high="orangered4")
  p7
  
  # Mentions
  p8 <- qplot(long, lat, data = choro, group = region, fill = AveMentions, geom = "polygon", 
              main = "Ave Mentions", xlab = NULL, ylab = NULL)
  p8 <- p8 + scale_fill_gradient(low="gray85", high="orangered4")
  p8
  
  # NGSS adopt
  p9 <- qplot(long, lat, data = choro, group = region, fill = ngss_adopt, geom = "polygon", 
              main = "NGSS adopt", xlab = NULL, ylab = NULL)
  p9 <- p9 + scale_fill_gradient(low="gray85", high="orangered4")
  p9
  
  ----------------------------------
    #Start a new search here
    ----------------------------------
    
    # myTDM <- TermDocumentMatrix(myCorpus)
    
  my_term <- "ngss"
  
  myTDM_freq <- lapply(seq_len(ncol(myTDM)), function(x) myTDM[,x]) #change this based on TDM above
  
  d <- data.frame()
  for (i in 1:length(myTDM_freq)){
    z <- inspect(myTDM_freq[[i]][my_term, dimnames(myTDM_freq[[i]])$Docs])
    d <- rbind(d, z[[1]])
  }
  
  d <- unlist(d)
  # names(d) <- SummaryData1$StateNames
  num_words <- colSums(as.matrix(myTDM))
  d <- append(d,c(0,0,0, 0))
  num_words <- append(num_words, c(.01,.01,.01, .01))
  my_plot <- (d/num_words)
  
  # Preparing data for maps
  str(my_plot)
  SummaryData2 <- cbind(SummaryData1, my_plot)
  
  states <- map_data("state")
  nums <- data.frame(SummaryData2[,], row.names = SummaryData2[,17])
  colnames(nums) <- colnames(SummaryData2)
  nums$region <- SummaryData2[,17]
  
  choro <- data.frame()
  str(states)
  choro <- merge(states, nums, sort = FALSE, by = "region")
  choro <- choro[order(choro$order),]
  
  # my_plot
  p_my_plot <- qplot(long, lat, data = choro, group = region, fill = my_plot, geom = "polygon", 
                     main = my_term, xlab = NULL, ylab = NULL)
  p_my_plot <- p_my_plot + scale_fill_gradient(low="gray85", high="orangered4")
  p_my_plot
  