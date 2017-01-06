setwd("~/Google Drive/1_Research/SETHs")

df <- readr::read_csv("all_data.csv")

df_ss <- dplyr::distinct(df)

table(df_ss$state)
str(df_ss)
