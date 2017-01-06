# -----------------------------------------------------------------------------------------
# 3_fin_network, takes "all_data_ss.RDS", makes "sna_out.RDS"
# -----------------------------------------------------------------------------------------

setwd("~/dropbox/research/state_tweets/") # Josh

library(igraph)

all_data_ss <- readRDS("dev/all_data_ss.RDS")
summary_data <- readRDS("dev/summary_data.RDS")

# -------------------
# 3. social network analysis # need to make sure this makes sense - and add
# -------------------

x <- strsplit(all_data_ss$URL, "/")
all_data_ss$user <- sapply(x, function(x) x[[4]]) # extracts users from URL
rm(x) # make this all into a one-liner

# Create output df

output_df <- data.frame(row.names = summary_data$state, density = rep(0, 47), 
                        reciprocity = rep(0, 47), length = rep(0, 47), mean_distance = rep(0, 47), 
                        diameter = rep(0, 47), mean_degree = rep(0, 47))

output_ls <- list()

# Iterate by state

for (i in 1:nrow(summary_data)){

  temp_df <- all_data_ss[all_data_ss$state == summary_data$state[i], ]
  
  print(paste0("Processing tweets from ", summary_data$state[i], " (", i, "/", nrow(summary_data), ")"))
  
  edges <- c()
  
  for (j in 1:length(temp_df$Tweet.Text)) {
    # get a list of accounts mentioned for each tweet and extract any string that matches a username
    mentions = unlist(str_extract_all(temp_df$Tweet.Text[j],"@[a-z0-9_]{2,15}"))
    if (length(mentions)!=0) {
      for (k in 1:length(mentions)) {
        if(temp_df$user[j]!="" && substring(mentions[k],2)!="") { #needed for when parser gets unhappy
          # add the tweeter and the mentionee to the edge list
          edges = c(edges, c(tolower(temp_df$user[j]), substring(mentions[k],2)))
        }
      }
    }
  }
  
  # turn the edgelist into a matrix
  edgematrix <- as.matrix(t(matrix(edges, nrow=2)))

  # turn the matrix into a graph
  g <- graph.edgelist(edgematrix)
  
  # simplify
  
  E(g)$weight <- 1
  
  g <- simplify(g, edge.attr.comb = list(weight = "sum"), remove.loops = T) # can change this to remove weights
  
  g <- simplify(g, remove.multiple = T) # can change this to remove weights
  
  g <- as.undirected(g)
  
  # saving output
  output_ls[[i]] <- g
  
  # calculate statistics
  output_df[i, 1] <- graph.density(g)
  output_df[i, 2] <- reciprocity(g)
  output_df[i, 3] <- length(E(g))
  output_df[i, 4] <- average.path.length(g, directed= F, unconnected= T)
  output_df[i, 5] <- diameter(g)
  output_df[i, 6] <- mean(degree(g, mode = "all"))
}

output_df <- cbind(output_df, state = row.names(output_df))

sna_out <- arrange(output_df, state)

saveRDS(sna_out, "sna_out.RDS")

rm(list = ls())
