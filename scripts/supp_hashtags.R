

library(stringr)

return_frequent_hashtags <- function(strList) {
      require(dplyr)
      BigString <- tolower(paste(strList, collapse=""))
      BigString <- sort(strsplit(BigString, " ")[[1]])
      y=unique(BigString)
      len <- length(BigString)
      df <- data.frame(x=BigString)
      copies <- group_size(group_by(df,x))
      df2 <- data.frame(x=copies,y=y)
      df3 <- df2[order(df2$x,decreasing=TRUE),]
      top10 <- paste0(df3$y,"(")
      top10 <- paste0(top10,df3$x)
      top10 <- paste0(top10,")")
      top10
}

out_ls <- list()
tbl_names <- names(table(all_data_ss$hashtag))

?glm

tbl_names[i]


?exp
?glm

for (i in 1:length(tbl_names)){
      
      temp_all_data <- filter(all_data_ss,
                              hashtag == tbl_names[i])
      
      temp_output <- return_frequent_hashtags(temp_all_data$HashTags)
      
      temp_tag <- sapply(temp_output, function(x) str_split(x, "\\("))
      tag <- sapply(temp_tag, function(x) x[1])
      names(tag) <- NULL
      
      temp_freq <- sapply(temp_tag, function(x) str_split(x[2], "\\)"))
      weight <- sapply(temp_freq, function(x) x[1])
      
      names(weight) <- NULL
      
      temp_out <- data.frame(tag = tbl_names[i], other_tag = tag, weight = weight, stringsAsFactors = F)
      
      temp_out$weight<- as.numeric(temp_out$weight) / 2
      
      out_ls[[i]] <- temp_out[temp_out$weight > 10 & temp_out$other_tag %in% tbl_names, ] # can change this to filter more tags
      
      print(paste0("Processing hashtag ", i))
      
}

names(out_ls) <- tbl_names
out_df <- plyr::ldply(out_ls)
out_df <- select(out_df, -.id)

# Plotting graph

g <- graph.data.frame(out_df, directed = F)
g.simple <- simplify(g, remove.multiple = T, remove.loops = T)

# Can use this to plot

g.simplest <- g.simple

plot(g.simplest, edge.width = E(g.simple)$weight / 60, vertex.size = 5)

# More complex plot

fg <- fastgreedy.community(g.simple)

sort(membership(fg))

#graph layout: can take a while
l <- layout.fruchterman.reingold(g.simple)

E(g.simple)$weight

plot(g.simple, vertex.color=fg$membership+1,vertex.frame.color=fg$membership+1,
     vertex.size=100*pr$vector, vertex.label.dist=0.1, E(g.simple)$weight)

# plotting a simple ring graph, all default parameters, except the layout
g <- make_ring(10)
g$layout <- layout_in_circle
plot(g)
tkplot(g)
rglplot(g)

# plotting a random graph, set the parameters in the command arguments
g <- barabasi.game(100)
plot(g, layout=layout_with_fr, vertex.size=4,
     vertex.label.dist=0.5, vertex.color="red", edge.arrow.size=0.5)

# plot a random graph, different color for each component
g <- sample_gnp(100, 1/100)
comps <- components(g)$membership
colbar <- rainbow(max(comps)+1)
V(g)$color <- colbar[comps+1]
plot(g, layout=layout_with_fr, vertex.size=5, vertex.label=NA)

# plot communities in a graph
g <- make_full_graph(5) %du% make_full_graph(5) %du% make_full_graph(5)
g <- add_edges(g, c(1,6, 1,11, 6,11))
com <- cluster_spinglass(g, spins=5)
V(g)$color <- com$membership+1
g <- set_graph_attr(g, "layout", layout_with_kk(g))
plot(g, vertex.label.dist=1.5)

# draw a bunch of trees, fix layout
igraph_options(plot.layout=layout_as_tree)
plot(make_tree(20, 2))
plot(make_tree(50, 3), vertex.size=3, vertex.label=NA)
tkplot(make_tree(50, 2, mode="undirected"), vertex.size=10,
       vertex.color="green")

## End(Not run)
