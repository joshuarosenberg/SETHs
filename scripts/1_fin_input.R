# -----------------------------------------------------------------------------------------
# 1_fin_input, takes raw data, makes "all_data.RDS"
# -----------------------------------------------------------------------------------------

source("~/dropbox/research/state_tweets/scripts/MJK_Optimized_1.R")

# MN and MO are dups - need to fix

#Enabling packages

require(RCurl)
require(ggplot2)
require(PerformanceAnalytics)
require(dplyr)
require(plyr)
require(png)
require(grid)
require(gplots)
require(gridExtra)
require(MASS)
require(car)
# require(lmSupport)
require(lubridate)

options(stringsAsFactors = FALSE)

# -----------------------------------------------------------------------------------------
#                               GLOBAL VARIABLES
# -----------------------------------------------------------------------------------------

# g_base_folder <- "/Users/mkoehler/dropbox/state_tweets/" # Matt
g_base_folder <- "~/dropbox/research/state_tweets/" # Josh
g_raw_data_folder <- paste0(g_base_folder,"raw_data/")
g_image_folder <- paste0(g_base_folder,"images/")

# From "https://docs.google.com/spreadsheets/d/1H5knIVjI3NF_bHHAA7_DXk67I8uJu18Cjt0Crt1oQUQ/pub?output=csv"
g_master_map_file <- paste0(g_base_folder,"master_map.txt")
g_all_data_file <- paste0(g_base_folder,"all_data.txt") # changed to all_data_ss for filtered data
g_summary_file <- paste0(g_base_folder,"summary_data.txt")

# -----------------------------------------------------------------------------------------
#                    PULL DATA FROM WEB
# -----------------------------------------------------------------------------------------

pull_data_from_web <- function () {
	cat("----------------------------------------------------------------------\n")
	cat("----                 PULLING DATA FROM WEB                       -----\n")
	cat("----------------------------------------------------------------------\n\n")
	
	start.time <- proc.time()

	file_mappings <- read.csv(g_master_map_file, header = TRUE, stringsAsFactors = FALSE)

	n <- nrow(file_mappings)
	
	cat(paste0("States to process: ", n,"\n"))
	
	for (i in 1:n) {
		
		one_file_name <- paste0(g_raw_data_folder,"file_",i,".txt")
		
		internet_url <- getURL(file_mappings$url[[i]], followlocation = T)
		
		one_file <- read.csv(textConnection(internet_url), header=TRUE, stringsAsFactors = FALSE,skip=1)
		
		one_file$hashtag <- file_mappings$name[[i]]
		one_file$correction <- file_mappings$correction[[i]]
		one_file$teachers <- file_mappings$teachers[[i]]
		one_file$state <- file_mappings$state[[i]]

		cat(paste0("   ===> Writing file for ", file_mappings$state[[i]],"\n"))
		write.csv(one_file, one_file_name, row.names=FALSE,  na="")	
		
	}
	
	end.time <- proc.time()
	time.taken <- end.time - start.time
	time.taken <- round(time.taken[3]/60,2)
	cat(paste0("\nTime Taken: ",time.taken, " minutes \n\n"))

}

# -----------------------------------------------------------------------------------------
#     WRITE ALL DATA
#			Writes our Master Data Frame to a file for later use
#
# -----------------------------------------------------------------------------------------


write_all_data <- function (df) {

	cat("----------------------------------------------------------------------\n")
	cat("----                WRITING MASTER DATA FILE                     -----\n")
	cat("----------------------------------------------------------------------\n\n")

	start.time <- proc.time()
	
	cat(paste0("Writing file: ", g_all_data_file,"\n\n"))
	write.csv(df, g_all_data_file, row.names=FALSE,  na="")	
	
	end.time <- proc.time()
	time.taken <- end.time - start.time
	time.taken <- round(time.taken[3]/60,2)
	cat(paste0("\nTime Taken: ",time.taken, " minutes \n\n"))

}



# -----------------------------------------------------------------------------------------
#    READ ALL DATA
#			Reads our Master Data Frame
#
# -----------------------------------------------------------------------------------------

pull_all_data_from_cache <- function () {


	cat("----------------------------------------------------------------------\n")
	cat("----                READING MASTER DATA FILE                     -----\n")
	cat("----------------------------------------------------------------------\n\n")

	start.time <- proc.time()
	
	cat(paste0("Reading file: ", g_all_data_file,"\n\n"))
	
	df <- read.csv(g_all_data_file, header=TRUE, stringsAsFactors = FALSE)
	
	df <- df[df$Tweet.Text != "",]

	end.time <- proc.time()
	time.taken <- end.time - start.time
	time.taken <- round(time.taken[3]/60,2)
	cat(paste0("\nTime Taken: ",time.taken, " minutes \n\n"))

	df
}



# -----------------------------------------------------------------------------------------
#                    READ CACHED DATA
#						set subset = TRUE if you only want a small portion of the data
#                                         for testing purposes
# -----------------------------------------------------------------------------------------

read_cached_raw_data <- function (subset = TRUE) {
	
	cat("----------------------------------------------------------------------\n")
	cat("----                    READING CACHED DATA                      -----\n")
	cat("----------------------------------------------------------------------\n\n")

	start.time <- proc.time()
	
	raw_file_list <- lapply(list.files(g_raw_data_folder),function(x,y) paste0(y,x), y=g_raw_data_folder)
		
	cat(paste0("Merging all cached data files ...  num files: ", length(raw_file_list), "\n"))
	
	df <- ldply(raw_file_list, read.csv, header=TRUE, na.string=c("", "null", "NaN", "X"), stringsAsFactors = FALSE)

	if (subset) {
		 df <- df[sample(nrow(df)),]
		 df <- df[1:5000,]
		 df
	}

	df <- df[df$Tweet.Text != "",]
	
	end.time <- proc.time()
	time.taken <- end.time - start.time
	time.taken <- round(time.taken[3]/60,2)
	cat(paste0("\nTime Taken: ",time.taken, " minutes \n\n"))
	
	df

}

# -----------------------------------------------------------------------------------------
#         GET ALL DATA
#				Three ways to get our data, determined by how method variable is set:
#                    1: Internet   -  pulls all data from the internet (slow, but recent)
#                    2: Raw Cache   -  Uses cached raw data, but computes derived data (faster,
#                                      but data may be outdated)
#                    3: Full Cache   -  Uses Full cache of raw and computed data
# -----------------------------------------------------------------------------------------


get_all_data <- function (method = 1) {
	
	if (method == 1) {		
		pull_data_from_web()	
	}
	
	if ((method == 1) || (method == 2)) {
		all_data <- read_cached_raw_data(subset = FALSE)
		all_data <- compute_additional_columns(all_data)
		write_all_data(all_data)
	}
	
	all_data <- pull_all_data_from_cache()
	
	all_data
}



# -----------------------------------------------------------------------------------------
#                    COMPUTE ADDITIONAL COLUMNS
#						Computes all derived data frame columns such as corrected time 
#                       stamp, word counts, hashtag counts, etc.
#           If date_filter = TRUE, filters tweets by before and after dates
# -----------------------------------------------------------------------------------------

compute_additional_columns <- function (df, date_filter = F) {

	cat("----------------------------------------------------------------------\n")
	cat("----               COMPUTING ADDITIONAL COLUMNS                  -----\n")
	cat("----------------------------------------------------------------------\n")

	cat("")
	
	start.time <- proc.time()

	
	cat(paste0("Total Tweets to process: ", nrow(df), "\n"))
	
	df_rows <- nrow(df)
	
	cat("   ===> Adding column: TimeStamp \n")
	
	#make a new column, TimeStamp, with the number of seconds since 1970-01-01 of the tweet, this has been timezone corrected, and is of type POSIXct
	df$TimeStamp <- unlist(mapply(get_corrected_time_stamp2, df$Date, df$correction)) # need to look at this - make sure it's correct
	
	cat("   ===> Adding column: NewTime \n")
	
	#make a new column, NewTime, which is the time corrected Readable String of the post
	df$NewTime <- unlist(lapply(df$TimeStamp, FUN=function(y){paste(as.POSIXct(y, origin = "1970-01-01"))}))
  
	# filters tweets by dates
	
	if (date_filter){
	  
	  cat("   *** Filters tweets by date \n")
	  
	  df$NewTime <- parse_date_time(df$NewTime, "ymd_hms")
	  
	  after <- parse_date_time("2015-01-01 00:00:00", "ymd_hms") # is this working? saw tweet on am of 7/1
	  before <- parse_date_time("2015-06-30 23:59:59", "ymd_hms")
	  
	  df <- filter(df, 
	               NewTime >= after,
	               NewTime <= before) 
	  
	  df$NewTime <- as.character(df$NewTime)
	  
	  cat(paste("   *** Filtered ", df_rows - nrow(df), " rows", "\n", sep = ""))
	  
	}
	
	cat("   ===> Adding column: NewDate \n")

	#make a new column, NewDate, which is the time corrected Date
	df$NewDate <- unlist(lapply(df$TimeStamp, FUN=function(y){paste(as.Date(as.POSIXct(y, origin = "1970-01-01")))}))
	
	cat("   ===> Adding column: Day \n")

	#make a new column, Day, which is the human readable day of the week (e.g.Monday, Tuesday, etc.)
	df$Day <- unlist(lapply(df$TimeStamp, get_day_of_week)) # the day seems to be recorded incorrectly if the hour is > 19:30
	
	cat("   ===> Adding column: Hour \n")

	#make a new column, Hour, which is the hour of the day the tweet was posted (e.g.4, 13, 21)
	df$Hour <- unlist(lapply(df$TimeStamp, get_hour_of_the_day))

	cat("   ===> Adding column: CharCount \n")
	df$CharCount <- unlist(nchar(df$Tweet.Text ))
	
	cat("   ===> Adding column: WordCount \n")
	df$WordCount <- unlist(lapply(df$Tweet.Text, get_word_count))
	
	cat("   ===> Adding column: HashTags \n")	
	df$HashTags <- unlist(mapply(get_hashtags,df$Tweet.Text, df$hashtag))

	cat("   ===> Adding column: HashTagsNum \n")
	df$HashTagsNum <- unlist(lapply(df$HashTags, get_word_count))

	cat("   ===> Adding column: Links \n")
	df$Links <- unlist(lapply(df$Tweet.Text, get_num_links))

	cat("   ===> Adding column: Mentions \n")
	df$Mentions <- unlist(lapply(df$Tweet.Text, get_num_mentions))

	
	end.time <- proc.time()
	time.taken <- end.time - start.time
	time.taken <- round(time.taken[3]/60,2)
	cat(paste0("\nTime Taken: ",time.taken, " minutes \n\n"))

	df
	
}

# -----------------------------------------------------------------------------------------
#    RETURN FREQUENT HASHTAGS
#			Returns the top 10 used hashtags in the text it is sent
#           Returns a string with hashtags separated by spaces
# -----------------------------------------------------------------------------------------

return_frequent_hashtags <- function(strList) {
	

#	start.time <- proc.time()

	require(dplyr)
	
	#assuming strList is a column of a data frame 
	
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

	top10 <- paste(top10[1:10],collapse=" ")
	
#	end.time <- proc.time()
#	time.taken <- end.time - start.time
#	time.taken <- round(time.taken[3]/60,2)
#	cat(paste0("\nTime Taken: ",time.taken, " minutes \n\n"))

	top10
}


# -----------------------------------------------------------------------------------------
#    MAKE SUMMARY DATA
#		Makes summary data for each state. There are multiple ways to do this as determined
#       by the method paramater:
#			1:  Rebuild the data from scratch
#			2:  Used a cached version (stored in the g_summary_file variable)
#  
#		(1) uses the most recent data, (2) is faster.
#
#		Every time this runs, it writes the summary data back to the cache file (g_summary_file)
#
# -----------------------------------------------------------------------------------------

make_summary_data <- function (df, method=1) {

	cat("----------------------------------------------------------------------\n")
	cat("----                  COMPUTING SUMMARY DATA                     -----\n")
	cat("----------------------------------------------------------------------\n")

	cat("")
	
	start.time <- proc.time()
	
	if (method==2) {
		cat(paste0("Reading file: ", g_summary_file,"\n\n"))
		finalDF <- read.csv(g_summary_file, header=TRUE, stringsAsFactors = FALSE)
		return (finalDF)
	}
	
	finalDF <- ddply(df, df$state, summarize,
			N=length(hashtag),
			Activity=length(hashtag)/teachers[[1]],
			Teachers=teachers[[1]],
			AveChars=mean(CharCount),
			AveWords=mean(WordCount),
			AveHashTags=mean(HashTagsNum),
			AveMentions=mean(Mentions),
			AveLinks=mean(Links),
			AveFollowers=mean(Followers),
			AveRetweets=mean(Retweets),
			AveFavorites=mean(Favorites),
			Voices=length(unique(Twitter.User)),
			VoicesPer=length(unique(Twitter.User))/teachers[[1]],
			TopTags=return_frequent_hashtags(HashTags) 
			)
	
	cat(paste0("Writing file: ", g_summary_file,"\n\n"))
	write.csv(finalDF, g_summary_file, row.names=FALSE,  na="")	

	end.time <- proc.time()
	time.taken <- end.time - start.time
	time.taken <- round(time.taken[3]/60,2)
	cat(paste0("\nTime Taken: ",time.taken, " minutes \n\n"))


	finalDF

	
}

# -----------------------------------------------------------------------------------------
#         THE "GET_ FUNCTIONS"
#				These functions are utility functions that return word counts, hashtag counts,
#               corrected timestamps, etc. 
# -----------------------------------------------------------------------------------------


get_num_hashtags <- function (x) {
	
	x <- paste(x," \n")
	x <- paste(x," \n")
	n<- length(regmatches(x,gregexpr("#(\\d|\\w)+",x))[[1]]) 
	
	if (n>0) n<- n-1	
}

get_hashtags <- function (x, not_y) {

	x <- paste(x," \n")
	x <-regmatches(x,gregexpr("#(\\d|\\w)+",x))[[1]]
	
	myAns <- ""
	
	y<-length(x)
	
	if (y>0) {
	
		for(i in 1:y) {
					
			if ((tolower(x[i])) != as.character(not_y)) myAns <-paste(myAns,x[i])
		} 
		
	}
	as.character(myAns)
}



get_num_mentions <- function (x) {


length(regmatches(x,gregexpr("@(\\d|\\w)+",x))[[1]])
	
}

get_num_links <- function (x) {
	
	length(grep("http", x))
	
}


get_word_count <- function (x) {
	
	str2 <- gsub(' {2,}',' ',x)
	length(strsplit(str2,' ')[[1]])

	
	
}


get_corrected_time_stamp <- function (x, correction) {
	
		z <- strptime(x, "%m/%d/%Y %H:%M")
		z$hour <- z$hour + correction
		y <- as.POSIXct(z, tz = "GMT")
		y

}


get_corrected_time_stamp2 <- function (x, correction) {
	
		z <- strptime(x, "%m/%d/%Y %H:%M")
		correction <- (correction + 4) * 60 * 60
		y <- as.POSIXct(z, format = "%Y-%m-%d %I:%M:%S", tz = "GMT")
		y <- y + correction
		y

}


get_day_of_week <- function (x) {
	
	
	c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", 
    "Friday", "Saturday")[as.POSIXlt(x, origin = "1970-01-01")$wday + 1]
    
}

get_hour_of_the_day <- function (x) {
	
	
	as.POSIXlt(x, origin = "1970-01-01")$hour
    
}





# -----------------------------------------------------------------------------------------
#    COR.PROB
#			Correlation matrix with p-values. See http://goo.gl/nahmV 
#           for documentation of this function
# -----------------------------------------------------------------------------------------


## Correlation matrix with p-values. See http://goo.gl/nahmV for documentation of this function
cor.prob <- function (X, dfr = nrow(X) - 2) {
  R <- cor(X, use="pairwise.complete.obs")
  above <- row(R) < col(R)
  r2 <- R[above]^2
  Fstat <- r2 * dfr/(1 - r2)
  R[above] <- 1 - pf(Fstat, 1, dfr)
  R[row(R) == col(R)] <- NA
  R
}



# -----------------------------------------------------------------------------------------
#    FLATTEN SQUARE MATRIX
#         Use this to dump the cor.prob output to a 4 column matrix
#         with row/column indices, correlation, and p-value.
#         See StackOverflow question: http://goo.gl/fCUcQ
# -----------------------------------------------------------------------------------------

flattenSquareMatrix <- function(m) {
  if( (class(m) != "matrix") | (nrow(m) != ncol(m))) stop("Must be a square matrix.") 
  if(!identical(rownames(m), colnames(m))) stop("Row and column names must be equal.")
  ut <- upper.tri(m)
  data.frame(i = rownames(m)[row(m)[ut]],
             j = rownames(m)[col(m)[ut]],
             cor=t(m)[ut],
             p=m[ut])
}
 

# -----------------------------------------------------------------------------------------
#   PRINT SUMMARY DATA
#       Just dumps the summary data frame to screen
# -----------------------------------------------------------------------------------------


print_summary_data <- function (df) {


	cat("----------------------------------------------------------------------\n")
	cat("----                        SUMMARY DATA                         -----\n")
	cat("----------------------------------------------------------------------\n")

	cat("")
	

	print(df)

}


# -----------------------------------------------------------------------------------------
#    SUMMARY SE - Summarizes Data
#         Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
#         data: a data frame.
#         measurevar: the name of a column that contains the variable to be summariezed
#         groupvars: a vector containing names of columns that contain grouping variables
#         na.rm: a boolean that indicates whether to ignore NA's
#         conf.interval: the percent range of the confidence interval (default is 95%)
# -----------------------------------------------------------------------------------------

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
    require(plyr)

    # New version of length which can handle NA's: if na.rm==T, don't count them
    length2 <- function (x, na.rm=FALSE) {
        if (na.rm) sum(!is.na(x))
        else       length(x)
    }

    # This does the summary. For each group's data frame, return a vector with
    # N, mean, and sd
    datac <- ddply(data, groupvars, .drop=.drop,
      .fun = function(xx, col) {
        c(N    = length2(xx[[col]], na.rm=na.rm),
          mean = mean   (xx[[col]], na.rm=na.rm),
          sd   = sd     (xx[[col]], na.rm=na.rm)
        )
      },
      measurevar
    )

    # Rename the "mean" column    
    datac <- plyr::rename(datac, c("mean" = measurevar))

    datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean

    # Confidence interval multiplier for standard error
    # Calculate t-statistic for confidence interval: 
    # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
    ciMult <- qt(conf.interval/2 + .5, datac$N-1)
    datac$ci <- datac$se * ciMult

    return(datac)
}

# -----------------------------------------------------------------------------------------
#	MAKE BARGRAPH
#		Given a data frame and a xvariable and yvariable within that frame to use, it 
#		makes a bargraph.  Some key parameters are:
#           yvar : what to plot on the y-axis
#			xvar:  what to plot on the x-axis
#			sort_by_label : if TRUE, will sort the x-axis according to the category labels
#			sort_by_size  : if TRUE, will sort the x-axis according to the biggest values
#                              on the y-axis
#           ylabel :  what to call the y-axis
#           xlabel :  what to call the x-axis
#           Title : what to title the graph
#           type : "mean" (plots the mean of of the yvar) or "frequency" (plots the frequency
#                     count of the yvar)
#
#		Returns the graph object, but saves the graph in an image file (in the
#		g_image_folder directory)
# -----------------------------------------------------------------------------------------

make_bargraph <- function (df, yvar, xvar, Title, ylabel=yvar, xlabel=xvar, sort_by_size = FALSE, sort_by_label= TRUE, type="mean") {
	
	imgFile <- paste0(g_image_folder,paste0(yvar,"_by_",xvar,".png"))
	
	png(imgFile, width=3200, height=1600, res=240)


	dfc <- summarySE(df, measurevar= yvar, groupvars=c(xvar))
	
	if (type == "mean") {names(dfc)[3] <- "y"}
	if (type == "frequency") {names(dfc)[2] <- "y"}
	
	names(dfc)[1] <- "x"
	
	if (sort_by_size) {
		gg <- ggplot(dfc, aes(x=reorder(x, desc(y)), y=y)) 
	} else if (sort_by_label) {
		gg <- ggplot(dfc, aes(x=reorder(x, x), y=y)) 
		
	} else {
		gg <- ggplot(dfc, aes(x=x, y=y))
	}
	
	
	gg <- gg + 
		geom_bar(position=position_dodge(), stat="identity",
             colour="black", # Use black outlines
		         fill = "orangered4",
             size=.3) +      # Thinner lines
    	geom_errorbar(aes(ymin=y-se, ymax=y+se),
                  size=.3,    # Thinner lines
                  width=.2,
                  position=position_dodge(.9)) +
    	xlab(xlabel) +
    	ylab(ylabel) +
   	 ggtitle(Title) +
#    scale_y_continuous(breaks=0:20*4) +
    	theme_classic() +
		theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +

    	theme(plot.margin=unit(c(0,0,0,0), "cm"),panel.margin=unit(c(0,0,0,0), "cm"))
                   
    
	print(gg)	
	dev.off()

	quartz(width=8, height=4)
	img <- readPNG(imgFile)
	grid::grid.raster(img)

	gg
}


# -----------------------------------------------------------------------------------------
#	PRINT CORRELATIONS
#		Prints a pretty correlation table for every quantitative variable in data frame.
#       The table is also stored as an image in the g_image_folder directory
# -----------------------------------------------------------------------------------------


print_correlations <- function (df) {
	
	imgFile <- paste0(g_image_folder,"correlations.png")
	
	png(imgFile, width=1800, height=1800, res=240)
	
	ndata <- df[sapply(df, is.numeric)]
	
	flattenSquareMatrix(cor.prob(ndata))
	library(PerformanceAnalytics)
	chart.Correlation(ndata)

	dev.off()
	
	quartz()
	img <- readPNG(imgFile)
	grid::grid.raster(img)


}


# -----------------------------------------------------------------------------------------
#	PLOT REGRESSION
#		Given an xvariable (xvar) and yvariable (yvar), this function makes a scatterplot,
#		and plots a fitted curve to that data.  The curve (curve parameter) is either "none",
#		"linear", "smooth", or "quadratic".  This function also takes a Title paramter that
#		determines the title of the graph.
# -----------------------------------------------------------------------------------------

plot_regression <- function (xvar, yvar, curve="linear", Title="") {
	
	
	
	if (curve == "linear") {
		my_method <- "lm"
		my_formula <- "y ~ x"
	} else if (curve == "smooth") {
		my_method <- "loess"
		my_formula <- "y ~ x"
		
	} else if (curve == "quadratic") {
		my_method <- "lm"
		my_formula <- "y ~ x + I(x^2)"
		
	}
	
	myDF <- data.frame(xvar,yvar)
	
	myPlot <- ggplot(myDF, aes(x=xvar, y=yvar)) +
    geom_point(shape=1) +    # Use hollow circles
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
    xlab ("") +
    ylab ("") + 
    ggtitle(Title) +
    theme(plot.title = element_text(size = 10))

    
    if (curve != "none") {
    	myPlot <- myPlot + stat_smooth(method = my_method, formula = my_formula, size = 1)
    }
                        
                  
    return(myPlot)		
}


# -----------------------------------------------------------------------------------------
#	PRETTY CONTINUOUS PLOTS
#		Given an xvariable (xvar) and yvariable (yvar), and Title, this function makes 4
#		Plots of the data (scatter, smooth, linear, and quadratic) and puts it all on one
#		big chart. That chart is saved in the image folder (g_image_folder)
# -----------------------------------------------------------------------------------------

pretty_continuous_plots <- function (xvar, yvar, Title=paste(xvar, "by", yvar)) {

	imgFile <- paste0(g_image_folder,paste0(Title,".png"))
	
	png(imgFile, width=1800, height=1800, res=240)

	
	
	plot1 <- plot_regression(xvar, yvar, curve="none", Title="SCATTERPLOT")
	plot2 <- plot_regression(xvar, yvar, curve="smooth", Title="BEST FIT")
	plot3 <- plot_regression(xvar, yvar, curve="linear", Title="LINEAR FIT")
	plot4 <- plot_regression(xvar, yvar,curve="quadratic", Title="QUADRATIC FIT")

	print(arrangeGrob(plot1,plot2,plot3,plot4, nrow=2, heights=c(2, 2), main=Title))
    dev.off()
	
	quartz()
	img <- readPNG(imgFile)
	grid::grid.raster(img)
         
}


# -----------------------------------------------------------------------------------------
#	PRINT SCATTERPLOTS
#		This function is where you can make all your calls to create the scatterplots you
#		want.
# -----------------------------------------------------------------------------------------

print_scatterplots <- function (all_data, summary_data) {
	
	pretty_continuous_plots(summary_data$AveLinks, summary_data$AveHashTags, Title="MEAN LINKS by MEAN HASHTAGS")
	
}



# -----------------------------------------------------------------------------------------
#	PRINT BARGRAPHS
#		This function is where you can make all your calls to create the bargraphs you
#		want.
# -----------------------------------------------------------------------------------------

print_bargraphs <- function (all_data, summary_data) {
	
	bg1 <- make_bargraph (all_data, "HashTagsNum", "state", Title="Mean Number of Hashtags per Tweet by State", xlabel="State", ylabel="Mean Hashtags per Tweet", sort_by_size=TRUE)

	bg2 <- make_bargraph (summary_data, "Activity", "state", Title="Tweeting Activity by State", xlabel="State", ylabel="Tweets per Teacher", sort_by_size=TRUE)

	bg3 <- make_bargraph (all_data, "WordCount", "state", Title="Average Word Count per Tweet by State", xlabel="State", ylabel="Average Word Count per Tweet", sort_by_size=TRUE)

	bg4 <- make_bargraph (all_data, "Day", "Day", Title="Number of Tweets by Day of the Week", xlabel="Day of the Week", ylabel="Number of Tweets", sort_by_size=FALSE, type="frequency")

	bg5 <- make_bargraph (all_data, "Hour", "Hour", Title="Number of Tweets by Hour of the Day", xlabel="Hour of the Day", ylabel="Number of Tweets", sort_by_size=FALSE, sort_by_label=TRUE, type="frequency")


	
	
}

make_histogram <- function (state, df) {
	
	print(paste0("state: ", state))
	df <- df[df$State == state,]
	gg <- ggplot(df, aes(x=PerDay)) +
		 	geom_histogram(binwidth=1)
		 	
	gg
}


user_activity_graphs <- function (df, Title="Distribution of Tweeters - Tweets per Day - By State") {
  
  imgFile <- paste0(g_image_folder,Title,".png")
  
  png(imgFile, width=2400, height=1800, res=240)
  
  min_date <- min(df$TimeStamp)
  max_date <- max(df$TimeStamp)
  
  days <- (max_date - min_date) / (60 * 60 * 24)
  
  
  my_states <- (unique(df$state))
  
  
  
  finalDF <- ddply(df, .(Twitter.User), summarize, N=length(Tweet.Text), PerDay=length(Tweet.Text)/days, State=state[[1]])
  
  plot1 <- plot_regression(finalDF$State, finalDF$PerDay, curve="none", Title="")
  plot2 <- plot1 + ylim(c(0,3))
  plot3 <- plot1 + ylim(c(0,2))
  plot4 <- plot1 + ylim(c(0,1))
  
  print(arrangeGrob(plot1, plot2, plot3, plot4, nrow=2, heights=c(2, 2), main=Title))
  
  dev.off()
  
  quartz(width=8, height=6)
  img <- readPNG(imgFile)
  grid::grid.raster(img)
  
}



# -----------------------------------------------------------------------------------------
#               COMMAND AND CONTROL - Here is where we decide what to do in what order
# -----------------------------------------------------------------------------------------

# We don't need to pull data every time, just when we need the latest freshest data. Comment
# this out when we're doing development of code so we work on cached data

# Method 1 means, get all fresh data from the web. Method 2 means use cached raw data, Method 3 means use all cached data.
# Use method 1 to get the latest data. Once you have it, use method 3 to work with data for awhile. It saves a lot of time
if(!exists('all_data')) all_data <- get_all_data(method = 2)
saveRDS(all_data, "all_data.rds")

# 
# finalDF <- ddply(all_data, all_data$state, summarize,
#                  N=length(hashtag),
#                  Activity=length(hashtag)/teachers[[1]],
#                  Teachers=teachers[[1]],
#                  AveChars=mean(CharCount),
#                  AveWords=mean(WordCount),
#                  AveHashTags=mean(HashTagsNum),
#                  AveMentions=mean(Mentions),
#                  AveLinks=mean(Links),
#                  AveFollowers=mean(Followers),
#                  AveRetweets=mean(Retweets),
#                  AveFavorites=mean(Favorites),
#                  Voices=length(unique(Twitter.User)),
#                  VoicesPer=length(unique(Twitter.User))/teachers[[1]],
#                  TopTags=return_frequent_hashtags(HashTags) 
# )

# Method 1 means, Create all the summary data anew. Method 2 means use cached summary data. Right now, method 1
# doesn't take much more time than method 2, so method 1 is probably appropriate unless it starts to save significant time.
if (!exists('summary_data')) summary_data <- make_summary_data(all_data, method = 2)

# -------------------
# Results
# -------------------

str(all_data)

saveRDS(all_data, "dev/all_data.RDS")

rm(all_data)

# print_summary_data(summary_data)

# print_correlations (summary_data)

# print_bargraphs(all_data, summary_data)

# print_scatterplots(all_data, summary_data)

# user_activity_graphs(all_data)

# readRDS("summary_data.RDS")

rm(list = ls())
