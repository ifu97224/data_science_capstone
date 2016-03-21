########################################################################
##  Data Science Capstone Project
##  3/12/16
##  This code reads the data from the 3 sources (news, blogs and twitter)
########################################################################

# Set the working directory
setwd("/users/Richard/Documents/Coursera Data Science Track/Data Science Capstone")
getwd()

library(stringr)
library(ggplot2)
library(scales)
library(gridExtra)

### start with the twitter data

# set the connection and read the first 100 lines
con <- file("en_US.twitter.txt", "r") 
nrows <- 100
twitter_data <- readLines(con, nrows) 

# loop over the data reading 100 lines at a time up to 4,000,000 records
for(i in 1:40000) {
  next_lines <- readLines(con,nrows,ok = T)
  twitter_data <- c(twitter_data,next_lines)
}
close(con)
# NOTE:  Data has 2,360,148 rows


### import the blogs

# set the connection and read the first 100 lines
con <- file("en_US.blogs.txt", "r") 
nrows <- 100
blogs_data <- readLines(con, nrows) 

# loop over the data reading 100 lines at a time up to 4,000,000 records
for(i in 1:40000) {
  next_lines <- readLines(con,nrows,ok = T)
  blogs_data <- c(blogs_data,next_lines)
}
close(con)


save(twitter_data, file='twitter_data.Rda')
save(blogs_data, file='blogs_data.Rda')
save(news_data, file='newss_data.Rda')

### import the news

# set the connection and read the first 100 lines
con <- file("en_US.news.txt", "r") 
nrows <- 100
news_data <- readLines(con, nrows) 

# loop over the data reading 100 lines at a time up to 4,000,000 records
for(i in 1:40000) {
  next_lines <- readLines(con,nrows,ok = T)
  news_data <- c(news_data,next_lines)
}
close(con)

#### count the number of words in each of the lines in each of the files ####
nwords <- function(string, pseudo = F) {
  ifelse(pseudo,
         pattern <- "\\S+",
         pattern <- "[[:alpha:]]+")
  str_count(string, pattern)
}

twitter_word_count <- data.frame(nwords(twitter_data,pseudo=T))
names(twitter_word_count)[names(twitter_word_count)=="nwords.twitter_data..pseudo...T."] <- "word_count"
data_source <- rep("Twitter",nrow(twitter_word_count))
twitter_word_count <- cbind(twitter_word_count,data_source)

blogs_word_count <- data.frame(nwords(blogs_data,pseudo=T))
names(blogs_word_count)[names(blogs_word_count)=="nwords.blogs_data..pseudo...T."] <- "word_count"
data_source <- rep("Blogs",nrow(blogs_word_count))
blogs_word_count <- cbind(blogs_word_count,data_source)

news_word_count <- data.frame(nwords(news_data,pseudo=T))
names(news_word_count)[names(news_word_count)=="nwords.news_data..pseudo...T."] <- "word_count"
data_source <- rep("News",nrow(news_word_count))
news_word_count <- cbind(news_word_count,data_source)

word_counts <- rbind(twitter_word_count,blogs_word_count,news_word_count)
head(word_counts)

# create box plots of word counts by source

# prepare the data
word_counts$data_source <- as.factor(word_counts$data_source)
table(word_counts$data_source)

g <- ggplot(word_counts,aes(data_source,word_count)) +
  geom_boxplot(fill="orange") +
  theme_bw(base_family = "Arial") + 
  ggtitle("Number of Words by Data Source") +
  xlab("Data Source") + 
  ylab("Number of Words") +
  theme(axis.title.y=element_text(vjust=1), 
        axis.title.x=element_text(vjust=-0.5),
        plot.title=element_text(vjust=1))

# compute lower and upper whiskers
ylim1 = boxplot.stats(word_counts$word_count)$stats[c(1, 5)]

# scale y limits based on ylim1
g <- g + coord_cartesian(ylim = ylim1*1.05)
g

### Create barplot of the number of records by data source ###

a <- ggplot(word_counts, aes(data_source)) +
     geom_bar(fill="orange") + 
     theme_bw(base_family = "Arial") + 
     ggtitle("Number of Observations by Data Source") +
     xlab("Data Source") + 
     ylab("Number of Observations") +
     theme(axis.title.y=element_text(vjust=1), 
           axis.title.x=element_text(vjust=-0.5), 
           plot.title=element_text(vjust=1)) +
     scale_y_continuous(labels = comma)
a


#### count the number of characters in each of the lines in each of the files ####

twitter_char <- data.frame(nchar(twitter_data, type = "chars", allowNA = FALSE))
names(twitter_char)[names(twitter_char)=="nchar.twitter_data..type....chars...allowNA...FALSE."] <- "number_characters"
data_source <- rep("Twitter",nrow(twitter_char))
twitter_char <- cbind(twitter_char,data_source)

blogs_char <- data.frame(nchar(blogs_data, type = "chars", allowNA = FALSE))
names(blogs_char)[names(blogs_char)=="nchar.blogs_data..type....chars...allowNA...FALSE."] <- "number_characters"
data_source <- rep("Blogs",nrow(blogs_char))
blogs_char <- cbind(blogs_char,data_source)

news_char <- data.frame(nchar(news_data, type = "chars", allowNA = FALSE))
names(news_char)[names(news_char)=="nchar.news_data..type....chars...allowNA...FALSE."] <- "number_characters"
data_source <- rep("News",nrow(news_char))
news_char <- cbind(news_char,data_source)

character_counts <- rbind(twitter_char,blogs_char,news_char)
head(character_counts)

# create box plots of character counts by source

# prepare the data
character_counts$data_source <- as.factor(character_counts$data_source)
table(character_counts$data_source)

b <- ggplot(character_counts,aes(data_source,number_characters)) +
     geom_boxplot(fill="orange") +
     theme_bw(base_family = "Arial") + 
     ggtitle("Number of Characters by Data Source") +
     xlab("Data Source") + 
     ylab("Number of Characters") +
     theme(axis.title.y=element_text(vjust=1), 
           axis.title.x=element_text(vjust=-0.5), 
           plot.title=element_text(vjust=1))

# compute lower and upper whiskers
ylim1 = boxplot.stats(character_counts$number_characters)$stats[c(1, 5)]

# scale y limits based on ylim1
b <- b + coord_cartesian(ylim = ylim1*1.05)
b

#### size of files ####
file_size <- as.numeric(object.size(twitter_data))
data_source <- "Twitter"
twitter_details <- data.frame(data_source,file_size)

file_size <- as.numeric(object.size(blogs_data))
data_source <- "Blogs"
blogs_details <- data.frame(data_source,file_size)

file_size <- as.numeric(object.size(news_data))
data_source <- "News"
news_details <- data.frame(data_source,file_size)

file_sizes <- rbind(twitter_details,blogs_details,news_details)

c <- ggplot(file_sizes, aes(x = data_source, y = file_size)) +
     geom_bar(fill="orange",stat = 'identity') + 
     theme_bw(base_family = "Arial") + 
     ggtitle("Size of File (Bytes) by Data Source") +
     xlab("Data Source") + 
     ylab("Size of File (Bytes") +
     theme(axis.title.y=element_text(vjust=1), 
           axis.title.x=element_text(vjust=-0.5),  
           plot.title=element_text(vjust=1)) +
     scale_y_continuous(labels = comma)
c

grid.arrange(g,a,b,c)


##### Quiz 1 Questions #######

# what is the longest line in any of the 3 files?
max(nchar(twitter_data, type = "chars", allowNA = FALSE))
max(nchar(blogs_data, type = "chars", allowNA = FALSE))
max(nchar(news_data, type = "chars", allowNA = FALSE))

# In the en_US twitter data set, if you divide the number of lines where the word "love" (all lowercase) 
# occurs by the number of lines the word "hate" (all lowercase) occurs, about what do you get?
love_count <- length(grep('love', tolower(twitter_data)))
hate_count <- length(grep('hate', tolower(twitter_data)))
love_count / hate_count

# The one tweet in the en_US twitter data set that matches the word "biostats" says what?
bio_index <- grep('biostats', tolower(twitter_data))
twitter_data[bio_index]

# How many tweets have the exact characters "A computer once beat me at chess, but it was no match for me at kickboxing". 
# (I.e. the line matches those characters exactly.)
chess_index <- grep('A computer once beat me at chess, but it was no match for me at kickboxing', twitter_data)
twitter_data[chess_index]


