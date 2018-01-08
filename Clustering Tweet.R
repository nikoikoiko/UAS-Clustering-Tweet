# Packages are required:
install.packages("twitteR")
install.packages("ROAuth")
install.packages("ggplot2")
install.packages("plyr")
install.packages("devtools")
install.packages("e1071")
install.packages("cluster")
install.packages("datasets")

# Packages that have non-zero exit state, need to change the installation by using devtools::install_cran.
# It's happen on R Version higher above than 3++.
# install.packages("fpc")
# install.packages("RTextTools")
# install.packages("wordcloud")
# install.packages("tm")

# Install Slam & Flexmix Dependencies of TM & FPC and also How to import some libraries
require('devtools')
devtools::install_url("https://cran.r-project.org/src/contrib/Archive/slam/slam_0.1-37.tar.gz")
devtools::install_url("https://cran.r-project.org/src/contrib/Archive/flexmix/flexmix_2.3-13.tar.gz")
devtools::install_cran("tm")
devtools::install_cran("wordcloud")
devtools::install_cran("RTextTools")
devtools::install_cran("fpc")
library('e1071')
library('twitteR')
library('ROAuth')
library('tm')
library('ggplot2')
library('wordcloud')
library('plyr')
library('RTextTools')
library('fpc')
library('cluster')
library('datasets')

# Twitter with account: @nikofitrianto in Developer Mode
# Params:
# 1). Consumer Key
# 2). Consumer Secret Key
# 3). Access Token
# 4). Access Token Secret Key
setup_twitter_oauth("1dddKM1AhDPtHWRCizmIkZWhM",
                    "EeWRcwQNvnIzrSsAxazZRSXw94XT99qzGvSIncSs1eyeTK4W6v",
                    "932581012587945985-fmFD1OfHaFdcX44ZQFqVp4PhyylcAEf",
                    "vpkrRs2jPbHid4l3cHGLDZQBBSiUqnNgpFeCEl2TvRgH3")

#WORDCLOUD

tweets <- userTimeline("Startup", n = 250)
show(tweets)
n.tweet <- length(tweets)
# convert tweets to a data frame
tweets.df <- twListToDF(tweets)

myCorpus <- Corpus(VectorSource(tweets.df$text))
# convert to lower case
myCorpus <- tm_map(myCorpus, content_transformer(tolower))
# remove URLs
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeURL))
# remove anything other than English letters or space
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))
# remove stopwords
myStopwords <- c(setdiff(stopwords('english'), c("r", "big")),"use", "see", "used", "via", "amp")
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
# remove extra whitespace
myCorpus <- tm_map(myCorpus, stripWhitespace)
# keep a copy for stem completion later
myCorpusCopy <- myCorpus
tdm <- TermDocumentMatrix(myCorpus)
term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >= 20)
df <- data.frame(term = names(term.freq), freq = term.freq)

ggplot(df, aes(x=term, y=freq)) + geom_bar(stat="identity") +
  xlab("Terms") + ylab("Count") + coord_flip() +
  theme(axis.text=element_text(size=7))

m <- as.matrix(tdm)
# calculate the frequency of words and sort it by frequency
word.freq <- sort(rowSums(m), decreasing = T)
# colors
pal <- brewer.pal(9, "BuGn")[-(1:4)]

# plot word cloud

wordcloud(words = names(word.freq), freq = word.freq, min.freq = 3,
          random.order = F, colors = pal)

# k-means clustering
d <- dist(term.freq, method="euclidian")
carsCluster <- kmeans(term.freq, 1)
clusplot(as.matrix(d), carsCluster$cluster, color=T, shade=T, labels=3, lines=0)
