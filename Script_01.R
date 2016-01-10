#################################################
#####     Primera apliacion con TwitteR     #####
#################################################

install.packages('twitteR', dependencies = TRUE)
install.packages('base64enc', dependencies = TRUE)
library(twitteR)
library(base64enc)

consumer_key <- "gKXs8cHpEqBbBpHY3rotdVTzH"
consumer_secret <- "tdIGk14MX3ix92u6JiFlUncw6z2q7n1kSbvqsQMEFOxloXfPvv"
access_token <- "180857891-595URwNzy5nyiCoOsoRvEQWLsghmwm6BJdcQlnDH"
access_secret <- "nf8EmxldgDH19xLq32ktnt6uWHYjH60dDEBogwig4b9dX"

setup_twitter_oauth(consumer_key, consumer_secret, access_token=access_token, access_secret=access_secret)

# Extrayendo primeros tweets

rstats <- searchTwitter("#rstats", n=9999, since='2015-10-01')
datos <- do.call("rbind", lapply(rstats, as.data.frame))
names(datos)
str(datos)

usuarios <- subset(datos, isRetweet==FALSE)$screenName
usuarios <- sort(table(usuarios), decreasing=T)
head(usuarios)
usuarios[1:30]

usuarios2 <- subset(datos, isRetweet==TRUE)$screenName
usuarios2 <- sort(table(usuarios2), decreasing=T)
head(usuarios2)


# Tweets de duhi23

duhi <- userTimeline('duhi23', n=50)
duhi2 <- do.call("rbind", lapply(duhi, as.data.frame))
