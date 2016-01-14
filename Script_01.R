#################################################
#####     Primera aplicacion con TwitteR     #####
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

rstats <- searchTwitter("#DiálogoRC", n=9999, since='2016-01-11')
datos <- do.call("rbind", lapply(rstats, as.data.frame))
names(datos)
str(datos)

usuarios <- subset(datos, isRetweet==FALSE)$screenName
usuarios <- sort(table(usuarios), decreasing=T)
head(usuarios)
usuarios[1:30]

# Texto tweets
texto <- subset(datos, isRetweet==FALSE)$text

pure_text <- function(texto){
      #texto <- tolower(texto)
      library(stringr)
      palabras <- unlist(strsplit(texto, split=" "))
      palabras <- palabras[str_length(palabras)>3]
      #palabras <- sub("\\.", "", palabras)
      #palabras <- sub("\\,", "", palabras)
      #palabras <- sub("\\:", "", palabras)
      palabras <- gsub(pattern = "[[:punct:]]", replacement = "", palabras)
      palabras <- tolower(palabras)
      #palabras <- palabras[nchar(palabras)!=0]
      conectores <- palabras %in% c("hemos", "para", "tiene", "está", "pero", "otro", "puede", "este", 
                                    "esta", "todos", "todas", "mucho", "será", "mejor", "gran", "como",
                                    "sido", "somos")
      palabras <- palabras[!conectores]
      return(palabras)
}

# Creamos un gran vector que contenga todas las palabras del los tweets
palabras <- do.call("c", lapply(texto, pure_text))
conteo <- sort(table(palabras), decreasing=T)
datos <- data.frame(palabra=rownames(conteo), frecuencia=as.vector(conteo))

# Consideraremos las primeras 50 palabras para graficarlas
datos <- datos[3:100,]

library(wordcloud)

wordcloud(datos$palabra, datos$frecuencia, colors = brewer.pal(8, "Dark2"))


# Obteniendo los Retweets

usuarios2 <- subset(datos, isRetweet==TRUE)$screenName
usuarios2 <- sort(table(usuarios2), decreasing=T)
head(usuarios2)


# Tweets de duhi23

duhi <- userTimeline('duhi23', n=50)
duhi2 <- do.call("rbind", lapply(duhi, as.data.frame))


## Usando el paquete: stream R 

install.packages("streamR")  # from CRAN
library(devtools)
install_github("streamR", "pablobarbera", subdir = "streamR")  # from GitHub

install.packages('ROAuth')
library(ROAuth)
requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumerKey <- "gKXs8cHpEqBbBpHY3rotdVTzH"
consumerSecret <- "tdIGk14MX3ix92u6JiFlUncw6z2q7n1kSbvqsQMEFOxloXfPvv"
my_oauth <- OAuthFactory$new(consumerKey = consumerKey, consumerSecret = consumerSecret, 
                             requestURL = requestURL, accessURL = accessURL, authURL = authURL)
my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
save(my_oauth, file = "my_oauth.Rdata")


library(streamR)
load("my_oauth.Rdata")
filterStream("tweets.json", track = c("Obama", "Biden"), timeout = 120, 
             oauth = my_oauth)

tweets.df <- parseTweets("tweets.json", simplify = TRUE)
head(tweets.df)

c( length(grep("obama", tweets.df$text, ignore.case = TRUE)),
   length(grep("biden", tweets.df$text, ignore.case = TRUE)) )



filterStream("tweetsUS.json", locations = c(-125, 25, -66, 50), timeout = 300, 
             oauth = my_oauth)
tweets.df <- parseTweets("tweetsUS.json", verbose = FALSE)
library(ggplot2)
library(grid)
map.data <- map_data("state")
points <- data.frame(x = as.numeric(tweets.df$lon), y = as.numeric(tweets.df$lat))
points <- points[points$y > 25, ]
ggplot(map.data) + geom_map(aes(map_id = region), map = map.data, fill = "white", 
            color = "grey20", size = 0.25) + expand_limits(x = map.data$long, y = map.data$lat) + 
            theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), 
            axis.title = element_blank(), panel.background = element_blank(), panel.border = element_blank(), 
            panel.grid.major = element_blank(), plot.background = element_blank(), 
            plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines")) + geom_point(data = points, 
            aes(x = x, y = y), size = 1, alpha = 1/5, color = "darkblue")


