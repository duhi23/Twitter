#################################################
#####     Primera aplicacion con TwitteR     #####
#################################################

## Twitter API: Extrae tweets historicos 

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
head(datos)
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


## streamR API: Extrae tweet momentáneos

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

# timeout: tiempo de apertura para filtrar los tweet

tweets.df <- parseTweets("tweets.json", simplify = TRUE)
head(tweets.df)

c( length(grep("obama", tweets.df$text, ignore.case = TRUE)),
   length(grep("biden", tweets.df$text, ignore.case = TRUE)) )


# tweet publicados actualmente en Ecuador

filterStream("tweetsEC.json", track = c("DialogoRC"), timeout = 300, oauth = my_oauth)
filterStream("tweetsEC.json", locations = c(-80.45,-4.28,-76.29,0.41), timeout = 300, oauth = my_oauth)
tweets.ec <- parseTweets("tweetsEC.json", verbose = FALSE)

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


# Seguidores y mas

doInstall <- TRUE  # Change to FALSE if you don't want packages installed.
toInstall <- c("ROAuth", "twitteR", "streamR", "igraph", "XML", "ggplot2", 
               "tm", "stringr", "plyr", "RCurl", "maps", "Snowball")
if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
if(doInstall){install.packages("Rstem", repos = "http://www.omegahat.org/R", type="source")}

library(twitteR)
library(ROAuth)
requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "http://api.twitter.com/oauth/access_token"
authURL <- "http://api.twitter.com/oauth/authorize"
consumerKey <- "gKXs8cHpEqBbBpHY3rotdVTzH"
consumerSecret <- "tdIGk14MX3ix92u6JiFlUncw6z2q7n1kSbvqsQMEFOxloXfPvv"
my_oauth <- OAuthFactory$new(consumerKey=consumerKey,
                             consumerSecret=consumerSecret, requestURL=requestURL,
                             accessURL=accessURL, authURL=authURL)

## run this line and go to the URL that appears on screen
my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))



save(my_oauth, file="my_oauth")



load("my_oauth")
registerTwitterOAuth(my_oauth)
seed <- getUser("duhi23")

# From a Windows machine...
# seed <- getUser("drewconway", cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))

# loading backup (in case internet does not work)
# load("backup/seed")

seed.n <- seed$screenName
seed.n
following <- seed$getFriends()

# From a Windows machine...
# following <- seed$getFriends(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))


# loading backup (in case internet does not work)
# load("backup/following")

following.n <- as.character(lapply(following, function(x) x$getScreenName()))
head(following.n)
# saving follow.list
follow.list <- list()
follow.list[[seed.n]] <- following.n

## extracting description of users
descriptions <- as.character(lapply(following, function(x) x$getDescription()))
descriptions[1]

# functions to subset only users from NYU-Politics
extract.nyu <- function(descriptions){
      nyu <- grep('nyu|new york university', descriptions, ignore.case=TRUE)
      poli <- grep('poli(tics|tical|sci)', descriptions, ignore.case=TRUE)
      others <- grep('policy|wagner|cooperation', descriptions, ignore.case=TRUE)
      nyu.poli <- intersect(nyu, poli)
      nyu.poli <- nyu.poli[nyu.poli %in% others == FALSE]
      return(nyu.poli)
}

nyu <- extract.nyu(descriptions)
nyu.users <- c(seed$screenName, following.n[nyu], "cdsamii", "zeitzoff")


# loop over NYU users following same steps
while (length(nyu.users) > length(follow.list)){
      
      # pick first user not done
      user <- sample(nyu.users[nyu.users %in% names(follow.list)==FALSE], 1)
      user <- getUser(user)
      user.n <- user$screenName
      cat(user.n, "\n")
      
      # download list of users he/she follows
      following <- user$getFriends()
      friends <- as.character(lapply(following, function(x) x$getScreenName()))
      follow.list[[user.n]] <- friends
      descriptions <- as.character(lapply(following, function(x) x$getDescription()))
      
      # subset and add users from NYU Politics
      nyu <- extract.nyu(descriptions)
      new.users <- lapply(following[nyu], function(x) x$getScreenName())
      new.users <- as.character(new.users)
      nyu.users <- unique(c(nyu.users, new.users))
      
      # if rate limit is hit, wait for a minute
      limit <- getCurRateLimitInfo()[44,3]
      while (limit == "0"){
            cat("sleeping for one minute")
            Sys.sleep(60)
            limit <- getCurRateLimitInfo()[44,3]
      }
      print(nyu.users)
}


# loading backup (in case internet does not work)
# load("backup/follow.list")

# a little bit of network analysis
nyu.users <- names(follow.list)
adjMatrix <- lapply(follow.list, function(x) (nyu.users %in% x)*1)
adjMatrix <- matrix(unlist(adjMatrix), nrow=length(nyu.users), byrow=TRUE, dimnames=list(nyu.users, nyu.users))

library(igraph)
network <- graph.adjacency(adjMatrix)
plot(network)

V(network)$size <- degree(network, mode="in")
V(network)$label.cex <- (degree(network, mode="in")/max(degree(network, mode="in"))*1.25)+0.5
set.seed(777)
l <- layout.fruchterman.reingold.grid(network, niter=500)
pdf("network_nyu.pdf", width=7, height=7)
plot(network, layout=l, edge.width=1, edge.arrow.size=.25, vertex.label.color="black", vertex.shape="none", margin=-.15)
dev.off()