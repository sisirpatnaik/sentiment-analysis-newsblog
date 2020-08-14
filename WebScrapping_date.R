###################################################################
#                                                                 #
#                                                                 #
#               Author: Sisir Patnaik                             #
#               Date: August 2017                                 #
#                                                                 #
###################################################################
#Prepare the NDTV Blog url for all 67 pages.
library(xml2)
library(rvest)

#Collect URLS
urlprt <- paste("http://www.ndtv.com/opinion/page-","1",sep = "")

i <- 1
urlpage <- list()
finalurls <- data.frame()
while (i <= 67) {
  urlpage[i] <- paste("http://www.ndtv.com/opinion/page-",i,sep = "")
  pg <- read_html(urlpage[[i]])
  urls <- html_attr(html_nodes(pg, "a"), "href")
  grx <- glob2rx("http://www.ndtv.com/opinion/*")
  grep(grx, urls)
  opurls <- grep(grx, urls, value = TRUE)
  dfurl <- data.frame(opurls)
  finalurls <- rbind(finalurls,dfurl)
  i = i+1
}

dim(finalurls)
finalurls <- unique(finalurls)
dim(finalurls)

grx <- glob2rx("http://www.ndtv.com/opinion/page-*")
pageidx <- grep(grx, finalurls$opurls)

finalurls <- data.frame(finalurls[-pageidx,])

dim(finalurls)

finalurls <- data.frame(finalurls[-4,])

colnames(finalurls) <- c("URL")
finalurls$URL <- as.character(finalurls$URL)

str(finalurls)
fix(finalurls)
i <- 1
totaltxt1 <- list()
totaltxt2 <- list()
totaltxt3 <- list()
totaltxt4 <- list()
dftextdt <- data.frame()


#Extract text from url 1 to 200
while (i <= 200) {
  #Extract text data from url
  hdata <- read_html(finalurls[i,])
  
  #ins_storybody
  
  htxtdata <- hdata %>%
    html_nodes("#ins_storybody") %>%
    html_text()
  htxtdate <- hdata %>%
    html_nodes(".firstpublising") %>%
    html_text()
  dftextdt1 <- data.frame(text=c(htxtdata),date=c(htxtdate))
  dftextdt <- rbind(dftextdt,dftextdt1)
  totaltxt1 <- paste(totaltxt1,htxtdata)
  i = i+1
}

#Extract text from url 200 to 400
while (i <= 400) {
  #Extract text data from url
  hdata <- read_html(finalurls[i,])
  
  #ins_storybody
  
  htxtdata <- hdata %>%
    html_nodes("#ins_storybody") %>%
    html_text()
  htxtdate <- hdata %>%
    html_nodes(".firstpublising") %>%
    html_text()
  dftextdt1 <- data.frame(text=c(htxtdata),date=c(htxtdate))
  dftextdt <- rbind(dftextdt,dftextdt1)
  totaltxt2 <- paste(totaltxt2,htxtdata)
  i = i+1
}
#Extract text from url 400 to 600
while (i <= 600) {
  #Extract text data from url
  hdata <- read_html(finalurls[i,])
  
  #ins_storybody
  
  htxtdata <- hdata %>%
    html_nodes("#ins_storybody") %>%
    html_text()
  htxtdate <- hdata %>%
    html_nodes(".firstpublising") %>%
    html_text()
  dftextdt1 <- data.frame(text=c(htxtdata),date=c(htxtdate))
  dftextdt <- rbind(dftextdt,dftextdt1)
  totaltxt3 <- paste(totaltxt3,htxtdata)
  i = i+1
}
#Extract text from url 600 to 766
while (i <= 766) {
  #Extract text data from url
  hdata <- read_html(finalurls[i,])
  
  #ins_storybody
  
  htxtdata <- hdata %>%
    html_nodes("#ins_storybody") %>%
    html_text()
  htxtdate <- hdata %>%
    html_nodes(".firstpublising") %>%
    html_text()
  dftextdt1 <- data.frame(text=c(htxtdata),date=c(htxtdate))
  dftextdt <- rbind(dftextdt,dftextdt1)
  totaltxt4 <- paste(totaltxt4,htxtdata)
  i = i+1
}
fix(dftextdt)

dim(totaltxt4)
#Create DFM and Word Cloud
#TF-IDF
#install.packages(c('tm', 'SnowballC', 'wordcloud', 'topicmodels'))
library(tm)
library(SnowballC)
library(wordcloud)
#install.packages("quanteda")
library(quanteda)
gc()

dfm <- dfm(totaltxt4, remove_punct  = TRUE, remove = stopwords("english")) 

set.seed(100)
textplot_wordcloud(dfm, min.freq = 15, random.order = FALSE,
                   rot.per = .25, 
                   colors = RColorBrewer::brewer.pal(8,"Dark2"))
topfeatures(dfm, 20)

#Creating JSON tags with each sentence
#Splitting a blog to sentences
months.regex <- paste(month.name, collapse='|')

dfsentdt <- data.frame()
dfsentdt1 <- data.frame()
k <- 1
while (k <= 766) {
  text<- as.character(dftextdt$text[k])
  z <- text %>% 
    gsub(" +", " ", .) %>% 
    strsplit(split = "[\\.?!] ")
  
  rawdate <- as.character(dftextdt$date[k])
  date <- gsub(paste0(".*(", months.regex, ")"), "\\1", 
            rawdate[grep(months.regex, rawdate, TRUE)], TRUE)
  
  sent <- z[[1]]
  dfsentdt1 <- data.frame(sentence=c(sent),date=c(date))
  dfsentdt <- rbind(dfsentdt,dfsentdt1)
  k <- k+1
}
dfsentdt$id <- seq.int(nrow(dfsentdt))
fix(dfsentdt)
dim(dfsentdt)

#Convert the Sentences to JSON tag and POST to congnitive service API
library(rjson)
library(jsonlite)
library(httr)
library(plyr)
dftot <- data.frame()
j <- 1
while (j <= 30000) {
  
  k <- j+999
  
  keyvallist <- data.frame(language="en",id=c(j:k),text=dfsentdt$sentence[j:k])

  jsonout <- toJSON(list(documents = keyvallist), pretty = TRUE)

  result <- POST("https://westus.api.cognitive.microsoft.com/text/analytics/v2.0/sentiment",
               body = jsonout,
               add_headers(.headers = c("Content-Type"="application/json","Ocp-Apim-Subscription-Key"="<Enter your Cognitive Service Key>")))

  Output <- content(result)
  

  dfout <- do.call("rbind.fill", lapply(Output$documents, as.data.frame))
  dftot <- rbind(dftot,dfout)
  j <- j + 1000
}
fix(dfsentdt)
fix(dftot)

#Remove IST and timestamp from the date
dfsentdt$date<- trimws(gsub("IST.*", "", dfsentdt$date))
dfsentdt$date <- trimws(substr(dfsentdt$date,1,nchar(dfsentdt$date)-5))

#Add a new column for month
substr(dfsentdt$date, nchar(dfsentdt$date)-3, nchar(dfsentdt$date))
substr(dfsentdt$date, 1, nchar(dfsentdt$date)-9)
dfsentdt$month <- paste(substr(dfsentdt$date, nchar(dfsentdt$date)-3,nchar(dfsentdt$date))
                        ,substr(dfsentdt$date, 1, nchar(dfsentdt$date)-9),sep="")


dfsentsub <- dfsentdt[grep("Modi", dfsentdt$sentence),]
modisub <- merge(dfsentsub,dftot)
fix(modisub)
write.csv(modisub, file = "C:\\RCode\\modisub.csv", row.names = FALSE)

modiplot <- aggregate( modisub$score ~ modisub$month, modisub, mean )

colnames(modiplot) <- c("month","meanscore")

require(zoo)

modiplot <- modiplot[order(as.yearmon(modiplot$month, format="%Y%b")),]
modiplot$month <- factor(modiplot$month, 
                         levels = modiplot$month[order(as.yearmon(modiplot$month, format="%Y%b"))])

library(ggplot2)
ggplot(data=modiplot, aes(x=modiplot$month, y=modiplot$meanscore)) +
  geom_point(alpha=.4, size=4, color="#880011") +
  ggtitle("Month vs. Sentiment Mean Score for PM Narendra Modi") +
  labs(x="Month", y="Sentiment Mean Score")



dfsentsub <- dfsentdt[grep("BJP", dfsentdt$sentence),]
bjpsub <- merge(dfsentsub,dftot)
write.csv(bjpsub, file = "C:\\RCode\\bjpsub.csv", row.names = FALSE)

bjpplot <- aggregate( bjpsub$score ~ bjpsub$month, bjpsub, mean )
colnames(bjpplot) <- c("month","meanscore")
require(zoo)
bjpplot <- bjpplot[order(as.yearmon(bjpplot$month, format="%Y%b")),]
bjpplot$month <- factor(bjpplot$month, 
                         levels = bjpplot$month[order(as.yearmon(bjpplot$month, format="%Y%b"))])

library(ggplot2)
ggplot(data=bjpplot, aes(x=bjpplot$month, y=bjpplot$meanscore)) +
  geom_point(alpha=.4, size=4, color="#880011") +
  ggtitle("Month vs. Sentiment Mean Score for BJP") +
  labs(x="Month", y="Sentiment Mean Score")



dfsentsub <- dfsentdt[grep("Congress", dfsentdt$sentence),]
congsub <- merge(dfsentsub,dftot)
write.csv(congsub, file = "C:\\RCode\\congsub.csv", row.names = FALSE)

congplot <- aggregate( congsub$score ~ congsub$month, congsub, mean )
colnames(congplot) <- c("month","meanscore")
require(zoo)
congplot <- congplot[order(as.yearmon(congplot$month, format="%Y%b")),]
congplot$month <- factor(congplot$month, 
                        levels = congplot$month[order(as.yearmon(congplot$month, format="%Y%b"))])

library(ggplot2)
ggplot(data=congplot, aes(x=congplot$month, y=congplot$meanscore)) +
  geom_point(alpha=.4, size=4, color="#880011") +
  ggtitle("Month vs. Sentiment Mean Score for Congress") +
  labs(x="Month", y="Sentiment Mean Score")

dfsentsub <- dfsentdt[grep("Trump", dfsentdt$sentence),]
trumpsub <- merge(dfsentsub,dftot)
write.csv(trumpsub, file = "C:\\RCode\\trumpsub.csv", row.names = FALSE)

trumpplot <- aggregate( trumpsub$score ~ trumpsub$month, trumpsub, mean )
colnames(trumpplot) <- c("month","meanscore")
require(zoo)
trumpplot <- trumpplot[order(as.yearmon(trumpplot$month, format="%Y%b")),]
trumpplot$month <- factor(trumpplot$month, 
                         levels = trumpplot$month[order(as.yearmon(trumpplot$month, format="%Y%b"))])

library(ggplot2)
ggplot(data=trumpplot, aes(x=trumpplot$month, y=trumpplot$meanscore)) +
  geom_point(alpha=.4, size=4, color="#880011") +
  ggtitle("Month vs. Sentiment Mean Score for Trump") +
  labs(x="Month", y="Sentiment Mean Score")


dfsentsub <- dfsentdt[grep("government|Government", dfsentdt$sentence),]
govtsub <- merge(dfsentsub,dftot)
write.csv(govtsub, file = "C:\\RCode\\govtsub.csv", row.names = FALSE)

write.csv(dftot, file = "C:\\RCode\\finalsentiment.csv", row.names = FALSE)
write.csv(dfsentdt, file = "C:\\RCode\\sentences.csv", row.names = FALSE)






