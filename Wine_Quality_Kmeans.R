library(tidyr)
library(dplyr)
library(caret)
library(caTools)
library(ranger)
library(tidyverse)
library(corrplot)
library(leaps)
library(SentimentAnalysis)
library(mice)

######################DATA PREPARATION######################

wine = read.csv('winemag-data-130k-v2.csv',na.strings = c("NA","N/A",""))

str(wine) #129971 obs. of  14 variables
sum(is.na(wine$country)) #country has 63 blanks
sum(is.na(wine$description)) #description has 0 blanks
sum(is.na(wine$designation)) #designation has 37465 blanks
sum(is.na(wine$points)) #points has 0 blanks
sum(is.na(wine$price)) #price has 8996 blanks
sum(is.na(wine$province)) #province has 63 blanks
sum(is.na(wine$region_1)) #region_1 has 21247 blanks
sum(is.na(wine$region_2)) #region_2 has 79460 blanks
sum(is.na(wine$taster_name)) #taster_name  has 26244 blanks
sum(is.na(wine$taster_twitter_handle)) #taster_twitter_handle  has 31213 blanks
sum(is.na(wine$title)) #title  has 0 blanks
sum(is.na(wine$variety)) #variety has 1 blanks
sum(is.na(wine$winery)) #wine$winery has 0 blanks

wine <- wine[,-c(1,4,9,10,11)] #removed ID, designation,region_2, taster_name and taster_twitter_handle
str(wine)

wineclean <- na.omit(wine) #removing all NAs. 
str(wineclean) #remaining dataset contains 101,400 obs

######################################BASIC DATA EXPLORATION######################################

#Graph showing count (y) vs points (x) with mean points
mean(wineclean$points) #88.46334
ggplot(wineclean, aes(x=points))+
  geom_bar()+
  ylim(0,16000)+
  geom_vline(xintercept=mean(wineclean$points), color='red')+
  ggtitle("Points Frequency")

#Graph showing count (y) vs price (x) with mean price
mean(wineclean$price) #36.93656
ggplot(wineclean, aes(x=price))+
  geom_bar()+
  xlim(0,250)+
  geom_vline(xintercept=mean(wineclean$price), color='red')+
  ggtitle("Price Frequency")

#Correlation between price and points
cor(wineclean$price,wineclean$points)
#0.4146704

######################GEOGRAPHY PLOTS######################################

#Limit data set to variables we're using and group by country
df<- select(wineclean, country, points, price)
df2 <- group_by(df, country)

#Create summary statistics for graphs
graphData <- summarise(df2, ptsMean=mean(points), priceMean=mean(price))

###GENERATE MEAN POINTS BY COUNTRY###
#Load mean points data, order by mean points in descending order
ptsMeanGraph <- ggplot(data=graphData, aes(x=reorder(country, -ptsMean), y=ptsMean)) 
#Create the bars and color them blue
ptsMeanGraph <- ptsMeanGraph + geom_bar(stat="identity", fill="steelblue") 
#Rescale the y-axis to improve visualization
ptsMeanGraph <- ptsMeanGraph + coord_cartesian(ylim=c(82.5,90)) 
#Add and format labels 
ptsMeanGraph <- ptsMeanGraph + geom_text(aes(label=round(ptsMean, digits=1)), vjust=1.6, color="white", size=3.5) 
#Update chart title and labels
ptsMeanGraph <- ptsMeanGraph + ggtitle("Mean Points by Country")
ptsMeanGraph <- ptsMeanGraph + xlab("Country")
ptsMeanGraph <- ptsMeanGraph + ylab("Points by Country")
#Apply minimal theme
ptsMeanGraph <- ptsMeanGraph + theme_minimal()
#display graph
ptsMeanGraph

###GENERATE MEAN PRICE BY COUNTRY###
#Load mean points data, order by mean points in descending order
priceMeanGraph <- ggplot(data=graphData, aes(x=reorder(country, -priceMean), y=priceMean)) 
#Create the bars and color them blue
priceMeanGraph <- priceMeanGraph + geom_bar(stat="identity", fill="darkgreen") 
#Rescale the y-axis to improve visualization
priceMeanGraph <- priceMeanGraph + coord_cartesian(ylim=c(0,50)) 
#Add and format labels 
priceMeanGraph <- priceMeanGraph + geom_text(aes(label=round(priceMean, digits=2)), vjust=1.6, color="white", size=3.5) 
#Update chart title and labels
priceMeanGraph <- priceMeanGraph + ggtitle("Mean Price by Country")
priceMeanGraph <- priceMeanGraph + xlab("Country")
priceMeanGraph <- priceMeanGraph + ylab("Price by Country")
#Apply minimal theme
priceMeanGraph <- priceMeanGraph + theme_minimal()
#display graph
priceMeanGraph

#######################VARIETY PLOTS###################################

library(ggplot2)
library(ggthemes)
head(wineclean$variety)
winevarietycount = count(wineclean$variety)
class(wineclean$variety)
winevarietycount = count(wineclean, 'variety')
winevarietycount
class(winevarietycount)
winevarietycount = group_by(winevarietycount, by = 'freq')
order(-winevarietycount$freq)
winevarietycount[,order(winevarietycount$freq)]


d1 <- wineclean %>% group_by(variety) %>% 
  summarise(avgPrice = mean(price), avgPoints = mean(points), count = n()) %>% 
  arrange(desc(count)) %>% 
  head(20)
d1

ggplot(d1, aes(x = variety, y = avgPrice)) +
  geom_point() + 
  geom_line(aes(group = 1)) +
  labs(title = "Top Variety Wines VS Average of Price",
       x = "",
       y = "Average Price") +
  theme_hc() +
  theme(axis.text.x = element_text(angle = 45))

ggplot(d1, aes(x = variety, y = avgPoints)) +
  geom_point() + 
  geom_line(aes(group = 1)) +
  labs(title = "Top Variety Wines VS Average of Points",
       x = "",
       y = "Average Points") +
  theme_hc() +
  theme(axis.text.x = element_text(angle = 45))

######################Preparing bag of words in description######################

wineclean$description <- as.character(wineclean$description) #changes description from factor to char
wineclean$title <- as.character(wineclean$title) #changes title from factor to char
str(wineclean)# to check if description and title have been changed successfully to char

#create a corpus
library(tm)
corpus = Corpus(VectorSource(wineclean$description))
#convert to lower case
corpus = tm_map(corpus,FUN = content_transformer(tolower))
#remove punctuation
corpus = tm_map(corpus,FUN = removePunctuation)
#remove stopwords
corpus = tm_map(corpus,FUN = removeWords,c(stopwords('english')))
#remove whitespace
corpus = tm_map(corpus,FUN = stripWhitespace)
#create a dictionary
dict = findFreqTerms(DocumentTermMatrix(Corpus(VectorSource(wineclean$description))),
                     lowfreq = 0)
dict_corpus = Corpus(VectorSource(dict))
#create a tm_map to stem words
corpus = tm_map(corpus,FUN = stemDocument)
#Create a DocumentTermMatrix
dtm = DocumentTermMatrix(corpus)
dtm
#removing sparse terms
xdtm = removeSparseTerms(dtm,sparse = 0.95)
xdtm
#complete stems
xdtm = as.data.frame(as.matrix(xdtm))
colnames(xdtm) = stemCompletion(x = colnames(xdtm),
                                dictionary = dict_corpus,
                                type='prevalent')
colnames(xdtm) = make.names(colnames(xdtm))
#browse tokens
sort(colSums(xdtm),decreasing = T)

###Document Term Matrix - TFIDF

dtm_tfidf = DocumentTermMatrix(x=corpus,
                               control = list(weighting=function(x) weightTfIdf(x,normalize=F)))
xdtm_tfidf = removeSparseTerms(dtm_tfidf,sparse = 0.95)
xdtm_tfidf = as.data.frame(as.matrix(xdtm_tfidf))
colnames(xdtm_tfidf) = stemCompletion(x = colnames(xdtm_tfidf),
                                      dictionary = dict_corpus,
                                      type='prevalent')
colnames(xdtm_tfidf) = make.names(colnames(xdtm_tfidf))
sort(colSums(xdtm_tfidf),decreasing = T)

###Document Term Matrix: Term Frequency vs. Term Frequency Inverse Document Frequency

library(tidyr); library(dplyr); library(ggplot2); library(ggthemes)
data.frame(term = colnames(xdtm),tf = colMeans(xdtm), tfidf = colMeans(xdtm_tfidf))%>%
  arrange(desc(tf))%>%
  top_n(20)%>%
  gather(key=weighting_method,value=weight,2:3)%>%
  ggplot(aes(x=reorder(term,weight),y=weight,fill=weighting_method))+
  geom_col(position='dodge')+
  coord_flip()+theme_bw()

wineclean_description = cbind(wineclean,xdtm_tfidf)

###Find top 10 words in description for 98 - 100 wine, and 80 - 82 wine###
wine98to100 <- wineclean_description %>% filter (points==98:100)
wine98to100words <- wine98to100[,-c(1:9)]
sort(colSums(wine98to100words),decreasing = T)

data.frame(term = colnames(wine98to100words), tfidf = colSums(wine98to100words))%>%
  arrange(desc(term))%>%
  top_n(10)%>%
  gather(key=weighting_method,value=weight,2:2)%>%
  ggplot(aes(x=reorder(term,weight),y=weight))+
  geom_col(position='dodge')+
  coord_flip()+theme_bw()

wine80to82 <- wineclean_description %>% filter (points==80:82)
wine80to82words <- wine80to82[,-c(1:9)]
sort(colSums(wine80to82words),decreasing = T)

data.frame(term = colnames(wine80to82words), tfidf = colSums(wine80to82words))%>%
  arrange(desc(term))%>%
  top_n(10)%>%
  gather(key=weighting_method,value=weight,2:2)%>%
  ggplot(aes(x=reorder(term,weight),y=weight))+
  geom_col(position='dodge')+
  coord_flip()+theme_bw()

######################Find top 10 words in description for top 10% most expensive and bottom 10% least expensive######################

quantile(wineclean_description$price,probs = c(0.1,0.9))

winetop10percentprice <- wineclean_description %>% filter (price>45)
winetop10percentpricewords <- winetop10percentprice[,-c(1:9)]
sort(colSums(winetop10percentpricewords),decreasing = T)

data.frame(term = colnames(winetop10percentpricewords), tfidf = colSums(winetop10percentpricewords))%>%
  arrange(desc(term))%>%
  top_n(10)%>%
  gather(key=weighting_method,value=weight,2:2)%>%
  ggplot(aes(x=reorder(term,weight),y=weight))+
  geom_col(position='dodge')+
  coord_flip()+theme_bw()

winebot10percentprice <- wineclean_description %>% filter (price < 13)
winebot10percentpricewords <- winebot10percentprice[,-c(1:9)]
sort(colSums(winebot10percentpricewords),decreasing = T)

data.frame(term = colnames(winebot10percentpricewords), tfidf = colSums(winebot10percentpricewords))%>%
  arrange(desc(term))%>%
  top_n(10)%>%
  gather(key=weighting_method,value=weight,2:2)%>%
  ggplot(aes(x=reorder(term,weight),y=weight))+
  geom_col(position='dodge')+
  coord_flip()+theme_bw()

######################Clustering Analysis######################
wine_data <- wineclean_description[,-c(1:9)]
str(wine_data)
data_cluster = scale(wine_data)

#distances = dist(data_cluster,method = 'euclidean') does not work

###within SS and elbow method###
within_ss = sapply(1:10,FUN = function(x){
  set.seed(100)
  kmeans(x = data_cluster,centers = x,iter.max = 100)$tot.withinss})
ggplot(data=data.frame(cluster = 1:10,within_ss),aes(x=cluster,y=within_ss))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(1,10,1))+theme_bw()

within_ss

# Does not work
# library(cluster)
# silhoette_width = sapply(2:10,FUN = function(x) pam(x = data_cluster,k = x)$silinfo$avg.width)
# ggplot(data=data.frame(cluster = 2:10,silhoette_width),aes(x=cluster,y=silhoette_width))+
#   geom_line(col='steelblue',size=1.2)+
#   geom_point()+
#   scale_x_continuous(breaks=seq(2,10,1))

###clustering into 6 clusters###

set.seed(100)
km6 = kmeans(x = data_cluster,centers = 6,iter.max=100)
table(km6$cluster)
k_segments = km6$cluster
data1 = cbind(wineclean,wine_data,k_segments)

###group 1####
group1 <- data1[data1$k_segments == 1,]
group1$variety
g1 <- colSums (group1[10:93], na.rm = FALSE, dims = 1)
sort(g1) #spicier, finish, wine, flavor, oak, fruit, cherried, berried, black plum
n1<- group1 %>% count(variety, sort = TRUE) # Pinot Noir, Cabernet Sauvignon,Red Blend
n1$n[1]*100/nrow(group1)
n1$n[2]*100/nrow(group1)
group1 %>% count(country, sort = TRUE) # US (mostly), France
group1 %>% count(province, sort = TRUE) # Cali (mostly), Oregon
group1 %>% count(region_1, sort = TRUE) # Russian River Valley, Napa Valley
mean(group1$price) #$34.66
mean(group1$points) #88.87

###group 2####
group2 <- data1[data1$k_segments == 2,]
group2$variety
g2 <- colSums (group2[10:93], na.rm = FALSE, dims = 1)
sort(g2) #applaud, peach, pear, white, lemon, citrus, fresh, palatability, crisp, acid
n2 <- group2 %>% count(variety, sort = TRUE) # Chardonnay, Riesling, Sauvignon Blanc
n2$n[1]*100/nrow(group2)
n2$n[2]*100/nrow(group2)
group2 %>% count(country, sort = TRUE) # US, Italy
group2 %>% count(province, sort = TRUE) # Cali (mostly), Alsace
group2 %>% count(region_1, sort = TRUE) # Alsace, Finger Lakes
mean(group2$price) #$25.56
mean(group2$points) #$88.14

###group 3####
group3 <- data1[data1$k_segments == 3,]
group3$variety
g3 <- colSums (group3[10:93], na.rm = FALSE, dims = 1)
sort(g3) #wine, age, drink, fruitier, fruit, ripe, structural, rich, will, acid
group3 %>% count(country, sort = TRUE) # France (mostly), US
group3 %>% count(province, sort = TRUE) #Bordeaux, Cali
group3 %>% count(region_1, sort = TRUE) #Champagne, Napa Valley 
n3 <- group3 %>% count(variety, sort = TRUE) # Bordeaux-style Red Blend, Pinot Noir, Chardonnay
n3$n[1]*100/nrow(group3)
n3$n[2]*100/nrow(group3)
mean(group3$price) #$48.39
mean(group3$points) #89.50

###group 4####
group4 <- data1[data1$k_segments == 4,]
group4$variety
g4 <- colSums (group4[10:93], na.rm = FALSE, dims = 1)
sort(g4) #black, palatability, cherried, plum, berried, aroma, tannin, spice, red, pepper
group4 %>% count(country, sort = TRUE) # Italy, US
group4 %>% count(province, sort = TRUE) # Cali, Tuscanny
group4 %>% count(region_1, sort = TRUE) # Barolo, Brunello di Montalcino 
n4 <- group4 %>% count(variety, sort = TRUE) # Red Blend, Nebbiolo, Sangiovese 
n4$n[1]*100/nrow(group4)
n4$n[2]*100/nrow(group4)
mean(group4$price) #$42.34
mean(group4$points) #89.22

###group 5####
group5 <- data1[data1$k_segments == 5,]
group5$variety
g5 <- colSums (group5[10:93], na.rm = FALSE, dims = 1)
sort(g5) #flavor, wine, fruit, sweet, cheried, finish, oak, dribble, soft, blackberried
group5 %>% count(country, sort = TRUE) # US (mostly)
group5 %>% count(province, sort = TRUE) # Cali
group5 %>% count(region_1, sort = TRUE) # Napa Valley, Columbia Valley (WA)
n5 <- group5 %>% count(variety, sort = TRUE) # Pinor Noir, Cabernet Sauvignon, Chardonnay
n5$n[1]*100/nrow(group5)
n5$n[2]*100/nrow(group5)
mean(group5$price) #$34.44
mean(group5$points) #87.78

###group 6####
group6 <- data1[data1$k_segments == 6,]
group6$variety
g6 <- colSums (group6[10:93], na.rm = FALSE, dims = 1)
sort(g6) #cabernet, sauvignon, blend, black, tannin, cherried, blackberried, currant, red, herb
group6 %>% count(country, sort = TRUE) # US (mostly)
group6 %>% count(province, sort = TRUE) # Cali, Washington
group6 %>% count(region_1, sort = TRUE) #Columbia Valley (WA), Napa Valley
n6 <- group6 %>% count(variety, sort = TRUE) # Red Blend, Bordeaux-style Red Blend, Cabernet Sauvignon
n6$n[1]*100/nrow(group6)
n6$n[2]*100/nrow(group6)
mean(group6$price) #$46.89
mean(group6$points) #89.30

###Word clouds generation####
library(wordcloud)
library(RColorBrewer)

matrix <- as.matrix(group1[10:93]) 
words <- sort(colSums(matrix),decreasing=TRUE)
df <- data.frame(word = names(words),freq=words)
wordcloud(words = df$word, freq = df$freq, min.freq = 1,max.words=20, random.order=FALSE, scale=c(2,1),rot.per=0,colors=brewer.pal(8, "Dark2"))

matrix <- as.matrix(group2[10:93]) 
words <- sort(colSums(matrix),decreasing=TRUE)
df <- data.frame(word = names(words),freq=words)
wordcloud(words = df$word, freq = df$freq, min.freq = 1,max.words=20, random.order=FALSE, scale=c(2,1),rot.per=0,colors=brewer.pal(8, "Dark2"))

matrix <- as.matrix(group3[10:93]) 
words <- sort(colSums(matrix),decreasing=TRUE)
df <- data.frame(word = names(words),freq=words)
wordcloud(words = df$word, freq = df$freq, min.freq = 1,max.words=20, random.order=FALSE, scale=c(2,0.5),rot.per=0,colors=brewer.pal(8, "Dark2"))

matrix <- as.matrix(group4[10:93]) 
words <- sort(colSums(matrix),decreasing=TRUE)
df <- data.frame(word = names(words),freq=words)
wordcloud(words = df$word, freq = df$freq, min.freq = 1,max.words=20, random.order=FALSE, scale=c(2,0.5),rot.per=0,colors=brewer.pal(8, "Dark2"))

matrix <- as.matrix(group5[10:93]) 
words <- sort(colSums(matrix),decreasing=TRUE)
df <- data.frame(word = names(words),freq=words)
wordcloud(words = df$word, freq = df$freq, min.freq = 1,max.words=20, random.order=FALSE, scale=c(2,0.5),rot.per=0,colors=brewer.pal(8, "Dark2"))

matrix <- as.matrix(group6[10:93]) 
words <- sort(colSums(matrix),decreasing=TRUE)
df <- data.frame(word = names(words),freq=words)
wordcloud(words = df$word, freq = df$freq, min.freq = 1,max.words=20, random.order=FALSE, scale=c(2,0.5),rot.per=0,colors=brewer.pal(8, "Dark2"))

