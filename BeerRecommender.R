set.seed(222)
library(ggplot2); library(LaF); library(stringr); library(tm); library(plyr); library(reshape2); library(RCurl); library(XML); library(RColorBrewer)
if (!exists("Data")){
  Data<-data.frame(BeerName=character() , BeerCompany=character() , BARating=double() , Comment=character(),stringsAsFactors = FALSE)
  Datarecomender<- data.frame(BeerName=character(), Username=character(), Rating=double(),stringsAsFactors = FALSE)
  download.file("https://www.dropbox.com/s/uv3m5yx01tuq527/BeerLinks.txt?dl=1", destfile = "./BeerLinks.txt")
  links1<- read.table( "BeerLinks.txt", fill=TRUE)
  link<- as.character(links1[,3])
  link<- link[grepl("beer/profile", link)]
  link<- link[!grepl("\\?", link)]
  link<- link[grepl("[0-9]+/[0-9]+", link)]
  link<- str_replace_all(link, ",", "")
  link<- unique(link)
  for (ii in 1:length(link)){
    for (zz in 0:5){
      theURL <-link[ii]
      extension<- sprintf("?view=beer&sort=&start=%d", zz*25)
      theURL<- paste0(theURL, extension)
      xURL <- getURL(theURL)
      beerdoc <- htmlParse(xURL)
      beername <- xpathSApply(beerdoc, "//*/div[@class='titleBar']", xmlValue)
      if (length(beername)>0 & zz==0){
        beername <- str_replace_all(beername, "\t|\n", "")
        splitednamecom<- str_split_fixed(beername, "\\|", 2)
        Data[ii,1]<- str_trim(splitednamecom[1])
        Data[ii,2]<- str_trim(splitednamecom[2])
     }
     bascore<- xpathSApply(beerdoc, "//*/span[@ class='BAscore_big ba-score']", xmlValue)
     if (length(bascore)>0 & zz==0){Data[ii,3]<-bascore }
     comments<-xpathSApply(beerdoc, "//*/div[@class='user-comment']", xmlValue)
     if (length(comments)>0){
       splitedcomments<- str_split_fixed(comments, "overall:", 2)
       splitedcommentsR<- splitedcomments[,2]
       splitedcommentsR<- gsub("[^[:alnum:][:blank:]+?&/\\-]", "", splitedcommentsR)
       splitedcommentsR<- gsub("[[:digit:]]+", "", splitedcommentsR)
       collapsedcomments<- paste (splitedcommentsR, collapse = " ")
       Data[ii,4]<-paste (Data[ii,4], collapsedcomments , collapse = " ")
     }
     Userrating<- as.numeric(xpathSApply(beerdoc, "//*/span[@ class='BAscore_norm']", xmlValue))
     usernames<- xpathSApply(beerdoc, "//*/a[@ class='username']", xmlValue)
     usernames<- usernames[usernames!=""]
     Datarecomender2=NULL
     Datarecomender2<- data.frame(BeerName=character(), Username=character(), Rating=double(),stringsAsFactors = FALSE)
     if (length(Userrating)>0 &length(beername)>0 & length(usernames)>0){
       for (jj in 1: length(Userrating)){Datarecomender2[jj,1]=str_trim(splitednamecom[1]); Datarecomender2[jj,2]= usernames[jj]; Datarecomender2[jj,3]= Userrating[jj]}
       Datarecomender<- rbind(Datarecomender,Datarecomender2)
     }
     print(ii)
   }
  }
  Data[,4]<- gsub("^NA", "",  Data[,4])
  Data<- Data[!duplicated(Data),]
  Datarecomender<- Datarecomender[!duplicated(Datarecomender),]
  Beers<- unique(Datarecomender["BeerName"])
  Users<- unique(Datarecomender["Username"])
  RatingUsers<- acast(Datarecomender, BeerName~Username, value.var="Rating")
  R<- matrix(data=0,nrow=dim(Beers)[1],ncol=dim(Users)[1])
  R[!is.na(RatingUsers)]<-1
}
Dataref<- Data
RatingUsersref<-RatingUsers

DataRecomPlot<- Datarecomender

myPalette <- colorRampPalette(brewer.pal(11, "Spectral"))
q <- ggplot(DataRecomPlot, aes(x=Username, y=BeerName))
q <- q + geom_point(aes(color=Rating), size=0.4)+ scale_colour_gradientn(colours = myPalette(100), limits=c(2,5))
q <- q + xlab( "6,288 Different Users") + ylab("279 Different Beers") 
q <- q + ggtitle("28,333 ratings out of (# of Users * # of Beers =) 1,754,352 possible\n user-beer combinations were mined")
pdf(file="BeerRecommend.pdf", width = 16/2, height = 9/2)
plot(q)
dev.off()



mycorpus <- Corpus(DataframeSource(Data["Comment"]))
mycorpus <- tm_map(mycorpus, removeWords, stopwords("english"))
mycorpus <- tm_map(mycorpus, removePunctuation)
mycorpus <- tm_map(mycorpus, stripWhitespace)
mycorpus <- tm_map(mycorpus,stemDocument)
dtmr <- DocumentTermMatrix(mycorpus, control=list(wordLengths=c(4, 20)))
freq <- colSums(as.matrix(dtmr))
ord <- order(freq,decreasing=TRUE)

Data$RatingLevel <- as.factor(ifelse(Data$BARating > 95,"HighQuality", "LowQuality"))
DataHigh<- Data[Data$RatingLevel=="HighQuality", ]
DataLow<- Data[Data$RatingLevel=="LowQuality", ]

mycorpusHigh <- Corpus(DataframeSource(DataHigh["Comment"]))
mycorpusHigh <- tm_map(mycorpusHigh, removeWords, stopwords("english"))
mycorpusHigh <- tm_map(mycorpusHigh, removePunctuation)
mycorpusHigh <- tm_map(mycorpusHigh, stripWhitespace)
mycorpusHigh <- tm_map(mycorpusHigh, stemDocument)
dtmrHigh <- DocumentTermMatrix(mycorpusHigh, control=list(wordLengths=c(4, 25)))
freqHigh <- colSums(as.matrix(dtmrHigh))/sum(sum(dtmrHigh))
ordHigh <- order(freqHigh,decreasing=TRUE)

mycorpusLow <- Corpus(DataframeSource(DataLow["Comment"]))
mycorpusLow <- tm_map(mycorpusLow, removeWords, stopwords("english"))
mycorpusLow <- tm_map(mycorpusLow, removePunctuation)
mycorpusLow <- tm_map(mycorpusLow, stripWhitespace)
mycorpusLow <- tm_map(mycorpusLow, stemDocument)
dtmrLow <- DocumentTermMatrix(mycorpusLow, control=list(wordLengths=c(4, 25)))
freqLow <- colSums(as.matrix(dtmrLow))/sum(sum(dtmrLow))
ordLow <- order(freqLow,decreasing=TRUE)

allwords <- c(names(freqLow[ordLow]),names(freqHigh[ordHigh]))
similarwords <- allwords[!(duplicated(allwords))]
differencefreqsimilarwords<- abs(freqHigh[similarwords] - freqLow[similarwords])
ordersimilar <- order(differencefreqsimilarwords, decreasing = TRUE)
impwords <- names(differencefreqsimilarwords[ordersimilar][1:25])

#ImportantWordFreq <- data.frame(Words=impwords, HighQualityFreq=freqHigh[impwords], LowQualityFreq=freqLow[impwords], stringsAsFactors = FALSE)
#data.m <- melt(ImportantWordFreq, id.vars='Words')
#g <- ggplot(data.m, aes(x=reorder(Words, -value*(data.m$variable=="HighQualityFreq")), y=value))   
#g <- g + geom_bar(aes(fill = variable), position = "dodge", stat="identity")
#g <- g +  theme(axis.text.x=element_text(angle=45, hjust=1))

ImportantWordFreqDiff <- data.frame(Words=impwords, FreqDiff=(freqHigh[impwords]-freqLow[impwords]), stringsAsFactors = FALSE)
w <- ggplot(ImportantWordFreqDiff, aes(x=reorder(Words, -FreqDiff), y=FreqDiff))   
w <- w + geom_bar(aes(fill = FreqDiff), position = "dodge", stat="identity")
w <- w +  theme(axis.text.x=element_text(angle=45, hjust=1))
w <- w + xlab( "Words") + ylab("Frequency difference") 
w <- w + ggtitle("Difference between frequencies of words in user comments given to beers grouped into \n highest rated (Rating>0.5) and others (Rating<0.5).")
pdf(file="BeerComments.pdf", width = 16/2, height = 9/2)
plot(w)
dev.off()