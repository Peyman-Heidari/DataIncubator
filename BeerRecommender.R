# Library packages
library(ggplot2); library(LaF); library(stringr); library(tm); library(plyr); 
library(reshape2); library(RCurl); library(XML); library(RColorBrewer)

if (!exists("db")){
    # initialize the text mining dataframe to hold beer name, company, rating on 
    # BeerAdvocate.com, and user reviews(comments).
    db <- data.frame(BeerName = character(), BeerCompany = character(),
                     BARating = double(), Comment = character(),
                     stringsAsFactors = FALSE)
    # initialize the filtering dataframe that holds beer name, user name, and rating given
    # by user to that beer.
    db_recommender <- data.frame(BeerName = character(), Username = character(),
                                 Rating = double(), stringsAsFactors = FALSE)
    # This text file was acquired using a software called linkcheker to extract all
    # links on www.BeerAdvocate.com to mine all links of specific beer.
    download.file(
        "https://raw.githubusercontent.com/Peyman-Heidari/DataIncubator/master/BeerLinks.txt",
        destfile = "./BeerLinks.txt")
    # Read the text of all links
    links1<- read.table("BeerLinks.txt", fill=TRUE)
    # Find links that are relevant to beer ratings and comments.
    link <- as.character(links1[, 3])
    link <- link[grepl("beer/profile", link)]
    link <- link[!grepl("\\?", link)]
    link <- link[grepl("[0-9]+/[0-9]+", link)]
    link <- str_replace_all(link, ",", "")
    link <- unique(link)
    # loop over all links
    for (ii in 1:length(link)){
        # For each link ckeck up to 6 pages of comments that add up to 150 comments.
        for (zz in 0:5){
            theURL <-link[ii]
            extension<- sprintf("?view=beer&sort=&start=%d", zz*25)
            theURL<- paste0(theURL, extension)
            xURL <- getURL(theURL)
            # Mine the html page for specific data.
            beerdoc <- htmlParse(xURL)
            # Find the division that corresponds to the name and company of each beer.
            beername <- xpathSApply(beerdoc, "//*/div[@class='titleBar']", xmlValue)
            if (length(beername)>0 & zz==0){
                beername <- str_replace_all(beername, "\t|\n", "")
                splitednamecom<- str_split_fixed(beername, "\\|", 2)
                # Save the name of the beer in the first column of the text mining database.
                db[ii,1] <- str_trim(splitednamecom[1])
                # Save the name of the company in the second columnof the text mining database.
                db[ii,2] <- str_trim(splitednamecom[2])
            }
            # Find the span of the html file that corresponds to the rating of each beer.
            bascore <- xpathSApply(beerdoc, "//*/span[@ class='BAscore_big ba-score']", xmlValue)
            if (length(bascore)>0 & zz==0){
                # Save the rating of the beer in the third column of the text mining database.
                db[ii,3] <- bascore
            }
            # Find the division of the html file that corresponds to comments submited 
            # by users for each beer.
            comments <- xpathSApply(beerdoc, "//*/div[@class='user-comment']", xmlValue)
            if (length(comments)>0){
                splitedcomments <- str_split_fixed(comments, "overall:", 2)
                splitedcommentsR <- splitedcomments[,2]
                splitedcommentsR <- gsub("[^[:alnum:][:blank:]+?&/\\-]", "", splitedcommentsR)
                splitedcommentsR <- gsub("[[:digit:]]+", "", splitedcommentsR)
                collapsedcomments <- paste (splitedcommentsR, collapse = " ")
                # Append the comments for the same beer on all pages and save them in the 
                # fourth column of the text mining database.
                db[ii,4] <- paste (db[ii,4], collapsedcomments , collapse = " ")
            }
            # Find spans of the html file that corresponds to rating given by specific users. 
            Userrating <- as.numeric(xpathSApply(beerdoc, "//*/span[@ class='BAscore_norm']", xmlValue))
            # Find parts of the html file that corresponds to the usernames the have rated the beer.
            usernames <- xpathSApply(beerdoc, "//*/a[@ class='username']", xmlValue)
            usernames <- usernames[usernames!=""]
            # Initialize a temporary filtering dataframe that gets set to NULL to start collecting data  for each beer.
            db_recommender_temp = NULL
            db_recommender_temp <- data.frame(BeerName=character(), Username=character(),
                                             Rating=double(),stringsAsFactors = FALSE)
            if (length(Userrating)>0 & length(beername)>0 & length(usernames)>0){
                for (jj in 1: length(Userrating)){
                    # Save the name of the beer in the first column of the text filtering database. 
                    db_recommender_temp[jj,1] = str_trim(splitednamecom[1])
                    # Save the username of the reviewer in the second column of the filtering mining database.
                    db_recommender_temp[jj,2] = usernames[jj]
                    # Save the rating given by the reviewer in the third column of the filtering mining database.
                    db_recommender_temp[jj,3] = Userrating[jj]
                }
                # Add the temporary filtering database to the main one.
                db_recommender <- rbind(db_recommender,db_recommender_temp)
            }
        }
        cat(sprintf("Mining the data for the number  %.0f beer out of %.0f.\n",ii,length(link)))
    }
    # Cleaning up the databases for redundant or values and final data cleaning
    db[,4] <- gsub("^NA", "",	db[,4])
    db <- db[!duplicated(db),]
    db_recommender <- db_recommender[!duplicated(db_recommender),]
    Beers <- unique(db_recommender["BeerName"])
    Users <- unique(db_recommender["Username"])
    # Resahping the long filtering dataframe into a matrix of ratings with
    # #of beer rows and #of users columns suitable for collaborative filtering.
    db_filtering<- acast(db_recommender, BeerName~Username, value.var="Rating")
    # Initializing and populating the R matrix with the same dimension as db_filtering
    # and 1 as elements for cases were user-rating pairs are avaialble ad 0 when no rating
    # of a specific beer was given by a specific user (most elements of this matrix are 0).
    R <- matrix(data=0,nrow=dim(Beers)[1],ncol=dim(Users)[1])
    R[!is.na(db_filtering)] <- 1
}
# saving copies of databses
db_copy <- db
db_filtering_copy <- db_filtering
db_recommender_copy <- db_recommender

# Ploting and image of the filtering database showing beer-user combination were ratings
# are avaiable and where ratings are not avaiable. The collaborative filtering slgoithm will
# fill the blank spaces (white).
myPalette <- colorRampPalette(brewer.pal(11, "Spectral"))
q <- ggplot(DataRecomPlot, aes(x=Username, y=BeerName))
q <- q + geom_point(aes(color=Rating), size=0.4)+ scale_colour_gradientn(colours = myPalette(100), limits=c(2,5))
q <- q + xlab( "6,288 Different Users") + ylab("279 Different Beers") 
q <- q + ggtitle(
    "28,333 ratings out of (# of Users * # of Beers =) 1,754,352 possible\n user-beer combinations were mined")
pdf(file="BeerRecommend.pdf", width = 16, height = 9)
plot(q)
dev.off()


# Text cleaning and mining of the comments.
mycorpus <- Corpus(DataframeSource(db["Comment"]))
# Remove very frequent words of english language from comments.
mycorpus <- tm_map(mycorpus, removeWords, stopwords("english"))
# Remove Punctuation.
mycorpus <- tm_map(mycorpus, removePunctuation)
# Remove extra spaces and white spaces.
mycorpus <- tm_map(mycorpus, stripWhitespace)
# Stemmize the words.
mycorpus <- tm_map(mycorpus,stemDocument)
# Form a document term matrix.
dtmr <- DocumentTermMatrix(mycorpus, control=list(wordLengths=c(4, 20)))
# Calculate number of occurence of words in all documents.
freq <- colSums(as.matrix(dtmr))
ord <- order(freq,decreasing=TRUE)

# Devide the text mining database into two databases based the rating given by www.BeerAdvocate.com.
db$RatingLevel <- as.factor(ifelse(db$BARating > 95,"HighQuality", "LowQuality"))
db_high <- db[db$RatingLevel=="HighQuality", ]
db_low <- db[db$RatingLevel=="LowQuality", ]

# Perform all text mining steps on the high rating portion.
mycorpus_high <- Corpus(DataframeSource(db_high["Comment"]))
mycorpus_high <- tm_map(mycorpus_high, removeWords, stopwords("english"))
mycorpus_high <- tm_map(mycorpus_high, removePunctuation)
mycorpus_high <- tm_map(mycorpus_high, stripWhitespace)
mycorpus_high <- tm_map(mycorpus_high, stemDocument)
dtmr_high <- DocumentTermMatrix(mycorpus_high, control=list(wordLengths=c(4, 25)))
freq_high <- colSums(as.matrix(dtmr_high))/sum(sum(dtmr_high))
ord_high <- order(freq_high,decreasing=TRUE)

# Perform all text mining steps on the low rating portion.
mycorpus_low <- Corpus(DataframeSource(db_low["Comment"]))
mycorpus_low <- tm_map(mycorpus_low, removeWords, stopwords("english"))
mycorpus_low <- tm_map(mycorpus_low, removePunctuation)
mycorpus_low <- tm_map(mycorpus_low, stripWhitespace)
mycorpus_low <- tm_map(mycorpus_low, stemDocument)
dtmr_low <- DocumentTermMatrix(mycorpus_low, control=list(wordLengths=c(4, 25)))
freq_low <- colSums(as.matrix(dtmr_low))/sum(sum(dtmr_low))
ord_low <- order(freq_low,decreasing=TRUE)

all_words <- c(names(freq_low[ord_low]),names(freq_high[ord_high]))
similar_words <- all_words[!(duplicated(all_words))]
diff_freq_similar_words <- abs(freq_high[similar_words] - freq_low[similar_words])
order_similar <- order(diff_freq_similar_words, decreasing = TRUE)
important_words <- names(diff_freq_similar_words[order_similar][1:25])


important_word_freq_diff <- data.frame(Words=important_words,
                                       FreqDiff=(freq_high[important_words]-freq_low[important_words]),
                                       stringsAsFactors = FALSE)

# Plot top 25 words with highest frequency difference between the high and low databases.
w <- ggplot(important_word_freq_diff, aes(x=reorder(Words, -FreqDiff), y=FreqDiff))   
w <- w + geom_bar(aes(fill = FreqDiff), position = "dodge", stat="identity")
w <- w +  theme(axis.text.x=element_text(angle=45, hjust=1))
w <- w + xlab( "Words") + ylab("Frequency difference") 
w <- w + ggtitle(
    "Difference between frequencies of words in user comments given to beers grouped into \n highest rated (Rating>0.5) and others (Rating<0.5).")
pdf(file="BeerComments.pdf", width = 16/2, height = 9/2)
plot(w)
dev.off()