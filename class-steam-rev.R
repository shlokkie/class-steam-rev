preprocessing <- function(seed = 6) {
	set.seed(seed)
	genres <- list.files("./dados-originais")
	sampling <- c(rep(1, 52), rep(0, 23))
	for (genre in genres) {
		games <- list.files(file.path("./dados-originais/", genre, fsep = ""))
		for(game in games) {
			reviews1 <- list.files(file.path("./dados-originais/", genre, "/", game, fsep = ""))
			i <- 0
			ordering <- sample(sampling, size = 75, replace = FALSE)
			for(steam_id in reviews1) {
				content <- readLines(file.path("./dados-originais/", genre, "/", game, "/", steam_id, fsep = ""), warn = FALSE)
				content <- gsub("-", " ", content)
				content <- gsub("â™¥", " ", content)
				content <- gsub("â€“", " ", content)
				content <- gsub("â€™", "'", content)
				content <- gsub("â€œ", "\"", content)
				content <- gsub("â€\u008d", "", content)
				content <- gsub("â€\u009d", "\"", content)
				content <- gsub("â˜…", "", content)
				content <- gsub("â˜†", "", content)
				content <- gsub("â€˜", "\'", content)
				content <- gsub("X COM", "XCOM", content)
				content <- gsub("x com", "xcom", content)

				i <- i + 1
				if(ordering[i] == 1) {
					writeLines(content, file.path("./Data - TM/", genre, " train/", paste(game, steam_id), fsep = ""))
				}
				else if(ordering[i] == 0) {
					writeLines(content, file.path("./Data - TM/", genre, " test/", paste(game, steam_id), fsep = ""))
				}
			}
		}
	}
}

preprocessing()

#### foi usado para reconhecer termos frequentes irrelevantes
horror.train <- list()
for(i in 1:length(hor.train)) {
	new <- c()
    for(j in 1: length(hor.train[[i]][[1]])) {
        new <- c(new, unlist(strsplit(hor.train[[i]][[1]][j], " ")))
      }
	new <- new[-which(new == "")]
	horror.train <- c(horror.train, list(new))
}
baghorror <- unlist(horror.train)
sort(table(baghorror),decreasing = TRUE)[1:50]

strategy.train <- list()
for(i in 1:length(strat.train)) {
	new <- c()
    for(j in 1: length(strat.train[[i]][[1]])) {
        new <- c(new, unlist(strsplit(strat.train[[i]][[1]][j], " ")))
      }
	new <- new[-which(new == "")]
	strategy.train <- c(strategy.train, list(new))
}
bagstrat <- unlist(strategy.train)
sort(table(bagstrat),decreasing = TRUE)[1:50]
###


library(tm)
useless.words <- c(stopwords(kind = "en"), "one", "game", "can", "play", "will", "like", "get", "time", "make", "just", "also", "even", "amnesia", "outlast", "soma", "dreadout", "take", "good", "look", "endless", "total", "sid", "meier", "meiers", "legend", "rome", "xcom", "civ", "horror", "strategy", "turn", "based", "gameplay") 




# Preprocessing using the tm package:
##Horror
hor.test <- Corpus(DirSource("Data - TM/Horror test"), readerControl=list(reader = readPlain,language="en"))
hor.test <- tm_map(hor.test, content_transformer(tolower))
hor.test <- tm_map(hor.test, removeWords, useless.words)
hor.test <- tm_map(hor.test, removePunctuation)
hor.test <- tm_map(hor.test, removeNumbers)
hor.test <- tm_map(hor.test, stemDocument)
hor.test <- tm_map(hor.test, removeWords, useless.words)
hor.test <- tm_map(hor.test, stripWhitespace)

hor.train <- Corpus(DirSource("Data - TM/Horror train"), readerControl=list(reader = readPlain,language="en"))
hor.train <- tm_map(hor.train, content_transformer(tolower))
hor.train <- tm_map(hor.train, removeWords, useless.words)
hor.train <- tm_map(hor.train, removePunctuation)
hor.train <- tm_map(hor.train, removeNumbers)
hor.train <- tm_map(hor.train, stemDocument)
hor.train <- tm_map(hor.train, removeWords, useless.words)
hor.train <- tm_map(hor.train, stripWhitespace)


###Strategy:
strat.test <- Corpus(DirSource("Data - TM/Strategy test"), readerControl=list(reader = readPlain,language="en"))
strat.test <- tm_map(strat.test, content_transformer(tolower))
strat.test <- tm_map(strat.test, removeWords, useless.words)
strat.test <- tm_map(strat.test, removePunctuation)
strat.test <- tm_map(strat.test, removeNumbers)
strat.test <- tm_map(strat.test, stemDocument)
strat.test <- tm_map(strat.test, removeWords, useless.words)
strat.test <- tm_map(strat.test, stripWhitespace)

strat.train <- Corpus(DirSource("Data - TM/Strategy train"), readerControl=list(reader = readPlain,language="en"))
strat.train <- tm_map(strat.train, content_transformer(tolower))
strat.train <- tm_map(strat.train, removeWords, useless.words)
strat.train <- tm_map(strat.train, removePunctuation)
strat.train <- tm_map(strat.train, removeNumbers)
strat.train <- tm_map(strat.train, stemDocument)
strat.train <- tm_map(strat.train, removeWords, useless.words)
strat.train <- tm_map(strat.train, stripWhitespace)


#Break the sample into 2 groups - train and test
reviews <- c(hor.train, strat.train)
testreviews <- c(hor.test, strat.test)

#Defaults:
#Why keep the defaults:
# 1) we don't want to remove any words that are shorter than 3 characters, as they make up for a lot of the frequent and relevant terms E.g. "map", "war", "run"
# 2) we used the default bounds, meaning the number of documents a term appears in, as 1, since it's not a problem for now that it only appears in one, since they will be trimmed out later when we Remove Sparse Terms which already works with percentages.
# 3) Weighting - this is a bit trickier. There are 4 different options for this. We chose first the weightTf option because it will make the findFreqTerms() more relevant: (we are not sure why is that, but it has something to do with how the frequent terms are calculated) 
#using the tfidf we get no results in the freqterms100:
#findFreqTerms(dtm.idf, 100)
#character(0)
#findFreqTerms(dtm.idf, 5)
#[1] "stori"


#dtm <- DocumentTermMatrix(reviews, control=list(weighting = weightTfIdf, wordLengths = c(3, Inf), bounds = list(local = c(1, Inf))))
dtm <- DocumentTermMatrix(reviews)
dtm.idf <- DocumentTermMatrix(reviews, control=list(weighting = weightTfIdf))

#finding the freqent terms for all documents
freqterms100 <- findFreqTerms(dtm, 100)
freqterms100
#  [1] "act"       "actual"    "armi"      "around"    "atmospher" "battl"     "best"     
#  [8] "build"     "buy"       "citi"      "come"      "complet"   "dark"      "design"   
# [15] "differ"    "end"       "enemi"     "enjoy"     "everi"     "experi"    "faction"  
# [22] "feel"      "find"      "first"     "give"      "graphic"   "great"     "hour"     
# [29] "howev"     "know"      "littl"     "lot"       "mani"      "map"       "much"     
# [36] "need"      "never"     "new"       "now"       "peopl"     "player"    "realli"   
# [43] "recommend" "right"     "run"       "say"       "scare"     "see"       "soldier"  
# [50] "someth"    "sound"     "start"     "still"     "stori"     "system"    "thing"    
# [57] "think"     "tri"       "unit"      "use"       "want"      "war"       "way"      
# [64] "well"      "work"      "world"  
table(as.matrix(dtm)[,"act"])
#  0   1   2   3   4   5   6   8   9  26 
#369  26  12   2   1   2   1   1   1   1 
# This means "act" appears 0 times on 369 documents, 1 time on 26 documents, ... , 26 times in 1 document.
sum(table(as.matrix(dtm)[,"act"]))
#[1] 416
26+12*2+2*3+4+5*2+6+8+9+26
#[1] 119

#??removeSparseTerms
#the resulting matrix contains only terms with a sparse factor of less than sparse. Example: sparse = 0.95 (95%)
#This means that the resulting matrix will have less than 95% (of the number of documents) of zeros (no occurrences)
#Example "act":
#369 documents with no occurrences of the word "act", from 416 documents
#In percentage:
#369/416
#[1] 0.8870192
#1 - 369/416
#[1] 0.1129808
#Sparsity: 88% (88% of the documents have no occurrences of the word "act")
#The higher the value of S, the higher the number of terms that will be kept. For example, if we change the value from 0.95 to 0.99, we are allowing more 0s in the matrix, so the number of terms/words will be higher
#By doing this, we are excluding any term that appears in too few documents (regardless of how many times it occurs in all documents)
#Combining with findFreqTerms() we can exclude any term that appears many times but in too few documents. Example: "SHIT SHIT"
# table(as.matrix(dtm.mx)[,"shit"])
#   0  16 
# 415   1 

#Sparse terms and class
dtm95 <- as.data.frame(inspect(removeSparseTerms(dtm, 0.95)))
dtm90 <- as.data.frame(inspect(removeSparseTerms(dtm, 0.90)))

dtm.idf95 <- as.data.frame(inspect(removeSparseTerms(dtm.idf, 0.95)))
dtm.idf90 <- as.data.frame(inspect(removeSparseTerms(dtm.idf, 0.90)))

dtm95 <- cbind(dtm95, genre_class = c(rep("horror", 208), rep("strategy", 208)))
dtm90 <- cbind(dtm90, genre_class = c(rep("horror", 208), rep("strategy", 208)))

dtm.idf95 <- cbind(dtm.idf95, genre_class = c(rep("horror", 208), rep("strategy", 208)))
dtm.idf90 <- cbind(dtm.idf90, genre_class = c(rep("horror", 208), rep("strategy", 208)))

#Checking if the attributes chosen with the different weights are in the end the same
# all(names(dtm90) == names(dtm.idf90))
# [1] TRUE
# all(names(dtm95) == names(dtm.idf95))
# [1] TRUE

#To make the test set into a data frame
dtm.te <- DocumentTermMatrix(testreviews)
dtm.te.idf <- DocumentTermMatrix(testreviews, control=list(weighting = weightTfIdf))
#default weight - test
terms95 <- names(dtm95)[-length(names(dtm95))]
dtm.te95 <- dtm.te[,terms95]
dtm.te95 <- as.data.frame(inspect(dtm.te95))
dtm.te95 <- cbind(dtm.te95, genre_class = c(rep("horror", 92), rep("strategy", 92)))

terms90 <- names(dtm90)[-length(names(dtm90))]
dtm.te90 <- dtm.te[,terms90]
dtm.te90 <- as.data.frame(inspect(dtm.te90))
dtm.te90 <- cbind(dtm.te90, genre_class = c(rep("horror", 92), rep("strategy", 92)))

#tfidf weight - test
dtm.te.idf95 <- dtm.te.idf[,terms95]
dtm.te.idf95 <- as.data.frame(inspect(dtm.te.idf95))
dtm.te.idf95 <- cbind(dtm.te.idf95, genre_class = c(rep("horror", 92), rep("strategy", 92)))

dtm.te.idf90 <- dtm.te.idf[,terms90]                        
dtm.te.idf90 <- as.data.frame(inspect(dtm.te.idf90))        
dtm.te.idf90 <- cbind(dtm.te.idf90, genre_class = c(rep("horror", 92), rep("strategy", 92)))




###INFORMATION GAIN
library(FSelector)
#No sparse condition
dtm.ig <- as.data.frame(inspect(dtm))
dtm.idf.ig <- as.data.frame(inspect(dtm.idf))

#Combined with sparse:
dtm.ig95 <- as.data.frame(inspect(removeSparseTerms(dtm, 0.95)))
dtm.ig90 <- as.data.frame(inspect(removeSparseTerms(dtm, 0.90)))

dtm.idf.ig95 <- as.data.frame(inspect(removeSparseTerms(dtm.idf, 0.95)))
dtm.idf.ig90 <- as.data.frame(inspect(removeSparseTerms(dtm.idf, 0.90)))

#Adding class column:
dtm.ig <- cbind(dtm.ig, genre_class = c(rep("horror", 208), rep("strategy", 208)))
dtm.idf.ig <- cbind(dtm.idf.ig, genre_class = c(rep("horror", 208), rep("strategy", 208)))

dtm.ig95 <- cbind(dtm.ig95, genre_class = c(rep("horror", 208), rep("strategy", 208)))
dtm.ig90 <- cbind(dtm.ig90, genre_class = c(rep("horror", 208), rep("strategy", 208)))

dtm.idf.ig95 <- cbind(dtm.idf.ig95, genre_class = c(rep("horror", 208), rep("strategy", 208)))
dtm.idf.ig90 <- cbind(dtm.idf.ig90, genre_class = c(rep("horror", 208), rep("strategy", 208)))

#To check number of terms that we will have before and after info gain:
inf <- information.gain(genre_class ~., dtm.ig)
length(inf$attr_importance)
#[1] 5780
length(which(inf$attr_importance != 0))
#[1] 95
#Check terms and the info gain:
inf[which(inf$attr_importance != 0),, drop = FALSE]

#Apply information gain:
dtm.ig <- dtm.ig[, c(which(information.gain(genre_class ~., dtm.ig)$attr_importance > 0.03), ncol(dtm.ig))]
dtm.idf.ig <- dtm.idf.ig[, c(which(information.gain(genre_class ~., dtm.idf.ig)$attr_importance > 0.03), ncol(dtm.idf.ig))]

dtm.ig95 <- dtm.ig95[, c(which(information.gain(genre_class ~., dtm.ig95)$attr_importance > 0.03), ncol(dtm.ig95))]
dtm.ig90 <- dtm.ig90[, c(which(information.gain(genre_class ~., dtm.ig90)$attr_importance > 0.03), ncol(dtm.ig90))]

dtm.idf.ig95 <- dtm.idf.ig95[, c(which(information.gain(genre_class ~., dtm.idf.ig95)$attr_importance > 0.03), ncol(dtm.idf.ig95))]
dtm.idf.ig90 <- dtm.idf.ig90[, c(which(information.gain(genre_class ~., dtm.idf.ig90)$attr_importance > 0.03), ncol(dtm.idf.ig90))]


#INFO GAIN Test sets:
dtm.te <- DocumentTermMatrix(testreviews)
dtm.te.idf <- DocumentTermMatrix(testreviews, control=list(weighting = weightTfIdf))

terms.ig <- names(dtm.ig)[-length(names(dtm.ig))]
dtm.te.ig <- dtm.te[,terms.ig]
dtm.te.ig <- as.data.frame(inspect(dtm.te.ig))
dtm.te.ig <- cbind(dtm.te.ig, genre_class = c(rep("horror", 92), rep("strategy", 92)))

terms.idf.ig <- names(dtm.idf.ig)[-length(names(dtm.idf.ig))]
dtm.te.idf.ig <- dtm.te.idf[,terms.idf.ig]
dtm.te.idf.ig <- as.data.frame(inspect(dtm.te.idf.ig))
dtm.te.idf.ig <- cbind(dtm.te.idf.ig, genre_class = c(rep("horror", 92), rep("strategy", 92)))

terms.ig95 <- names(dtm.ig95)[-length(names(dtm.ig95))]
dtm.te.ig95 <- dtm.te[,terms.ig95]
dtm.te.ig95 <- as.data.frame(inspect(dtm.te.ig95))
dtm.te.ig95 <- cbind(dtm.te.ig95, genre_class = c(rep("horror", 92), rep("strategy", 92)))

terms.idf.ig95 <- names(dtm.idf.ig95)[-length(names(dtm.idf.ig95))]
dtm.te.idf.ig95 <- dtm.te.idf[,terms.idf.ig95]
dtm.te.idf.ig95 <- as.data.frame(inspect(dtm.te.idf.ig95))
dtm.te.idf.ig95 <- cbind(dtm.te.idf.ig95, genre_class = c(rep("horror", 92), rep("strategy", 92)))

terms.ig90 <- names(dtm.ig90)[-length(names(dtm.ig90))]
dtm.te.ig90 <- dtm.te[,terms.ig90]
dtm.te.ig90 <- as.data.frame(inspect(dtm.te.ig90))
dtm.te.ig90 <- cbind(dtm.te.ig90, genre_class = c(rep("horror", 92), rep("strategy", 92)))

terms.idf.ig90 <- names(dtm.idf.ig90)[-length(names(dtm.idf.ig90))]
dtm.te.idf.ig90 <- dtm.te.idf[,terms.idf.ig90]
dtm.te.idf.ig90 <- as.data.frame(inspect(dtm.te.idf.ig90))
dtm.te.idf.ig90 <- cbind(dtm.te.idf.ig90, genre_class = c(rep("horror", 92), rep("strategy", 92)))



###Just trying this out:
#To make it easier to choose the data frame
train <-  list(dtm.ig, dtm.ig90, dtm.ig95, dtm.idf.ig, dtm.idf.ig90, dtm.idf.ig95)
#To make it easier to create the data frames
inf.gain <- function(train, weight = "tf", min.inf = 0) {
	library(FSelector)
	newig <<- train[, c(which(information.gain(genre_class ~., train)$attr_importance > min.inf), ncol(train))]
	newigterms <<- names(newig)[-length(names(newig))]
	if(weight == "tf") {
		newig.te <<- dtm.te[,newigterms]
	} else if (weight == "tfidf") {
		newig.te <<- dtm.te.idf[,newigterms]
	}
	newig.te <<- as.data.frame(inspect(newig.te))
	newig.te <<- cbind(newig.te, genre_class = c(rep("horror", 92), rep("strategy", 92)))
}
#Example:
#inf.gain(train[[1]], "tf", 0.05)
run.model(newig, newig.te, "rpart")
run.model(newig, newig.te, "knn")



#Classification Models
library(caret)
#	method = "nb" - Naive Bayes
#	method = "rpart" - Decision Tree
#	method = "knn" - KNN
#	method = "nnet" - Neural Networks
#	method = "svmRadial" - SVM with Radial Kernel (there's more)
#	or SVM: library(e1071); svm(), default is also Radial Kernel

###dtm90
#DECISION TREE
# mo.dt90 <- train(genre_class ~ ., data = dtm90, method = "rpart")
set.seed(5465)
mo.dt90 <- train(dtm90[,-ncol(dtm90)], dtm90[, ncol(dtm90)], method = "rpart")


###dtm95
#DECISION TREE
set.seed(5465)
mo.dt95 <- train(dtm95[,-ncol(dtm95)], dtm95[,ncol(dtm95)], method = "rpart")


###dtm.idf90
#DECISION TREE
set.seed(5465)
mo.dt.idf90 <- train(dtm.idf90[,-ncol(dtm.idf90)], dtm.idf90[,ncol(dtm.idf90)], method = "rpart")


###dtm.idf95
#DECISION TREE
set.seed(5465)
mo.dt.idf95 <- train(dtm.idf95[,-ncol(dtm.idf95)], dtm.idf95[,ncol(dtm.idf95)], method = "rpart")

#confusion matrices
table(dtm.te90$genre_class, predict(mo.dt90, dtm.te90))
table(dtm.te95$genre_class, predict(mo.dt95, dtm.te95))
table(dtm.te.idf90$genre_class, predict(mo.dt.idf90, dtm.te.idf90))
table(dtm.te.idf95$genre_class, predict(mo.dt.idf95, dtm.te.idf95))


###dtm90
#KNN
set.seed(5465)
mo.knn90 <- train(dtm90[,-ncol(dtm90)], dtm90[, ncol(dtm90)], method = "knn")

###dtm95
#KNN
set.seed(5465)
mo.knn95 <- train(dtm95[,-ncol(dtm95)], dtm95[,ncol(dtm95)], method = "knn")

###dtm.idf90
#KNN
set.seed(5465)
mo.knn.idf90 <- train(dtm.idf90[,-ncol(dtm.idf90)], dtm.idf90[,ncol(dtm.idf90)], method = "knn")

###dtm.idf95
#KNN
set.seed(5465)
mo.knn.idf95 <- train(dtm.idf95[,-ncol(dtm.idf95)], dtm.idf95[,ncol(dtm.idf95)], method = "knn")

#confusion matrices
#because of the way the knn works, and specially how it solves ties in the data to make prediction, which is random, we must set a seed in order to get the same predictions everytime.
set.seed(5465)
table(dtm.te90$genre_class, predict(mo.knn90, dtm.te90[,-ncol(dtm.te90)]))
set.seed(5465)
table(dtm.te95$genre_class, predict(mo.knn95, dtm.te95[,-ncol(dtm.te95)]))
set.seed(5465)
table(dtm.te.idf90$genre_class, predict(mo.knn.idf90, dtm.te.idf90[,-ncol(dtm.te.idf90)]))
set.seed(5465)
table(dtm.te.idf95$genre_class, predict(mo.knn.idf95, dtm.te.idf95[,-ncol(dtm.te.idf95)]))

##KNN package 'class'
###dtm90
#knn(train, test, cl, k = 1, l = 0, prob = FALSE, use.all = TRUE)
##knn(dtm90[, -ncol(dtm90)], dtm.te90[,-ncol(dtm.te90)], dtm90$genre_class, k= 9)
##We used this different package (from the slides), in order to test if we would get different predictions as well, which we did, so it's not a problem of the package, just how knn works.


###dtm90
#Neural Networks
set.seed(5465)
mo.nnet90 <- train(dtm90[,-ncol(dtm90)], dtm90[, ncol(dtm90)], method = "nnet")

###dtm95
#Neural Networks
set.seed(5465)
mo.nnet95 <- train(dtm95[,-ncol(dtm95)], dtm95[,ncol(dtm95)], method = "nnet")

###dtm.idf90
#Neural Networks
set.seed(5465)
mo.nnet.idf90 <- train(dtm.idf90[,-ncol(dtm.idf90)], dtm.idf90[,ncol(dtm.idf90)], method = "nnet")

###dtm.idf95
#Neural Networks
set.seed(5465)
mo.nnet.idf95 <- train(dtm.idf95[,-ncol(dtm.idf95)], dtm.idf95[,ncol(dtm.idf95)], method = "nnet")

mo.nnet90
mo.nnet95
mo.nnet.idf90
mo.nnet.idf95

#confusion matrices
table(dtm.te90$genre_class, predict(mo.nnet90, dtm.te90[,-ncol(dtm.te90)]))
table(dtm.te95$genre_class, predict(mo.nnet95, dtm.te95[,-ncol(dtm.te95)]))
table(dtm.te.idf90$genre_class, predict(mo.nnet.idf90, dtm.te.idf90[,-ncol(dtm.te.idf90)]))
table(dtm.te.idf95$genre_class, predict(mo.nnet.idf95, dtm.te.idf95[,-ncol(dtm.te.idf95)]))

###dtm90
#SVM
set.seed(5465)
mo.svm90 <- train(dtm90[,-ncol(dtm90)], dtm90[, ncol(dtm90)], method = "svmRadial")

###dtm95
#SVM
set.seed(5465)
mo.svm95 <- train(dtm95[,-ncol(dtm95)], dtm95[,ncol(dtm95)], method = "svmRadial")

###dtm.idf90
#SVM
set.seed(5465)
mo.svm.idf90 <- train(dtm.idf90[,-ncol(dtm.idf90)], dtm.idf90[,ncol(dtm.idf90)], method = "svmRadial")

###dtm.idf95
#SVM
set.seed(5465)
mo.svm.idf95 <- train(dtm.idf95[,-ncol(dtm.idf95)], dtm.idf95[,ncol(dtm.idf95)], method = "svmRadial")

mo.svm90
mo.svm95
mo.svm.idf90
mo.svm.idf95

#confusion matrices
table(dtm.te90$genre_class, predict(mo.svm90, dtm.te90[,-ncol(dtm.te90)]))
table(dtm.te95$genre_class, predict(mo.svm95, dtm.te95[,-ncol(dtm.te95)]))
table(dtm.te.idf90$genre_class, predict(mo.svm.idf90, dtm.te.idf90[,-ncol(dtm.te.idf90)]))
table(dtm.te.idf95$genre_class, predict(mo.svm.idf95, dtm.te.idf95[,-ncol(dtm.te.idf95)]))

##NAIVE BAYES
library(RWeka)

NB <- make_Weka_classifier("weka/classifiers/bayes/NaiveBayes")
###dtm90
set.seed(5465)
mo.nb90 <- NB(genre_class ~., dtm90)
table(dtm90$genre_class, predict(mo.nb90, dtm90[,-ncol(dtm90)]))

###dtm95
set.seed(5465)
mo.nb95 <- NB(genre_class ~., dtm95)
table(dtm95$genre_class, predict(mo.nb95, dtm95[,-ncol(dtm95)]))

###dtm.idf90
set.seed(5465)
mo.nb.idf90 <- NB(genre_class ~., dtm.idf90)
table(dtm.idf90$genre_class, predict(mo.nb.idf90, dtm.idf90[,-ncol(dtm.idf95)]))
###dtm.idf90
set.seed(5465)
mo.nb.idf95 <- NB(genre_class ~., dtm.idf95)
table(dtm.idf95$genre_class, predict(mo.nb.idf95, dtm.idf95[,-ncol(dtm.idf95)]))

#confusion matrices
table(dtm.te90$genre_class, predict(mo.nb90, dtm.te90[,-ncol(dtm.te90)]))
table(dtm.te95$genre_class, predict(mo.nb95, dtm.te95[,-ncol(dtm.te95)]))
table(dtm.te.idf90$genre_class, predict(mo.nb.idf90, dtm.te.idf90[,-ncol(dtm.te.idf90)]))
table(dtm.te.idf95$genre_class, predict(mo.nb.idf95, dtm.te.idf95[,-ncol(dtm.te.idf95)]))




##Now to find out the misclassified reviews:
#We will only look into the better performing models: mo.nb.idf90, mo.nb.idf95, mo.nnet.idf95, mo.svm.idf95.

#NB - idf95
rownames(dtm.te.idf95[which(dtm.te.idf95$genre_class != predict(mo.nb.idf95, dtm.te.idf95[,-ncol(dtm.te.idf95)])) , ])
# [1] "Amnesia 28 Snake.txt"              "Amnesia 66 Aridere.txt"            "DreadOut 27 Ragnarokgc.txt"        "DreadOut 50 Commander Kotori.txt" 
# [5] "Outlast 48 mostly nice.txt"        "Outlast 53 straightdopeorbit .txt" "Outlast 55 Greta.txt"              "Civ5 74 DalekSupremacy.txt"       
# [9] "Endless Legend 63 Philosfr.txt"    "XCOM 4 MrBubbles.txt"              "XCOM 50 Salty.txt"                 "XCOM 52 badassgrunt.txt" 

#NNET - idf95
rownames(dtm.te.idf95[which(dtm.te.idf95$genre_class != predict( mo.nnet.idf95, dtm.te.idf95[,-ncol(dtm.te.idf95)])) , ])
# [1] "Amnesia 2 King Spodes.txt"        "DreadOut 50 Commander Kotori.txt" "DreadOut 6 Virtual.txt"           "Outlast 47 Virtual.txt"          
# [5] "Outlast 55 Greta.txt"             "Civ5 33 brampage.txt"             "Civ5 40 Clam Bake.txt"            "XCOM 4 MrBubbles.txt"            
# [9] "XCOM 50 Salty.txt"

#svmRadial - idf95
rownames(dtm.te.idf95[which(dtm.te.idf95$genre_class != predict( mo.svm.idf95, dtm.te.idf95[,-ncol(dtm.te.idf95)])) , ])
# [1] "Amnesia 2 King Spodes.txt"         "Amnesia 28 Snake.txt"              "Amnesia 65 Daat One Guy.txt"       "DreadOut 11 Lizten.txt"           
# [5] "Outlast 47 Virtual.txt"            "Outlast 53 straightdopeorbit .txt" "Outlast 55 Greta.txt"              "Civ5 33 brampage.txt"             
# [9] "Civ5 72 Reyals.txt"                "Civ5 74 DalekSupremacy.txt"        "Endless Legend 59 maestro35.txt"   "XCOM 4 MrBubbles.txt" 

#NB - idf90
rownames(dtm.te.idf90[which(dtm.te.idf90$genre_class != predict(mo.nb.idf90, dtm.te.idf90[,-ncol(dtm.te.idf90)])) , ])
# [1] "Amnesia 10 WarriorVet.txt"         "Amnesia 32 HyDraWx.txt"            "Amnesia 58 jefequeso.txt"          "Amnesia 66 Aridere.txt"           
# [5] "DreadOut 11 Lizten.txt"            "Outlast 14 arvix8.txt"             "Outlast 53 straightdopeorbit .txt" "Outlast 55 Greta.txt"             
# [9] "Civ5 40 Clam Bake.txt"             "XCOM 10 Taberone.txt"              "XCOM 4 MrBubbles.txt"              "XCOM 52 badassgrunt.txt"






#Run model function:
run.model <- function(train, test, method, seed = "5465", return_model = FALSE) {
	if(method == "nb") {
		library(RWeka)
		set.seed(seed)
		NB <- make_Weka_classifier("weka/classifiers/bayes/NaiveBayes")
		model <- NB(genre_class ~., train)
		cat("\n", "Training set confusion matrix:", sep = "")
		print(table(train[, ncol(train)], predict(model, train[,-ncol(train)])))
		cat("\n", "Test set confusion matrix:", sep = "")
		print(table(test[, ncol(test)], predict(model, test[,-ncol(test)])))
	} else {
		library(caret)
		set.seed(seed)
		model <- train(train[,-ncol(train)], train[, ncol(train)], method = method)
		print(model)
		if(method == "knn") set.seed(seed)
		cat("\n", "Test set confusion matrix:", sep = "")
		print(table(test[, ncol(test)], predict(model, test[,-ncol(test)])))
	}
	if(return_model == TRUE) return(model)
}

#Decision trees, no information gain:
run.model(dtm90, dtm.te90, "rpart")
run.model(dtm95, dtm.te95, "rpart")
run.model(dtm.idf90, dtm.idf.te90, "rpart")
run.model(dtm.idf95, dtm.idf.te95, "rpart")

#KNN:
run.model(dtm90, dtm.te90, "knn")
run.model(dtm95, dtm.te95, "knn")
run.model(dtm.idf90, dtm.idf.te90, "knn")
run.model(dtm.idf95, dtm.idf.te95, "knn")

#Naive Bayes:
run.model(dtm90, dtm.te90, "nb")
run.model(dtm95, dtm.te95, "nb")
run.model(dtm.idf90, dtm.idf.te90, "nb")
run.model(dtm.idf95, dtm.idf.te95, "nb")

#Neural network:
run.model(dtm90, dtm.te90, "nnet")
run.model(dtm95, dtm.te95, "nnet")
run.model(dtm.idf90, dtm.idf.te90, "nnet")
run.model(dtm.idf95, dtm.idf.te95, "nnet")

#SVM radial:
run.model(dtm90, dtm.te90, "svmRadial")
run.model(dtm95, dtm.te95, "svmRadial")
run.model(dtm.idf90, dtm.idf.te90, "svmRadial")
run.model(dtm.idf95, dtm.idf.te95, "svmRadial")


#Run decision trees:
run.model(dtm.ig, dtm.te.ig, "rpart")
run.model(dtm.ig90, dtm.te.ig90, "rpart")
run.model(dtm.ig95, dtm.te.ig95, "rpart")
run.model(dtm.idf.ig, dtm.te.idf.ig, "rpart")
run.model(dtm.idf.ig90, dtm.te.idf.ig90, "rpart")
run.model(dtm.idf.ig95, dtm.te.idf.ig95, "rpart")

#Run knn:
run.model(dtm.ig, dtm.te.ig, "knn")
run.model(dtm.ig90, dtm.te.ig90, "knn")
run.model(dtm.ig95, dtm.te.ig95, "knn")
run.model(dtm.idf.ig, dtm.te.idf.ig, "knn")
run.model(dtm.idf.ig90, dtm.te.idf.ig90, "knn")
run.model(dtm.idf.ig95, dtm.te.idf.ig95, "knn")

#Run nb:
run.model(dtm.ig, dtm.te.ig, "nb")
run.model(dtm.ig90, dtm.te.ig90, "nb")
run.model(dtm.ig95, dtm.te.ig95, "nb")
run.model(dtm.idf.ig, dtm.te.idf.ig, "nb")
run.model(dtm.idf.ig90, dtm.te.idf.ig90, "nb")
run.model(dtm.idf.ig95, dtm.te.idf.ig95, "nb")

#Run nnet:
run.model(dtm.ig, dtm.te.ig, "nnet")
run.model(dtm.ig90, dtm.te.ig90, "nnet")
run.model(dtm.ig95, dtm.te.ig95, "nnet")
run.model(dtm.idf.ig, dtm.te.idf.ig, "nnet")
run.model(dtm.idf.ig90, dtm.te.idf.ig90, "nnet")
run.model(dtm.idf.ig95, dtm.te.idf.ig95, "nnet")

#Run svmRadial
run.model(dtm.ig, dtm.te.ig, "svmRadial")
run.model(dtm.ig90, dtm.te.ig90, "svmRadial")
run.model(dtm.ig95, dtm.te.ig95, "svmRadial")
run.model(dtm.idf.ig, dtm.te.idf.ig, "svmRadial")
run.model(dtm.idf.ig90, dtm.te.idf.ig90, "svmRadial")
run.model(dtm.idf.ig95, dtm.te.idf.ig95, "svmRadial")


#With inf.gain function:

inf.gain(train[[1]], "tf", 0.03)
run.model(newig, newig.te, "rpart")
run.model(newig, newig.te, "knn")
run.model(newig, newig.te, "nb")
run.model(newig, newig.te, "nnet")
run.model(newig, newig.te, "svmRadial")

inf.gain(train[[2]], "tf", 0.03)
run.model(newig, newig.te, "rpart")
run.model(newig, newig.te, "knn")
run.model(newig, newig.te, "nb")
run.model(newig, newig.te, "nnet")
run.model(newig, newig.te, "svmRadial")

inf.gain(train[[3]], "tf", 0.03)
run.model(newig, newig.te, "rpart")
run.model(newig, newig.te, "knn")
run.model(newig, newig.te, "nb")
run.model(newig, newig.te, "nnet")
run.model(newig, newig.te, "svmRadial")

inf.gain(train[[1]], "tfidf", 0.03)
run.model(newig, newig.te, "rpart")
run.model(newig, newig.te, "knn")
run.model(newig, newig.te, "nb")
run.model(newig, newig.te, "nnet")
run.model(newig, newig.te, "svmRadial")

inf.gain(train[[1]], "tfidf", 0.03)
run.model(newig, newig.te, "rpart")
run.model(newig, newig.te, "knn")
run.model(newig, newig.te, "nb")
run.model(newig, newig.te, "nnet")
run.model(newig, newig.te, "svmRadial")

inf.gain(train[[1]], "tfidf", 0.03)
run.model(newig, newig.te, "rpart")
run.model(newig, newig.te, "knn")
run.model(newig, newig.te, "nb")
run.model(newig, newig.te, "nnet")
run.model(newig, newig.te, "svmRadial")


#Wordcloud
library(tm)
library(SnowballC)
library(wordcloud)
horror <- Corpus(DirSource("Data - TM/Horror train"), readerControl=list(reader = readPlain,language="en"))
horror <- tm_map(horror, content_transformer(tolower))
horror <- tm_map(horror, removeWords, stopwords(kind ="en"))
horror <- tm_map(horror, removePunctuation)
horror <- tm_map(horror, removeNumbers)
strategy <- Corpus(DirSource("Data - TM/Strategy train"), readerControl=list(reader = readPlain,language="en"))
strategy <- tm_map(strategy, content_transformer(tolower))
strategy <- tm_map(strategy, removeWords, stopwords(kind ="en"))
strategy <- tm_map(strategy, removePunctuation)
strategy <- tm_map(strategy, removeNumbers)
fullreviews <- c(horror, strategy)
paired <- brewer.pal(n = 10, "Paired")
wordcloud(fullreviews, color = paired, min.freq = 40, random.order = F, use.r.layout = F, scale = c(5, 1))











