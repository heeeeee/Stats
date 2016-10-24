###############################################
# Final Project 
# Stat 701
#
# Vaughn Baker, Paulynn Yu, Hitomi Umeki
# 2015/05/02
###############################################

#######################################
#### Setup ############################
#######################################
## cleanup
rm(list=ls())

## install required libraries
#install.packages("maps")
#install.packages("tm")
#install.packages("stringr")
#install.packages("wordcloud")
#install.packages("lubridate")
#library(rpart)
#library(rpart.plot)
#library(randomForest)


library(maps)
library(tm) 
library(lubridate)
library(wordcloud)
library(randomForest)
library(rpart)
library(rpart.plot)

#data.table
#plyr

## set working directory
dir=c("/Users/umekihitomi/Statistics/STAT701/FinalProject/data") 
setwd(dir)   
getwd()

## load file
data = read.csv("crunchbase_final.csv", sep=",", header=T, stringsAsFactors=T,na.strings="")

# check data
nrow(data)
names(data)
str(data)


#######################################
#### Summary Stats ####################
#######################################

# summary statistics
summary(data)
summary(data$country_code)
summary(data$region)
summary(data$market)

# check na data
#View(data[is.na(data$country),])


#######################################
#### Data Cleanup #####################
#######################################

# Note: Some cleanup and data merge was also performed with Excel

## data cleanup 0
# remove unnecessary column (webpage URL)
data$homepage_url <- NULL
data$category_list <- NULL
data$city <- NULL
data$founded_month<-NULL

# fix data types
str(data)
data$success=as.factor(data$success)
data$unique_id=as.character(data$unique_id)
data$name=as.character(data$name)
data$description=as.character(data$description)
data$funding_total_usd=as.numeric(data$funding_total_usd)

## data cleanup 1: Treating dates
data$first_funding_at=as.character(data$first_funding_at)
data$first_funding_at=as.Date(data$first_funding_at,"%m/%d/%y")
data$last_funding_at=as.character(data$last_funding_at)
data$last_funding_at=as.Date(data$last_funding_at,"%m/%d/%y")

first_funding_year<-as.numeric(format(data$first_funding_at, "%Y")) 
last_funding_year<-as.numeric(format(data$last_funding_at, "%Y")) 
data2 <-data.frame(data, first_funding_year, last_funding_year)

## data cleanup 2: Extract USA data only
# extract
working<-data2[(data2$country_code=="USA"),]
working<-working[!is.na(working$country_code=="USA"),]
# drop the other empty factors
working$country_code<-(droplevels(working$country_code))
str(working$country_code)

## data cleanup 3: remove the duplicates
#View(working[working$unique_id=="Bella Vita Consultants_San Diego",])
# remove duplication of rows because we want to predict per individual and not by cases
working<-working[!duplicated(working),]
dim(working)

## data cleanup 4: SF/NY/Bosoton/LA/Seattle/DC/others: 
# region
summary(working)
summary(working$region)
working$region <-as.character(working$region)
working$region[!working$region=="SF Bay Area"&
               !working$region=="New York City"&
               !working$region=="Boston"&
               !working$region=="Los Angeles"&
               !working$region=="Seattle"&
               !working$region=="Washington, D.C."&
               !working$region=="Chicago"&
               !working$region=="San Diego"]<- "others"
working$region <-as.factor(working$region)
summary(working)

## data cleanup 5: market 
summary(working)

## data cleanup 6: eliminating NAs 
summary(working)
#View(working[is.na(working[,"status"]),])
working<-working[!is.na(working[,"status"]),] # NAs
dim(working)

#View(working[is.na(working[,"first_funding_year"]),])
working<-working[!is.na(working[,"first_funding_year"]),] # NAs
dim(working)

# check
dim(working)
summary(working)
# save clean data
write.csv(working, file="Crunchbase_Clean.csv", row.names=F)


#######################################
#### Name Analysis ####################
#######################################

# Note: Some analysis was performed using Perl


# FUNCTION: createWdcloud
# arguments: list of text
#
createWdcloud<-function(text)
{ 
  # cleanup
  cleanedtext <- gsub("[^[:alnum:]///' ]", "", text)
  # object
  mycorpus <- VCorpus( VectorSource(cleanedtext) )
  # remove stop words
  mycorpus <- tm_map(mycorpus, removeWords, stopwords("english"))
  # remove punctuation
  mycorpus <- tm_map(mycorpus, removePunctuation)
  # change to lower case
  myCorpus <- tm_map(mycorpus, stemDocument,lazy=TRUE)
  # change to lower case
  mycorpus <- tm_map(mycorpus, content_transformer(tolower))

  #inspect(mycorpus)
  wordcloud(mycorpus, 
          scale=c(5,0.5), 
          max.words=100, 
          random.order=FALSE, 
          rot.per=0.35, 
          use.r.layout=FALSE, 
          colors=brewer.pal(8, "Dark2"))
}


# subset data into pass data and non pass data
passdata <- subset(working, success == 1)
nonpassdata <- subset(working, success == 0)

par(mfrow=c(1,2))
# plot word cloud for name
createWdcloud(passdata$name)
createWdcloud(nonpassdata$name)

# plot word cloud for description
createWdcloud(passdata$description)
createWdcloud(nonpassdata$description)



#######################################
#### Random Forest ####################
#######################################

samplesize = 2647*2/3 #(size of smaller category)

## adjust (tune a little) by changing the sample size
out1<-randomForest(success ~ region 
#                   + market 
#                  + founded_year 
                   + first_funding_year
                   + status
                   + funding_total_usd
                   + funding_rounds
                   + funding_rounds,
                   data=working,
                   sampsize=c(samplesize,samplesize),
                   importance=T) 
out1
par(mfrow=c(1,1))
plot(out1, main="error rate vs number of trees") # 500 trees is enough


###############################################
### Varibale Important Plot ###################
###############################################
#help(varImpPlot)
par(mfrow=c(2,1))
varImpPlot(out1,type=1,class=1,scale=F,main="")
varImpPlot(out1,type=1,class=0,scale=F,main="")


###############################################
### Partial Dependence Plot ###################
###############################################
#help(partialPlot)

# FUNCTION: plotPartial
# arguments: variable
#

plotPartial<-function(text){
  textQ<-quote(text)
  pp<-partialPlot(out1, 
                    pred.data=working, 
                    x.var=sprintf("%", text), 
                    which.class=1, 
                    #xlab=variable, 
                    ylab="Centered Logits", 
                    #main=variable, 
                    rug=T)

  scatter.smooth(pp$x,pp$y, 
                 xlab=textQ, 
                 span=1/3,
                 ylab="Centered Logits", 
                 #main=variable, 
                 lpars=list(col="blue",lwd=3))
  
}
plotPartial(region)

pp_AP<-partialPlot(out2, pred.data=working, x.var="funding_total_usd", which.class=1, xlab="funding_total_usd", ylab="Centered Logits", main="funding_total_usd", rug=T) 
pp_JP<-partialPlot(out2, pred.data=working, x.var="JuvenilePriors", which.class=1, xlab="Juvenile Priors", ylab="Centered Logits", main="JuvenilePriors", rug=T) 
pp_iViol<-partialPlot(out2, pred.data=working, x.var="iViolCount", which.class=1, xlab="iViolCount", ylab="Centered Logits", main="iViolCount", rug=T) 
pp_M<-partialPlot(out2, pred.data=working, x.var="Male", which.class=1, xlab="Male", ylab="Centered Logits", main="Male", rug=T) 

par(mfrow=c(2,2))
scatter.smooth(pp_AP$x,pp_AP$y, xlab="AdultPriors", span=1/3,ylab="Centered Logits", main="Adult Priors", lpars=list(col="blue",lwd=3))
scatter.smooth(pp_JP$x,pp_JP$y, xlab="Juvenile Priors", span=1/3,ylab="Centered Logits", main="Juvenile Priors", lpars=list(col="blue",lwd=3))
scatter.smooth(pp_iViol$x,pp_iViol$y, xlab="iViolCount", span=1/3,ylab="Centered Logits", main="iViol Count", lpars=list(col="blue",lwd=3))



###############################################################
## Centered Binary Logits to Probabilities ####################
###############################################################
probs<-function(logits)
{
  exp(2*logits)/(1+exp(2*logits)) 
}


p<-probs(pp$y)
scatter.smooth(pp_DOB$x, probs(pp_DOB$y) , xlab="DOB", span=1/3, ylab="Probability", main="DOB", lpars=list(col="blue",lwd=3)) 
scatter.smooth(pp_AP$x, probs(pp_AP$y), xlab="AdultPriors", span=1/3, ylab="Probability", main="AdultPriors", lpars=list(col="blue",lwd=3)) 
scatter.smooth(pp_JP$x, probs(pp_JP$y), xlab="JuniorPriors", span=1/3, ylab="Probability", main="JuniorPriors", lpars=list(col="blue",lwd=3)) 
scatter.smooth(pp_iViol$x, probs(pp_iViol$y), xlab="iViolCount", span=1/3, ylab="Probability", main="iViolCount", lpars=list(col="blue",lwd=3)) 
plot(pp_M$x, probs(pp_M$y))

, xlab="Male", ylab="Probability", main="Male", lpars=list(col="blue",lwd=3)) 

