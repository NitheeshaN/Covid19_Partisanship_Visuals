###############################################################################   
#### Replication Materials                                                 #### 
#### Kim, Nakka, Gopal, Desmrais, Mancinelli, Harden, Ko, Boehmke. 2021.   ####
#### Attention to the COVID-19 pandemic on Twitter:                        ####
#### Partisan differences among U.S. state legislators                     ####
#### Legislative Studies Quarterly                                         ####
###############################################################################  


###############################################################################
################################### Set Up ####################################
###############################################################################

# packages -------------------------
library(tidyverse)
library(readr)
library(DMwR2)
library(sqldf)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(tm)
library(knitr)

lapply(c('readr'), 
  require, 
  character.only = TRUE
  )



###############################################################################
###################### Generate FW Plots for Figure S1 ########################
###############################################################################

# step 1 Read in data to get header
set.seed(81380)
RowsInCSV = nrLinesFile("labeled_tweets_9_27_2020.csv") #Or however many rows there are

first_twts <- read.csv(file="labeled_tweets_9_27_2020.csv",nrows=500,header=T)


# step 2 Set random line sampler function
readNRandomLines <-  function(file,numlines,nvars,nobs=10){
  # file is the file name
  # numlines is the number of lines in the file on disk
  # nvars is the number of columns to expect
  # nobs is the number of observations to select at once
  skp <- sample(2:(numlines-nobs),1)
  suppressMessages(dat2 <- read_csv(file, n_max=nobs, skip = skp, col_names=F,progress=F))
  while( ( (ncol(dat2)==nvars) + (nrow(dat2)==nobs) ) < 2){
    skp <- sample(2:(numlines-nobs),1)
    suppressMessages(dat2 <- read_csv(file, n_max=nobs, skip = skp, col_names=F,progress=F))
  }
  data.frame(dat2)
}


# step 3 Collect 20k Samples of Tweets
set.seed(9202011)
# increase the second argument to rep() to draw more
# samples (10=1000 obs, 50 = 5000 obs, etc). 
system.time(tweet_dat_sample20 <- lapply(rep("labeled_tweets_9_27_2020.csv",200), 
                                         readNRandomLines,numlines=RowsInCSV,nvars=ncol(first_twts),nobs=100))

sample20_twts <- do.call('rbind',tweet_dat_sample20)
names(sample20_twts) <- names(first_twts)
saveRDS(sample20_twts, file="~/Desktop/FW/sampletwts20.rds")


# step 4 collect 100k sample of Tweets
system.time(tweet_dat_sample100 <- lapply(rep("labeled_tweets_9_27_2020.csv",1000), 
                                          readNRandomLines,numlines=RowsInCSV,nvars=ncol(first_twts),nobs=100))

sample100_twts <- do.call('rbind',tweet_dat_sample100)
names(sample100_twts) <- names(first_twts)
saveRDS(sample100_twts, file="~/Desktop/FW/sampletwts100.rds")


# step 5 Collect 5k Sample of Pandemic related Tweets (label = 1) using 100k sample
sample100_twts <- readRDS("~/Desktop/FW/sampletwts100.rds")
only1label = sample100_twts[which(sample100_twts$final_label == 1),][1:5000,]


# step 6 Collect 5k Sample of NON Pandemic related Tweets (label = 0) using 20k sample
sample20_twts <- readRDS("~/Desktop/FW/sampletwts20.rds")
only0label = sample20_twts[which(sample20_twts$final_label==0),][1:5000,]


# step 7 Combine into single dataframe
combo_10k = rbind(only1label,only0label)


# step 8 Preprocess/Clean: remove punctuation
combo10k_text <- removePunctuation(as.character(combo_10k$full_text))


# step 9 Preprocess/Clean: all lower case
combo10k_text <- tolower(combo10k_text)


# step 10 Preprocess/Clean: create corpus
control10k = list(combo10k_text, removeNumbers=TRUE, stemming=TRUE, stopwords=TRUE)
combo10k_corpus <- SimpleCorpus(VectorSource(control10k[[1]]))


# step 11 Create DTM
combo10k_dtm <- as.matrix(DocumentTermMatrix(combo10k_corpus))
saveRDS(combo10k_dtm, file="~/Desktop/FW/combo10k_dtm.rds")

# step 12 Specify Labels
final_label= c(rep("Pandemic",5000), rep("Non-Pandemic",5000))
  #note in line 89 we merged 1's FIRST and THEN 0's
    #create labels in the order in which you merged dataframes


# step 13 Run FW Function
source("~/Desktop/FW/fwgroups_function.r") #or run entire fwgroups_function.r script
tenk_groups <- fwgroups(combo10k_dtm, groups = as.factor(final_label))

fwkeys.10k <- fw.keys(tenk_groups, n.keys=20)

kable(fwkeys.10k)

as.data.frame(fwkeys.10k)

# step 14 Create FW Plot
fw.10k <- fw.ggplot.groups(tenk_groups,sizescale=4,max.words= 100,max.countrank=400,colorpalette=c("black","blue"))
fw.10k

ggsave("alltwts(100w)_0vs1_color.pdf", fw.10k, width=8, height=8)



dev.off()










