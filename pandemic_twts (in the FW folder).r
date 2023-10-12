########################## MERGE PARTY METADATA W/ PANDEMIC_TWTS
install.packages("tidyverse")
library(tidyverse)
library(readr)
library(DMwR2)
library(sqldf)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(tm)

merged_pan_twts <- left_join(all_pandemic_tweets, party_handle_metadata_senarchambault, by = c("user.screen_name" = "handle"))
  #check before merge: head(all_pandemic_tweets, n = 1) --> Jaylivingstone is a Dem
  #check after merge: head(merged_pan_twts, n = 1) --> Jaylivingstone is a Dem
class(merged_pan_twts) #dataframe
########################## PUll A RANDOM SAMPLE OF 5k DEMS
dem_5k <- sample(nrow(merged_pan_twts[which(merged_pan_twts$party == "D"),]), 5000, replace = FALSE, prob = NULL)
  #no replacement so every line is unique
  #no rows are weighted differently...every has an equal chance of getting in the sample
  #nrow tells how many rows to choose from
head(dem_5k, n=5)
  #46459 37894  9208 21482 19775 --> these are row numbers that it has sampled for me
sample_dem_5k <- merged_pan_twts[which(merged_pan_twts$party == "D"),][dem_5k,]
  #this is a new df w/ only dem's and the dem_5k - of this dem df give me all of these row numbers
head(sample_dem_5k, n=5)

########################## PULL A RANDOM SAMPLE OF 5k REPS
rep_5k <- sample(nrow(merged_pan_twts[which(merged_pan_twts$party == "R"),]), 5000, replace = FALSE, prob = NULL)
head(rep_5k, n=5)
sample_rep_5k <- merged_pan_twts[which(merged_pan_twts$party == "R"),][rep_5k,]
head(sample_rep_5k)
#class(sample_rep_5k)

########################## PUT TWO DF's TOGETHER
dim(sample_dem_5k)
dim(sample_rep_5k)
party_10k = rbind(sample_dem_5k,sample_rep_5k)
#dim(party_10k)
saveRDS(party_10k, file = "~/Desktop/FW/party_10k")

########################## PREPROCESSS/ CLEAN
# remove punctuation
party_10k_clean <- removePunctuation(as.character(party_10k$full_text))
# all lower case 
party_10k_clean <- tolower(party_10k_clean)
control100 = list(party_10k_clean, removeNumbers=TRUE, stemming=TRUE, stopwords=TRUE)
party_10k_corpus <- SimpleCorpus(VectorSource(control100[[1]]))
#find a way to clean the symbol for bullet points 

########################## TURN DF INTO DTM and SAVE
#dtm100_tweets <- as.matrix(DocumentTermMatrix(tweet100_corpus))
party_10k_dtm <- as.matrix(DocumentTermMatrix(party_10k_corpus))
saveRDS(party_10k_dtm, file = "~/Desktop/FW/party_10k_dtm.rds")

party_10k_dtm <- readRDS("~/Desktop/FW/party_10k_dtm.rds")
party_10k <- readRDS("~/Desktop/FW/party_10k")

########################## SPECIFY WHERE TO FIND PARTY LABELS
party = party_10k$party
party = c(rep("D", 5000), rep("R",5000))

########################## RUN FW
#run the FW function
source("~/Desktop/FW/fwgroups_function.r")
#OG code: fw.blogideo <- fwgroups(poliblog.dfm,groups = poliblog5k.meta$rating)
party_10k_grps <- fwgroups(party_10k_dtm, groups = as.factor(party))

install.packages("knitr")
library(knitr)
#OG code: fwkeys.blogideo <- fw.keys(fw.blogideo, n.keys=20)
fwkeys.party.10k <- fw.keys(party_10k_grps, n.keys=20)
#OG code: kable(fwkeys.blogideo)
kable(fwkeys.party.10k)
as.data.frame(fwkeys.party.10k)

#black dot search
which(grepl("•",party_10k$full_text))
  #which(): tells you the location of where something that I'm looking for is
  #grepl(): see's whether a specific word is contained in a string,
    ##you want to apply grepl to all of the rows in the full_text col, returns either T or F
  #gives me all the rows that contain this "•" symbol
class(party_10k) #df
party_10k[943,"full_text"]
#\n = new line --> going to next line
#conclusion these dot symbols are bullet points 
#seems to be popular to use this symbol in the tweets 

#plot
#OG code: p.fw.blogideo <- fw.ggplot.groups(fw.blogideo,sizescale=4,max.words=200,max.countrank=400,colorpalette=c("red","blue"))
fw.party.10k <- fw.ggplot.groups(party_10k_grps,sizescale=4,max.words= 100,max.countrank=400,colorpalette=c("blue","red"))
fw.party.10k
ggsave("Party(100w)_0vs1.pdf", fw.party.10k, width=8, height=8)
ggsave("Party(100w)_0vs1.png", fw.party.10k, width=8, height=8)

#pdf(fw.party.10k, file = "~/Desktop/FW/pltparty_10k_100w.pdf")

dev.off()
