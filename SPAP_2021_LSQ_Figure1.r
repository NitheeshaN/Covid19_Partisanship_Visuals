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
###################### Generate FW Plots for Figure 1 #########################
###############################################################################

# step 1 
merged_pan_twts <- left_join(all_pandemic_tweets, party_handle_metadata_senarchambault, by = c("user.screen_name" = "handle"))

# step 2 Pull Random Sample of Dem tweets
dem_5k <- sample(nrow(merged_pan_twts[which(merged_pan_twts$party == "D"),]), 5000, replace = FALSE, prob = NULL)
sample_dem_5k <- merged_pan_twts[which(merged_pan_twts$party == "D"),][dem_5k,]

# step 3 Pull Random Sample of Rep tweets
rep_5k <- sample(nrow(merged_pan_twts[which(merged_pan_twts$party == "R"),]), 5000, replace = FALSE, prob = NULL)
sample_rep_5k <- merged_pan_twts[which(merged_pan_twts$party == "R"),][rep_5k,]

# step 4 Merge Two DataFrames together
party_10k = rbind(sample_dem_5k,sample_rep_5k)

# step 5 Preprocess/Clean: remove punctuation
party_10k_clean <- removePunctuation(as.character(party_10k$full_text))

# step 6 Preprocess/Clean: all lower case
party_10k_clean <- tolower(party_10k_clean)

# step 7 Preprocess/Clean: create corpus
control100 = list(party_10k_clean, removeNumbers=TRUE, stemming=TRUE, stopwords=TRUE)
party_10k_corpus <- SimpleCorpus(VectorSource(control100[[1]]))

# step 8 Create DTM
party_10k_dtm <- as.matrix(DocumentTermMatrix(party_10k_corpus))
saveRDS(party_10k_dtm, file = "party_10k_dtm.rds")

# step 9 Specify Party Labels
party = c(rep("Democrat", 5000), rep("Republican",5000))
  #note in line 50 we merged Dem's FIRST and THEN Rep's
    #create party labels in the order in which you merged dataframes

# step 10 Run FW Function
source("~/Desktop/FW/fwgroups_function.r") #or run entire fwgroups_function.r script
party_10k_grps <- fwgroups(party_10k_dtm, groups = as.factor(party))

fwkeys.party.10k <- fw.keys(party_10k_grps, n.keys=20)

kable(fwkeys.party.10k)

as.data.frame(fwkeys.party.10k)

#step 11 Search for "•" or any other prominent symbols
which(grepl("•",party_10k$full_text))
class(party_10k) #df
party_10k[943,"full_text"]
  #\n = new line --> going to next line
  #conclusion dot symbols are bullet points in the tweets 

#step 12 Create FW Plot
fw.party.10k <- fw.ggplot.groups(party_10k_grps,sizescale=4,max.words= 100,max.countrank=400,colorpalette=c("blue","red"))
fw.party.10k

ggsave("Party(100w)_0vs1.pdf", fw.party.10k, width=8, height=8)


dev.off()















