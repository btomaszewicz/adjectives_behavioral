#clear workspace
rm(list=ls())

##check R version (if using packages; to report in publication)
version

setwd("~/research/adjectives")

raw_data <- read.csv("Absolute2_Qualtrics_myedit.csv")

library(plyr)
#convert from wide to long format
library(tidyr)    #to use gather()
library(reshape2) #to use melt()


# Make sure the subject column is a factor
names(raw_data)[names(raw_data)=="ProlificID"] <- "subject"
raw_data$subject <- factor(raw_data$subject)

#see the variable names
str(raw_data) 

#getting rid of columns
raw_data$StartDate <- NULL
raw_data$EndDate <- NULL
raw_data$Status <- NULL
raw_data$IPAddress <- NULL
raw_data$Progress <- NULL
raw_data$Duration..in.seconds. <- NULL
raw_data$Finished <- NULL
raw_data$RecordedDate <- NULL
raw_data$ResponseId <- NULL
raw_data$RecipientLastName <- NULL
raw_data$RecipientFirstName <- NULL
raw_data$RecipientEmail <- NULL
raw_data$ExternalReference <- NULL
raw_data$LocationLatitude <- NULL
raw_data$LocationLongitude <- NULL
raw_data$DistributionChannel <- NULL
raw_data$UserLanguage <- NULL
raw_data$MetaInfo_Browser <- NULL
raw_data$MetaInfo_Version <- NULL
raw_data$MetaInfo_Operating.System <- NULL
raw_data$MetaInfo_Resolution <- NULL
raw_data$Age <- NULL
raw_data$Sex <- NULL
raw_data$NativeSpeaker <- NULL
raw_data$ColorBlind <- NULL
raw_data$Agreement <- NULL
raw_data$Monitor <- NULL

library(reshape2)
long <- melt(raw_data, id.vars = c("subject"))

names(long)[names(long)=="variable"] <- "item"


#for splitting the timing from rating

long$measure <- c(rep("-", nrow(long)))

for (i in 1:nrow(long)) {
  if (grepl("Timing", long$item[i])) {    
    long$measure[i] <- "timing"
  }
}

for (i in 1:nrow(long)) {
  if (grepl("-", long$measure[i])) {
    long$measure[i] <- "rating"
  }
}

#add condition names: item_type

long$item_type <- c(rep("-", nrow(long)))

for (i in 1:nrow(long)) {
  if (grepl("filler", long$item[i])) {    
    long$item_type[i] <- "filler"
  }
}

for (i in 1:nrow(long)) {
  if (grepl("-", long$item_type[i])) {
    long$item_type[i] <- "target"
  }
}

#add condition names: pict_order

long$pict_order <- c(rep("-", nrow(long)))

for (i in 1:nrow(long)) {
  if (grepl("reversed", long$item[i])) {    
    long$pict_order[i] <- "reversed"
  }
}

for (i in 1:nrow(long)) {
  if (grepl("-", long$pict_order[i])) {
    long$pict_order[i] <- "leftno_rightyes"
  }
}

long$pict_order <- as.factor(long$pict_order)


#add condition names: adjective

long$adjective <- c(rep("-", nrow(long)))

for (i in 1:nrow(long)) {
  if (grepl("schwarz", long$item[i])) {    
    long$adjective[i] <- "schwarz"
  }
}

for (i in 1:nrow(long)) {
  if (grepl("blau", long$item[i])) {    
    long$adjective[i] <- "blau"
  }
}

for (i in 1:nrow(long)) {
  if (grepl("grau", long$item[i])) {    
    long$adjective[i] <- "grau"
  }
}

for (i in 1:nrow(long)) {
  if (grepl("gruen", long$item[i])) {    
    long$adjective[i] <- "gruen"
  }
}

for (i in 1:nrow(long)) {
  if (grepl("rosa", long$item[i])) {    
    long$adjective[i] <- "rosa"
  }
}

for (i in 1:nrow(long)) {
  if (grepl("lila", long$item[i])) {    
    long$adjective[i] <- "lila"
  }
}

for (i in 1:nrow(long)) {
  if (grepl("pink", long$item[i])) {    
    long$adjective[i] <- "pink"
  }
}

for (i in 1:nrow(long)) {
  if (grepl("gelb", long$item[i])) {    
    long$adjective[i] <- "gelb"
  }
}

for (i in 1:nrow(long)) {
  if (grepl("braun", long$item[i])) {    
    long$adjective[i] <- "braun"
  }
}

for (i in 1:nrow(long)) {
  if (grepl("orange", long$item[i])) {    
    long$adjective[i] <- "orange"
  }
}

for (i in 1:nrow(long)) {
  if (grepl("rot", long$item[i])) {    
    long$adjective[i] <- "rot"
  }
}

for (i in 1:nrow(long)) {
  if (grepl("dreckig", long$item[i])) {    
    long$adjective[i] <- "dreckig"
  }
}

for (i in 1:nrow(long)) {
  if (grepl("gebogen", long$item[i])) {    
    long$adjective[i] <- "gebogen"
  }
}

for (i in 1:nrow(long)) {
  if (grepl("gepunktet", long$item[i])) {    
    long$adjective[i] <- "gepunktet"
  }
}

for (i in 1:nrow(long)) {
  if (grepl("gerade", long$item[i])) {    
    long$adjective[i] <- "gerade"
  }
}

for (i in 1:nrow(long)) {
  if (grepl("geschlossen", long$item[i])) {    
    long$adjective[i] <- "geschlossen"
  }
}

for (i in 1:nrow(long)) {
  if (grepl("glatt", long$item[i])) {    
    long$adjective[i] <- "glatt"
  }
}

for (i in 1:nrow(long)) {
  if (grepl("klar", long$item[i])) {    
    long$adjective[i] <- "klar"
  }
}

for (i in 1:nrow(long)) {
  if (grepl("leer", long$item[i])) {    
    long$adjective[i] <- "leer"
  }
}

for (i in 1:nrow(long)) {
  if (grepl("nass", long$item[i])) {    
    long$adjective[i] <- "nass"
  }
}

for (i in 1:nrow(long)) {
  if (grepl("offen", long$item[i])) {    
    long$adjective[i] <- "offen"
  }
}

for (i in 1:nrow(long)) {
  if (grepl("sauber", long$item[i])) {    
    long$adjective[i] <- "sauber"
  }
}

for (i in 1:nrow(long)) {
  if (grepl("trocken", long$item[i])) {    
    long$adjective[i] <- "trocken"
  }
}

for (i in 1:nrow(long)) {
  if (grepl("trueb", long$item[i])) {    
    long$adjective[i] <- "trueb"
  }
}

for (i in 1:nrow(long)) {
  if (grepl("voll", long$item[i])) {
    long$adjective[i] <- "voll"
  }
}


#add condition names: adj_type

long$adj_type <- c(rep("-", nrow(long)))

for (i in 1:nrow(long)) {
  if (grepl("_min..", long$item[i])) {    
    long$adj_type[i] <- "min"
  }
}

for (i in 1:nrow(long)) {
  if (grepl("max", long$item[i])) {    
    long$adj_type[i] <- "max"
  }
}

for (i in 1:nrow(long)) {
  if (grepl("-", long$adj_type[i])) {
    long$adj_type[i] <- "color"
  }
}

#add condition names: pict_position

long$pict_position <- c(rep("-", nrow(long)))

for (i in 1:nrow(long)) {
  if (grepl(".1$", long$item[i])) {    # $ Asserts that we are at the end.
    long$pict_position[i] <- "01"
  }
}

for (i in 1:nrow(long)) {
  if (grepl(".2$", long$item[i])) {
    long$pict_position[i] <- "02"
  }
}

for (i in 1:nrow(long)) {
  if (grepl(".3$", long$item[i])) {
    long$pict_position[i] <- "03"
  }
}

for (i in 1:nrow(long)) {
  if (grepl(".4$", long$item[i])) {
    long$pict_position[i] <- "04"
  }
}

for (i in 1:nrow(long)) {
  if (grepl(".5$", long$item[i])) {
    long$pict_position[i] <- "05"
  }
}

#add condition names: noun

long$test2 <- gsub("\\.\\.\\.+.+", "", long$item)   #keep everything before the "..." (replace everything after by nothing "")

long$noun <- gsub(".*\\.\\.", "", long$test2)   #remove everything before 

long$test2 <- NULL

for (i in 1:nrow(long)) {
  if (grepl("b1__filler_2", long$item[i])) {    
    long$noun[i] <- "slipper"
  }
}

for (i in 1:nrow(long)) {
  if (grepl("b1__filler_3", long$item[i])) {    
    long$noun[i] <- "scarf"
  }
}

for (i in 1:nrow(long)) {
  if (grepl("b1__filler_4", long$item[i])) {    
    long$noun[i] <- "shopping-basket"
  }
}

for (i in 1:nrow(long)) {
  if (grepl("b1__filler_5", long$item[i])) {    
    long$noun[i] <- "bubblegum"
  }
}

for (i in 1:nrow(long)) {
  if (grepl("b1__filler_6", long$item[i])) {    
    long$noun[i] <- "tulip"
  }
}

for (i in 1:nrow(long)) {
  if (grepl("b1__filler_7", long$item[i])) {    
    long$noun[i] <- "baloon"
  }
}

for (i in 1:nrow(long)) {
  if (grepl("b1__filler_8", long$item[i])) {    
    long$noun[i] <- "duck"
  }
}

for (i in 1:nrow(long)) {
  if (grepl("b2__filler_1", long$item[i])) {    
    long$noun[i] <- "mouse"
  }
}

for (i in 1:nrow(long)) {
  if (grepl("b2__filler_9", long$item[i])) {    
    long$noun[i] <- "acorn"
  }
}

for (i in 1:nrow(long)) {
  if (grepl("b2__filler_3", long$item[i])) {    
    long$noun[i] <- "toymouse"
  }
}

for (i in 1:nrow(long)) {
  if (grepl("b2__filler_10", long$item[i])) {    
    long$noun[i] <- "basketball"
  }
}

for (i in 1:nrow(long)) {
  if (grepl("b2__filler_5", long$item[i])) {    
    long$noun[i] <- "bucket"
  }
}

for (i in 1:nrow(long)) {
  if (grepl("b2__filler_11", long$item[i])) {    
    long$noun[i] <- "belt"
  }
}

for (i in 1:nrow(long)) {
  if (grepl("b3__filler_2", long$item[i])) {    
    long$noun[i] <- "ribbon"
  }
}

for (i in 1:nrow(long)) {
  if (grepl("b3__filler_9", long$item[i])) {    
    long$noun[i] <- "hat"
  }
}

for (i in 1:nrow(long)) {
  if (grepl("b3__filler_4", long$item[i])) {    
    long$noun[i] <- "leaf"
  }
}

for (i in 1:nrow(long)) {
  if (grepl("b3__filler_10", long$item[i])) {    
    long$noun[i] <- "billiardball"
  }
}

for (i in 1:nrow(long)) {
  if (grepl("b3__filler_6", long$item[i])) {    
    long$noun[i] <- "icecream"
  }
}

for (i in 1:nrow(long)) {
  if (grepl("b3__filler_11", long$item[i])) {    
    long$noun[i] <- "dice"
  }
}

for (i in 1:nrow(long)) {
  if (grepl("b1__filler_1", long$item[i])) {    #this was in block 4, but got the wrong code as b1
    long$noun[i] <- "comb"
  }
}

for (i in 1:nrow(long)) {
  if (grepl("b2__filler_7", long$item[i])) {    #this was in block 4, but got the wrong code as b2
    long$noun[i] <- "notebook"
  }
}

for (i in 1:nrow(long)) {
  if (grepl("b3__filler_8", long$item[i])) {    #this was in block 4, but got the wrong code as b3
    long$noun[i] <- "button"
  }
}

long$noun <- as.factor(long$noun)

levels(long$noun)
#wrong noun: Timing 
long <- droplevels(subset(long, noun != "Timing"))

ts <- unique(long[, c("adjective","noun")])
ts[order(ts$adjective),]
#comparing with list in 'Deviation from Threshold' - the same

levels(long$pict_order)

#fix mistake: 'trocken	pfannenwender' was shown as REVERSED and not left.right
pfannenwender <- droplevels(subset(long, noun=="pfannenwender"))

for (i in 1:nrow(long)) {
  if (grepl("pfannenwender", long$noun[i])) {    
    long$pict_order[i] <- "reversed"
  }
}

ts <- unique(long[, c("pict_order", "adjective","noun")])
ts[order(ts$pict_order),]
trocken <- droplevels(subset(long, adjective=="trocken"))
ts <- unique(trocken[, c("pict_order", "noun")])
ts[order(ts$pict_order),]

write.csv(long, file = "AbsoluteExp2_long.csv")



