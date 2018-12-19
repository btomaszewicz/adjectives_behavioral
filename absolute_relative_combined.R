#clear workspace
rm(list=ls())

##check R version (if using packages; to report in publication)
version

setwd("~/research/adjectives")

library(plyr)
library(ggplot2)
library('lme4')
library(lmerTest)
library(tidyr)
library(tidyverse)

#prepare 1 data file
absolute <- read.csv("AbsoluteExp2_long.csv") #with NAs
str(absolute)
absolute$X <- NULL
absolute$group <- "Abs" #separate groups of participants

relative <- read.csv("RelativeExp2_long.csv") #with NAs
str(relative)
relative$X <- NULL
relative$group <- "Rel"

dat <- rbind(absolute, relative)
dat$group <- as.factor(dat$group)
str(dat)
levels(dat$adj_type)
levels(dat$group)
levels(dat$noun)
levels(dat$pict_order) #combine these into "degree"

#combine "leftno_rightyes" and "reversed" into "degree":
#add new column pasting "leftno_rightyes01", "reversed01", 
#then copy pict_position as "degree" and in it replace only the reversed by 5 loops

dat <- transform(dat, newcol=paste(pict_order, pict_position, sep="_"))
head(dat)
levels(dat$newcol)

dat$degree <- dat$pict_position
head(dat)

for (i in 1:nrow(dat)) {
  if (grepl("reversed_01", dat$newcol[i])) {   
    dat$degree[i] <- "05"
  }
}

for (i in 1:nrow(dat)) {
  if (grepl("reversed_02", dat$newcol[i])) {   
    dat$degree[i] <- "04"
  }
}

for (i in 1:nrow(dat)) {
  if (grepl("reversed_03", dat$newcol[i])) {   
    dat$degree[i] <- "03"
  }
}

for (i in 1:nrow(dat)) {
  if (grepl("reversed_04", dat$newcol[i])) {   
    dat$degree[i] <- "02"
  }
}

for (i in 1:nrow(dat)) {
  if (grepl("reversed_05", dat$newcol[i])) {   
    dat$degree[i] <- "01"
  }
}

for (i in 1:nrow(dat)) {
  if (grepl("reversed_-", dat$newcol[i])) {   
    dat$degree[i] <- "-"
  }
}

#add a variable for series identity: 
#(although better to include separately 'adjective' and 'noun' in random effects, ‘pict_order’ as a fixed effect (counterbalanced Left-Right and Right-Left order was part of the design))
dat$series <- gsub("\\.+.+", "", dat$item)   #keep everything before the . (replace everything after by nothing "")
dat$series <- as.factor(dat$series)
str(dat)

#'block' as fixed effect? block was completely randomized, AND no pauses between blocks (4 blocks with Absolute-2)
dat$block <- gsub("\\_+.+", "", dat$item) #keep everything before the _
dat$block <- as.factor(dat$block)

#remove fillers
str(dat)
ratings <- droplevels(subset(dat, measure=="rating"))
target_ratings <- droplevels(subset(ratings, item_type=="target"))
str(target_ratings)

#remove NAs - how many?
nrow(target_ratings)
target_ratings_NoNA <- na.omit(target_ratings)
nrow(target_ratings_NoNA)
1-nrow(target_ratings_NoNA)/nrow(target_ratings)


#### PLOT the effect of 'pict_order'
source(file = "Helper_functions-within-subjects-se.R")
datac <- summarySEwithin(target_ratings_NoNA, measurevar="value", withinvars=c("degree","pict_order", "adj_type"), idvar="subject")

labels <- c(max = "Max", min = "Min", relative = "Relative")

ggplot(datac, aes(degree, value, group=pict_order, color=pict_order)) +
  geom_errorbar(aes(ymin=(value-se), ymax=(value+se)), size=.75, width=0.2) + 
  geom_line(aes(linetype="solid"), size=1.2) + 
  geom_point(aes(shape=pict_order), size=4) + theme_bw() +
  #scale_shape_manual(values=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14)) +                  # Change shapes
  theme(legend.key = element_blank(), legend.title = element_text(size=12)) +
  facet_grid(. ~ adj_type, labeller=labeller(adj_type = labels)) +
  xlab("Degree of Property") + ylab("% Yes")


#### PLOT the effect of 'adj_type' (before aligning Min and Max)
source(file = "Helper_functions-within-subjects-se.R")
datac <- summarySEwithin(target_ratings_NoNA, measurevar="value", withinvars=c("degree", "adj_type"), idvar="subject")

ggplot(datac, aes(x=degree, y=value, group=adj_type, colour=adj_type)) +
  geom_errorbar(aes(ymin=value-se, ymax=value+se), size=.75, width=.2) +
  geom_line(aes(linetype=adj_type), size=1.2) +
  geom_point(aes(shape=adj_type), size=4) + theme_bw() +
  theme(legend.key = element_blank(), legend.title = element_text(size=12)) +
  xlab("Degree of Property") + ylab("% Yes")


####Flip the MAX to align it with MIN

#new degree to include the flipped for Max
target_ratings$degree2 <- target_ratings$degree
#new value to include the flipped yes-no for Max
target_ratings$value2 <- target_ratings$value

#split into 2 datasets
maxadj <- subset(target_ratings, adj_type=="max")
otheradj <- subset(target_ratings, adj_type!="max")

#flip degrees in MAX
for (i in 1:nrow(maxadj)) {
  if (grepl("01", maxadj$degree[i])) {   
    maxadj$degree2[i] <- "05"
  }
}

for (i in 1:nrow(maxadj)) {
  if (grepl("02", maxadj$degree[i])) {   
    maxadj$degree2[i] <- "04"
  }
}

for (i in 1:nrow(maxadj)) {
  if (grepl("04", maxadj$degree[i])) {   
    maxadj$degree2[i] <- "02"
  }
}

for (i in 1:nrow(maxadj)) {
  if (grepl("05", maxadj$degree[i])) {   
    maxadj$degree2[i] <- "01"
  }
}

maxadj$degree2 <- as.factor(maxadj$degree2)

#flip values in MAX
for (i in 1:nrow(maxadj)) {
  if (grepl("1", maxadj$value[i])) {   
    maxadj$value2[i] <- "0"
  }
}

for (i in 1:nrow(maxadj)) {
  if (grepl("0", maxadj$value[i])) {   
    maxadj$value2[i] <- "1"
  }
}

maxadj$value2 <- as.numeric(maxadj$value2)

#combine back
target_ratings2 <- rbind(maxadj, otheradj)
str(target_ratings2)

write.csv(target_ratings2, file = "AbsoluteRelativeExp2-FlipMax.csv")


#### PLOT the effect of 'adj_type' (aligned Min and Max (Max flipped))

target_ratings2_NoNA <- na.omit(target_ratings2)
source(file = "Helper_functions-within-subjects-se.R")
datac <- summarySEwithin(target_ratings2_NoNA, measurevar="value2", withinvars=c("degree2", "adj_type"), idvar="subject")

ggplot(datac, aes(x=degree2, y=value2, group=adj_type, colour=adj_type)) +
  geom_errorbar(aes(ymin=value2-se, ymax=value2+se), size=.75, width=.2) +
  geom_line(aes(linetype=adj_type), size=1.2) +
  geom_point(aes(shape=adj_type), size=4) + theme_bw() +
  theme(legend.key = element_blank(), legend.title = element_text(size=12)) +
  xlab("Degree of Property") + ylab("% Yes for Min/Rel; % No for Max")

ggsave('AbsoluteRelativeExp2.png', width = 6, height = 4) 

#values in the plot

AbsoluteRelativeExp2 <- as_tibble(datac)
AbsoluteRelativeExp2
#When you have two or more packages loaded that contain functions with the same name, you'll need to use the double-colon operator :: to get the version of the function from the package that was not the last loaded package
AbsoluteRelativeExp2 <- dplyr::rename(AbsoluteRelativeExp2, MeanYes = value2, MeanYesNorm = value2_norm)

AbsoluteRelativeExp2 %>%
  mutate(MeanYes = round(MeanYes, 2), MeanYesNorm = round(MeanYesNorm, 2),
         sd = round(sd, 2), se = round(se, 3), ci = round(ci, 3))

AbsoluteRelativeExp2 %>% group_by(adj_type) %>%
  mutate(MeanYes = round(MeanYes, 2), MeanYesNorm = round(MeanYesNorm, 2),
         sd = round(sd, 2), se = round(se, 3), ci = round(ci, 3)) %>%
  arrange(desc(adj_type))




###### STATS
target_ratings2 <- read.csv("AbsoluteRelativeExp2-FlipMax.csv")


str(target_ratings2)
target_ratings2$degree2 <- as.factor(target_ratings2$degree2)

contrasts(target_ratings2$adj_type) #Dummy/Treatment Coding: Compare all levels to a baseline level
#we can keep Dummy coding for ease of interpretation of glmer, and use afex for main effects (it recodes as sum)
contrasts(target_ratings2$degree2)

# contr.sum(3)
# #The columns denote the contrasts. For a factor with 3 levels, you obtain 2 contrasts. 
# #The first one tests the first level of the factor against levels 2 and 3. 
# #The second contrasts tests the second levels against levels 1 and 3.
# 
# #With [-.5 .5] 1 unit change in contrast IS the difference between levels.
# #Doesn't affect significance test. Does affect β weight (estimate) and Std error.
# contrasts(target_ratings2$adj_type) <- cbind(relative=c(-1/2, 0, 1/2), min=c(-1/2, 1/2, 0))
# contrasts(target_ratings2$adj_type) #relative against rest, min against rest
# 
# contrasts(target_ratings2$degree2)
# contr.sum(5)
# contrasts(target_ratings2$degree2) <- cbind("1vsMean"=c(1/2, 0, 0, 0, -1/2), 
#                                             "2vsMean"=c(0, 1/2, 0, 0, -1/2), 
#                                             "3vsMean"=c(0, 0, 1/2, 0, -1/2), 
#                                             "4vsMean"=c(0, 0, 0, 1/2, -1/2))
# contrasts(target_ratings2$degree2)

levels(target_ratings2$series) #there were 57 sets of 5 pics/items

# Dummy Coding
# We want Rel to be the reference level.
target_ratings2$adj_type <- relevel(target_ratings2$adj_type, ref="relative")
contrasts(target_ratings2$adj_type)

dat_reduced <- target_ratings2[,c("value2", "adj_type", "degree2", "subject", "item", "noun", "adjective", "pict_order", "series")]

# fullmodel <- glmer(value2 ~ adj_type * degree2 + pict_order +
#                      (1 + adj_type|subject) +
#                      (1|series) +
#                      (1|adjective) +
#                      (1|noun) +
#                      (1|item),
#              data = dat_reduced, family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 0)
# #nAGQ = 0 means that the random effects only influence the estimates of the fixed effects through their estimated conditional modes -- therefore, nAGQ = 0 does not completely account for the randomness of the random effects. To fully account for the random effects, they need to be integrated out. However, this difference between nAGQ = 0 and nAGQ = 1 can often be fairly small.
# 
# summary(fullmodel)
# out <- capture.output(summary(fullmodel))
# cat("Exp2: Absolute-Relative, Full Model, nAGQ = 0", out, file="Exp2_AbsoluteRelative_Glmer.txt", 
#     sep="\n", append=TRUE)

fullmodel <- glmer(value2 ~ adj_type * degree2 + pict_order +
                     (1 + adj_type|subject) +
                     (1|series) +
                     (1|adjective) +
                     (1|noun) +
                     (1|item),
                   data = dat_reduced, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(fullmodel)
out <- capture.output(summary(fullmodel))
cat("Exp2: Absolute-Relative, Full Model", out, file="Exp2_AbsoluteRelative_Glmer.txt", 
    sep="\n", append=TRUE)


library(afex)
fullmodel.afex <- mixed(value2 ~ adj_type * degree2 + pict_order +
                     (1|subject) +  #1 + adj_type failed to converge
                     (1|series) +
                     (1|adjective) +
                     (1|noun) +
                     (1|item),
                   data = dat_reduced, method = "LRT", family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(fullmodel.afex)
out <- capture.output(summary(fullmodel.afex))
cat("Exp2: Absolute-Relative, Full Model, Sum-Coding (Afex)", out, file="Exp2_AbsoluteRelative_Glmer.txt", 
    sep="\n", append=TRUE)
fullmodel.afex
out <- capture.output(fullmodel.afex)
cat("Exp2: Absolute-Relative, Full Model, Sum-Coding (Afex) - MAIN EFFECTS", out, file="Exp2_AbsoluteRelative_Glmer.txt", 
    sep="\n", append=TRUE)


#'Parsimonious Mixed Models' by Douglas Bates, Reinhold Kliegl, Shravan Vasishth, Harald Baayen
#(Submitted on 16 Jun 2015) https://arxiv.org/abs/1506.04967
#The analysis of experimental data with mixed-effects models requires decisions about the specification of 
#the appropriate random-effects structure. Recently, Barr et al. (2013) recommended fitting 'maximal' models 
#with all possible random effect components included. Estimation of maximal models, however, may not converge. 
#We show that failure to converge typically is not due to a suboptimal estimation algorithm, but is a consequence of 
#attempting to fit a model that is too complex to be properly supported by the data, irrespective of whether 
#estimation is based on maximum likelihood or on Bayesian hierarchical modeling with uninformative or 
#weakly informative priors. Importantly, even under convergence, overparameterization may lead to uninterpretable 
#models. We provide diagnostic tools for detecting overparameterization and guiding model simplification. 
#Finally, we clarify that the simulations on which Barr et al. base their recommendations are atypical for real data. A detailed example is provided of how subject-related attentional fluctuation across trials may further qualify statistical inferences about fixed effects, and of how such nonlinear effects can be accommodated within the mixed-effects modeling framework.

#Is the me model overparameterized/degenerate?
#list the percentages of variance for components.
devtools::install_github("dmbates/RePsychLing")
library(RePsychLing)
#Drop variance components?
#"Please note that taking a correlation parameter or a variance component out of a model does not mean that it is
#zero, but that the data do not support a model that assumes that it is different from zero."
#if a component has 0 proportion of variance, drop it
summary(rePCA(fullmodel))