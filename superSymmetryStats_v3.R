# Script made by Santiago Castiello 30/05/2021. Any comments, questions or 
# suggestions please write me at santiago.castiello@psy.ox.ac.uk
# To see more ideas of analysis, visualizations and code go to my github account
# you can find it in my personal website: 
# https://sites.google.com/view/santiagocastiello/home

# Note: This version contain the last Andy Baker's Suggestions + posthoc ANOVAs

# Remove all of the elements currently loaded in R
rm(list = ls()) # erase previous Global Environment, import manually the file

# libraries
if (!require(ggplot2)) {install.packages("ggplot2")}; library(ggplot2)
if (!require(dplyr)) {install.packages("dplyr")}; library(dplyr)
if (!require(BayesFactor)) {install.packages("BayesFactor")}; library(BayesFactor)

# import data Experiment 1 and 2
lf_post <- read.csv("data/lf_old_e1&e4.csv")
lf_post$Subject <- as.factor(paste(lf_post$Experiment,lf_post$Subject))
lf_post$Trials <- as.factor(as.character(lf_post$Trials))
lf_post$PosNeg <- lf_post$CondType; lf_post$CondType <- NULL

# import data Experiment 3
# call my functions
source("functions.R")
genChar <- f_generalCharacteristics_e2()


db3 <- f_load_SSL_e2(write_csv = 1)
db3 <- db3$rat
db3$Subject <- as.factor(db3$parPri)
db3$nRat[db3$phaseNum == 2] <- db3$nRat[db3$phaseNum == 2] - 16
# positive and negative
db3$PosNeg <- db3$excInh
# Trials labels
db3$Trials <- as.factor(db3$nRat)



## ## ## ## ## ## ## ## ## ##  ## ## ## ## ## ## ## ## ## ## # # # # # # # # # #
## ## ## ## ## ## ## ## EXPERIMENT 1 ## ## ## ## ## ## ## ## # # # # # # # #####
## ## ## ## ## ## ## ## ## ##  ## ## ## ## ## ## ## ## ## ## # # # # # # # # # #



## ## ## ## EXPERIMENT 1, PHASE 1 ## ## ## ####
# extract relevant data
main <- lf_post[lf_post$Experiment == "Experiment 1" & lf_post$Phase == "Phase 1",]
# super and control factor
main$SupCtrl <- ifelse(main$Cue == "v" | main$Cue == "u", "control", "super")
# create contrast zero complement
main$zeroComp <- ifelse(main$Cue == "n" | main$Cue == "v", -1, 1)
main$AdjRating <- main$Rating * main$zeroComp 
# anova
e1_ph1 <- aov(AdjRating ~ Trials * PosNeg * SupCtrl + Error(Subject/(Trials * PosNeg * SupCtrl)), data = main)
summary(e1_ph1); effectsize::eta_squared(e1_ph1, partial = TRUE, ci.lvl = .95,
                                         alternative = "two.sided")
# graph
ggplot2::ggplot(main, aes(x = Trials, y = AdjRating, col = PosNeg, shape = SupCtrl)) + 
  stat_summary(position = position_dodge(0.1)) + theme_classic()



## ## ## ## EXPERIMENT 1, PHASE 2 ## ## ## ####
# extract relevant data
main <- lf_post[lf_post$Experiment == "Experiment 1" & lf_post$Phase == "Phase 2",]
# super and control factor
main$SupCtrl <- ifelse(main$Cue == "SP" | main$Cue == "SN", "super", "control")
# create contrast zero complement
main$zeroComp <- ifelse(main$Cue == "CN" | main$Cue == "SN", -1, 1)
main$AdjRating <- main$Rating * main$zeroComp 
# anova
e1_ph2 <- aov(AdjRating ~ Trials * PosNeg * SupCtrl + Error(Subject/(Trials * PosNeg * SupCtrl)), data = main)
summary(e1_ph2); effectsize::eta_squared(e1_ph2, partial = TRUE, ci.lvl = .95,
                                         alternative = "two.sided")
# graph
ggplot2::ggplot(main, aes(x = Trials, y = AdjRating, col = PosNeg, shape = SupCtrl)) + 
  stat_summary(position = position_dodge(0.1)) + theme_classic()



# Bayes Factor
if (!require(BayesFactor)) {install.packages("BayesFactor")}; library(BayesFactor)
main$trial2 <- as.factor(main$Trials)
main$PosNeg <- as.factor(main$PosNeg)
main$SupCtrl <- as.factor(main$SupCtrl)
bf <- anovaBF(AdjRating ~ trial2 * PosNeg * SupCtrl + Subject, data = main, whichRandom="Subject")
bf
plot(bf)
# compare not symmetry vs symmetry models
assym = lmBF(AdjRating ~ trial2 * PosNeg * SupCtrl, whichRandom="Subject",data = main)
symm = lmBF(AdjRating ~ trial2 * SupCtrl, whichRandom="Subject",data = main)
bf2 = symm / assym
bf2






## ## ## ## ## ## ## ## ## ##  ## ## ## ## ## ## ## ## ## ## # # # # # # # # # #
## ## ## ## ## ## ## ## EXPERIMENT 2 ## ## ## ## ## ## ## ## # # # # # # # #####
## ## ## ## ## ## ## ## ## ##  ## ## ## ## ## ## ## ## ## ## # # # # # # # # # #



## ## ## ## EXPERIMENT 2, PHASE 1 ## ## ## ####
# extract relevant data
main <- lf_post[lf_post$Experiment == "Experiment 2" & lf_post$Phase == "Phase 1",]
# super and control factor
main$SupCtrl <- ifelse(main$Cue == "v" | main$Cue == "u", "control", "super")
# create contrast zero complement
main$zeroComp <- ifelse(main$Cue == "n" | main$Cue == "v", -1, 1)
main$AdjRating <- main$Rating * main$zeroComp 
# anova
e2_ph1 <- aov(AdjRating ~ Trials * PosNeg * SupCtrl + Error(Subject/(Trials * PosNeg * SupCtrl)), data = main)
summary(e2_ph1); effectsize::eta_squared(e2_ph1, partial = TRUE, ci.lvl = .95,
                                         alternative = "two.sided")
# graph
ggplot2::ggplot(main, aes(x = Trials, y = AdjRating, col = PosNeg, shape = SupCtrl)) + 
  stat_summary(position = position_dodge(0.1)) + theme_classic()



#### 21/05/2020
## ## POST SUPER/CONTROL BY POSITIVE AND NEGATIVE
# Per positive and negative (e2-ph1)
pos_e2_ph1 <- aov(Rating ~ SupCtrl * Trials + Error(Subject/(Trials * SupCtrl)), 
                  data = main[main$PosNeg == "positive",])
summary(pos_e2_ph1); effectsize::eta_squared(pos_e2_ph1, partial = TRUE, ci.lvl = .95,
                                             alternative = "two.sided")
neg_e2_ph1 <- aov(Rating ~ SupCtrl * Trials + Error(Subject/(Trials * SupCtrl)), 
                  data = main[main$PosNeg == "negative",])
summary(neg_e2_ph1); effectsize::eta_squared(neg_e2_ph1, partial = TRUE, ci.lvl = .95,
                                             alternative = "two.sided")



## ## POST HOC E1_P1 t test per block ## ##
ttest_trials <- matrix(NA, 8, 3); colnames(ttest_trials) <- c("t val.","df","p val")
anovas_trials <- matrix(NA, 8, 6); colnames(anovas_trials) <- paste0(rep(c("F","p"),2),"_",
                                                                     rep(c("SupCtrl","PosNeg","interaction"),each=2))
for (i in 1:8) {
  # pos_e2_ph1 <- aov(AdjRating ~ Trials * SupCtrl + Error(Subject/(Trials * SupCtrl)), data = main[main$Trials == i,])
  a <- t.test(AdjRating ~ SupCtrl, paired = T, data = main[main$Trials == i & main$PosNeg == "negative",])
  ttest_trials[i,1] <- a$statistic
  ttest_trials[i,2] <- a$parameter
  ttest_trials[i,3] <- a$p.value
  
  # Andy's email on the 22/01/2021
  b <- summary(aov(AdjRating ~ SupCtrl * PosNeg + Error(Subject/(PosNeg * SupCtrl)), data = main[main$Trials == i,]))
  anovas_trials[i,1] <- b$`Error: Subject:SupCtrl`[[1]][1,4]
  anovas_trials[i,2] <- b$`Error: Subject:SupCtrl`[[1]][1,5]
  anovas_trials[i,3] <- b$`Error: Subject:PosNeg`[[1]][1,4]
  anovas_trials[i,4] <- b$`Error: Subject:PosNeg`[[1]][1,5]
  anovas_trials[i,5] <- b$`Error: Subject:PosNeg:SupCtrl`[[1]][1,4]
  anovas_trials[i,6] <- b$`Error: Subject:PosNeg:SupCtrl`[[1]][1,5]
}; remove(a,b)
round(ttest_trials,4)
round(anovas_trials,4)
# write.csv(anovas_trials,"figures/table2ph1.csv",row.names = F)



## ## ## ## EXPERIMENT 2, PHASE 2 ## ## ## ####
# extract relevant data
main <- lf_post[lf_post$Experiment == "Experiment 2" & lf_post$Phase == "Phase 2",]
# super and control factor
main$SupCtrl <- ifelse(main$Cue == "SP" | main$Cue == "SN", "super", "control")
# create contrast zero complement
main$zeroComp <- ifelse(main$Cue == "CN" | main$Cue == "SN", -1, 1)
main$AdjRating <- main$Rating * main$zeroComp 
# anova
e2_ph2 <- aov(AdjRating ~ (Trials * PosNeg * SupCtrl) + Error(Subject/(Trials*PosNeg*SupCtrl)), data = main)
summary(e2_ph2); effectsize::eta_squared(e2_ph2, partial = TRUE, ci.lvl = .95,
                                         alternative = "two.sided")
# graph
ggplot2::ggplot(main, aes(x = Trials, y = AdjRating, col = PosNeg, shape = SupCtrl)) + 
  stat_summary(position = position_dodge(0.1)) + theme_classic()



# Bayes Factor
main$trial2 <- as.factor(main$Trials)
main$PosNeg <- as.factor(main$PosNeg)
main$SupCtrl <- as.factor(main$SupCtrl)
bf <- anovaBF(AdjRating ~ trial2 * PosNeg * SupCtrl + Subject, data = main, whichRandom="Subject")
bf
plot(bf)
# compare not symmetry vs symmetry models
assym = lmBF(AdjRating ~ trial2 * PosNeg * SupCtrl, whichRandom="Subject",data = main)
symm = lmBF(AdjRating ~ trial2 * SupCtrl, whichRandom="Subject",data = main)
bf2 = symm / assym
bf2



# First trial Super vs Control in Positive and in Negative
t.test(Rating ~ SupCtrl, main[main$Trials == 1 & main$PosNeg == "positive",], paired = T)
t.test(Rating ~ SupCtrl, main[main$Trials == 1 & main$PosNeg == "negative",], paired = T)



## ## POST HOC E1_P1 t test per block ## ##
anovas_trials <- matrix(NA, 4, 6); colnames(anovas_trials) <- paste0(rep(c("F","p"),2),"_",
                                                                     rep(c("SupCtrl","PosNeg","interaction"),each=2))
for (i in 1:4) {
  # Andy's email on the 22/01/2021
  b <- summary(aov(AdjRating ~ SupCtrl * PosNeg + Error(Subject/(PosNeg * SupCtrl)), data = main[main$Trials == i,]))
  anovas_trials[i,1] <- b$`Error: Subject:SupCtrl`[[1]][1,4]
  anovas_trials[i,2] <- b$`Error: Subject:SupCtrl`[[1]][1,5]
  anovas_trials[i,3] <- b$`Error: Subject:PosNeg`[[1]][1,4]
  anovas_trials[i,4] <- b$`Error: Subject:PosNeg`[[1]][1,5]
  anovas_trials[i,5] <- b$`Error: Subject:PosNeg:SupCtrl`[[1]][1,4]
  anovas_trials[i,6] <- b$`Error: Subject:PosNeg:SupCtrl`[[1]][1,5]
}; remove(b)
round(anovas_trials,4)
# write.csv(anovas_trials,"figures/table2ph2.csv",row.names = F)



## ## ## ## ## ## ## ## ## ##  ## ## ## ## ## ## ## ## ## ## # # # # # # # # # #
## ## ## ## ## ## ## ## EXPERIMENT 1 and 2 ## ## ## ## ## ## # # # # # # # #####
## ## ## ## ## ## ## ## ## ##  ## ## ## ## ## ## ## ## ## ## # # # # # # # # # #

# # # # ANALYSIS (4 x 2 x 2 x 2) # # # #
# Experiment 1 and 2
main <- lf_post[lf_post$Phase == "Phase 2",]
# super and control factor
main$SupCtrl <- ifelse(main$Cue == "SP" | main$Cue == "SN", "super", "control")
# create contrast zero complement
main$zeroComp <- ifelse(main$Cue == "CN" | main$Cue == "SN", -1, 1)
main$AdjRating <- main$Rating * main$zeroComp 
# anova
e1and2_ph2 <- aov(AdjRating ~ (Trials * PosNeg * SupCtrl * Experiment) + 
                    Error(Subject/(Trials*PosNeg*SupCtrl)), data = main)
summary(e1and2_ph2); effectsize::eta_squared(e1and2_ph2, partial = TRUE, ci.lvl = .95,
                                             alternative = "two.sided")
ggplot2::ggplot(main, aes(x = Trials, y = AdjRating, col = Experiment)) + 
  stat_summary(position = position_dodge(0.2)) + theme_classic() +
  facet_grid(SupCtrl ~ PosNeg)



# # # # ANALYSIS (2 x 2 x 4) # # # #
# Experiment 1 and 2
# extract relevant data
main <- lf_post[lf_post$Phase == "Phase 2",]
# super and control factor
main$SupCtrl <- ifelse(main$Cue == "SP" | main$Cue == "SN", "super", "control")
# create contrast zero complement
main$zeroComp <- ifelse(main$Cue == "CN" | main$Cue == "SN", -1, 1)
main$AdjRating <- main$Rating * main$zeroComp 
# anova
main <- main[main$Cue == "CP" | main$Cue == "CN",]
# main <- main[main$Cue == "SP" | main$Cue == "SN",]
e1and2_ph2 <- aov(AdjRating ~ (Trials * PosNeg * Experiment) + Error(Subject/(Trials*PosNeg)), 
                  data = main)
summary(e1and2_ph2); effectsize::eta_squared(e1and2_ph2, partial = TRUE, ci.lvl = .95,
                                             alternative = "two.sided")
# graph
ggplot(main, aes(x = Trials, y = AdjRating, col = Experiment, shape = PosNeg)) + 
  stat_summary(position = position_dodge(0.2)) + theme_classic() #+ facet_grid(PosNeg ~ .)



## ## POST HOC trial by blocks ## ##
anovas_trials <- matrix(NA, 4, 6); colnames(anovas_trials) <- paste0(rep(c("F","p"),2),"_",
                                                                     rep(c("Exp","PosNeg","interaction"),each=2))
for (i in 1:4) {
  # Andy's email on the 22/01/2021
  b <- summary(aov(AdjRating ~ PosNeg * Experiment + Error(Subject/PosNeg), data = main[main$Trials == i,]))
  anovas_trials[i,1] <- b$`Error: Subject`[[1]][1,4]
  anovas_trials[i,2] <- b$`Error: Subject`[[1]][1,5]
  anovas_trials[i,3] <- b$`Error: Subject:PosNeg`[[1]][1,4]
  anovas_trials[i,4] <- b$`Error: Subject:PosNeg`[[1]][1,5]
  anovas_trials[i,5] <- b$`Error: Subject:PosNeg`[[1]][2,4]
  anovas_trials[i,6] <- b$`Error: Subject:PosNeg`[[1]][2,5]
}; remove(b,i)
rownames(anovas_trials) <- 1:4
round(anovas_trials,4)
# write.csv(anovas_trials,"figures/table2.5.csv",row.names = F)






## ## ## ## ## ## ## ## ## ##  ## ## ## ## ## ## ## ## ## ## # # # # # # # # # #
## ## ## ## ## ## ## ## EXPERIMENT 3 ## ## ## ## ## ## ## #### # # # # # # # # #
## ## ## ## ## ## ## ## ## ##  ## ## ## ## ## ## ## ## ## ## # # # # # # # # # #



# # # # # Phase 1 # # # # #
# extract relevant data
main <- db3[db3$phaseNum  == 1,]
# anova
e3_ph1 <- aov(rat0comp ~ Trials * PosNeg + Error(Subject/(Trials * PosNeg)), 
              data = main)
summary(e3_ph1); effectsize::eta_squared(e3_ph1, partial = TRUE, ci.lvl = .95,
                                         alternative = "two.sided")
# graph
ggplot2::ggplot(main, aes(x = Trials, y = rat0comp, col = PosNeg)) + 
  stat_summary(position = position_dodge(0.1)) + theme_classic()



# # # # # Phase 2 # # # # #
# extract relevant data
main <- db3[db3$phaseNum  == 2 & db3$ctrlType != "pre. (E & I)",]
# main <- main$trial
# anova
e3_ph2 <- aov(rat0comp ~ Trials * PosNeg * ctrlType  + Error(Subject/(Trials * PosNeg * ctrlType)), 
              data = main)
summary(e3_ph2); effectsize::eta_squared(e3_ph2, partial = TRUE, ci.lvl = .95,
                                         alternative = "two.sided")
# graph
ggplot2::ggplot(main, aes(x = Trials, y = rat0comp, col = PosNeg, shape = ctrlType)) + 
  stat_summary(position = position_dodge(0.1)) + theme_classic()



# Bayes factor
main$trial2 <- as.factor(main$Trials)
main$PosNeg <- as.factor(main$PosNeg)
main$ctrlType <- as.factor(main$ctrlType)
bf <- anovaBF(rat0comp ~ trial2 * PosNeg * ctrlType + Subject, data = main, whichRandom="Subject")
bf
plot(bf)
# compare not symmetry vs symmetry models
assym = lmBF(rat0comp ~ trial2 * PosNeg * ctrlType, whichRandom="Subject",data = main)
symm = lmBF(rat0comp ~ trial2 * ctrlType, whichRandom="Subject",data = main)
bf2 = symm / assym
bf2



## ## POST HOC E3_P2 anova per block ## ##
# anovas_trials <- matrix(NA, 5, 6); colnames(anovas_trials) <- paste0(rep(c("F","p"),2),"_",
#                                                                      rep(c("ctrlType","PosNeg","interaction"),each=2))
anovas_trials <- matrix(NA, 5, 2); colnames(anovas_trials) <- paste0(rep(c("F","p"),1),"_",
                                                                     rep(c("ctrlType"),each=2))
for (i in 1:nrow(anovas_trials)) {
  # Andy's email on the 30/04/2024
  b <- summary(aov(rat0comp ~ ctrlType + Error(Subject/ctrlType), data = main[main$Trials == levels(main$Trials)[i],]))
  anovas_trials[i,1] <- b$`Error: Subject:ctrlType`[[1]][1,4]
  anovas_trials[i,2] <- b$`Error: Subject:ctrlType`[[1]][1,5]
  # anovas_trials[i,3] <- b$`Error: Subject:PosNeg`[[1]][1,4]
  # anovas_trials[i,4] <- b$`Error: Subject:PosNeg`[[1]][1,5]
  # anovas_trials[i,5] <- b$`Error: Subject:PosNeg:ctrlType`[[1]][1,4]
  # anovas_trials[i,6] <- b$`Error: Subject:PosNeg:ctrlType`[[1]][1,5]
}; remove(b)
round(anovas_trials,4)
# write.csv(anovas_trials,"figures/table3ph2.csv",row.names = F)



# # # supplementary model before revision # # #
# no super control
e3_ph2 <- aov(rat0comp ~ Trials * PosNeg * ctrlType + Error(Subject/(Trials * PosNeg * ctrlType)), 
              data = main[main$ctrlType != "sup. ctrl. (K & C)",])
summary(e3_ph2); effectsize::eta_squared(e3_ph2, partial = TRUE, ci.lvl = .95,
                                         alternative = "two.sided")
ggplot2::ggplot(main[main$ctrlType != "sup. ctrl. (K & C)",], 
                aes(x = ctrlType, y = rat0comp, col = PosNeg, shape = ctrlType)) + 
  stat_summary(position = position_dodge(0.1)) + theme_classic()

# no relative validity control
e3_ph2 <- aov(rat0comp ~ Trials * PosNeg * ctrlType + Error(Subject/(Trials * PosNeg * ctrlType)), 
              data = main[main$ctrlType != "rel. val. ctrl. (B & R)",])
summary(e3_ph2); effectsize::eta_squared(e3_ph2, partial = TRUE, ci.lvl = .95,
                                         alternative = "two.sided")
ggplot2::ggplot(main[main$ctrlType != "rel. val. ctrl. (B & R)",], 
                aes(x = ctrlType, y = rat0comp, col = PosNeg, shape = ctrlType)) + 
  stat_summary(position = position_dodge(0.1)) + theme_classic()

# no experimental condition
e3_ph2 <- aov(rat0comp ~ Trials * PosNeg * ctrlType + Error(Subject/(Trials * PosNeg * ctrlType)), 
              data = main[main$ctrlType != "sup. (S & Z)",])
summary(e3_ph2); effectsize::eta_squared(e3_ph2, partial = TRUE, ci.lvl = .95,
                                         alternative = "two.sided")
ggplot2::ggplot(main[main$ctrlType != "sup. (S & Z)",], 
                aes(x = ctrlType, y = rat0comp, col = PosNeg, shape = ctrlType)) + 
  stat_summary(position = position_dodge(0.1)) + theme_classic()


