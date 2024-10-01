# Script made by Santiago Castiello 30/05/2021. Any comments, questions or 
# suggestions please write me at santiago.castiello@ccc.ox.ac.uk or santiagocdo@gmail.com
# To see more ideas of analysis, visualizations and code go to my github account
# you can find it in my personal website: 
# https://sites.google.com/view/santiagocastiello/home

# Remove all of the elements currently loaded in R
rm(list=ls(all=TRUE))
# set work directory in the same place where this script is saved
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# call my functions
source("functions.R")

# libraries
if (!require(ggplot2)) {install.packages("ggplot2")}; library(ggplot2) # ggplot()
if (!require(ggpubr)) {install.packages("ggpubr")}; library(ggpubr) # ggarrange()
if (!require(cowplot)) {install.packages("cowplot")}; library(cowplot) # ggdraw()

# load data bases from experiment 1 & 2 and experiment 3 
db <- read.csv("data/lf_old_e1&e4.csv")



# # # # # Experiment 1# # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
db1 <- db[db$Experiment == "Experiment 1",]

# add adjusted rating
db1$adjRating <- ifelse(db1$Cue == "v" | db1$Cue == "n" |
                          db1$Cue == "CN" | db1$Cue == "SN", -1, 1)
db1$adjRating <- db1$Rating * db1$adjRating 

# # # # # # # # # # Paper Figure 2 # # # # # # # # # #
db1$Cue2 <- factor(dplyr::recode(db1$Cue, "n"="A", "p"="B", "u"="u", "v"="v",
                                 "SP"="C","SN"="D","CP"="E","CN"="F",),
                   levels = c("A","B","u","v","C","D","E","F"))

fig2a <- ggplot(db1, aes(x = Trials, y = Rating, shape = Cue2, fill = Cue2)) +  
  labs(y = "Mean Rating", x = "Trials") +
  geom_hline(yintercept = 0, col = "gray50") +
  stat_summary(geom = "line",position = position_dodge(0.2)) +
  stat_summary(fun.data = "mean_se",position = position_dodge(0.2)) +
  scale_shape_manual(name = "Test Cues",
                     values = c(21,21,3,4,24,25,22,22)) + #"p","u","v","n","SP","CP1","CP2","CN2","CN1","SN"
  scale_fill_manual(name = "Test Cues",
  values = c("black","white","black","black","white","black","white","black")) +
  scale_y_continuous(breaks = seq(-60, 60, by = 20)) +
  scale_x_continuous(breaks = seq(0, 8, by = 1)) +
  facet_grid(. ~ Phase, scales = "free_x", space = "free_x") +
  theme_classic()
# fig2a

fig2b <- ggplot(db1, aes(x = Trials, y = adjRating, shape = Cue2, fill = Cue2)) +  
  labs(y = "Mean Adj. Rating", x = "Trials") +
  geom_hline(yintercept = 0, col = "gray50") +
  stat_summary(geom = "line",position = position_dodge(0.2)) +
  stat_summary(fun.data = "mean_se",position = position_dodge(0.2)) +
  scale_shape_manual(name = "Test Cues",
                     values = c(21,21,3,4,24,25,22,22)) + #"p","u","v","n","SP","CP1","CP2","CN2","CN1","SN"
  scale_fill_manual(name = "Test Cues",
                    values = c("black","white","black","black","white","black","white","black")) +
  scale_y_continuous(breaks = seq(0, 60, by = 20)) +
  scale_x_continuous(breaks = seq(0, 8, by = 1)) +
  facet_grid(. ~ Phase, scales = "free_x", space = "free_x") +
  theme_classic()
# fig2b

fig2 <- ggarrange(fig2a,fig2b, nrow=2, common.legend = T, labels = c("A","B"))
fig2



# # # # # Experiment 2# # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
db2 <- db[db$Experiment == "Experiment 2",]

# add adjusted rating
db2$adjRating <- ifelse(db2$Cue == "v" | db2$Cue == "n" |
                          db2$Cue == "CN" | db2$Cue == "SN", -1, 1)
db2$adjRating <- db2$Rating * db2$adjRating 

# # # # # # # # # # Paper Figure 2 # # # # # # # # # #
db2$Cue2 <- factor(dplyr::recode(db2$Cue, "n"="A", "p"="B", "u"="u", "v"="v",
                                 "SP"="C","SN"="D","CP"="G","CN"="H"),
                   levels = c("A","B","u","v","C","D","G","H"))

fig3a <- ggplot(db2, aes(x = Trials, y = Rating, shape = Cue2, fill = Cue2)) +  
  labs(y = "Mean Rating", x = "Trials") +
  geom_hline(yintercept = 0, col = "gray50") +
  stat_summary(geom = "line",position = position_dodge(0.2)) +
  stat_summary(fun.data = "mean_se",position = position_dodge(0.2)) +
  scale_shape_manual(name = "Test Cues",
                     values = c(21,21,3,4,24,25,23,23)) + #"p","u","v","n","SP","CP1","CP2","CN2","CN1","SN"
  scale_fill_manual(name = "Test Cues",
                    values = c("black","white","black","black","white","black","white","black")) +
  scale_y_continuous(breaks = seq(-60, 60, by = 20)) +
  scale_x_continuous(breaks = seq(0, 8, by = 1)) +
  facet_grid(. ~ Phase, scales = "free_x", space = "free_x") +
  theme_classic()
# fig3a

fig3b <- ggplot(db2, aes(x = Trials, y = adjRating, shape = Cue2, fill = Cue2)) +  
  labs(y = "Mean Adj. Rating", x = "Trials") +
  geom_hline(yintercept = 0, col = "gray50") +
  stat_summary(geom = "line",position = position_dodge(0.2)) +
  stat_summary(fun.data = "mean_se",position = position_dodge(0.2)) +
  scale_shape_manual(name = "Test Cues",
                     values = c(21,21,3,4,24,25,23,23)) + #"p","u","v","n","SP","CP1","CP2","CN2","CN1","SN"
  scale_fill_manual(name = "Test Cues",
                    values = c("black","white","black","black","white","black","white","black")) +
  scale_y_continuous(breaks = seq(0, 60, by = 20)) +
  scale_x_continuous(breaks = seq(0, 8, by = 1)) +
  facet_grid(. ~ Phase, scales = "free_x", space = "free_x") +
  theme_classic()
# fig3b

fig3 <- ggarrange(fig3a,fig3b, nrow=2, common.legend = T, labels = c("A","B"))
fig3



# # # # # Experiment 3# # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
db3 <- f_load_SSL_e2(write_csv = 1)

# add correct cue labeling
db3$rat$stim <- as.factor(db3$rat$stim)
levels(db3$rat$stim) <- c("E","G","B","A","H","F","C","D") #c("R","C","E","I","K","V","S","Z")
db3$rat$stim <- factor(db3$rat$stim, levels = c("A","B","C","D","E","F","G","H"))

# remove pre cues in phase 2
db3$rat <- db3$rat[!(db3$rat$phaseNum == 2 & db3$rat$ctrlType == "pre. (E & I)"),]

# create log x axis
db3$rat$nRat[db3$rat$phaseNum == 2] <- db3$rat$nRat[db3$rat$phaseNum == 2] - 16
db3$rat$nRat <- as.factor(db3$rat$nRat)
labs <- levels(db3$rat$nRat)
db3$rat$nRat <- as.integer(db3$rat$nRat)

# New facet label names for supp variable
phaseNumLabs <- c("Phase 1", "Phase 2")
names(phaseNumLabs) <- c("1", "2")


fig4a <- ggplot(db3$rat, aes(x=nRat,y=rat,shape=stim,fill=stim)) +  
  labs(y = "Mean Rating", x = "Trials") +
  geom_hline(yintercept = 0, col = "gray50") +
  stat_summary(geom = "line",position = position_dodge(0.2)) +
  stat_summary(fun.data = "mean_se",position = position_dodge(0.2)) +
  scale_shape_manual(name = "Test Cues:",
                     values = c(21,21,24,25,22,22,23,23)) + #"p","u","v","n","SP","CP1","CP2","CN2","CN1","SN"
  scale_fill_manual(name = "Test Cues:",
                    values = c("black","white","white","black","white","black","white","black")) +
  scale_y_continuous(breaks = seq(-60, 60, by = 20)) +
  scale_x_continuous(breaks = seq(1, 5, by = 1), labels = labs) +
  facet_grid(. ~ phaseNum, scales = "free_x", space = "free_x",
             labeller=labeller(phaseNum=phaseNumLabs)) +
  theme_classic()
# fig4a

fig4b <- ggplot(db3$rat, aes(x=nRat,y=rat0comp,shape=stim,fill=stim)) +  
  labs(y = "Mean Adj. Rating", x = "Trials") +
  geom_hline(yintercept = 0, col = "gray50") +
  stat_summary(geom = "line",position = position_dodge(0.2)) +
  stat_summary(fun.data = "mean_se",position = position_dodge(0.2)) +
  scale_shape_manual(name = "Test Cues:",
                     values = c(21,21,24,25,22,22,23,23)) + #"p","u","v","n","SP","CP1","CP2","CN2","CN1","SN"
  scale_fill_manual(name = "Test Cues:",
                    values = c("black","white","white","black","white","black","white","black")) +
  scale_y_continuous(breaks = seq(-60, 60, by = 20)) +
  scale_x_continuous(breaks = seq(1, 5, by = 1), labels = labs) +
  facet_grid(. ~ phaseNum, scales = "free_x", space = "free_x",
             labeller=labeller(phaseNum=phaseNumLabs)) +
  theme_classic()
# fig4b

fig4 <- ggarrange(fig4a,fig4b, nrow=2, common.legend = T, labels = c("A","B"))
fig4






print_fig <- 0
if (print_fig == 1) {
  ggsave("figures/fig2.pdf",plot = fig2,
         width = 12, height = 12, units = "cm", dpi = 1200, limitsize = T)
  ggsave("figures/fig3.pdf",plot = fig3,
         width = 12, height = 12, units = "cm", dpi = 1200, limitsize = T)
  ggsave("figures/fig4.pdf",plot = fig4,
         width = 12, height = 12, units = "cm", dpi = 1200, limitsize = T)
}