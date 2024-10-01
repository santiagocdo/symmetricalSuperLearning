# code written on the 17/08/2022 by Santiago Castiello de Obeso
# erase previous Global Environment
rm(list = ls())
# set work directory in the same place where this script is saved
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("simulations/functions.R")



# # # # # # # # # # training protocol # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# read csv file
exp1 <- read.csv("simulations/exp1_training_protocol.csv")
exp2 <- read.csv("simulations/exp2_training_protocol.csv")
exp3 <- read.csv("simulations/exp3_training_protocol.csv")

# prepare data for simulations
train1 <- f_prepData(exp1)
train2 <- f_prepData(exp2)
train3 <- f_prepData(exp3)

# rescorla wagner parameters
param <- list(LR=0.2,nO=1)

# number of simulations
nSim <- 100



# # # # # # # # # # simulations # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
for (s in 1:nSim) {
  e1 <- f_RW_mod(param,train1)
  e2 <- f_RW_mod(param,train2)
  e3 <- f_RW_mod(param,train3)
 
  colnames(e1$V) <- colnames(train1$x)
  colnames(e2$V) <- colnames(train2$x)
  colnames(e3$V) <- colnames(train3$x)
  
  Ve1 <- cbind(train1$trialType,e1$V[,c("in.S","in.Z","in.R","in.V")])
  Ve2 <- cbind(train2$trialType,e2$V[,c("in.S","in.Z","in.C","in.K")])
  Ve3 <- cbind(train3$trialType,e3$V[,c("in.S","in.Z","in.R","in.V","in.C","in.K")])
  
  Ve1 <- reshape2::melt(Ve1, id.vars = c("ph","blocks","trials","trialType"))
  Ve2 <- reshape2::melt(Ve2, id.vars = c("ph","blocks","trials","trialType"))
  Ve3 <- reshape2::melt(Ve3, id.vars = c("ph","blocks","trials","trialType"))
  
  Vs <- rbind(data.frame(exp=1,Ve1),data.frame(exp=2,Ve2),data.frame(exp=3,Ve3))
  
  if (s == 1) {
    sim <- data.frame(subject=s,Vs)
  } else {
    sim <- rbind(sim,data.frame(subject=s,Vs))
  }
}

sim$variable <- factor(sim$variable,levels=paste0("in.",c("S","R","C","K","V","Z")))
# after review (30/04/2024)
levels(sim$variable) <- c("C","E","G","H","F","D")

expLabs <- c("Experiment 1", "Experiment 2", "Experiment 3")
names(expLabs) <- c("1", "2", "3")

library(ggplot2)
fig5 <- ggplot(sim, aes(x=blocks,y=value,shape=variable,fill=variable)) +
  labs(title="Rescorla-Wagner simulation",y="Associative Strenght (V)",x="Blocks") +
  geom_hline(yintercept = 0, col = "gray50") +
  stat_summary(geom="line", position=position_dodge(0.1)) + 
  stat_summary(fun.data="mean_cl_boot",position=position_dodge(0.1)) + 
  scale_x_continuous(breaks = seq(0,10,by=2)) +
  scale_shape_manual(name = "Cues",
                     values = c(24,22,23,23,22,25),
                     labels = c("C [super excitatory (+)]","E [relative validity (+)]",
                                "G [super control (+)]","H [super control (-)]",
                                "F [relative validity (-)]","D [super inhibitory (-)]")) +
  scale_fill_manual(name = "Cues",
                    values = c(rep("white",3),rep("black",3)),
                    labels = c("C [super excitatory (+)]","E [relative validity (+)]",
                               "G [super control (+)]","H [super control (-)]",
                               "F [relative validity (-)]","D [super inhibitory (-)]")) +
  facet_grid(.~exp,labeller = labeller(exp=expLabs)) + theme_classic()
fig5

print_fig <- 0
if (print_fig == 1) {
  ggsave("figures/fig5.pdf",plot = fig5,
         width = 18, height = 12, units = "cm", dpi = 1200, limitsize = T)
}
