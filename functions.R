# load data set from experiment 1 (new)
f_load_SSL_e1 <- function(write_csv) {
  
  # libraries
  if (!require(plyr)) {install.packages("plyr")}; library(plyr) # revalue()
  
  # data folder name
  datFolName <- "data_exp_68408-v14"
  
  # load .csv files names
  file_names <- list.files(path = paste0("symmetry2_gaby/",datFolName),pattern = ".csv")
  
  # combine both arms into one phase 1 data set
  ph1 <- rbind(read.csv(paste0("symmetry2_gaby/",datFolName,"/",
                               file_names[grepl("2zvi",file_names)])),
               read.csv(paste0("symmetry2_gaby/",datFolName,"/",
                               file_names[grepl("ngb6",file_names)])))
  ph1 <- rbind(read.csv(paste0("symmetry2_gaby/",datFolName,"/",
                               file_names[grepl("2zvi",file_names)])),
               read.csv(paste0("symmetry2_gaby/",datFolName,"/",
                               file_names[grepl("ngb6",file_names)])))
  
  # combine both arms into one phase 2 data set
  ph2.1 <- rbind(read.csv(paste0("symmetry2_gaby/",datFolName,"/",
                                 file_names[grepl("hgt6",file_names)])),
                 read.csv(paste0("symmetry2_gaby/",datFolName,"/",
                                 file_names[grepl("hkvy",file_names)])))
  ph2.2 <- rbind(read.csv(paste0("symmetry2_gaby/",datFolName,"/",
                                 file_names[grepl("o6yy",file_names)])),
                 read.csv(paste0("symmetry2_gaby/",datFolName,"/",
                                 file_names[grepl("qvln",file_names)])))
  ph2.3 <- rbind(read.csv(paste0("symmetry2_gaby/",datFolName,"/",
                                 file_names[grepl("7s4k",file_names)])),
                 read.csv(paste0("symmetry2_gaby/",datFolName,"/",
                                 file_names[grepl("ocii",file_names)])))
  ph2 <- rbind(ph2.1,ph2.2,ph2.3)
  
  # relevant columns
  relCols <- c("Participant.Private.ID","Participant.Public.ID","Trial.Number",
               "Zone.Type","Response","Correct","phase","trials","nRating","condition")
  
  # filter relevant columns
  ph1 <- ph1[,relCols]
  ph2 <- ph2[,relCols]
  
  # combine both arms into one phase 1 data set
  spq <- read.csv(paste0("symmetry2_gaby/data_exp_68408-v14/",
                         file_names[grepl("j1wq",file_names)]))
 
  
  
  # remove irrelevant created elements from the environment (some of my OCD traits)
  relCols <- c("Participant.Private.ID","Participant.Public.ID","Question.Key","Response")
  spq <- spq[grepl("quantised",spq$Question.Key),relCols]
  spq$Response <- as.integer(as.character(spq$Response))
  
  
  
  
  # remove irrelevant structures
  remove(file_names,relCols,ph2.1,ph2.2,ph2.3)
  
  
  
  # # # # # # # # # # Ratings # # # # # # # # # # # # # # # # # # # # # # # # ####
  # filter by rows (only the end value of a rating)
  ph1_rat <- ph1[ph1$Zone.Type == "response_slider_endValue",]
  ph2_rat <- ph2[ph2$Zone.Type == "response_slider_endValue",]
  
  # change trial number variable type
  ph1_rat$Trial.Number <- as.integer(as.character(ph1_rat$Trial.Number))
  ph2_rat$Trial.Number <- as.integer(as.character(ph2_rat$Trial.Number))
  
  # change the variable to numeric
  ph1_rat$Response <- as.integer(as.character(ph1_rat$Response))
  ph2_rat$Response <- as.integer(as.character(ph2_rat$Response))
  
  # add correct rating number phase 1
  ph1_rat$nRat <- as.integer(substr(ph1_rat$nRating,5,nchar(ph1_rat$nRating)))
  # transform ratings from [0,100] to [-100,100]
  ph1_rat$Response <- (ph1_rat$Response*2)-100
  
  # add correct rating number phase 2
  ph2_rat$nRat <- as.integer(substr(ph2_rat$nRating,5,nchar(ph2_rat$nRating)))
  ph2_rat$nRat <- ph2_rat$nRat + 8 # add 8 trials from phase 1
  # transform ratings from [0,100] to [-100,100]
  ph2_rat$Response <- (ph2_rat$Response*2)-100
  
  # Add 0 complement ratings
  ph1_rat$rat0comp <- as.integer(revalue(ph1_rat$condition,c("E"=1,"I"=-1)))
  ph1_rat$rat0comp <- ph1_rat$rat0comp * ph1_rat$Response
  ph2_rat$rat0comp <- as.integer(revalue(ph2_rat$condition,c("E"=1,"I"=-1,"K"=-1,"C"=1,
                                                             "Z"=-1,"B"=1,"R"=-1,"S"=1)))
  ph2_rat$rat0comp <- ph2_rat$rat0comp * ph2_rat$Response
  
  # combine phases
  db_rat <- data.frame(phase = c(rep("ph1",each=nrow(ph1_rat)),
                                 rep("ph2",each=nrow(ph2_rat))),
                       rbind(ph1_rat,ph2_rat))
  
  # participants vector
  subjs <- intersect(db_rat$Participant.Private.ID,spq$Participant.Private.ID)
  nSubj <- length(subjs)
  
  # add spq
  db_rat$spq <- NA
  for (i in 1:nSubj) {
    db_rat$spq[db_rat$Participant.Private.ID == subjs[i]] <-
      sum(spq$Response[spq$Participant.Private.ID == subjs[i]])
    
    
  }
  
  # create factor for excitation and inhibitory
  db_rat$excInh <- revalue(db_rat$condition,c("E"="Inh","I"="Exc","K"="Inh","C"="Exc",
                                              "Z"="Inh","B"="Exc","R"="Inh","S"="Exc"))
  db_rat$ctrlType <- revalue(db_rat$condition,c("E"="pre. (E & I)","I"="pre. (E & I)","K"="sup. ctrl. (K & C)","C"="sup. ctrl. (K & C)",
                                                "Z"="sup. (S & Z)","B"="rel. val. ctrl. (B & R)","R"="rel. val. ctrl. (B & R)","S"="sup. (S & Z)"))
  
  
  
  # # # # # # # # # # Behaviour # # # # # # # # # # # # # # # # # # # # # # # ####
  # filter by rows (only when the keyboard was pressed)
  ph1_beh <- ph1[ph1$Zone.Type == "response_keyboard",]
  ph2_beh <- ph2[ph2$Zone.Type == "response_keyboard",]
  dim(ph1_beh[ph1_beh$Participant.Private.ID=="5881945",])
  dim(ph2_beh[ph2_beh$Participant.Private.ID=="5881945",])
  ph1_beh$Trial.Number <- rep(1:32,nSubj)
  ph2_beh$Trial.Number <- rep(1:384,nSubj)
  
  # combine phases
  db_beh <- data.frame(phase = c(rep("ph1",each=nrow(ph1_beh)),
                                 rep("ph2",each=nrow(ph2_beh))),
                       rbind(ph1_beh,ph2_beh))
  
  # change the variable to numeric
  db_beh$Response <- ifelse(db_beh$Response=="u",1,0)
  # add condition type (excitatory or inhibitory)
  db_beh$excInh <- revalue(db_beh$condition,c("Ie1"="Exc","e1"="Exc","SI"="Exc","I"="Exc",
                                              "Ce2"="Exc","e2"="Exc","Be3"="Exc","e3"="Exc",
                                              "Ee4"="Inh","e4"="Inh","ZE"="Inh","E"="Inh",
                                              "Ke5"="Inh","e5"="Inh","Re6"="Inh","e6"="Inh"))
  # change character to factor
  db_beh$condition <- factor(db_beh$condition, levels=c("Ie1","e1","SI","I",
                                                        "Ce2","e2","Be3","e3",
                                                        "Ee4","e4","ZE","E",
                                                        "Ke5","e5","Re6","e6"))
  levels(db_beh$condition) <- paste0(levels(db_beh$condition),
                                     c("-","+","+","-","+","-","+","+",
                                       "+","-","-","+","-","+","-","-"))
  
  db_rat$trials <- db_rat$Zone.Type <- db_rat$Correct <- NULL
  colnames(db_rat) <- c("phase","parPri","parPub","trial","rat","phaseNum",
                        "nRating","stim","nRat","rat0comp","spq","excInh",
                        "ctrlType")
  db_beh$Zone.Type <- db_beh$trials <- db_beh$nRating <- NULL
  colnames(db_beh) <- c("phase","parPri","parPub","trial","predResp","corr",
                        "phaseNum","trialType","excInh")
 
  if (write_csv == 1) {
    write.csv(db_rat,"data/rat_exp1.csv",na="",row.names=F)
    write.csv(db_beh,"data/beh_exp1.csv",na="",row.names=F)
  } 
  
  return(list(rat=db_rat,beh=db_beh))
} # end function for experiment 1

# load data set from experiment 2 (new)
f_load_SSL_e2 <- function(write_csv) {
  
  # libraries
  if (!require(plyr)) {install.packages("plyr")}; library(plyr) # revalue()
  
  # data folder name
  datFolName <- "data_exp_88195-v10"
  
  # load .csv files names fro our work directory (i.e., where this R script is saved)
  file_names <- list.files(path = paste0("../symmetry2_block_practical/",datFolName), pattern = ".csv")
  
  # combine both phase 1 arms (counterbalanced stimuli) into one
  ph1 <- rbind(read.csv(paste0("../symmetry2_block_practical/",datFolName,"/",
                               file_names[grepl("racf",file_names)])),
               read.csv(paste0("../symmetry2_block_practical/",datFolName,"/",
                               file_names[grepl("9pcx",file_names)])))
  
  # combine both phase 2 arms (counterbalanced stimuli) into one
  # first phase 2 blocks
  ph2.1 <- rbind(read.csv(paste0("../symmetry2_block_practical/",datFolName,"/",
                                 file_names[grepl("okxx",file_names)])),
                 read.csv(paste0("../symmetry2_block_practical/",datFolName,"/",
                                 file_names[grepl("vgx1",file_names)])))
  # second phase 2 blocks
  ph2.2 <- rbind(read.csv(paste0("../symmetry2_block_practical/",datFolName,"/",
                                 file_names[grepl("4mm1",file_names)])),
                 read.csv(paste0("../symmetry2_block_practical/",datFolName,"/",
                                 file_names[grepl("z11d",file_names)])))
  
  
  
  
  
  # combine first and second phase 2 blocks
  ph2 <- rbind(ph2.1,ph2.2)
  
  # vector with relevant columns' names
  relCols <- c("Participant.Private.ID","Participant.Public.ID","Trial.Number",
               "Zone.Type","Response","Correct","phase","trials","nRating","condition")
  
  # filter data.frame phases by relevant columns
  ph1 <- ph1[,relCols]
  ph2 <- ph2[,relCols]
  
  # get SPQ and BDI2
  spq <- read.csv(paste0("../symmetry2_block_practical/",datFolName,"/",
                         file_names[grepl("1p7h",file_names)]))
  bdi <- read.csv(paste0("../symmetry2_block_practical/",datFolName,"/",
                         file_names[grepl("1z9u",file_names)]))
  
  # relevant columns
  relCols <- c("Participant.Private.ID","Participant.Public.ID","Question.Key","Response")
  # filter rows by any row in Question.Key not saying "quantised" buy yes "bdi-"
  bdi <- bdi[!grepl("quantised",bdi$Question.Key) & grepl("bdi-",bdi$Question.Key),relCols]
  bdi$Response <- as.integer(substr(bdi$Response,1,1))
  spq <- spq[grepl("quantised",spq$Question.Key),relCols]
  spq$Response <- as.integer(as.character(spq$Response))
  
  # remove irrelevant created elements from the environment (some of my OCD traits)
  remove(file_names,relCols,ph2.1,ph2.2)
  
  
  
  # # # # # # # # # # Ratings # # # # # # # # # # # # # # # # # # # # # # # # ####
  # filter by rows (only the end value of a rating)
  ph1_rat <- ph1[ph1$Zone.Type == "response_slider_endValue",]
  ph2_rat <- ph2[ph2$Zone.Type == "response_slider_endValue",]
  
  # change trial number variable type
  ph1_rat$Trial.Number <- as.integer(as.character(ph1_rat$Trial.Number))
  ph2_rat$Trial.Number <- as.integer(as.character(ph2_rat$Trial.Number))
  
  # change the variable to numeric
  ph1_rat$Response <- as.integer(as.character(ph1_rat$Response))
  ph2_rat$Response <- as.integer(as.character(ph2_rat$Response))
  
  # add correct rating number phase 1
  ph1_rat$nRat <- as.integer(substr(ph1_rat$nRating,5,nchar(ph1_rat$nRating)))
  # # # # # visualize phase 1 ratings # # # # #
  ph1_rat$Response <- (ph1_rat$Response*2)-100
  
  # add correct rating number phase 2
  ph2_rat$nRat <- as.integer(substr(ph2_rat$nRating,5,nchar(ph2_rat$nRating)))
  ph2_rat$nRat <- ph2_rat$nRat + 16 # add 16 trials from phase 1
  # transform ratings from [0,100] to [-100,100]
  ph2_rat$Response <- (ph2_rat$Response*2)-100
  
  # Add 0 complement ratings
  ph1_rat$rat0comp <- as.integer(revalue(ph1_rat$condition,c("E"=1,"I"=-1)))
  ph1_rat$rat0comp <- ph1_rat$rat0comp * ph1_rat$Response
  ph2_rat$rat0comp <- as.integer(revalue(ph2_rat$condition,c("E"=1,"I"=-1,"K"=-1,"C"=1,
                                                             "Z"=-1,"B"=1,"R"=-1,"S"=1)))
  ph2_rat$rat0comp <- ph2_rat$rat0comp * ph2_rat$Response
  
  # combine phases
  db_rat <- data.frame(phase = c(rep("ph1",each=nrow(ph1_rat)),
                                 rep("ph2",each=nrow(ph2_rat))),
                       rbind(ph1_rat,ph2_rat))
  
  # participants vector
  subjs <- intersect(db_rat$Participant.Private.ID,spq$Participant.Private.ID)
  nSubj <- length(subjs)
  
  # add spq and bdi
  db_rat$spq <- db_rat$bdi <- NA
  for (i in 1:nSubj) {
    db_rat$spq[db_rat$Participant.Private.ID == subjs[i]] <-
      sum(spq$Response[spq$Participant.Private.ID == subjs[i]])
    db_rat$bdi[db_rat$Participant.Private.ID == subjs[i]] <-
      sum(bdi$Response[bdi$Participant.Private.ID == subjs[i]])
  }
  
  # create factor for excitation and inhibitory
  db_rat$excInh <- revalue(db_rat$condition,c("E"="Inh","I"="Exc","K"="Inh","C"="Exc",
                                              "Z"="Inh","B"="Exc","R"="Inh","S"="Exc"))
  db_rat$ctrlType <- revalue(db_rat$condition,c("E"="pre. (E & I)","I"="pre. (E & I)","K"="sup. ctrl. (K & C)","C"="sup. ctrl. (K & C)",
                                                "Z"="sup. (S & Z)","B"="rel. val. ctrl. (B & R)","R"="rel. val. ctrl. (B & R)","S"="sup. (S & Z)"))
  
  
  
  # # # # # # # # # # Behaviour # # # # # # # # # # # # # # # # # # # # # # # ####
  # filter by rows (only when the keyboard was pressed)
  ph1_beh <- ph1[ph1$Zone.Type == "response_keyboard",]
  ph2_beh <- ph2[ph2$Zone.Type == "response_keyboard",]
  dim(ph1_beh[ph1_beh$Participant.Private.ID=="6526300",])
  dim(ph2_beh[ph2_beh$Participant.Private.ID=="6526300",])
  ph1_beh$Trial.Number <- rep(1:64,nSubj)
  ph1_beh$Block.Number <- rep(rep(1:16,each=4),nSubj)
  ph2_beh$Trial.Number <- rep(1:192,nSubj)
  ph2_beh$Block.Number <- rep(rep(1:16,each=12),nSubj)
  
  # combine phases
  db_beh <- data.frame(phase = c(rep("ph1",each=nrow(ph1_beh)),
                                 rep("ph2",each=nrow(ph2_beh))),
                       rbind(ph1_beh,ph2_beh))
  
  # change the variable to numeric
  db_beh$Response <- ifelse(db_beh$Response=="u",1,0)
  # add condition type (excitatory or inhibitory)
  db_beh$excInh <- revalue(db_beh$condition,c("Ie1"="Exc","e1"="Exc","SI"="Exc","I"="Exc",
                                              "Ce2"="Exc","e2"="Exc","Be3"="Exc","e3"="Exc",
                                              "Ee4"="Inh","e4"="Inh","ZE"="Inh","E"="Inh",
                                              "Ke5"="Inh","e5"="Inh","Re6"="Inh","e6"="Inh"))
  db_beh$ctrlType <- revalue(db_beh$condition,
                             c("Ie1"="pre. (E & I)","e1"="pre. (E & I)",
                               "SI"="sup. (S & Z)","I"="sup. (S & Z)",
                               "Ce2"="sup. ctrl. (K & C)","e2"="sup. ctrl. (K & C)",
                               "Be3"="rel. val. ctrl. (B & R)","e3"="rel. val. ctrl. (B & R)",
                               "Ee4"="pre. (E & I)","e4"="pre. (E & I)",
                               "ZE"="sup. (S & Z)","E"="sup. (S & Z)",
                               "Ke5"="sup. ctrl. (K & C)","e5"="sup. ctrl. (K & C)",
                               "Re6"="rel. val. ctrl. (B & R)","e6"="rel. val. ctrl. (B & R)"))
  
  # change character to factor
  db_beh$condition <- factor(db_beh$condition, levels=c("Ie1","e1","SI","I",
                                                        "Ce2","e2","Be3","e3",
                                                        "Ee4","e4","ZE","E",
                                                        "Ke5","e5","Re6","e6"))
  levels(db_beh$condition) <- paste0(levels(db_beh$condition),
                                     c("-","+","+","-","+","-","+","+",
                                       "+","-","-","+","-","+","-","-"))
  
  db_rat$trials <- db_rat$Zone.Type <- db_rat$Correct <- NULL
  colnames(db_rat) <- c("phase","parPri","parPub","trial","rat","phaseNum",
                        "nRating","stim","nRat","rat0comp","bdi","spq","excInh",
                        "ctrlType")
  db_beh$Zone.Type <- db_beh$trials <- db_beh$nRating <- NULL
  colnames(db_beh) <- c("phase","parPri","parPub","trial","predResp","corr",
                        "phaseNum","trialType","block","excInh","ctrlType")
  
  if (write_csv == 1) {
    write.csv(db_rat,"data/rat_exp2.csv",na="",row.names=F)
    write.csv(db_beh,"data/beh_exp2.csv",na="",row.names=F)
  } 
  
  return(list(rat=db_rat,beh=db_beh))
} # end function for experiment 2

f_generalCharacteristics_e2 <- function() {
  # data folder name
  datFolName <- "data_exp_88195-v10"
  # load .csv files names fro our work directory (i.e., where this R script is saved)
  file_names <- list.files(path = paste0("../symmetry2_block_practical/",datFolName), pattern = ".csv")
  # read consent questionnaire
  db <- read.csv(paste0("../symmetry2_block_practical/",datFolName,"/",
                        file_names[grepl("questionnaire-3237",file_names)]))
  
  ids <- unique(db$Participant.Private.ID)[!is.na(unique(db$Participant.Private.ID))]
  sex <- db$Response[db$Question.Key == "resp-sex"]
  age <- as.integer(db$Response[db$Question.Key == "resp-age"])
  
  genChar <- data.frame(ids,sex,age)
  summary <- list(n=length(ids),
                  sex=data.frame(cbind(freq=table(sex),prop=table(sex)/length(ids))),
                  age=data.frame(mean=mean(age),sd=sd(age),range=range(age)))
  return(list(genChar=genChar,summary=summary))
}

# load data set from experiments 1 and 2 (old)
f_clean_old_exp <- function(write_csv) {
  
}

# old figures adapted from "ANOVAs_analysis_V8.R"
f_old_fig <- function(db) {
  
  # Figure 2, Phase 1
  for_plot2 <- db[db$Phase != "Phase 2",]
  
  # add adjusted rating
  for_plot2$adjRating <- ifelse(for_plot2$Cue == "v" | for_plot2$Cue == "n", -1, 1)
  for_plot2$adjRating <- for_plot2$Rating * for_plot2$adjRating 
  
  # # # # # # # # # # Paper Figure 2 # # # # # # # # # #
  for_plot2$Cue <- factor(for_plot2$Cue, levels = c("p","u","v","n"))
  fig2 <- ggplot(for_plot2, aes(x = Trials, y = Rating, shape = Cue, fill = Cue)) +  
    labs(y = "Mean Rating", x = "Blocks") +
    geom_hline(yintercept = 0, col = "gray50") +
    stat_summary(geom = "line",position = position_dodge(0.1)) +
    stat_summary(fun.data = "mean_cl_boot",position = position_dodge(0.1)) +
    facet_grid(. ~ Experiment) +
    scale_shape_manual(name = "Test\n Cues",
                       values = c(21,21,22,22), # c(15,22,3,4,8,11,16,15,17,18) #22,23,18,15
                       breaks = c("p","u","v","n"),
                       labels = c("E","e1","e2","I")) +
    scale_fill_manual(name = "Test\n Cues",
                      values = c("white","black","black","white"), # c(15,22,3,4,8,11,16,15,17,18)
                      breaks = c("p","u","v","n"),
                      labels = c("E","e1","e2","I")) + # c("p / pv+","u / u+","v / v-","n / nu-")
    scale_y_continuous(breaks = seq(-60, 60, by = 20)) +
    scale_x_continuous(breaks = seq(0, 8, by = 1)) +
    theme_classic()
  fig2
  

  # # # # # # # # # # Paper: Inset Figure 2 # # # # # # # # # #
  fig2_inset1 <- ggplot(for_plot2[for_plot2$Experiment == "Experiment 1",], 
         aes(x = Trials, y = adjRating, shape = Cue, fill = Cue)) + 
    stat_summary(geom = "line",position = position_dodge(0.1)) +
    stat_summary(fun = "mean",position = position_dodge(0.1)) +
    scale_y_continuous(breaks = c(10,60)) +
    scale_x_continuous(breaks = c(1, 8)) +
    facet_grid(. ~ Experiment) +
    scale_shape_manual(name = "Test\n Cues",
                       values = c(21,21,22,22), # c(15,22,3,4,8,11,16,15,17,18) #22,23,18,15
                       breaks = c("p","u","v","n")) +
    scale_fill_manual(name = "Test\n Cues",
                      values = c("white","black","black","white"), # c(15,22,3,4,8,11,16,15,17,18)
                      breaks = c("p","u","v","n")) + # c("p / pv+","u / u+","v / v-","n / nu-")
    theme_classic()

  fig2_inset2 <- ggplot(for_plot2[for_plot2$Experiment == "Experiment 2",], 
                        aes(x = Trials, y = adjRating, shape = Cue, fill = Cue)) + 
    stat_summary(geom = "line",position = position_dodge(0.1)) +
    stat_summary(fun = "mean",position = position_dodge(0.1)) +
    scale_y_continuous(breaks = c(10,60)) +
    scale_x_continuous(breaks = c(1, 8)) +
    facet_grid(. ~ Experiment) +
    scale_shape_manual(name = "Test\n Cues",
                       values = c(21,21,22,22), # c(15,22,3,4,8,11,16,15,17,18) #22,23,18,15
                       breaks = c("p","u","v","n")) +
    scale_fill_manual(name = "Test\n Cues",
                      values = c("white","black","black","white"), # c(15,22,3,4,8,11,16,15,17,18)
                      breaks = c("p","u","v","n")) + # c("p / pv+","u / u+","v / v-","n / nu-")
    theme_classic()

  
  # Figure 3, Phase 2
  for_plot3 <- db[db$Phase == "Phase 2",]
  
  # add adjusted rating
  for_plot3$adjRating <- ifelse(for_plot3$Cue == "CN" | for_plot3$Cue == "SN", -1, 1)
  for_plot3$adjRating <- for_plot3$Rating * for_plot3$adjRating 
  
  # # # # # # # # # # Paper Figure 3 # # # # # # # # # #
  for_plot3$Cue2 <- ifelse(for_plot3$Cue == "SP" | for_plot3$Cue == "SN",for_plot3$Cue,
                           paste0(for_plot3$Cue,substr(for_plot3$Experiment,12,12)))
  for_plot3$Cue2 <- factor(for_plot3$Cue2, levels = c("SP","CP1","CP2","CN2","CN1","SN"))
  fig3 <- ggplot(for_plot3, aes(x = Trials, y = Rating, shape = Cue2, fill = Cue2)) +  
    labs(y = "Mean Rating", x = "Blocks") +
    geom_hline(yintercept = 0, col = "gray50") +
    stat_summary(geom = "line",position = position_dodge(0.1)) +
    stat_summary(fun.data = "mean_cl_boot",position = position_dodge(0.1)) +
    facet_grid(. ~ Experiment) +
    scale_shape_manual(name = "Test\n Cues",
                       values = c(24,22,24,25,23,25),
                       breaks = c("SP","CP1","CP2","CN2","CN1","SN"),
                       labels = c("S","R","C","K","V","Z")) +
    scale_fill_manual(name = "Test\n Cues",
                      values = c("white","black","black","black","black","white"),
                      breaks = c("SP","CP1","CP2","CN2","CN1","SN"),
                      labels = c("S","R","C","K","V","Z")) +
    scale_y_continuous(breaks = seq(-60, 60, by = 20)) +
    scale_x_continuous(breaks = seq(0, 8, by = 1)) +
    theme_classic()
  fig3
  
  
  # # # # # # # # # # Paper: Inset Figure 3 # # # # # # # # # #
  fig3_inset1 <- ggplot(for_plot3[for_plot3$Experiment == "Experiment 1",], 
                        aes(x = Trials, y = adjRating, shape = Cue2, fill = Cue2)) + 
    stat_summary(geom = "line",position = position_dodge(0.1)) +
    stat_summary(fun = "mean",position = position_dodge(0.1)) +
    facet_grid(. ~ Experiment) +
    scale_shape_manual(name = "Test\n Cues",
                       values = c(24,22,24,25,23,25),
                       breaks = c("SP","CP1","CP2","CN2","CN1","SN")) +
    scale_fill_manual(name = "Test\n Cues",
                      values = c("white","black","black","black","black","white"),
                      breaks = c("SP","CP1","CP2","CN2","CN1","SN")) +
    scale_y_continuous(breaks = c(30,60)) +
    scale_x_continuous(breaks = c(1,4)) +
    theme_classic()
  
  fig3_inset2 <- ggplot(for_plot3[for_plot3$Experiment == "Experiment 2",], 
                        aes(x = Trials, y = adjRating, shape = Cue2, fill = Cue2)) + 
    stat_summary(geom = "line",position = position_dodge(0.1)) +
    stat_summary(fun = "mean",position = position_dodge(0.1)) +
    facet_grid(. ~ Experiment) +
    scale_shape_manual(name = "Test\n Cues",
                       values = c(24,22,24,25,23,25),
                       breaks = c("SP","CP1","CP2","CN2","CN1","SN")) +
    scale_fill_manual(name = "Test\n Cues",
                      values = c("white","black","black","black","black","white"),
                      breaks = c("SP","CP1","CP2","CN2","CN1","SN")) +
    scale_y_continuous(breaks = c(30,60)) +
    scale_x_continuous(breaks = c(1,4)) +
    theme_classic()
  
  return(list(fig2=fig2,fig2_inset1=fig2_inset1,fig2_inset2=fig2_inset2,
              fig3=fig3,fig3_inset1=fig3_inset1,fig3_inset2=fig3_inset2))
}

# new figures adapted from "extract_visualize_analyze_v2"
f_new_fig <- function(db3) {
  # add correct cue labeling
  db3$rat$stim <- as.factor(db3$rat$stim)
  levels(db3$rat$stim) <- c("R","C","E","I","K","V","S","Z")
  
  # create log x axis
  db3$rat$nRat[db3$rat$phaseNum == 2] <- db3$rat$nRat[db3$rat$phaseNum == 2] - 16
  db3$rat$nRat <- as.factor(db3$rat$nRat)
  labs <- levels(db3$rat$nRat)
  db3$rat$nRat <- as.integer(db3$rat$nRat)
  
  # remove pre cues in phase 2
  db3$rat <- db3$rat[!(db3$rat$phaseNum == 2 & db3$rat$ctrlType == "pre. (E & I)"),]
  
  # New facet label names for supp variable
  phaseNumLabs <- c("Phase 1", "Phase 2")
  names(phaseNumLabs) <- c("1", "2")
  
  fig4 <- ggplot2::ggplot(db3$rat,aes(x=nRat,y=rat,shape=stim,fill=stim)) + 
    labs(title="Experiment 3",y="Mean Rating",x="Blocks") +
    geom_hline(yintercept = 0, col = "gray50") +
    stat_summary(geom="line", position=position_dodge(0.1)) + 
    stat_summary(fun.data="mean_cl_boot",position=position_dodge(0.1)) + 
    scale_x_continuous(breaks = unique(db3$rat$nRat),labels = labs) +
    scale_y_continuous(breaks = seq(-100,100,20)) +
    scale_shape_manual(name = "Test\n Cues",
                       values = c(21,22,24,22,24,25,23,25),
                       breaks = c("E","I","S","R","C","K","V","Z")) +
    scale_fill_manual(name = "Test\n Cues",
                      values = c(rep("white",3),rep("black",4),"white"),
                      breaks = c("E","I","S","R","C","K","V","Z")) +
    facet_grid(. ~ phaseNum,labeller=labeller(phaseNum=phaseNumLabs),
               scales="free_x") +
    coord_cartesian(ylim = c(-70,70)) +
    theme_classic()
  fig4
  
  # remove pre cues in phase 2
  db3$rat <- db3$rat[db3$rat$phaseNum == 2,]
  fig4_inset <- ggplot2::ggplot(db3$rat,aes(x=nRat,y=rat0comp,shape=stim,fill=stim)) + 
    labs(y="Absolute Mean Rating",x="Blocks") +
    geom_hline(yintercept = 0, col = "gray50") +
    stat_summary(geom="line", position=position_dodge(0.1),size=0.4) + 
    stat_summary(fun="mean",position=position_dodge(0.1),size=0.4) + 
    scale_x_continuous(breaks = unique(db3$rat$nRat),labels = labs) +
    scale_y_continuous(breaks = c(0,60)) +
    scale_shape_manual(name = "Test\n Cues",
                       values = c(21,22,24,22,24,25,23,25),
                       breaks = c("E","I","S","R","C","K","V","Z")) +
    scale_fill_manual(name = "Test\n Cues",
                      values = c(rep("white",3),rep("black",4),"white"),
                      breaks = c("E","I","S","R","C","K","V","Z")) +
    coord_cartesian(ylim = c(0,70)) +
    theme_classic()
  fig4_inset
  
  return(list(fig4=fig4,fig4_inset=fig4_inset))
}

