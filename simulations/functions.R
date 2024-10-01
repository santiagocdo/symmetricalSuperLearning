rep_row <- function(x,n){matrix(rep(x,each=n),nrow=n)}


f_prepData <- function (csv) {
  # number of blocks per phase
  nPhaseBlock <- csv$nPhaseBlock[!is.na(csv$nPhaseBlock)]
  # this is statement is a warning lock
  if (length(nPhaseBlock) != length(unique(csv$phase))) {
    warning("Missmatch between number of phases and number of nBlock values.")
  }
  
  # this for loop create a list where each space is a different phase, within 
  # each phase contains the training and test information: nBlocks, input, test,
  # output, trialType, trialTypeFreq, and trialTest
  trainPhases <- list()
  for (ph in 1:length(unique(csv$phase))) {
    temp <- csv[csv$phase == unique(csv$phase)[ph],]
    temp1 <- as.matrix(temp[temp$phaseType=="training",grepl("in",colnames(temp))])
    temp2 <- as.matrix(temp[temp$phaseType=="training",grepl("out",colnames(temp))])
    temp3 <- temp$trialType[temp$phaseType=="training"]
    trialTypeFreq <- temp$trialFrequency[temp$phaseType=="training"]
    trainPhases[[ph]] <- list(nBlock=csv$nPhaseBlock[ph],
                              inputBlock=NA,outputBlock=NA,trialTypeBlock=NA,
                              test=as.matrix(temp[temp$phaseType =="test",
                                                  grepl("in",colnames(temp))]),
                              trialTest=temp$trialType[temp$phaseType=="test"])
    
    # this for loop repeat trialTypeFreq[i] times the trial types in the 
    # matrices and vector: inputBlock, outputBlock, and trialTypeBlock
    for (i in 1:length(trialTypeFreq)) {
      temp1.0 <- rep_row(temp1[i,],trialTypeFreq[i])
      temp2.0 <- rep_row(temp2[i,],trialTypeFreq[i])
      temp3.0 <- rep(temp3[i],trialTypeFreq[i])
      if (i == 1) {
        trainPhases[[ph]]$inputBlock <- temp1.0
        trainPhases[[ph]]$outputBlock <- temp2.0
        trainPhases[[ph]]$trialTypeBlock <- temp3.0
      } else {
        trainPhases[[ph]]$inputBlock <- rbind(trainPhases[[ph]]$inputBlock,temp1.0)
        trainPhases[[ph]]$outputBlock <- rbind(trainPhases[[ph]]$outputBlock,temp2.0)
        trainPhases[[ph]]$trialTypeBlock <- c(trainPhases[[ph]]$trialTypeBlock,temp3.0)
      }
    } # end i loop
    colnames(trainPhases[[ph]]$inputBlock) <- colnames(temp[grepl("in",colnames(temp))])
    colnames(trainPhases[[ph]]$outputBlock) <- colnames(temp[grepl("out",colnames(temp))])
    # this for loop randomized the training blocks matrices and create a whole
    # training matrices: input, output, and trialType 
    for (i in 1:nPhaseBlock[ph]) {
      rand <- sample(1:nrow(trainPhases[[ph]]$inputBlock))
      if (i == 1) {
        temp.x <- as.matrix(trainPhases[[ph]]$inputBlock[rand,])
        temp.out <- as.matrix(trainPhases[[ph]]$outputBlock[rand,])
        temp.trialType <- trainPhases[[ph]]$trialTypeBlock[rand]
      } else {
        temp.x <- rbind(temp.x,as.matrix(trainPhases[[ph]]$inputBlock[rand,]))
        temp.out <- rbind(temp.out,as.matrix((trainPhases[[ph]]$outputBlock[rand,])))
        temp.trialType <- c(temp.trialType,trainPhases[[ph]]$trialTypeBlock[rand])
      }
    } # end block cycle
    
    # combine sequentially phases
    if (ph == 1) {
      x <- temp.x
      out <- temp.out
      trialType <- data.frame(ph,blocks=rep(1:nPhaseBlock[ph],each=sum(trialTypeFreq)),
                              trials=1:length(temp.trialType),
                              trialType=temp.trialType)
    } else {
      x <- rbind(x,temp.x)
      out <- rbind(out,temp.out)
      trialType <- rbind(trialType,data.frame(ph,blocks=rep(1:nPhaseBlock[ph],each=sum(trialTypeFreq)),
                                              trials=1:length(temp.trialType),
                                              trialType=temp.trialType))
    }
    colnames(x) <- colnames(temp[grepl("in",colnames(temp))])
    colnames(out) <- colnames(temp[grepl("out",colnames(temp))])
    
  } # end ph loop
  return(list(trainPhases=trainPhases,x=x,out=out,trialType=trialType))
}


#### model functions ####

# rescorla wagner model
f_RW_mod <- function (param,train) {
  # parameters
  a <- param$LR
  nO <- param$nO
  # training
  x <- train$x
  if (nO > ncol(train$out)) warning("specify the adecuate outcomes")
  out <- train$out[,nO]
  trialType <- train$trialType
  
  #### create matrices and scalars ####
  nStim <- ncol(x)
  nTrials <- nrow(x)
  sumV <- PE <- matrix(0,nrow=nTrials, ncol=1)
  V <- matrix(0,nrow=nTrials, ncol=nStim)
  
  #### rescorla-wagner learning rule ###
  for (t in 1:nTrials) {
    
    # sum term
    sumV[t] <- t(V[t,]) %*% x[t,]
    
    # prediction error
    PE[t] <- out[t] - sumV[t]
    
    if (t < nTrials) { 
      # update weights
      V[t+1,] <- V[t,] + (a * PE[t])*x[t,]
    }
  } # end t loop
  return(list(V=V,PE=PE,sumV=sumV,trialType=trialType))
}