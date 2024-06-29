#' Title
#'
#' @param fileName The name of the file you want to classify
#' @param threshold The threshold wanted for small values (default .001)
#' @param ... Extra parameters for using or not using attributes for tree and hyperparameters for extra noise removal
#'
#' @return Character vector of classes
#' @export
#'
#' @examples classify("check2", threshold=.0001, "median", "deleteStart")
#'
classify <- function(fileName,  threshold=.001, ...){
  print(fileName)
  print(paste0("threshold", threshold))
  p <- list(...)
  print(p)
  # source("treeCreation.R")
  # source("chooseAttributesUpdated.R")
  # source("join.R")
  # source("F1Score.R")

  #
  # #run to create initial data for tree OR use dataOriginal.csv
  # source("dataCreation.R")
  # data <- dataCreation()
  #
  # file<- "dataOriginal"
  # #data <- read.csv(paste0("./Data/dataLabelled.csv"))
  # labels <-as.character(t(data[1,]))
  # data <- data[2:nrow(data),]
  #
  # #run once to create initial tree
  # classData <- attributes(file, .001, labels, TRUE,...) #classifierTrue
  # tree <- createTree(classData, classData$labels) #treeCreation
  # # c <- as.data.frame(apply(classData[,2:8], 2, as.numeric))
  # # c <- cbind(classData[,1], c, classData[,9])
  # # names(c) <- names(classData)
  # # tree2 <- createTree(c, c$labels)
  # prp(tree$finalModel, varlen = 0, cex=.9, type=0, extra=2)
  #
  # #run repeatedly for desired results
  # #file <- "check3"
  #
  # classDataTest <- attributes(fileName, threshold,NULL, useLabels=FALSE,...) #classifierTrue
  # a <- predict(tree, classDataTest)
  # print(a)
  # #data <- resultComparison(file, a) #join
  # df <- read.csv(paste0("./Data/",fileName,".csv"))
  #
  # ######################
  # behaviours <- list()
  # u <- unique(a)
  # print(u)
  # for(i in 1:length(u)){
  #   behaviours[[i]] <- as.data.frame(df[,which(a==u[i])])
  #
  # }
  # plots <- list()
  # names(behaviours) <- u
  # for(i in 1:length(behaviours)){
  #   d <- data.frame("t"=1:nrow(behaviours[[i]]), behaviours[[i]])
  #   d2 <- reshape2::melt(d, "t")
  #   #print(ggplot2::ggplot(d2, aes(t, value, col=variable))+geom_line()+ggtitle(names(behaviours)[[i]])+theme(legend.position="none"))
  #   plots[[length(plots)+1]] <- ggplot(d2, aes(t, value, col=variable))+geom_line()+ggtitle(names(behaviours)[[i]])+theme(legend.position="none")
  # }
  #
  # print(length(plots))
  # gridExtra::grid.arrange(grobs=plots, ncol=3)
  # #grid.arrange(plots[[1]], plots[[2]], plots[[3]],ncol=3)
  # return(a)
}

