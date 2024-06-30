#' Title
#'
#' @param fileName The name of the file you want to classify
#' @param threshold The threshold wanted for small values (default .001)
#' @param ... Extra parameters for using or not using attributes for tree and hyperparameters for extra noise removal
#'
#' @return Character vector of classes
#' @export
#'
#' @examples classify("C:/users/techstaff/desktop/packagetest/supplementary data/data/check2", threshold=.0001, "median", "deleteStart")
#'
classify <- function(fileName,  threshold=.001, ...){

  #run to create initial data for tree OR use dataOriginal.csv

  # data <- dataCreation()
  #
  # file<- "dataOriginal"
  #
  # labels <-as.character(t(data[1,]))
  # data <- data[2:nrow(data),]

  #run once to create initial tree
  print("creating attributes for training data")
  classData <- attributes(dataOriginal, .001, ourLabels, TRUE,...) #classifierTrue
print("creating tree")
  tree <- createTree(classData, classData$classes) #treeCreation
  rpart.plot::prp(tree$finalModel, varlen = 0, cex=.9, type=0, extra=2)

  #run repeatedly for desired data
print("creating attributes for testing data")
  classDataTest <- attributes(fileName, threshold,NULL, useLabels=FALSE,...) #classifierTrue
  a <- stats::predict(tree, classDataTest)
  #data <- resultComparison(file, a) #join
  df <- utils::read.csv(paste0(fileName,".csv"))

  ######################
  behaviours <- list()
  u <- unique(a)
  for(i in 1:length(u)){
    behaviours[[i]] <- as.data.frame(df[,which(a==u[i])])

  }
  plots <- list()
  names(behaviours) <- u
  for(i in 1:length(behaviours)){
    d <- data.frame("t"=1:nrow(behaviours[[i]]), behaviours[[i]])
    d2 <- reshape2::melt(d, "t")

    plots[[length(plots)+1]] <- ggplot2::ggplot(d2, aes(t, value, col=variable))+geom_line()+ggtitle(names(behaviours)[[i]])+theme(legend.position="none")
  }

  print(paste0("creating plots of behaviours. You have ", length(plots), " behaviours"))
  gridExtra::grid.arrange(grobs=plots, ncol=3)

  return(a)
}

