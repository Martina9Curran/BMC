set.seed(565)

createTree <- function(classData, classes){
print("training tree")
  caret.control <- caret::trainControl(method = "repeatedcv", number=15, repeats=10)
  rpart.cv <- caret::train(classes ~ .,
                    data = classData,
                    method = "rpart",
                    trControl = caret.control,
                    tuneLength =15)

  #prp(tree$finalModel, varlen = 0, cex=.5, type=4)

  return(rpart.cv)
}
