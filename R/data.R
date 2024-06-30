#' Training data for the classifier
#'
#' This dataset was created using mathematical functions to create 'clean' data
#' for training the decision tree. It is available for information purposes
#' but should not be changed.
#'
#' @format A data frame with 2001 rows and 260 columns:
#'
#'   There are 13 classes each with 20 examples (columns)
#'   The timeseries outputs are all of length 2001
#'   The order of the columns match the order of classes

"dataOriginal"

#' Classes for the training data
#'
#' The classes are to be used with the training set "dataOriginal".
#'
#' @format A character vector of length 260
#' There are 13 classes each with 20 examples
#' The order of labels match the order of the columns in "dataOriginal"

"ourLabels"
