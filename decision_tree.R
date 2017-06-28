# define Gini Index function

giniIndex <- function(groups, class_values, class_index) {
  # groups: a list of dataframes corresponding to the datasets in each group.
  #         the last column of each dataframe corresponds to the true class
  # class_values: the possible class values
  # class_index: the index of the column contianing the class variable
  # id_index: the index of the column containing the id variable
  
  # note the length of each entry in groups must equal the 
  # length on class_vlaues
  
  # initialize gini index
  gini <- 0
  for (class_value in class_values) {
    # let group one of the subsets of the data
    for (group in groups) {
      # calculate the number of observations in the group
      size <- nrow(group)
      if (size == 0) {
        next()
      } else {
        # calculate the number of times the same class appears in the group
        # the observed class for each observation is recorded in the 
        # last column of group
        proportion <- sum(group[, class_index] == class_value) / size
        gini <- gini + proportion * (1 - proportion)
      }
    }
  }

  
  return(gini)
}

# both groups have mismatched classes {1,0} and {1,0}
groups1 <- list(data.frame(c(1, 1), c(1, 0)),
               data.frame(c(1, 1), c(1, 0)))
# both groups have matched classes {0,0} and {1,1}
groups2 <- list(data.frame(c(1, 1), c(0, 0)),
                data.frame(c(1, 1), c(1, 1)))
class_values <- c(0, 1)
giniIndex(groups1, class_values, 2)
giniIndex(groups2, class_values, 2)



testSplit <- function(index, value, dataset) {
  # index: which entry are we considering
  # value: the split value
  # dataset: the dataset whose rows form the observations
  left <- as.data.frame(matrix(NA, ncol = ncol(dataset)))
  right <- as.data.frame(matrix(NA, ncol = ncol(dataset)))
  # right contains all rows with a value at the index above or 
  # equal to the split vlaue
  if (nrow(dataset) == 0) {
    return(list(left = left, right = right))
  }
  for (row_index in 1:nrow(dataset)) {
    
    if (dataset[row_index, index] < value) {
      left <- rbind(left, dataset[row_index, ])
    } else {
      right <- rbind(right, dataset[row_index, ])
    }
  }
  # remove the first NA row
  left <- left[-1, ]
  right <- right[-1, ]
  # remove the rownames
  rownames(left) <- NULL
  rownames(right) <- NULL
  
  return(list(left = left, right = right))
}


getSplit <- function(dataset, class_index, id_index = NULL) {
  # take the final column of the dataset to be the class_values
  class_values <- dataset[, class_index]
  # set indices
  b_index <- 999
  b_value <- 999
  b_score <- 999
  b_groups <- NA
  # how many rows contain either class or id values
  n_ignore_rows <- 1 + length(id_index)
  for (var_index in 1:(ncol(dataset) - n_ignore_rows)) {
    for (row_index in 1:nrow(dataset)) {
      # loop through all possible cut values of the dataset
      value <- dataset[row_index, var_index]
      # split the data into two groups based on the current cut value
      groups <- testSplit(var_index, value, dataset)
      # calculate the gini index
      gini <- giniIndex(groups, class_values, class_index)
      # if the gini_index is smaller than the previous gini index (b_score), 
      # then update the b_score, record the variable (b_index) and the value 
      # at which the cut occured
      if (gini < b_score) {
        b_index <- var_index # which variable does the cut refer to?
        b_value <- value # what value did we cut at?
        b_score <- gini # what is the gini score
        b_groups <- groups # record the split data
      }
    }
  }
  return(list(gini = b_score, index = b_index, value = b_value, groups = b_groups))
}






# claculate the value for the terminal node
toTerminal <- function(group, class_index) {
  # calculate the modal class
  outcomes <- group[, class_index]
  return(outcomes[which.max(table(outcomes))])
}


Split <- function(node, max_depth, min_size, depth, class_index) {
  
  left <- node$groups$left
  right <- node$groups$right
  
  node$groups <- NULL
  
  # check for a no split
  if (nrow(left) == 0 | nrow(right) == 0) {
    node$left <- toTerminal(rbind(left, right), class_index)
    node$right <- toTerminal(rbind(left, right), class_index)
    return(NULL)
  }
  # check for max depth
  if (depth >= max_depth) {
    node$left <- toTerminal(left, class_index)
    node$right <- toTerminal(right, class_index)
    return(NULL)
  }
  # process left child
  if (length(left) <= min_size) {
    node$left <- toTerminal(left, class_index)
  } else {
    node$left <- getSplit(left, class_index)
    Split(node$left, max_depth, min_size, depth + 1, class_index)
  }
  # process right child
  if (length(right) <= min_size) {
    node$right <- toTerminal(right, class_index)
  } else {
    node$right <- getSplit(right, class_index)
    Split(node$right, max_depth, min_size, depth + 1, class_index)
  }
  return(node)
}



buildTree <- function(train, max_depth, min_size, class_index) {
  root <- getSplit(train, class_index)
  Split(root, max_depth, min_size, min_size, class_index)
  return(root)
}








dataset <- t(data.frame(c(2.771244718, 1.784783929, 0),
                        c(1.728571309, 1.169761413, 0),
                        c(3.678319846, 2.81281357, 0),
                        c(3.961043357, 2.61995032, 0),
                        c(2.999208922, 2.209014212, 0),
                        c(7.497545867, 3.162953546, 1),
                        c(9.00220326, 3.339047188, 1),
                        c(7.444542326, 0.476683375, 1),
                        c(10.12493903, 3.234550982, 1),
                        c(6.642287351, 3.319983761, 1)))
rownames(dataset) <- NULL






tree <- buildTree(dataset, max_depth = 2, min_size = 1, class_index = 3)
tree
