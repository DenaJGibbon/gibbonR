#' Run linear discriminant function analysis on a text file
#'
#' The first column must be the class labels, and the rest are the features.
#' @param feature.df A set of features estimated for each call where the first column designates class membership
#' @param train.n Proportion of data used to train if CV=FALSE; defaults to 0.7
#' @param test.n Proportion of data used to test if CV=FALSE; defaults to 0.3
#' @param set.seed Starting point for random number generator; defaults to 1
#' @param CV Logical, determines whether the LDA should be run on test and training data set, or done using leave-one-out cross-validation
#' @keywords
#' @export
#' @examples



trainLDA <- function(feature.df, train.n=0.7, test.n=0.3, set.seed=1, CV) {

  # Set seed for reproducibility
  set.seed(set.seed)

  # Isolate training and test data
  feature.df$ran.num <- runif(nrow(feature.df), 0, 1)

  #Assign random number
  mfcc.train <- feature.df[feature.df$ran.num < train.n, ]
  #Drop random number
  mfcc.train$ran.num <- NULL
  #Assign random number
  mfcc.test <- feature.df[feature.df$ran.num >= test.n, ]
  #Drop random number
  mfcc.test$ran.num <- NULL

  ## Linear discriminant function analysis
  if(CV=="FALSE"){

    lda.output <- MASS::lda(class  ~ ., data= mfcc.train, CV=CV)

    lda.predict <- predict(lda.output,mfcc.test[,2:ncol(mfcc.test)])

    lda.predict.table <- table(lda.predict$class,mfcc.test$class)

    # total percent correct
    print(sum(diag(prop.table(lda.predict.table))))

    return(list(conf.mat=lda.predict.table,correct.prob = sum(diag(prop.table(lda.predict.table)))))

  }

  if(CV=="TRUE"){

    ## Set number of females for prior
    n.class <- length(unique(feature.df$class))

    ###Run LDA on the data
    lda.output <- MASS::lda(
      class  ~ .,
      data=feature.df,
      center = TRUE,
      prior = rep(1 / n.class, n.class),
      scale. = TRUE,
      #grouping = feature.df$class,
      CV = T
    )

    ####Assess how well the leave one out cross validation did
    ct <-
      table(grouping = feature.df$class, lda.output$class)

    ####Calculate total percent correct
    percent <- sum(diag(prop.table(ct)))
    print("Percent correct classification")
    print(percent)
    return(list(conf.mat=ct,correct.prob = sum(diag(prop.table(ct)))))

  }
}
