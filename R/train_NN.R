#' Train and test a neural network for classification
#' @param feature.df A set of features estimated for each call where the first column designates class membership.
#' @param train.n Proportion of data used to train; defaults to 0.7
#' @param test.n Proportion of data used to test; defaults to 0.3
#' @export
#' @examples
#' train_ML()



trainNN <- function(feature.df, train.n=0.7, test.n=0.3, cross=5 ) {

  feature.df$ran.num <- runif(nrow(feature.df), 0, 1)

  #Assign random number
  mfcc.train <- feature.df[feature.df$ran.num < train.n, ]
  #Drop random number
  mfcc.train$ran.num <- NULL
  #Assign random number
  mfcc.test <- feature.df[feature.df$ran.num >= test.n, ]
  #Drop random number
  mfcc.test$ran.num <- NULL


  nnet.output <- train(class ~ .,
                       data= mfcc.train,
                       method="avNNet",
                       trControl = trainControl(method = "CV", number = cross)
  )


  nnet.predict <- predict(nnet.output, mfcc.test[,2:ncol(mfcc.test)])

  nnet.predict.table <- table(nnet.predict, mfcc.test$class)

  # total percent correct
  sum(diag(prop.table(nnet.predict.table)))

  return(list(conf.mat=nnet.predict.table,correct.prob = sum(diag(prop.table(nnet.predict.table)))))
}
