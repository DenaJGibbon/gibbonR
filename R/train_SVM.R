#' Train and test a support-vector machine
#' @param feature.df A set of features estimated for each call where the first column designates class membership.
#' @param train.n Proportion of data used to train; defaults to 0.7
#' @param tune Logical; indicates whether to tune SVM hyperparameters
#' @param cost Cost of constraints violation
#' @param gamma Parameter for all kernels apart from "linear"
#' @param kernel Choice of "linear", "polynomial", "radial", or "sigmoid"; defaults to "radial"
#' @param cross A k-fold cross validation on the training data is performed to assess the quality of the model; defaults to 5
#' @param test.n Proportion of data used to test; defaults to 0.3
#' @export
#' @examples
#'



train_SVM <- function(feature.df, train.n=0.7, test.n=0.3, tune="TRUE", cost=c(0.001, 0.01, 0.1, 1, 2, 10, 100, 1000),
                      gamma=c(0.01, 0.1, 0.5, 1.0, 2.0), kernel="radial", cross=5) {

  feature.df$ran.num <- runif(nrow(feature.df), 0, 1)

  #Assign random number
  mfcc.train <- feature.df[feature.df$ran.num < train.n, ]
  #Drop random number
  mfcc.train$ran.num <- NULL
  #Assign random number
  mfcc.test <- feature.df[feature.df$ran.num >= test.n, ]
  #Drop random number
  mfcc.test$ran.num <- NULL

  if(tune=="TRUE"){
  tune.rad <-
    tune(
      svm,
      mfcc.train[, 2:ncol(mfcc.train)],
      mfcc.train$class,
      type="C-classification",
      kernel = kernel,
      tunecontrol = tune.control(cross = 5),
      ranges = list(
        cost = cost,
        gamma = gamma
      )
    )

  svm.model <-
    svm(
      mfcc.train[, 2:ncol(mfcc.train)],
      mfcc.train$class,
      kernel = kernel,
      type="C-classification",
      gamma = tune.rad$best.parameters$gamma,
      cost = tune.rad$best.parameters$cost,
      cross = cross
    )
  } else {
    svm.model <-
      svm(
        mfcc.train[, 2:ncol(mfcc.train)],
        type="C-classification",
        mfcc.train$class,
        kernel = kernel,
        cross = cross
      )
 }
  mod.pred <- predict(svm.model, mfcc.test[, 2:ncol(mfcc.test)])

  confusion.mat <-
    table(pred = mod.pred, true = mfcc.test$class)

  print(confusion.mat)


  print(sum(diag(prop.table(confusion.mat))))

  return(list(conf.mat=confusion.mat,correct.prob = sum(diag(prop.table(confusion.mat)))))
}

