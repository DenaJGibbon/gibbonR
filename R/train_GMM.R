#' Train and test a Gaussian mixture model for classification
#' @param feature.df A set of features estimated for each call where the first column designates class membership.
#' @param train.n Proportion of data used to train; defaults to 0.7
#' @param test.n Proportion of data used to test; defaults to 0.3
#' @export
#' @examples
#'


train_GMM <- function(feature.df, train.n=0.7, test.n=0.3 ) {

  feature.df$ran.num <- runif(nrow(feature.df), 0, 1)

  #Assign random number
  mfcc.train <- feature.df[feature.df$ran.num < train.n, ]
  #Drop random number
  mfcc.train$ran.num <- NULL
  #Assign random number
  mfcc.test <- feature.df[feature.df$ran.num >= test.n, ]
  #Drop random number
  mfcc.test$ran.num <- NULL


  clust.model <- MclustDA(mfcc.train[, 2:ncol(mfcc.train)],
                          class = mfcc.train$class, modelType = "EDDA")

  gmm.mod.predict <- predict(clust.model,mfcc.test[, 2:ncol(mfcc.test)])

  predictions <- cbind.data.frame(gmm.mod.predict$classification, mfcc.test$class)
  colnames(predictions) <- c("model.pred","classification")


  # # total percent correct

  print("Percent correct classification")
  print(sum(diag(prop.table(table(predictions)))))

  return(list(conf.mat=table(predictions),correct.prob = sum(diag(prop.table(table(predictions))))))
}
