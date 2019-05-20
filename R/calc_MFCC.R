#' Calculate MFCCs from .wav files
#' @param list.wav.files A list of the .wav files
#' @param n.window  The number of windows that will divide each .wav file.
#' @param n.cep Number of cepstra to return
#' @param min.freq Minimum frequency (in Hz).
#' @param max.freq Maximum frequency (in Hz)
#' @param feature.red Whether to use SVM RFE to reduce number of features
#' @return
#' @seealso \code{\link{tuneR}} which this function wraps
#' @import stringr
#' @examples
#' @export
#'

## Define for function


calcMFCC <- function(list.wav.files, n.window,n.cep=12,min.freq=400, max.freq=2000,
                      win.avg="TRUE", win.hop.time=0.25, feature.red = "FALSE") {

  if(win.avg=="FALSE"){

    mfcc.data.frame <- data.frame()
    ####Loop to calculate MFCC for each .wav file in the directory
    for (j in 1:length(list.wav.files)) {
      print(paste("processing",j))
      wav.name <- str_split_fixed(list.wav.files[[j]][[1]], pattern = '_', n=2)[1]
      wav.file <- list.wav.files[[j]][[2]]


      # Calculate MFCCs
      melfcc.output <- tuneR::melfcc(
        wav.file,
        minfreq = min.freq,
        maxfreq = max.freq,
        wintime = win.hop.time,
        hoptime = win.hop.time,
        numcep = n.cep
      )

      melfcc.output <- as.data.frame(melfcc.output)
      class <- rep(wav.name, nrow(melfcc.output))
      melfcc.output <- cbind.data.frame(class,melfcc.output)

      mfcc.data.frame <- rbind.data.frame(mfcc.data.frame,melfcc.output)
    }

    return(mfcc.data.frame=mfcc.data.frame)
  }

  if(win.avg=="TRUE"){
  ####Create empty list to hold MFCC values
  mfcc.vector.list = list()
  file.name.list = list()
  ####Loop to calculate MFCC for each .wav file in the directory
  for (j in 1:length(list.wav.files)) {
    print(paste("processing",j))
    wav.name <- str_split_fixed(list.wav.files[[j]][[1]], pattern = '_', n=2)[1]
    wav.file <- list.wav.files[[j]][[2]]

    # Find duration of .wav file and divide into windows
    wav.dur <- seewave::duration(wav.file)
    win.time <- wav.dur / n.window

    # Calculate MFCCs
    melfcc.output <- tuneR::melfcc(
      wav.file,
      minfreq = min.freq,
      hoptime = win.time,
      maxfreq = max.freq,
      numcep = n.cep,
      wintime = win.time
    )

    # Calculate delta cepstral coefficients
    deltas.output <- tuneR::deltas(melfcc.output)

    # Ensure only 8 time windows are used for MFCC and delta coefficients
    # Also append .wav duration
    mfcc.vector <-
      c(as.vector(t(melfcc.output[1:(n.window-1), 2:n.cep])), as.vector(t(deltas.output[1:(n.window-1), 2:n.cep])),wav.dur)

    #Add MFCC values to list
    mfcc.vector.list[[j]] <- mfcc.vector
    file.name.list[[j]] <- wav.name
  }


  ####Check structure of MFCC list; there should be a vector for each call
  str(mfcc.vector.list)

  mfcc.vector.list.df <- as.data.frame(as.matrix(unlist(mfcc.vector.list), nrow=length(unlist(file.name.list)), ncol= length(mfcc.vector), byrow=T))

  data.matrix.all <-
    matrix(
      unlist(mfcc.vector.list),
      nrow = length(unlist(file.name.list)),
      ncol = length(mfcc.vector.list[[1]]),
      byrow = T
    )


  class <- unlist(file.name.list)

  mfcc.data.frame <-
    cbind.data.frame(class, data.matrix.all)

  if(feature.red=="TRUE"){

    # Copyright (C) 2011  John Colby
    # http://github.com/johncolby/SVM-RFE

    svmRFE.wrap <- function(test.fold, X, ...) {
      # Wrapper to run svmRFE function while omitting a given test fold
      train.data = X[-test.fold, ]
      test.data  = X[test.fold, ]

      # Rank the features
      features.ranked = svmRFE(train.data, ...)

      return(list(feature.ids=features.ranked, train.data.ids=row.names(train.data), test.data.ids=row.names(test.data)))
    }

    svmRFE <- function(X, k=1, halve.above=5000) {
      # Feature selection with Multiple SVM Recursive Feature Elimination (RFE) algorithm
      n = ncol(X) - 1

      # Scale data up front so it doesn't have to be redone each pass
      cat('Scaling data...')
      X[, -1] = scale(X[, -1])
      cat('Done!\n')
      flush.console()

      pb = txtProgressBar(1, n, 1, style=3)

      i.surviving = 1:n
      i.ranked    = n
      ranked.list = vector(length=n)

      # Recurse through all the features
      while(length(i.surviving) > 0) {
        if(k > 1) {
          # Subsample to obtain multiple weights vectors (i.e. mSVM-RFE)
          folds = rep(1:k, len=nrow(X))[sample(nrow(X))]
          folds = lapply(1:k, function(x) which(folds == x))

          # Obtain weights for each training set
          w = lapply(folds, getWeights, X[, c(1, 1+i.surviving)])
          w = do.call(rbind, w)

          # Normalize each weights vector
          w = t(apply(w, 1, function(x) x / sqrt(sum(x^2))))

          # Compute ranking criteria
          v    = w * w
          vbar = apply(v, 2, mean)
          vsd  = apply(v, 2, sd)
          c    = vbar / vsd
        } else {
          # Only do 1 pass (i.e. regular SVM-RFE)
          w = getWeights(NULL, X[, c(1, 1+i.surviving)])
          c = w * w
        }

        # Rank the features
        ranking = sort(c, index.return=T)$ix
        if(length(i.surviving) == 1) {
          ranking = 1
        }

        if(length(i.surviving) > halve.above) {
          # Cut features in half until less than halve.above
          nfeat = length(i.surviving)
          ncut  = round(nfeat / 2)
          n     = nfeat - ncut

          cat('Features halved from', nfeat, 'to', n, '\n')
          flush.console()

          pb = txtProgressBar(1, n, 1, style=3)

        } else ncut = 1

        # Update feature list
        ranked.list[i.ranked:(i.ranked-ncut+1)] = i.surviving[ranking[1:ncut]]
        i.ranked    = i.ranked - ncut
        i.surviving = i.surviving[-ranking[1:ncut]]

        setTxtProgressBar(pb, n-length(i.surviving))
        flush.console()
      }

      close(pb)

      return (ranked.list)
    }

    getWeights <- function(test.fold, X) {
      # Fit a linear SVM model and obtain feature weights
      train.data = X
      if(!is.null(test.fold)) train.data = X[-test.fold, ]

      svmModel = e1071::svm(train.data[, -1], train.data[, 1], cost=10, cachesize=500,
                     scale=F, type="C-classification", kernel="linear")

      t(svmModel$coefs) %*% svmModel$SV
    }

    WriteFeatures <- function(results, input, save=T, file='features_ranked.txt') {
      # Compile feature rankings across multiple folds
      featureID = sort(apply(sapply(results, function(x) sort(x$feature, index.return=T)$ix), 1, mean), index=T)$ix
      avg.rank  = sort(apply(sapply(results, function(x) sort(x$feature, index.return=T)$ix), 1, mean), index=T)$x
      feature.name = colnames(input[, -1])[featureID]
      features.ranked = data.frame(FeatureName=feature.name, FeatureID=featureID, AvgRank=avg.rank)
      if(save==T) {
        write.table(features.ranked, file=file, quote=F, row.names=F)
      } else {
        features.ranked
      }
    }

    FeatSweep.wrap <- function(i, results, input) {
      # Wrapper to estimate generalization error across all hold-out folds, for a given number of top features
      svm.list = lapply(results, function(x) e1071::tune(svm,
                                                  train.x      = input[x$train.data.ids, 1+x$feature.ids[1:i]],
                                                  train.y      = input[x$train.data.ids, 1],
                                                  validation.x = input[x$test.data.ids, 1+x$feature.ids[1:i]],
                                                  validation.y = input[x$test.data.ids, 1],
                                                  # Optimize SVM hyperparamters
                                                  ranges       = e1071::tune(svm,
                                                                      train.x = input[x$train.data.ids, 1+x$feature.ids[1:i]],
                                                                      train.y = input[x$train.data.ids, 1],
                                                                      ranges  = list(gamma=2^(-12:0), cost=2^(-6:6)))$best.par,
                                                  tunecontrol  = tune.control(sampling='fix'))$perf)

      error = mean(sapply(svm.list, function(x) x$error))
      return(list(svm.list=svm.list, error=error))
    }

    PlotErrors <- function(errors, errors2=NULL, no.info=0.5, ylim=range(c(errors, errors2), na.rm=T), xlab='Number of Features',  ylab='10x CV Error') {
      # Makes a plot of average generalization error vs. number of top features
      AddLine <- function(x, col='black') {
        lines(which(!is.na(errors)), na.omit(x), col=col)
        points(which.min(x), min(x, na.rm=T), col='red')
        text(which.min(x), min(x, na.rm=T), paste(which.min(x), '-', format(min(x, na.rm=T), dig=3)), pos=4, col='red', cex=0.75)
      }

      plot(errors, type='n', ylim=ylim, xlab=xlab, ylab=ylab)
      AddLine(errors)
      if(!is.null(errors2)) AddLine(errors2, 'gray30')
      abline(h=no.info, lty=3)
    }

    ##### Read in MFCC data with standardized number of time window
    for.svm.rfe <- mfcc.data.frame


    ##### We want k=10 for the k-fold cross validation as the “multiple” part of mSVM-RFE.
    svm.rfe.output <- svmRFE(for.svm.rfe, k = 10, halve.above = 100)
    str(svm.rfe.output)

    ##### Reorder the data so highest ranked feature is first
    new.svm.rfe <-
      for.svm.rfe[, 2:ncol(for.svm.rfe)][, dput(svm.rfe.output)]
    str(new.svm.rfe)

    ##### Create a list to store cross-validation accuracies
    accuracy.list <- list()

    ##### Set prior for LDA so that class membership is equally likely
    n.class <- length(unique(for.svm.rfe$class))

    ##### Loop to add features one by one and calculate accuracy using leave-one-out cross-validation
    for (j in 2:length(svm.rfe.output)) {
      svm.rfe.for.lda <- new.svm.rfe[1:j]

      fit.svm.rfe <- MASS::lda(
        svm.rfe.for.lda,
        center = TRUE,
        prior = rep(1 / n.class, n.class),

        scale. = TRUE,
        grouping = mfcc.data.frame$class,
        CV = T
      )

      ##### Assess how well the leave one out cross validation did
      ct <- table(grouping = for.svm.rfe$class, fit.svm.rfe$class)

      ##### total percent correct
      percent <- sum(diag(prop.table(ct)))
      accuracy.list[[j]] <- percent
    }

    #if(features.max==TRUE){
    ##### Find which number of features provides the maximum classification accuracy
    max.feature <- which.max(unlist(accuracy.list)) + 1

    ##### Subset the highest ranked variables which yield the highest accuracy
    svm.rfe.for.lda <- new.svm.rfe[1:max.feature]

    ##### Combine class labels with new subset of features into a data frame for analysis
    svm.rfe.for.classification <-
      cbind.data.frame(mfcc.data.frame$class, svm.rfe.for.lda)
    colnames(svm.rfe.for.classification)[1] <- "class"

    mfcc.data.frame <- svm.rfe.for.classification
    #return(mfcc.data.frame)
    #}

    # if(features.max==FALSE){
    # svm.rfe.for.lda <- new.svm.rfe[1:nrow(new.svm.rfe)]
    #
    # ##### Combine class labels with new subset of features into a data frame for analysis
    # svm.rfe.for.classification <-
    #   cbind.data.frame(mfcc.data.frame$class, svm.rfe.for.lda)
    # colnames(svm.rfe.for.classification)[1] <- "class"
    #
    # mfcc.data.frame <- svm.rfe.for.classification
    # return(mfcc.data.frame)
    # }

  }
  return(mfcc.data.frame=mfcc.data.frame)
}

}


