#' Function to train ML algorithm and classify sound events from a directory
#'
#'
#' @param feature.df Dataframe of training dataset; first column must be class labes
#' @param model.type Choice of "NNET", "SVM", or "GMM"
#' @param tune Logical; if want to use "tune" function for SVM NOTE: for large datasets adds significant computing time
#' @param wav.name Can be either full file path to .wav file location or R .wav object
#' @param output Either "spectro", "table" or "wav"
#' @keywords
#' @import e1071
#' @import mclust
#' @import caret
#' @import tuneR
#' @import signal
#' @export
#' @examples
#'


classifyGibbonR <- function(feature.df, model.type, tune=FALSE,input.dir, plot=TRUE,
                          min.freq=400, max.freq=2000, n.window=9,
                           n.cep=12,
                           min.sound.event.dur=4, max.sound.event.dur=10,output="wav", probability.thresh=0.75,output.dir
) {

  print("machine learning in progress...")
  ## Neural network
  if(model.type=="NNET"){
    ml.model.nnet <- caret::train(class ~ .,
                           data= feature.df,
                           method="nnet",
                           trControl = trainControl(method = "CV",number=5)
    )
  }

  if(model.type=="SVM" ){
    if(tune==TRUE){
      ## SVM classification

      tune.rad <-
        tune(
          svm,
          feature.df[, 2:ncol(feature.df)],
          feature.df$class,
          kernel = "radial",
          tunecontrol = tune.control(cross = 5),
          ranges = list(
            cost = c(0.001, 0.01, 0.1, 1, 2, 10, 100, 1000),
            gamma = c(0.01, 0.1, 0.5, 1.0, 2.0)
          )
        )


      ml.model.svm <-
        svm(
          feature.df[, 2:ncol(feature.df)],
          feature.df$class,
          kernel = "radial",
          gamma = tune.rad$best.parameters$gamma,
          cost = tune.rad$best.parameters$cost,
          cross = 5, probability=TRUE
        )

    } else {
      ml.model.svm <-
        svm(
          feature.df[, 2:ncol(feature.df)],
          feature.df$class,
          kernel = "radial",
          gamma= 0.01,
          cost = 2,
          cross = 5, probability=TRUE
        )
    }
  }


  if(model.type=="GMM"){

    tryCatch({
      ml.model.gmm <- mclust::MclustDA(feature.df[, 2:ncol(feature.df)],
                                       class = feature.df$class, modelType = "EDDA")
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

    if((length(ml.model.gmm) == "NA")) stop("GMM could not be trained! Try with SVM or NNET.")
  }

  input.dir.list.files <- list.files(input.dir,pattern = c(".wav",".WAV"),full.names = TRUE)
  input.dir.list.files.short <- list.files(input.dir,pattern = c(".wav",".WAV"),full.names = FALSE)

  mfcc.vector.list = list()
  file.name.list = list()
  ####Loop to calculate MFCC for each .wav file in the directory
  for (j in 1:length(input.dir.list.files)) {
    print(paste("processing",j))
    wav.name <- input.dir.list.files.short[[j]]
    wav.file <- input.dir.list.files[[j]]
    wav.file <- tuneR::readWave(wav.file)

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

  colnames(mfcc.data.frame) <- c("class",colnames(feature.df[, 2:ncol(feature.df)]))

  model.out.df <- data.frame()
  if(model.type=="NNET" ){
    for(x in 1:nrow(mfcc.data.frame)){
      print(paste("processing",x))
      mfcc.vector <- mfcc.data.frame[x,2:ncol(mfcc.data.frame)]
      nnet.prob <- predict(ml.model.nnet,mfcc.vector, type="prob")
      model.pred <- nnet.prob[which.max(nnet.prob)]
      model.pred.val <-colnames(model.pred)
      model.prob <- model.pred[1,]
      temp.df <- cbind.data.frame(mfcc.data.frame[x,1],model.pred.val,model.prob)
      colnames(temp.df) <- c("file.name","model.prediction","probability")
      model.out.df <- rbind.data.frame(model.out.df,temp.df)
    }
  }

  if(model.type=="SVM" ){
  for(x in 1:nrow(mfcc.data.frame)){
    print(paste("processing",x))
    mfcc.vector <- mfcc.data.frame[x,2:ncol(mfcc.data.frame)]
    svm.prob <- predict(ml.model.svm,mfcc.vector, probability = T)
    model.output <-as.data.frame(attr(svm.prob,"probabilities"))
    model.pred <- model.output[which.max(model.output)]
    model.pred.val <-colnames(model.pred)
    model.prob <- model.pred[1,]
    temp.df <- cbind.data.frame(mfcc.data.frame[x,1],model.pred.val,model.prob)
    colnames(temp.df) <- c("file.name","model.prediction","probability")
    model.out.df <- rbind.data.frame(model.out.df,temp.df)
  }
  }

  if(model.type=="GMM" ){
    for(x in 1:nrow(mfcc.data.frame)){
      print(paste("processing",x))
      mfcc.vector <- mfcc.data.frame[x,2:ncol(mfcc.data.frame)]
      gmm.prob <- predict(ml.model.gmm,mfcc.vector, probability = T)
      model.pred.val <- gmm.prob$classification
      model.output <- gmm.prob$z
      signal.loc <-which.max(model.output)
      model.prob <- model.output[signal.loc]
      temp.df <- cbind.data.frame(mfcc.data.frame[x,1],model.pred.val,model.prob)
      colnames(temp.df) <- c("file.name","model.prediction","probability")
      model.out.df <- rbind.data.frame(model.out.df,temp.df)
    }
  }


  if(plot=="TRUE"){

    for(a in 1:length(input.dir.list.files)){
      print(paste("plotting",a))
      temp.wav <- tuneR::readWave(input.dir.list.files[a])
      temp.spec <- signal::specgram(temp.wav@left, Fs=temp.wav@samp.rate, n=512, overlap = 0)
      modelprediction <- as.character(model.out.df[a,]$model.prediction)
      main.title <- paste(modelprediction,a)
      plot(temp.spec,xlab="Time (s)", ylab="Frequency (Hz)",col=viridis::viridis(512),main=main.title,useRaster = TRUE)
    }
  }
  return(list(classification.df=model.out.df))
}
