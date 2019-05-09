#' Function to train ML algorithm, do audio segmentation and classification on multiple sound files
#'
#' @param input Either full path to directory containing .wav files or a list with file name as first element and .wav as second element
#' @param feature.df Dataframe of features from labeled sound files; first column must be class labes
#' @param model.type Choice of "NNET", "SVM", or "GMM"
#' @param feature.df Dataframe of MFCCs from labeled sound files
#' @param tune Logical; if want to use "tune" function for SVM; NOTE: for large datasets adds significant computing time
#' @param wav.name Can be either full file path to .wav file location or R .wav object
#' @param which.quant Option of "intersection", "low.quant" or "high.quant"
#' @param sound.event.dur Minimum time (in seconds)
#' @param output Either "spectro", "table" or "wav"
#' @keywords
#' @export
#' @examples
#' train_ML()
#'

batchdetect_gibbonR <- function(input, feature.df, model.type, tune=FALSE, target.signal="female.gibbon",
which.quant="intersection",min.freq=0.4, max.freq=2, n.windows=9,
density.plot=TRUE, low.quant.val=0.15, high.quant.val=0.25,num.cep=12,
min.sound.event.dur=4, max.sound.event.dur=10,output="wav", probability.thresh=0.75, wav.output="TRUE", output.dir=NULL
) {

  if((unique(feature.df$class) != target.signal)) stop("Training and target classes do not match! Check to make sure target.signal exists in training dataset")
  if((wav.output=="TRUE" & output.dir=="")) stop("Specify output directory")

  ## Neural network
  if(model.type=="NNET"){
    ml.model.nnet <- caret::train(class ~ .,
                           data= feature.df,
                           method="nnet",
                           trControl = trainControl(method = "CV", number = 10,classProbs =  TRUE)
    )
  }

  if(model.type=="SVM" ){
    if(tune==TRUE){
      ## SVM classification

      tune.rad <-
        e1071::tune(
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
        e1071::svm(
          feature.df[, 2:ncol(feature.df)],
          feature.df$class,
          kernel = "radial",
          gamma = tune.rad$best.parameters$gamma,
          cost = tune.rad$best.parameters$cost,
          cross = 5, probability=TRUE
        )

    } else {
      ml.model.svm <-
        e1071::svm(
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

    if((length(ml.model.gmm) == 0)) stop("GMM could not be trained! Try with SVM or NNET.")
  }


if(class(input)=="list"){
  list.file.input <- input
} else {
  list.file.input <- list.files(input, full.names = TRUE)
  list.file.input.short <- list.files(input, full.names = FALSE)
}

  if(length(list.file.input)==0){
    print("No sound files detected")
  } else{
timing.df <- data.frame()
  for(a in 1:length(list.file.input)){
    if(class(input)=="list"){
      temp.name <- list.file.input[[a]][[1]]
      temp.wav <- list.file.input[[a]][[2]]
    } else {
      temp.wav <- readWave(list.file.input[[a]])
      temp.name <- list.file.input.short[[a]]
    }

  temp.name <- stringr::str_split_fixed(temp.name,pattern = '.wav',n=2)[1]
  # Convert .wav file to spectrogram
  print(paste("Computing spectrogram",a))
  swift.spectro <- seewave::spectro(temp.wav,plot=F)

  # Identify the frequency band of interest
  min.freq.cols <- which.min(abs(round(swift.spectro$freq,digits=2) - min.freq))
  max.freq.cols <- which.min(abs(round(swift.spectro$freq,digits=2) - max.freq))

  # Calculate the column sums for each time window
  col.sum <- colSums(swift.spectro$amp[min.freq.cols:max.freq.cols, ])

  # Calculate a two-mixture Gaussian process model
  print(paste("Audio segmentation in process",a))
  fit <- mixtools::normalmixEM(col.sum, k=2)

  # Create a simulated distribution of the two mixture model
  N <- 1000
  combined.df <- data.frame(vals=c(rnorm(N, mean= fit$mu[1], sd=fit$sigma[1]),rnorm(N, mean= fit$mu[2], sd=fit$sigma[2])),
                            labels= rep(c("Noise", "Signal"), each=N))
  lower.limit <- min(combined.df$vals)
  upper.limit <- max(combined.df$vals)
  long.density <- density(subset(combined.df, labels=="Noise")$vals, from=lower.limit,to=upper.limit,n=N)
  not.long.density <- density(subset(combined.df, labels=="Signal")$vals, from=lower.limit,to=upper.limit,n=N)
  dens.diff <- long.density$y-not.long.density$y

  # Calculate the boundary between the signal and the noise based on the intersection, or quantiles of user input
  intersection <- long.density$x[which(diff(dens.diff > 0) != 0)][2]
  low.quant <- qnorm(low.quant.val,fit$mu[1], fit$sigma[1])
  high.quant <- qnorm(high.quant.val,fit$mu[2], fit$sigma[2])

  # Combine into a dataframe
  vals.df <- cbind.data.frame(intersection,low.quant,high.quant)

  # Find which values fall within the specified threshold
  list.sub <- which(col.sum > max(vals.df[which.quant]))

  # Find cumulative bins in which values are above threshold
  call.timing <- split(list.sub, cumsum(c(1,diff(list.sub))!=1))

  # Determine length of sound event, based on user input of desired minimum duration in seconds
  sound.event.index <- which.min(abs(swift.spectro$time-min.sound.event.dur))

  # Create a list of sound events
  call.timing.list <- as.list(call.timing[which(sapply(call.timing, length) > sound.event.index)])

  # If user indicated maximum duration create list of sound events under certain duration
  if(max.sound.event.dur != ""){
    sound.event.index.max <- which.min(abs(swift.spectro$time-max.sound.event.dur))
    call.timing.list <- call.timing.list[which(sapply(call.timing.list, length) < sound.event.index.max)]
  }

  if(length(call.timing.list)>=1){

    for (x in 1: length(call.timing.list)){
      call.time.sub <- call.timing.list[[x]]
      short.wav <- seewave::cutw(temp.wav, from=swift.spectro$time[min(call.time.sub)], to=swift.spectro$time[max(call.time.sub)],output = "Wave")

      wav.dur <- duration(short.wav)
      win.time <- wav.dur / n.windows

      # Calculate MFCCs
      melfcc.output <- tuneR::melfcc(short.wav, minfreq = min.freq*1000, hoptime = win.time, maxfreq = max.freq*1000, numcep = num.cep,
                              wintime = win.time)

      # Calculate delta cepstral coefficients
      deltas.output <- deltas(melfcc.output)

      # Ensure only same number of time windows are used for MFCC and delta coefficients
      # Also append .wav duration
      mfcc.vector <-
        c(as.vector(t(melfcc.output[1:(n.windows-1), 2:num.cep])), as.vector(t(deltas.output[1:(n.windows-1), 2:num.cep])),wav.dur)


      mfcc.vector <- as.data.frame(t(mfcc.vector))
      colnames(mfcc.vector) <- colnames(feature.df[, 2:ncol(feature.df)])

      if(model.type=="NNET" ){
        nnet.prob <- predict(ml.model.nnet,mfcc.vector, type="prob")
        signal.loc <-which(names(nnet.prob)==target.signal)
        signal.probability <- nnet.prob[signal.loc]
        temp.nnet.df <- signal.probability

        if(temp.nnet.df[1,] >= probability.thresh){
          if(wav.output=="TRUE"){
          tuneR::writeWave(short.wav, filename = paste(output.dir, "/", temp.name, "_",target.signal,"_", model.type, "_", signal.probability, "_",
                                                swift.spectro$time[min(call.time.sub)], "_",
                                                swift.spectro$time[max(call.time.sub)], ".wav", sep=""),extensible = F)
          }
          #
          temp.df <- cbind.data.frame(temp.name,x,target.signal, signal.probability, swift.spectro$time[min(call.time.sub)],swift.spectro$time[max(call.time.sub)])
          colnames(temp.df) <- c("file.name","detect.num", "signal", "probability", "start.time","end.time")
          timing.df <- rbind.data.frame(timing.df,temp.df)
        }
      }

      if(model.type=="SVM" ){
        svm.prob <- predict(ml.model.svm,mfcc.vector, probability = T)
        model.output <-attr(svm.prob,"probabilities")
        signal.loc <-which(attr(model.output, "dimnames")[[2]] == target.signal)
        signal.probability <- model.output[signal.loc]
        temp.svm.df <- cbind.data.frame(target.signal,signal.probability)

        if(temp.svm.df$signal.probability >= probability.thresh){
          if(wav.output=="TRUE"){
            tuneR::writeWave(short.wav, filename = paste(output.dir, "/", temp.name, "_",target.signal,"_", model.type, "_", signal.probability, "_",
                                                swift.spectro$time[min(call.time.sub)], "_",
                                                swift.spectro$time[max(call.time.sub)], ".wav", sep=""),extensible = F)
          }
          #
          temp.df <- cbind.data.frame(temp.name,x,target.signal, signal.probability, swift.spectro$time[min(call.time.sub)],swift.spectro$time[max(call.time.sub)])
          colnames(temp.df) <- c("file.name","detect.num", "signal", "probability", "start.time","end.time")
          timing.df <- rbind.data.frame(timing.df,temp.df)
        }

      }

      if(model.type=="GMM" ){
        gmm.mod.predict <- predict(ml.model.gmm,mfcc.vector)
        model.output <- gmm.mod.predict$z
        signal.loc <-which(attr(model.output, "dimnames")[[2]] == target.signal)
        signal.probability <- model.output[signal.loc]
        temp.gmm.df <- cbind.data.frame(target.signal,signal.probability)
        temp.gmm.df
        if(temp.gmm.df$signal.probability >= probability.thresh){
          if(wav.output=="TRUE"){
            tuneR::writeWave(short.wav, filename = paste(output.dir, "/", temp.name, "_", target.signal,"_", model.type, "_", signal.probability,"_",
                                                swift.spectro$time[min(call.time.sub)], "_",
                                                swift.spectro$time[max(call.time.sub)], ".wav", sep=""),extensible = F)
          }
          #
          temp.df <- cbind.data.frame(temp.name,x,target.signal, signal.probability, swift.spectro$time[min(call.time.sub)],swift.spectro$time[max(call.time.sub)])
          colnames(temp.df) <- c("file.name","detect.num", "signal", "probability", "start.time","end.time")
          timing.df <- rbind.data.frame(timing.df,temp.df)
        }

      }
    }
  }
  }
return(list(timing.df=timing.df))
  }
  #}
}

