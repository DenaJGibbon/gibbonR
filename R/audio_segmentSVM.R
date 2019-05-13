#' @title Audio segmentation using support vector machines
#' @description This function requires the user to input a training data file with calculated MFCCs.It then calculates MFCCs for a user-specified input .wav file and uses SVM to identify the "target signal" from the sound file.
#'
#' @param wav.file A R wave object or file path to .wav object; the sound file with audio signal(s) of interest
#' @param min.freq Minimum frequency of the signal of interest in Hz
#' @param max.freq Maximum frequency of the signal of interest in Hz
#' @param win.hop.time Window length (in sec); wintime and hoptime are equal so there is no overlap between successive windows
#' @param n.cep Number of cepstra to return when calculating Mel-frequency cepstral coefficients
#' @param tune Logical; whether to use parameter tuning of the SVM. If set to "TRUE" will add substantial amount of processing time with a large dataset
#' @param cost.list List of values to tune the cost parameter for SVM; If "TUNE" is set equal to "TRUE"
#' @param gamma.list List of values to tune the gamma parameter for SVM; If "TUNE" is set equal to "TRUE"
#' @param trainingdata Dataframe of MFCCs from labeled sound files. The first column is the class labels. MFCC parameters for training data must be equal to the those used for SVM.
#' @param target.signal Which class from "trainingdata" is the target signal.
#' @param prob.signal Minimum SVM probability for target signal to be considered as signal.
#' @param min.signal.dur Minimum duration for a sound event to be considered target signal.
#' @param wav.name Can be either full file path to .wav file location or R .wav object
#' @param min.signal.dur Minimum time (in seconds) of sound events
#' @param writetodir Logical. If "TRUE" will write sound events to specified output directory
#' @param output.dir File path where SVM identified sound events will be written.
#' @seealso calc_MFCC()
#' @export
#' @examples
#' @import stringr
#'
#'
#'

audio_segment_SVM <- function(wav.file, min.freq=400, max.freq=2000,
                              win.hop.time = 0.25,n.cep=12,tune=FALSE,
                              cost.list=c(0.001, 0.01, 0.1, 1, 2, 10, 100, 1000),
                              gamma.list=c(0.01, 0.1, 0.5, 1.0, 2.0),
                              cost.val = 1,
                              gamma.val= 0.5,
                              trainingdata,
                              target.signal = "gibbon",
                              prob.signal=0.8,
                              min.signal.dur=1,
                              writetodir=TRUE,
                              output.dir
                              ) {

  if(is.element(target.signal,unique(trainingdata$class))==FALSE){
    stop("Training data does not contain target signal")
  }

  # If full file path is given instead of .wav read in as .wav file
  if(class(wav.file)[1]=="Wave"){
    temp.wav <- wav.file } else {
      temp.wav <- tuneR::readWave(wav.file)
    }

  # Calculate MFCCs
  print("Calculating MFCCs")
  melfcc.output <- tuneR::melfcc(
    temp.wav,
    minfreq = min.freq,
    maxfreq = max.freq,
    wintime = win.hop.time,
    hoptime = win.hop.time,
    numcep = n.cep
  )

  print("Calculating SVM")
  if (tune=="TRUE"){
    tune.rad.segmentation <-
      e1071::tune(
        svm,
        trainingdata[, 2:ncol(trainingdata)],
        trainingdata$class,
        type="C-classification",
        kernel = 'radial',
        tunecontrol = tune.control(cross = 5),
        ranges = list(
          cost = cost.list,
          gamma = gamma.list
        )
      )
    cost <- tune.rad.segmentation$best.parameters$cost
    gamma <- tune.rad.segmentation$best.parameters$gamma
  } else {
    cost = cost.val
    gamma= gamma.val
  }
  svm.model <-
    e1071::svm(
      trainingdata[, 2:ncol(trainingdata)],
      trainingdata$class,
      kernel = "radial",
      type="C-classification",
      gamma = gamma.val,
      cost = cost.val,
      cross = 5,
      probability=TRUE
    )


  number.time.windows <- nrow(melfcc.output)
  detection.df <- data.frame()

  print("Calculating sound events")
  for(a in 1:number.time.windows){
    print(paste("processing",a))
    temp.mfcc.vector <- as.data.frame(t(melfcc.output[a,]))
    colnames(temp.mfcc.vector) <- colnames(trainingdata[, 2:ncol(trainingdata)])
    svm.prob <- predict(svm.model,temp.mfcc.vector,probability=TRUE)

    model.output <-attr(svm.prob,"probabilities")
    signal.loc <-which(attr(model.output, "dimnames")[[2]] == target.signal)

    temp.df <- cbind.data.frame(a,a*win.hop.time, target.signal,model.output[signal.loc])
    colnames(temp.df) <- c("time.window", "time (s)", "target.signal","probability")
    detection.df <- rbind.data.frame(detection.df,temp.df)
  }

  detection.df$probability[detection.df$probability < prob.signal] <- 0

  runs <- rle(detection.df$probability > 0)

  #
  signal.dur <- min.signal.dur/win.hop.time

  myruns = which(runs$values == TRUE & runs$lengths >= signal.dur)

  runs.lengths.cumsum = cumsum(runs$lengths)
  ends = runs.lengths.cumsum[myruns]

  newindex = ifelse(myruns>1, myruns-1, 0)
  starts = runs.lengths.cumsum[newindex] + 1
  if (0 %in% newindex) starts = c(1,starts)

  call.timing.df <- cbind.data.frame(starts,ends)
  call.timing.df

  sound.event.df <- data.frame()
    if(nrow(call.timing.df)==0){
      print("No sound events match specified criteria")
    } else{

    for(b in 1:nrow(call.timing.df)){

      call.time.sub <- call.timing.df[b,]
      call.time.sub <- (call.time.sub)*win.hop.time

      short.wav <- seewave::cutw(temp.wav, from=call.time.sub$starts, to=call.time.sub$ends,output = "Wave")

      print(paste("processing", b))
      if(writetodir =="TRUE"){
      tuneR::writeWave(short.wav, filename = paste(output.dir, "/",
                                                   "sound.event","_", b, "_", call.time.sub$starts, "_", call.time.sub$ends, ".wav", sep=""),extensible = F)
      }
      temp.sound.event.df <- cbind.data.frame(call.time.sub$starts,call.time.sub$ends,target.signal)
      colnames(temp.sound.event.df) <- c("time.start.sec","time.stop.sec","target.signal")
      sound.event.df <- rbind.data.frame(sound.event.df,temp.sound.event.df)
      }
  }
  return(list(svm.accuracy=svm.model$tot.accuracy,detection.df=detection.df, sound.event.df =sound.event.df ))
}
