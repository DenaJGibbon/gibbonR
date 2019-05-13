#' @title Create multiple spectrograms to inspect sound events
#' @description
#' @export
#'
plotSoundevents <- function(input.dir, nrow=3, ncol=2, from=1, n.soundevents=length(list.wav.files),
                             return.table=TRUE) {

list.wav.files <- list.files(input.dir, pattern = c(".wav",".WAV"),full.names = TRUE)
list.wav.files.short <- list.files(input.dir, pattern = c(".wav",".WAV"),full.names = FALSE)

par(mfrow=c(nrow,ncol))

spectro.df <- list()
for(a in from:n.soundevents){
  print(paste("processing",a))
  temp.wav <- tuneR::readWave(list.wav.files[a])
  temp.spec <- signal::specgram(temp.wav@left, Fs=temp.wav@samp.rate, n=512, overlap = 0)
  plot(temp.spec,xlab="Time (s)", ylab="Frequency (Hz)",col=viridis::viridis(512),main=paste("sound event",a),useRaster = TRUE)
  sound.event <- paste("sound event",a)
  temp.df <- cbind.data.frame(sound.event,list.wav.files.short[a])
  colnames(temp.df) <- c("sound.event","file.name")
  spectro.df <- rbind.data.frame(spectro.df, temp.df)
  }

if(return.table=="TRUE")
  return(list(sound.event.table=spectro.df))
}

