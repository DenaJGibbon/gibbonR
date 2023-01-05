#' DetectBLED
#' @description Function to do band-limited energy summation to find sound events. This function only identifies sound events based on frequency and duration so is not expected to have high precision.
#' @param input Either full path to directory containing .wav files or a list with file name as first element and .wav as second element
#' @param input.type Either 'directory', 'list' or 'wav'
#' @param min.freq Minimum frequency (Hz) of signal of interest
#' @param max.freq Maximum frequency (Hz) of signal of interest
#' @param pattern.split Pattern to find and remove to create file name; currently set to ".rda"
#' @param output Either 'spectro', 'table' or 'wav'
#' @param noise.quantile.val A quantile value between 0 to 1 for the band energy summation
#' @param spectrogram.window Window length for spectrogram analysis (input to spectro fuction from 'seewave')
#' @param subsample.dur Duration (s) to divide longer sound file to increase processing efficiency
#' @param training.label Label to append to saved .wav files
#' @param min.signal.dur The minimum duration (s) sound events must be to be considered sound events
#' @param max.sound.event.dur The maximum duration (s) sound events must be to be considered sound events
#' @param wav.output Logical; output wave file of sound events?
#' @param swift.time If file name is in structure recorder_YYYYMMDD_HHMMSS can subset files based on specific times
#' @param time.start Time recordings start (hour)
#' @param time.stop Time recordings stop (hour)
#' @param write.table.output Logical; write Raven selection tables to output directory
#' @param verbose Logical; print out steps
#' @param random.sample If a random subset of files in a directory are desired specify a value, otherwise 'NA'
#' @param output.dir Specified output directory; set to current working directory
#' @export
#' @import e1071
#' @import tuneR
#' @import seewave
#' @import tuneR
#' @import stringr
#' @examples

DetectBLED <- function(input,input.type ='wav',
                       min.freq = 200,
                       max.freq = 6000,
                       noise.quantile.val = 0.75,
                       spectrogram.window = 1600,
                       subsample.dur = 300,
                       training.label = 'noise',
                       pattern.split = ".wav",
                       min.signal.dur = 1,
                       max.sound.event.dur = 6,
                       wav.output = "TRUE",
                       output.dir = getwd(),
                       swift.time = TRUE,
                       time.start = 18,
                       time.stop = 23,
                       write.table.output = TRUE,
                       verbose = TRUE,
                       random.sample = 100) {
  if (wav.output == "TRUE" & output.dir == "") {
    stop("Specify output directory")
  }

  if (input.type == 'list') {
    list.file.input <- unlist(input)
    nslash <- str_count(input, pattern = '/') + 1
    list.file.input.short <-
      str_split_fixed(input, pattern = '/', nslash)[, nslash]
  }

  if (input.type == "directory") {
    list.file.input <-
      list.files(input, full.names = TRUE, recursive = T)
    list.file.input.short <-
      list.files(input, full.names = FALSE, recursive = T)
  }

  if (input.type == "wav") {
    list.file.input <- input
  }


  if (swift.time == TRUE) {
    number.of.slash <- str_count(list.file.input, pattern = "/")[1]
    base.file.name.all <-
      str_split_fixed(list.file.input,
                      pattern = "/",
                      n = (number.of.slash + 1))[, number.of.slash + 1]
    temp.name.all <-
      stringr::str_split_fixed(base.file.name.all, pattern = pattern.split, n = 2)[, 1]
    times <- str_split_fixed(temp.name.all, pattern = '_', n = 3)[, 3]
    times <- as.numeric(substr(times, start = 1, stop = 2))
    list.file.input <-
      list.file.input[which(times >= time.start & times <= time.stop)]
  }

  if (length(list.file.input) == 0) {
    print("No sound files detected")
    break
  }

  if (is.numeric(random.sample) == TRUE) {
    list.file.input <-
      list.file.input[sample(1:length(list.file.input), random.sample, replace =
                               F)]
  }


  for (i in 1:length(list.file.input)) {
    timing.df <- data.frame()


    contains.slash <- str_detect(list.file.input[i], pattern = "/")

    if (contains.slash == 'TRUE') {
      number.of.slash <- str_count(list.file.input[i], pattern = "/")
      base.file.name <-
        str_split_fixed(list.file.input[i],
                        pattern = "/",
                        n = (number.of.slash + 1))[, number.of.slash + 1]
      temp.name <-
        stringr::str_split_fixed(base.file.name, pattern = pattern.split, n = 2)[1]
    } else{
      temp.name <-
        stringr::str_split_fixed(list.file.input[i], pattern = pattern.split, n = 2)[1]

    }

    # Convert .wav file to spectrogram
    if (verbose == TRUE) {
      print(paste(
        "Computing spectrogram for file",
        temp.name,
        i,
        'out of',
        length(list.file.input)
      ))
    }

    RavenSelectionTableDF <- data.frame()
    temp.wav <- readWave(list.file.input[i])

    sound_length <-
      round(length(temp.wav@left) / temp.wav@samp.rate, 2)

    cutwave.list <-
      c(seq(
        from = 1,
        to = (sound_length),
        by = subsample.dur
      ), sound_length)

    short.sound.files <- lapply(1:(length(cutwave.list) - 1),
                                function(i)
                                  extractWave(
                                    temp.wav,
                                    from = cutwave.list[i],
                                    to = cutwave.list[i +
                                                        1],
                                    xunit = c("time"),
                                    plot = F,
                                    output = "Wave"
                                  ))

    for (j in 1:length(short.sound.files)) {
      swift.spectro <-
        spectro(
          short.sound.files[[j]],
          wl = spectrogram.window,
          overlap = 0,
          plot = F
        )


      # Identify the frequency band of interest
      min.freq.cols <-
        which.min(abs(round(swift.spectro$freq, digits = 2) - (min.freq / 1000)))
      max.freq.cols <-
        which.min(abs(round(swift.spectro$freq, digits = 2) - (max.freq / 1000)))


      # Calculate the column sums for each time window
      col.sum <-
        colSums(swift.spectro$amp[min.freq.cols:max.freq.cols,])


      # Calculate noise value
      noise.value <-
        quantile(unlist(col.sum), c(noise.quantile.val))

      # Determine which values are above specified cutoff
      list.sub <- which(col.sum > noise.value)
      call.timing <-
        split(list.sub, cumsum(c(1, diff(list.sub)) != 1))

      # Calculate minimum signal duration to be considered signal
      if( length(which(swift.spectro$time > 1))>0){
        number.time.windows.1sec <- min(which(swift.spectro$time > 1))
        signal.dur <- number.time.windows.1sec * min.signal.dur

        # Combine all potential sound events into a list
        call.timing.list <-
          as.list(call.timing[which(sapply(call.timing, length) > signal.dur)])

        # If user indicated maximum duration create list of sound events under certain duration
        if (max.sound.event.dur != 'NULL') {
          sound.event.index.max <-
            which.min(abs(swift.spectro$time - max.sound.event.dur))
          call.timing.list <-
            call.timing.list[which(sapply(call.timing.list, length) < sound.event.index.max)]
        }
      } else{
        call.timing.list <- list()
      }

      if (length(call.timing.list) >= 1) {
        subsamps <- lapply(1:length(call.timing.list),
                           function(i)
                             extractWave(
                               short.sound.files[[j]],
                               from = swift.spectro$time[min(call.timing.list[[i]])],
                               to = swift.spectro$time[max(call.timing.list[[i]])],
                               xunit = c("time"),
                               plot = F,
                               output = "Wave"
                             ))

        if (j == 1) {
          if (wav.output == "TRUE")
            lapply(1:length(subsamps),
                   function(i)
                     writeWave(
                       subsamps[[i]],
                       filename = paste(
                         output.dir,
                         training.label,
                         '_',
                         paste(
                           temp.name,
                           swift.spectro$t[min(call.timing.list[[i]])],
                           swift.spectro$t[max(call.timing.list[[i]])],
                           '.wav',
                           sep = '_'
                         ),
                         sep = ''
                       ),
                       extensible = FALSE
                     ))
        }

        if (j > 1) {
          if (wav.output == "TRUE")
            lapply(1:length(subsamps),
                   function(i)
                     writeWave(
                       subsamps[[i]],
                       filename =  paste(
                         output.dir,
                         training.label,
                         '_',
                         paste(
                           temp.name,
                           (swift.spectro$t[min(call.timing.list[[i]])] +
                              (subsample.dur * (j - 1))),
                           (swift.spectro$t[max(call.timing.list[[i]])] +
                              (subsample.dur * (j - 1))),
                           '.wav',
                           sep = '_'
                         ),
                         sep = ''
                       ),
                       extensible = FALSE
                     ))
        }

        timing.df <- lapply(1:length(call.timing.list),
                            function(i)
                              cbind.data.frame(swift.spectro$t[min(call.timing.list[[i]])],
                                               swift.spectro$t[max(call.timing.list[[i]])]))

        timing.df <- do.call(rbind.data.frame, timing.df)

        colnames(timing.df) <- c('start.time', 'stop.time')
        file.name <- rep(temp.name, nrow(timing.df))
        timing.df <- cbind.data.frame(timing.df, file.name)

        if (j > 1) {
          timing.df$start.time <- timing.df$start.time + (subsample.dur * (j - 1))
          timing.df$stop.time <-
            timing.df$stop.time + (subsample.dur * (j - 1))
        }

        timing.df <- rbind.data.frame(timing.df)
        Selection <- seq(1, nrow(timing.df))
        View <- rep('Spectrogram 1', nrow(timing.df))
        Channel <- rep(1, nrow(timing.df))
        MinFreq <- rep(min.freq, nrow(timing.df))
        MaxFreq <- rep(max.freq, nrow(timing.df))
        timing.df.temp <-
          cbind.data.frame(Selection, View, Channel, MinFreq, MaxFreq, timing.df)

        timing.df.temp <-
          timing.df.temp[, c(
            "Selection",
            "View",
            "Channel",
            "start.time",
            "stop.time",
            "MinFreq",
            "MaxFreq",
            "file.name"
          )]

        colnames(timing.df.temp) <-
          c(
            "Selection",
            "View",
            "Channel",
            "Begin Time (s)",
            "End Time (s)",
            "Low Freq (Hz)",
            "High Freq (Hz)",
            "File Name"
          )


        RavenSelectionTableDF <-
          rbind.data.frame(RavenSelectionTableDF, timing.df.temp)
        RavenSelectionTableDF$Selection <-
          seq(1, nrow(RavenSelectionTableDF), 1)
        if (write.table.output == TRUE) {
          csv.file.name <-
            paste(output.dir, '/', temp.name, 'BLED.txt', sep = '')
          write.table(
            x = RavenSelectionTableDF,
            sep = "\t",
            file = csv.file.name,
            row.names = FALSE,
            quote = FALSE
          )
        }
        rm(subsamps)
      }
    }
  }
  print(RavenSelectionTableDF)
  rm(RavenSelectionTableDF)
  rm(swift.spectro)
  rm(temp.wav)
}
