#' MFCCFunction
#' @description Function to calculate Mel-frequency cepstral coefficents over a directory of focal recordings
#' @param input.dir where the .wav files are stored
#' @param min.freq the minimum frequency (Hz) of the signal of interest
#' @param max.freq the maximum frequency (Hz) of the signal of interest
#' @param n.windows the number of time windows to divide the signal by
#' @param win.avg Option of 'false','mean.sd' or 'standard'; whether to return MFCCs for each non-overlapping time window, calculate mean and SD over each MFCC or calculated MFCCs for a set number of time windows.
#' @param win.hop.time If win.avg='standard' the specified window size.
#' @param num.cep the number of cepstra to calculate for each time window
#' @export
#' @return a data frame with a row of MFCCs for each .wav file
#' @examples
#' \donttest{MFCCFunction(input.dir = "FocalRecordings",min.freq = 400,max.freq=2500)}

MFCCFunction <-
  function(input.dir,
           min.freq = 400,
           max.freq = 2000,
           n.windows = 9,
           num.cep = 12,
           win.avg = 'standard',
           win.hop.time = 0.25) {
    call.timing.list <-
      list.files(input.dir, full.names = T, pattern = '.wav')

    call.timing.list.short <-
      list.files(input.dir, full.names = F, pattern = '.wav')

    subsamps <- lapply(1:length(call.timing.list),
                       function(i)
                         readWave(call.timing.list[[i]]))

    if (win.avg == "false") {
      mfcc.output.df <- data.frame()
      ####Loop to calculate MFCC for each .wav file in the directory
      for (j in 1:length(subsamps)) {
        #print(paste("processing",j))
        wav.name <- call.timing.list.short[[j]]
        wav.file <- subsamps[[j]]


        # Calculate MFCCs
        melfcc.output <- tuneR::melfcc(
          wav.file,
          minfreq = min.freq,
          maxfreq = max.freq,
          wintime = win.hop.time,
          hoptime = win.hop.time,
          numcep = num.cep
        )

        melfcc.output <- as.data.frame(melfcc.output)
        class <-
          rep(stringr::str_split_fixed(wav.name, pattern = '_', n = 2)[, 1],
              nrow(melfcc.output))
        melfcc.output <- cbind.data.frame(class, melfcc.output)

        mfcc.output.df <-
          rbind.data.frame(mfcc.output.df, melfcc.output)
      }

      return(mfcc.output.df)
    }

    if (win.avg == "mean.sd") {
      mfcc.vector.list <- list()
      #Class <- stringr::str_split_fixed(call.timing.list.short,pattern = '_',n=2)[,1]
      Class <-
        stringr::str_split_fixed(call.timing.list.short, pattern = '_', n = 3)[, 2]
      for (x in 1:length(subsamps)) {
        #print(paste("processing sound event", x, 'out of',length(subsamps) ))

        short.wav <- subsamps[[x]]
        wav.dur <- seewave::duration(short.wav)
        win.time <- wav.dur / n.windows

        # Calculate MFCCs
        melfcc.output <- tuneR::melfcc(
          short.wav,
          minfreq = min.freq,
          hoptime = win.time,
          maxfreq = max.freq,
          numcep = num.cep,
          wintime = win.time
        )

        # Calculate delta cepstral coefficients
        deltas.output <- tuneR::deltas(melfcc.output)

        # Ensure only same number of time windows are used for MFCC and delta coefficients Also append .wav duration
        mfcc.vector <-
          c(as.vector(t(melfcc.output[1:(n.windows - 1), 2:num.cep])), as.vector(t(deltas.output[1:(n.windows - 1), 2:num.cep])), wav.dur)
        mfcc.vector.list[[x]] <- mfcc.vector
      }

      mfcc.output <- mfcc.vector.list
      class <-
        stringr::str_split_fixed(call.timing.list.short, pattern = '_', n = 2)[, 1]


      mfcc.output.df <- do.call(rbind.data.frame, mfcc.output)
      colnames(mfcc.output.df) <-
        seq(from = 1,
            to = ncol(mfcc.output.df),
            by = 1)

      mfcc.output.df <- cbind.data.frame(class, mfcc.output.df)
      return(mfcc.output.df)
    }

    if (win.avg == 'standard') {
      mfcc.vector.list <- list()
      for (x in 1:length(subsamps)) {
        short.wav <- subsamps[[x]]
        wav.dur <- duration(short.wav)
        win.time <- wav.dur / n.windows

        # Calculate MFCCs
        melfcc.output <-
          tuneR::melfcc(
            short.wav,
            minfreq = min.freq,
            hoptime = win.time,
            maxfreq = max.freq,
            numcep = num.cep,
            wintime = win.time
          )

        # Calculate delta cepstral coefficients
        deltas.output <- tuneR::deltas(melfcc.output)

        # Ensure only same number of time windows are used for MFCC and delta coefficients Also append .wav duration
        mfcc.vector <-
          c(as.vector(t(melfcc.output[1:(n.windows - 1), 2:num.cep])), as.vector(t(deltas.output[1:(n.windows - 1), 2:num.cep])), wav.dur)
        mfcc.vector.list[[x]] <- mfcc.vector
      }

      mfcc.output <- mfcc.vector.list
      class <-
        stringr::str_split_fixed(call.timing.list.short, pattern = '_', n = 2)[, 1]


      mfcc.output.df <- do.call(rbind.data.frame, mfcc.output)
      colnames(mfcc.output.df) <-
        seq(from = 1,
            to = ncol(mfcc.output.df),
            by = 1)

      mfcc.output.df <- cbind.data.frame(class, mfcc.output.df)
      return(mfcc.output.df)
    }

  }
