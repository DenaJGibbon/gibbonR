#' @title  Audio segmentation using Gaussian mixture models (GMM)
#' @description This function calculates a bi-Gaussian mixture model to identify signal from noise in the recording of interest.
#' @param wav.file A R wave object; the sound file with audio signal(s) of interest
#' @param window.len Window length (in samples); default is 512.
#' @param window.type Which window type to use to calculate the spectrogram; default "hanning"
#' @param min.freq Minimum frequency of the signal of interest in kHz
#' @param max.freq Maximum frequency of the signal of interest in kHz
#' @param low.quant.val When determing the cutoff for "noise", the low quantile will be calculated on the "noise" density distribution
#' @param high.quant.val When determing the cutoff for "noise", the high quantile will be calculated on the "signal" density distribution
#' @param density.plot Logical; whether to output density plot
#' @param min.signal.dur Minimum duration of sound event in seconds
#' @param output.type Option of "wav" or "mfcc". If "wav" option is chosen then all sound events will be saved to specified directory. If "mfcc" is chosen, then MFCCs for all sound events will be calculated.
#' @param output.dir Specified location to write .wav files of sound events.
#' @param n.window If "mfcc" option is chosen number of time windows to calculate MFCCs for each sound event
#' @param n.cep If "mfcc" option is chosen number of MFCC cepstra to calculate
#' @export
#' @import ggplot2
#' @import tuneR
#' @import seewave

audioSegmentGMM <- function(wav.file, window.len=512, window.type="hanning", min.freq=0.4, max.freq=2,
                          low.quant.val=0.15, high.quant.val=0.25, which.quant="intersection",
                          density.plot=TRUE,
                          min.signal.dur=3,
                          output.type="wav",
                          output.dir,
                          n.window=10,n.cep=12) {

  if(output.type=="wav" & output.dir=="") {
    stop("Must specify output directory")
  }

    # Read in spectrogram of target .wav file

    swift.spectro <- seewave::spectro(wav.file, wl=window.len, wn=window.type, plot=F)

    # Determine which columns correspond to indicated min and max frequency
    min.freq.cols <- which.min(abs(round(swift.spectro$freq,digits=2) - min.freq))
    max.freq.cols <- which.min(abs(round(swift.spectro$freq,digits=2) - max.freq))

    # Calculate column sums for subsequent calculations
    col.sum <- colSums(swift.spectro$amp[min.freq.cols:max.freq.cols, ])

    # Calculate a bi-Gaussian mixture model
    fit <- mixtools::normalmixEM(col.sum, k=2)

    # Simulate two distributions from the above model; N equals the number of draws from the distributions
    N <- 1000

    # Combine values into a dataframe
    combined.df <- data.frame(vals=c(rnorm(N, mean= fit$mu[1], sd=fit$sigma[1]),rnorm(N, mean= fit$mu[2], sd=fit$sigma[2])),
                              labels= rep(c("Noise", "Signal"), each=N))

    # Calculate density distributions for "signal" and "noise"
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

    # Plot density

     dens.plot <- ggplot2::ggplot(combined.df, ggplot2::aes(vals, fill=labels))+ ggplot2::geom_density( alpha=0.45) + ggplot2::geom_vline(xintercept=max(vals.df[which.quant]))+
       ggplot2::scale_fill_manual(values=c("red", "blue")) + ggplot2::theme_bw() + ggplot2::xlab("Log-energy") + ggplot2::ylab("Density")+
       ggplot2::theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + ggplot2::guides(fill=guide_legend(title=""))+
       ggplot2::theme(axis.text.x = element_text(size=16)) + ggplot2::theme(axis.text.y = element_text(size=16))+ ggplot2::theme(axis.title.y = element_text(size=16))+ ggplot2::theme(axis.title.x = element_text(size=16))+
       ggplot2::theme(legend.text = element_text(size=16))
     if(density.plot==TRUE){
     print(dens.plot)
    }


    # Determine which values are above specified cutoff
    list.sub <- which(col.sum > max(vals.df[which.quant]))
    call.timing <- split(list.sub, cumsum(c(1,diff(list.sub))!=1))

    # Calculate minimum number of consecuite values above threshold to be considered signal
    number.time.windows.1sec <- min(which(swift.spectro$time >1))
    signal.dur <- number.time.windows.1sec*min.signal.dur

    # Combine all potential sound events into a list
    call.timing.list <- as.list(call.timing[which(sapply(call.timing, length) > signal.dur)])

    if(length(call.timing.list)==0){
      print("No sound events match specified criteria")
    } else{

    # Write all sound events to specified output directory
    if(output.type=="wav"){
      sound.event.df <- data.frame()
    for (x in 1: length(call.timing.list)){tryCatch({
      call.time.sub <- call.timing.list[[x]]
      short.wav <- seewave::cutw(wav.file, from=swift.spectro$time[min(call.time.sub)], to=swift.spectro$time[max(call.time.sub)],output = "Wave")
      print(paste("processing", x))
      tuneR::writeWave(short.wav, filename = paste(output.dir, "/",
                                             "sound.event","_", x, "_", swift.spectro$time[min(call.time.sub)], "_", swift.spectro$time[max(call.time.sub)], ".wav", sep=""),extensible = F)
      temp.df <- cbind.data.frame(paste("sound.event","_", x,sep=""),swift.spectro$time[min(call.time.sub)],swift.spectro$time[max(call.time.sub)])

      colnames(temp.df) <- c("sound event", "start.time", "end.time")
      sound.event.df <- rbind.data.frame(sound.event.df,temp.df)
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    }
      return(list(sound.event.df=sound.event.df,dens.plot=dens.plot))
    }

    sound.event.mfcc.list <- list()
      if(output.type=="mfcc"){
        for (x in 1: length(call.timing.list)){tryCatch({
          call.time.sub <- call.timing.list[[x]]
          print(paste("processing", x))

          call.time.sub <- call.timing.list[[x]]
          short.wav <- seewave::cutw(wav.file, from=swift.spectro$time[min(call.time.sub)],
                                     to=swift.spectro$time[max(call.time.sub)],output = "Wave")

        # Find duration of .wav file and divide into windows
        wav.dur <- seewave::duration(short.wav)
        win.time <- wav.dur / n.window

        # Calculate MFCCs
        melfcc.output <- melfcc(
          short.wav,
          minfreq = min.freq*1000,
          hoptime = win.time,
          maxfreq = max.freq*1000,
          numcep = n.cep,
          wintime = win.time
        )

        # Calculate delta cepstral coefficients
        deltas.output <- deltas(melfcc.output)

        # Also append .wav duration
        mfcc.vector <-
          c(paste(from=swift.spectro$time[min(call.time.sub)], to=swift.spectro$time[max(call.time.sub)]),as.vector(t(melfcc.output[1:(n.window-1), 2:n.cep])), as.vector(t(deltas.output[1:(n.window-1), 2:n.cep])),wav.dur)

        sound.event.mfcc.list[[x]] <- mfcc.vector

        }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
        }


        sound.event.df <-
          matrix(
            unlist(sound.event.mfcc.list),
            nrow = length(sound.event.mfcc.list),
            ncol = length(sound.event.mfcc.list[[1]]),
            byrow = T
          )

        # Fix time columns
        seq.length <- (ncol(sound.event.df)-2)
        mfcc.names <- paste(rep("mfcc", seq.length), seq(seq.length), sep = "_")
        delta.names <- paste(rep("delta", seq.length), seq(seq.length), sep = "_")

        start.time <- stringr::str_split_fixed(sound.event.df[,1], " ", n=2)[,1]
        end.time <- stringr::str_split_fixed(sound.event.df[,1], " ", n=2)[,2]

        sound.event.df <- cbind.data.frame(start.time, end.time, sound.event.df[,2:ncol(sound.event.df)])

        as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

        for(y in 1:ncol(sound.event.df)){
          sound.event.df[,y] <- as.numeric.factor(sound.event.df[,y])
        }

        return(list(sound.event.df=sound.event.df,dens.plot=dens.plot))
      }
    }
    }

