#' gibbonR
#' @description This function identifies sound events using band-limited energy summation and then classifies the sound events using a trained support vector machine or random forest algorithm.
#' @usage input, input.type='list', feature.df,model.type.list=c("SVM"), tune = FALSE, target.signal = "female.gibbon",
#' short.wav.duration=300,min.freq = 400, max.freq = 2000,
#' noise.quantile.val=0.5, minimum.separation =5, n.windows = 9, num.cep = 12, spectrogram.window =1600,
#' pattern.split = ".wav", min.signal.dur = 4, maximum.separation =1,max.sound.event.dur = 12,
#' probability.thresh.svm = 0.75, probability.thresh.rf = 0.75, wav.output = "TRUE", output.dir = getwd(),
#' swift.time=TRUE,time.start=6,time.stop=12, write.table.output=TRUE,verbose=TRUE, random.sample='NA'}
#' @param input Either full path to directory containing .wav files, a list of .wav files, or a the path to a single .wav file
#' @param input.type Either 'directory', 'list' or 'wav'
#' @param feature.df Data frame of features from labeled sound files; first column must be class labels
#' @param tune Logical; if want to use 'tune' function for SVM; NOTE: for large datasets adds significant computing time
#' @param target.signal Labeled signal(s) of interest from training data (feature.df); can include multiple classes.
#' @param min.freq Minimum frequency (Hz) of signal of interest
#' @param max.freq Maximum frequency (Hz) of signal of interest
#' @param n.windows Number of time windows to calculate for MFCCs
#' @param num.cep Number of cepstra coefficients to calculate for MFCCs
#' @param pattern.split Pattern to find and remove to create full sound file name; currently set to ".wav"
#' @param probability.thresh.svm Probability threshold (provided by SVM) to be considered as target signal
#' @param probability.thresh.rf Probability threshold (provided by RF) to be considered as target signal
#' @param model.type.list Which machine learning model to use; SVM or RF
#' @param short.wav.duration Duration (s) to divide longer sound file to increase processing efficiency
#' @param noise.quantile.val A quantile value between 0 to 1 for the band energy summation
#' @param minimum.separation The minimum number of consecutive time windows that signals must be separated by to be considered a separate sound event
#' @param maximum.separation The minimum number of consecutive time windows that signals must be separated by to be considered a separate sound event
#' @param spectrogram.window Window length for spectrogram analysis (input to spectro fuction from 'seewave')
#' @param min.signal.dur The minimum duration (s) sound events must be to be considered sound events
#' @param max.sound.event.dur The maximum duration (s) sound events must be to be considered sound events; NOTE this only happens when writing text file
#' @param wav.output Logical; output .wav files of detections in specified directory
#' @param swift.time If file name is in structure recorder_YYYYMMDD_HHMMSS can subset files based on specific times
#' @param time.start Time recordings start (hour)
#' @param time.stop Time recordings stop (hour)
#' @param write.table.output Logical; write Raven selection tables to output directory
#' @param verbose Logical; print out steps
#' @param random.sample If a random subset of files in a directory are desired specify a value, otherwise 'NA'
#' @param output.dir Specified output directory; set to current working directory
#' @details
#' @export
#' @import e1071
#' @import randomForest
#' @import tuneR
#' @import seewave
#' @import tuneR
#' @import stringr
#' @return If write.table.output=TRUE writes a .txt file for each sound file with detections
#' @return If write.table.output=TRUE writes a .txt file for each sound file with detections
#' @examples
#' \donttest{MFCCFunction(input.dir = "FocalRecordings",min.freq = 400,max.freq=2500)}




gibbonR <-
  function(input,
           input.type = 'list',
           feature.df,
           model.type.list = c("SVM"),
           tune = FALSE,
           target.signal = "female.gibbon",
           short.wav.duration = 300,
           min.freq = 400,
           max.freq = 2000,
           noise.quantile.val = 0.5,
           minimum.separation = 5,
           n.windows = 9,
           num.cep = 12,
           spectrogram.window = 1600,
           pattern.split = ".wav",
           min.signal.dur = 4,
           maximum.separation = 1,
           max.sound.event.dur = 12,
           probability.thresh.svm = 0.75,
           probability.thresh.rf = 0.75,
           wav.output = "TRUE",
           output.dir = getwd(),
           swift.time = TRUE,
           time.start = 6,
           time.stop = 12,
           write.table.output = TRUE,
           verbose = TRUE,
           random.sample = 'NA') {

    target.signal.in.training <-
      as.factor(target.signal) %in% feature.df$class

    if (length(unique(target.signal.in.training)) > 1 |
        unique(target.signal.in.training) %in% FALSE) {
      print("Training data does not contain target signal")
    }


    if ((wav.output == "TRUE" & output.dir == ""))
      stop("Specify output directory")


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

    print("Machine learning in progress...")

    if ("SVM" %in% model.type.list == TRUE) {
      print("SVM in progress...")
      start_time <- Sys.time()
      if (tune == TRUE) {
        ## SVM classification

        tune.rad <-
          e1071::tune(
            svm,
            feature.df[, 2:ncol(feature.df)],
            feature.df$class,
            kernel = "radial",
            tunecontrol = tune.control(cross = 5),
            ranges = list(
              cost = c(0.001, 0.01, 0.1, 1, 2,
                       10, 100, 1000),
              gamma = c(0.01, 0.1, 0.5, 1, 2)
            )
          )


        ml.model.svm <-
          e1071::svm(
            feature.df[, 2:ncol(feature.df)],
            feature.df$class,
            kernel = "radial",
            gamma = tune.rad$best.parameters$gamma,
            cost = tune.rad$best.parameters$cost,
            cross = 5,
            probability = TRUE
          )

      } else {
        ml.model.svm <-
          e1071::svm(
            feature.df[, 2:ncol(feature.df)],
            feature.df$class,
            kernel = "radial",
            gamma = 0.01,
            cost = 2,
            cross = 25,
            probability = TRUE
          )
      }
      print(paste('SVM accuracy', ml.model.svm$tot.accuracy))
      end_time <- Sys.time()
      print(end_time - start_time)
    }


    if ("RF" %in% model.type.list == TRUE) {
      print("RF in progress...")
      tryCatch({
        start_time <- Sys.time()

        ml.model.rf <-
          randomForest::randomForest(x = feature.df[, 2:ncol(feature.df)], y = feature.df$class)


        print(ml.model.rf)

        end_time <- Sys.time()
        print(end_time - start_time)
      }, error = function(e) {
        cat("ERROR :", conditionMessage(e), "\n")
      })
    }

    print(paste("Classifying for target signal", c(target.signal)))


    for (i in 1:length(list.file.input)) {
      model.results.list <- list()
      RavenSelectionTableDF <- data.frame()
      tryCatch({
        start_time <- Sys.time()
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

        temp.wav <- readWave(list.file.input[i])

        sound_length <-
          round(length(temp.wav@left) / temp.wav@samp.rate, 2)
        cutwave.list <-
          c(seq(
            from = 1,
            to = (sound_length),
            by = short.wav.duration
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

        print('Running detector over sound files')
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

          # Determine which columns are above specified cutoff
          list.sub <- which(col.sum > noise.value)

          if (minimum.separation != 1) {
            # Find length differences between columns that match the specified cutoff
            detection.differences <-
              unlist(lapply(1:(length(list.sub) - 1),
                            function(i)
                              c(list.sub[i + 1] - list.sub[i])))
            #
            detection.separation.list <-
              which(detection.differences >= minimum.separation)

            # Add one to remove
            detection.separation.list <-
              c(1, detection.separation.list + 1)
            call.timing <- list()
            for (x in 1:(length(detection.separation.list) - 1)) {
              start.index <- detection.separation.list[x]
              finish.index <- detection.separation.list[x + 1]
              call.timing[[x]] <-
                list.sub[start.index]:list.sub[finish.index]
            }


          } else {
            call.timing <- split(list.sub, cumsum(c(1, diff(list.sub)) != 1))
          }

          # Calculate minimum signal duration to be considered signal
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

            calltimes <- lapply(1:length(call.timing.list),
                                function(i)
                                  cbind.data.frame(from = swift.spectro$time[min(call.timing.list[[i]])],
                                                   to = swift.spectro$time[max(call.timing.list[[i]])]))


            mfcc.list <- list()
            temp.model.results.list.svm <- list()
            temp.model.results.list.rf <- list()
            for (x in 1:length(subsamps)) {
              for (y in 1:length(target.signal)) {
                calltimes.subset <- calltimes[[x]]

                start.time <- calltimes.subset$from
                end.time <- calltimes.subset$to

                if (j > 1) {
                  start.time <- short.wav.duration * (j - 1) + start.time
                  end.time <- short.wav.duration * (j - 1) + end.time
                }

                start.time <-  round(start.time, 3)
                end.time <- round(end.time, 3)
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
                deltas.output <- deltas(melfcc.output)

                # Ensure only same number of time windows are used for MFCC and delta coefficients Also append .wav duration
                mfcc.vector <-
                  c(as.vector(t(melfcc.output[1:(n.windows - 1), 2:num.cep])), as.vector(t(deltas.output[1:(n.windows - 1), 2:num.cep])), wav.dur)


                mfcc.vector <- as.data.frame(t(mfcc.vector))

                if (length(colnames(mfcc.vector)) != length(colnames(feature.df[, 2:ncol(feature.df)]))) {
                  print(
                    'Training dataset columns do not match test dataset; check MFCC settings'
                  )
                  break
                }

                colnames(mfcc.vector) <-
                  colnames(feature.df[, 2:ncol(feature.df)])


                if ("SVM" %in% model.type.list == TRUE) {
                  svm.prob <- predict(ml.model.svm, mfcc.vector, probability = T)

                  model.output <- attr(svm.prob, "probabilities")
                  signal.loc <-
                    which(attr(model.output, "dimnames")[[2]] == target.signal[y])
                  signal.probability <- model.output[signal.loc]
                  temp.svm.df <-
                    cbind.data.frame(target.signal[y], signal.probability)
                  if (temp.svm.df$signal.probability >= probability.thresh.svm) {
                    if (wav.output == "TRUE") {
                      tuneR::writeWave(
                        subsamps[[x]],
                        filename = paste(
                          output.dir,
                          "/",
                          temp.name,
                          "_",
                          target.signal[y],
                          "_",
                          "SVM",
                          "_",
                          start.time,
                          "_",
                          end.time,
                          "_",
                          round(signal.probability, 3),
                          ".wav",
                          sep = ""
                        ),
                        extensible = F
                      )
                    }
                    #
                    temp.df <-
                      cbind.data.frame(
                        temp.name,
                        paste(j, x, sep = '.'),
                        "SVM",
                        target.signal[y],
                        round(signal.probability, 3),
                        start.time,
                        end.time
                      )
                    colnames(temp.df) <-
                      c(
                        "file.name",
                        "detect.num",
                        "model.type",
                        "signal",
                        "probability",
                        "start.time",
                        "end.time"
                      )
                    temp.model.results.list.svm[[x]] <- temp.df
                  }

                }

                if ("RF" %in% model.type.list == TRUE) {
                  RF.prob <- predict(ml.model.rf, mfcc.vector, type = 'prob')

                  model.output <- colnames(RF.prob)
                  signal.loc <- which(model.output == target.signal[y])
                  signal.probability <- RF.prob[, signal.loc]
                  temp.RF.df <-
                    cbind.data.frame(target.signal[y], signal.probability)
                  if (temp.RF.df$signal.probability >= probability.thresh.rf) {
                    if (wav.output == "TRUE") {
                      tuneR::writeWave(
                        subsamps[[x]],
                        filename = paste(
                          output.dir,
                          "/",
                          temp.name,
                          "_",
                          target.signal[y],
                          "_",
                          "RF",
                          "_",
                          start.time,
                          "_",
                          end.time,
                          "_",
                          round(signal.probability, 3),
                          ".wav",
                          sep = ""
                        ),
                        extensible = F
                      )
                    }
                    #
                    temp.df <-
                      cbind.data.frame(
                        temp.name,
                        paste(j, x, sep = '.'),
                        "RF",
                        target.signal[y],
                        round(signal.probability, 3),
                        start.time,
                        end.time
                      )
                    colnames(temp.df) <-
                      c(
                        "file.name",
                        "detect.num",
                        "model.type",
                        "signal",
                        "probability",
                        "start.time",
                        "end.time"
                      )
                    temp.model.results.list.rf[[x]] <- temp.df
                  }

                }



                if (exists("temp.model.results.list.svm") == TRUE  |
                    exists("temp.model.results.list.rf") == TRUE) {
                  if ("SVM" %in% model.type.list == TRUE &
                      "RF" %in% model.type.list == TRUE) {
                    if (exists("temp.model.results.list.svm") == TRUE  &
                        exists("temp.model.results.list.rf") == TRUE) {
                      temp.model.results.list.svm <-
                        temp.model.results.list.svm[lengths(temp.model.results.list.svm) != 0]
                      temp.model.results.list.rf <-
                        temp.model.results.list.rf[lengths(temp.model.results.list.rf) != 0]

                      temp.model.results.list <-
                        append(temp.model.results.list.svm,
                               temp.model.results.list.rf)
                    }

                    if (exists("temp.model.results.list.svm") == FALSE  &
                        exists("temp.model.results.list.rf") == TRUE) {
                      temp.model.results.list.rf <-
                        temp.model.results.list.rf[lengths(temp.model.results.list.rf) != 0]
                      temp.model.results.list <-
                        temp.model.results.list.rf
                    }

                    if (exists("temp.model.results.list.svm") == TRUE  &
                        exists("temp.model.results.list.rf") == FALSE) {
                      temp.model.results.list.svm <-
                        temp.model.results.list.svm[lengths(temp.model.results.list.svm) != 0]
                      temp.model.results.list <-
                        temp.model.results.list.svm
                    }

                  }

                  if ("SVM" %in% model.type.list == TRUE &
                      "RF" %in% model.type.list == FALSE &
                      exists("temp.model.results.list.svm") == TRUE) {
                    temp.model.results.list.svm <-
                      temp.model.results.list.svm[lengths(temp.model.results.list.svm) != 0]
                    temp.model.results.list <-
                      temp.model.results.list.svm
                  }

                  if ("SVM" %in% model.type.list == FALSE &
                      "RF" %in% model.type.list == TRUE &
                      exists("temp.model.results.list.rf") == TRUE) {
                    temp.model.results.list.rf <-
                      temp.model.results.list.rf[lengths(temp.model.results.list.rf) != 0]
                    temp.model.results.list <-
                      temp.model.results.list.rf
                  }


                }

              }

              model.results.list[[j]] <-
                do.call(rbind.data.frame, temp.model.results.list)
            }
          }
        }

        model.results.list <-
          model.results.list[lengths(model.results.list) != 0]

        if (exists("model.results.list") == TRUE &
            length(model.results.list) > 0) {
          if (exists("model.results.list") == TRUE &
              length(model.results.list) > 0) {
            print('Creating datasheet')

            timing.df <-  do.call(rbind.data.frame, model.results.list)



            # Add minimum separation
            for (k in 1:length(model.type.list)) {
              timing.df.subset <-
                subset(timing.df, model.type == model.type.list[[k]])

              detection.time.differences <-
                unlist(lapply(1:(nrow(timing.df.subset) - 1),
                              function(i)
                                c(
                                  timing.df.subset$start.time[i + 1] - timing.df.subset$end.time[i]
                                )))

              detection.separation.list <-
                which(detection.time.differences < maximum.separation)

              detection.timing <-
                split(detection.separation.list, cumsum(c(
                  1, diff(detection.separation.list)
                ) != 1))

              if (length(detection.timing) > 1) {
                for (j in 1:length(detection.timing)) {
                  temp.df <- detection.timing[[j]]
                  detection.timing[[j]] <-  c(temp.df, max(temp.df) + 1)
                }

                DetectionDFtemp <-
                  timing.df.subset[-c(unlist(detection.timing)), ]


                for (l in 1:length(detection.timing)) {
                  temp.subset <- detection.timing[[l]]

                  temprow1 <-
                    timing.df.subset[min(temp.subset):max(temp.subset), ]

                  probability <- median(temprow1$probability)
                  start.time <- round(min(temprow1$start.time), 3)
                  end.time <- round(max(temprow1$end.time), 3)
                  newselection <-
                    cbind.data.frame(temprow1[1, 1:4], probability, start.time, end.time)
                  DetectionDFtemp <-
                    rbind.data.frame(DetectionDFtemp, newselection)
                }
              }
              else {
                DetectionDFtemp <- timing.df.subset
              }

              RavenSelectionTableDF <-
                rbind.data.frame(RavenSelectionTableDF, DetectionDFtemp)
            }

            RavenSelectionTableDF <-
              RavenSelectionTableDF[order(RavenSelectionTableDF$start.time), ]

            Selection <- seq(1, nrow(RavenSelectionTableDF))
            View <- rep('Spectrogram 1', nrow(RavenSelectionTableDF))
            Channel <- rep(1, nrow(RavenSelectionTableDF))
            MinFreq <- rep(min.freq, nrow(RavenSelectionTableDF))
            MaxFreq <- rep(max.freq, nrow(RavenSelectionTableDF))

            if (nrow(RavenSelectionTableDF) > 0) {
              RavenSelectionTableDF <-
                cbind.data.frame(Selection,
                                 View,
                                 Channel,
                                 MinFreq,
                                 MaxFreq,
                                 RavenSelectionTableDF)

              RavenSelectionTableDF <-
                RavenSelectionTableDF[, c(
                  "Selection",
                  "View",
                  "Channel",
                  "start.time",
                  "end.time",
                  "MinFreq",
                  "MaxFreq",
                  "file.name",
                  'model.type',
                  'probability',
                  'signal'
                )]

              colnames(RavenSelectionTableDF) <-
                c(
                  "Selection",
                  "View",
                  "Channel",
                  "Begin Time (s)",
                  "End Time (s)",
                  "Low Freq (Hz)",
                  "High Freq (Hz)",
                  "File Name",
                  'model.type',
                  'probability',
                  'signal'
                )



              if (write.table.output == TRUE) {
                csv.file.name <-
                  paste(output.dir,
                        '/',
                        temp.name,
                        'gibbonRresults.txt',
                        sep = '')
                write.table(
                  x = RavenSelectionTableDF,
                  sep = "\t",
                  file = csv.file.name,
                  row.names = FALSE,
                  quote = FALSE
                )
                print(paste(
                  "'Here are results for soundfile",
                  temp.name,
                  i,
                  'out of',
                  length(list.file.input)
                ))
                print(RavenSelectionTableDF)


              }

              end_time <- Sys.time()
              print(
                paste(
                  'System processed',
                  round(seewave::duration(temp.wav)),
                  'seconds in',
                  round(end_time - start_time),
                  'seconds',
                  'this translates to',
                  round(
                    round(seewave::duration(temp.wav)) / 60 / 60 * 3600 / as.numeric(end_time - start_time) ,
                    1
                  ),
                  'hours processed in 1 hour'
                )
              )
            }
          }

          rm(RavenSelectionTableDF)
          rm(swift.spectro)
          rm(temp.wav)
          rm(short.sound.files)
        }
      }, error = function(e) {
        cat("ERROR :", conditionMessage(e), "\n")
      })
    }
  }
