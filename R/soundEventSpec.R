spec.col ="D"
min.freq
max.freq

soundEventSpec

temp.spec <-
  signal::specgram(wav.for.detection@left,
                   Fs = wav.for.detection@samp.rate,
                   n = 1024,
                   overlap = 0)

# Then plot the spectrogram
plot(
  temp.spec,
  xlab = "Time (s)",
  ylab = "Frequency (Hz)",
  col = viridis::viridis(12,option=spec.col),
  ylim=c(min.freq,max.freq),
  useRaster = TRUE
)

# And add boxes for the identified sound events
for (x in 1:nrow(detectiontiming$timing.df)) {
  rect(detectiontiming$timing.df[x, 3],
       400,
       detectiontiming$timing.df[x, 4],
       2000,
       border = "blue")
  vec <-
    c(detectiontiming$timing.df[x, 3], detectiontiming$timing.df[x, 4])
  x.val <- vec[-length(vec)] + diff(vec) / 2
  text(x.val, 2200,
       labels = round(detectiontiming$timing.df[x, 5], digits = 1),cex=2.5)
}
