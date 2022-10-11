#' UMAPBiplotAddSpectrograms
#'
#' @param input.dir.Focal
#' @param output.dir.Focal
#' @param min.freq
#' @param max.freq
#' @param main
#' @param pattern
#'
#' @return
#' @export
#'
#' @examples
UMAPBiplotAddSpectrograms <- function(input.dir.Focal,output.dir.Focal,min.freq,max.freq,main,
                                      add.spectrograms=FALSE,pattern = '.wav',
                                      class='unsupervised',max.clusters=25,Nclusters=7,color='class',recursive=TRUE)
  {


Focal.exemplars <- list.files(input.dir.Focal,full.names = T,pattern = pattern, recursive = recursive)

print('Step 1 Calculating MFCCs')
AcousticSignalsMFCCs <- MFCCFunction(input.dir=input.dir.Focal,
                                           min.freq = min.freq, max.freq = max.freq)


AcousticSignals.umap <-
  umap::umap(AcousticSignalsMFCCs[,c(3:ncol(AcousticSignalsMFCCs))],
             n_neighbors = 12,
             controlscale=TRUE,scale=3)

print('Step 2 Creating biplot')

plot.for.AcousticSignals <-
  cbind.data.frame(AcousticSignals.umap$layout[,1:2],
                   AcousticSignalsMFCCs$class)

colnames(plot.for.AcousticSignals) <-
  c("Dim.1", "Dim.2", "class")

plot.for.AcousticSignals$class <- as.factor(plot.for.AcousticSignals$class)

my_plot_AcousticSignals <-
  ggpubr::ggscatter(data = plot.for.AcousticSignals,x = "Dim.1",
                    y = "Dim.2",
                    color  = "class") +
  geom_point(size = 1) +
  scale_color_manual(values = matlab::jet.colors(length(unique(plot.for.AcousticSignals$class)))) +
  theme_bw() + ggtitle(main) + xlab('UMAP: Dim 1')+ylab('UMAP: Dim 2')#+ theme(legend.position = "none")



if(add.spectrograms==TRUE){
print('Step 3 Creating Spectrograms')

if (!dir.exists(output.dir.Focal)){
  dir.create(output.dir.Focal)
  print(paste('Created output dir',output.dir.Focal))

for(b in 1:length(Focal.exemplars)) {
  #print(b)
  short.wav <- tuneR::readWave(Focal.exemplars[[b]])

  png(filename = paste(output.dir.Focal,b,'Focal.png',sep=''), width=1000)
  temp.spec <- signal::specgram(short.wav@left, Fs = short.wav@samp.rate, n = 1024, overlap = 0)
  plot(temp.spec, xlab = "", ylab = "", ylim = c(min.freq, max.freq), rev(gray(0:512 / 512)),
       axes=F,useRaster = TRUE)

  graphics.off()

}
} else {
  print(paste(output.dir.Focal,'already exists'))
}


print('Step 4 Adding Spectrograms to Plot ')
for(y in 1:length(Focal.exemplars)) {
  col.index <- unique(plot.for.AcousticSignals$class)
  xrange <- (abs(range(plot.for.AcousticSignals$Dim.1)[1])+abs(range(plot.for.AcousticSignals$Dim.1)[2]))/40
  yrange <- (abs(range(plot.for.AcousticSignals$Dim.2)[1])+abs(range(plot.for.AcousticSignals$Dim.2)[2]))/40
  color.vals <- matlab::jet.colors (length(unique(plot.for.AcousticSignals$class)))

  print(y)
  figure1.png <- magick::image_trim(magick::image_read(paste(output.dir.Focal,y,'Focal.png',sep='')))
  figure1.png <- magick::image_modulate(figure1.png,brightness = 300)

  figure1.png <- magick::image_border(figure1.png,col=color.vals[which(col.index==plot.for.AcousticSignals[y,]$class)])

  figure1.png <- as.raster(figure1.png)

  clust.df.subset <- plot.for.AcousticSignals[y,]
  xmin= clust.df.subset$Dim.1-xrange
  xmax=clust.df.subset$Dim.1+xrange
  ymin=clust.df.subset$Dim.2 + yrange
  ymax=clust.df.subset$Dim.2 -yrange
  my_plot_AcousticSignals <-
    my_plot_AcousticSignals + annotation_raster(figure1.png, xmin,xmax,ymin,ymax)

}

}

ggsave(
  paste(output.dir.Focal,"UMAPPlot.png",sep=''),
  my_plot_AcousticSignals,
  width = 4.25,
  height = 3.25,
  dpi = 1200
)
return(my_plot_AcousticSignals)
}

