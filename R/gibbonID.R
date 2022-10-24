#' gibbonID
#'
#' @param input.dir.Focal
#' @param output.dir.Focal
#' @param min.freq
#' @param max.freq
#' @param main
#' @param pattern
#' @param add.spectrograms
#' @param class
#' @param q.fixed
#' @param max.clusters
#'
#' @return
#' @export
#'
#' @examples
gibbonID <- function(input.dir.Focal,output.dir.Focal,min.freq,max.freq,main,pattern = '.wav',
                                          add.spectrograms=FALSE,
                                      class='fixed', q.fixed=0.1, max.clusters=25)
{


  Focal.exemplars <- list.files(input.dir.Focal,full.names = T,pattern = pattern)


  print('Step 1 Calculating MFCCs')
  AcousticSignalsMFCCs <- MFCCFunction(input.dir=input.dir.Focal,
                                             min.freq = min.freq, max.freq = max.freq,
                                             num.cep = 12)

  if(class=='affinity.adaptive'){
    print('Step 2 Computing unsupervised clustering')

    q.val.seq <- seq(from=0.1,to=0.9,by=0.1)

    AcousticSignal.sil.df <- data.frame()
    for(a in 1:length(q.val.seq)){
      print(a)
      AcousticSignalsAP <-
        apcluster::apcluster(negDistMat(r=2),q=q.val.seq[a],
                             AcousticSignalsMFCCs[,c(2:ncol(AcousticSignalsMFCCs))],
                             maxits=100000,convits=10000)


      sil <-
        cluster::silhouette(x = AcousticSignalsAP@idx,
                            dist = dist(AcousticSignalsMFCCs[,c(2:ncol(AcousticSignalsMFCCs))]))

      sil.val <- (summary(sil)$avg.width)
      temp.sil.df <-  cbind.data.frame(sil.val,q.val.seq[a])
      AcousticSignal.sil.df <- rbind.data.frame(AcousticSignal.sil.df,temp.sil.df)
    }

    MaxSil <- which.max(AcousticSignal.sil.df$sil.val)


    AcousticSignalsAP <-
      apcluster::apcluster(negDistMat(r=2),q= q.val.seq[MaxSil],
                           AcousticSignalsMFCCs[,c(2:ncol(AcousticSignalsMFCCs))],
                           maxits=100000,convits=10000)

    print(q.val.seq[MaxSil])
    print(paste('N clusters=', length(AcousticSignalsAP@exemplars)))
    AcousticSignalsMFCCs$class <- as.factor(AcousticSignalsAP@idx)
  }

  if(class=='affinity.fixed'){
    print('Step 2 Computing unsupervised clustering with fixed q')

    AcousticSignalsAP <-
      apcluster::apcluster(negDistMat(r=2),q=q.fixed,
                           AcousticSignalsMFCCs[,c(2:ncol(AcousticSignalsMFCCs))],
                           maxits=100000,convits=10000)

    AcousticSignalsMFCCs$class <- as.factor(AcousticSignalsAP@idx)

  }

  if(class=='no.clustering'){
    AcousticSignalsMFCCs$class <- AcousticSignalsMFCCs$class
  }

  AcousticSignals.umap <-
    umap::umap(AcousticSignalsMFCCs[,c(2:ncol(AcousticSignalsMFCCs))],
               n_neighbors = 12,
               controlscale=TRUE,scale=3)

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
    geom_point(size = 3) +
    scale_color_manual(values = matlab::jet.colors (length(unique(plot.for.AcousticSignals$class)))) +
    theme_bw() + ggtitle(main) + xlab('UMAP: Dim 1')+ylab('UMAP: Dim 2')+ theme(legend.position = "none")+
    ggtitle(paste('N Clusters =', length(AcousticSignalsAP@exemplars)))+
    theme(axis.text.x=element_blank(), #remove x axis labels
          axis.ticks.x=element_blank(), #remove x axis ticks
          axis.text.y=element_blank(),  #remove y axis labels
          axis.ticks.y=element_blank()  #remove y axis ticks
    )

  if(add.spectrograms==TRUE){

  print('Step 3 Creating Spectrograms ')

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



  print('Adding Spectrograms to Plot Step 3 of 3')

  col.index <- unique(plot.for.AcousticSignals$class)
  xrange <- (abs(range(plot.for.AcousticSignals$Dim.1)[1])+abs(range(plot.for.AcousticSignals$Dim.1)[2]))/40
  yrange <- (abs(range(plot.for.AcousticSignals$Dim.2)[1])+abs(range(plot.for.AcousticSignals$Dim.2)[2]))/40
  color.vals <- matlab::jet.colors (length(unique(plot.for.AcousticSignals$class)))

  for(y in 1:length(Focal.exemplars)) {

    #print(y, 'out of', length(Focal.exemplars))
    figure1.png <- magick::image_trim(magick::image_read(paste(output.dir.Focal,y,'Focal.png',sep='')))
    figure1.png <- magick::image_modulate(figure1.png,brightness = 300)

    figure1.png <- magick::image_border(figure1.png,col=color.vals[which(col.index==plot.for.AcousticSignals[y,]$class)])

    figure1.png <- as.raster(figure1.png)
    #exemplar.index <- Focal.cluster.results@idx[y]

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
    "DetectionsAffinityPlot.png",
    my_plot_AcousticSignals,
    width = 4.25,
    height = 3.25,
    dpi = 1200
  )


  return(my_plot_AcousticSignals)
}

