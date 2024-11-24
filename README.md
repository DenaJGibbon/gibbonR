gibbonR: An R package for the automated detection and classification of
female gibbon calls from long-term acoustic recordings
================

- [Authors](#authors)
- [Package description](#package-description)
- [Tutorial](#tutorial)
- [Quick start guide](#quick-start-guide)
  - [You can install the development version from GitHub
    with:](#you-can-install-the-development-version-from-github-with)
- [Part 1. Training Data with Labeled .wav
  clips](#part-1-training-data-with-labeled-wav-clips)
- [Part 2. Run the
  detector/classifier](#part-2-run-the-detectorclassifier)
  - [Part 2a. Feature extraction](#part-2a-feature-extraction)
  - [Part 2b. Run DetectClassify](#part-2b-run-detectclassify)
- [Part 3. Data visualization](#part-3-data-visualization)
  - [Part 3a. Create a UMAP plot colored by
    class](#part-3a-create-a-umap-plot-colored-by-class)
  - [Part 3b. Create a UMAP plot colored by affinity propagation
    clustering](#part-3b-create-a-umap-plot-colored-by-affinity-propagation-clustering)

<!-- README.md is generated from README.Rmd. Please edit that file -->

# Authors

Dena J. Clink & Holger Klinck  
K. Lisa Yang Center for Conservation Bioacoustics, Cornell Lab of
Ornithology, Cornell University

# Package description

The field of bioacoustics is inherently multidisciplinary and relies on
computer scientists, engineers, and ecologists. This package is directed
towards ecologists who are interested in incorporating bioacoustics into
their research, but may not have the skills or training. The goal for
the creation of this package was to make commonly used signal processing
techniques and various machine learning algorithms readily available in
R for anyone interested in using bioacoustics in their research.

# Tutorial

<https://denajgibbon.github.io/gibbonR-tutorial/>

# Quick start guide

## You can install the development version from [GitHub](https://github.com/DenaJGibbon) with:

``` r
# install.packages("devtools")
# devtools::install_github("DenaJGibbon/gibbonR")
library(gibbonR)
```

```
# You need to tell R where to store the zip files on your computer.
destination.file.path.zip <-
  "dataBorneoExampleData.zip"

# You also need to tell R where to save the unzipped files
destination.file.path <- "data/gibbonR/data/"

# This function will download the data from github

utils::download.file("https://github.com/DenaJGibbon/BorneoExampleData/archive/master.zip",
                     destfile = destination.file.path.zip)

# This function will unzip the file
utils::unzip(zipfile = destination.file.path.zip,
             exdir = destination.file.path)

# Examine the contents
list.of.sound.files <- list.files(paste(destination.file.path,
                                        "BorneoExampleData-master", "data", sep =
                                          "/"),
                                  full.names = T)
list.of.sound.files
```

Use this function to read in the .RDA file and save it as an R object
from
<https://stackoverflow.com/questions/5577221/how-can-i-load-an-object-into-a-variable-name-that-i-specify-from-an-r-data-file>

``` r
loadRData <- function(fileName) {
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}
```

This function will load the entire list of r data files

``` r
list.rda.files <- list()
for(x in 1:length(list.of.sound.files)){
  list.rda.files[[x]] <-  loadRData(list.of.sound.files[[x]])
}
```

Assign each rda an informative name

``` r
multi.class.list <- list.rda.files[[1]]
S11_20180219_060002_1800sto3600s <- list.rda.files[[2]]
```

Now we create a directory with the training .wav files

``` r
TrainingDataDirectory <- "data/gibbonR/data/BorneoMultiClass"

for(a in 1:length(multi.class.list)){
  Temp.element <- multi.class.list[[a]]
  writeWave(Temp.element[[2]], paste(TrainingDataDirectory,Temp.element[[1]],sep='/'))
}
```

# Part 1. Training Data with Labeled .wav clips

### Read in clips and calculate MFCCs

``` r
TrainingWavFilesDir <- 
  "data/gibbonR/data/BorneoMultiClass/"

trainingdata <- gibbonR::MFCCFunction(input.dir=TrainingWavFilesDir, min.freq = 400, max.freq = 1600,win.avg='standard')

trainingdata$class <- as.factor(trainingdata$class)
```

### Compare Random Forest and Support Vector Machine for Supervised Classification

``` r
trainingdata$class <- as.factor(trainingdata$class)


ml.model.svm <- e1071::svm(trainingdata[, 2:ncol(trainingdata)], trainingdata$class, kernel = "radial", 
                           cross = 25,
                           probability = TRUE)

print(paste('SVM accuracy',ml.model.svm$tot.accuracy))


ml.model.rf <- randomForest::randomForest(x=trainingdata[, 2:ncol(trainingdata)], y = trainingdata$class)


print(ml.model.rf)
```

# Part 2. Run the detector/classifier

## Part 2a. Feature extraction

``` r
# Specify the folder where the training data will be saved
TrainingDataFolderLocation <- "data/gibbonR/data/TrainingDataFromRavenSelectionTables/"
  
TrainingDataMFCC <- MFCCFunction(input.dir= TrainingDataFolderLocation, min.freq = 400, max.freq = 1600,win.avg="standard")
  
TrainingDataMFCC$class <- as.factor(TrainingDataMFCC$class)
```

## Part 2b. Run DetectClassify

``` r
  TestFileDirectory <- '/Users/denaclink/Library/CloudStorage/Box-Box/gibbonRSampleFiles/GibbonTestFiles'
  
  OutputDirectory <-  "data/gibbonR/data/DetectAndClassifyOutput"
  
  gibbonR(input=TestFileDirectory,
                    feature.df=TrainingDataMFCC,
                    model.type.list=c('SVM','RF'),
                    tune = TRUE,
                    short.wav.duration=300,
                    target.signal = c("female.gibbon"),
                    min.freq = 400, max.freq = 1600,
                    noise.quantile.val=0.15,
                    minimum.separation =3,
                    n.windows = 9, num.cep = 12,
                    spectrogram.window =160,
                    pattern.split = ".wav",
                    min.signal.dur = 3,
                    max.sound.event.dur = 25,
                    maximum.separation =1,
                    probability.thresh.svm = 0.15,
                    probability.thresh.rf = 0.15,
                    wav.output = "TRUE",
                    output.dir =OutputDirectory,
                    swift.time=TRUE,time.start=5,time.stop=10,
                    write.table.output=FALSE,verbose=TRUE,
                    random.sample='NA')
```

# Part 3. Data visualization

## Part 3a. Create a UMAP plot colored by class

``` r
library(gibbonR)
library(ggpubr)
gibbonID(input.dir="data/gibbonR/data/MultipleSoundClasses/",output.dir="data/gibbonR/data/MultipleSoundClasses/Thumbnails/",win.avg='standard',add.spectrograms=TRUE,min.freq=400,max.freq=1600,class='no.clustering')
```

## Part 3b. Create a UMAP plot colored by affinity propagation clustering

``` r
library(gibbonR)
library(ggpubr)
library(apcluster)
gibbonID(input.dir="data/gibbonR/data/MultipleSoundClasses/",output.dir="data/gibbonR/data/MultipleSoundClasses/Thumbnails/",win.avg='standard',class='affinity.fixed', q.fixed=0.1,add.spectrograms=TRUE,min.freq=400,max.freq=1600)
```

### How to cite

This package is currently in development, with submission to JOSS
planned shortly. In the interim, please cite the arXiv preprint:

Clink, D. J. & H. Klinck. (2019). gibbonR: An R package for the
detection and classification of acoustic signals using machine learning.
arXiv, 1906.02572. <https://doi.org/10.48550/arXiv.1906.02572>
