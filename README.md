gibbonR: An R package for the automated detection and classification of
female gibbon calls from long-term acoustic recordings
================

-   [Authors](#authors)
-   [Package description](#package-description)
-   [Tutorial](#tutorial)
-   [Quick start guide](#quick-start-guide)
    -   [You can install the development version from GitHub
        with:](#you-can-install-the-development-version-from-github-with)
-   [Part 1. Training Data with Labeled .wav
    clips](#part-1-training-data-with-labeled-wav-clips)
-   [Part 2. Run the
    detector/classifier](#part-2-run-the-detectorclassifier)
    -   [Part 2a. Feature extraction](#part-2a-feature-extraction)
    -   [Part 2b. Run DetectClassify](#part-2b-run-detectclassify)
-   [Part 3. Data visualization](#part-3-data-visualization)
    -   [Part 3a. Create a UMAP plot colored by
        class](#part-3a-create-a-umap-plot-colored-by-class)
    -   [Part 3b. Create a UMAP plot colored by affinity propagation
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

*Planned release of updated package October 31, 2022*

# Tutorial

<https://denajgibbon.github.io/gibbonR-tutorial/>

# Quick start guide

## You can install the development version from [GitHub](https://github.com/DenaJGibbon) with:

``` r
# install.packages("devtools")
# devtools::install_github("DenaJGibbon/gibbonR")

library(gibbonR)
#> Loading required package: stringr
#> Loading required package: e1071
#> Loading required package: randomForest
#> randomForest 4.7-1
#> Type rfNews() to see new features/changes/bug fixes.
#> Loading required package: tuneR
#> Loading required package: seewave
```

# Part 1. Training Data with Labeled .wav clips

### Read in clips and calculate MFCCs

``` r
TrainingWavFilesDir <- 
  "/Users/denaclink/Desktop/RStudio Projects/gibbonR/data/MultipleSoundClasses/"

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
#> [1] "SVM accuracy 88"


ml.model.rf <- randomForest::randomForest(x=trainingdata[, 2:ncol(trainingdata)], y = trainingdata$class)


print(ml.model.rf)
#> 
#> Call:
#>  randomForest(x = trainingdata[, 2:ncol(trainingdata)], y = trainingdata$class) 
#>                Type of random forest: classification
#>                      Number of trees: 500
#> No. of variables tried at each split: 13
#> 
#>         OOB estimate of  error rate: 10.67%
#> Confusion matrix:
#>               female.gibbon leaf.monkey noise solo.gibbon class.error
#> female.gibbon            17           0     2           1   0.1500000
#> leaf.monkey               0          11     4           0   0.2666667
#> noise                     0           0    19           1   0.0500000
#> solo.gibbon               0           0     0          20   0.0000000
```

# Part 2. Run the detector/classifier

## Part 2a. Feature extraction

``` r
# Specify the folder where the training data will be saved
TrainingDataFolderLocation <- "/Users/denaclink/Desktop/RStudio Projects/gibbonR/data/TrainingDataFromRavenSelectionTables/"
  
TrainingDataMFCC <- MFCCFunction(input.dir= TrainingDataFolderLocation, min.freq = 400, max.freq = 1600,win.avg="TRUE")
  
TrainingDataMFCC$class <- as.factor(TrainingDataMFCC$class)
```

## Part 2b. Run DetectClassify

``` r
  TestFileDirectory <- '/Users/denaclink/Library/CloudStorage/Box-Box/gibbonRSampleFiles/GibbonTestFiles'
  
  OutputDirectory <-  "/Users/denaclink/Desktop/RStudio Projects/gibbonR/data/DetectAndClassifyOutput"
  
  DetectAndClassify(input=TestFileDirectory,
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
#> Loading required package: ggplot2
#> 
#> Attaching package: 'ggplot2'
#> The following object is masked from 'package:randomForest':
#> 
#>     margin
gibbonID(input.dir="/Users/denaclink/Desktop/RStudio Projects/gibbonR/data/MultipleSoundClasses/",output.dir="/Users/denaclink/Desktop/RStudio Projects/gibbonR/data/MultipleSoundClasses/Thumbnails/",win.avg='standard',add.spectrograms=TRUE,min.freq=400,max.freq=1600,class='no.clustering')
#> [1] "Step 1 Calculating MFCCs"
#> [1] "processing sound event 1 out of 75"
#> [1] "processing sound event 2 out of 75"
#> [1] "processing sound event 3 out of 75"
#> [1] "processing sound event 4 out of 75"
#> [1] "processing sound event 5 out of 75"
#> [1] "processing sound event 6 out of 75"
#> [1] "processing sound event 7 out of 75"
#> [1] "processing sound event 8 out of 75"
#> [1] "processing sound event 9 out of 75"
#> [1] "processing sound event 10 out of 75"
#> [1] "processing sound event 11 out of 75"
#> [1] "processing sound event 12 out of 75"
#> [1] "processing sound event 13 out of 75"
#> [1] "processing sound event 14 out of 75"
#> [1] "processing sound event 15 out of 75"
#> [1] "processing sound event 16 out of 75"
#> [1] "processing sound event 17 out of 75"
#> [1] "processing sound event 18 out of 75"
#> [1] "processing sound event 19 out of 75"
#> [1] "processing sound event 20 out of 75"
#> [1] "processing sound event 21 out of 75"
#> [1] "processing sound event 22 out of 75"
#> [1] "processing sound event 23 out of 75"
#> [1] "processing sound event 24 out of 75"
#> [1] "processing sound event 25 out of 75"
#> [1] "processing sound event 26 out of 75"
#> [1] "processing sound event 27 out of 75"
#> [1] "processing sound event 28 out of 75"
#> [1] "processing sound event 29 out of 75"
#> [1] "processing sound event 30 out of 75"
#> [1] "processing sound event 31 out of 75"
#> [1] "processing sound event 32 out of 75"
#> [1] "processing sound event 33 out of 75"
#> [1] "processing sound event 34 out of 75"
#> [1] "processing sound event 35 out of 75"
#> [1] "processing sound event 36 out of 75"
#> [1] "processing sound event 37 out of 75"
#> [1] "processing sound event 38 out of 75"
#> [1] "processing sound event 39 out of 75"
#> [1] "processing sound event 40 out of 75"
#> [1] "processing sound event 41 out of 75"
#> [1] "processing sound event 42 out of 75"
#> [1] "processing sound event 43 out of 75"
#> [1] "processing sound event 44 out of 75"
#> [1] "processing sound event 45 out of 75"
#> [1] "processing sound event 46 out of 75"
#> [1] "processing sound event 47 out of 75"
#> [1] "processing sound event 48 out of 75"
#> [1] "processing sound event 49 out of 75"
#> [1] "processing sound event 50 out of 75"
#> [1] "processing sound event 51 out of 75"
#> [1] "processing sound event 52 out of 75"
#> [1] "processing sound event 53 out of 75"
#> [1] "processing sound event 54 out of 75"
#> [1] "processing sound event 55 out of 75"
#> [1] "processing sound event 56 out of 75"
#> [1] "processing sound event 57 out of 75"
#> [1] "processing sound event 58 out of 75"
#> [1] "processing sound event 59 out of 75"
#> [1] "processing sound event 60 out of 75"
#> [1] "processing sound event 61 out of 75"
#> [1] "processing sound event 62 out of 75"
#> [1] "processing sound event 63 out of 75"
#> [1] "processing sound event 64 out of 75"
#> [1] "processing sound event 65 out of 75"
#> [1] "processing sound event 66 out of 75"
#> [1] "processing sound event 67 out of 75"
#> [1] "processing sound event 68 out of 75"
#> [1] "processing sound event 69 out of 75"
#> [1] "processing sound event 70 out of 75"
#> [1] "processing sound event 71 out of 75"
#> [1] "processing sound event 72 out of 75"
#> [1] "processing sound event 73 out of 75"
#> [1] "processing sound event 74 out of 75"
#> [1] "processing sound event 75 out of 75"
#> [1] "Step 3 Creating Spectrograms "
#> [1] "/Users/denaclink/Desktop/RStudio Projects/gibbonR/data/MultipleSoundClasses/Thumbnails/ already exists"
#> [1] "Adding Spectrograms to Plot Step 3 of 3"
```

<img src="man/figures/README-unnamed-chunk-8-1.png" width="100%" />

## Part 3b. Create a UMAP plot colored by affinity propagation clustering

``` r
library(gibbonR)
library(ggpubr)
library(apcluster)
#> 
#> Attaching package: 'apcluster'
#> The following object is masked from 'package:stats':
#> 
#>     heatmap
gibbonID(input.dir="/Users/denaclink/Desktop/RStudio Projects/gibbonR/data/MultipleSoundClasses/",output.dir="/Users/denaclink/Desktop/RStudio Projects/gibbonR/data/MultipleSoundClasses/Thumbnails/",win.avg='standard',class='affinity.fixed', q.fixed=0.1,add.spectrograms=TRUE,min.freq=400,max.freq=1600,)
#> [1] "Step 1 Calculating MFCCs"
#> [1] "processing sound event 1 out of 75"
#> [1] "processing sound event 2 out of 75"
#> [1] "processing sound event 3 out of 75"
#> [1] "processing sound event 4 out of 75"
#> [1] "processing sound event 5 out of 75"
#> [1] "processing sound event 6 out of 75"
#> [1] "processing sound event 7 out of 75"
#> [1] "processing sound event 8 out of 75"
#> [1] "processing sound event 9 out of 75"
#> [1] "processing sound event 10 out of 75"
#> [1] "processing sound event 11 out of 75"
#> [1] "processing sound event 12 out of 75"
#> [1] "processing sound event 13 out of 75"
#> [1] "processing sound event 14 out of 75"
#> [1] "processing sound event 15 out of 75"
#> [1] "processing sound event 16 out of 75"
#> [1] "processing sound event 17 out of 75"
#> [1] "processing sound event 18 out of 75"
#> [1] "processing sound event 19 out of 75"
#> [1] "processing sound event 20 out of 75"
#> [1] "processing sound event 21 out of 75"
#> [1] "processing sound event 22 out of 75"
#> [1] "processing sound event 23 out of 75"
#> [1] "processing sound event 24 out of 75"
#> [1] "processing sound event 25 out of 75"
#> [1] "processing sound event 26 out of 75"
#> [1] "processing sound event 27 out of 75"
#> [1] "processing sound event 28 out of 75"
#> [1] "processing sound event 29 out of 75"
#> [1] "processing sound event 30 out of 75"
#> [1] "processing sound event 31 out of 75"
#> [1] "processing sound event 32 out of 75"
#> [1] "processing sound event 33 out of 75"
#> [1] "processing sound event 34 out of 75"
#> [1] "processing sound event 35 out of 75"
#> [1] "processing sound event 36 out of 75"
#> [1] "processing sound event 37 out of 75"
#> [1] "processing sound event 38 out of 75"
#> [1] "processing sound event 39 out of 75"
#> [1] "processing sound event 40 out of 75"
#> [1] "processing sound event 41 out of 75"
#> [1] "processing sound event 42 out of 75"
#> [1] "processing sound event 43 out of 75"
#> [1] "processing sound event 44 out of 75"
#> [1] "processing sound event 45 out of 75"
#> [1] "processing sound event 46 out of 75"
#> [1] "processing sound event 47 out of 75"
#> [1] "processing sound event 48 out of 75"
#> [1] "processing sound event 49 out of 75"
#> [1] "processing sound event 50 out of 75"
#> [1] "processing sound event 51 out of 75"
#> [1] "processing sound event 52 out of 75"
#> [1] "processing sound event 53 out of 75"
#> [1] "processing sound event 54 out of 75"
#> [1] "processing sound event 55 out of 75"
#> [1] "processing sound event 56 out of 75"
#> [1] "processing sound event 57 out of 75"
#> [1] "processing sound event 58 out of 75"
#> [1] "processing sound event 59 out of 75"
#> [1] "processing sound event 60 out of 75"
#> [1] "processing sound event 61 out of 75"
#> [1] "processing sound event 62 out of 75"
#> [1] "processing sound event 63 out of 75"
#> [1] "processing sound event 64 out of 75"
#> [1] "processing sound event 65 out of 75"
#> [1] "processing sound event 66 out of 75"
#> [1] "processing sound event 67 out of 75"
#> [1] "processing sound event 68 out of 75"
#> [1] "processing sound event 69 out of 75"
#> [1] "processing sound event 70 out of 75"
#> [1] "processing sound event 71 out of 75"
#> [1] "processing sound event 72 out of 75"
#> [1] "processing sound event 73 out of 75"
#> [1] "processing sound event 74 out of 75"
#> [1] "processing sound event 75 out of 75"
#> [1] "Step 2 Computing unsupervised clustering with fixed q"
#> [1] "Step 3 Creating Spectrograms "
#> [1] "/Users/denaclink/Desktop/RStudio Projects/gibbonR/data/MultipleSoundClasses/Thumbnails/ already exists"
#> [1] "Adding Spectrograms to Plot Step 3 of 3"
```

<img src="man/figures/README-unnamed-chunk-9-1.png" width="100%" />

### How to cite

This package is currently in development, with submission to rOpenSci
planned shortly. In the interim, please cite the arXiv preprint:

Clink, D. J. & H. Klinck. (2019). gibbonR: An R package for the
detection and classification of acoustic signals using machine learning.
arXiv, 1906.02572. <https://doi.org/10.48550/arXiv.1906.02572>
