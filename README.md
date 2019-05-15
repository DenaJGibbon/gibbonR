gibbonR: An R package for classification, detection and visualization of
acoustic signals using machine learning
================
Dena J. Clink & Holger Klinck
2019-05-15

# PACKAGE DESCRIPTION

The field of bioacoustics is inherently multidisciplinary and relies on
computer scientists, engineers, and ecologists. This package is directed
towards ecologists who are interested in incorporating bioacoustics into
their research, but may not have the skills or training. The goal for
the creation of this package was to make commonly used signal processing
techniques and various machine learning algorithms readily available for
anyone interested in using bioacoustics in their research.

# GETTING STARTED

First you need to install the package using the following lines of code:

``` r
install.packages(c("coda","mvtnorm","devtools","loo"))
library(devtools)
devtools::install_github("DenaJGibbon/gibbonR")
```

Then load the library

``` r
library(gibbonR)
```

# CLASSIFICATION OF SOUND EVENTS 
This function classifies sound events from a specified directory using user-trained machine learning algorithms.

We will now load a training dataset of MFCCs that was calculated for 4337 observer-labeled sound events.

```{r eval=TRUE, warning=FALSE}
# MFCC settings were as follows: min.freq=400, max.freq=2000, n.window=9, n.cep=12.
data("training.MFCC.long")
```

This function will read in a set of .wav files that were created using either method of audio segmentation (GMM or SVM). It can also be used on user-labeled calls in a directory. This function requires the user to input labeled training data and gives the option to use SVM, GMM or NNET models for classification. The option is set to plot=TRUE which means that spectrograms for each sound event will be plotted along with the predicted class.

```{r eval=TRUE, results='hide', warning=FALSE}

classification.df <- classifyGibbonR(
  feature.df = training.MFCC.long,
  model.type = "SVM",
  tune = FALSE,
  input.dir = "/Users/denasmacbook/Desktop/output.test/temp",
  plot = TRUE,
  min.freq = 400,
  max.freq = 2000,
  n.window = 9,
  n.cep = 12
)

# The function returns a dataframe which includes the file name, model prediction and probability
classification.df
```
