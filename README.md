# Package Description

The field of bioacoustics is inherently multidisciplinary and relies on computer scientists, engineers, and ecologists. This package is directed towards ecologists who are interested in incorporating bioacoustics into their research, but may not have the skills or training.The goal for the creation of this package was to make commonly used signal processing techniques and various machine learning algorithms readily available for anyone interested in using bioacoustics in their research.

# Getting Started 
First you need to install the package using the following lines of code:

```{r eval=FALSE}
install.packages(c("coda","mvtnorm","devtools","loo"))
library(devtools)
devtools::install_github("DenaJGibbon/gibbonR-package")
```

Then load the library
```{r eval=FALSE}
library(gibbonR)
```

# Loading the data
Data is structured as a list with the name of the sound file as the first element and the .wav file as the second element
```{r eval=FALSE}
data("gibbon.females")
str(gibbon.females)
```

Alternatively, load your own data into the correct list format
```{r eval=FALSE}
# Set the working directory where your .wav files are stored
setwd("wav.file.directory")

# Create empty list for the .wav list
gibbon.females <- list()

# Create a list of .wav files in the above specified directory
list.wav.file.names <- list.files()

# Loop to read in all .wav files in directory and add the file names and .wav files to a list
for(x in 1:length(list.wav.file.names)){
  tmp.wav <- tuneR::readWave(list.wav.file.names[x])
  gibbon.females[[x]] <- list(list.wav.file.names[x],tmp.wav)
}
```
