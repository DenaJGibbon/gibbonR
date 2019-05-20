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

# SEE THE VIGNETTE FOR DETAILED EXAMPLES

https://github.com/DenaJGibbon/gibbonR-package/blob/master/gibbonr_vignette.pdf
