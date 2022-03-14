**IMPORTANT UPDATE: An updated version of this package will be released soon!**


gibbonR: An R package for classification, detection and visualization of
acoustic signals using machine learning
================
Dena J. Clink & Holger Klinck  
Sys. Date()

### Package description

The field of bioacoustics is inherently multidisciplinary and relies on
computer scientists, engineers, and ecologists. This package is directed
towards ecologists who are interested in incorporating bioacoustics into
their research, but may not have the skills or training. The goal for
the creation of this package was to make commonly used signal processing
techniques and various machine learning algorithms readily available in R for
anyone interested in using bioacoustics in their research.

### Summary 

# Flow chart of R functions
```{r eval=TRUE, warning=FALSE, echo=FALSE}
library(DiagrammeR)
grViz("digraph flowchart {
      # node definitions with substituted label text
      graph [layout = dot]
      node [fontname = Helvetica, shape = rectangle]        
      tab1 [label = '@@1']
      tab2 [label = '@@2']
      tab3 [label = '@@3']
      tab4 [label = '@@4']
      tab5 [label = '@@5']
      tab6 [label = '@@6']
      tab7 [label = '@@7']
      tab8 [label = '@@8']
      tab9 [label = '@@9']
      tab10 [label = '@@10']
      tab11 [label = '@@11']
      tab12 [label = '@@12']
      
      
      # edge definitions with the node IDs
      tab1 -> tab2 -> tab3;
     
      tab3 -> {tab4 tab5 tab6}
      tab6 -> tab7 
      tab6 -> tab8
      tab4 -> tab9
      tab5 -> tab9
      tab7 -> tab9
      tab8 -> tab9
      tab9 ->tab10
      tab9 ->tab11
      tab10 -> tab12
      tab11 -> tab12
      
      {rank = same; tab4; tab5; tab6}
      }
     
      
      [1]: '1. Create labelled training dataset'
      [2]:  paste0('2. Extract features','\\n ','[MFCCfunction]')
      [3]: '3. Identify sound events in long-term recording'
      [4]: paste0('3a. Detect with band-limited energy detector','\\n ','[DetectBLED]')
      [5]: paste0('3b. Detect with support-vector machine','\\n ','[DetectSVM]')
      [6]: paste0('3c. Detect and classify sound events', '\\n ','[DetectAndClassify] ')
      [7]: paste0('4a. Use random forest algorithm',':\\n ','[model.type= RF]')
      [8]: paste0('4b. Use support vector machine algorithm','\\n ','[model.type= SVM]')
      [9]: '5. Validate output using a human observer'
      [10]: paste0('5a. Validate using gibbonR package','\\n ','[ValidateML]')
      [11]: paste0('5b. Validate using external software','\\n ','(e.g. Raven Pro)')
      [12]: paste0('6. Calculate performance metrics','\\n ','[PerformanceML]')
      ")

```


### How to cite

This package is currently in development, with submission to rOpenSci planned shortly. In the interim, please cite the arXiv preprint:

Clink, D. J. & H. Klinck. (2019). gibbonR: An R package for the detection and classification of acoustic signals using machine learning. arXiv, 1906.02572.
https://doi.org/10.48550/arXiv.1906.02572
