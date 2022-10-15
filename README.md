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

## Part 1. Training Data with Labeled .wav clips

### Read in clips and calculate MFCCs

``` r
TrainingWavFilesDir <- 
  "/Users/denaclink/Desktop/RStudio Projects/gibbonR/data/MultipleSoundClasses/"

trainingdata <- gibbonR::MFCCFunction(input.dir=TrainingWavFilesDir, min.freq = 400, max.freq = 1600,win.avg="TRUE")


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
#>         OOB estimate of  error rate: 14.67%
#> Confusion matrix:
#>               female.gibbon leaf.monkey noise solo.gibbon class.error
#> female.gibbon            17           0     1           2   0.1500000
#> leaf.monkey               0          11     4           0   0.2666667
#> noise                     0           0    18           2   0.1000000
#> solo.gibbon               0           0     2          18   0.1000000
```

# Part 2. Run the detector/classifier

## Part 2a. Feature extraction

``` r
# Specify the folder where the training data will be saved
TrainingDataFolderLocation <- "/Users/denaclink/Desktop/RStudio Projects/gibbonR/data/TrainingDataFromRavenSelectionTables/"
  
TrainingDataMFCC <- MFCCFunction(input.dir= TrainingDataFolderLocation, min.freq = 400, max.freq = 1600,win.avg="TRUE")
  
TrainingDataMFCC$class <- as.factor(TrainingDataMFCC$class)

head(TrainingDataMFCC)
#>           class           1          2          3          4           5
#> 1 female.gibbon -1.67865415  0.5729994  -3.984233 -1.3090477 -3.37351780
#> 2 female.gibbon  0.64396030 -2.9424900 -13.514235 -6.3147138 -1.70131078
#> 3 female.gibbon -0.05582056 -6.7204884 -13.687933 -3.3906449  1.57056739
#> 4 female.gibbon -0.75413788 -4.5651883 -12.879696 -7.1132525 -0.09392132
#> 5 female.gibbon -2.02203971 -2.4971321  -7.102662 -0.7696289 -3.18975338
#> 6 female.gibbon  1.08622443 -1.8177994 -15.103233 -7.2686634 -0.57552482
#>           6         7        8          9         10         11           12
#> 1 0.3105936 0.1696459 1.311983 -0.5641073  2.4008722  0.9321205 -1.479120213
#> 2 5.3058564 4.9527350 5.193831  3.7647316  3.3873655 -1.7425736  0.906573640
#> 3 6.0878100 9.0943708 5.366268 -6.5849606 -0.3351189 -3.2181154 -2.134002169
#> 4 6.6230861 5.0109042 6.423887  4.0958264  1.9896681 -7.8584063  1.550414059
#> 5 1.4712819 2.8849289 7.056083  3.4086032  0.3740126 -3.7597120 -0.707106608
#> 6 5.3941266 0.5985080 3.287024  4.9605174  5.9785839  1.3343790 -0.001672048
#>           13        14        15         16         17         18       19
#> 1  -0.640649  -3.51664 -2.297406 -4.5273685  0.2348174 -0.2871091 4.287281
#> 2  -5.685564 -17.20058 -6.776460  6.8010640 12.0718331  3.8829946 1.174075
#> 3 -10.028059 -12.31573 -2.530264  3.2662186 10.1496824  3.8178378 2.503676
#> 4  -7.387470 -18.82988 -5.973804  4.3949646 11.1801173  3.2073070 4.716534
#> 5 -14.110787 -13.93834  5.612468 -0.8847168  2.3381042  8.5964263 3.631353
#> 6  -1.056987 -11.39812 -6.082625 -4.8052532  2.6040039  3.3119408 6.535049
#>           20         21         22         23           24         25
#> 1  5.0822113  3.9989260  0.7012323 -1.9121147   0.08280462  -5.487350
#> 2 -0.7041363 -3.1754792 -6.7757897 -0.1878303 -14.99119139 -16.111626
#> 3 -3.1512581 -5.1687262 -6.5817486 -2.4547459 -16.43124829  -9.456785
#> 4  1.8800319 -4.6680108 -9.6699613 -0.9882553 -17.86431154 -12.318537
#> 5 -4.0990332 -0.2399214  0.6841001 -0.3020701 -12.91210642 -14.112810
#> 6  5.6244545  5.0509595 -2.7117487  0.6735534 -14.34269787 -17.193150
#>           26         27        28         29         30        31        32
#> 1 -4.6905421 -0.2580836  1.960718  0.6923722  3.1430810  1.472173  2.264346
#> 2  3.3945852  6.8777319  6.565395  4.3592916  0.4288946 -5.148960 -5.149956
#> 3  0.6513362  3.9368430 10.647064  1.3445279  4.8927661 -7.829705 -7.845089
#> 4  4.7800301 -0.2841027  9.436862  8.3262033 -1.6364753 -4.273354 -2.148725
#> 5  5.5172791  2.8411258  2.762101 10.5359368  4.7633358 -8.664251 -4.496385
#> 6  4.3326418  2.0655813  2.927222  9.3196375  7.4466520 -4.535600 -5.171398
#>          33        34         35        36         37         38        39
#> 1 -1.983381 -2.309594  -1.616975 -6.660904 -2.2153856  1.8470061  3.464155
#> 2 -4.272262 -4.025316 -17.257192 -3.729469  1.4667429  2.6625000  8.852140
#> 3 -1.823385 -4.480690 -17.792666 -5.322850 -0.3582371  4.4932761 10.874459
#> 4 -5.360246 -3.250139 -16.674493 -5.485547  1.7335093 -0.9984589 12.359558
#> 5 -3.300529 -4.266743 -19.761705 -3.395808  0.9689629  2.6060063 10.369635
#> 6 -5.309965 -4.354349 -21.356777 -3.694090 -0.6225666  3.7583759 12.574223
#>           40         41         42          43         44        45         46
#> 1  0.3937689  2.9301821 -0.8730177 -0.05075467 -1.5968011 -6.652179   0.825201
#> 2  2.1377834  1.8083295 -5.8420207 -4.22966769 -0.2731013 -3.492307 -16.831704
#> 3 -1.5557986  1.6351875 -8.5382397 -4.71550544  3.2816416 -7.066540 -16.052042
#> 4  3.4177981 -0.3288512 -5.2714302 -6.94709263 -2.0253302 -3.850590 -19.021071
#> 5 -3.0185784  5.3288969 -6.0985439 -6.39039066  2.3874106 -4.345643 -17.626993
#> 6 -3.1883324  4.6308559 -7.1801511 -7.32610930  2.3559516 -4.629842 -15.707738
#>           47        48          49        50         51          52         53
#> 1 -5.7390746 -0.759871  2.96470967 -3.278249  5.4915900 -3.02445521 -0.7672330
#> 2 -4.4316068  4.771087  2.07423661  9.708605 -1.9914358 -4.03669176 -2.8924365
#> 3 -2.0658999  2.272661  0.64405859  3.389321  2.6336048 -0.03970172 -3.9265139
#> 4 -5.5117502  5.898038 -0.09892835 10.404459  1.7116837 -4.69003818 -2.8906372
#> 5 -1.3372002  5.327700 -2.11005442  8.516354  0.9596168  1.12196750 -1.7192486
#> 6 -0.4151541  2.764132 -2.03958432  8.830802 -2.1023343  1.67645728 -0.9151646
#>           54         55        56         57         58        59         60
#> 1  2.2171205 -2.5714773 -2.900364  -2.480874 -4.5527145  1.679542  0.8152644
#> 2  0.1375317  2.5127565 -6.476970 -13.869389 -2.0663047  5.449661  0.8643290
#> 3  0.5561424 -2.0991191 -7.415145 -14.933622  1.7541393  1.673452  1.6611881
#> 4 -0.7731013 -0.8316873 -7.544732 -14.443017 -0.2702325  1.111978  0.4883531
#> 5 -5.1594065  1.1894325 -2.682735 -15.672964 -1.8305790 11.317693 -4.4240299
#> 6 -4.0706042 -0.1076527 -2.923152 -15.377485 -3.3986020  7.680569 -4.5696693
#>          61         62        63         64         65          66        67
#> 1 0.3395093  0.7688115  1.024854  0.4768496 -0.2703771 -0.08078384 -2.589356
#> 2 4.1839371 -0.5882082 -1.880708 -1.0314691 -0.3454899 -0.40920612 -6.437668
#> 3 0.4483167  2.6727849 -2.125546 -2.1093001  0.9915142 -0.99942253 -7.004642
#> 4 2.8203948  1.4821403  1.252465 -2.3250599 -2.2191582 -1.06117999 -7.536912
#> 5 3.9493510  2.9043901 -4.759194 -1.4315153 -0.6980086 -3.73419877 -3.576921
#> 6 4.4313841  3.1229987 -2.481033  0.8768428 -2.0650228 -3.60489827 -3.469349
#>            68         69        70        71         72       73         74
#> 1  -0.7663297 -3.2782402  2.605591 -1.023368  0.5755397 1.354820  0.9237194
#> 2 -12.9860153  0.5733554  2.099415  1.801634  2.2397563 2.301610 -3.2324125
#> 3 -12.8962750  0.4700829  1.543309  2.172546 -0.5690281 1.019493  1.4715497
#> 4 -11.7471734  2.5399325 -1.186612  1.540041 -0.5215302 2.474505  1.5732413
#> 5 -13.4982869 -2.7579642 12.420980 -3.465436 -0.5888938 4.720315 -1.8455201
#> 6 -15.0922906 -3.3563799 10.239235 -6.454013  1.8963328 4.089230 -0.7607551
#>           75         76         77        78         79        80         81
#> 1 -2.8597882 -0.8192997  1.0190540 -2.383779  -4.329652 -3.478868  5.6235404
#> 2 -1.2608958  2.0880796 -3.9893154 -6.820092 -11.765022  1.520962  3.3209745
#> 3 -1.3143488  2.0144007 -2.7817260 -6.289175 -13.124400  2.189789  4.4374574
#> 4 -4.2815748 -2.5167657 -2.7604960 -7.795882  -7.165617  4.801503 -0.6776114
#> 5 -3.5523274  2.7531042 -0.4516247 -3.279527 -11.560738 -1.763522 12.6817430
#> 6 -0.4844795  2.0886877 -1.5251347 -3.391463 -10.968545 -2.847411 11.0495538
#>           82          83         84         85        86        87         88
#> 1 -0.2915626 -2.45136806 -1.6263098  2.6265661  1.104626 -1.818318 -2.6395749
#> 2  3.1955328 -2.72562339  1.2937863  1.5673524 -4.347222  5.317942 -2.1892469
#> 3 -0.5928926 -1.49280941  0.6409924  2.4265344 -7.781154  5.103738  2.7145156
#> 4  3.2926710  0.06783946 -2.8136563  5.3265616 -4.499313  2.214063 -2.8078598
#> 5 -3.5381562 -1.96821243  2.7635528 -0.5490254 -3.237087  2.799096 -0.3037433
#> 6 -5.5066604 -2.21307199  3.7728120 -0.9762743 -4.747671  3.278160  1.6535202
#>           89        90        91        92        93        94        95
#> 1  -979.0139 -872.5793 -671.5994 -370.8001  16.60254  26.35709  37.41538
#> 2 -1127.1717 -974.9744 -722.3502 -366.5693  85.44656 106.82746  87.01294
#> 3 -1131.4207 -975.4417 -704.1808 -347.5831  65.18677  71.88802  34.01325
#> 4 -1157.2540 -994.8341 -734.9646 -363.9326 101.88462 109.32510  55.57422
#> 5 -1077.2579 -951.2428 -716.4956 -363.4000  72.59831  64.74448  35.58424
#> 6 -1131.8631 -979.0345 -743.2095 -389.6558  74.87791 113.66219 114.45717
#>          96           97          98          99       100        101       102
#> 1  25.32839   21.7009721    5.510034    3.434014 -1001.013  -890.7785 -686.0332
#> 2  13.13979  -36.5074235  -67.659760  -63.734997 -1114.683  -936.2933 -694.5840
#> 3 -42.20112  -77.3988668  -87.963082  -71.152401 -1150.446  -973.9797 -720.8640
#> 4 -36.45598 -104.6752575 -137.052721 -128.080663 -1144.729  -963.8284 -716.0656
#> 5 -15.34107  -37.9441683  -69.657803  -77.496303 -1149.136 -1003.7106 -723.5451
#> 6  45.67280   -0.3763257  -21.562806  -14.810933 -1121.491  -981.7135 -735.6913
#>         103      104       105       106       107        108         109
#> 1 -365.8695 50.02040  61.76449 59.032108  40.57789   24.00198   -6.722392
#> 2 -359.4464 75.15151  72.51266 26.593932 -75.80964 -136.51741 -129.338226
#> 3 -364.6967 78.47327  57.55026 -5.460407 -83.35575 -118.05774 -119.725823
#> 4 -355.5391 98.85879  93.20060 29.063621 -90.00294 -153.94902 -162.355102
#> 5 -359.7915 81.45389  90.52030 35.797217 -32.85853  -18.38267  -31.464367
#> 6 -374.0717 83.38736 108.85204 92.936689  26.97044  -21.10471  -66.163877
#>          110        111        112       113       114      115      116
#> 1  -18.86443  -995.7747  -878.6886 -671.6588 -358.5406 41.72868 46.22243
#> 2  -82.22835 -1145.1047  -984.2663 -725.2592 -372.7745 70.52838 63.80312
#> 3  -77.12862 -1171.9951  -990.2171 -740.8611 -363.2215 74.07056 45.11642
#> 4 -122.77049 -1191.6090 -1010.9552 -727.9499 -376.8903 81.48943 82.77519
#> 5  -30.00078 -1120.2113  -979.4953 -699.7471 -342.9263 66.11992 56.19549
#> 6  -76.27027 -1159.4908 -1010.1156 -724.8958 -343.2854 96.15159 88.13993
#>          117        118       119        120       121        122        123
#> 1  39.523415   7.039806 -24.96373  -37.51214 -37.24123  -983.9158  -862.1315
#> 2  -8.466053 -97.056324 -99.95490  -77.77090 -45.99859 -1171.7772  -999.2748
#> 3 -24.243071 -73.338106 -79.52594  -66.81151 -14.78576 -1177.4013  -993.8562
#> 4 -12.004942 -91.560268 -92.58105 -108.78222 -71.30242 -1189.7193  -999.2779
#> 5  -7.801385 -87.413355 -79.68716  -76.52392 -67.61416 -1183.8301 -1001.4415
#> 6  10.337690 -90.749542 -96.09100 -103.12515 -98.47556 -1208.0162 -1012.2062
#>         124       125      126      127       128       129       130
#> 1 -659.1209 -349.3866 39.17666 32.46644  20.77628 -16.15369 -35.92017
#> 2 -746.2708 -380.5113 69.04965 45.93090 -27.31661 -49.90960 -49.97790
#> 3 -754.3204 -383.8787 60.82016 40.60025 -13.95614 -27.23776 -24.30080
#> 4 -740.6350 -384.0686 69.38450 39.84349 -33.43529 -65.41850 -64.76665
#> 5 -765.9146 -376.9185 78.09974 48.47233 -21.39502 -31.58201 -25.72837
#> 6 -772.7770 -379.6389 80.86799 49.22462 -25.73088 -36.08568 -37.13291
#>           131         132       133        134       135       136      137
#> 1 -35.9932833 -24.5368431 -1000.011  -903.8687 -669.4755 -374.7444 31.93377
#> 2 -42.3275617  -0.7934263 -1141.313  -970.7459 -742.8642 -407.3492 50.60238
#> 3  -0.7461581  55.9260326 -1194.238 -1040.0686 -773.9659 -402.8452 71.11279
#> 4 -74.0157937 -15.4479873 -1189.588 -1007.7012 -753.3064 -408.5460 65.78620
#> 5 -13.1079487  38.5492072 -1156.384  -987.8638 -743.7244 -385.5327 74.53475
#> 6 -19.2539386  44.1066892 -1160.723  -990.9394 -759.2876 -395.9865 69.70361
#>        138        139       140        141        142        143        144
#> 1 18.48091  14.391272 -19.12307 -30.908991 -22.260402 -39.290422  -962.7755
#> 2 50.81322  -9.801857 -27.76091  -6.650858   3.233272  50.850725 -1140.1115
#> 3 58.21572 -17.852601 -37.94313 -36.307636 -38.443374 -24.109620 -1162.3341
#> 4 61.48968 -21.971080 -56.07462 -37.809279 -42.799046   5.460883 -1188.6110
#> 5 43.38241 -27.967564 -36.68500 -14.604560 -25.574629  13.287860 -1126.7322
#> 6 42.81040 -24.179709 -30.30699 -20.845336 -32.530482   8.204371 -1150.0586
#>          145       146       147      148      149       150       151
#> 1  -858.0295 -655.9568 -357.6799 33.32906 22.57485  12.04648 -11.24118
#> 2  -993.0855 -755.1404 -407.5856 59.43852 41.08683 -20.61330 -36.75819
#> 3 -1027.0235 -767.0059 -412.5970 60.25976 45.52383 -23.17490 -22.36902
#> 4 -1038.7801 -779.2309 -404.1909 73.17829 44.00571 -20.35195 -27.22754
#> 5  -985.2766 -739.5848 -415.0862 39.84779 36.27166 -46.38510 -71.76870
#> 6 -1000.4801 -747.3017 -406.3978 63.68335 53.44565 -26.08135 -57.48652
#>          152        153        154        155        156       157       158
#> 1  -7.839495  -6.998869  -7.640970  -956.1717  -853.5606 -653.1507 -360.5628
#> 2 -16.980348 -14.270299   6.311325 -1134.1034  -996.6182 -747.5484 -409.3395
#> 3 -19.213015 -13.445455 -11.081641 -1135.3326 -1008.2241 -760.5228 -398.3631
#> 4 -26.401120 -26.519670 -13.428478 -1158.7049 -1028.9365 -771.1987 -404.0766
#> 5 -29.507317 -50.902659 -31.120927 -1091.6989  -973.5682 -724.0033 -395.8134
#> 6 -33.289547 -59.058292 -40.786541 -1133.7126  -996.3136 -740.3583 -396.2114
#>        159         160        161        162        163        164        165
#> 1 11.22449  0.05792039   3.589844  -7.223169   7.085679   4.836272   8.538979
#> 2 53.56475 44.62994681 -29.881918 -45.593629 -49.112370 -48.031289 -38.968646
#> 3 64.85113 52.99283550 -16.527173 -29.329941 -33.514770 -30.228377 -35.695584
#> 4 53.51682 22.91127918 -34.740636 -29.943700 -38.479057 -31.807263 -31.142789
#> 5 37.00336 42.27522696 -21.208676 -44.037629   6.721799  -9.078249 -13.509396
#> 6 61.48240 65.88287810  -7.844680 -36.459840   1.071826 -33.098377 -30.445729
#>          166        167       168       169      170      171        172
#> 1  -969.2763  -876.8522 -681.3070 -369.7548 30.45248 16.46712 -11.098092
#> 2 -1118.9909 -1006.0444 -760.9935 -402.8407 43.38765 45.31831 -19.642466
#> 3 -1119.6392 -1000.0477 -758.2447 -397.7569 31.65703 40.21176  -6.359429
#> 4 -1091.3333  -973.0261 -755.8220 -392.7524 36.17795 15.51829 -32.087672
#> 5 -1081.6757  -973.0403 -734.4616 -402.8487 27.60909 32.85881 -21.787501
#> 6 -1093.5875  -981.9706 -735.3925 -402.7790 24.52980 36.51386  -2.555680
#>          173       174         175       176       177
#> 1 -36.099463 -16.42808 -18.0691061 -28.16114  7.702312
#> 2 -27.749101 -23.14547 -13.6588168 -28.39317 10.226562
#> 3   7.801406  32.67586  34.1215009  27.76015 10.958000
#> 4 -24.215795 -36.13013 -26.0627971 -26.01909 11.779938
#> 5 -38.704730  15.14459   0.8798961  -8.76949  9.838188
#> 6 -13.641611  40.25635  20.7692744  10.58996 11.585750
```

## Part 2b. Run DetectClassify

``` r
  TestFileDirectory <- '/Users/denaclink/Library/CloudStorage/Box-Box/gibbonRSampleFiles/GibbonTestFiles'
  
  OutputDirectory <-  "/Users/denaclink/Desktop/RStudio Projects/gibbonR/data/DetectAndClassifyOutput"
  
  DetectAndClassify(input=TestFileDirectory,
                    input.type='directory',
                    feature.df=TrainingDataMFCC,
                    model.type.list=c('SVM','RF'),
                    tune = TRUE,
                    short.wav.duration=300,
                    target.signal = c("female.gibbon"),
                    min.freq = 400, max.freq = 1600,
                    noise.quantile.val=0.15,
                    time.window.number =3,
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
                    write.csv.output=FALSE,verbose=TRUE,
                    random.sample='NA')
#> [1] "Machine learning in progress..."
#> [1] "SVM in progress..."
#> [1] "SVM accuracy 98.1132075471698"
#> Time difference of 1.620221 secs
#> [1] "RF in progress..."
#> 
#> Call:
#>  randomForest(x = feature.df[, 2:ncol(feature.df)], y = feature.df$class) 
#>                Type of random forest: classification
#>                      Number of trees: 500
#> No. of variables tried at each split: 13
#> 
#>         OOB estimate of  error rate: 3.77%
#> Confusion matrix:
#>               female.gibbon noise class.error
#> female.gibbon            25     1  0.03846154
#> noise                     1    26  0.03703704
#> Time difference of 0.06292009 secs
#> [1] "Classifying for target signal female.gibbon"
#> [1] "Computing spectrogram for file S11_20180217_080003 1 out of 1"
#> [1] "Running detector over sound files"
#> [1] "Creating datasheet"
#> [1] "System processed 7201 seconds in 15 seconds this translates to 480.5 hours processed in 1 hour"
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
UMAPBiplotAddSpectrograms(input.dir.Focal="/Users/denaclink/Desktop/RStudio Projects/gibbonR/data/MultipleSoundClasses/",output.dir.Focal="/Users/denaclink/Desktop/RStudio Projects/gibbonR/data/MultipleSoundClasses/Thumbnails/",add.spectrograms=TRUE,min.freq=400,max.freq=1600,main="UMAP Plot")
#> [1] "Step 1 Calculating MFCCs"
#> [1] "Step 2 Creating biplot"
#> [1] "Step 3 Creating Spectrograms"
#> [1] "/Users/denaclink/Desktop/RStudio Projects/gibbonR/data/MultipleSoundClasses/Thumbnails/ already exists"
#> [1] "Step 4 Adding Spectrograms to Plot "
```

<img src="man/figures/README-unnamed-chunk-8-1.png" width="100%" /> \##
Part 3b. Create a UMAP plot colored by affinity propagation clustering

``` r
library(gibbonR)
library(ggpubr)
library(apcluster)
#> 
#> Attaching package: 'apcluster'
#> The following object is masked from 'package:stats':
#> 
#>     heatmap
AffinityBiplotAddSpectrograms(input.dir.Focal="/Users/denaclink/Desktop/RStudio Projects/gibbonR/data/MultipleSoundClasses/",output.dir.Focal="/Users/denaclink/Desktop/RStudio Projects/gibbonR/data/MultipleSoundClasses/Thumbnails/",class='fixed', q.fixed=0.1,add.spectrograms=TRUE,min.freq=400,max.freq=1600,main="UMAP Plot")
#> [1] "Step 1 Calculating MFCCs"
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
