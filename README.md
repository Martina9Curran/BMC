
<!-- README.md is generated from README.Rmd. Please edit that file -->

# BMC

<!-- badges: start -->

<!-- badges: end -->

The goal of BMC is to classify timeseries data into one of 13
behaviours: G: Growth D: Decline E: Equilibrium GD: Growth and Decline
GE: Growth and Equilibrium DG: Decline and Growth DE: Decline and
Equilibrium GDE: Growth, Decline and Equilibrium DGE: Decline, Growth
and Equilibrium OC: Oscillating Constant(ly) OG: Oscillating Growth OD:
Oscillating Decline OE: Oscillating Equilibrium

## Installation

You can install the development version of BMC from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Martina9Curran/BMC")
```

## Example

This is a basic example which shows you how to solve a common problem:
Please note you need to include the path to the file, it works with .csv
only, but do not add .csv to the path\!

``` r
library(BMC)
## basic example code
behaviours <- classify("C:/Users/techstaff/Desktop/packagetest/Supplementary Data/Data/check3")
#> [1] "creating attributes for training data"
#> [1] "getting behaviour modes of data"
#> [1] "compressing feature vectors"
#> [1] "creating attributes"
#> [1] "creating tree"
#> [1] "training tree"
#> Loading required package: lattice
#> Loading required package: ggplot2
```

<img src="man/figures/README-example-1.png" width="100%" />

    #> [1] "creating attributes for testing data"
    #> [1] "getting behaviour modes of data"
    #> [1] "compressing feature vectors"
    #> [1] "creating attributes"
    #> [1] "creating plots of behaviours. You have 6 behaviours"

<img src="man/figures/README-example-2.png" width="100%" />

For classification that needs more input, first check the equilibrium on
your plots: The threshold value is default at .001 If you get
equilibrium when you don’t want it, make the threshold smaller (eg 0,
.0001) If there is no equilibrium seen by the classifier, but you want
it to be, make the threshold bigger (eg .01, .025)

``` r
classify("C:/Users/techstaff/Desktop/packagetest/Supplementary Data/Data/check3", threshold=.0001)
#> [1] "creating attributes for training data"
#> [1] "getting behaviour modes of data"
#> [1] "compressing feature vectors"
#> [1] "creating attributes"
#> [1] "creating tree"
#> [1] "training tree"
```

<img src="man/figures/README-threshold-1.png" width="100%" />

    #> [1] "creating attributes for testing data"
    #> [1] "getting behaviour modes of data"
    #> [1] "compressing feature vectors"
    #> [1] "creating attributes"
    #> [1] "creating plots of behaviours. You have 7 behaviours"

<img src="man/figures/README-threshold-2.png" width="100%" />

    #>  [1] OE  GE  OC  OE  OE  GE  OE  GDE OE  OE  GE  OE  OE  OD  OD  GDE OE  GD  DGE
    #> [20] OE 
    #> Levels: D DE DG DGE E G GD GDE GE OC OD OE OG

If there are problems with the behaviours (it is growing then declining,
but says Decline Growth) change the attributes of the tree. This is
generally the only issue with median needing to be changed, but in case
of others, check the attributes that are involved in the decision of the
behaviour given/required (follow the branches from the node up to the
root). To change these, they are included as extra parameters in ‘…’ and
as such, are included as strings in the classify function. Note the
threshold needs to be added whether or not it is being changed\!

``` r
classify("C:/Users/techstaff/Desktop/packagetest/Supplementary Data/Data/check3", threshold=.001, "median")
#> [1] "creating attributes for training data"
#> [1] "getting behaviour modes of data"
#> [1] "compressing feature vectors"
#> [1] "creating attributes"
#> [1] "creating tree"
#> [1] "training tree"
```

<img src="man/figures/README-attributes-1.png" width="100%" />

    #> [1] "creating attributes for testing data"
    #> [1] "getting behaviour modes of data"
    #> [1] "compressing feature vectors"
    #> [1] "creating attributes"
    #> [1] "creating plots of behaviours. You have 6 behaviours"

<img src="man/figures/README-attributes-2.png" width="100%" />

    #>  [1] OE  GE  OE  OE  OE  GE  OE  DGE GDE OE  GE  OE  OE  OE  OD  DGE DE  GE  GDE
    #> [20] OE 
    #> Levels: D DE DG DGE E G GD GDE GE OC OD OE OG

Hyperparameters available for removing unwanted noise include:
“deleteStart” (Default FALSE, using this will set it to TRUE): which
deletes the start of the output in the feature vector when creating
attributes. It does not change the data “keepDiscrete” (Default FALSE to
help remove noise, using this will set it to TRUE, keeping all small
changes in the data except for the threshold, this still works the same)

``` r
classify("C:/Users/techstaff/Desktop/packagetest/Supplementary Data/Data/check3", threshold=.001, "median", "deleteStart")
#> [1] "creating attributes for training data"
#> [1] "getting behaviour modes of data"
#> [1] "compressing feature vectors"
#> [1] "creating attributes"
#> [1] "creating tree"
#> [1] "training tree"
```

<img src="man/figures/README-numerous attributes and attributes-1.png" width="100%" />

    #> [1] "creating attributes for testing data"
    #> [1] "getting behaviour modes of data"
    #> [1] "compressing feature vectors"
    #> [1] "creating attributes"
    #> [1] "creating plots of behaviours. You have 6 behaviours"

<img src="man/figures/README-numerous attributes and attributes-2.png" width="100%" />

    #>  [1] OE  GE  OE  OE  OE  GE  OE  DGE GDE OE  GE  OE  OE  OE  OD  DGE DE  GE  GDE
    #> [20] OE 
    #> Levels: D DE DG DGE E G GD GDE GE OC OD OE OG
