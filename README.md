# PANTS
R package for Pathway analysis via network smoothing.

[![Build Status](https://travis-ci.org/jdreyf/PANTS.svg?branch=master)](https://travis-ci.org/jdreyf/PANTS)
[![Coverage Status](https://img.shields.io/codecov/c/github/jdreyf/PANTS/master.svg)](https://codecov.io/github/jdreyf/PANTS?branch=master)

## Install
Install `PANTS` from GitHub using `devtools` within R. You must install `devtools` if you haven't before. `PANTS` depends on `ezlimma`, which depends on `limma`, so you must also install these if you haven't before.
```
source("http://bioconductor.org/biocLite.R")
biocLite("limma") #if haven't already installed limma
install.packages("devtools") #if haven't already installed devtools
library(devtools)
devtools::install_github(repo="jdreyf/ezlimma", build_vignettes = TRUE)
devtools::install_github(repo="jdreyf/PANTS", build_vignettes = TRUE)
```

## Usage
The vignette presents a tutorial. To see the vignette:
```
library(limma)
library(ezlimma)
library(PANTS)
browseVignettes(package="PANTS")
```
and click on HTML.
