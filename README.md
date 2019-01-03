# PANTS
R package for Pathway analysis via network smoothing.

[![Build Status](https://travis-ci.org/jdreyf/PANTS.svg?branch=master)](https://travis-ci.org/jdreyf/PANTS)
[![Coverage Status](https://img.shields.io/codecov/c/github/jdreyf/PANTS/master.svg)](https://codecov.io/github/jdreyf/PANTS?branch=master)

## Install
Install `PANTS` from GitHub using `devtools` within R. You must install `devtools` if you haven't before. `PANTS` depends on `ezlimma` so you must also install this if you haven't before.
```
install.packages("devtools") #if haven't already installed devtools
library(devtools)
devtools::install_github(repo="jdreyf/ezlimma")
devtools::install_github(repo="jdreyf/PANTS", build_vignettes = TRUE)
```

## Usage
The vignette presents a tutorial. To see the vignette:
```
library(ezlimma)
library(PANTS)
browseVignettes(package="PANTS")
```
and click on HTML.
