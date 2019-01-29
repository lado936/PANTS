# PANTS
Pathway analysis via network smoothing R package.

[![Build Status](https://travis-ci.org/jdreyf/PANTS.svg?branch=master)](https://travis-ci.org/jdreyf/PANTS)
[![Coverage Status](https://img.shields.io/codecov/c/github/jdreyf/PANTS/master.svg)](https://codecov.io/github/jdreyf/PANTS?branch=master)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

## Install
On Windows, you need to install [Rtools](https://cran.r-project.org/bin/windows/Rtools/).

Install `PANTS` from GitHub using `remotes` within R. You must install `remotes` if you haven't before. `PANTS` depends on `ezlimma`, which depends on `limma`, so you must also install these if you haven't before.
```
#if haven't already installed limma
source("http://bioconductor.org/biocLite.R")
biocLite("limma")

install.packages("remotes") #if haven't already installed remotes
library(remotes)
remotes::install_github(repo="jdreyf/ezlimma", build_opts = c("--no-resave-data", "--no-manual"))
remotes::install_github(repo="jdreyf/PANTS", build_opts = c("--no-resave-data", "--no-manual"))
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
