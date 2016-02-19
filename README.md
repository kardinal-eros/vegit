vegit
=====

Build status
------------

[![Travis-CI Build Status](https://travis-ci.org/kardinal-eros/vegit.svg?branch=master)](https://travis-ci.org/kardinal-eros/vegit)
<!-- [![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/vegit)](http://cran.r-project.org/package=vegit) -->

These are utility functions to maintain, extend and create data inside and for the [**vegsoup-data** repository](https://github.com/kardinal-eros/vegsoup-data) repository. The package is closely related to the [**vegsoup** *R*-package](http://r-forge.r-project.org/projects/vegsoup/) and contains functions that are not – and will (maybe) never be – part of that package. 

Installation
------------

You may directly install the package from GitHub using the below set of commands.

```R
# if not already installed
install.packages("devtools")

library(devtools)

install_github("kardinal-eros/vegit")

library(vegit)
```

Functionality
-------------

+ `char2dd` helps in converting coordinate formats
+ `join` helps in joining taxonomic reference lists
+ `csv2txt`, `extractTaxon`, `replaceTaxon`, `splitArray`, `trimTaxon` are functions that are useful when working with text files, especially regarding the `read.verbatim` function supplied by the *vegsoup* package.
