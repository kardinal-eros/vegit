R CMD build /Users/roli/Documents/vegit/pkg
R CMD check vegit_0.1-1.tar.gz
R CMD INSTALL -l /Users/roli/Library/R/3.1/library vegit_0.1-1.tar.gz
